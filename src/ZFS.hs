{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Zipper-based File/Operating system
with threading and exceptions all realized via delimited continuations.
There are no unsafe operations, no GHC (let alone) Unix threads,
no concurrency problems. Our threads can't even do IO and can't
mutate any global state -- and the type system sees to it.

Please see http://pobox.com/~oleg/ftp/papers/zfs-talk.pdf
for the demo and explanations.

-- $Id: ZFS.hs,v 1.8 2005/10/14 23:00:41 oleg Exp $

NOTE: the above demo and explanation can be viewed at the following url:
  - https://web.archive.org/web/20190809002903/http://okmij.org/ftp/continuations/ZFS/zfs-talk.pdf
-}



module ZFS where

import ZipperM

import Control.Exception (try, bracket)
import Control.Monad.Trans (liftIO, MonadIO())
import qualified Data.List as List
import qualified Data.Map  as Map
import Foreign                          -- needed for select hacks:
import Foreign.C                        -- Unix select is not available in
import Foreign.Ptr                      -- GHC
import Network.Socket
import System.IO
import qualified System.IO.Error as IO
import System.Posix (closeFd)
import System.Posix.Types(Fd(..))

-- import CC_FrameT (runCC) -- have to import runCC manually, even though the import of
                         -- ZipperM should pull it in.

-- Port to serve clients from
newClientPort :: PortNumber
newClientPort = 1503
-- select_timeout = 100000 -- microseconds

-- Initial content of the file system
-- Certainly, structurally richer filesystems are equally possible
-- (where content is annotated with attributes, e.g.)
-- A lambda-term can be made a filesystem too
fs1 :: Term
fs1 =   Folder $ Map.fromList [("d1",d1), ("d2",Folder $ Map.empty),
                              ("fl1", File "File1"),
                              ("fl2", File "File2")]
           where d1 = Folder $ Map.fromList [("fl13",File "File 3"),
                                             ("d11", d11)]
                 d11 = Folder $ Map.fromList [("d111", Folder $ Map.empty)]

-- Another file system -- this time, it is cyclic!
fs2 :: Term
fs2 = Folder $ Map.fromList [("d1",fs2), ("fl1", File "File1")]

-- Operating system requests: from a ``process'' to the ``OS''
type FSZipper r m = DZipper r m Term Path

-- Note: the base monad type `m' is left polymorphic.
-- A Process doesn't do any IO (it asks the ``OS'').
-- So, the significant part of the OS, the process itself, is overtly
-- outside the IO monad!
-- Note: using different prompts, the requests can be modularized.
-- Unlike OS (with its only one syscall handler), we can have as
-- many syscall handlers as we wish.
data OSReq r m = OSRDone
               | OSRRead (ReadK r m)
               | OSRWrite String (UnitK r m)
               | OSRTrace String (UnitK r m) -- so a process can syslog
               | OSRCommit Term (UnitK r m)
               | OSRefresh (CCT r m (FSZipper r m) -> CCT r m (OSReq r m))

type UnitK r m = CCT r m () -> CCT r m (OSReq r m)
type ReadK r m = CCT r m String -> CCT r m (OSReq r m)

data ProcessCTX = ProcessCTX { psocket :: Socket -- process' socket
                             }

-- A process can only be blocked on reading. For simplicity we assume
-- that writing into the client socket never blocks

data JobQueueT r = JQBlockedOnRead ProcessCTX (ReadK r IO)
                 | JQRunnable ProcessCTX (UnitK r IO)
                 | JQNewClient Socket  -- accept new clients from

data World r = World { mountedFS :: Term
                     , jobQueue  :: [JobQueueT r]
                     , osPrompt  :: Prompt r (OSReq r IO)
                     }

main' :: Term -> IO a
main' fs = bracket (serverSocket newClientPort) sClose $
  \s ->
    do
    -- The following doesn't help: accept blocks anyway...
    -- setFdOption (Fd (fdSocket s)) NonBlockingRead True
    runCCT $ do
           p <- newPrompt
           syslog ["Entering the osloop",show s]
           osloop $ World{
                        mountedFS = fs,
                        jobQueue = [JQNewClient s],
                        osPrompt = p}
               where
                 serverSocket port = do
                   s <- socket AF_INET Stream 0
                   setSocketOption s ReuseAddr 1
                   localhost <- inet_addr "127.0.0.1"
                   bindSocket s (SockAddrInet port localhost)
                   listen s 5
                   return s

-- In OS parlance, the following is the interrupt handler.
-- It `waits' for interrupts that is, if any input socket has something
-- to read from.
-- It doesn't actually return, so the answer type is just any
-- osloop :: World r -> CCT r IO any
osloop world =
    maybe (wait'for'intr world) (uncurry try'to'run) (find'runnable world)
    >>= osloop

  where
  -- Try to find the first runnable job
  find'runnable world = case break is'runnable (jobQueue world) of
       (_,[]) -> Nothing
       (jq1,(runnable:jq2)) -> Just (runnable, world{jobQueue=jq1++jq2})
   where is'runnable (JQRunnable _ _) = True
         is'runnable _ = False

  wait'for'intr world@World{jobQueue=jq} =
      do readyfd <- liftIO $ select'read'pending mfd
         case break (\e -> maybe False (`elem` readyfd) (toFD e)) jq of
            (_,[]) -> return world -- nothing found
            (jq1,(now'runnable:jq2)) ->
                try'to'run now'runnable world{jobQueue=jq1++jq2}
   where
   -- compile the list of file descriptors we are waiting at
   mfd = foldr (\e a -> maybe [] (:a) (toFD e)) [] jq
   toFD (JQNewClient s) = Just $ fdSocket s
   toFD (JQBlockedOnRead ProcessCTX{psocket=s} _) = Just $ fdSocket s
   toFD _ = Nothing

  -- Add to the end of the job queue
  enqueue el world = world{jobQueue = jobQueue world ++ [el]}

--  ifnM action onf ont = liftIO action >>= \b -> if b then ont else onf

  -- New client is trying to connect
  try'to'run qe@(JQNewClient s) world =
      do
      syslog ["accepting from",show s]
      (clientS,addr) <- liftIO $ accept s
      liftIO $ setSocketOption clientS NoDelay 1
      syslog ["accepted new client connection from ", show addr]
      let newCtx = ProcessCTX clientS
      run'process (fsProcess (dzip'term (mountedFS world)))(osPrompt world)
              >>= interpret'req (enqueue qe world) newCtx

  try'to'run (JQRunnable ctx k) world =
      k (return ()) >>= interpret'req world ctx

  -- A client socket may have something to read
  try'to'run (JQBlockedOnRead ctx@ProcessCTX{psocket=s} k) world =
      do
      syslog ["reading from",show s]
      syslog ["osloop: queue size: ", show $ length $ jobQueue world]
      dat <- liftIO $ (
             do r <- try (recv s (1024 * 8))
                case r of
                       Left err  -> if IO.isEOFError err then return ""
                                    else ioError err
                       Right msg -> return msg)
      k (return dat) >>= interpret'req world ctx

-- The system logger
syslog :: (Control.Monad.Trans.MonadIO m) => [String] -> m ()
syslog s = liftIO $ putStrLn (concat s)

-- The interpreter of OS requests -- the syscall handler, in OS parlance
-- It handles simple requests by itself. When the request involves
-- rescheduling or change in the global OS state, it returns to
-- the scheduler/interrupt-handler/osloop.

-- The process is finished
interpret'req :: World r -> ProcessCTX -> OSReq r IO -> CCT r IO (World r)
interpret'req world ctx OSRDone = (liftIO $ sClose $ psocket ctx)
                                  >> return world

-- The request for read may block. So, we do the context switch and go
-- to the main loop, to check if the process socket has something to read
-- from
interpret'req world ctx (OSRRead k) =
       return world{jobQueue = (jobQueue world) ++ [JQBlockedOnRead ctx k]}

-- We assume that writing to a socket never blocks
interpret'req world ctx (OSRWrite datum k) =
      do
      send' (psocket ctx) datum
      k (return ()) >>= interpret'req world ctx
 where
  send' _ ""  = return ()
  send' s msg = do  c <- liftIO $ send s msg
                    send' s (drop c msg)

interpret'req world ctx (OSRTrace datum k) =
      do
      syslog ["Trace from",show $ psocket ctx,": ",datum]
      k (return ()) >>= interpret'req world ctx

interpret'req world ctx (OSRCommit term k) =
       return world{jobQueue = (jobQueue world) ++ [JQRunnable ctx k],
                    mountedFS = term}

interpret'req world ctx (OSRefresh k) =
       k (dzip'term $ mountedFS world) >>= interpret'req world ctx

-- We have the functionality of threads -- although our whole program
-- is simply threaded, both at the OS level and at the GHC runtime level.
-- Our process functions don't even have the IO type!
-- Note, the function to run the process has forall m. That means, a process
-- function can't do any IO and can't have any reference cells.
-- Processes can't mutate the global state -- and the type system checks that!
-- Because processes can't interfere with each other and with the OS, there
-- is no need for any thread synchronization, locking, etc. We get
-- the transactional semantics for free.
-- Of course, as different processes manipulate their own (copy-on-write)
-- terms (file systems), when the processes commit, there may be conflicts.
-- So, one has to implement some conflict resolution -- be it versioning,
-- patching, asking for permission for update, etc. But
-- these policies are implemented at the higher-level; the programmer can
-- implement any set of policies. Because processes always ask the supervisor
-- for anything, and the supervisor has the view of the global state,
-- the resolution policies are easier to implement in this execution model.
run'process :: (forall m. Monad m =>
                (Prompt r (OSReq r m)) -> CCT r m (OSReq r m))
            -> Prompt r (OSReq r IO) -> CCT r IO (OSReq r IO)
run'process body p = pushPrompt p (body p)

-- Processes. No IO action is possible in here
fsProcess :: Monad m =>
             CCT r m (FSZipper r m) -> Prompt r (OSReq r m)
          -> CCT r m (OSReq r m)
fsProcess zipper'action svcp =
    do
    z <- zipper'action
    svc svcp $ OSRTrace "Begin process"
    fsloop z svcp ""

fsloop :: forall r (m :: * -> *).
                                      (Monad m) =>
                                      DZipper r m Term Path
                                      -> Prompt r (OSReq r m)
                                      -> String
                                      -> CCT r m (OSReq r m)
fsloop z svcp line'acc
    = do
      send_shell_prompt z svcp
      (line,rest) <- read'line line'acc
      let (cmd,arg) = breakspan is'whitespace line
      svc svcp $ OSRTrace $ "received command: " ++ cmd
      maybe (svc svcp (OSRWrite $ "bad command: " ++ cmd) >>
             fsloop z svcp rest)
            (\h -> h z svcp cmd arg rest)
            (List.lookup cmd fsCommands)
  where
  -- Read until we get newline
  read'line acc = case break is'nl acc of
                  (_,"") -> do
                            b <- svc svcp OSRRead
                            svc svcp $ OSRTrace $ "Read str: " ++ b
                            (l,rest) <- read'line b
                            return (acc ++ l, rest)
                  (l,rest) -> return (l,snd $ span is'nl rest)

  send_shell_prompt z svcp =
    svc svcp $ OSRWrite $ ("\n" ++ show_path (dz_path z) ++ "> ")

show_path :: [Path] -> String
show_path path = concatMap (\pc -> case pc of
                                           Down -> "/"
                                           DownTo s -> s ++ "/")
                           (reverse path)

fsCommands :: Monad m => [(String,FSZipper r m -> Prompt r (OSReq r m) ->
                                  String -> String -> String ->
                                  CCT r m (OSReq r m))]

fsCommands =
    [
     ("quit", \_ svcp _ _ _ -> svc svcp $ const OSRDone),
     ("cd", fsWrapper
      (\z shp _ path -> cd'zipper z shp path >>= return . FSCZ)),
     ("ls",    fsWrapper cmd'ls),
     ("cat",   fsWrapper cmd'ls),
     ("next",  fsWrapper cmd'next),

     ("mkdir", fsWrapper (cmd'mknode (Folder Map.empty))),
     ("touch", fsWrapper (cmd'mknode (File ""))),

     ("echo",  fsWrapper cmd'echo),
     ("rm",    fsWrapper cmd'rm),
     ("mv",    fsWrapper cmd'mv),
     ("cp",    fsWrapper cmd'cp),

     ("help",  fsWrapper cmd'help),

     ("commit",  fcmd'commit),
     ("refresh", \_ svcp _ _ rest -> svc svcp OSRefresh >>=
                                         \z -> fsloop z svcp rest)
   -- could have a command ``down N'' -- positional descend
   -- Note: next is really cool!
   -- Note, we can cd inside a file! So, cat is just `ls' inside a file
    ]

fcmd'commit :: forall t t1 r (m :: * -> *).
                                           (Monad m) =>
                                           DZipper r m Term Path
                                           -> Prompt r (OSReq r m)
                                           -> t
                                           -> t1
                                           -> String
                                           -> CCT r m (OSReq r m)
fcmd'commit z svcp _ _ rest = aux z
    where
    aux (DZipDone term) = (svc svcp $ OSRCommit term) >>
                          fsloop z svcp rest
    aux DZipper{dz_k = k} = k (return (Nothing,Up)) >>= aux


data FSCmdResp r m = FSCS String | FSCZ (FSZipper r m)

-- We use delimited continuations rather than an Error monad
-- A delimited continuation suffices!
fsWrapper :: forall t t1 r (m :: * -> *).
                                         (Monad m) =>
                                         (FSZipper r m
                                          -> Prompt r (FSCmdResp r m)
                                          -> t
                                          -> t1
                                          -> CCT r m (FSCmdResp r m))
                                         -> FSZipper r m
                                         -> Prompt r (OSReq r m)
                                         -> t
                                         -> t1
                                         -> String
                                         -> CCT r m (OSReq r m)
fsWrapper cmd z svcp cmd'name cmd'arg rest =
    do
    shp <- newPrompt
    resp <- pushPrompt shp (cmd z shp cmd'name cmd'arg)
    z' <- case resp of
                    FSCS str -> (svc svcp $ OSRWrite str) >> return z
                    FSCZ z   -> return z
    fsloop z' svcp rest

cmd'help :: forall t
                                               t1
                                               t2
                                               (m :: * -> *)
                                               r
                                               (m1 :: * -> *)
                                               r1
                                               (m2 :: * -> *).
                                        (Monad m, Monad m1) =>
                                        FSZipper r m -> t -> t1 -> t2 -> m1 (FSCmdResp r1 m2)
cmd'help z _ _ _ = return $ FSCS $ "Commands: " ++
                     (concat $ List.intersperse ", " $ List.map fst cmds)
  where

   cmds :: [(String, FSZipper r2 m
                    -> Prompt r2 (OSReq r2 m)
                    -> String
                    -> String
                    -> String
                    -> CCT r2 m (OSReq r2 m))]
   cmds = fsCommands

cmd'ls :: forall t
                                             r
                                             (m :: * -> *)
                                             r1
                                             (m1 :: * -> *).
                                      (Monad m) =>
                                      FSZipper r m
                                      -> Prompt r (FSCmdResp r m)
                                      -> t
                                      -> String
                                      -> CCT r m (FSCmdResp r1 m1)
cmd'ls z shp _ slash'path = cd'zipper z shp slash'path
                            >>= return . FSCS . list_node

cmd'next :: forall t t1 t2 r (m :: * -> *).
                                        (Monad m) =>
                                        DZipper r m Term Path
                                        -> t
                                        -> t1
                                        -> t2
                                        -> CCT r m (FSCmdResp r m)
cmd'next z _ _ _ =
    do z' <- dz_k z (return (Nothing,Next))
       return $ FSCZ $ case z' of DZipDone _ -> z; _ -> z'

-- main navigation function
cd'zipper :: Monad m =>
             FSZipper r m -> Prompt r (FSCmdResp r m) -> String
             -> CCT r m (FSZipper r m)
cd'zipper z _ "" = return z
cd'zipper z shp ('/':path) = do z' <- ascend'to'root z; cd'zipper z' shp path
  where
  ascend'to'root z =
      dz_k z (return (Nothing,Up)) >>= ascend'to'root' z
  ascend'to'root' z (DZipDone _) = return z
  ascend'to'root' _ z = ascend'to'root z

cd'zipper z shp ('.':'.':path) = aux z (snd $ span (=='/') path)
 where
 aux DZipper{dz_path = [Down]} _ = return z -- already at the top
 aux DZipper{dz_k = k} path = k (return (Nothing,Up)) >>=
                              (\z -> cd'zipper z shp path)
 aux (DZipDone _) _ = return z

cd'zipper DZipper{dz_term = File _} shp _ =
    abort shp (return $ FSCS "cannot descend down the file")
cd'zipper DZipper{dz_term = Folder fld, dz_k = k} shp path
    = let (pc,prest) = breakspan (== '/') path
      in if Map.member pc fld then do
                                   z' <- k (return (Nothing,DownTo pc))
                                   cd'zipper z' shp prest
         else abort shp (return $ FSCS $ "No such dir component " ++ pc)

-- List the current contents of the node pointed by the zipper
-- This function subsumes both `ls' and `cat'
-- For files, it sends the content of the file
list_node :: forall t (t1 :: * -> *) t2.
             DZipper t t1 Term t2 -> String
list_node DZipper{dz_term = File str} = str
list_node DZipper{dz_term = Folder fld} =
    Map.foldWithKey (\name el acc ->
                     "\n" ++ name ++ (case el of Folder _ -> "/"
                                                 _ -> "") ++ acc)
                    "" fld
list_node _ =  ""

-- make a node (an empty directory or an empty file or a moved node)
-- named 'dirn' in the current directory
cmd'mknode :: forall t
                                                 r
                                                 r1
                                                 (m :: * -> *)
                                                 (m1 :: * -> *).
                                          (Monad m1) =>
                                          Term
                                          -> DZipper r m1 Term Path
                                          -> Prompt r (FSCmdResp r1 m)
                                          -> t
                                          -> String
                                          -> CCT r m1 (FSCmdResp r m1)
cmd'mknode _ _ shp _ dirn | '/' `elem` dirn =
    abort shp (return $ FSCS "the name of the new node can't contain slash")
cmd'mknode _ _ shp _ "" =
    abort shp (return $ FSCS "the name of the new node is empty")
cmd'mknode _ DZipper{dz_term = File _} shp _ _ =
    abort shp (return $ FSCS "cannot create anything in a file")
cmd'mknode _ DZipper{dz_term = Folder fld} shp _ dirn
    | Map.member dirn fld =
        abort shp (return $ FSCS $ "node " ++ dirn ++ " already exists")
cmd'mknode newnode DZipper{dz_term = Folder fld, dz_k = k, dz_dir = cn}
   _ _ dirn =
    let fld' = Folder $ Map.insert dirn newnode fld
    in k (return (Just fld',Up)) >>= adj cn >>= return . FSCZ
  where
  -- go back to the current directory
    adj _ (DZipDone term) = dzip'term term
    adj cn z = dz_k z $ return (Nothing,cn)

-- echo string > path
cmd'echo :: forall t r (m :: * -> *).
                                        (Monad m) =>
                                        DZipper r m Term Path
                                        -> Prompt r (FSCmdResp r m)
                                        -> t
                                        -> String
                                        -> CCT r m (FSCmdResp r m)
cmd'echo z shp _ args = aux $ (reads::ReadS String) args
 where
 aux [(content,rest)] = aux1 content (snd $ span is'whitespace rest)
 aux _ = abort shp (return $ FSCS $ "bad format, str, of the echo cmd")
 aux1 content ('>':rest) =
     cd'zipper z shp (snd $ span is'whitespace rest) >>= aux2 content rest
 aux1 _ _ = abort shp (return $ FSCS $ "bad format, path, of the echo cmd")
 aux2 content _t DZipper{dz_term = File _, dz_k = k} =
     k (return (Just $ File content,Up)) >>= zip'back'to'place shp z
           >>= return . FSCZ
 aux2 _ rest _ = abort shp
                 (return $ FSCS $ rest ++ " does not point to a file")

-- |zip'back'to'place z z1| brings z1 to the same place as z
-- Right now we use a pathetic algorithm -- but it works...
zip'back'to'place :: forall r
                                                        (m :: * -> *)
                                                        r1
                                                        (m1 :: * -> *)
                                                        term.
                                                 (Monad m) =>
                                                 Prompt r (FSCmdResp r m)
                                                 -> DZipper r1 m1 term Path
                                                 -> DZipper r m Term Path
                                                 -> CCT r m (FSZipper r m)
zip'back'to'place shp z (DZipDone term) =
    dzip'term term >>= zip'back'to'place shp z
zip'back'to'place shp z z1 = cd'zipper z1 shp (show_path (dz_path z))

-- Delete the node pointed to by path and return the
-- updated zipper (which points to the same location as z) and the
-- deleted node
del'zipper :: forall r (m :: * -> *).
                                          (Monad m) =>
                                          DZipper r m Term Path
                                          -> Prompt r (FSCmdResp r m)
                                          -> String
                                          -> CCT r m (FSZipper r m, Term)
del'zipper z shp path = cd'zipper z shp path >>=
                      \z -> dz_k z (return (Nothing,Up)) >>= aux (dz_dir z)
  where
  aux _ (DZipDone _) =
      abort shp (return $ FSCS $ "cannot remove the root folder")
  aux (DownTo pc) DZipper{dz_term = Folder fld, dz_k = k} =
   let (Just old'node, fld') = Map.updateLookupWithKey (\_ _ -> Nothing) pc fld
   in k (return (Just $ Folder $ fld',Up))
      >>= zip'back'to'place shp z >>= \z -> return (z,old'node)

-- insert a node as `path'
ins'zipper :: forall r (m :: * -> *).
                                          (Monad m) =>
                                          Term
                                          -> FSZipper r m
                                          -> Prompt r (FSCmdResp r m)
                                          -> String
                                          -> CCT r m (FSCmdResp r m)
ins'zipper node z0 shp path =
    do
    let (dirname,basename) = split'path path
    z <- if dirname == "" then return z0 else cd'zipper z0 shp dirname
    FSCZ z <- cmd'mknode node z shp "mv" basename
    zip'back'to'place shp z0 z >>= return . FSCZ

-- rm path
-- works both on directories and files
-- One can even try to remove one's own parent -- and this is safe!
cmd'rm :: forall t r (m :: * -> *).
                                      (Monad m) =>
                                      DZipper r m Term Path
                                      -> Prompt r (FSCmdResp r m)
                                      -> t
                                      -> String
                                      -> CCT r m (FSCmdResp r m)
cmd'rm z shp _ path = del'zipper z shp path >>= return . FSCZ . fst

-- mv path_from path_to
cmd'mv :: forall t r (m :: * -> *).
                                      (Monad m) =>
                                      DZipper r m Term Path
                                      -> Prompt r (FSCmdResp r m)
                                      -> t
                                      -> String
                                      -> CCT r m (FSCmdResp r m)
cmd'mv z shp _ args = aux $ breakspan is'whitespace args
  where
  aux ("",_) = abort shp (return $ FSCS $ "mv: from-path is empty")
  aux (_,"") = abort shp (return $ FSCS $ "mv: to-path is empty")
  aux (pfrom,pto) = del'zipper z shp pfrom >>=
                    \ (z,node) -> ins'zipper node z shp pto

-- cp path_from path_to
-- We don't do any copying: we merely establish sharing:
-- so a node accessible via `from_path' becomes accessible via `to_path'
-- The copy-on-write semantics of ZFS does the rest.
-- So, in ZFS, we can copy arbitrary file systems trees in constant time!
cmd'cp :: forall t r (m :: * -> *).
                                      (Monad m) =>
                                      DZipper r m Term Path
                                      -> Prompt r (FSCmdResp r m)
                                      -> t
                                      -> String
                                      -> CCT r m (FSCmdResp r m)
cmd'cp z0 shp _ args = aux $ breakspan is'whitespace args
  where
  aux ("",_) = abort shp (return $ FSCS $ "cp: from-path is empty")
  aux (_,"") = abort shp (return $ FSCS $ "cp: to-path is empty")
  aux (pfrom,pto) = cd'zipper z0 shp pfrom >>=
                      \z -> dz_k z (return (Nothing,Up)) >>=
                            aux' (dz_dir z) pto
  aux' _ pto (DZipDone term) =
      dzip'term term >>= zip'back'to'place shp z0 >>=
                         \z -> ins'zipper term z shp pto
  aux' (DownTo pc) pto z@DZipper{dz_term = Folder fld} =
      zip'back'to'place shp z0 z >>=
         \z -> ins'zipper ((Map.!) fld pc) z shp pto

-- Supervisor call
svc :: (Monad m) => Prompt r b -> ((CCT r m a -> CCT r m b) -> b) -> CCT r m a
svc p req = ZipperM.shift p (return . req)

is'nl, is'whitespace :: Char -> Bool
is'whitespace c = c == ' ' || c == '\t'
is'nl c = c == '\n' || c == '\r'

breakspan :: (a -> Bool) -> [a] -> ([a], [a])
breakspan pred l = let (p1,p2) = break pred l
                   in (p1,snd $ span pred p2)

-- break the path into (dirname,basename)
split'path :: String -> (String, String)
split'path path = let (p1,p2) = breakspan (=='/') (reverse path)
                  in (reverse p2, reverse p1)

------------------------------------------------------------------------
-- Some hacks to get around the lack of select

  -- Darn! We don't have the real select over several descriptors!
  -- We have to implement it ourselves
type FDSET = CUInt
type TIMEVAL = CLong -- Two longs
foreign import ccall "unistd.h select" c_select
  :: CInt -> Ptr FDSET -> Ptr FDSET -> Ptr FDSET -> Ptr TIMEVAL -> IO CInt

-- Convert a file descriptor to an FDSet (for use with select)
-- essentially encode a file descriptor in a big-endian notation
fd2fds :: CInt -> [FDSET]
fd2fds fd = (replicate nb 0) ++ [setBit 0 off]
  where
    (nb,off) = quotRem (fromIntegral fd) (bitSize (undefined::FDSET))

fds2mfd :: [FDSET] -> [CInt]
fds2mfd fds = [fromIntegral (j+i*bitsize) |
               (afds,i) <- zip fds [0..], j <- [0..bitsize],
               testBit afds j]
  where bitsize = bitSize (undefined::FDSET)

test_fd_conv, test_fd_conv' :: Bool
test_fd_conv = and $ List.map (\e -> [e] == (fds2mfd $ fd2fds e)) lst
  where
  lst = [0,1,5,7,8,9,16,17,63,64,65]
test_fd_conv' = mfd == fds2mfd fds
  where
    mfd = [0,1,5,7,8,9,16,17,63,64,65]
    fds :: [FDSET] = foldr ormax [] (List.map fd2fds mfd)
--    fdmax = maximum $ List.map fromIntegral mfd
    ormax [] x = x
    ormax x [] = x
    ormax (a:ar) (b:br) = (a .|. b) : ormax ar br

-- poll if file descriptors have something to read
-- Return the list of read-pending descriptors
select'read'pending :: [CInt] -> IO [CInt]
select'read'pending mfd =
    withArray ([0,1]::[TIMEVAL]) ( -- holdover...
    \_ ->
      withArray fds (
       \readfs ->
         do
         _ <- throwErrnoIfMinus1 "select"
                 (c_select (fdmax+1) readfs nullPtr nullPtr nullPtr)
         -- because the wait was indefinite, rc must be positive!
         peekArray (length fds) readfs))
    >>= (return . fds2mfd)
  where
    fds :: [FDSET] = foldr ormax [] (List.map fd2fds mfd)
    fdmax = maximum $ List.map fromIntegral mfd
    ormax [] x = x
    ormax x [] = x
    ormax (a:ar) (b:br) = (a .|. b) : ormax ar br

foreign import ccall "fcntl.h fcntl" fcntl :: CInt -> CInt -> CInt -> IO CInt

-- use it as cleanup'fd [5..6] to clean up the sockets left hanging...
cleanup'fd :: [CInt] -> IO ()
cleanup'fd = mapM_ (closeFd . Fd)


