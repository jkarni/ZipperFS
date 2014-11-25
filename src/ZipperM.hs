{-# LANGUAGE ScopedTypeVariables #-}
module ZipperM (Term(..)
               , FileName
               , FileCont
               , Path(..)
               , DZipper(..)
               , dzip'term
               , module Control.Monad.CC
               , promptP
              ) where

import Control.Monad.CC
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Map as Map

---------------------------------------------------------------
-- Control operators

-- Non-darcs
-- promptP :: (MonadDelimitedCont p s t) => (p a -> t a) -> t a

-- Darcs repo of CC-delcont
-- promptP :: (MonadDelimitedCont t) => (Prompt t a -> t a) -> t a

promptP f = do p <- newPrompt; pushPrompt p (f p)

---------------------------------------------------------------
-- Term to traverse

type FileName = String
type FileCont = String
data Term = File String | Folder (Map.Map FileName Term)

instance Show Term where
   showsPrec _ (File file) = (file ++)
   showsPrec _ (Folder dir) =
       ("\n >>>" ++) . (Map.foldWithKey fl ("\n<<<" ++) dir)
       where fl k term acc = ("\n" ++) . (k ++) . (": " ++) .
                             (showsPrec 5 term) . acc

-- Path in the Term
-- Down is the same as DownToN 0 -- descend to the first child
data Path = Down | DownTo FileName | DownToN Int | Up | Next
              deriving (Eq, Show)

-- Updateable traverse that maximally preserves the sharing
traverse tf term = traverse' id Down term >>= maybeM term id
   where traverse' next_dir init_dir term =
             do
             (term', direction) <- tf init_dir term
             let new_term = maybe term id term'
             select (next_dir direction) new_term >>= maybeM term' Just
         select Up t = return Nothing
         select Next t@(File _) = return Nothing
         select dir@(DownTo fname) t@(Folder fld) =
             select (DownToN (Map.findIndex fname fld)) t
         select dir t@(Folder _) | dir == Next || dir == Down =
             select (DownToN 0) t
         select (DownToN n) t@(Folder fld) | n >= Map.size fld =
             return Nothing
         select (DownToN n) t@(Folder fld) =
             do
             let (fname,term) = Map.elemAt n fld
             t' <- traverse' id (DownTo fname) term  >>=
                   (return . fmap (\newv -> Folder $
                                   Map.adjust (const newv) fname fld))
             let nextd = let idx = succ n
                         in if idx == Map.size fld then next Up
                            else next (DownToN idx)
             traverse' nextd Up (maybe t id t') >>= maybeM t' Just

         next next_dir dir = if dir == Next then next_dir else dir
         maybeM onn onj v = return $ maybe onn onj v


fs1 :: Term =
      Folder $ Map.fromList [("d1",d1), ("d2",Folder $ Map.empty),
                             ("fl1", File "File1"),
                             ("fl2", File "File2")]
          where d1 = Folder $ Map.fromList [("fl13",File "File 3"),
                                            ("d11", d11)]
                d11 = Folder $ Map.fromList [("d111", Folder $ Map.empty)]


{-
-- self-application...
-- A sort of a 2-place Y-combinator: term2 f = f (term2 f) (term2 f)
-- The recursion is represented via sharing indeed
-- term2 represents an infinite tree spanning in depth and in breadth
term2 = L "f" (A (A f (A term2 f)) (A term2 f)) where f = Var "f"

-}

testt1 = runIdentity (traverse (\_ term -> return (Nothing,Next)) fs1)
-- *Zipper2> testt1 == fs1
-- True

testt2 = traverse tf fs1
   where tf dir term = do print dir; print term; return (Nothing,Next)
testt3 = traverse tf fs1
   where
   tf (DownTo "d11") term  = do
                             print "cutting"
                             print term
                             return (Nothing,Up)
   tf dir term = do
                 print term
                 return (Nothing,Next)


testt4 = runIdentity (traverse tf fs1)
   where tf (DownTo "d11") _ = return (Just $ Folder $ Map.empty ,Up)
         tf (DownTo "fl2") _ = return (Just $ File $ "New file2", Up)
         tf _ _ = return (Nothing,Next)

lprint x = liftIO $ print x

-- fs2 is harder to handle via traverse as we are liable to loop
-- easily. Zipper is far better for fs2
-- In general, traverse is better for context-insensitive transformations
-- and zipper is for context-sensitive

-- Note that the zipper data structure is very generic
-- It depends only on the _interface_ of the traversal function
-- (but not on its implementation)

-- One may say, why not to put path accumulation into `traverse' itself?
-- We could have. However, we wish to illustrate here that the traverse
-- deals only with the local information. Accumulating it into a global
-- state is left for the clients. Zipper can let us add a new, `missing'
-- aspect to the enumerator.

data DZipper r m term dir =
   DZipper{
           dz_dir  :: dir,
           dz_path :: [dir],
           dz_term :: term,
           dz_k :: CCT r m (Maybe term, dir) -> CCT r m (DZipper r m term dir)
           }
 | DZipDone term

data HPReq r m dir = HPReq dir (CCT r m [dir] -> CCT r m (HPReq r m dir))

dzip'term term = do
                p <- newPrompt
                path_pr <- newPrompt
                pushPrompt p (acc_path [] (pushPrompt path_pr (
                                         traverse (tf p path_pr) term >>=
                                               done p)))
   where tf p path_pr dir term =
             do
             path <- shift path_pr (\k -> return (HPReq dir k))
             shift p (\k -> return (DZipper dir path term k))
         acc_path path body =
             do
             HPReq dir k <- body
             let new_path = if dir == Up then tail path else dir:path
             acc_path new_path (k (return new_path))
         -- we use abort to return the result...
         done p term = abort p (return $ DZipDone term)

testdz1 :: IO ()
   = runCCT (
        do
        dz <- dzip'term fs1
        let loop (DZipDone term) = lprint "Finished" >> lprint term
            loop dz =
                do
                 lprint $ (show $ dz_dir dz) ++ "->" ++ (show $ dz_path dz)
                 lprint $ dz_term dz
                 dz_k dz (return (Nothing,Next)) >>= loop
        loop dz
           )


{-


zip'through (ZipDone term) = lprint "Done" >> lprint term
zip'through (Zipper dir term k) = do lprint dir; lprint term
                                    nz <- k (return (Nothing,Next))
                                    zip'through nz

zip'move dir (Zipper _ term k) = do lprint dir; lprint term
                                   k (return (Nothing,dir))


tz1 :: IO () = runCCT (zip'term traverse term1 >>= zip'through)

tz2 :: IO ()
   = runCCT (
        do
        zipper <- zip'term traverse term1
        z1 <- zip'move Next zipper
        Zipper d (A _ _) k <- zip'move Next z1
        k (return (Just (A (Var "x") (Var "x")),Up)) >>= zip'move Down
                                                     >>= zip'through
        -- uncomment the following to see that the cursor z1
        -- is still valid, but it doesn't see the changes
        --zip'through z1
        -- but the same cursor sees its own changes!
        )

tz3 :: IO ()
   = runCCT (
        do
        zipper <- zip'term traverse term2
        let max_depth = 5
        t <- traverse_replace max_depth zipper 0
        lprint "Final"; lprint t)
     where
     traverse_replace max_depth (Zipper dir term k) depth =
         do
         let new_depth = update_depth dir depth
         let loop z = traverse_replace max_depth z new_depth
         if new_depth <= max_depth then k (return (Nothing, Next)) >>= loop
            else case term of
                    L "f" _ -> k (return (Just (L "f" (Var "f")),Up)) >>=
                               loop
                    _ -> k (return (Nothing, Next)) >>= loop
     traverse_replace max_depth (ZipDone term) depth = return term

     update_depth Up = (+ (-1))
     update_depth _  = (+ 1)

-}
