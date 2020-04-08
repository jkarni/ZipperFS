# ZipperFS

> Zipper-based File/Operating system with threading and exceptions all realized
> via delimited continuations.  There are no unsafe operations, no GHC (let
> alone) Unix threads, no concurrency problems. Our threads can't even do IO and
> can't mutate any global state - and the type system sees to it.

## setup

```
git clone https://github.com/jkarni/ZipperFS
cd ZipperFS
cabal install
```

## usage

```
$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> :load src/ZFS.hs src/ZipperM.hs
[1 of 2] Compiling ZipperM          ( src/ZipperM.hs, interpreted )
[2 of 2] Compiling ZFS              ( src/ZFS.hs, interpreted )
...
*ZFS> main' fs1
Entering the osloop<socket: 11>
```

then, in another terminal

```
telnet localhost 1503
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.

/> help
Commands: quit, cd, ls, cat, next, mkdir, touch, echo, rm, mv, cp, help, commit, refresh
```

## license

This code is in the public domain.

## references

- [original source website](https://web.archive.org/web/20200215013516/http://okmij.org/ftp/continuations/zipper.html)
- [the slides from ZipperFS's presentation](https://web.archive.org/web/20190809002903/http://okmij.org/ftp/continuations/ZFS/zfs-talk.pdf)
- [ZipperFS's paper](https://web.archive.org/web/20190809002914/http://okmij.org/ftp/continuations/ZFS/context-OS.pdf)
- [zipper definition](https://web.archive.org/web/20181013022915/https://xlinux.nist.gov/dads/HTML/zipper.html)
- [Using zippers to handle huge trees](https://web.archive.org/web/20181013022915/http://caml.inria.fr/pub/ml-archives/caml-list/2003/04/d9701aacd4580cf3feb60ae8cd7a1836.en.html)
