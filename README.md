hsdev
=====

Haskell development library and tool with support of autocompletion, symbol info, go to declaration, find references etc.

Start `hsdev` and input commands.

First of all, let's scan cabal packages and one project:

```
scan -cabal
Ok
scan -project e:\users\voidex\Documents\Projects\hsdev
Ok
cache -dump -cabal
Ok
cache -dump -project e:\users\voidex\Documents\Projects\hsdev\hsdev.cabal
Ok
exit
```

Now we can load this cache and use in commands:

```
cache -load -cabal
cache -load -project e:\users\voidex\Documents\Projects\hsdev\hsdev.cabal
complete e:\users\voidex\Documents\Projects\hsdev\src\HsDev\Commands.hs fol
Ok
foldl
foldl1
foldr
foldr1
foldM
foldM_
foldl
foldl1
foldr
foldr1
complete e:\users\voidex\Documents\Projects\hsdev\src\HsDev\Commands.hs -qualified M fro
Ok
fromAscList
fromAscListWith
fromAscListWithKey
fromDistinctAscList
fromList
fromListWith
fromListWithKey
fromSet
goto e:\users\voidex\Documents\Projects\hsdev\src\HsDev\Commands.hs locateProject
Ok
[locateProject :: FilePath -> IO (Maybe Project)
        module:
        docs:
        location:Nothing
,locateProject :: FilePath -> IO (Maybe Project)
        module:
        docs: Find project file is related to

        location:Just E:\users\voidex\Documents\Projects\hsdev\src\HsDev\Util.hs:14:1
]
exit

```
