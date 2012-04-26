#Biegunka - Utility scripts and configs management framework

##Motivation

It is actually hard to maintain all these configs and self-written scripts by hand.  
Another problem is installing familiar environment on the new machine.  
Another slightly bigger problem is _uninstalling_ familiar environment from the wrong machine.
All this issues should be addressed by this framework.

##Details

The main idea is to have something like current _state_ of installed stuff:

```haskell
install ∷ Biegunka ()
install = bzdury
  [ git "one repo" "this clone here" --> installOne
  , hg "another one" "that clone here" --> installAnother
  ]
```

Then you can save this state somewhere and when the time comes

```haskell
removeRepo ∷ Repository α ⇒ Biegunka () → α → Biegunka ()
--or even
wipe ∷ Biegunka () → Biegunka ()
```

Install scripts themselves become easier

```haskell
installRepoAsLink ∷ Biegunka ()
installRepoAsLink = repoTo Home "path/from/home/directory"
--or
installRepoFilesAsLinks ∷ Biegunka ()
installRepoFilesAsLinks src = do
  fromRepoTo Root ("path/from/repo/root", "path/from/system/root")
  fromRepoTo Home ("path/from/repo/root", "path/from/home/directory")
```
