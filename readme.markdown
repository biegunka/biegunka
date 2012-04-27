#Biegunka - Utility scripts and configs management framework

##Motivation

It is actually hard to maintain all these configs and self-written scripts by hand.  
Another problem is installing familiar environment on the new machine.  
Another slightly bigger problem is _uninstalling_ familiar environment from the wrong machine.
All this issues should be addressed by this framework.

##Details

The main idea is to have something like current _state_ of installed stuff:

```haskell
install ∷ IO Biegunka
install = bzdury
  [ git "one repo" "this clone here" --> one
  , hg "another one" "that clone here" --> installAnother
  ]
```

Then you can save this state somewhere and when the time comes

```haskell
removeRepo ∷ Repository α ⇒ Biegunka → α → IO Biegunka
--or even
wipe ∷ Biegunka → IO Biegunka
```

Install scripts themselves become easier

```haskell
one ∷ Script ()
one = link_repo_inself "path/from/home/directory"
--or
another ∷ Script ()
another = do
  link_repo_file "path/from/repo/root" "path/from/home/directory"
  link_repo_file "path/from/repo/root" "path/from/home/directory"
```
