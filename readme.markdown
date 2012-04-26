#Biegunka - Utility scripts and configs management framework

##Motivation

It is actually hard to maintain all these configs and self-written scripts by hand.  
Another problem is installing familiar environment on the new machine.  
Another slightly bigger problem is _uninstalling_ familiar environment from the wrong machine.
All this issues should be addressed by this framework.

##Details

The main idea is to have something like current _state_ of installed stuff:

```haskell
install ∷ InstallationState ()
install = mconcat
  [ Git "one repo" --> Path "this clone here" --> Install OneScript
  , Hg "another one" --> Path "that clone here" --> Install AnotherScript
  ]
```

Then you can save this state somewhere and when the time comes

```haskell
removeRepo ∷ InstallationState () → Repository → InstallationState ()
--or even
wipe ∷ InstallationState () → InstallationState ()
```
