#Biegunka - .dotfiles framework

##Motivation

It is actually hard to maintain all these configs and self-written scripts by hand.  
Another problem is installing familiar environment on the new machine.  
Another slightly bigger problem is _uninstalling_ familiar environment from the wrong machine.
All this issues should be addressed by this framework.

##Details

###Main functionality
The main idea is to have something like current state of installed stuff:
```haskell
install ∷ IO Biegunka
install = bzdury
  [ git "one repo" "this clone here" --> one
  , hg "another one" "that clone here" --> another
  ]
```
Then you can save this state somewhere
```haskell
main ∷ IO ()
main = do
  α ← install
  β ← load
  let γ = merge α β
  save γ
```
and when the time comes you can uninstall stuff:
```haskell
main ∷ IO ()
main = do
  α ← load
  delete α "one repo" -- just one repo
  wipe α -- everything
```

###Scripting
Installation scripts themselves become easier, for example:
```haskell
one ∷ Script ()
one = link_repo_inself "path/from/home/directory"
```
or:
```haskell
another ∷ Script ()
another = do
  link_repo_file "path/from/repo/root" "path/from/home/directory"
  link_repo_file "path/from/repo/root" "path/from/home/directory"
```

###Minor
There is ability to test your scripts before doing actual work, just import `Biegunka.DryRun` instead of `Biegunka` and you'll get all debug information.
