module Main where


import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = biegunka id script (execute id)
 where
  script = link ".xmonad/xmonad.hs" ".xmonad/xmonad.hs.bak"
