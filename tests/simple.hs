import Biegunka
import Biegunka.Script.DryRun

simpleLink = git "https://github.com/supki/utils" "/home/maksenov/sandbox/utils" --> install
  where install = repoTo Home "sandbox/utils-link"
