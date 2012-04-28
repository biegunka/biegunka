import Biegunka.DryRun

link = git "https://github.com/supki/utils" "/home/maksenov/git/utils" --> utils
  where utils = link_repo_itself "sandbox/utils-link"
