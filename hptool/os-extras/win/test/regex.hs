import Text.Regex.Posix

main = print $ ("bar" =~ "(foo|bar)" :: Bool)