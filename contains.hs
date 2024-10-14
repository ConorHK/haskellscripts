#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import System.Environment (getArgs)
import Data.List (isInfixOf)

-- Function to check if substring exists in string
contains :: String -> String -> Bool
contains str subStr = subStr `isInfixOf` str

main :: IO ()
main = do
    args <- getArgs
    case args of
        [str, subStr] -> 
            if contains str subStr
            then putStrLn "Substring found"
            else putStrLn "Substring not found"
        _ -> putStrLn "Usage: provide two arguments (string and substring)"
