module CsvIo (readLines) where

import Data.ByteString.Lazy (readFile)
import Data.Csv (decode, HasHeader(..))
import Data.Foldable (toList)
import System.Exit (exitFailure)

readLines :: String -> IO [[String]]
readLines f = do
    putStrLn ("Reading profile from " ++ f)
    csvData <- Data.ByteString.Lazy.readFile f
    case decode HasHeader csvData of
        Left err -> do
            putStrLn ("Profile failed to parse: " ++ err);
            exitFailure
        Right v -> do
            putStrLn "Successfully read profile!"
            return (toList v)
