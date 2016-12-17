module Main where
import           Control.Monad
import           Game.Minecraft.Map
import           System.Environment
import           System.Exit

showHelp :: IO ()
showHelp = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " INPUT_REGION_DIR OUTPUT.png"

-- main
main :: IO ()
main = do
        argv <- getArgs
        when (length argv < 2) showHelp
        let inputFile = head argv
            outputFile = last argv
        print "loading resources..."
        rs <- loadRegionsP inputFile
        buildAndWritePng rs outputFile True
        print "done"
