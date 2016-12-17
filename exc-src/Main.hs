module Main where
import           Game.Minecraft.Map
import           System.Environment

-- main
main :: IO ()
main = do
        print "starting"
        [inputFile,outputFile] <- getArgs
        print "loading resources..."
        rs <- loadRegionsP inputFile
        buildAndWritePng rs outputFile True
        print "done"
