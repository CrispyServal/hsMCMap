module Main where
import           Game.Minecraft.Map
import           System.Environment
import           System.Exit

showHelp :: IO ()
showHelp = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " INPUT_REGION_DIR OUTPUT.png"

buildAndSave :: FilePath -> FilePath -> Int -> IO ()
buildAndSave inDir outDir h = do
        print "loading resources..."
        rs <- loadRegionsP h inDir
        buildAndWritePng rs outDir True
        print "done"

-- main
main :: IO ()
main = do
        argv <- getArgs
        case length argv of
            2 -> buildAndSave (head argv) (last argv) 255
            3 -> buildAndSave (head argv) (argv !! 1) (read $ last argv)
            _ -> showHelp
