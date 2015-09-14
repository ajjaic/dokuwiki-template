module CmdLineProcessing
    ( processCmdLine
    ) where

import System.Environment (getArgs)
import Data.List (partition, sort, intercalate)
import System.Console.GetOpt

import Tags

data TemplateOpts = H1
                  | H2
                  | Count Int
                  deriving(Show, Eq, Ord)

tempfilename :: String
tempfilename = "./dokuwiki.template"

options :: [OptDescr TemplateOpts]
options = [
    Option "c" ["count"] (ReqArg (\s -> Count (read s :: Int)) "") "Count of required templates",
    Option "1" ["heading-level-1"] (NoArg H1) "Create Heading Level 1",
    Option "2" ["heading-level-2"] (NoArg H2) "Create Heading Level 2" ]

processCmdLine :: IO ()
processCmdLine = do
    args <- getArgs
    let opts = getOpt RequireOrder options args
    case opts of
        (o, no, []) -> execute o no
        (_, _, err) -> ioError (userError (mappend (mconcat err) $ usageInfo header options)) :: IO ()
    where
    header = "Usage: dkwikitemp [OPTION...] files..."

execute :: [TemplateOpts] -> [FilePath] -> IO ()
execute opts files = do
    let (opts', count) = getCount opts
        templatestring = constructTemplateString count opts'
    if null files
        then writeFile tempfilename templatestring
        else mapM_ (flip writeFile templatestring) files

getCount :: [TemplateOpts] -> ([TemplateOpts], TemplateOpts)
getCount o = let (count, opts) = partition helper o
              in (opts, head count)
    where
    helper (Count _) = True
    helper _         = False

constructTemplateString :: TemplateOpts -> [TemplateOpts] -> String
constructTemplateString (Count c) = intercalate "\n\n"
                                    . foldr helper []
                                    . concat
                                    . replicate c
                                    . sort
    where
    helper H1 acc = h1:acc
    helper H2 acc = h2:acc



