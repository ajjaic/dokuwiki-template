module CmdLineProcessing
    ( processCmdLine
    ) where

import System.Environment (getArgs)
import Data.List (partition, sort, intercalate)
import Data.Tuple (swap)
import System.Console.GetOpt

import Tags

data TemplateOpts = H1
                  | H2
                  | Count { count :: Int }
                  deriving(Show, Eq, Ord)


tempfilename :: String
tempfilename = "./dokuwiki.template"

options :: [OptDescr TemplateOpts]
options = [
    Option "c" ["count"] (ReqArg (\s -> Count (read s :: Int)) "") "Count of required templates",
    Option "1" ["heading-level-1"] (NoArg H1) "Create Heading Level 1",
    Option "2" ["heading-level-2"] (NoArg H2) "Create Heading Level 2"
    ]

processCmdLine :: IO ()
processCmdLine = do
    args <- getArgs
    let opts = getOpt RequireOrder options args
        header = "Usage: dkwikitemp [OPTION...] files..."
    case opts of
        (o, no, []) -> execute o no
        (_, _, err) -> ioError (userError (mappend (mconcat err) $ usageInfo header options)) :: IO ()


execute :: [TemplateOpts] -> [FilePath] -> IO ()
execute opts files
    | null files = writeFile tempfilename templatestring
    | otherwise  = mapM_ (flip writeFile templatestring) files
    where
    templatestring = constructTemplateString opt_count opt_headers
    count_headers  = getCount opts
    opt_headers    = fst count_headers
    opt_count      = snd count_headers


getCount :: [TemplateOpts] -> ([TemplateOpts], TemplateOpts)
getCount = fmap head . swap . partition helper
    where
    helper :: TemplateOpts -> Bool
    helper (Count _) = True
    helper _         = False


constructTemplateString :: TemplateOpts -> [TemplateOpts] -> String
constructTemplateString (Count c) = intercalate "\n\n"
                                    . foldr helper []
                                    . concat
                                    . replicate c
                                    . sort
    where
    {- TODO: make total-}
    helper :: TemplateOpts -> [String] -> [String]
    helper H1 acc = h1:acc
    helper H2 acc = h2:acc



