{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AnnotateExec
  ( exec 
  , printTerminal
  , printMarkdown
  , saveMarkdown
  , prn
  , pprn
  , Result
  ) where


import           Protolude
import           GHC.Stack (HasCallStack, SrcLoc(..), callStack, getCallStack)
import           Text.Pretty.Simple (pShowNoColor)
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Data.Text.Lazy as TxtL
import qualified Data.Colour.Names as Clr
import qualified System.FilePath as Fp
import qualified System.Console.ANSI as AN

data Result = Result { resResult :: !Text
                     , resFile :: !FilePath
                     , resStartLine :: !Int
                     , resEndLine :: !Int
                     , resStartCol :: !Int
                     , resEndCol :: !Int
                     } deriving (Show)


-- | Run a block of code that is to be annotated
exec :: [FilePath -> Map Int (Text, Int, Text) -> IO ()] -- ^ printers
     -> StateT [Result] IO ()                            -- ^ code to annotate
     -> IO ()
exec printers run = do
  rs <- snd <$> runStateT run []

  -- Get all files being processed
  let files' = Lst.nub . sort $ resFile <$> rs

  fileData1 <- traverse Txt.readFile files'
  let fileData2 = Txt.lines <$> fileData1
  let fileData3 = fileData2 <&> (Map.fromList . zip [1..])
  let fileData4 = (, 0, "") <<$>> fileData3
  let files = Map.fromList $ zip files' fileData4

  let res = foldl' (\a r -> Map.update (updateRes r) (resFile r) a) files rs
  traverse_ (\printer -> Map.traverseWithKey printer res) printers

  where
    updateRes :: Result -> Map Int (Text, Int, Text) -> Maybe (Map Int (Text, Int, Text))
    updateRes r m = Just $
      Map.update (\(ls, _, p) -> Just (ls, resStartCol r, resResult r <> "\n" <> p)) (resEndLine r) m


-- | Print annotated results to the terminal
printTerminal :: FilePath -> Map Int (Text, Int, Text) -> IO ()
printTerminal path rs = do
  putStrLn $ Fp.takeBaseName path
  traverse_ printResLines rs

  where
    printResLines (orig, col, executed) = do
      AN.setSGR [ AN.SetRGBColor AN.Foreground Clr.darkgray ]
      putText orig
      AN.setSGR [AN.Reset]
      traverse_ (printExecutedLines col) $ Txt.lines executed

    printExecutedLines col l = do
      AN.setSGR [ AN.SetRGBColor AN.Foreground Clr.lightgray ]
      putStr . Txt.unpack $ Txt.replicate (col - 1) " " <> "-- >  "
      AN.setSGR [ AN.SetColor AN.Foreground AN.Vivid AN.Yellow ]
      putText l
      AN.setSGR [AN.Reset]


-- | Save the annotated results as markdown to a file
saveMarkdown :: FilePath -> FilePath -> Map Int (Text, Int, Text) -> IO ()
saveMarkdown savePath = runMarkdown (Just savePath)


-- | Print the annotated results as markdown to the terminal
printMarkdown :: FilePath -> Map Int (Text, Int, Text) -> IO ()
printMarkdown = runMarkdown Nothing


runMarkdown :: Maybe FilePath -> FilePath -> Map Int (Text, Int, Text) -> IO ()
runMarkdown savePath' sourcePath rs = do
  let (_, res) = foldl' mkdown (True, "") rs
  let heading = "# " <> Txt.pack (Fp.takeBaseName sourcePath)

  case savePath' of
    Nothing -> do
      putText heading
      putText "```haskell"
      putText res
    Just savePath ->
      Txt.writeFile savePath $ heading <> "\n```haskell\n" <> res

  where
    mkdown :: (Bool, Text) -> (Text, Int, Text) -> (Bool, Text)
    mkdown (inHaskell, acc) (orig, _col, executed) =
      if not inHaskell && Txt.null (Txt.strip orig)
      then (inHaskell, acc)
      else
        let pre =
              if inHaskell
              then ""
              else "\n```haskell\n"
        in
        if Txt.null . Txt.strip $ executed
        then (True, acc <> pre <> orig <> "\n")
        else (False, acc <> pre <> orig <> "\n```\n\n```\n" <> formatExecuted executed <> "```\n")

    formatExecuted e =
      Txt.unlines $ (">  " <>) <$> Txt.lines e


-- | Capture a result
prn :: (HasCallStack, Show a) => a -> StateT [Result] IO ()
prn = prn' False
  
-- | Capture a result and pretty print
pprn :: (HasCallStack, Show a) => a -> StateT [Result] IO ()
pprn = prn' True

prn' :: (HasCallStack, Show a) => Bool -> a -> StateT [Result] IO ()
prn' pretty a =
  case take 1 . drop 1 $ getCallStack callStack of
    ((_,src):_) -> do
      let r = Result { resResult = if pretty then TxtL.toStrict $ pShowNoColor a else show a
                     , resFile = srcLocFile src
                     , resStartLine = srcLocStartLine src
                     , resEndLine = srcLocEndLine src
                     , resStartCol = srcLocStartCol src
                     , resEndCol = srcLocEndCol src
                     }
      
      modify (r :)
    _ -> pass


