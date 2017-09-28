module Data.Attoparsec.Path (rootPath, relFilePath, absFilePath, relDirPath, absDirPath) where

import Data.Attoparsec.Text (Parser, char, takeWhile1)
import Data.Functor.Identity (Identity (..))
import qualified Data.Text as T
import Control.Applicative (Alternative (..))
import Path (Path, File, Dir, Abs, Rel, (</>))
import Unsafe.Coerce (unsafeCoerce)



rootPath :: Parser (Path Abs Dir)
rootPath =
  (unsafeCoerce . (:[])) <$> char '/'

chunkPath :: Parser (Path Rel File)
chunkPath =
  (unsafeCoerce . T.unpack) <$> takeWhile1 (/= '/')

relFilePath :: Parser (Path Rel File)
relFilePath =
  let parseComplex =
        (\head tail -> unsafeCoerce (unsafeCoerce head ++ unsafeCoerce tail))
          <$> chunkPath <*> absFilePath
      parseSimple = chunkPath
  in  parseComplex <|> parseSimple

absFilePath :: Parser (Path Abs File)
absFilePath =
  (</>) <$> rootPath <*> relFilePath

relDirPath :: Parser (Path Rel Dir)
relDirPath =
  (\files suffix -> unsafeCoerce (unsafeCoerce files ++ unsafeCoerce suffix))
    <$> relFilePath <*> rootPath

absDirPath :: Parser (Path Abs Dir)
absDirPath =
  (</>) <$> rootPath <*> relDirPath
