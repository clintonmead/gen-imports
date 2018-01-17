{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution.Hackage.Imports.Generate where

import Data.ByteString (ByteString)

import Distribution.Hackage.DB (Hackage, Version, readHackage)
import Distribution.PackageDescription (
  GenericPackageDescription(packageDescription, condLibrary),
  PackageDescription(library),
  Library(exposedModules),
  CondTree(CondNode, condTreeData)
  )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName
import Distribution.Version (Version(versionBranch))
import Distribution.Text (disp)
import qualified Data.Map.Lazy
import Control.Arrow (second)
import Text.PrettyPrint (Doc, text, ($+$), render)
import GHC.Exts (sortWith, Down(Down))
import Data.Monoid ((<>), mempty)
import Data.Foldable (foldl')
import System.FilePath (FilePath, (</>))
import Data.String (fromString)
import Data.Char (toUpper)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Debug.Trace (traceShowId)

type PackageName = String

class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

stringToFilePath :: String -> FilePath
stringToFilePath = id

getExposedModules :: GenericPackageDescription -> [ModuleName]
getExposedModules gpd = let
  libs1 = case library (packageDescription gpd) of
    Nothing -> []
    Just l -> exposedModules l
  libs2 = case condLibrary gpd of
    Nothing -> []
    Just (CondNode{condTreeData=l}) -> exposedModules l
  in
    libs1 ++ libs2

getExposedModulesAllVersions :: Hackage -> PackageName -> [(Version, [ModuleName])]
getExposedModulesAllVersions hackage pn = case Data.Map.Lazy.lookup pn hackage of
  Nothing -> []
  Just gpdm -> (second getExposedModules) <$> Data.Map.Lazy.toList gpdm

genImportList :: (ModuleName -> Doc) -> ModuleName -> PackageName -> [(Version, [ModuleName])] -> Doc
genImportList f moduleName packageName l = languageCpp $+$ moduleHeader $+$ guard $+$ body $+$ endIf $+$ "#endif" where
  languageCpp = "{-# LANGUAGE CPP #-}"
  guard = "#ifdef MIN_VERSION_" <> packageNameDoc
  packageNameDoc = text (replace_dash <$> toString packageName)
  moduleHeader = "module " <> disp moduleName <> " where"
  endIf = case l of
    [] -> mempty
    _ -> "#endif"
  body = (foldl' ($+$) mempty $ zipWith g ("#if":(repeat "#elif")) (sortWith Down l))
  g macro (version, module_list) = preprocessorLine $+$ modules where
    preprocessorLine = macro <> " MIN_VERSION_" <> packageNameDoc <> "(" <> v1 <> "," <> v2 <> "," <> v3 <> ")"
    modules = foldl' ($+$) mempty (f <$> module_list)
    [v1, v2, v3] = (text . show) <$> (take 3 (versionBranch version ++ [0..]))


genImportListFromHackage :: Hackage -> ModuleName -> PackageName -> Doc
genImportListFromHackage hackage moduleName packageName = genImportList toImportLine moduleName packageName (getExposedModulesAllVersions hackage packageName) where

toImportLine :: ModuleName -> Doc
toImportLine mn = "import " <> disp mn <> " ()"

writeImportListToFile :: Hackage -> FilePath -> [String] -> PackageName  -> IO ()
writeImportListToFile hackage basePath baseModuleStrList packageName =
  writeFile moduleFileName $ render (genImportListFromHackage hackage moduleName packageName) where
    capitalisedPackageName = let (s:ss) = toString packageName in fromString (((toUpper s):(replace_dash <$> ss)))
    capitalisedPackageFileName = capitalisedPackageName <> ".hs"
    moduleFileName = (foldl' (</>) basePath (map stringToFilePath baseModuleStrList)) </> capitalisedPackageFileName
    moduleName = Distribution.ModuleName.fromString ((intercalate "." baseModuleStrList) <> "." <> capitalisedPackageName)

replace_dash :: Char -> Char
replace_dash c = case c of
  '-' -> '_'
  x -> x

writeImportListsToDir :: Foldable f => FilePath -> [String] -> f PackageName -> IO ()
writeImportListsToDir basePath baseModuleStrList packageNames = do
  hackage <- readHackage
  traverse_ (writeImportListToFile hackage basePath baseModuleStrList) packageNames

writeGHCPackageImportListsToDir :: FilePath -> [String] -> IO ()
writeGHCPackageImportListsToDir basePath baseModuleStrList = writeImportListsToDir basePath baseModuleStrList ghcPackages

ghcPackages :: [PackageName]
ghcPackages = [
  "array",
  "base",
  "binary",
  "bytestring",
  "Cabal",
  "containers",
  "deepseq",
  "directory",
  "filepath",
  "ghc",
  "ghc-boot",
  "ghc-compact",
  "ghc-prim",
  "haskell98",
  "haskell2010",
  "hoopl",
  "hpc",
  "old-locale",
  "old-time",
  "integer-gmp",
  "process",
  "template-haskell",
  "time",
  "unix",
  "Win32"
  ]

{-
getExposedModulesAllVersionsIO :: PackageName -> IO [(Version, [ModuleName])]
getExposedModulesAllVersionsIO pn = do
  hackage <- readHackage
  pure $ getExposedModulesAllVersions hackage pn
-}
