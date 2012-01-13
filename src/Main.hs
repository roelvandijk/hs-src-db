{-# LANGUAGE DeriveDataTypeable
           , PackageImports
           , TupleSections
           , UnicodeSyntax
  #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Applicative ( (<*>) )
import "base" Control.Monad       ( forM )
import "base" Data.Data           ( Data )
import "base" Data.Function       ( on )
import "base" Data.Functor        ( (<$>) )
import "base" Data.List           ( find, groupBy, sortBy )
import "base" Data.Tuple          ( fst, snd )
import "base" Data.Typeable       ( Typeable )
import "base" Data.Version        ( showVersion )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "Cabal" Distribution.ModuleName as CBL
    ( ModuleName, main, toFilePath )
import qualified "Cabal" Distribution.PackageDescription as CBL
    ( PackageDescription
    , library, executables, testSuites
    , allBuildInfo, usedExtensions
    , Library, exposedModules, libBuildInfo
    , Executable, modulePath, buildInfo
    , TestSuite, testBuildInfo
    , BuildInfo, otherModules, hsSourceDirs
    )
import qualified "Cabal" Distribution.PackageDescription.Parse as CBL
    ( readPackageDescription )
import qualified "Cabal" Distribution.PackageDescription.Configuration as CBL
    ( flattenPackageDescription )
import qualified "Cabal" Distribution.Verbosity as CBL ( normal )
import "cmdargs" System.Console.CmdArgs ( cmdArgs, (&=), def, typFile
                                        , help, verbosity, summary
                                        )
import "directory" System.Directory ( doesFileExist )
import "filepath" System.FilePath
    ( FilePath, (</>), (<.>), dropFileName, dropExtension, normalise )
import qualified "haskell-src-exts" Language.Haskell.Exts.Extension as HSE
    ( Extension, classifyExtension )
import "this" Paths_hs_src_db ( version )
import "this" ProcessModule ( processModule )
import "this" Types


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

data Opts = Opts { package ∷ FilePath
                 } deriving (Show, Data, Typeable)

defOpts ∷ Opts
defOpts = Opts { package = def &= typFile &= help "Cabal package to process"
               }
          &= verbosity
          &= help "Parse haskell code to find reverse function dependencies"
          &= summary ("process-source " ++ showVersion version)

main ∷ IO ()
main = do opts ← cmdArgs defOpts
          processPackage (package opts)


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

haskellExts ∷ [String]
haskellExts = ["hs", "lhs", "hsc"]

processPackage ∷ FilePath → IO ()
processPackage fp = do
    -- TODO: gracefully handle parse failure.
    gpd ← CBL.readPackageDescription CBL.normal fp

    let pd = CBL.flattenPackageDescription gpd
    putStrLn ""
    modules ← packageModules (dropFileName fp) pd
    mapM_ (\m → print $ (moduleTargets m, moduleFilePath m)) modules
    putStrLn ""
    let pkgExts = packageExtensions pd
    mapM_ (\m → do putStr "Processing: "
                   print $ moduleFilePath m
                   processModule pkgExts m
                   putStrLn ""
          )
          modules

packageExtensions ∷ CBL.PackageDescription → [HSE.Extension]
packageExtensions =
    map (HSE.classifyExtension ∘ show)
    ∘ concatMap CBL.usedExtensions
    ∘ CBL.allBuildInfo

packageModules ∷ FilePath → CBL.PackageDescription → IO [Module]
packageModules pkgDir pd = do
    -- Library modules.
    lms ← maybe (return []) (libraryModules pkgDir) (CBL.library pd)
    -- Executable modules.
    ems ← mapM (executableModules pkgDir) (CBL.executables pd)
    -- Test suite modules.
    tms ← mapM (testSuiteModules  pkgDir) (CBL.testSuites  pd)
    return $ groupModuleTargets $ lms ++ concat ems ++ concat tms

groupModuleTargets ∷ [Module] → [Module]
groupModuleTargets = map (\(m:ms) → concatTargets m ms)
                   ∘ groupBy ((≡) `on` moduleName)
                   ∘ sortBy (compare `on` moduleName)
  where
    concatTargets ∷ Module → [Module] → Module
    concatTargets m ms = Module (moduleTargets m ++ concatMap moduleTargets ms)
                                (moduleName m)
                                (moduleFilePath m)

libraryModules ∷ FilePath → CBL.Library → IO [Module]
libraryModules pkgDir lib =
    (++) <$> makeModules pkgDir (CBL.hsSourceDirs bi) [Library Exposed] (CBL.exposedModules lib)
         <*> makeModules pkgDir (CBL.hsSourceDirs bi) [Library Internal] (CBL.otherModules bi)
  where
    bi = CBL.libBuildInfo lib

-- TODO: also process an executable's modulePath.
executableModules ∷ FilePath → CBL.Executable → IO [Module]
executableModules pkgDir exe =
    (:) <$> makeMainModule pkgDir srcDirs [Executable] (CBL.modulePath exe)
        <*> makeModules    pkgDir srcDirs [Executable] (CBL.otherModules bi)
  where
    bi = CBL.buildInfo exe
    srcDirs = CBL.hsSourceDirs bi

testSuiteModules ∷ FilePath → CBL.TestSuite → IO [Module]
testSuiteModules pkgDir tst =
    makeModules pkgDir (CBL.hsSourceDirs bi) [TestSuite] (CBL.otherModules bi)
  where
    bi = CBL.testBuildInfo tst

makeModules ∷ FilePath → [FilePath] → [Target] → [CBL.ModuleName] → IO [Module]
makeModules pkgDir srcDirs targets mods =
    mapM (\n → Module targets n <$> findModulePath pkgDir srcDirs n) mods

makeMainModule ∷ FilePath → [FilePath] → [Target] → FilePath → IO Module
makeMainModule pkgDir srcDirs targets mainFP =
    Module targets CBL.main <$> findModulePath' pkgDir srcDirs (dropExtension mainFP)

-- | Find the location of a module on the filesystem.
--
-- Results in Nothing if the module's file can not be found.
findModulePath ∷ FilePath   -- ^ Package directory.
               → [FilePath] -- ^ Source directories relative to the package directory.
               → CBL.ModuleName
               → IO (Maybe FilePath)
findModulePath pkgDir srcDirs m = findModulePath' pkgDir srcDirs $ CBL.toFilePath m

findModulePath' ∷ FilePath   -- ^ Package directory.
                → [FilePath] -- ^ Source directories relative to the package directory.
                → FilePath   -- ^ Module filepath
                → IO (Maybe FilePath)
findModulePath' pkgDir srcDirs mfp =
    (fmap (normalise ∘ fst) ∘ find snd)
    <$> ( -- Try every combination of source directory and haskell
          -- file extension.
          forM [(d, e) | d ← srcDirs, e ← haskellExts]
               $ \(d, e) → let fp = pkgDir </> d </> mfp <.> e
                           in (fp,) <$> doesFileExist fp
        )
