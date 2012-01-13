{-# LANGUAGE NoImplicitPrelude
           , PackageImports
           , UnicodeSyntax
  #-}

module Types where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Maybe ( Maybe )
import "base" Text.Show ( Show )
import "Cabal" Distribution.ModuleName ( ModuleName )
import "filepath" System.FilePath ( FilePath )


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Target = Library ModuleVisibility | Executable | TestSuite deriving Show
data ModuleVisibility = Exposed | Internal deriving Show
data Module = Module { moduleTargets  ∷ [Target]
                     , moduleName     ∷ ModuleName
                     , moduleFilePath ∷ Maybe FilePath
                     } deriving Show
