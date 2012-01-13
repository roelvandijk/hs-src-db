{-# LANGUAGE DeriveDataTypeable
           , PackageImports
           , TupleSections
           , UnicodeSyntax
  #-}

module ProcessModule where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Arrow ( first )
import "base" Data.Either   ( partitionEithers )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "haskell-src-exts" Language.Haskell.Exts.Extension
    ( Extension, classifyExtension )
import "haskell-src-exts" Language.Haskell.Exts.Parser
    ( ParseMode, parseFilename, extensions
    , defaultParseMode
    , ParseResult(ParseOk, ParseFailed)
    , getTopPragmas, parseModuleWithMode
    )
import "haskell-src-exts" Language.Haskell.Exts.Pretty ( prettyPrint )
import "haskell-src-exts" Language.Haskell.Exts.Syntax
    ( ModulePragma(LanguagePragma)
    , Name(Ident, Symbol)
    )
import qualified "strict" System.IO.Strict as S ( readFile )
import "this" Types


--------------------------------------------------------------------------------
-- Process module
--------------------------------------------------------------------------------

processModule ∷ [Extension] → Module → IO ()
processModule pkgExts module' =
    maybe (putStrLn "Can't locate module on filesystem.")
          processModule'
          (moduleFilePath module')
  where
    processModule' ∷ FilePath → IO ()
    processModule' fp = do
      moduleStr ← S.readFile fp
      case getTopPragmas moduleStr of
        ParseFailed srcLoc err → putStrLn $ (prettyPrint srcLoc) ++ ": " ++ err
        ParseOk ps → do
          let (es, _) = pragmasToExts ps
              mode = defaultParseMode { parseFilename = fp
                                      , extensions = pkgExts ++ es
                                      }
          -- TODO: If the CPP extension is enabled, run
          -- "cpphs" Language.Preprocessor.Cpphs.runCpphs
          case parseModuleWithMode mode moduleStr of
            ParseFailed srcLoc err → putStrLn $ (prettyPrint srcLoc) ++ ": " ++ err
            ParseOk m → putStrLn "Parsing succesfull!"


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

-- Convert a list of parsed module pragmas to extensions.
-- The resulting list of pragmas are those that can not be converted.
pragmasToExts ∷ [ModulePragma] → ([Extension], [ModulePragma])
pragmasToExts = first concat ∘ partitionEithers ∘ map convertPragma
    where
      convertPragma ∷ ModulePragma → Either [Extension] ModulePragma
      convertPragma (LanguagePragma _ ns) = Left $ map nameToExt ns
      convertPragma p                     = Right p

      nameToExt ∷ Name → Extension
      nameToExt (Ident  str) = classifyExtension str
      nameToExt (Symbol str) = classifyExtension str
