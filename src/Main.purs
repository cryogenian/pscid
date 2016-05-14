module Main where

import Blessed
import Node.Process as P
import Ansi.Codes (GraphicsParam(Reset), EscapeCode(Graphics), escapeCodeToString, Color(Green))
import Ansi.Output (foreground)
import Control.Bind ((=<<))
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, catchException, EXCEPTION)
import Data.Argonaut (Json, jsonParser, foldJsonObject, (.?))
import Data.Array ((!!))
import Data.Either (either, Either(Right, Left))
import Data.Foldable (elem)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.Options ((:=))
import Data.StrMap (StrMap, keys)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Prelude (id, flip, (<$>), map, pure, unit, const, (<>), bind, Unit)
import PscIde (pursuitCompletion, NET)
import PscIde.Command (PursuitCompletion(PursuitCompletion))
import Pscid.Util (shush, (∘))

type Screens =
  { mainScreen   ∷ Element Screen
  , searchScreen ∷ Element Form
  , resultScreen ∷ Element Form
  }

type BlessEff e = Eff ( bless ∷ BLESS | e)

mkScreens
  ∷ ∀ e
  . Element Screen
  → Element Form
  → Element Form
  → BlessEff e Screens
mkScreens mainScreen searchScreen resultScreen = do
  append mainScreen searchScreen
  append mainScreen resultScreen
  pure {mainScreen, searchScreen, resultScreen}

hideScreens ∷ ∀ e. Screens → BlessEff e Unit
hideScreens {searchScreen, resultScreen} = do
  hide searchScreen
  hide resultScreen

main ∷ ∀ e. Eff ( bless ∷ BLESS, process ∷ P.PROCESS, net ∷ NET, fs ∷ FS | e) Unit
main = do
  s ← screen defaultScreenOptions
  title ← text (defaultTextOptions
                     <> content := Just "PURR"
                     <> top     := Just (colDistance 0)
                     <> height  := Just (colDistance 1))
  append s title
  search ← form (defaultFormOptions
                         <> label  := Just "Pursuit"
                         <> bottom := Just (colDistance 0)
                         <> width  := Just (percentDistance 100)
                         <> height := Just (colDistance 2))
  searchInput ← textbox (defaultTextboxOptions
                           <> bottom := Just (colDistance 0)
                           <> left   := Just (colDistance 2)
                           <> height := Just (colDistance 1))
  append search searchInput
  pursuitResult ← form (defaultFormOptions
                      <> label  := Just "Pursuit Results"
                      <> top := Just (colDistance 2)
                      <> width  := Just (percentDistance 100))
  psList ← pursuitList
  psDetail ← detailView
  append pursuitResult psList
  append pursuitResult psDetail
  screens ← mkScreens s search pursuitResult
  hideScreens screens
  render s

  key s "q" (P.exit 0)
  key s "p" do
    hideScreens screens
    show screens.searchScreen
    clearValue searchInput
    render screens.mainScreen
    readInput searchInput (\i → do
                              runAff' (pursuitCompletion 4243 i) \cs → case cs of
                                Left _ → pure unit
                                Right completions → do
                                  installed ← installedPackages
                                  let items = map (showPC installed) completions
                                  setItems psList items
                                  onScroll psList \ix → do
                                    setContent psDetail (fromMaybe "" (showPrettyPC <$> (completions !! ix)))
                                    render screens.mainScreen
                                  hide screens.searchScreen
                                  show screens.resultScreen
                                  render screens.mainScreen
                                  focus psList
                              hide screens.searchScreen
                              render s)

showPC ∷ Array String → PursuitCompletion → String
showPC installed (PursuitCompletion {type': ident, identifier: modu, module': ty, package}) =
  let
    isInstalled = package `elem` installed
  in
   (if isInstalled then colored Green "I" else " ") <>
   " (" <> package <> ") " <> modu <> "." <> ident

showPrettyPC ∷ PursuitCompletion → String
showPrettyPC (PursuitCompletion {type': ident, identifier: modu, module': ty, package}) =
  "PACKAGE: " <> package <>
  "\nMODULE: " <> modu <>
  "\nIDENTIFIER: " <> ident <>
  "\nTYPE: " <> ty

installedPackages ∷ ∀ e. Eff ( fs ∷ FS | e ) (Array String)
installedPackages = do
  f ← readJsonFile "bower.json"
  case f of
    Nothing → pure []
    Just bowerFile → pure (flip (foldJsonObject []) bowerFile \o →
                              either (const []) id do
                                deps ∷ StrMap String ← o .? "dependencies"
                                pure (keys deps))

runAff' ∷ ∀ e a. Aff e a → (a → Eff e Unit) → Eff e Unit
runAff' a s = runAff (const (pure unit)) s a

detailView ∷ ∀ e. Eff (bless ∷ BLESS | e) (Element Text)
detailView =
  text (defaultTextOptions
        <> top := Just (colDistance 7))

pursuitList ∷ ∀ e. Eff (bless ∷ BLESS | e) (Element (List Unit))
pursuitList =
  list (defaultListOptions
        <> top         := Just (colDistance 1)
        <> height      := Just (colDistance 5)
        <> scrollable  := Just true
        <> width       := Just (percentDistance 100)
        <> interactive := Just true
        <> style       := Just {fg: "blue", bg: "black"})

try ∷ ∀ a e. Eff (err ∷ EXCEPTION | e) a → Eff e (Either Error a)
try action = catchException (pure ∘ Left) (Right <$> action)

tryShush ∷ ∀ a e. Eff (err ∷ EXCEPTION | e) a → Eff e (Maybe a)
tryShush = map shush ∘ try

readJsonFile ∷ ∀ eff. String → Eff ( fs ∷ FS | eff ) (Maybe Json)
readJsonFile path = do
  text ← tryShush (readTextFile UTF8 path)
  pure (shush ∘ jsonParser =<< text)

colored ∷ Color → String → String
colored c s =
  escapeCodeToString (Graphics (foreground c)) <> s <> escapeCodeToString (Graphics [Reset])
