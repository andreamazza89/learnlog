{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.List as WidgetList
import Data.Vector
import Graphics.Vty.Attributes as Vty

newtype AppState = AppState EntryList

type EntryList = WidgetList.List FocusPoint Int

type AppEvent = ()

type FocusPoint = ()

type AppWidget = Widget FocusPoint

main :: IO ()
main = do
  _ <- defaultMain app initialState
  return ()

app :: App AppState () FocusPoint
app =
  App
    draw
    showFirstCursor
    (\s _ -> halt s)
    return
    (const (attrMap Vty.defAttr []))

initialState :: AppState
initialState =
  AppState (list () (fromList [1, 2]) 10)

draw :: AppState -> [AppWidget]
draw (AppState lst) =
  [renderList (\_ _ -> str "list item") True lst]
