{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Forms as Form
import Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style as BorderStyle
import Brick.Widgets.Center as Center
import Brick.Widgets.List as WidgetList
import Data.Text
import Data.Time
import Data.Vector
import Graphics.Vty as V
import Graphics.Vty.Attributes as Vty
import Lens.Micro

data AppState = AppState
  { logEntries :: EntryList,
    entryForm :: FormState
  }

type EntryList = WidgetList.List FocusPoint LogEntry

type AppEvent = ()

data FocusPoint = FocusEntryForm EntryFieldName | SomethingElse
  deriving (Eq, Ord, Show)

data EntryFieldName = Description
  deriving (Eq, Ord, Show)

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
    handleEvent
    return
    ( const
        ( attrMap
            Vty.defAttr
            [ (boldAttrName, withStyle Vty.currentAttr Vty.bold)
            ]
        )
    )

handleEvent :: AppState -> BrickEvent FocusPoint AppEvent -> EventM FocusPoint (Next AppState)
handleEvent state@(AppState entries form) event =
  case event of
    VtyEvent (V.EvKey V.KEsc []) -> halt state
    VtyEvent vtyEvent -> handleListEventVi handleListEvent vtyEvent entries >>= continue . (`AppState` form)
    _ -> continue state

initialState :: AppState
initialState =
  AppState
    (list SomethingElse (fromList logStub) 10)
    Viewing

draw :: AppState -> [AppWidget]
draw state =
  [drawEntries state]

drawEntries :: AppState -> AppWidget
drawEntries AppState {logEntries} =
  renderList drawEntry True logEntries

drawEntry :: Bool -> LogEntry -> AppWidget
drawEntry isSelected entry =
  hCenter $
    hLimit 90 $
      addBorder $
        vBox
          [ field "description" (description entry),
            field "notes" "stubbed notes - waiting to add to the type",
            field "length" "stubbed time - waiting to add to the type",
            field "date" (pack . show $ date entry)
          ]
  where
    addBorder =
      if isSelected
        then withBorderStyle BorderStyle.ascii . Border.border
        else Border.border

drawEntry2 :: a -> b -> AppWidget
drawEntry2 _ _ =
  Center.hCenter $
    Border.border $
      hLimit 90 $
        drawForm stubForm
  where
    stubForm =
      Form.newForm
        [ descriptionField
        ]
        (Prelude.head logStub)

-- Form stuff

data FormState
  = Creating EntryForm
  | Editing Int EntryForm
  | Viewing

type EntryForm = Form.Form LogEntry AppEvent FocusPoint

type EntryField = FormFieldState LogEntry AppEvent

drawForm :: EntryForm -> AppWidget
drawForm = Form.renderForm

descriptionField :: LogEntry -> EntryField FocusPoint
descriptionField =
  (label "description" <+>)
    @@= Form.editTextField descriptionFieldLens (FocusEntryForm Description) (Just 1)

descriptionFieldLens :: Lens' LogEntry Text
descriptionFieldLens =
  lens description (\logEntry newDesc -> logEntry {description = newDesc})

-- Drawing helpers

field :: Text -> Text -> Widget n
field lbl content =
  hBox
    [ label lbl,
      txtWrap content
    ]

label :: Text -> Widget n
label lbl =
  myBold $ txt (lbl <> ": ")

-- Attributes stuff

myBold :: Widget n -> Widget n
myBold =
  withAttr boldAttrName

boldAttrName :: AttrName
boldAttrName =
  attrName "bold"

-- Log data

data LogEntry = LogEntry
  { description :: Text,
    date :: Day
  }

logStub :: [LogEntry]
logStub =
  [ LogEntry "Did this and that" (fromGregorian 2021 12 9),
    LogEntry "Did something else" (fromGregorian 2021 12 9)
  ]
