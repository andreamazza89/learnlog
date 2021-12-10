{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- ideas for things to refactor/play with:

-- - create meaningful events that are 'parsed' from the state/raw event. something like FormEvent (exit) | FormEvent (type) | ListEvent (exit)
-- - some kind of abstraction to abstract how to draw a field, be it in view or edit mode

-- things to do:

-- - add remaining fields for LogEntry
-- - new entry to be based on the one that was selected
-- - highlight field red if in error

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

data EntryFieldName = Description | Date | Notes | Length
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
    (const (attrMap Vty.defAttr [(boldAttrName, withStyle Vty.currentAttr Vty.bold)]))

handleEvent :: AppState -> BrickEvent FocusPoint AppEvent -> EventM FocusPoint (Next AppState)
handleEvent state@(AppState entries fstate) event =
  case (fstate, event) of
    (Viewing, VtyEvent (V.EvKey V.KEnter [])) -> continue $ AppState (listMoveTo 0 $ listInsert 0 newEntry entries) (Creating $ mkForm newEntry)
    (Viewing, VtyEvent (V.EvKey V.KEsc [])) -> halt state
    (Viewing, VtyEvent vtyEvent) -> handleListEventVi handleListEvent vtyEvent entries >>= continue . (`AppState` fstate)
    (Creating _, VtyEvent (V.EvKey V.KEsc [])) -> continue (AppState (listRemove 0 entries) Viewing)
    (Creating form, VtyEvent (V.EvKey V.KEnter [])) -> continue $ AppState (listInsert 0 (formState form) . listRemove 0 $ entries) Viewing
    (Creating form, _) -> handleFormEvent event form >>= continue . AppState entries . Creating
    _ -> continue state

newEntry :: LogEntry
newEntry = LogEntry "Ruby" "This IS THE new one" 1.5 (fromGregorian 2021 12 9)

initialState :: AppState
initialState =
  AppState
    (list SomethingElse (fromList logStub) 10)
    Viewing

draw :: AppState -> [AppWidget]
draw state =
  [drawEntries state]

drawEntries :: AppState -> AppWidget
drawEntries AppState {logEntries, entryForm} =
  renderListWithIndex (drawEntry entryForm) True logEntries

drawEntry :: FormState -> Int -> Bool -> LogEntry -> AppWidget
drawEntry (Creating form) 0 _ _ =
  Center.hCenter $
    hLimit 90 $
      Border.border $
        drawForm form
drawEntry (Editing _ _) _ _ _ =
  undefined
drawEntry _ _ isSelected entry =
  Center.hCenter $
    hLimit 90 $
      addBorder $
        vBox
          [ field "description" (description entry),
            field "notes" (notes entry),
            field "length" (pack . show $ time entry),
            field "date" (pack . show $ date entry)
          ]
  where
    addBorder =
      if isSelected
        then withBorderStyle BorderStyle.ascii . Border.border
        else Border.border

mkForm :: LogEntry -> Form LogEntry AppEvent FocusPoint
mkForm =
  Form.newForm
    [ descriptionField,
      notesField,
      lengthField,
      dateField
    ]

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

notesField :: LogEntry -> EntryField FocusPoint
notesField =
  (label "notes" <+>)
    @@= Form.editTextField notesFieldLens (FocusEntryForm Notes) (Just 1)

notesFieldLens :: Lens' LogEntry Text
notesFieldLens =
  lens notes (\logEntry newNotes -> logEntry {notes = newNotes})

lengthField :: LogEntry -> EntryField FocusPoint
lengthField =
  (label "length" <+>)
    @@= Form.editShowableField lengthFieldLens (FocusEntryForm Length)

lengthFieldLens :: Lens' LogEntry Float
lengthFieldLens =
  lens time (\logEntry newLength -> logEntry {time = newLength})

dateField :: LogEntry -> EntryField FocusPoint
dateField =
  (label "date" <+>)
    @@= Form.editShowableField dateFieldLens (FocusEntryForm Date)

dateFieldLens :: Lens' LogEntry Day
dateFieldLens =
  lens date (\logEntry newDate -> logEntry {date = newDate})

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
    notes :: Text,
    time :: Float,
    date :: Day
  }

logStub :: [LogEntry]
logStub =
  [ LogEntry "AWS" "Did this and that" 3.3 (fromGregorian 2021 12 9),
    LogEntry "Haskell" "Did something else" 2 (fromGregorian 2021 12 9)
  ]
