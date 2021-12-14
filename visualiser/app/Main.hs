{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- ideas for things to refactor/play with:

-- - create meaningful events that are 'parsed' from the state/raw event. something like FormEvent (exit) | FormEvent (type) | ListEvent (exit)
-- - some kind of abstraction to abstract how to draw a field, be it in view or edit mode
-- - would it be possible to rewrite handleFormEvent that just returns the new Form rather than the EventM?

-- things to do:

-- v add remaining fields for LogEntry
-- v highlight field red if in error
-- v new entry to be based on the one that was selected
-- v disable 'creating' or 'saving' an entry when the form is in error
-- - add tags (comma separated?)
-- - try abstract Form and List Widgets, so that if one day I rewrite my own then it'll be easier to migrate
-- - figure out a way to have a faster feedback loop (build/exec)
-- - try ad a popup ? like something around 'are you sure you wanna quit'?

module Main where

import Brick
import Brick.Forms as Form
import Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style as BorderStyle
import Brick.Widgets.Center as Center
import Brick.Widgets.List as WidgetList
import Data.Aeson
import Data.ByteString.Lazy as ByteString
import Data.Maybe (fromMaybe)
import Data.Text as Text
import Data.Time
import Data.Vector
import Graphics.Vty as V
import Graphics.Vty.Attributes as Vty
import Lens.Micro

data AppState = AppState
  { logEntries :: EntryList,
    entryForm :: FormState,
    today :: Day
  }

type EntryList = WidgetList.List FocusPoint LogEntry

type AppEvent = ()

data FocusPoint = FocusEntryForm EntryFieldName | SomethingElse
  deriving (Eq, Ord, Show)

data EntryFieldName = Description | Date | Notes | Length | Tags
  deriving (Eq, Ord, Show)

type AppWidget = Widget FocusPoint

logFileName :: FilePath
logFileName = "../log.json"

main :: IO ()
main = do
  entries <- readEntries
  today <- utctDay <$> getCurrentTime
  finalState <- defaultMain app $ initialState today entries
  encodeFile logFileName $ listElements (logEntries finalState)

readEntries :: IO [LogEntry]
readEntries = do
  entries <- eitherDecode <$> ByteString.readFile logFileName
  case entries of
    Right e -> return e
    Left e -> fail e

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
            [ (boldAttrName, withStyle Vty.currentAttr Vty.bold),
              (invalidFormInputAttr, fg $ ISOColor 1)
            ]
        )
    )

-- a nicer language for events to be handled

data MyEvent
  = ListEvent ListEvent
  | FormEvent FormEvent
  | UninterestingEvent

data ListEvent
  = CreateNewEntry
  | ExitProgram
  | UpdateList Event

data FormEvent
  = ExitForm
  | SaveEntry EntryForm
  | UpdateForm EntryForm

toMyEvent :: FormState -> BrickEvent FocusPoint AppEvent -> MyEvent
toMyEvent fstate event =
  case (fstate, event) of
    (Viewing, VtyEvent (V.EvKey V.KEnter [])) -> ListEvent CreateNewEntry
    (Viewing, VtyEvent (V.EvKey V.KEsc [])) -> ListEvent ExitProgram
    (Viewing, VtyEvent vtyEvent) -> ListEvent (UpdateList vtyEvent)
    (Creating _, VtyEvent (V.EvKey V.KEsc [])) -> FormEvent ExitForm
    (Creating f, VtyEvent (V.EvKey V.KEnter [])) -> FormEvent (SaveEntry f)
    (Creating f, _) -> FormEvent (UpdateForm f)
    _ -> UninterestingEvent

handleEvent :: AppState -> BrickEvent FocusPoint AppEvent -> EventM FocusPoint (Next AppState)
handleEvent state@(AppState entries fstate today) event =
  case toMyEvent fstate event of
    ListEvent CreateNewEntry -> continue $ createNewEntry entries state
    ListEvent ExitProgram -> halt state
    ListEvent (UpdateList vtyEvent) -> handleListEventVi handleListEvent vtyEvent entries >>= \entries' -> continue $ AppState entries' fstate today
    FormEvent ExitForm -> continue (AppState (listRemove 0 entries) Viewing today)
    FormEvent (SaveEntry f) -> continue $ saveEntry today f entries
    FormEvent (UpdateForm f) -> handleFormEvent event f >>= \f' -> continue $ AppState entries (Creating f') today
    UninterestingEvent -> continue state

createNewEntry :: EntryList -> AppState -> AppState
createNewEntry entries currentState =
  fromMaybe
    currentState
    ( do
        (_, selectedEntry) <- listSelectedElement entries
        let newEntry = withToday selectedEntry
        return $ AppState (selectFirstItem . addAtTop newEntry $ entries) (Creating $ mkForm newEntry) (today currentState)
    )
  where
    withToday entry = entry {date = today currentState}
    addAtTop = listInsert 0
    selectFirstItem = listMoveTo 0

saveEntry :: Day -> EntryForm -> EntryList -> AppState
saveEntry today' f entries =
  if Form.allFieldsValid f
    then
      AppState
        ( listInsert 0 (formState f)
            . listRemove 0
            $ entries
        )
        Viewing
        today'
    else AppState entries (Creating f) today'

initialState :: Day -> [LogEntry] -> AppState
initialState day entries =
  AppState (list SomethingElse (fromList entries) 10) Viewing day

draw :: AppState -> [AppWidget]
draw state =
  [drawEntries state]

drawEntries :: AppState -> AppWidget
drawEntries AppState {logEntries, entryForm} =
  renderListWithIndex (drawEntry entryForm) True logEntries

drawEntry :: FormState -> Int -> Bool -> LogEntry -> AppWidget
drawEntry (Creating form) _ True _ =
  Center.hCenter $
    hLimit 90 $
      Border.border $
        drawForm form
drawEntry (Editing _ _) _ _ _ =
  undefined -- not supported yet
drawEntry _ _ isSelected entry =
  Center.hCenter $
    hLimit 90 $
      addBorder $
        vBox
          [ field "description" (description entry),
            field "notes" (notes entry),
            field "length" (Text.pack . show $ time entry),
            field "date" (Text.pack . show $ date entry),
            field "tags" (Text.pack . show $ tags entry)
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
      dateField,
      tagsField
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

tagsField :: LogEntry -> EntryField FocusPoint
tagsField =
  (label "tags" <+>)
    @@= Form.editShowableField tagsFieldLens (FocusEntryForm Tags)

tagsFieldLens :: Lens' LogEntry [Tag]
tagsFieldLens =
  lens tags (\logEntry newTags -> logEntry {tags = newTags})

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
    date :: Day,
    tags :: [Tag]
  }

type Tag = Text

instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \entryJson ->
    LogEntry
      <$> entryJson .: "description"
      <*> entryJson .: "notes"
      <*> entryJson .: "lengthInHours"
      <*> entryJson .: "date"
      <*> entryJson .: "tags"

instance ToJSON LogEntry where
  toJSON entry =
    object
      [ "description" .= description entry,
        "notes" .= notes entry,
        "lengthInHours" .= time entry,
        "date" .= date entry,
        "tags" .= tags entry
      ]

--  mmm these two above should be isomorphic - maybe I can add property testing?

