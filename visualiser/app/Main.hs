{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusRing, focusRingCursor)
import Brick.Widgets.List
import qualified Brick.Forms as Form
import Data.Text
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Vector
import Graphics.Vty (defAttr, withBackColor)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color as Color
import Lens.Micro (Lens', lens)

data LogEntry = LogEntry
  { description :: Text,
    date :: Day,
    number :: Integer
  }
  deriving (Show)

data StuffToFocus
  = FormField FormState
  | Other
  | Mother
  | ThaList
  deriving (Eq, Ord, Show)

data FormState = Viewing | Editing FField
  deriving (Eq, Ord, Show)

data FField
  = Description
  | Number
  | Date
  deriving (Eq, Ord, Show)

type MyForm = Form.Form LogEntry () StuffToFocus

data MyState = MyState
  { form :: MyForm,
    focus :: StuffToFocus,
    ring :: FocusRing StuffToFocus,
    l :: GenericList StuffToFocus Vector Int
  }

instance Show MyState where
  show = show . focusGetCurrent . ring

main :: IO ()
main = do
  _ <- defaultMain app (MyState myForm Other (focusRing [Mother, Other, ThaList, FormField Viewing])  (list ThaList (fromList [1, 2, 3]) 4))
  return ()

app :: App MyState () StuffToFocus
app =
  App
    (\s -> [ui s])
    (focusRingCursor ring)
    handleEvent
    return
    (\_ -> attrMap Graphics.Vty.defAttr [(someAttr, withBackColor defAttr Color.brightGreen)])

handleEvent :: MyState -> BrickEvent StuffToFocus () -> EventM StuffToFocus (Next MyState)
handleEvent state@(MyState s focus ring lst) ev =
  case (focusGetCurrent ring, ev) of
    (Just (FormField Viewing), VtyEvent (V.EvKey V.KEnter [])) -> continue (MyState s focus (Form.formFocus s) lst)
    (Just (FormField (Editing _)), VtyEvent (V.EvKey V.KEsc [])) -> continue (MyState s focus (focusRing [FormField Viewing, Mother, Other, ThaList]) lst)
    (Just (FormField (Editing _)), _) -> Form.handleFormEvent ev s >>= (\s' -> continue (MyState s' focus (Form.formFocus s') lst))
    (_, VtyEvent (V.EvKey V.KEsc [])) -> halt state
    (Just ThaList, VtyEvent e) -> handleListEvent e lst >>= (continue . MyState s focus ring)
    (_, VtyEvent (V.EvKey (V.KChar '\t') [])) -> continue (MyState s focus (focusNext ring) lst)
    _ -> continue state

ui :: MyState -> Widget StuffToFocus
ui state@(MyState form focus ring l) =
  vBox
    [ str (show state),
      showCursor (FormField Viewing) (Location (1, 0)) $ Form.renderForm form,
      showCursor Mother (Location (1, 0)) $ str "some stuff",
      showCursor Other (Location (1, 0)) $ str "other stuff",
      someList l
    ]

someAttr :: AttrName
someAttr = attrName "red"

myForm :: MyForm
myForm =
  Form.newForm
    [ numberField,
      descriptionField,
      dateField
    ]
    (LogEntry "desc" (fromOrdinalDate 2021 340) 33)

descriptionField :: LogEntry -> Form.FormFieldState LogEntry e StuffToFocus
descriptionField = Form.editTextField descriptionLens (FormField (Editing Description)) Nothing

descriptionLens :: Lens' LogEntry Text
descriptionLens = lens description (\entry desc -> entry {description = desc})

numberField :: LogEntry -> Form.FormFieldState LogEntry e StuffToFocus
numberField = Form.editShowableField numberLens (FormField (Editing Number))

numberLens :: Lens' LogEntry Integer
numberLens = lens number (\entry num -> entry {number = num})

dateField :: LogEntry -> Form.FormFieldState LogEntry e StuffToFocus
dateField = Form.editShowableField dateLens (FormField (Editing Date))

dateLens :: Lens' LogEntry Day
dateLens = lens date (\entry d -> entry {date = d})


someList :: GenericList StuffToFocus Vector e -> Widget StuffToFocus
someList =
    renderList (\b i -> str (show b)) True
