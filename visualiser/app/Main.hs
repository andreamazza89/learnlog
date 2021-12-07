{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Focus (focusRingCursor)
import qualified Brick.Forms as Form
import Data.Text
import Data.Time
import Data.Time.Calendar.OrdinalDate
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
  
data StuffToFocus = FormField | Other

data FormField
  = Description
  | Number
  | Date
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  _ <- defaultMain app myForm
  return ()

app :: App (Form.Form LogEntry () FormField) () FormField
app =
  App
    (\s -> [ui s])
    (focusRingCursor Form.formFocus)
    handleEvent
    return
    (\_ -> attrMap Graphics.Vty.defAttr [(someAttr, withBackColor defAttr Color.brightGreen)])

handleEvent :: Form.Form LogEntry e FormField -> BrickEvent FormField e -> EventM FormField (Next (Form.Form LogEntry e FormField))
handleEvent s ev =
  case ev of
    VtyEvent (V.EvKey V.KEsc []) -> halt s
    _ -> Form.handleFormEvent ev s >>= continue

ui :: Form.Form LogEntry () FormField -> Widget FormField
ui form =
    vBox
      [ str (show $ Form.formState form),
        Form.renderForm form,
        str "other stuff"
      ]

someAttr :: AttrName
someAttr = attrName "red"

myForm :: Form.Form LogEntry () FormField
myForm =
  Form.newForm
    [ numberField,
      descriptionField,
      dateField
    ]
    (LogEntry "desc" (fromOrdinalDate 2021 340) 33)

descriptionField :: LogEntry -> Form.FormFieldState LogEntry e FormField
descriptionField = Form.editTextField descriptionLens Description Nothing

descriptionLens :: Lens' LogEntry Text
descriptionLens = lens description (\entry desc -> entry {description = desc})

numberField :: LogEntry -> Form.FormFieldState LogEntry e FormField
numberField = Form.editShowableField numberLens Number

numberLens :: Lens' LogEntry Integer
numberLens = lens number (\entry num -> entry {number = num})

dateField :: LogEntry -> Form.FormFieldState LogEntry e FormField
dateField = Form.editShowableField dateLens Date

dateLens :: Lens' LogEntry Day
dateLens = lens date (\entry d -> entry {date = d})
