{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Data.Text (Text, pack)
import Expression
import Monomer
import TextShow

runCalculation :: Text -> Text
runCalculation i = do
  let expr = parseExpression i
  let calced = evalExpression expr
  pack (show calced)

data Calculation = Calculation
  { _ts :: Millisecond,
    _input :: Text,
    _output :: Text
  }
  deriving (Eq, Show)

data AppModel = AppModel
  { _newCalculationInput :: Text,
    _calculations :: [Calculation]
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AddCalculation
  | RemoveCalculation Int
  deriving (Eq, Show)

makeLenses 'Calculation
makeLenses 'AppModel

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    listCalculations idx calculation =
      vstack
        [ label_ (calculation ^. input) [ellipsis] `styleBasic` [textSize 12, paddingH 8],
          spacer,
          hstack
            [ textField_ (calculations . singular (ix idx) . output) [readOnly],
              spacer,
              button "Delete" (RemoveCalculation idx)
            ]
        ]
        `nodeKey` showt (calculation ^. ts)
        `styleBasic` [paddingT 10]

    widgetTree =
      vstack
        [ keystroke [("Enter", AddCalculation)] $
            hstack
              [ label "In:",
                spacer,
                textField_ newCalculationInput [placeholder "Type here..."]
                  `nodeKey` "In:",
                spacer,
                button "Run" AddCalculation
                  `styleBasic` [paddingH 5]
                  `nodeEnabled` (model ^. newCalculationInput /= "")
              ],
          separatorLine `styleBasic` [paddingT 20, paddingB 10],
          vstack (zipWith listCalculations [0 ..] (model ^. calculations))
        ]
        `styleBasic` [padding 20]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AddCalculation
    | model ^. newCalculationInput /= "" ->
        [ Model $
            model
              & newCalculationInput .~ ""
              & calculations .~ newCalculation : model ^. calculations,
          SetFocusOnKey "In:"
        ]
  RemoveCalculation idx ->
    [ Model $
        model
          & calculations .~ removeIdx idx (model ^. calculations)
    ]
  _ -> []
  where
    newCalculation = do
      let user_input = model ^. newCalculationInput
      Calculation (currentTimeMs wenv) user_input (runCalculation user_input)

removeIdx :: Int -> [a] -> [a]
removeIdx idx lst = part1 ++ drop 1 part2
  where
    (part1, part2) = splitAt idx lst

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "HPlot",
        appWindowIcon "./assets/icons/icon.png",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]
    model =
      AppModel
        { _newCalculationInput = "",
          _calculations = []
        }