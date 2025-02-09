{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Notebook (main) where

import Control.Exception (catch, try)
import Control.Exception.Base (SomeException (..))
import Control.Lens
import Data.Text (Text, pack)
import Expression
import Monomer

runCalculation :: Text -> [FunctionDouble] -> IO ([FunctionDouble], Text)
runCalculation i context = do
  let parsed = parseStatement i
  case parsed of
    Left expression -> makeEvaluation expression context
    Right function -> return (function : context, pack (show function))
  where
    makeEvaluation :: ExpressionDouble -> [FunctionDouble] -> IO ([FunctionDouble], Text)
    makeEvaluation expr execution_context = do
      let calced = evalFunction expr execution_context
      case calced of
        Left value -> return (context, pack (show value))
        Right err -> return (context, pack (show err))

runCalculationSafe :: Text -> [FunctionDouble] -> IO ([FunctionDouble], Text)
runCalculationSafe i context = do
  calculation_result <- try (runCalculation i context) :: IO (Either SomeException ([FunctionDouble], Text))
  case calculation_result of
    Left e -> return (context, pack (show e))
    Right r -> return r

data Calculation = Calculation
  { _ts_begin :: Millisecond,
    _ts_end :: Millisecond,
    _input :: Text,
    _output :: Text
  }
  deriving (Eq, Show)

data AppModel = AppModel
  { _newCalculationInput :: Text,
    _calculations :: [Calculation],
    _evaluationContext :: [FunctionDouble]
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | StartCalculation Text Millisecond
  | FinishCalculation Calculation [FunctionDouble]
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
            ],
          spacer,
          label_ (pack (show (calculation ^. ts_end - calculation ^. ts_begin) ++ " ms")) [ellipsis] `styleBasic` [textSize 12, paddingH 8]
        ]
        `styleBasic` [paddingT 10]

    widgetTree =
      vstack
        [ keystroke [("Enter", StartCalculation (model ^. newCalculationInput) (currentTimeMs wenv))] $
            hstack
              [ label "In:",
                spacer,
                textField_ newCalculationInput [placeholder "Type here..."]
                  `nodeKey` "In:",
                spacer,
                button "Run" (StartCalculation (model ^. newCalculationInput) (currentTimeMs wenv))
                  `styleBasic` [paddingH 5]
                  `nodeEnabled` (model ^. newCalculationInput /= "")
              ],
          separatorLine `styleBasic` [paddingT 20, paddingB 10],
          vscroll
            ( hstack
                [ vstack (zipWith listCalculations [0 ..] (model ^. calculations)),
                  spacer
                ]
            )
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
  StartCalculation text begin
    | model ^. newCalculationInput /= "" ->
        [ Model $ model & newCalculationInput .~ "",
          SetFocusOnKey "In:",
          Task $ beginCalculations wenv text begin (model ^. evaluationContext)
        ]
  RemoveCalculation idx ->
    [ Model $
        model
          & calculations .~ removeIdx idx (model ^. calculations)
    ]
  FinishCalculation calc new_context ->
    [ Model $ model & calculations .~ calc : model ^. calculations & evaluationContext .~ new_context
    ]
  _ -> []
  where
    beginCalculations :: WidgetEnv AppModel AppEvent -> Text -> Millisecond -> [FunctionDouble] -> IO AppEvent
    beginCalculations env user_input begin context = do
      (new_context, caclulation_result) <- runCalculationSafe user_input context
      return (FinishCalculation (Calculation begin (currentTimeMs env) user_input caclulation_result) new_context)

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
          _calculations = [],
          _evaluationContext = []
        }