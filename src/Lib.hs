module Lib where

import qualified Data.Map as M

import           Control.Arrow
import           Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Language.Javascript.JSaddle
       (jsg, js, js1, jss, fun, valToNumber, syncPoint)
import           Language.Javascript.JSaddle.Warp (run)
import           Miso
import           Miso.String (MisoString, pack, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)
import           Touch


main :: IO ()
main = run 3709 $ do
    doc <- jsg "document"
    doc ^. js "body" ^. jss "innerHTML" "<h1>Kia ora (Hi)</h1>"

    -- Create a haskell function call back for the onclick event
    doc ^. jss "onclick" (fun $ \ _ _ [e] -> do
        x <- e ^. js "clientX" >>= valToNumber
        y <- e ^. js "clientY" >>= valToNumber
        newParagraph <- doc ^. js1 "createElement" "p"
        newParagraph ^. js1 "appendChild" (
            doc ^. js1 "createTextNode" ("Click " ++ show (x, y)))
        doc ^. js "body" ^. js1 "appendChild" newParagraph
        return ())

    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    exit <- doc ^. js1 "createElement" "span"
    exit ^. js1 "appendChild" (
        doc ^. js1 "createTextNode" "Click here to exit")
    doc ^. js "body" ^. js1 "appendChild" exit
    exit ^. jss "onclick" (fun $ \ _ _ _ -> liftIO $ putMVar exitMVar ())

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    doc ^. js "body" ^. jss "innerHTML" "<h1>Ka kite ano (See you later)</h1>"
    return ()




-- trunc = truncate *** truncate

-- main :: IO ()
-- main = startApp App {..}
--   where
--     initialAction = Id
--     model         = emptyModel
--     update        = updateModel
--     view          = viewModel
--     events        = M.insert (pack "mousemove") False $
--                     M.insert (pack "touchstart") False $
--                     M.insert (pack "touchmove") False defaultEvents
--     subs          = [ mouseSub HandleMouse ]
--     mountPoint    = Nothing

emptyModel :: Model
emptyModel = Model (0,0)

-- updateModel :: Action -> Model -> Effect Action Model
-- updateModel (HandleTouch (TouchEvent touch)) model =
--   model <# do
--     putStrLn "Touch did move"
--     print touch
--     return $ HandleMouse $ trunc . page $ touch
-- updateModel (HandleMouse newCoords) model =
--   noEff model { mouseCoords = newCoords }
-- updateModel Id model = noEff model

data Action
  = HandleMouse (Int, Int)
  | HandleTouch TouchEvent
  | Id

newtype Model
  = Model
  { mouseCoords  :: (Int, Int)
  } deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel (Model (x,y)) =
  div_ [ ] [
    svg_ [ style_ $ M.fromList [ ("border-style", "solid")
                               , ("height", "700px")
                               ]
         , width_ "auto"
         , onTouchMove HandleTouch
       ] [
     g_ [] [
     ellipse_ [ cx_ $ ms x
              , cy_ $ ms y
              , style_ svgStyle
              , rx_ "100"
              , ry_ "100"
              ] [ ]
     ]
     , text_ [ x_ $ ms x
             , y_ $ ms y
             ] [ text $ ms $ show (x,y) ]
   ]
 ]

svgStyle :: M.Map MisoString MisoString
svgStyle =
  M.fromList [
      ("fill", "yellow")
    , ("stroke", "purple")
    , ("stroke-width", "2")
    ]
