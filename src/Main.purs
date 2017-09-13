module Main where

import Prelude
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Control.Monad.Eff (Eff)

import OutWatch.Dom.VDomModifier (VDom)
import OutWatch.Tags (div, input, span) as H
import OutWatch.Attributes (text, tpe, inputString, value, childShow, (:=), (<==), (==>))
import OutWatch.Core (VDOM, render) as OutWatch
import OutWatch.Sink (createStringHandler)

import RxJS.Observable (combineLatest, startWith)
    
main :: ∀ e. Eff (vdom :: OutWatch.VDOM | e) Unit
main =
    OutWatch.render "#app" app

    where

    sum :: String -> String -> Int
    sum sx sy =
        (t sx) + (t sy)
        where
        t = fromMaybe 0 <<< fromString

    app :: ∀ f. VDom f
    app =
        let
            xHandler = createStringHandler ["0"]
            yHandler = createStringHandler ["0"]

            state = combineLatest
                sum xHandler.src yHandler.src
                # startWith 0

            mkInput handler =
                H.input
                    [ tpe := "text"
                    , value <== handler.src
                    , inputString ==> handler
                    ]
        in
        H.div
            [ mkInput xHandler
            , H.span [ text " + " ]
            , mkInput yHandler
            , H.span [ text " = " ]
            , H.span [ childShow <== state ]
            ]