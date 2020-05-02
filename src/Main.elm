module Main exposing (..)

import Browser
import Decimal as D exposing (Decimal)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { wallet : Decimal
    , caseCost : Decimal
    , keyCost : Decimal
    }


init : ( Model, Cmd Msg )
init =
    ( { wallet = Maybe.withDefault D.zero <| D.fromString "15"
      , caseCost = Maybe.withDefault D.zero <| D.fromString "0.85"
      , keyCost = Maybe.withDefault D.zero <| D.fromString "2.3"
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetWalletAmount String
    | SetCaseCost String
    | SetKeyCost String


emptyToZero : String -> Decimal -> Decimal
emptyToZero s def =
    case s of
        "" ->
            D.zero

        _ ->
            D.fromString s |> Maybe.withDefault def


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWalletAmount s ->
            ( { model | wallet = emptyToZero s model.wallet }, Cmd.none )

        SetCaseCost s ->
            ( { model | caseCost = emptyToZero s model.caseCost }, Cmd.none )

        SetKeyCost s ->
            ( { model | keyCost = emptyToZero s model.keyCost }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        -- the cost of a pair of a case and a key
        -- pair cost = case cost + key cost
        pairCost =
            D.add model.caseCost model.keyCost

        -- the number of keys that can be bought
        -- num keys = wallet `div` pair cost
        numKeys =
            D.fastdiv model.wallet pairCost
                |> Maybe.withDefault D.zero
                |> D.round -2
                |> D.truncate 0

        -- amount left = wallet `mod` pair cost
        amountLeft =
            if D.toFloat numKeys > 0 then
                D.sub model.wallet (D.mul numKeys pairCost)
                    |> D.round -2

            else
                D.fromInt 0

        -- total cost = num keys * pair cost
        totalCost =
            if D.toFloat numKeys > 0 then
                D.mul numKeys pairCost

            else
                D.fromInt 0
    in
    div
        [ class "wrapper" ]
        [ div [ class "inner" ]
            [ div [] [ text "Wallet amount:" ]
            , input [ type_ "number", onInput SetWalletAmount, value <| D.toString model.wallet ] []
            , div [] [ text "Case cost:" ]
            , input [ type_ "number", onInput SetCaseCost, value <| D.toString model.caseCost ] []
            , div [] [ text "Key cost:" ]
            , input [ type_ "number", onInput SetKeyCost, value <| D.toString model.keyCost ] []
            , div [] [ text "Number of keys to buy:" ]
            , input [ type_ "number", disabled True, value <| D.toString numKeys ] []
            , div [] [ text "Total cost:" ]
            , input [ type_ "number", disabled True, value <| D.toString totalCost ] []
            , div [] [ text "Amount left:" ]
            , input [ type_ "number", disabled True, value <| D.toString amountLeft ] []
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
