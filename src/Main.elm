module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, input, div, h2, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale, Locale)
import FormatNumber.Locales exposing (Decimals(..))

noLocale : Locale
noLocale =
    { usLocale
        | decimals = Exact 0
    }

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

percent : Float
percent = 0.63

employeeTax : Float
employeeTax = 1.141

holidayPercent : Float
holidayPercent = 1.12

g : Float
g = 111447


type alias Model =
  { hourPrice: Int
    , totalDays: Int
    , hoursPerDay: Float
  }


init : Model
init =
  { hourPrice = 1400
    , totalDays = 226
    , hoursPerDay = 7.5
  }



-- UPDATE


type Msg =
  UpdateHourPrice String
  | UpdateTotalDays String
  | UpdateHoursPerDay String


update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateHourPrice price ->
      { model | hourPrice = (Maybe.withDefault 0 (String.toInt price)) }
    UpdateTotalDays days ->
      { model | totalDays = (Maybe.withDefault 0 (String.toInt days)) }
    UpdateHoursPerDay hours ->
      { model | hoursPerDay = (Maybe.withDefault 7.5 (String.toFloat hours)) }

-- VIEW

-- div [] [ text ("Konsulent omsetning: " ++ (renderFloat totalEarnedCalculated)) ]
info : String -> Float -> Html Msg
info label money =
  div [class "info-field"] [
    div [] [text label]
    , div [class "number"] [text <| renderFloat money]
  ]

field : String -> Html Msg -> Html Msg
field label html =
  div [class "input-field"] [
    div [] [text label]
    , html
  ]

renderFloat: Float -> String
renderFloat float =
  (format noLocale (toFloat <| round float)) ++ " kr"

totalEarned: Int -> Int -> Float -> Float
totalEarned hourPrice totalDays hoursPerDay =
 (toFloat hourPrice) * (toFloat totalDays) * hoursPerDay


-- IF (H10>H11) (IF (H10>7.1) (IF (H10>H11) ((H10*G10*1.141*5%)+(3%*((H10-7.1)*G10))*1.141)-(H11*G10*5%*1.141),((H10*G10*1.141*5%)+(3%*((H10-7.1)*G10))*1.141)),IF(H10>H11,(5%*(H10-H11)*G10*1.141),)))""

-- IF(H10>H11,
--   IF(H10>7.1,
--     IF(H10>H11,
--       ((H10*G10*1.141*5%)+(3%*((H10-7.1)*G10))*1.141)-(H11*G10*5%*1.141),
--       ((H10*G10*1.141*5%)+(3%*((H10-7.1)*G10))*1.141)),
--       IF(H10>H11,
--         (5%*(H10-H11)*G10*1.141)
--         ,"")
--     ),"")



pensionShare5to71 : Float -> Float
pensionShare5to71 salary =
 if (salary > (6.0 * g)) then ( ( (Basics.min (7.1 * g) salary) - (6.0 * g)) * 0.05 * employeeTax ) else 0.0

pensionShare71to12 : Float -> Float
pensionShare71to12 salary =
 if (salary > (7.1 * g)) then ( ( (Basics.min (12 * g) salary) - (7.1 * g)) * 0.08 * employeeTax) else 0.0

view : Model -> Html Msg
view model =
  let
    totalEarnedCalculated = totalEarned model.hourPrice model.totalDays  model.hoursPerDay
    consultantsShare = (totalEarnedCalculated * percent)
    consultantsShareWithoutEmployeeTax = (consultantsShare / employeeTax)
    consultantsShareWithoutEmployeeTaxAndHoliday = (consultantsShareWithoutEmployeeTax / holidayPercent)
    holidayMoney = consultantsShareWithoutEmployeeTax - consultantsShareWithoutEmployeeTaxAndHoliday
    -- pensionShare = (pensionShare5to71 consultantsShareWithoutEmployeeTaxAndHoliday) + (pensionShare71to12 consultantsShareWithoutEmployeeTaxAndHoliday)
    pensionShare = (pensionShare5to71 consultantsShare) + (pensionShare71to12 consultantsShare)
  in
  div [class "container"]
    [
      h2 [] [text "Din Skatt"]
    , field "Timepris:" (input [type_ "number", placeholder "Timepris", value (String.fromInt model.hourPrice), onInput UpdateHourPrice] [])
    , field "Fakturerbare dager:" (input [type_ "number", placeholder "Fakturerbare dager", value (String.fromInt model.totalDays), onInput UpdateTotalDays] [])
    , field "Timer per dag:" (input [type_ "number", placeholder "Timer per dag", value (String.fromFloat model.hoursPerDay), onInput UpdateHoursPerDay] [])
    , info "Konsulent omsetning: " totalEarnedCalculated
    , info "Konsulent lønnsandel: " consultantsShare
    , info "Fratrukket arbeidsgiveravgift: " consultantsShareWithoutEmployeeTax
    , info "Fratrukket arbeidsgiveravgift og feriepenger: " consultantsShareWithoutEmployeeTaxAndHoliday
    , info "Feriepenger: " holidayMoney
    , info "Bruttolønn før pensjon/forsikring: " consultantsShareWithoutEmployeeTaxAndHoliday
    , info "Pensjonsfratrekk inkl. arbeidsgiveravgift: " pensionShare
    , div [class "summary"] [
      info "Lønn  utbetalt på ett år: " (consultantsShareWithoutEmployeeTaxAndHoliday - pensionShare)
      , info "Avsatt feriepenger for neste år: " (holidayMoney)
      , info "Antatt snitt månedslønn: " (((consultantsShareWithoutEmployeeTaxAndHoliday - pensionShare))/12)
    ]
    -- , div [] [ text ("sd" ++ (String.fromInt model.hourPrice)) ]
    ]