{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Data.Char (isSpace, isDigit)

trim :: Chars -> Chars
trim = f . f
   where f = reverse . dropWhile isSpace


-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const "" , const "un" , const "do" , const "tre" , const "quattuor" , const "quin" , const "sex" , const "septen" , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion" , "trigintillion" , "quadragintillion" , "quinquagintillion" , "sexagintillion" , "septuagintillion" , "octogintillion" , "nonagintillion" , "centillion" , "decicentillion" , "viginticentillion" , "trigintacentillion" , "quadragintacentillion" , "quinquagintacentillion" , "sexagintacentillion" , "septuagintacentillion" , "octogintacentillion" , "nonagintacentillion" , "ducentillion" , "deciducentillion" , "vigintiducentillion" , "trigintaducentillion" , "quadragintaducentillion" , "quinquagintaducentillion" , "sexagintaducentillion" , "septuagintaducentillion" , "octogintaducentillion" , "nonagintaducentillion" , "trecentillion" , "decitrecentillion" , "vigintitrecentillion" , "trigintatrecentillion" , "quadragintatrecentillion" , "quinquagintatrecentillion" , "sexagintatrecentillion" , "septuagintatrecentillion" , "octogintatrecentillion" , "nonagintatrecentillion" , "quadringentillion" , "deciquadringentillion" , "vigintiquadringentillion" , "trigintaquadringentillion" , "quadragintaquadringentillion" , "quinquagintaquadringentillion" , "sexagintaquadringentillion" , "septuagintaquadringentillion" , "octogintaquadringentillion" , "nonagintaquadringentillion" , "quingentillion" , "deciquingentillion" , "vigintiquingentillion" , "trigintaquingentillion" , "quadragintaquingentillion" , "quinquagintaquingentillion" , "sexagintaquingentillion" , "septuagintaquingentillion" , "octogintaquingentillion" , "nonagintaquingentillion" , "sescentillion" , "decisescentillion" , "vigintisescentillion" , "trigintasescentillion" , "quadragintasescentillion" , "quinquagintasescentillion" , "sexagintasescentillion" , "septuagintasescentillion" , "octogintasescentillion" , "nonagintasescentillion" , "septingentillion" , "deciseptingentillion" , "vigintiseptingentillion" , "trigintaseptingentillion" , "quadragintaseptingentillion" , "quinquagintaseptingentillion" , "sexagintaseptingentillion" , "septuagintaseptingentillion" , "octogintaseptingentillion" , "nonagintaseptingentillion" , "octingentillion" , "decioctingentillion" , "vigintioctingentillion" , "trigintaoctingentillion" , "quadragintaoctingentillion" , "quinquagintaoctingentillion" , "sexagintaoctingentillion" , "septuagintaoctingentillion" , "octogintaoctingentillion" , "nonagintaoctingentillion" , "nongentillion" , "decinongentillion" , "vigintinongentillion" , "trigintanongentillion" , "quadragintanongentillion" , "quinquagintanongentillion" , "sexagintanongentillion" , "septuagintanongentillion" , "octogintanongentillion" , "nonagintanongentillion"
        ]
  in listh [
       "" , "thousand" , "million" , "billion" , "trillion" , "quadrillion" , "quintillion" , "sextillion" , "septillion" , "octillion" , "nonillion" , "decillion" , "undecillion" , "duodecillion" , "tredecillion" , "quattuordecillion" , "quindecillion" , "sexdecillion" , "septendecillion" , "octodecillion" , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Eq, Ord)

showDigit :: Digit -> Chars
showDigit Zero = "zero"
showDigit One = "one"
showDigit Two = "two"
showDigit Three = "three"
showDigit Four = "four"
showDigit Five = "five"
showDigit Six = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine = "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _ = Empty

toInt :: Digit -> Int
toInt Zero = 0
toInt One = 1
toInt Two = 2
toInt Three = 3
toInt Four = 4
toInt Five = 5
toInt Six = 6
toInt Seven = 7
toInt Eight = 8
toInt Nine = 9

teens :: Digit -> Chars
teens Zero  = "ten"
teens One   = "eleven"
teens Two   = "twelve"
teens Three = "thirteen"
teens Four  = "fourteen"
teens Five  = "fifteen"
teens Six   = "sixteen"
teens Seven = "seventeen"
teens Eight = "eighteen"
teens Nine  = "nineteen"

showTens :: Digit -> Chars
showTens Zero   = ""
showTens One    = "ten"
showTens Two    = "twenty"
showTens Three  = "thirty"
showTens Four   = "forty"
showTens Five   = "fifty"
showTens Six    = "sixty"
showTens Seven  = "seventy"
showTens Eight  = "eighty"
showTens Nine   = "ninety"

stripZero :: Chars -> Chars
stripZero "zero" = ""
stripZero x = x

data Digit3 =
  D1 Digit Int
  | D2 Digit Digit Int
  | D3 Digit Digit Digit Int
  deriving Eq

chooseWord :: (Num n, Ord n) => n -> Chars
chooseWord n =
  let x = (headOr "" (drop n illion))
  in if x == "" then "" else " " ++ x ++ " "

displaySingleDigit :: List Char -> Digit -> (List Char, Bool)
displaySingleDigit _ Zero = ("", True)
displaySingleDigit word One = (showDigit One ++ word, False)
displaySingleDigit word ch = (showDigit ch ++ word, True)

wordify :: List Char -> Int -> Optional (List Digit3)
wordify (ones :. tens :. hundreds :. rest) d = do
  oP <- fromChar ones
  tP <- fromChar tens
  hP <- fromChar hundreds
  (flip (++) ((D3 hP tP oP d) :. Nil)) <$> wordify rest (d+1)
wordify (ones :. tens :.  _) d = do
  oP <- fromChar ones
  tP <- fromChar tens
  return (D2 tP oP d :. Nil)
wordify (ones :. Nil) d = do
  oP <- fromChar ones
  return (D1 oP d :. Nil)
wordify Nil _ = Full Nil

-- >>> doDollars "1234"
printDollars :: Digit3 -> Chars
printDollars (D1 ones d) = showDigit ones ++ chooseWord d
printDollars (D2 One ones d) = teens ones ++ chooseWord d
printDollars (D2 Zero Zero _) = ""
printDollars (D2 tens Zero d) = showTens tens ++ chooseWord d
printDollars (D2 Zero ones d) = printDollars (D1 ones d)
printDollars (D2 tens ones d) = showTens tens ++ "-" ++ printDollars (D1 ones d)
printDollars (D3 Zero Zero Zero _) = ""
printDollars (D3 Zero tens ones d) =  printDollars (D2 tens ones d)
printDollars (D3 hundreds Zero Zero d) = showDigit hundreds ++ " hundred" ++ chooseWord d
printDollars (D3 hundreds tens ones d) = showDigit hundreds ++ " hundred and " ++ printDollars (D2 tens ones d)

-- >>> doDollars "1211560"
doDollars :: Chars -> Optional Chars
doDollars hs =
  let ds = wordify (reverse hs) 0
  in (\s ->
        let result = flatMap printDollars s
        in if result == "" then "zero" else result) <$> ds

-- >>> doCents "1"
doCents :: Chars -> Optional Chars
doCents (tens :. Nil) = doDollars (tens :. '0' :. Nil)
doCents (tens :. ones :. _) = doDollars (tens :. ones :. Nil)
doCents _ = Empty

removeNonDigit :: List Char -> List Char
removeNonDigit = filter (\x -> isDigit x || (x=='.'))

removeExtraDots :: List Char -> List Char
removeExtraDots = filter isDigit

-- >>> dollars "9abc9def9ghi.jkl9mno"
-- >>> dollars "12020.6130"
dollars :: Chars -> Chars
dollars i =
  let input = removeNonDigit i
      (whole, frac) = break (== '.') input
      x = map fromChar whole
      cents = doCents (removeExtraDots frac) ?? "zero"
      centsName = if cents == "one" then " cent" else " cents"
      dolls = doDollars whole ?? "zero"
      dollarsName = if dolls == "one" then " dollar" else " dollars"
  in (trim dolls) ++  dollarsName ++ " and " ++ (trim cents) ++ centsName

