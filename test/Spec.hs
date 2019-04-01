{-# LANGUAGE TemplateHaskell #-}
import RecordWrangler

data Person = Person { name :: String, age :: Int }

wrangle ''Person defWrangleOpts

personWrangled :: Person'
personWrangled = Person'
    { name' = "Fred"
    , age' = 35
    }

converted :: Person -> Person'
converted = wranglePersonToPerson'

main :: IO ()
main = putStrLn "The test suite for record-wrangler only requires compilation"
