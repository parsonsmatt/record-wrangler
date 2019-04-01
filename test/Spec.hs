{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

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

funWithViewPatterns :: Person -> IO ()
funWithViewPatterns (wranglePersonToPerson' -> Person'{..}) = do
    putStrLn personName'
    print personAge'

main :: IO ()
main = do
    putStrLn "The test suite for record-wrangler only requires compilation"
    putStrLn "It demonstrates usage of the record-wrangler library"
