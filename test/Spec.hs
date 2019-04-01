{-# LANGUAGE TypeApplications #-}
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
    putStrLn name'
    print age'

data Dog = Dog { dogFriendly :: Bool }

wrangle ''Dog defWrangleOpts
    { addFields = [field "dogAge" ''Int]
    }

convertDog :: Dog -> Int -> Dog'
convertDog = wrangleDogToDog'

data User = User { userId :: Int }

wrangle ''User defWrangleOpts
    { addFields = [field "userName" [t|Char -> String|]]
    }

convertUser :: User -> (Char -> String) -> User'
convertUser = wrangleUserToUser'

main :: IO ()
main = do
    putStrLn "The test suite for record-wrangler only requires compilation"
    putStrLn "It demonstrates usage of the record-wrangler library"
