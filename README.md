# record-wrangler

[![Build Status](https://travis-ci.org/parsonsmatt/record-wrangler.svg?branch=master)](https://travis-ci.org/parsonsmatt/record-wrangler)

This package uses `TemplateHaskell` to alter records in ways that may be convenient for you.

## Inspiration

Let's say you've got a set of API types and a set of domain types.
The API types define your HTTP contract, and you generically derive ToJSON and FromJSON instances from their field labels.
They look like this:

```haskell
data User = User
    { id :: Int
    , name :: String
    , age :: Int
    }

data Dog = Dog
    { id :: Int
    , name :: String
    , toy :: FavoriteToy
    , ownerId :: Int
    }
```

The domain types for your business logic look a little different, though.
The record fields are prefixed with an `_` underscore character, to allow for `lens` derivation.
We can pretend they have other differences that are relevant to internal details and irrelevant for showing off `record-wrangler` if that makes you feel better.

```haskell
data Dog = Dog
    { _id :: Int
    , _name :: String
    , _toy :: FavoriteToy
    , _ownerId :: Int
    }

makeLenses ''Dog

data User = User
    { _id :: Int
    , _name :: String
    , _age :: Int
    }

makeLenses ''User
```

If the record fields were the same, then we could just write the conversion function using `RecordWildCards` and it'd be fine:

```haskell
convertUser Api.User{..} = Domain.User{..}
```

However, because the field labels are different, we have to do it manually:

```haskell
convertUser Api.User{..} = Domain.User
    { _id = id
    , _name = name
    , _age = age 
    }
```

This is annoying and boilerplatey.
I don't know about you, but I hate writing code, for every line of code I write is a chance to mess up.
Let's use `record-wrangler` to make these conversions easier.

```haskell
wrangle ''Api.User defWrangleOpts 
    { fieldLabelModifier = \fieldStr -> '_' : fieldStr 
    }
```

This is going to take the `Api.User` type and modify it slightly.
By default, we append a `'` to the constructor name, type name, and field labels.
Here, we've decided to prepend a `_` character to the field label.
It also generates a function `wrangleUserToUser'` which converts the old record to the modified one.

With the power of view patterns, our conversion function is now quite concise:

```haskell
convertUser :: Api.User -> Domain.User
convertUser (wrangleUserToUser' -> User'{..}) = Domain.User{..}
```
