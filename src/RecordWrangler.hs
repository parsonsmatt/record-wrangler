{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | This module contains a Template Haskell helper to produce a new datatype
-- with modified field names. The initial use case is to allow for easier record
-- construction with Lumi's databases models, which have record fields prefixed
-- with an `_`, and the Stats records, which do not have this underscore. The
-- use of a naming scheme convention allows one to write the conversion function
-- as:
--
-- > convertData (Entity id Old.Record{..}) RecordStats{..} =
-- >   Entity (coerce id) New.Record
-- >     { ..
-- >     -- Some fields need massaging
-- >     , _recordClientId = coerce _recordClientId
-- >     -- Some fields don't need massaging, but need to be explicitly labeled.
-- >     , _recordStatsFoo = recordStatsFoo
-- >     }
--
-- where each field in @RecordStats@ must be repeated. This can be accomplished
-- fairly easily with a vim macro, but it's more fun and less error prone to
-- write Haskell.
--
-- With this module, we can instead write:
--
-- > wrangle ''RecordStats with { fieldLabelModifier = ('_' :) }
--
-- which generates a new type @RecordStats'@ with the same fields, but modified
-- to have different field labels. It also creates a conversion function. Now,
-- we can write (with @ViewPatterns@):
--
-- > convertData
-- >   (Entity id Old.Record{..})
-- >   (wrangleRecordStatsToRecordStats' -> RecordStats'{..})
-- >  =
-- >   Entity (coerce id) New.Record
-- >     { ..
-- >     , _recordClientId = coerce _recordClientId
-- >     }
--
-- Now, the only terms that need to be mentioned are the ones that cause
-- a compile-time error due to the types not matching up.
module RecordWrangler
    ( -- * The Wranglin One
      wrangle
      -- * The Options For Wranglin
    , WrangleOpts
    , defWrangleOpts
    , fieldLabelModifier
    , constructorModifier
    , typeNameModifier
    ) where

import           Data.Traversable
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

-- | The options for wrangling records. The constructor is hidden so that
-- we can add new features and powers without breaking your code!
data WrangleOpts = WrangleOpts
    { fieldLabelModifier :: String -> String
    -- ^ This function will be applied to every field label in the provided
    -- record.
    --
    -- @since 0.1.0.0
    , typeNameModifier :: String -> String
    -- ^ This function will be applied to the type name.
    --
    -- @since 0.1.0.0
    , constructorModifier :: String -> String
    -- ^ This function will be applied to the constructor name.
    --
    -- @since 0.1.0.0
    }

-- | This is the default set of 'WrangleOpts'. It affixes a @'@ character to
-- the end of the fields, type, and constructor. If you want different behavior,
-- then you will want to alter the fields:
--
-- @
-- wrangle ''Record defWrangleOpts { fieldLabelModifier = ('_' :) }
-- @
--
-- @since 0.1.0.0
defWrangleOpts :: WrangleOpts
defWrangleOpts = WrangleOpts
    { fieldLabelModifier = (++ "'")
    , typeNameModifier = (++ "'")
    , constructorModifier = (++ "'")
    }

-- | Create a new datatype with altered field labels, type name, and constructor
-- names along with a conversion function.
--
-- The conversion function will have a name matching the pattern:
--
-- > wrangle + OldTypeName + To + NewTypeName
--
-- As an example, consider the following datatype and wrangling:
--
-- > data Person = Person { name :: String, age :: Int }
-- >
-- > 'wrangle' ''Person 'with'
-- >   { 'fieldLabelModifier' = ('_' :)
-- >   , 'typeNameModifier' = ("Powerful" ++)
-- >   }
--
-- This has the effect of creating this new datatype and function:
--
-- > data PowerfulPerson = Person' { _name :: String, _age :: Int }
-- >
-- > wranglePersonToPowerfulPerson :: Person -> PowerfulPerson
-- > wranglePersonToPowerfulPerson (Person x0 x1) = Person' x0 x1
--
-- @since 0.1.0.0
wrangle :: Name -> WrangleOpts -> DecsQ
wrangle tyName WrangleOpts {..} = do
    TyConI theDec <- reify tyName
    (name, tyvars, constrs) <-
        case theDec of
            DataD _ctx name tyVarBinders _mkind constructors _derivs ->
                pure (name, tyVarBinders, constructors)
            NewtypeD _ctx name tyVarBinders _mkind constructor _derivs ->
                pure (name, tyVarBinders, [constructor])
            _ ->
                fail
                  $ "Expected a data or newtype declaration, but the given name \""
                  <> show tyName
                  <> "\" is neither of these things."
    let modifyName f = mkName . f . nameBase
        newRecName = modifyName typeNameModifier name

    recConstrs <- for constrs $ \constr -> case constr of
        RecC recName fields ->
            pure (recName, fields)
        _ ->
            fail
                $ "Expected a record constructor, but got: "
                <> show constr

    let newConstrs = flip map recConstrs $ \(recName, fields) ->
          ( modifyName constructorModifier recName
          , flip map fields $ \(fieldName, bang', typ) ->
              (modifyName fieldLabelModifier fieldName, bang', typ)
          )

    let mkPatternFrom (recName, _) vars =
            ConP recName $ map VarP vars
        mkVariableNames (_, fields) =
            for fields $ \_ -> newName "x"
        mkBodyFrom (recName, _) vars =
            NormalB $ foldl AppE (ConE recName) (map VarE vars)

    convClauses <-
        for (zip recConstrs newConstrs) $ \(constr, newConstr) -> do
            vars <- mkVariableNames constr
            pure $ Clause [mkPatternFrom constr vars] (mkBodyFrom newConstr vars) []

    let convertName =
            "wrangle" <> nameBase tyName <> "To" <> nameBase newRecName
        convert =
            FunD (mkName convertName) convClauses

    sig <- [t| $(conT tyName) -> $(conT newRecName)|]

    pure
        [ DataD [] newRecName tyvars Nothing (map (uncurry RecC) newConstrs) []
        , SigD (mkName convertName) sig
        , convert
        ]
