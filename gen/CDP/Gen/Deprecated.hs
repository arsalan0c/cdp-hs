-- | Module for filtering out deprecated things.
{-# LANGUAGE RecordWildCards #-}
module CDP.Gen.Deprecated
    ( removeDeprecated
    ) where

import CDP.Definition

removeDeprecated :: TopLevel -> TopLevel
removeDeprecated topLevel@TopLevel {..} = topLevel
    { topLevelDomains =
        map goDomain . filter (not . domainDeprecated) $ topLevelDomains
    }
  where
    goDomain domain@Domain {..} = domain
        { domainCommands = map goCommand $ filter (not . commandDeprecated) domainCommands
        , domainTypes    = map goType    $ filter (not . typeDeprecated)    domainTypes
        , domainEvents   = map goEvent   $ filter (not . eventDeprecated)   domainEvents
        }

    goCommand command@Command {..} = command
        { commandReturns    = filter (not . propertyDeprecated) commandReturns
        , commandParameters = filter (not . propertyDeprecated) commandParameters
        }

    goType ty@Type {..} = ty
        { typeProperties = filter (not . propertyDeprecated) <$> typeProperties
        }

    goEvent event@Event {..} = event
        { eventParameters = filter (not . propertyDeprecated) eventParameters
        }
