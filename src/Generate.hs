{-# LANGUAGE OverloadedStrings, FlexibleContexts   #-}

module Generate where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Text (unpack)
import Text.Casing (camel, pascal)
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))

import qualified Protocol as P

type Program = String

newtype StringAlt = StringAlt String
instance Show StringAlt where
    show (StringAlt s) = s

generate :: P.TopLevel -> Program
generate = (prelude <>) . unlines . map genDomain . P.topLevelDomains
  where
    genDomain de = (genTypes . P.domainsEltTypes $ de) <> 
        (unlines . map (genReturns (unpack $ P.domainsEltDomain de)) . map (\(Just p,q) -> (p,q)) . filter (isJust . fst) . map (P.commandsEltReturns &&& unpack . P.commandsEltName) . filter (not . isTrue . P.commandsEltExperimental) . P.domainsEltCommands $ de) <>
        (unlines . map (genCommand . unpack $ P.domainsEltDomain de) . 
            filter (not . isTrue . P.commandsEltExperimental) . 
            P.domainsEltCommands $ de)
    genCommand dn ce = genCommandName dn (unpack . P.commandsEltName $ ce) <>
        " conn " <>
        (intercalate " " $ genParams ce) <>
        " = " <> 
        genBody dn (unpack $ P.commandsEltName ce) (genParams ce)

    genCommandName dn cn = (camel dn) <> (pascal cn)
  
    genReturns dn (rets, cn) = 
        let name = genReturnTypeName dn cn in
        unlines
        [ "data " <> name <> " = " <> name <> " {"
        , intercalate ",\n" . map (\ret -> "    " <> (unpack $ P.returnsEltName ret) <> " :: " <> 
            ((if isTrue (P.returnsEltOptional ret) then "Maybe " else "") <> (convertType . unpack . getLeft $ P.returnsEltType ret))) $ rets  
        , "} deriving Show"
        , genJSONInstance name 
            (map (unpack . P.returnsEltName) . filter (not . isTrue . P.returnsEltOptional) $ rets) 
            (map (unpack . P.returnsEltName) . filter (isTrue . P.returnsEltOptional) $ rets)
        ]
    genReturnTypeName dn cn = pascal $ genCommandName dn cn
    genJSONInstance name reqFields optFields = 
        let fields = map (id &&& (const False)) reqFields ++ map (id &&& (const True)) optFields
            headField = head fields
            sep isOpt = if isOpt then ".:? " else ".: " in
        unlines
        [ "instance FromJSON " <> name <> " where"
        , "    parseJSON = A.withObject " <> quote name <> " $ \\v -> "
        , "        " <> name <> " <$> v " <> sep (snd headField) <> quote (fst headField)
        , unlines . map ("            " <>) . map (\(field, isOpt) -> "<*> v " <> sep isOpt <> quote field) $ tail fields

        ]

    genParams ce = fromMaybe [] $ fmap (map genParam) $ P.commandsEltParameters ce
    genParam pe = unpack . P.parametersEltName $ pe
    genBody dn methodName params = "sendReceiveCommand conn " <>
        "(" <> ("\"") <> dn <> ("\"") <> "," <> ("\"") <> methodName <> ("\"") <> ") " <>
        show (map (id &&& StringAlt) params)
    genTypes Nothing = []
    genTypes (Just tes) = intercalate "\n\n" . map genType . filter (not . isTrue . P.typesEltExperimental) $ tes
    genType te = unpack . P.typesEltId $ te

    getLeft (Just (AltLeft v)) = v
    getLeft _ = "()"
    isTrue = (== (Just $ AltLeft True))
    quote s = "\"" <> s <> "\""

convertType "string" = "String"
convertType "()" = "()"
convertType "" = "()"
convertType s = error $ "unsupported type for conversion: " <> s

prelude = intercalate "\n\n" $
    [ commandTy
    , sendCommand
    , commandResponse
    , sendReceiveCommand
    , commandResult
    , ""
    ]

commandTy = "\
    \data Command = Command\n\
    \    { commandId :: Int\n\
    \    , commandMethod :: String\n\
    \    , commandParams :: [(String, String)]\n\
    \    } deriving Show\n\

    \instance ToJSON Command where\n\
    \   toJSON cmd = A.object\n\
    \        [ \"id\"     .= commandId cmd\n\
    \        , \"method\" .= commandMethod cmd\n\
    \        , \"params\" .= (M.fromList $ commandParams cmd)\n\
    \        ]"

sendCommand = "\
    \sendCommand :: WS.Connection -> (String, String) -> [(String, String)] -> IO ()\n\
    \sendCommand conn (domain, method) params = WS.sendTextData conn $\n\
    \    A.encode $\n\ 
    \    Command { commandId = 1\n\
    \            , commandMethod = domain <> \".\" <> method\n\
    \            , commandParams = params\n\
    \            }"

commandResponse = "\
    \receiveCommandResponse :: (FromJSON a) => WS.Connection -> IO (Maybe a)\n\
    \receiveCommandResponse conn = A.decode <$> do\n\
    \     dm <- WS.receiveDataMessage conn\n\
    \     pure $ WS.fromDataMessage dm"


sendReceiveCommand = "\
    \sendReceiveCommand :: (FromJSON a) =>\n\
    \    WS.Connection ->\n\
    \         (String, String) -> [(String, String)]\n\ 
    \         -> IO (Maybe a)\n\
    \sendReceiveCommand conn domain_method params = do\n\ 
    \     sendCommand conn domain_method params\n\
    \     receiveCommandResponse conn"

commandResult = "\
    \data CommandResult a = CommandResult { id :: Int, result :: a }\n\
    \instance (Show a) => Show (CommandResult a) where\n\
    \    show = show . result\n\
    \instance (FromJSON a) => FromJSON (CommandResult a) where\n\
    \    parseJSON = A.withObject \"CommandResult\" $ \\v ->\n\
    \        CommandResult <$> v .: \"id\" <*> v .: \"result\""
