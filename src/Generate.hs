{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections #-}

module Generate where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Char
import Data.Text (unpack)
import Text.Casing (camel)
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))

import qualified Protocol as P

newtype StringAlt = StringAlt String
instance Show StringAlt where
    show (StringAlt s) = s

type Program = String

generate :: P.TopLevel -> Program
generate = ("\n\n" <>) . unlines . map genDomain . P.topLevelDomains
  where
    genDomain de = 
        let domainName = unpack $ P.domainsEltDomain de
            validCommands = 
                filter (not . isTrue . P.commandsEltDeprecated) . 
                filter (not . isTrue . P.commandsEltExperimental) . 
                P.domainsEltCommands $ de 
        in
        unlines
        [ genTypes . P.domainsEltTypes $ de
        , unlines . map (genCommand domainName) $ validCommands
        ]

    genCommand dn ce = 
        let commandName = unpack . P.commandsEltName $ ce
            name = genCommandName dn commandName
            params = filter (not . isTrue . P.parametersEltDeprecated) . 
                filter (not . isTrue . P.parametersEltExperimental) . 
                fromMaybe [] . P.commandsEltParameters $ ce
            paramNames = map (unpack . P.parametersEltName) params
            paramOptionals = map (isTrue . P.parametersEltOptional) params
            paramTypes = map (uncurry genEltType) . zip paramOptionals . map P.parametersEltType $ params
            (returnTypeDecl, returnTypeSig, isEmptyReturn) = (maybe ("","IO (Maybe Error)", True) (tupleToTriple False . fmap (<> ")") . fmap ("IO (Either Error " <>) . genReturnType dn . (,commandName))) . P.commandsEltReturns $ ce
        in 
        unlines [ returnTypeDecl
            , intercalate " " 
                [ name, "::"
                , intercalate " -> " ("Session":paramTypes ++ [returnTypeSig])
                ]
            , intercalate " "
                [ name, "session", (intercalate " " paramNames), "="
                , genBody isEmptyReturn dn commandName $ zip paramNames paramOptionals
                ]
        ]

    genCommandName dn cn = (camel dn) <> (capitalizeFirst cn)
    capitalizeFirst [] = []
    capitalizeFirst (hd:tl) = toUpper hd : tl
    uncapitalizeFirst [] = []
    uncapitalizeFirst (hd:tl) = toLower hd : tl
  
    genReturnType dn (rets, cn) = 
        let name = genReturnTypeName dn cn
            validRets = 
                filter (not . isTrue . P.returnsEltDeprecated) . 
                filter (not . isTrue . P.returnsEltExperimental) $ rets 
        in
        (,name) . unlines $
                [ "data " <> name <> " = " <> name <> " {"
                , intercalate ",\n" . 
                    map (\ret -> "    " <> ((uncapitalizeFirst name <>) . 
                    capitalizeFirst . unpack $ P.returnsEltName ret) <> " :: " <> 
                    (genEltType (isTrue . P.returnsEltOptional $ ret) (P.returnsEltType ret))) $ validRets  
                , "} deriving Show"
                , genJSONInstance name
                        (map (unpack . P.returnsEltName) . filter (not . isTrue . P.returnsEltOptional) $ validRets) 
                        (map (unpack . P.returnsEltName) . filter (isTrue . P.returnsEltOptional) $ validRets)]
                        
    genReturnTypeName dn cn = capitalizeFirst $ genCommandName dn cn

    genJSONInstance name reqFields optFields = 
        let fields = map (id &&& (const False)) reqFields ++ map (id &&& (const True)) optFields
            headField = head fields
            sep isOpt = if isOpt then ".:? " else ".: " in
        unlines
        [ "instance FromJSON " <> name <> " where"
        , "    parseJSON = A.withObject " <> show name <> " $ \\v -> "
        , "        " <> name <> " <$> v " <> sep (snd headField) <> show (fst headField)
        , unlines . map ("            " <>) . map (\(field, isOpt) -> "<*> v " <> sep isOpt <> show field) $ tail fields

        ]

    genBody isEmptyReturn dn commandName paramsOpts = 
        intercalate " " 
        [ if isEmptyReturn then "sendReceiveCommand" else "sendReceiveCommandResult" 
        , "(conn session)"
        ,"(" <> ("\"") <> dn <> ("\"") <> "," <> ("\"") <> commandName <> ("\"") <> ")"
        , genBodyArgs paramsOpts
        ]
        

    genBodyArgs paramsOpts = 
        let optArgs = map fst . filter (not . snd) $ paramsOpts
            reqArgs = map fst . filter (snd) $ paramsOpts
            optArgsGen =  ("[" <>) . (<> "]") . intercalate ", " . map genOptArg $ optArgs
            reqArgsGen =  ("catMaybes [" <>) . (<> "]") . intercalate ", " . map genReqArg $ reqArgs in
        wrapRoundBrackets . intercalate " " $ [optArgsGen, "++", wrapRoundBrackets reqArgsGen]
            
    genOptArg param = "(" <> show param <> ", ToJSONEx " <> param <> ")"
    genReqArg param = "fmap ((" <> show param <> ",) . ToJSONEx) " <> param
        
    genTypes Nothing = []
    genTypes (Just tes) = [] -- intercalate "\n\n" . map genType . filter (not . isTrue . P.typesEltDeprecated) . filter (not . isTrue . P.typesEltExperimental) $ tes
    genType te = unpack . P.typesEltId $ te
    -- TODO: ToJSON instances for types

    genEltType isOptional eltt = (if isOptional then "Maybe " else "") <> (convertType . unpack . leftType $ eltt)
    leftType (Just (AltLeft ty)) = ty
    leftType _ = "()"
    isTrue = (== (Just $ AltLeft True))
    tupleToTriple c (a, b) = (a, b, c)
    wrapRoundBrackets s = "(" <> s <> ")"

convertType "string" = "String"
convertType "integer" = "Int"
convertType "boolean" = "Bool"
convertType "number" = "Int" -- TODO
convertType "()" = "()"
convertType "array" = "[String]" -- TODO
convertType "" = "object"
convertType s = error $ "unsupported type for conversion: " <> s
