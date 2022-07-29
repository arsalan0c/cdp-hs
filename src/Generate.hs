{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections, TypeOperators #-}

module Generate where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Char
import Data.Text (Text(..), unpack)
import qualified Text.Casing as C
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))
import qualified Data.Aeson as A

import qualified Protocol as P

newtype StringAlt = StringAlt String
instance Show StringAlt where
    show (StringAlt s) = s

type Program = String

supportedDomains :: [Text]
supportedDomains = ["Browser", "Page"]

generate :: P.TopLevel -> Program
generate tl = ("\n\n" <>) . 
    unlines . map genDomain . 
    filter (not . hasExperimentalDependencies) . 
    filter (not . isTrue . P.domainsEltExperimental) . 
    P.topLevelDomains $ 
    tl
  where
    hasExperimentalDependencies domain = any (`elem` experimentalDomains) . (fromMaybe [] . P.domainsEltDependencies) $ domain
    experimentalDomains = map P.domainsEltDomain . filter (isTrue . P.domainsEltExperimental) . P.topLevelDomains $ tl

    genDomain de = 
        let domainName = unpack $ P.domainsEltDomain de
            validCommands = 
                filter (not . isTrue . P.commandsEltDeprecated) . 
                filter (not . isTrue . P.commandsEltExperimental) . 
                P.domainsEltCommands $ de 
        in
        unlines
        [ genTypes domainName . P.domainsEltTypes $ de
        , unlines . map (genCommand domainName) $ validCommands
        ]

    genCommand dn ce = 
        let cn = unpack . P.commandsEltName $ ce
            name = genCommandName dn cn
            params = filter (not . isTrue . P.parametersEltDeprecated) . 
                filter (not . isTrue . P.parametersEltExperimental) . 
                fromMaybe [] . P.commandsEltParameters $ ce
            paramNames = map ((name <>) . C.toPascal . C.fromAny . unpack . P.parametersEltName) params
            paramOptionals = map (isTrue . P.parametersEltOptional) params
            paramTypes = map (\(isOpt, (t1, (t2, items))) -> (genEltType dn isOpt t1 t2 items)) . zip paramOptionals . map (P.parametersEltType &&& P.parametersEltRef &&& P.parametersEltItems) $ params
            (returnTypeDecl, returnTypeSig, isEmptyReturn) = 
                (\rets -> 
                    if length rets == 0 
                        then ("", emptyReturnSig, True) 
                        else tupleToTriple False $ resultReturnSig <$> (genReturnType dn (rets, name))
                ) .
                filter (not . isTrue . P.returnsEltDeprecated) .
                filter (not . isTrue . P.returnsEltExperimental) .
                fromMaybe [] .
                P.commandsEltReturns $ 
                ce
        in 
        unlines [ returnTypeDecl
            , unwords 
                [ name, "::"
                , intercalate " -> " ("Session":paramTypes ++ [returnTypeSig])
                ]
            , unwords
                [ name, "session", (unwords paramNames), "="
                , genBody isEmptyReturn dn cn $ zip paramNames paramOptionals
                ]
        ]
    genCommandName dn cn = (C.camel dn) <> (capitalizeFirst cn)

    emptyReturnSig           = "IO (Maybe Error)"
    resultReturnSig resultTy = "IO (Either Error " <> resultTy <> ")" 
    capitalizeFirst [] = []
    capitalizeFirst (hd:tl) = toUpper hd : tl
    uncapitalizeFirst [] = []
    uncapitalizeFirst (hd:tl) = toLower hd : tl

    genReturnType dn (rets, commandName) = 
        let name = capitalizeFirst commandName
            reqRets = filter (not . isTrue . P.returnsEltOptional) $ rets
            optRets = filter (isTrue . P.returnsEltOptional) $ rets
        in
        (,name) . unlines $
                [ unwords ["data", name, "=", name, "{"]
                , intercalate ",\n" . 
                    map (\ret -> "    " <> (((uncapitalizeFirst name) <>) . C.toPascal . C.fromAny . unpack $ P.returnsEltName ret) <> 
                        " :: " <> 
                        (genEltType dn (isTrue . P.returnsEltOptional $ ret) 
                                (P.returnsEltType ret)
                                (P.returnsEltRef ret) 
                                (P.returnsEltItems ret))) $ reqRets ++ optRets
                , "} deriving Show"
                , genFromJSONInstance name 
                    (map (unpack . P.returnsEltName) reqRets) 
                    (map (unpack . P.returnsEltName) optRets)
                ]
                        
                        

    genFromJSONInstance name reqFields optFields = 
        let fields = f False reqFields ++ f True optFields
            headField = head fields
            sep isOpt = if isOpt then ".:? " else ".: " in
        unlines
        [ unwords ["instance FromJSON ", name, "where"]
        , unwords ["    parseJSON = A.withObject", show name, "$ \\v ->"]
        , unwords ["        ", name, "<$> v", sep (snd headField), show (fst headField)]
        , unlines . map (\(field, isOpt) -> unwords ["            <*> v ", sep isOpt, show field]) $ tail fields
        ]
      where
        f b = map (id &&& (const b))

    genToJSONInstance name reqFieldsHS optFieldsHS =
        unlines
        [ unwords ["instance ToJSON", name, " where"]
        , unwords ["    toJSON v = A.object"]
        , unwords ["        ["
            , intercalate "\n        , " $ (map fieldToJSON reqFieldsHS) ++ (map optFieldToJSON optFieldsHS)
            ]
        , unwords ["        ]"]
        ]
      where
        fieldToJSON (f, hsf) = unwords [show f, ".=", hsf, "v"]
        optFieldToJSON = fieldToJSON 

    genBody isEmptyReturn dn commandName paramsOpts = 
        unwords 
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
        wrapRoundBrackets . unwords $ [optArgsGen, "++", wrapRoundBrackets reqArgsGen]
            
    genOptArg param = "(" <> show param <> ", ToJSONEx " <> param <> ")"
    genReqArg param = "fmap ((" <> show param <> ",) . ToJSONEx) " <> param

    genFromJSONInstanceEnum name vals hsVals = 
        unlines $
        [ unwords ["instance FromJSON", name, "where"]
        , unwords ["    parseJSON = A.withText ", show name, " $ \\v -> "]
        , unwords ["        pure $ case v of"]
        ] ++ (map (\(v, hsv) -> unwords ["               ", show v, "->", hsv]) $ zip vals hsVals)

    genToJSONInstanceEnum name vals hsVals = 
        unlines $
        [ unwords ["instance ToJSON", name, "where"]
        , unwords ["    toJSON v = A.String $"]
        , unwords ["        case v of"]
        ] ++ (map (\(v, hsv) -> unwords ["               ", hsv, "->", show v]) $ zip vals hsVals)
    
    genTypeName dn tn = dn <> tn
    genTypes _ Nothing    = []
    genTypes dn (Just tes) = intercalate "\n\n" . map (genType dn) . filter (not . isTrue . P.typesEltDeprecated) . filter (not . isTrue . P.typesEltExperimental) $ tes
    genType dn te = let name = genTypeName dn (unpack . P.typesEltId $ te) in
            case (P.typesEltEnum te, unpack . P.typesEltType $ te) of
                (Just enumValues,_) -> genTypeEnum name . map unpack $ enumValues
                (Nothing,"object") -> maybe 
                        (genTypeObjNoParams dn name)
                        (genTypeObj dn name) 
                        (P.typesEltProperties te)
                (Nothing,ty) -> unwords ["type", name, "=", tyl]
        where
            tyl = leftType dn (Just . AltLeft . P.typesEltType $ te) Nothing (P.typesEltItems te)

    genTypeEnum name values = let hsValues = map ((name <>) . C.toPascal . C.fromAny) values in
        unlines
            [ unwords 
                ["data", name, "=", intercalate " | " hsValues]
            , "    deriving Show"
            , genFromJSONInstanceEnum name values hsValues
            , genToJSONInstanceEnum name values hsValues
            ]

    genTypeObjNoParams dn name = unwords ["type", name, "=", typeCDPToHS dn "object" Nothing]

    genTypeObj dn name [] = genTypeObjNoParams dn name 
    genTypeObj dn name params = 
        let validParams =
                filter (not . isTrue . P.parametersEltDeprecated) . 
                filter (not . isTrue . P.parametersEltExperimental) $ params
            reqParams =  filter (not . isTrue . P.parametersEltOptional) $ validParams
            optParams =  filter (isTrue . P.parametersEltOptional) $ validParams
            reqParamNames = map (unpack . P.parametersEltName) reqParams
            optParamNames = map (unpack . P.parametersEltName) $ optParams
            paramNameToHSName = ((C.toCamel . C.fromAny $ name) <>) . C.toPascal . C.fromAny

        in
        unlines [ "data " <> name <> " = " <> name <> " {"
                , intercalate ",\n" . 
                    map (\param -> "    " <> (paramNameToHSName . unpack . P.parametersEltName $ param) <> 
                        " :: " <> 
                        (genEltType dn (isTrue . P.parametersEltOptional $ param) 
                                (P.parametersEltType param)
                                (P.parametersEltRef param) 
                                (P.parametersEltItems param))) $ reqParams ++ optParams
                , "} deriving Show"
                , genFromJSONInstance name reqParamNames optParamNames
                , genToJSONInstance name 
                    (zip reqParamNames (map paramNameToHSName reqParamNames)) 
                    (zip optParamNames (map paramNameToHSName optParamNames))
                ]

genEltType :: String -> Bool -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (P.Items:|:[(Maybe A.Value)])) -> String
genEltType domain isOptional t1 t2 items = (if isOptional then "Maybe " else "") <> (leftType domain t1 t2 items)
leftType :: String -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (P.Items:|:[(Maybe A.Value)])) -> String
leftType _ (Just (AltLeft ty1)) (Just (AltLeft ty2)) _ = error "impossible"
leftType domain (Just (AltLeft ty)) _ itemsElt = typeCDPToHS domain (unpack ty) itemsElt
leftType domain _ (Just (AltLeft ty)) itemsElt = typeCDPToHS domain (unpack ty) itemsElt
leftType _ _ _ _ = error "no type found" 

typeCDPToHS :: String -> String -> (Maybe (P.Items:|:[(Maybe A.Value)])) -> String
typeCDPToHS _ "object" _ = "[(String, String)]"
typeCDPToHS domain "array" (Just (AltLeft items)) = "[" <> (leftType domain (P.itemsType items) (P.itemsRef items) Nothing) <> "]"
typeCDPToHS _ ty (Just (AltLeft items)) = error $ "non-array type with items: " <> ty
typeCDPToHS domain ty Nothing = convertType domain ty
typeCDPToHS _ _ _ = error "no matching type"

isTrue = (== (Just $ AltLeft True))
tupleToTriple c (a, b) = (a, b, c)
wrapRoundBrackets s = "(" <> s <> ")"

convertType _ "string" = "String"
convertType _ "integer" = "Int"
convertType _ "boolean" = "Bool"
convertType _ "number" = "Int" -- TODO
convertType _ "()" = "()"
convertType _ "any" = "Int" -- TODO
convertType _ "array" = error "got array conversion" -- TODO
convertType _ "object" = error "got object type"
convertType _ "" = error "got empty type"
convertType domain s = case splitOn "." s of
    [otherDomain, ty] -> otherDomain <> ty
    _ -> domain <> s 

-- TODO: use Data.List.Extra
breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn needle haystack | needle `isPrefixOf` haystack = ([], haystack)
breakOn needle [] = ([], [])
breakOn needle (x:xs) = first (x:) $ breakOn needle xs

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn [] _ = error "splitOn, needle may not be empty"
splitOn _ [] = [[]]
splitOn needle haystack = a : if null b then [] else splitOn needle $ drop (length needle) b
    where (a,b) = breakOn needle haystack
