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

generate :: [P.DomainsElt] -> Program
generate des = ("\n\n" <>) $
    unlines 
        [ allEventsType
        , allEventsFromJSON
        , allEventReturns
        , allEventReturnsFromJSON
        -- , allEventReturnsDeconstruct
        , unlines . map genDomain $ validDomains
        ]
  where
    validDomains = 
        filter (not . hasExperimentalDependencies) . 
        filter (not . isTrue . P.domainsEltExperimental) $ 
        des

    hasExperimentalDependencies domain = any (`elem` experimentalDomains) . (fromMaybe [] . P.domainsEltDependencies) $ domain
    experimentalDomains = map P.domainsEltDomain . filter (isTrue . P.domainsEltExperimental) $ des
    

    allEventConstructors = concatMap (\d -> map (eventName (domainName d)) . (validEvents . fromMaybe []) $ P.domainsEltEvents d) $ validDomains 
            
    allEventsType = (<> "\n    deriving (Eq, Show, Read)") . 
        ("data EventName = "<>) . 
        intercalate " | " . 
        map ("EventName" <>) $ 
        allEventConstructors

    allEventsFromJSON = genFromJSONInstanceEnum "EventName" allEventConstructors (map ("EventName"<>) allEventConstructors)
  
    allEventReturns = (<> "\n    deriving (Eq, Show, Read)") . ("data EventReturn = "<>) . intercalate " | " .
        map (\c -> unwords ["EventReturn" <> c, c]) $ 
        allEventConstructors
    allEventReturnsFromJSON = unlines $
        [ unwords ["instance FromJSON", "EventResponseResult", "where"]
        , unwords ["    parseJSON = A.withObject ", show "EventResponseResult", " $ \\obj -> do"]
        , unwords ["        errEvent", "<-", "obj", ".:", show "method"]
        , unwords ["        evn", "<-", "methodToName", "<$>", "obj", ".:", show "method"]
        , unwords ["        errParams <- case evn of"]
        ] ++ 
            (map (\(l,r) -> unwords ["               ", l, "->", r]) $ 
                map (show &&& (\c -> unwords ["EventReturn" <> c, "<$> obj .:", show "params"])) allEventConstructors ++ 
                [emptyCase "EventResponseResult"]) ++
                [unwords ["        pure EventResponseResult{..}"]]
    allEventReturnsDeconstruct = unlines $
        [ unwords ["deconstructEventReturn", "::", "EventReturn", "->", "EventResult"]
        , unwords ["deconstructEventReturn", "er", "=", "EventResult", "$", "case er of"]
        ] ++ 
            (map (\(l,r) -> unwords ["       ", l, "->", r]) $ 
                map ((\c -> unwords ["EventReturn" <> c, "val"]) &&& (const "val")) allEventConstructors)


    validEvents  = filter (not . isTrue . P.eventsEltExperimental) . filter (not . isTrue . P.eventsEltDeprecated)

    eventName domainName ev = (domainName <>) . C.pascal . unpack . P.eventsEltName $ ev
    domainName = unpack . P.domainsEltDomain
    genDomain de = 
        let 
            validCommands = 
                filter (not . isTrue . P.commandsEltDeprecated) . 
                filter (not . isTrue . P.commandsEltExperimental) . 
                P.domainsEltCommands $ de
            events = maybe "" (unlines . (map eventReturnType) . validEvents) . P.domainsEltEvents $ de
            eventReturnType ev = let evn = eventName (domainName de) ev in
                maybe ((<> "\n" <> genFromJSONInstanceEnum evn [evn] [evn]) . (<> "\n    deriving (Eq, Show, Read)") $ unwords ["data", evn, "=", evn]) (genTypeObj dn evn) . P.eventsEltParameters $ ev
            dn = domainName de 
        in
        unlines
        [ events
        , genTypes dn . P.domainsEltTypes $ de
        , unlines . map (genCommand dn) $ validCommands
        ]

    genCommand dn ce = 
        let cn = unpack . P.commandsEltName $ ce
            name = genCommandName dn cn
            params = filter (not . isTrue . P.parametersEltDeprecated) . 
                filter (not . isTrue . P.parametersEltExperimental) . 
                fromMaybe [] . P.commandsEltParameters $ ce
            
            paramNameToHSName = ((C.toCamel . C.fromAny $ name) <>) . C.toPascal . C.fromAny
            paramNames = map (unpack . P.parametersEltName) params
            paramHSNames = map paramNameToHSName paramNames
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
                [ name, "session", (unwords paramHSNames), "="
                , genBody isEmptyReturn dn cn (zip (zip paramNames paramHSNames) paramOptionals)
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
                , "} deriving (Eq, Show, Read)"
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

    genBody isEmptyReturn dn commandName paramNamesOptional = 
        unwords 
        [ if isEmptyReturn then "sendReceiveCommand" else "sendReceiveCommandResult" 
        , "(conn session)"
        ,"(" <> ("\"") <> dn <> ("\"") <> "," <> ("\"") <> commandName <> ("\"") <> ")"
        , genBodyArgs paramNamesOptional
        ]
        

    genBodyArgs paramNamesOptional = 
        let optArgs = map fst . filter (not . snd) $ paramNamesOptional
            reqArgs = map fst . filter (snd) $ paramNamesOptional
            optArgsGen =  ("[" <>) . (<> "]") . intercalate ", " . map genOptArg $ optArgs
            reqArgsGen =  ("catMaybes [" <>) . (<> "]") . intercalate ", " . map genReqArg $ reqArgs in
        wrapRoundBrackets . unwords $ [optArgsGen, "++", wrapRoundBrackets reqArgsGen]
            
    genOptArg (paramName,paramHSName) = "(" <> show paramName <> ", ToJSONEx " <> paramHSName <> ")"
    genReqArg (paramName,paramHSName) = "fmap ((" <> show paramName <> ",) . ToJSONEx) " <> paramHSName

    emptyCase name = ("_", unwords ["error", show $ "failed to parse " <> name])

    genFromJSONInstanceEnum name vals hsVals = 
        unlines $
        [ unwords ["instance FromJSON", name, "where"]
        , unwords ["    parseJSON = A.withText ", show name, " $ \\v -> do"]
        , unwords ["        pure $ case v of"]
        ] ++ (map (\(v, hsv) -> unwords ["               ", v, "->", hsv]) $ zip (map show vals) hsVals ++ [emptyCase name])

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
            , "    deriving (Eq, Show, Read)"
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
                , "} deriving (Eq, Show, Read)"
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
