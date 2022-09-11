{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections, TypeOperators #-}

module CDP.Gen.Program where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Char
import Data.Text (Text(..), unpack)
import qualified Text.Casing as C
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))
import qualified Data.Aeson as A

import qualified CDP.Definition as D

newtype StringAlt = StringAlt String
instance Show StringAlt where
    show (StringAlt s) = s

type Program = String

supportedDomains :: [Text]
supportedDomains = ["Browser", "Page"]

generate :: [D.DomainsElt] -> Program
generate des = ("\n\n" <>) $
    unlines 
        [ eventsSumType
        , eventResponseFromJSON
        , unlines . map genDomain $ validDomains
        ]
  where
    validDomains = 
        filter (not . hasExperimentalDependencies) . 
        filter (not . isTrue . D.domainsEltExperimental) $ 
        des

    hasExperimentalDependencies domain = any (`elem` experimentalDomains) . (fromMaybe [] . D.domainsEltDependencies) $ domain
    experimentalDomains = map D.domainsEltDomain . filter (isTrue . D.domainsEltExperimental) $ des
    
    allEventConstructors = concatMap (\d -> map (eventName (domainName d)) . (validEvents . fromMaybe []) $ D.domainsEltEvents d) $ validDomains 
    allEventStrs = concatMap (\d -> map (eventStr (domainName d)) . (validEvents . fromMaybe []) $ D.domainsEltEvents d) $ validDomains 

    eventsSumTypeName = "Event"
    eventsSumType     = (<> "\n    deriving (Eq, Show, Read)") . 
        (unwords ["data", eventsSumTypeName, "= "] <>) .   
        intercalate " | " . 
        map (\c -> unwords [eventsSumTypeConstructor c, c]) $ 
        allEventConstructors

    eventsSumTypeConstructor = ("EV" <>)
            
    allEventsType = (<> "\n    deriving (Ord, Eq, Show, Read)") . 
        ("data EventName = "<>) . 
        intercalate " | " . 
        map ("EventName" <>) $ 
        allEventConstructors

    allEventsFromJSON = genFromJSONInstanceEnum "EventName" allEventConstructors (map ("EventName"<>) allEventConstructors)
  
    allEventReturns = (<> "\n    deriving (Eq, Show, Read)") . ("data EventReturn = "<>) . intercalate " | " .
        map (\c -> unwords ["EventReturn" <> c, c]) $ 
        allEventConstructors
    eventResponseFromJSON = unlines $
        [ unwords ["instance FromJSON", "(EventResponse", eventsSumTypeName, ") where"]
        , unwords ["    parseJSON = A.withObject ", show "EventResponse", " $ \\obj -> do"]
        , unwords ["        name", "<-", "obj", ".:", show "method"]
        , unwords ["        case (name :: String) of"]
        ] ++ 
            (map (\(l,r) -> unwords ["               ", l, "->", r]) $ zip (map show allEventStrs)
                (map ((\c -> unwords ["EventResponse", proxy eventsSumTypeName, proxy c, ". fmap", eventsSumTypeConstructor c, "<$> obj .:?", show "params"])) allEventConstructors) ++ 
                [emptyCase "EventResponse"])
    proxy s = "(Proxy :: Proxy " <> s <> ")"

    validEvents  = filter (not . isTrue . D.eventsEltExperimental) . filter (not . isTrue . D.eventsEltDeprecated)

    eventStr domainName    ev = (domainName <>) . ("." <>) . unpack . D.eventsEltName $ ev
    commandStr domainName  c  = (domainName <>) . ("." <>) . unpack . D.commandsEltName $ c

    eventClassName = "FromEvent"
    eventName domainName   ev = (domainName <>) . C.pascal . unpack . D.eventsEltName $ ev
    commandName domainName c  = (domainName <>) . C.pascal . unpack . D.commandsEltName $ c
    domainName = unpack . D.domainsEltDomain
    
    genDomain de = unlines
        [ events
        , genTypes dn . D.domainsEltTypes $ de
        , unlines . map (genCommand dn) $ validCommands
        ]
      where
        validCommands = 
            filter (not . isTrue . D.commandsEltDeprecated) . 
            filter (not . isTrue . D.commandsEltExperimental) . 
            D.domainsEltCommands $ de
        events = maybe "" (unlines . concatMap (\ev -> [eventReturnType ev, eventInstance ev]) . validEvents) . D.domainsEltEvents $ de
        eventReturnType ev = let evn = eventName (domainName de) ev in
            maybe ((<> "\n" <> genFromJSONInstanceEnum evn [evn] [evn]) . (<> "\n    deriving (Eq, Show, Read)") $ unwords ["data", evn, "=", evn]) (genTypeObj dn evn "") . D.eventsEltParameters $ ev
        dn = domainName de 
        eventInstance ev = 
            let evn = eventName dn ev
                evc = eventsSumTypeConstructor evn in
            unlines $
                [ unwords ["instance", eventClassName, eventsSumTypeName, evn, "where"]
                , unwords ["   ", "eventName  _ _    = ", show $ eventStr dn ev] -- TODO: parameterize
                , unwords ["   ", "fromEvent ev = ", "case ev of", evc, "v -> Just v; _ -> Nothing"] -- TODO: parameterize
                ]
    
    genCommand dn ce = 
        let cn = unpack . D.commandsEltName $ ce
            name = uncapitalizeFirst $ commandName dn ce
            params = filter (not . isTrue . D.parametersEltDeprecated) . 
                filter (not . isTrue . D.parametersEltExperimental) . 
                fromMaybe [] . D.commandsEltParameters $ ce
            
            paramNameToHSName = ((C.toCamel . C.fromAny $ name) <>) . C.toPascal . C.fromAny
            paramNames   = map (unpack . D.parametersEltName) params
            paramHSNames = map paramNameToHSName paramNames
            paramOptionals = map (isTrue . D.parametersEltOptional) params
            paramTypes = map (\(isOpt, (t1, (t2, items))) -> (genEltType dn isOpt t1 t2 items)) . zip paramOptionals . map (D.parametersEltType &&& D.parametersEltRef &&& D.parametersEltItems) $ params
            (returnTypeDecl, returnTypeSig, isEmptyReturn) = 
                (\rets -> 
                    if length rets == 0 
                        then ("", emptyReturnSig, True) 
                        else tupleToTriple False $ resultReturnSig <$> (genReturnType dn (rets, name))
                ) .
                filter (not . isTrue . D.returnsEltDeprecated) .
                filter (not . isTrue . D.returnsEltExperimental) .
                fromMaybe [] .
                D.commandsEltReturns $ 
                ce

            isEmptyParams = length params == 0
            paramsTypeName = "P" <> (commandName dn ce)
            paramsTypeDecl = maybe ""
                (genTypeObj dn paramsTypeName "") (D.commandsEltParameters ce)

            commandInstance ce =
                let cn = commandName dn ce in
                unlines $
                    [ unwords ["instance Command ", cn, "where"]
                    , unwords ["   ", "commandName _ =", show $ commandStr dn ce]
                    ]

        in 
        unlines [ returnTypeDecl, if isEmptyReturn then "" else commandInstance ce, paramsTypeDecl
            , unwords 
                [ name, "::"
                , intercalate " -> " $ ["Handle Event"] ++ (if isEmptyParams then [] else [paramsTypeName]) ++ [returnTypeSig]
                ]
            , unwords
                [ name, "handle", if isEmptyParams then "=" else "params ="
                , genBody isEmptyParams isEmptyReturn dn ce (zip (zip paramNames paramHSNames) paramOptionals)
                ]
        ]

    emptyReturnSig           = "IO (Maybe Error)"
    resultReturnSig resultTy = "IO (Either Error " <> resultTy <> ")" 
    capitalizeFirst [] = []
    capitalizeFirst (hd:tl) = toUpper hd : tl
    uncapitalizeFirst [] = []
    uncapitalizeFirst (hd:tl) = toLower hd : tl

    genReturnType dn (rets, commandName) = 
        let name = capitalizeFirst commandName
            reqRets = filter (not . isTrue . D.returnsEltOptional) $ rets
            optRets = filter (isTrue . D.returnsEltOptional) $ rets
        in
        (,name) . unlines $
                [ unwords ["data", name, "=", name, "{"]
                , intercalate ",\n" . 
                    map (\ret -> "    " <> (((uncapitalizeFirst name) <>) . C.toPascal . C.fromAny . unpack $ D.returnsEltName ret) <> 
                        " :: " <> 
                        (genEltType dn (isTrue . D.returnsEltOptional $ ret) 
                                (D.returnsEltType ret)
                                (D.returnsEltRef ret) 
                                (D.returnsEltItems ret))) $ reqRets ++ optRets
                , "} deriving (Eq, Show, Read, Generic)"
                , genFromJSONInstance name 
                    (map (unpack . D.returnsEltName) reqRets) 
                    (map (unpack . D.returnsEltName) optRets)
                ]
                        
    genFromJSONInstance name reqFields optFields = 
        let fields = f False reqFields ++ f True optFields in
        if length fields == 0 then "" else
            let headField = head fields
                sep isOpt = if isOpt then ".:? " else ".: " in
            unlines
            [ unwords ["instance FromJSON ", name, "where"]
            , unwords ["    parseJSON = A.genericParseJSON", fromJSONOpts (length name)]
            ]
      where
        f b = map (id &&& (const b))

    toJSONOpts   n    = unwords ["A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop", show n, ", A.omitNothingFields = True}"]
    fromJSONOpts n    = unwords ["A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop", show n, "}"]


    genToJSONInstance name reqFieldsHS optFieldsHS =
        unlines
        [ unwords ["instance ToJSON", name, " where"]
        , unwords ["    toJSON = A.genericToJSON", toJSONOpts (length name)]
        ]
    
    genBody isEmptyParams isEmptyReturn dn commandName paramNamesOptional = 
        unwords 
        [ if isEmptyReturn then "sendReceiveCommand" else "sendReceiveCommandResult" 
        , "handle"
        , show $ commandStr dn commandName
        , if isEmptyParams then "(Nothing :: Maybe ())" else "(Just params)" -- genBodyArgs paramNamesOptional
        ]
        
    genBodyArgs paramNamesOptional = 
        let optArgs = map fst . filter (not . snd) $ paramNamesOptional
            reqArgs = map fst . filter (snd) $ paramNamesOptional
            optArgsGen =  ("[" <>) . (<> "]") . intercalate ", " . map genOptArg $ optArgs
            reqArgsGen =  ("catMaybes [" <>) . (<> "]") . intercalate ", " . map genReqArg $ reqArgs in
        wrapRoundBrackets . unwords $ [optArgsGen, "++", wrapRoundBrackets reqArgsGen]
            
    genOptArg (paramName,paramHSName) = "(" <> show paramName <> ", ToJSONEx " <> paramHSName <> ")"
    genReqArg (paramName,paramHSName) = "fmap ((" <> show paramName <> ",) . ToJSONEx) " <> paramHSName

    emptyCase name = ("_", unwords ["fail", show $ "failed to parse " <> name])

    genFromJSONInstanceEnum name vals hsVals = 
        unlines $
        [ unwords ["instance FromJSON", name, "where"]
        , unwords ["    parseJSON = A.withText ", show name, " $ \\v -> do"]
        , unwords ["        case v of"]
        ] ++ (map (\(v, hsv) -> unwords ["               ", v, "->", hsv]) $ zip (map show vals) (map ("pure $ " <>) hsVals) ++ [emptyCase name])

    genToJSONInstanceEnum name vals hsVals = 
        unlines $
        [ unwords ["instance ToJSON", name, "where"]
        , unwords ["    toJSON v = A.String $"]
        , unwords ["        case v of"]
        ] ++ (map (\(v, hsv) -> unwords ["               ", hsv, "->", show v]) $ zip vals hsVals)
    
    genTypeName dn tn = dn <> tn
    genTypes _ Nothing    = []
    genTypes dn (Just tes) = intercalate "\n\n" . map (genType dn) . filter (not . isTrue . D.typesEltDeprecated) . filter (not . isTrue . D.typesEltExperimental) $ tes
    genType dn te = let name = genTypeName dn (unpack . D.typesEltId $ te) in
            case (D.typesEltEnum te, unpack . D.typesEltType $ te) of
                (Just enumValues,_) -> genTypeEnum name . map unpack $ enumValues
                (Nothing,"object") -> maybe 
                        (genTypeObjNoParams dn name)
                        (genTypeObj dn name "") 
                        (D.typesEltProperties te)
                (Nothing,ty) -> unwords ["type", name, "=", tyl]
        where
            tyl = leftType dn (Just . AltLeft . D.typesEltType $ te) Nothing (D.typesEltItems te)

    genTypeEnum name values = let hsValues = map ((name <>) . C.toPascal . C.fromAny) values in
        unlines
            [ unwords 
                ["data", name, "=", intercalate " | " hsValues]
            , "    deriving (Eq, Show, Read, Generic)"
            , genFromJSONInstanceEnum name values hsValues
            , genToJSONInstanceEnum name values hsValues
            ]

    genTypeObjNoParams dn name = unwords ["type", name, "=", typeCDPToHS dn "object" Nothing]

    genTypeObj dn name fieldPrefix [] = genTypeObjNoParams dn name 
    genTypeObj dn name fieldPrefix params = 
        let validParams =
                filter (not . isTrue . D.parametersEltDeprecated) . 
                filter (not . isTrue . D.parametersEltExperimental) $ params
            reqParams =  filter (not . isTrue . D.parametersEltOptional) $ validParams
            optParams =  filter (isTrue . D.parametersEltOptional) $ validParams
            reqParamNames = map (unpack . D.parametersEltName) reqParams
            optParamNames = map (unpack . D.parametersEltName) $ optParams
            paramNameToHSName = (uncapitalizeFirst) . (fieldPrefix <>) . (name <>) . capitalizeFirst

        in
        if length validParams == 0 then "" else
        unlines [ "data " <> name <> " = " <> name <> " {"
                , intercalate ",\n" . 
                    map (\param -> "    " <> (paramNameToHSName . unpack . D.parametersEltName $ param) <> 
                        " :: " <> 
                        (genEltType dn (isTrue . D.parametersEltOptional $ param) 
                                (D.parametersEltType param)
                                (D.parametersEltRef param) 
                                (D.parametersEltItems param))) $ reqParams ++ optParams
                , "} deriving (Eq, Show, Read, Generic)"
                , genFromJSONInstance name reqParamNames optParamNames
                , genToJSONInstance name 
                    (zip reqParamNames (map paramNameToHSName reqParamNames)) 
                    (zip optParamNames (map paramNameToHSName optParamNames))
                ]

genEltType :: String -> Bool -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> String
genEltType domain isOptional t1 t2 items = (if isOptional then "Maybe " else "") <> (leftType domain t1 t2 items)
leftType :: String -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> String
leftType _ (Just (AltLeft ty1)) (Just (AltLeft ty2)) _ = error "impossible"
leftType domain (Just (AltLeft ty)) _ itemsElt = typeCDPToHS domain (unpack ty) itemsElt
leftType domain _ (Just (AltLeft ty)) itemsElt = typeCDPToHS domain (unpack ty) itemsElt
leftType _ _ _ _ = error "no type found" 

typeCDPToHS :: String -> String -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> String
typeCDPToHS _ "object" _ = "[(String, String)]"
typeCDPToHS domain "array" (Just (AltLeft items)) = "[" <> (leftType domain (D.itemsType items) (D.itemsRef items) Nothing) <> "]"
typeCDPToHS _ ty (Just (AltLeft items)) = error $ "non-array type with items: " <> ty
typeCDPToHS domain ty Nothing = convertType domain ty
typeCDPToHS _ _ _ = error "no matching type"

isTrue = (== (Just $ AltLeft True))
tupleToTriple c (a, b) = (a, b, c)
wrapRoundBrackets s = "(" <> s <> ")"

convertType _ "string" = "String"
convertType _ "integer" = "Int"
convertType _ "boolean" = "Bool"
convertType _ "number" = "Double"
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
