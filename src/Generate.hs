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
import qualified Data.Map as Map

import qualified Protocol as P

newtype StringAlt = StringAlt String
instance Show StringAlt where
    show (StringAlt s) = s

generate :: String -> String -> [P.DomainsElt] -> (String, String, Map.Map String String)
generate extensions imports des = (allDomainImports,,generatedDomains) . ("\n\n" <>) $
    unlines 
        [ eventsSumType
        , allEventsResponseFromJSON
        ]
  where
    validDomains = 
        filter (not . hasExperimentalDependencies) . 
        filter (not . isTrue . P.domainsEltExperimental) $ 
        des

    importDomain dn = unwords ["import", "qualified", domainModuleName dn, "as", dn]
    generatedDomains = Map.fromList . map (domainName &&& genDomain) $ validDomains
    allDomainImports = unlines . map (importDomain . unpack . P.domainsEltDomain) $ validDomains

    hasExperimentalDependencies domain = any (`elem` experimentalDomains) . (fromMaybe [] . P.domainsEltDependencies) $ domain
    experimentalDomains = map P.domainsEltDomain . filter (isTrue . P.domainsEltExperimental) $ des
    

    eventsSumTypeName = "Event"
    eventsSumType     = (<> "\n    deriving (Eq, Show, Read)") . 
        (unwords ["data", eventsSumTypeName, "= "] <>) .   
        intercalate " | " . 
        map (\de -> unwords [eventsSumTypeConstructor (domainName de), domainEventsSumTypeName (domainName de)]) $ 
        validDomains

    eventsSumTypeConstructor = ("EV" <>)
    validDomainEvents          = maybe [] filterValidEvents . P.domainsEltEvents
    allEventNames              = concatMap (\de -> map (eventStr (domainName de)) (validDomainEvents de)) validDomains
    allEventConstructors       = concatMap (\de -> map (\_ -> eventsSumTypeConstructor (domainName de)) (validDomainEvents de)) validDomains
    domainEventConstructors    = concatMap (\de -> map (eventsSumTypeConstructor . eventName (domainName de)) (validDomainEvents de)) validDomains
    allEventsResponseFromJSON  = eventResponseFromJSON eventsSumTypeName allEventNames domainEventConstructors allEventConstructors
       
    domainEventsSumTypeName   = (<> "Event")             
    eventResponseFromJSON tn names domainEventConstructors allEventConstructors = unlines $
        [ unwords ["instance FromJSON", "(EventResponse", tn, ") where"]
        , unwords ["    parseJSON = A.withObject ", show "EventResponse", " $ \\obj -> do"]
        , unwords ["        name", "<-", "obj", ".:", show "method"]
        , unwords ["        case (name :: String) of"]
        ] ++ 
            (map (\(l,r) -> unwords ["               ", l, "->", r]) $ zip (map show names)
                (map ((\(dc, ac) -> unwords ["EventResponse", proxy tn, proxy dc, ". fmap", ac, ". fmap", dc, "<$> obj .:?", show "params"])) $ zip domainEventConstructors allEventConstructors) ++ 
                [emptyCase $ "EventResponse" <> tn])
    proxy s = "(Proxy :: Proxy " <> s <> ")"


    handlerType var = unwords [var, "->", "IO", "()"]

    subscribeEvents eventsTypeName = unlines 
        [ unwords ["subscribe :: forall a.", "FromEvent", eventsTypeName, "a => Session -> (", handlerType "a", ") -> IO ()"]
        , unwords ["subscribe (Session session') handler1 = subscribe' paev session' name handler2"]
        , unwords ["  where"]
        , unwords ["    handler2 = maybe (pure ()) handler1 . fromEvent"]
        , unwords ["    name     = eventName pev pa"]
        , unwords ["    paev     = Proxy :: Proxy", eventsSumTypeName]
        , unwords ["    pev      = Proxy :: Proxy", eventsTypeName]
        , unwords ["    pa       = Proxy :: Proxy a"]
        ]

    filterValidEvents = filter (not . isTrue . P.eventsEltExperimental) . filter (not . isTrue . P.eventsEltDeprecated)

    eventStr domainName    ev = (domainName <>) . ("." <>) . unpack . P.eventsEltName $ ev
    commandStr domainName  c  = (domainName <>) . ("." <>) . unpack . P.commandsEltName $ c

    eventClassName = "FromEvent"

    eventNameQualified domainName ev = (domainName <>) . ("." <>) . C.pascal . unpack . P.eventsEltName $ ev
    eventName domainName   ev = (domainName <>) . C.pascal . unpack . P.eventsEltName $ ev
    commandName domainName c  = (domainName <>) . C.pascal . unpack . P.commandsEltName $ c
    domainName = unpack . P.domainsEltDomain
    
    genDomain de = intercalate "\n\n"
        [ extensions
        , domainModuleHeader dn
        , imports
        , otherDomainImports
        , domainEventsSumType
        , unlines . map (\ev -> unlines [eventReturnType ev, eventInstance ev]) $ validEvents
        , subscribeEvents (domainEventsSumTypeName dn)
        , genTypes dn . P.domainsEltTypes $ de
        , unlines . map (genCommand dn) $ validCommands
        ]
      where
        otherDomainImports = unlines . map importDomain . maybe [] (map unpack) . P.domainsEltDependencies $ de
        -- unlines . map (importDomain . domainName) . filter (/= de) $ validDomains

        validCommands      = 
            filter (not . isTrue . P.commandsEltDeprecated) . 
            filter (not . isTrue . P.commandsEltExperimental) . 
            P.domainsEltCommands $ de

        domainEventsSumType = (<> "\n    deriving (Eq, Show, Read)") . 
            (unwords ["data", domainEventsSumTypeName dn, "= "] <>) .   
            intercalate " | " . 
            map (\ev -> let evn = eventName dn ev in unwords [eventsSumTypeConstructor evn, evn]) $ 
            validEvents

        validEvents = maybe [] filterValidEvents . P.domainsEltEvents $ de
        eventReturnType ev = let evn = eventName (domainName de) ev in
            maybe ((<> "\n" <> genFromJSONInstanceEnum evn [evn] [evn]) . (<> "\n    deriving (Eq, Show, Read)") $ unwords ["data", evn, "=", evn]) (genTypeObj dn evn "") . P.eventsEltParameters $ ev
        dn = domainName de 
        eventInstance ev = 
            let evn = eventName dn ev
                evc = eventsSumTypeConstructor evn in
            unlines $
                [ unwords ["instance", eventClassName, domainEventsSumTypeName dn, evn, "where"]
                , unwords ["   ", "eventName  _ _    = ", show $ eventStr dn ev] -- TODO: parameterize
                , unwords ["   ", "fromEvent ev = ", "case ev of", evc, "v -> Just v; _ -> Nothing"] -- TODO: parameterize
                ]
    
    genCommand dn ce = 
        let cn = unpack . P.commandsEltName $ ce
            name = uncapitalizeFirst $ commandName dn ce
            params = filter (not . isTrue . P.parametersEltDeprecated) . 
                filter (not . isTrue . P.parametersEltExperimental) . 
                fromMaybe [] . P.commandsEltParameters $ ce
            
            paramNameToHSName = ((C.toCamel . C.fromAny $ name) <>) . C.toPascal . C.fromAny
            paramNames   = map (unpack . P.parametersEltName) params
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

            isEmptyParams = length params == 0
            paramsTypeName = "P" <> (commandName dn ce)
            paramsTypeDecl = maybe "" -- (unwords  ["type", paramsTypeName, "= ()"]) 
                (genTypeObj dn paramsTypeName "") (P.commandsEltParameters ce)

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
                , intercalate " -> " $ ["Session"] ++ (if isEmptyParams then [] else [paramsTypeName]) ++ [returnTypeSig]
                ]
            , unwords
                [ name, "session", if isEmptyParams then "=" else "params ="
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
        let fields = f False reqFields ++ f True optFields in
        if length fields == 0 then "" else
            let headField = head fields
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

    genBody isEmptyParams isEmptyReturn dn commandName paramNamesOptional = 
        unwords 
        [ if isEmptyReturn then "sendReceiveCommand" else "sendReceiveCommandResult" 
        , "session"
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
    genTypes dn (Just tes) = intercalate "\n\n" . map (genType dn) . filter (not . isTrue . P.typesEltDeprecated) . filter (not . isTrue . P.typesEltExperimental) $ tes
    genType dn te = let name = genTypeName dn (unpack . P.typesEltId $ te) in
            case (P.typesEltEnum te, unpack . P.typesEltType $ te) of
                (Just enumValues,_) -> genTypeEnum name . map unpack $ enumValues
                (Nothing,"object") -> maybe 
                        (genTypeObjNoParams dn name)
                        (genTypeObj dn name "") 
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

    genTypeObj dn name fieldPrefix [] = genTypeObjNoParams dn name 
    genTypeObj dn name fieldPrefix params = 
        let validParams =
                filter (not . isTrue . P.parametersEltDeprecated) . 
                filter (not . isTrue . P.parametersEltExperimental) $ params
            reqParams =  filter (not . isTrue . P.parametersEltOptional) $ validParams
            optParams =  filter (isTrue . P.parametersEltOptional) $ validParams
            reqParamNames = map (unpack . P.parametersEltName) reqParams
            optParamNames = map (unpack . P.parametersEltName) $ optParams
            paramNameToHSName = (C.toCamel . C.fromAny) . (fieldPrefix <>) . (name <>) . C.toPascal . C.fromAny

        in
        if length validParams == 0 then "" else
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

domainModuleName dn = intercalate "." ["Domains", dn]
moduleHeader dn = unwords ["module", dn, "(module", dn <>")", "where"]
domainModuleHeader dn = unwords ["module", domainModuleName dn, "(module", domainModuleName dn <>")", "where"]
importDomain dn = unwords ["import", "qualified", domainModuleName dn, "as", dn]
