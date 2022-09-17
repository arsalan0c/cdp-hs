{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections, TypeOperators #-}

module CDP.Gen.Program where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import qualified Text.Casing as C
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))
import qualified Data.Aeson as A

import qualified CDP.Definition as D

----------- Constants from the handwritten runtime of the CDP library ----------- 

eventClassName, eventResponseName, handleType, commandClassName, sendCommandName, sendCommandResultName, emptyReturnSig :: T.Text
eventClassName         = "FromEvent"
eventResponseName      = "EventResponse"
handleType             = "Handle " <> eventsSumTypeName
commandClassName       = "Command"
sendCommandName        = "sendReceiveCommand"
sendCommandResultName  = "sendReceiveCommandResult"
emptyReturnSig         = "IO (Maybe Error)"

resultReturnSig :: T.Text -> T.Text
resultReturnSig resultTy = "IO (Either Error " <> resultTy <> ")" 

----------- Start of program generation ----------- 

type Program = T.Text

genProgram :: [D.DomainsElt] -> Program
genProgram domainElts = T.unlines 
    [ eventsSumType evnsHS
    , eventResponseFromJSON evns evnsHS
    , T.unlines . map genDomain $ delts
    ]
  where
    evns    = allEventNames delts
    evnsHS  = allEventNamesHS delts
    delts   = validDomains domainElts

----------- Generation of global types / values ----------- 

eventsSumTypeName :: T.Text
eventsSumTypeName = "Event"

eventsSumTypeConstructor :: T.Text -> T.Text
eventsSumTypeConstructor = ("EV" <>)

eventsSumType :: [T.Text] -> T.Text
eventsSumType eventNamesHS = (<> ("\n" <> space 3 <> derivingBase)) . 
        (T.unwords ["data", eventsSumTypeName, "= "] <>) .
        T.intercalate " | " . 
        map (\c -> T.unwords [eventsSumTypeConstructor c, c]) $ eventNamesHS
        
eventResponseFromJSON :: [T.Text] -> [T.Text] -> T.Text
eventResponseFromJSON eventNames eventNamesHS = T.unlines $
    [ T.unwords ["instance FromJSON", "(" <> eventResponseName, eventsSumTypeName, ") where"]
    , T.unwords [space 3, "parseJSON = A.withObject ", (T.pack . show) eventResponseName, " $ \\obj -> do"]
    , T.unwords [space 7, "name", "<-", "obj", ".:", (T.pack . show) "method"]
    , T.unwords [space 7, "case (name :: String) of"]
    ] ++ 
        (map (\(l,r) -> T.unwords [space 11, l, "->", r]) $ zip (map (T.pack . show) $ eventNames)
            (map ((\c -> T.unwords [eventResponseName, proxy eventsSumTypeName, proxy c, ". fmap", eventsSumTypeConstructor c, "<$> obj .:?", (T.pack . show) "params"])) eventNamesHS) ++ 
            [emptyCase eventResponseName])

----------- Generation of domain types / values ----------- 

genDomain :: D.DomainsElt -> T.Text
genDomain domainElt = T.unlines . map T.unlines $
    [ map (genType dn)    . validTypes    $ domainElt
    , map (genEvent dn)   . validEvents   $ domainElt
    , map (genCommand dn) . validCommands $ domainElt 
    ]
  where
    dn = domainName domainElt

genType :: T.Text -> D.TypesElt -> T.Text
genType domainName telt = case D.typesEltEnum telt of
    Just enumValues -> genTypeEnum tn enumValues
    Nothing         -> case tytelt of
            "object" -> maybe 
                (genTypeSynonynm domainName tn) 
                (genParamsType domainName tn) 
                tpeltsM
            ty       ->  T.unwords ["type", tn, "=", lty]                 
  where
    lty      = leftType domainName (Just . AltLeft $ tytelt) Nothing (D.typesEltItems telt)
    tytelt   = D.typesEltType telt
    tpeltsM  = guardEmptyList isValidParam $ D.typesEltProperties telt
    tn       = typeNameHS domainName telt 

genTypeEnum :: T.Text -> [T.Text] -> T.Text
genTypeEnum typeEnumName values = T.unlines
    [ T.unwords ["data", typeEnumName, "=", T.intercalate " | " constructors]
    , space 4 <> derivingOrd
    , genFromJSONInstanceEnum typeEnumName values constructors
    , genToJSONInstanceEnum typeEnumName values constructors
    ]
  where
    constructors = map (tyNameHS typeEnumName) values

genEvent :: T.Text -> D.EventsElt -> T.Text
genEvent domainName eventElt = T.unlines $
    [ genEventReturnType domainName eventElt
    , genEventInstance domainName eventElt
    ]

genEventReturnType :: T.Text -> D.EventsElt -> T.Text
genEventReturnType domainName eventElt = maybe emptyParams genNonEmptyParams eveltsM
  where
    emptyParams = T.unlines 
        [ T.unwords ["data", evrn, "=", evrn]
        , space 4 <> derivingBase
        , genFromJSONInstanceEnum evrn [evrn] [evrn]
        ]
    genNonEmptyParams = genParamsType domainName evrn
    eveltsM = guardEmptyList isValidParam $ D.eventsEltParameters eventElt
    evrn = eventNameHS domainName eventElt

genEventInstance :: T.Text -> D.EventsElt -> T.Text
genEventInstance domainName eventElt = T.unlines $
    [ T.unwords ["instance", eventClassName, eventsSumTypeName, evrn, "where"]
    , T.unwords [space 3, "eventName  _ _    = ", (T.pack . show) $ eventName domainName eventElt] -- TODO: parameterize
    , T.unwords [space 3, "fromEvent ev      = ", "case ev of", eventsSumTypeConstructor evrn, "v -> Just v; _ -> Nothing"] -- TODO: parameterize
    ]
  where
    evrn = eventNameHS domainName eventElt

genCommand :: T.Text -> D.CommandsElt -> T.Text
genCommand domainName commandElt = T.unlines . catMaybes $
    [ paramsTypeDef
    , Just $ genCommandFn (peltsM == Nothing) (reltsM == Nothing) cn ptn rtn
    , returns
    ]
  where
    cn   = commandName domainName commandElt 
    ptn  = commandParamsNameHS domainName commandElt
    rtn  = commandNameHS domainName commandElt 

    paramsTypeDef = genParamsType domainName ptn <$> peltsM
    peltsM = guardEmptyList isValidParam $ D.commandsEltParameters commandElt

    returns = genNonEmptyReturns <$> reltsM
    genNonEmptyReturns relts = T.unlines
        [ genReturnType domainName rtn relts
        , genFromJSONInstance rtn
        , genCommandInstance 
            (commandNameHS domainName commandElt) 
            (commandName domainName commandElt)
        ]

    reltsM = guardEmptyList isValidReturn $ D.commandsEltReturns commandElt

genCommandInstance :: T.Text -> T.Text -> T.Text
genCommandInstance commandNameHS commandName =
    T.unlines $
        [ T.unwords ["instance", commandClassName, commandNameHS, "where"]
        , T.unwords [space 3, "commandName _ =", (T.pack . show) commandName]
        ]

genCommandFn :: Bool -> Bool -> T.Text -> T.Text -> T.Text -> T.Text
genCommandFn isEmptyParams isEmptyReturn commandName paramsTypeName returnTypeName = T.unlines
    [ fnType
    , T.unwords [fnHeader, fnBody]
    ]
  where
    fnType = T.unwords
        [ fnName
        , "::"
        , T.intercalate " -> " $ [handleType] ++ 
            (if isEmptyParams then [] else [paramsTypeName]) ++ 
            [returnTypeSig]
        ]
    fnHeader = T.unwords [fnName, "handle", if isEmptyParams then "=" else "params ="]
    fnBody   = T.unwords 
        [ if isEmptyReturn then sendCommandName else sendCommandResultName
        , "handle"
        , (T.pack . show) commandName
        , if isEmptyParams then "(Nothing :: Maybe ())" else "(Just params)"
        ]
    returnTypeSig  = if isEmptyReturn then emptyReturnSig else resultReturnSig returnTypeName
    fnName         = uncapitalizeFirst returnTypeName

genParamsType :: T.Text -> T.Text -> [D.ParametersElt] -> T.Text
genParamsType domainName paramsTypeName []     = genTypeSynonynm domainName paramsTypeName
genParamsType domainName paramsTypeName paramElts = T.unlines
    [ T.unlines enumDecls
    , T.unwords ["data", paramsTypeName, "=", paramsTypeName, "{"]
    , T.intercalate ",\n" $ fields
    , "} " <> derivingGeneric
    , genToJSONInstance paramsTypeName
    , genFromJSONInstance paramsTypeName
    ]
  where
    fields = map toField fieldSigDecls
    toField (fn, ftn, _) = T.unwords [space 3, fn, "::", ftn]

    enumDecls    = catMaybes . map (\(_,_,d) -> d) $ fieldSigDecls
    fieldSigDecls = map peltToFieldSigDecl paramElts

    peltToFieldSigDecl pelt = case D.parametersEltEnum pelt of
        Just enumValues -> (fn, ftn, ) . Just $ genTypeEnum ftn enumValues
        Nothing         -> (fn, , Nothing) $ genEltType domainName (isTrue . D.parametersEltOptional $ pelt)
                (D.parametersEltType pelt)
                (D.parametersEltRef pelt) 
                (D.parametersEltItems pelt)
      where
        ftn = tyNameHS "" fn
        fn = fieldNameHS paramsTypeName . D.parametersEltName $ pelt

genTypeSynonynm :: T.Text -> T.Text -> T.Text
genTypeSynonynm domainName typeName = T.unwords ["type", typeName, "=", typeCDPToHS domainName "object" Nothing]

genReturnType :: T.Text -> T.Text -> [D.ReturnsElt] -> T.Text
genReturnType domainName returnTypeName returnElts = T.unlines
    [ T.unwords ["data", returnTypeName, "=", returnTypeName, "{"]
    , T.intercalate ",\n" $ fields
    , "} " <> derivingGeneric
    ]
  where
    fields = map ((space 4 <>) . reltToField) $ returnElts
    reltToField relt = T.unwords
        [ fieldNameHS returnTypeName . D.returnsEltName $ relt
        , "::"
        , reltToTypeSig relt
        ]

    reltToTypeSig relt = genEltType domainName (isTrue . D.returnsEltOptional $ relt) 
            (D.returnsEltType relt)
            (D.returnsEltRef relt) 
            (D.returnsEltItems relt)
            
----------- Generation rules ----------- 

genFromJSONInstance :: T.Text -> T.Text
genFromJSONInstance name = T.unlines
    [ T.unwords ["instance FromJSON ", name, "where"]
    , T.unwords [space 3, "parseJSON = A.genericParseJSON", fromJSONOpts (T.length name)]
    ]

genToJSONInstance :: T.Text -> T.Text
genToJSONInstance name = T.unlines
    [ T.unwords ["instance ToJSON", name, " where"]
    , T.unwords [space 3, "toJSON = A.genericToJSON", toJSONOpts (T.length name)]
    ]

genFromJSONInstanceEnum :: T.Text -> [T.Text] -> [T.Text] -> T.Text
genFromJSONInstanceEnum name vals hsVals = T.unlines $
    [ T.unwords ["instance FromJSON", name, "where"]
    , T.unwords [space 3, "parseJSON = A.withText ", (T.pack . show) name, " $ \\v -> do"]
    , T.unwords [space 6, "case v of"]
    ] ++ (map (\(v, hsv) -> T.unwords [space 9, v, "->", hsv]) $ zip (map (T.pack . show) vals) (map ("pure $ " <>) hsVals) ++ 
        [emptyCase name])
    
genToJSONInstanceEnum :: T.Text -> [T.Text] -> [T.Text] -> T.Text
genToJSONInstanceEnum name vals hsVals = T.unlines $
    [ T.unwords ["instance ToJSON", name, "where"]
    , T.unwords [space 3, "toJSON v = A.String $"]
    , T.unwords [space 6, "case v of"]
    ] ++ (map (\(v, hsv) -> T.unwords [space 9, hsv, "->", (T.pack . show) v]) $ zip vals hsVals)

toJSONOpts   :: Int -> T.Text
toJSONOpts   n    = T.unwords ["A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop", ((T.pack . show) n), ", A.omitNothingFields = True}"]

fromJSONOpts :: Int -> T.Text
fromJSONOpts n    = T.unwords ["A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop", ((T.pack . show) n), "}"]

genEltType :: T.Text -> Bool -> (Maybe (T.Text:|:[(Maybe A.Value)])) -> (Maybe (T.Text:|:[(Maybe A.Value)])) -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> T.Text
genEltType name isOptional t1 t2 items = (if isOptional then "Maybe " else "") <> (leftType name t1 t2 items)

leftType :: T.Text -> (Maybe (T.Text:|:[(Maybe A.Value)])) -> (Maybe (T.Text:|:[(Maybe A.Value)])) -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> T.Text
leftType _ (Just (AltLeft ty1)) (Just (AltLeft ty2)) _ = error "impossible"
leftType domain (Just (AltLeft ty)) _ itemsElt = typeCDPToHS domain ty itemsElt
leftType domain _ (Just (AltLeft ty)) itemsElt = typeCDPToHS domain ty itemsElt
leftType _ _ _ _ = error "no type found"

typeCDPToHS :: T.Text -> T.Text -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> T.Text
typeCDPToHS _ "object" _ = "[(T.Text, T.Text)]"
typeCDPToHS domain "array" (Just (AltLeft items)) = "[" <> (leftType domain (D.itemsType items) (D.itemsRef items) Nothing) <> "]"
typeCDPToHS _ ty (Just (AltLeft items)) = error . T.unpack $ "non-array type with items: " <> ty
typeCDPToHS domain ty Nothing = convertType domain ty
typeCDPToHS _ _ _ = error "no matching type"

convertType :: T.Text -> T.Text -> T.Text
convertType _ "string" = "String"
convertType _ "integer" = "Int"
convertType _ "boolean" = "Bool"
convertType _ "number" = "Double"
convertType _ "()" = "()"
convertType _ "any" = "Int" -- TODO
convertType _ "array" = error "got array conversion" -- TODO
convertType _ "object" = error "got object type"
convertType _ "" = error "got empty type"
convertType domain s = case T.splitOn "." s of
    [otherDomain, ty] -> tyNameHS otherDomain ty
    _ -> tyNameHS domain s 

domainName :: D.DomainsElt -> T.Text
domainName = D.domainsEltDomain

validDomains :: [D.DomainsElt] -> [D.DomainsElt]
validDomains ds = filter (not . hasExperimentalDependencies) . 
    filter (not . isTrue . D.domainsEltExperimental) .
    filter (not . isTrue . D.domainsEltDeprecated) $
    ds
  where
    hasExperimentalDependencies d = any (`elem` experimentalDomains) . (fromMaybe [] . D.domainsEltDependencies) $ d
    experimentalDomains = map D.domainsEltDomain . filter (isTrue . D.domainsEltExperimental) $ ds

isValidType :: D.TypesElt -> Bool
isValidType telt = (not . isTrue . D.typesEltDeprecated $ telt) && (not . isTrue . D.typesEltExperimental $ telt)

isValidEvent :: D.EventsElt -> Bool
isValidEvent evelt = (not . isTrue . D.eventsEltExperimental $ evelt) && (not . isTrue . D.eventsEltDeprecated $ evelt)

isValidCommand :: D.CommandsElt -> Bool
isValidCommand celt = (not . isTrue . D.commandsEltDeprecated $ celt) && (not . isTrue . D.commandsEltExperimental $ celt)

isValidParam :: D.ParametersElt -> Bool
isValidParam pelt = (not . isTrue . D.parametersEltDeprecated $ pelt) && (not . isTrue . D.parametersEltExperimental $ pelt)

isValidReturn :: D.ReturnsElt -> Bool
isValidReturn relt = (not . isTrue . D.returnsEltDeprecated $ relt) && (not . isTrue . D.returnsEltExperimental $ relt)

validTypes :: D.DomainsElt -> [D.TypesElt]
validTypes = filter isValidType . fromMaybe [] . D.domainsEltTypes

validEvents :: D.DomainsElt -> [D.EventsElt]
validEvents = filter isValidEvent . fromMaybe [] . D.domainsEltEvents

validCommands :: D.DomainsElt -> [D.CommandsElt]
validCommands = filter isValidCommand . D.domainsEltCommands

allEventNamesHS :: [D.DomainsElt] -> [T.Text]
allEventNamesHS = concatMap (\(d, evs) -> map (eventNameHS $ domainName d) evs) . map (id &&& validEvents)

allEventNames :: [D.DomainsElt] -> [T.Text]
allEventNames = concatMap (\(d, evs) -> map (eventName $ domainName d) evs) . map (id &&& validEvents)

eventName   :: T.Text -> D.EventsElt -> T.Text
eventName   domainName ev = (domainName <>) . ("." <>) . D.eventsEltName $ ev
eventNameHS :: T.Text -> D.EventsElt -> T.Text
eventNameHS domainName ev = tyNameHS domainName (D.eventsEltName ev)

commandName   :: T.Text -> D.CommandsElt -> T.Text
commandName domainName c = (domainName <>) . ("." <>) . D.commandsEltName $ c
commandNameHS :: T.Text -> D.CommandsElt -> T.Text
commandNameHS domainName c = tyNameHS domainName (D.commandsEltName c)
commandParamsNameHS :: T.Text -> D.CommandsElt -> T.Text
commandParamsNameHS domainName c = paramsTypePrefix $ commandNameHS domainName c

typeNameHS :: T.Text -> D.TypesElt -> T.Text
typeNameHS domainName t = tyNameHS domainName (D.typesEltId t)

tyNameHS :: T.Text -> T.Text -> T.Text
tyNameHS prefix tyName = (T.pack . C.pascal . T.unpack $ prefix) <> (T.pack . C.pascal . T.unpack $ tyName)

fieldNameHS :: T.Text -> T.Text -> T.Text
fieldNameHS tyName fieldName = (uncapitalizeFirst tyName <>) . T.pack . C.pascal . T.unpack $ fieldName 

paramsTypePrefix :: T.Text -> T.Text
paramsTypePrefix = ("P" <>)

proxy :: T.Text -> T.Text
proxy s = "(Proxy :: Proxy " <> s <> ")"

emptyCase :: T.Text -> (T.Text, T.Text)
emptyCase name = ("_", T.unwords ["fail", (T.pack . show) $ "failed to parse " <> name])

derivingBase    :: T.Text
derivingBase    = "deriving (Eq, Show, Read)"
derivingOrd     :: T.Text
derivingOrd     = "deriving (Ord, Eq, Show, Read)"
derivingGeneric :: T.Text
derivingGeneric = "deriving (Generic, Eq, Show, Read)"

-----------  Utilities ----------- 

guardEmptyList :: (a -> Bool) -> Maybe [a] -> Maybe [a]
guardEmptyList f v = filter f <$> v >>= go 
  where
    go [] = Nothing
    go xs = Just xs 

space :: Int -> T.Text
space n = T.unwords $ replicate n ""

isTrue :: Eq a => Maybe (Bool:|:a) -> Bool
isTrue = (== (Just $ AltLeft True))

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t = maybe t (\(first, rest) -> T.singleton (toUpper first) `T.append` rest) . T.uncons $ t

uncapitalizeFirst :: T.Text -> T.Text
uncapitalizeFirst t = maybe t (\(first, rest) -> T.singleton (toLower first) `T.append` rest) . T.uncons $ t
