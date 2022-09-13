{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections, TypeOperators #-}

module CDP.Gen.Program where

import Control.Arrow
import Data.List
import Data.List.Extra (splitOn)
import Data.Maybe
import Data.Char
import Data.Text (Text(..), unpack)
import qualified Text.Casing as C
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))
import qualified Data.Aeson as A

import qualified CDP.Definition as D

----------- Constants from the handwritten runtime of the CDP library ----------- 

eventClassName, eventResponseName, handleType, commandClassName, sendCommandName, sendCommandResultName, emptyReturnSig :: String
eventClassName         = "FromEvent"
eventResponseName      = "EventResponse"
handleType             = "Handle " <> eventsSumTypeName
commandClassName       = "Command"
sendCommandName        = "sendReceiveCommand"
sendCommandResultName  = "sendReceiveCommandResult"
emptyReturnSig         = "IO (Maybe Error)"

resultReturnSig :: String -> String
resultReturnSig resultTy = "IO (Either Error " <> resultTy <> ")" 

----------- Start of program generation ----------- 

type Program = String

genProgram :: [D.DomainsElt] -> Program
genProgram domainElts = unlines 
    [ eventsSumType evnsHS
    , eventResponseFromJSON evns evnsHS
    , unlines . map genDomain $ delts
    ]
  where
    evns    = allEventNames delts
    evnsHS  = allEventNamesHS delts
    delts   = validDomains domainElts

----------- Generation of global types / values ----------- 

eventsSumTypeName :: String
eventsSumTypeName = "Event"

eventsSumTypeConstructor :: String -> String
eventsSumTypeConstructor = ("EV" <>)

eventsSumType :: [String] -> String
eventsSumType eventNamesHS = (<> ("\n" <> space 3 <> derivingBase)) . 
        (unwords ["data", eventsSumTypeName, "= "] <>) .   
        intercalate " | " . 
        map (\c -> unwords [eventsSumTypeConstructor c, c]) $ eventNamesHS
        
eventResponseFromJSON :: [String] -> [String] -> String
eventResponseFromJSON eventNames eventNamesHS = unlines $
    [ unwords ["instance FromJSON", "(" <> eventResponseName, eventsSumTypeName, ") where"]
    , unwords [space 3, "parseJSON = A.withObject ", show eventResponseName, " $ \\obj -> do"]
    , unwords [space 7, "name", "<-", "obj", ".:", show "method"]
    , unwords [space 7, "case (name :: String) of"]
    ] ++ 
        (map (\(l,r) -> unwords [space 11, l, "->", r]) $ zip (map show $ eventNames)
            (map ((\c -> unwords [eventResponseName, proxy eventsSumTypeName, proxy c, ". fmap", eventsSumTypeConstructor c, "<$> obj .:?", show "params"])) eventNamesHS) ++ 
            [emptyCase eventResponseName])

----------- Generation of domain types / values ----------- 

genDomain :: D.DomainsElt -> String
genDomain domainElt = unlines . map unlines $
    [ map (genType dn)    . validTypes    $ domainElt
    , map (genEvent dn)   . validEvents   $ domainElt
    , map (genCommand dn) . validCommands $ domainElt 
    ]
  where
    dn = domainName domainElt

genType :: String -> D.TypesElt -> String
genType domainName telt = case D.typesEltEnum telt of
    Just enumValues -> genTypeEnum tn . map unpack $ enumValues
    Nothing         -> case tytelt of
            "object" -> maybe 
                (genTypeSynonynm domainName tn) 
                (genParamsType domainName tn) 
                tpelt
            ty       ->  unwords ["type", tn, "=", lty]                 
  where
    lty    = leftType domainName (Just . AltLeft $ tytelt) Nothing (D.typesEltItems telt)
    tytelt = D.typesEltType telt
    tpelt  = filter isValidParam <$> D.typesEltProperties telt
    tn     = typeNameHS domainName telt 

genTypeEnum :: String -> [String] -> String
genTypeEnum typeEnumName values = unlines
    [ unwords ["data", typeEnumName, "=", intercalate " | " constructors]
    , space 4 <> derivingOrd
    , genFromJSONInstanceEnum typeEnumName values constructors
    , genToJSONInstanceEnum typeEnumName values constructors
    ]
  where
    constructors = map (tyNameHS typeEnumName) values

genEvent :: String -> D.EventsElt -> String
genEvent domainName eventElt = unlines
    [ genEventReturnType domainName eventElt
    , genEventInstance domainName eventElt
    ]

genEventReturnType :: String -> D.EventsElt -> String
genEventReturnType domainName eventElt = maybe emptyParams genNonEmptyParams evelts
  where
    emptyParams = unlines 
        [ unwords ["data", evrn, "=", evrn]
        , space 4 <> derivingBase
        , genFromJSONInstanceEnum evrn [evrn] [evrn]
        ]
    genNonEmptyParams = genParamsType domainName evrn
    evelts = filter isValidParam <$> D.eventsEltParameters eventElt
    evrn = eventNameHS domainName eventElt

genEventInstance :: String -> D.EventsElt -> String
genEventInstance domainName eventElt = unlines $
    [ unwords ["instance", eventClassName, eventsSumTypeName, evrn, "where"]
    , unwords [space 3, "eventName  _ _    = ", show $ eventName domainName eventElt] -- TODO: parameterize
    , unwords [space 3, "fromEvent ev      = ", "case ev of", eventsSumTypeConstructor evrn, "v -> Just v; _ -> Nothing"] -- TODO: parameterize
    ]
  where
    evrn = eventNameHS domainName eventElt

genCommand :: String -> D.CommandsElt -> String
genCommand domainName commandElt = unlines
    [ paramsTypeDef
    , genCommandFn (isNothing pelts) (isNothing relts) cn ptn rtn
    , returns
    ]
  where
    cn   = commandName domainName commandElt 
    ptn  = commandParamsNameHS domainName commandElt
    rtn  = commandNameHS domainName commandElt 

    paramsTypeDef = maybe "" (genParamsType domainName ptn) pelts
    pelts = filter isValidParam <$> D.commandsEltParameters commandElt

    returns = maybe "" (genNonEmptyReturns) relts
    genNonEmptyReturns relts = unlines
        [ genReturnType domainName rtn relts
        , genFromJSONInstance rtn
        , genCommandInstance 
            (commandNameHS domainName commandElt) 
            (commandName domainName commandElt)
        ]
    relts = filter isValidReturn <$> D.commandsEltReturns commandElt

genCommandInstance :: String -> String -> String
genCommandInstance commandNameHS commandName =
    unlines $
        [ unwords ["instance", commandClassName, commandNameHS, "where"]
        , unwords [space 3, "commandName _ =", show commandName]
        ]

genCommandFn :: Bool -> Bool -> String -> String -> String -> String
genCommandFn isEmptyParams isEmptyReturn commandName paramsTypeName returnTypeName = unlines
    [ fnType
    , unwords [fnHeader, fnBody]
    ]
  where
    fnType = unwords
        [ fnName, "::"
        , intercalate " -> " $ [handleType] ++ 
            (if isEmptyParams then [] else [paramsTypeName]) ++ 
            [returnTypeSig]
        ]
    fnHeader = unwords [fnName, "handle", if isEmptyParams then "=" else "params ="]
    fnBody   = unwords 
        [ if isEmptyReturn then sendCommandName else sendCommandResultName
        , "handle"
        , show commandName
        , if isEmptyParams then "(Nothing :: Maybe ())" else "(Just params)"
        ]
    returnTypeSig  = if isEmptyReturn then emptyReturnSig else resultReturnSig returnTypeName
    fnName         = uncapitalizeFirst returnTypeName

genParamsType :: String -> String -> [D.ParametersElt] -> String
genParamsType domainName paramsTypeName []     = genTypeSynonynm domainName paramsTypeName
genParamsType domainName paramsTypeName paramElts = unlines
    [ unlines enumDecls
    , unwords ["data", paramsTypeName, "=", paramsTypeName, "{"]
    , intercalate ",\n" fields
    , "} " <> derivingGeneric
    , genToJSONInstance paramsTypeName
    , genFromJSONInstance paramsTypeName
    ]
  where
    fields = map toField fieldSigDecls
    toField (fn, ftn, _) = unwords [space 3, fn, "::", ftn]

    enumDecls    = catMaybes . map (\(_,_,d) -> d) $ fieldSigDecls
    fieldSigDecls = map peltToFieldSigDecl paramElts

    peltToFieldSigDecl pelt = case D.parametersEltEnum pelt of
        Just enumValues -> (fn, ftn, ) . Just . genTypeEnum ftn . map unpack $ enumValues
        Nothing         -> (fn, , Nothing) $ genEltType domainName (isTrue . D.parametersEltOptional $ pelt)
                (D.parametersEltType pelt)
                (D.parametersEltRef pelt) 
                (D.parametersEltItems pelt)
      where
        ftn = tyNameHS "" fn
        fn = fieldNameHS paramsTypeName . unpack . D.parametersEltName $ pelt

genTypeSynonynm :: String -> String -> String
genTypeSynonynm domainName typeName = unwords ["type", typeName, "=", typeCDPToHS domainName "object" Nothing]

genReturnType :: String -> String -> [D.ReturnsElt] -> String
genReturnType domainName returnTypeName returnElts = unlines
    [ unwords ["data", returnTypeName, "=", returnTypeName, "{"]
    , intercalate ",\n" fields
    , "} " <> derivingGeneric
    ]
  where
    fields = map ((space 4 <>) . reltToField) $ returnElts
    reltToField relt = unwords
        [ fieldNameHS returnTypeName . unpack . D.returnsEltName $ relt
        , "::"
        , reltToTypeSig relt
        ]

    reltToTypeSig relt = genEltType domainName (isTrue . D.returnsEltOptional $ relt) 
            (D.returnsEltType relt)
            (D.returnsEltRef relt) 
            (D.returnsEltItems relt)
            
----------- Generation rules ----------- 

genFromJSONInstance :: String -> String
genFromJSONInstance name = unlines
    [ unwords ["instance FromJSON ", name, "where"]
    , unwords [space 3, "parseJSON = A.genericParseJSON", fromJSONOpts (length name)]
    ]

genToJSONInstance :: String -> String
genToJSONInstance name = unlines
    [ unwords ["instance ToJSON", name, " where"]
    , unwords [space 3, "toJSON = A.genericToJSON", toJSONOpts (length name)]
    ]

genFromJSONInstanceEnum :: String -> [String] -> [String] -> String
genFromJSONInstanceEnum name vals hsVals = unlines $
    [ unwords ["instance FromJSON", name, "where"]
    , unwords [space 3, "parseJSON = A.withText ", show name, " $ \\v -> do"]
    , unwords [space 6, "case v of"]
    ] ++ (map (\(v, hsv) -> unwords [space 9, v, "->", hsv]) $ zip (map show vals) (map ("pure $ " <>) hsVals) ++ 
        [emptyCase name])
    
genToJSONInstanceEnum :: String -> [String] -> [String] -> String
genToJSONInstanceEnum name vals hsVals = unlines $
    [ unwords ["instance ToJSON", name, "where"]
    , unwords [space 3, "toJSON v = A.String $"]
    , unwords [space 6, "case v of"]
    ] ++ (map (\(v, hsv) -> unwords [space 9, hsv, "->", show v]) $ zip vals hsVals)

toJSONOpts   :: Int -> String
toJSONOpts   n    = unwords ["A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop", show n, ", A.omitNothingFields = True}"]

fromJSONOpts :: Int -> String
fromJSONOpts n    = unwords ["A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop", show n, "}"]

genEltType :: String -> Bool -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (Text:|:[(Maybe A.Value)])) -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> String
genEltType name isOptional t1 t2 items = (if isOptional then "Maybe " else "") <> (leftType name t1 t2 items)

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

convertType :: String -> String -> String
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
    [otherDomain, ty] -> tyNameHS otherDomain ty
    _ -> tyNameHS domain s 

domainName :: D.DomainsElt -> String
domainName = unpack . D.domainsEltDomain

validDomains :: [D.DomainsElt] -> [D.DomainsElt]
validDomains ds = filter (not . hasExperimentalDependencies) . filter (not . isTrue . D.domainsEltExperimental) $ ds
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

allEventNamesHS :: [D.DomainsElt] -> [String]
allEventNamesHS = concatMap (\(d, evs) -> map (eventNameHS $ domainName d) evs) . map (id &&& validEvents)

allEventNames :: [D.DomainsElt] -> [String]
allEventNames = concatMap (\(d, evs) -> map (eventName $ domainName d) evs) . map (id &&& validEvents)

eventName   :: String -> D.EventsElt -> String
eventName   domainName ev = (domainName <>) . ("." <>) . unpack . D.eventsEltName $ ev
eventNameHS :: String -> D.EventsElt -> String
eventNameHS domainName ev = tyNameHS domainName (unpack . D.eventsEltName $ ev)

commandName   :: String -> D.CommandsElt -> String
commandName domainName c = (domainName <>) . ("." <>) . unpack . D.commandsEltName $ c
commandNameHS :: String -> D.CommandsElt -> String
commandNameHS domainName c = tyNameHS domainName (unpack . D.commandsEltName $ c)
commandParamsNameHS :: String -> D.CommandsElt -> String
commandParamsNameHS domainName c = paramsTypePrefix $ commandNameHS domainName c

typeNameHS :: String -> D.TypesElt -> String
typeNameHS domainName t = tyNameHS domainName (unpack . D.typesEltId $ t)

tyNameHS :: String -> String -> String
tyNameHS prefix tyName = C.pascal prefix <> C.pascal tyName

fieldNameHS :: String -> String -> String
fieldNameHS tyName fieldName = (uncapitalizeFirst tyName <>) $ C.pascal fieldName 

paramsTypePrefix :: String -> String
paramsTypePrefix = ("P" <>)

proxy :: String -> String
proxy s = "(Proxy :: Proxy " <> s <> ")"

emptyCase :: String -> (String, String)
emptyCase name = ("_", unwords ["fail", show $ "failed to parse " <> name])

derivingBase    :: String
derivingBase    = "deriving (Eq, Show, Read)"
derivingOrd     :: String
derivingOrd     = "deriving (Ord, Eq, Show, Read)"
derivingGeneric :: String
derivingGeneric = "deriving (Generic, Eq, Show, Read)"

-----------  Utilities ----------- 

space :: Int -> String
space n = unwords $ replicate n " "

isTrue :: Eq a => Maybe (Bool:|:a) -> Bool
isTrue = (== (Just $ AltLeft True))

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (hd:tl) = toUpper hd : tl

uncapitalizeFirst :: String -> String
uncapitalizeFirst [] = []
uncapitalizeFirst (hd:tl) = toLower hd : tl
