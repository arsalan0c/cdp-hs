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

newtype StringAlt = StringAlt String
instance Show StringAlt where
    show (StringAlt s) = s

type Program = String

eventClassName, eventResponseName, handleType, commandClassName, sendCommandName, sendCommandResultName, emptyReturnSig :: String
eventClassName         = "FromEvent"
eventResponseName      = "EventResponse"
handleType             = "Handle " <> eventSumTypeName
commandClassName       = "Command"
sendCommandName        = "sendReceiveCommand"
sendCommandResultName  = "sendReceiveCommandResult"
emptyReturnSig         = "IO (Maybe Error)"

resultReturnSig :: String -> String
resultReturnSig resultTy = "IO (Either Error " <> resultTy <> ")" 

--

eventSumTypeName :: String
eventsSumTypeName = "Event"

eventsSumTypeConstructor :: String -> String
eventsSumTypeConstructor = ("EV" <>)

eventsSumType :: [String] -> String
eventsSumType eventNamesHS = (<> "\n    " <> derivingBase) . 
        (unwords ["data", eventsSumTypeName, "= "] <>) .   
        intercalate " | " . 
        map (\c -> unwords [eventsSumTypeConstructor c, c]) $ eventNamesHS
        
eventResponseFromJSON :: [String] -> [String] -> String
eventResponseFromJSON eventNames eventNamesHS = unlines $
    [ unwords ["instance FromJSON", "(" <> eventResponseName, eventsSumTypeName, ") where"]
    , unwords ["    parseJSON = A.withObject ", show eventResponseName, " $ \\obj -> do"]
    , unwords ["        name", "<-", "obj", ".:", show "method"]
    , unwords ["        case (name :: String) of"]
    ] ++ 
        (map (\(l,r) -> unwords ["               ", l, "->", r]) $ zip (map show $ eventNames)
            (map ((\c -> unwords [eventResponseName, proxy eventsSumTypeName, proxy c, ". fmap", eventsSumTypeConstructor c, "<$> obj .:?", show "params"])) eventNamesHS) ++ 
            [emptyCase eventResponseName])

-- 

domainName :: D.DomainsElt -> String
domainName = unpack . D.domainsEltDomain

validDomains :: [D.DomainsElt] -> [D.DomainsElt]
validDomains ds = filter (not . hasExperimentalDependencies) . filter (not . isTrue . D.domainsEltExperimental)
  where
    hasExperimentalDependencies d = any (`elem` experimentalDomains) . (fromMaybe [] . D.domainsEltDependencies) $ d
    experimentalDomains = map D.domainsEltDomain . filter (isTrue . D.domainsEltExperimental) $ ds

isValidEvent :: D.EventsElt -> Bool
isValidEvent ev = (not . isTrue . D.eventsEltExperimental $ ev) && (not . isTrue . D.eventsEltDeprecated $ ev)

isValidCommand :: D.CommandsElt -> Bool
isValidCommand = (not . isTrue . D.commandsEltDeprecated) && (not . isTrue . D.commandsEltExperimental)

isValidParam :: D.ParametersElt -> Bool
isValidParam = (not . isTrue . D.parametersEltDeprecated) && (not . isTrue . D.parametersEltExperimental)

isValidReturn :: D.ReturnsElt -> Bool
isValidReturn = (not . isTrue . D.returnsEltDeprecated) && (not . isTrue . D.returnsEltExperimental)

isValidType :: D.TypesElt -> Bool
isValidType = (not . isTrue . D.typesEltDeprecated) && (not . isTrue . D.typesEltExperimental)

validEvents :: D.DomainsElt -> [D.EventsElt]
validEvents = filter isValidEvent . fromMaybe [] . D.domainsEltEvents

validCommands :: D.DomainsElt -> [D.CommandsElt]
validCommands = filter isValidCommand . D.domainsEltCommands

validTypes :: D.DomainsElt -> [D.TypesElt]
validTypes = filter isValidType . D.domainsEltTypes

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

typeNameHS :: String -> D.TypeElt -> String
typeNameHS domainName t = tyNameHS domainName (unpack . D.typesEltId $ t)

tyNameHS :: String -> String -> String
tyNameHS prefix tyName = unwords . map C.pascal $ [prefix, tyName]

fieldNameHS :: String -> String -> String
fieldNameHS tyName fieldName = (uncapitalizeFirst tyName <>) $ C.pascal fieldName 

paramsTypeName :: String -> String
paramsTypeName = ("P" <>)

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

space :: Int -> String
space n = replicate n " "

genEventInstance :: String -> D.EventsElt -> String
genEventInstance domainName eventElt = unlines $
        [ unwords ["instance", eventClassName, eventsSumTypeName, evnHS, "where"]
        , unwords [space 3, "eventName  _ _    = ", show $ eventName domainName eventElt] -- TODO: parameterize
        , unwords [space 3, "fromEvent ev      = ", "case ev of", eventsSumTypeConstructor evnHS, "v -> Just v; _ -> Nothing"] -- TODO: parameterize
        ]
  where
    evnHS = eventNameHS domainName eventElt

genEventReturnType :: String -> D.EventsElt -> String
genEventReturnType domainName eventElt = unlines
    [ unwords ["data", evn, "=", evn]
    , maybe emptyParams (map genNonEmptyParam . filter isValidParam) . 
        fromMaybe [] .
        D.eventsEltParameters $ eventElt
    , space 4 <> derivingBase
    ]
  where
    emptyParams       = genFromJSONInstanceEnum evnHS
    genNonEmptyParam  = genParamsType domainName evn
    evnHS = eventNameHS domainName eventElt

genType :: String -> D.TypesElt -> String
genType domainName telt = case D.typesEltEnum telt of
    Just enumValues -> genTypeEnum name . map unpack $ enumValues
    Nothing         -> case tyTelt of
            "object" -> maybe 
                (genParamsTypeNoParams domainName name) 
                (genParamsType dn name) 
                (D.typesEltProperties te)
            ty       ->  unwords ["type", name, "=", tyl]                 
  where
    tyl    = leftType domainName (Just . AltLeft $ tyTelt) Nothing (D.typesEltItems telt)
    name   = typeNameHS domainName telt 
    tyTelt = D.typesEltType telt

genTypeEnum :: String -> [String] -> String
genTypeEnum typeEnumName values = unlines
    [ unwords ["data", typeEnumName, "=", intercalate " | " hsValues]
    , "    " <> derivingGeneric
    , genFromJSONInstanceEnum typeEnumName values hsValues
    , genToJSONInstanceEnum typeEnumName values hsValues
    ]
  where
    hsValues = map (tyNameHS typeEnumName) values

genDomain :: D.DomainsElt -> String
genDomain domainElt = unlines . map unlines $
    [ map (genEventReturnType dn) vevs
    , map (genEventInstance dn) vevs
    , map (genType dn) . validTypes $ domainElt
    , map (genCommand dn) . validCommands $ domainElt 
    ]
  where
    vevs = validEvents domainElt
    dn = domainName de 
    
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
    paramsTypeName = paramsTypeName returnTypeName
    fnName         = uncapitalizeFirst returnTypeName

genCommandInstance :: String -> String -> String
genCommandInstance commandNameHS commandName =
    unlines $
        [ unwords ["instance", commandClassName, commandNameHS, "where"]
        , unwords [space 3, "commandName _ =", show commandName]
        ]
    
genReturnType :: String -> String -> [D.ReturnsElt] -> String
genReturnType domainName returnTypeName returnElts = unlines
    [ unwords ["data", name, "=", name, "{"]
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

-- enum account
genParamsType :: String -> String -> [D.ParamsElt] -> String
genParamsType domainName paramsTypeName []     = genParamsTypeNoParams domainName name 
genParamsType domainName paramsTypeName params = unlines
    [ unwords ["data", name, "=", name, "{"]
    , intercalate ",\n" fields
    , "} " <> derivingGeneric
    ]
  where
    fields = map ((space 4 <>) . peltToField) $ paramElts
    peltToField pelt = unwords
        [ fieldNameHS paramsTypeName . unpack . D.parametersEltName $ pelt
        , "::"
        , peltToTypeSig pelt
        ]

    peltToTypeSig pelt = genEltType domainName (isTrue . D.parametersEltOptional $ pelt)
            (D.parametersEltType pelt)
            (D.parametersEltRef pelt) 
            (D.parametersEltItems pelt)

genParamsTypeNoParams :: String -> String -> String
genParamsTypeNoParams domainName name = unwords ["type", name, "=", typeCDPToHS domainName "object" Nothing]

genCommand :: String -> D.CommandsElt -> String
genCommand domainName commandElt = unlines
    [ paramsTypeDecl
    , genCommandFn isEmptyParams isEmptyReturn cn ptn rtn
    , returnTypeDecl
    , returnTypeFromJSON
    , commandInstance
    ]
  where
    cn   = commandName domainName commandElt 
    ptn  = paramsTypeName cnHS
    rtn  = commandNameHS domainName commandElt 

    paramsTypeDecl     = maybe "" (genParamsType domainName ptn) (D.commandsEltParameters commandElt)
    returnTypeDecl     = if isEmptyReturn then "" else genReturnType domainName rtn relts
    returnTypeFromJSON = genFromJSONInstance rtn
    commandInstance    = if isEmptyReturn then "" else genCommandInstance 
            (commandNameHS domainName commandElt) 
            (commandName domainName commandElt)

    isEmptyReturn = length relts == 0
    relts = filter isValidReturn .
        fromMaybe [] . D.commandsEltReturns $ commandElt

    isEmptyParams = length pelts == 0
    pelts = filter isValidParam . 
        fromMaybe [] . D.commandsEltParameters $ commandElt

genProgram :: [D.DomainsElt] -> Program
genProgram domainElts = unlines 
    [ eventsSumType . allEventNamesHS $ domainElts
    , eventResponseFromJSON . allEventNames $ domainElts
    , unlines . map genDomain . filter validDomains $ domainElts
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
    [otherDomain, ty] -> otherDomain <> ty
    _ -> domain <> s 

isTrue :: Maybe (Bool:|:a) -> Bool
isTrue = (== (Just $ AltLeft True))

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (hd:tl) = toUpper hd : tl

uncapitalizeFirst :: String -> String
uncapitalizeFirst [] = []
uncapitalizeFirst (hd:tl) = toLower hd : tl
