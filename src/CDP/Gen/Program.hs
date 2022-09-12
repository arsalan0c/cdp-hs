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


-- valid elements
-- name
-- prefix
-- c
-- casing
-- deriving
-- fromJson / toJson
-- all domains vs per-domain


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

eventsSumTypeName      = "Event"


        
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

validEvents :: D.DomainsElt -> [D.EventsElt]
validEvents = filter isValidEvent . fromMaybe [] . D.domainsEltEvents

validCommands :: D.DomainsElt -> [D.CommandsElt]
validCommands = filter isValidCommand . D.domainsEltCommands

allEventNamesHS :: [D.DomainsElt] -> [String]
allEventNamesHS = concatMap (\(d, evs) -> map (eventNameHS $ domainName d) evs) . map (id &&& validEvents)

allEventNames :: [D.DomainsElt] -> [String]
allEventNames = concatMap (\(d, evs) -> map (eventName $ domainName d) evs) . map (id &&& validEvents)

eventName   :: String -> D.EventsElt -> String
eventName   domainName  ev = (domainName <>) . ("." <>) . unpack . D.eventsEltName $ ev
eventNameHS :: String -> D.EventsElt -> String
eventNameHS domainName   ev = (domainName <>) . C.pascal . unpack . D.eventsEltName $ ev

commandName   :: String -> D.CommandsElt -> String
commandName domainName  c  = (domainName <>) . ("." <>) . unpack . D.commandsEltName $ c
commandNameHS :: String -> D.CommandsElt -> String
commandNameHS domainName c = (domainName <>) . C.pascal . unpack . D.commandsEltName $ c

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


genEventInstance :: D.EventsElt -> String
genEventInstance eventElt = unlines $
        [ unwords ["instance", eventClassName, eventsSumTypeName, evnHS, "where"]
        , unwords [space 3, "eventName  _ _    = ", show $ eventName dn eventElt] -- TODO: parameterize
        , unwords [space 3, "fromEvent ev      = ", "case ev of", eventsSumTypeConstructor evnHS, "v -> Just v; _ -> Nothing"] -- TODO: parameterize
        ]
  where
    evnHS = eventNameHS dn eventElt

genEventReturnType :: String -> D.EventsElt -> String
genEventReturnType domainName eventElt = unlines
    [ unwords ["data", evn, "=", evn]
    , maybe emptyParams (map genNonEmptyParam . filter isValidParam) . 
        fromMaybe [] .
        D.eventsEltParameters $ eventElt
    , space 4 <> derivingBase
    ]
  where
    emptyParams       = genFromJSONInstanceEnum evnHS [evnHS] [evnHS]
    genNonEmptyParam  = genTypeObj dn evn ""
    evnHS = eventNameHS dn eventElt

genDomain :: D.DomainsElt -> String
genDomain domainElt = unlines
        [ unlines $ map (genEventReturnType dn) vevs
        , unlines $ map genEventInstance vevs
        , genTypes dn . D.domainsEltTypes $ de
        , unlines $ map (genCommand dn) . map validCommands $ domainElt 
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
    fnType   = unwords
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
        [ fieldName . unpack . D.returnsEltName $ relt
        , "::"
        , reltToTypeSig relt
        ]

    reltToTypeSig relt = genEltType domainName (isTrue . D.returnsEltOptional $ relt) 
            (D.returnsEltType relt)
            (D.returnsEltRef relt) 
            (D.returnsEltItems relt)

    fieldName = (uncapitalizeFirst returnTypeName <>) . C.pascal

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

    paramsTypeDecl = maybe "" (genTypeObj domainName ptn "") (D.commandsEltParameters commandElt)
    returnTypeDecl     = if isEmptyReturn then "" else genReturnType domainName rtn relts
    returnTypeFromJSON = genFromJSONInstance rtn
    commandInstance = if isEmptyReturn then "" else genCommandInstance 
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
  where                    
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
            , "    " <> derivingGeneric
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
            paramNameToHSName = (uncapitalizeFirst) . (fieldPrefix <>) . (name <>) . capitalizeFirst
        in
        if length validParams == 0 then "" else
        unlines [ "data " <> name <> " = " <> name <> " {"
                , intercalate ",\n" . 
                    map (\param -> space 4 <> (paramNameToHSName . unpack . D.parametersEltName $ param) <> 
                        " :: " <> 
                        (genEltType dn (isTrue . D.parametersEltOptional $ param) 
                                (D.parametersEltType param)
                                (D.parametersEltRef param) 
                                (D.parametersEltItems param))) $ reqParams ++ optParams
                , "} " <> derivingGeneric
                , genFromJSONInstance name
                , genToJSONInstance name
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

isTrue = (== (Just $ AltLeft True))

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (hd:tl) = toUpper hd : tl

uncapitalizeFirst :: String -> String
uncapitalizeFirst [] = []
uncapitalizeFirst (hd:tl) = toLower hd : tl

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
