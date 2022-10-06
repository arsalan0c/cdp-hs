{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections, TypeOperators #-}

module CDP.Gen.Program where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import qualified Data.Text as T
import qualified Text.Casing as C
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))
import qualified Data.Aeson as A

import qualified CDP.Definition as D

----------- Constants from the handwritten runtime of the CDP library ----------- 

eventClassName, eventResponseName, handleType, commandClassName, sendCommandName, sendCommandResultName, emptyReturnSig :: T.Text
eventClassName         = "FromEvent"
eventResponseName      = "EventResponse"
handleType             = "Handle ev" -- <> eventsSumTypeName
handleTypeName         = "Handle"
commandClassName       = "Command"
sendCommandName        = "sendReceiveCommand"
sendCommandResultName  = "sendReceiveCommandResult"
emptyReturnSig         = "IO ()"
configType              = "Config(..)"

resultReturnSig :: T.Text -> T.Text
resultReturnSig resultTy = "IO " <> resultTy 

----------- Start of program generation ----------- 

data Program = Program
    { pComponents       :: Map.Map ComponentName T.Text
    , pComponentImports :: T.Text
    , pEvents           :: T.Text
    }

data Context = Context { ctxDomainComponents :: DomainComponents }

genProgram :: T.Text -> T.Text -> [D.DomainsElt] -> Program
genProgram domainExtensions domainImports domainElts = Program
    { pComponents        = allComponents ctx domainExtensions domainImports $ Map.elems dc
    , pComponentImports  = T.unlines $ allComponentImports ctx
    , pEvents            = genEvents ctx delts 
    }
  where
    ctx    = Context dc
    dc     = domainComponents delts
    delts  = validDomains domainElts

----------- Generation of global types / values ----------- 

genProtocolModule :: T.Text -> T.Text -> [ComponentName] -> T.Text -> T.Text
genProtocolModule extensions imports names source = T.unlines
    [ extensions
    , ""
    , protocolModuleHeader names
    , imports
    , source
    ]

allComponentImports :: Context -> [T.Text]
allComponentImports = Set.toList . Set.fromList . 
    concatMap ((\n -> [importDomain False n, importDomain True n]) . unComponentName . cName) . Map.elems . ctxDomainComponents

allComponents :: Context -> T.Text -> T.Text -> [Component] -> Map.Map ComponentName T.Text
allComponents ctx extensions imports components = Map.fromList . 
    map (cName &&& genComponent ctx extensions imports) $ 
    components

genEvents :: Context -> [D.DomainsElt] -> T.Text
genEvents ctx delts = T.unlines $ 
    [ eventsSumType evnCs evnQNs
    , eventResponseFromJSON evnQNs evnCs evns 
    , T.unlines $ evnInsts 
    ]
  where
    evnInsts = map evnInst $ zip evnQNs . zip evnCs $ evns
    evnInst (evnHS, (evnC, evn)) = genEventInstance evnHS evnC evn

    evnQNs = allEventQualifiedNames ctx delts
    evnCs  = map eventsSumTypeConstructor evnHS'
    evnHS' = allEventNamesHS delts
    evns   = allEventNames delts

allEventNamesHS :: [D.DomainsElt] -> [T.Text]
allEventNamesHS = concatMap f . map (id &&& validEvents)
  where
    f (d, evelts) = map (eventNameHS $ domainName d) evelts

allEventQualifiedNames :: Context -> [D.DomainsElt] -> [T.Text]
allEventQualifiedNames ctx = concatMap f
  where
    f d = map (g (cn d) (domainName d)) (validEvents d)
    g cn dn evelt = domainQualifiedName cn (eventNameHS dn evelt)
    cn d = unComponentName . domainToComponentName ctx . domainName $ d
    dn d = domainName d

allEventNames :: [D.DomainsElt] -> [T.Text]
allEventNames = concatMap (\(d, evs) -> map (eventName $ domainName d) evs) . map (id &&& validEvents)

eventsSumTypeName :: T.Text
eventsSumTypeName = "Event"

eventsSumTypeConstructor :: T.Text -> T.Text
eventsSumTypeConstructor = ("EV" <>)

eventsSumType :: [T.Text] -> [T.Text] -> T.Text
eventsSumType constructors qualifiedNames = (<> ("\n" <> space 3 <> derivingBase)) . 
        (T.unwords ["data", eventsSumTypeName, "= "] <>) .
        T.intercalate " | " . 
        map f $ zip constructors qualifiedNames
  where
    f (evnC, evnQN) = T.unwords [evnC, evnQN]
        
eventResponseFromJSON :: [T.Text] -> [T.Text] -> [T.Text] -> T.Text
eventResponseFromJSON eventNamesHS eventConstructorNames eventNames = T.unlines $
    [ T.unwords ["instance FromJSON", "(" <> eventResponseName, eventsSumTypeName, ") where"]
    , T.unwords [space 3, "parseJSON = A.withObject ", (T.pack . show) eventResponseName, " $ \\obj -> do"]
    , T.unwords [space 7, "name", "<-", "obj", ".:", (T.pack . show) "method"]
    , T.unwords [space 7, "case (name :: String) of"]
    ] ++ 
        (map (\(l,r) -> T.unwords [space 11, l, "->", r]) $ zip (map (T.pack . show) $ eventNames)
            (map ((\(evnHS, evnC) -> T.unwords [eventResponseName, proxy eventsSumTypeName, proxy evnHS, ". fmap", evnC, "<$> obj .:?", (T.pack . show) "params"])) $ zip eventNamesHS eventConstructorNames) ++ 
            [emptyCase eventResponseName])
      
genEventInstance :: T.Text -> T.Text -> T.Text -> T.Text
genEventInstance eventNameHS eventConstructorName eventName = T.unlines $
    [ T.unwords ["instance", eventClassName, eventsSumTypeName, eventNameHS, "where"]
    , T.unwords [space 3, "eventName  _ _    = ", (T.pack . show) $ eventName]
    , T.unwords [space 3, "fromEvent ev      = ", "case ev of", eventConstructorName, "v -> Just v; _ -> Nothing"]
    ]
 
----------- Generation of domain types / values -----------

genComponent :: Context -> T.Text -> T.Text -> Component -> T.Text
genComponent ctx extensions imports component = T.intercalate "\n\n" $
    [ extensions
    , formatComponentDescription component
    , domainModuleHeader cn
    , imports
    , T.unlines $ map (importDomain True) deps
    , T.unlines $ map (genDomain ctx) delts
    ]
  where
    deps  = Set.toList . Set.fromList . map unComponentName . 
        map (domainToComponentName ctx) . concatMap snd . Map.elems $ cDomDeps component
    delts = map fst . Map.elems $ cDomDeps component
    cn    = unComponentName . cName $ component

genDomain :: Context -> D.DomainsElt -> T.Text
genDomain ctx domainElt = T.intercalate "\n\n" $
    [ T.unlines $ map (genType ctx dn)             . validTypes    $ domainElt
    , T.unlines $ map (genEventReturnType ctx dn)  . validEvents   $ domainElt
    , T.unlines $ map (genCommand ctx dn)          . validCommands $ domainElt 
    ]
  where
    dn    = domainName domainElt

genType :: Context -> T.Text -> D.TypesElt -> T.Text
genType ctx domainName telt = T.unlines . (desc :) . pure $ case D.typesEltEnum telt of
    Just enumValues -> genTypeEnum ctx tn enumValues
    Nothing         -> case tytelt of
            "object" -> maybe 
                (genTypeSynonynm ctx domainName tn) 
                (genParamsType ctx domainName tn) 
                tpeltsM
            ty       ->  T.unwords ["type", tn, "=", lty]                 
  where
    desc     = let td = "Type '" <> (domainName <> "." <> D.typesEltId telt) <> "' ." in 
        formatDescription 0 . (td <>) . maybe "" fromAltLeft $ D.typesEltDescription telt
    lty      = leftType ctx domainName (Just . AltLeft $ tytelt) Nothing (D.typesEltItems telt)
    tytelt   = D.typesEltType telt
    tpeltsM  = guardEmptyList isValidParam $ D.typesEltProperties telt
    tn       = typeNameHS domainName telt 

genTypeEnum :: Context -> T.Text -> [T.Text] -> T.Text
genTypeEnum _ typeEnumName values = T.unlines . filter ((/= 0) . T.length) $
    [ T.unwords ["data", typeEnumName, "=", T.intercalate " | " constructors]
    , space 4 <> derivingOrd
    , genFromJSONInstanceEnum typeEnumName values constructors
    , genToJSONInstanceEnum typeEnumName values constructors
    ]
  where
    constructors = map (tyNameHS typeEnumName) values

genEventReturnType :: Context -> T.Text -> D.EventsElt -> T.Text
genEventReturnType ctx domainName eventElt = T.unlines . (desc :) . pure $
    maybe emptyParams genNonEmptyParams eveltsM
  where
    desc = formatDescription 0 $ "Type of the '" <> 
        (eventName domainName eventElt) <> "' event."
    emptyParams = T.unlines 
        [ T.unwords ["data", evrn, "=", evrn]
        , space 4 <> derivingBase
        , genFromJSONInstanceEnum evrn [evrn] [evrn]
        ]
    genNonEmptyParams = genParamsType ctx domainName evrn
    eveltsM = guardEmptyList isValidParam $ D.eventsEltParameters eventElt
    evrn = eventNameHS domainName eventElt

genCommand :: Context -> T.Text -> D.CommandsElt -> T.Text
genCommand ctx domainName commandElt = T.unlines . catMaybes $
    [ ((pdesc <> "\n") <>) <$> paramsTypeDef
    , Just desc
    , Just $ genCommandFn ctx (peltsM == Nothing) (reltsM == Nothing) cn ptn rtn
    , returns
    ]
  where
    pdesc = formatDescription 0 $ "Parameters of the '" <> (commandFnName rtn) <> "' command."
    desc = let td = "Function for the '" <> cn <> "' command.\n" in
        formatDescription 0 . T.intercalate "\n" . catMaybes $ 
            [ Just $ (td <>) . maybe "" fromAltLeft $ D.commandsEltDescription commandElt
            , const ("Parameters: '" <> ptn <> "'") <$> peltsM
            , const ("Returns: '" <> rtn <> "'") <$> reltsM
            ]
    cn   = commandName domainName commandElt 
    ptn  = commandParamsNameHS domainName commandElt
    rtn  = commandNameHS domainName commandElt 

    paramsTypeDef = genParamsType ctx domainName ptn <$> peltsM
    peltsM = guardEmptyList isValidParam $ D.commandsEltParameters commandElt

    returns = genNonEmptyReturns <$> reltsM
    genNonEmptyReturns relts = T.unlines
        [ genReturnType ctx domainName rtn relts
        , genFromJSONInstance rtn
        , genCommandInstance ctx
            (commandNameHS domainName commandElt) 
            (commandName domainName commandElt)
        ]

    reltsM = guardEmptyList isValidReturn $ D.commandsEltReturns commandElt

genCommandInstance :: Context -> T.Text -> T.Text -> T.Text
genCommandInstance _ commandNameHS commandName =
    T.unlines $
        [ T.unwords ["instance", commandClassName, commandNameHS, "where"]
        , T.unwords [space 3, "commandName _ =", (T.pack . show) commandName]
        ]

genCommandFn :: Context -> Bool -> Bool -> T.Text -> T.Text -> T.Text -> T.Text
genCommandFn _ isEmptyParams isEmptyReturn commandName paramsTypeName returnTypeName = T.unlines
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
    fnName         = commandFnName returnTypeName

genParamsType :: Context -> T.Text -> T.Text -> [D.ParametersElt] -> T.Text
genParamsType ctx domainName paramsTypeName []        = genTypeSynonynm ctx domainName paramsTypeName
genParamsType ctx domainName paramsTypeName paramElts = T.unlines . filter ((> 0) . T.length) $
    [ T.unlines enumDecls
    , T.unwords ["data", paramsTypeName, "=", paramsTypeName, "{"]
    , T.intercalate ",\n" fields
    , "} " <> derivingGeneric
    , genToJSONInstance paramsTypeName
    , genFromJSONInstance paramsTypeName
    ]
  where
    fields = formatFieldDescription . map toField $ fieldSigDecls
    toField ((fn,ftn,_),descM) = (T.unwords [fn, "::", ftn], fmap fromAltLeft descM)

    enumDecls    = catMaybes . map (\((_,_,d),_) -> d) $ fieldSigDecls
    fieldSigDecls = map (peltToFieldSigDecl &&& D.parametersEltDescription) paramElts

    peltToFieldSigDecl pelt = case D.parametersEltEnum pelt of
        Just enumValues -> (fn, ftn, ) . Just $ genTypeEnum ctx ftn enumValues
        Nothing         -> (fn, , Nothing) $ genEltType ctx domainName (isTrue . D.parametersEltOptional $ pelt)
                (D.parametersEltType pelt)
                (D.parametersEltRef pelt) 
                (D.parametersEltItems pelt)
      where
        ftn = tyNameHS "" fn
        fn = fieldNameHS paramsTypeName . D.parametersEltName $ pelt

genTypeSynonynm :: Context -> T.Text -> T.Text -> T.Text
genTypeSynonynm ctx domainName typeName = T.unwords ["type", typeName, "=", typeCDPToHS ctx domainName "object" Nothing]

genReturnType :: Context -> T.Text -> T.Text -> [D.ReturnsElt] -> T.Text
genReturnType ctx domainName returnTypeName returnElts = T.unlines
    [ desc
    , T.unwords ["data", returnTypeName, "=", returnTypeName, "{"]
    , T.intercalate ",\n" fields
    , "} " <> derivingGeneric
    ]
  where
    desc = formatDescription 0 $ "Return type of the '" <> 
        (commandFnName returnTypeName) <> "' command."
    fields = formatFieldDescription . 
        map (reltToField &&& fmap fromAltLeft . D.returnsEltDescription) $ returnElts
    reltToField relt = T.unwords
        [ fieldNameHS returnTypeName . D.returnsEltName $ relt
        , "::"
        , reltToTypeSig relt
        ]

    reltToTypeSig relt = genEltType ctx domainName (isTrue . D.returnsEltOptional $ relt) 
            (D.returnsEltType relt)
            (D.returnsEltRef relt) 
            (D.returnsEltItems relt)
            
----------- Generation rules ----------- 
----- Docs    -----
formatComponentDescription :: Component -> T.Text
formatComponentDescription component = T.unlines
    [ "{- |"
    , T.intercalate "\n" $ map f delts
    , "-}"
    ]
  where
    f delt = T.unwords 
        [ space 2
        , domainName delt
        , flip (maybe "") (D.domainsEltDescription delt) $ \descAltM -> 
                (":\n" <>) . T.unlines . map (space 6 <>) . T.lines $ 
                fromAltLeft $
                descAltM
        ]
    delts  = map fst . Map.elems . cDomDeps $ component

formatDescription :: Int -> T.Text -> T.Text
formatDescription indent desc = go $ T.lines desc 
  where
    go (hd:tl) = T.intercalate "\n" $ f indent ("-- | " <> hd) : map (f indent . (("--" <> space 4) <>)) tl
    f i = (space i <>)

formatFieldDescription :: [(T.Text, Maybe T.Text)] -> [T.Text]
formatFieldDescription = map go
  where
    go (field,descM) = maybe (space 3 <> field) ((<> (space 3 <> field)) . (<> "\n") . formatDescription 3) descM

----- Imports -----
importDomain :: Bool -> T.Text -> T.Text 
importDomain as' domainName = T.unwords $ 
    if as' then imp ++ ["as", domainName] else imp
  where
    imp = ["import", domainModuleName domainName]
   
componentToImport :: [ComponentName] -> [T.Text]
componentToImport = map (("module " <>) . domainModuleName . unComponentName)

protocolModuleHeader :: [ComponentName] -> T.Text
protocolModuleHeader names = T.unlines
    [ mod
    , "( " <> (T.intercalate "\n, " $ handleTypeName : configType : mod : componentToImport names)
    , ") where"
    ]
  where
    mod = T.unwords [ "module", protocolModuleName ]

protocolModuleName :: T.Text
protocolModuleName = "CDP"

domainModuleHeader :: T.Text -> T.Text
domainModuleHeader domainName = T.unwords ["module", domainModuleName domainName, "(module", domainModuleName domainName <>")", "where"]

domainModuleName :: T.Text -> T.Text
domainModuleName domainName = T.intercalate "." ["CDP", "Domains", domainName]

domainQualifiedName :: T.Text -> T.Text -> T.Text
domainQualifiedName domainName n =  T.intercalate "." [domainName, n]

domainName :: D.DomainsElt -> T.Text
domainName = D.domainsEltDomain

----- JSON -----
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
    ] ++ (map (\(v, hsv) -> T.unwords [space 9, v, "->", hsv]) $ zip (map (T.pack . show) vals) (map ("pure " <>) hsVals) ++ 
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

----- Types -----
genEltType :: Context -> T.Text -> Bool -> (Maybe (T.Text:|:[(Maybe A.Value)])) -> (Maybe (T.Text:|:[(Maybe A.Value)])) -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> T.Text
genEltType ctx name isOptional t1 t2 items = (if isOptional then "Maybe " else "") <> (leftType ctx name t1 t2 items)

leftType :: Context -> T.Text -> (Maybe (T.Text:|:[(Maybe A.Value)])) -> (Maybe (T.Text:|:[(Maybe A.Value)])) -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> T.Text
leftType ctx _ (Just (AltLeft ty1)) (Just (AltLeft ty2)) _ = error "impossible"
leftType ctx domain (Just (AltLeft ty)) _ itemsElt = typeCDPToHS ctx domain ty itemsElt
leftType ctx domain _ (Just (AltLeft ty)) itemsElt = typeCDPToHS ctx domain ty itemsElt
leftType ctx _ _ _ _ = error "no type found"

typeCDPToHS :: Context -> T.Text -> T.Text -> (Maybe (D.Items:|:[(Maybe A.Value)])) -> T.Text
typeCDPToHS ctx _ "object" _ = "[(String, String)]"
typeCDPToHS ctx domain "array" (Just (AltLeft items)) = "[" <> (leftType ctx domain (D.itemsType items) (D.itemsRef items) Nothing) <> "]"
typeCDPToHS ctx _ ty (Just (AltLeft items)) = error . T.unpack $ "non-array type with items: " <> ty
typeCDPToHS ctx domain ty Nothing = convertType ctx domain ty
typeCDPToHS ctx _ _ _ = error "no matching type"

convertType :: Context -> T.Text -> T.Text -> T.Text
convertType _ _ "string"  = "String"
convertType _ _ "integer" = "Int"
convertType _ _ "boolean" = "Bool"
convertType _ _ "number"  = "Double"
convertType _ _ "()" = "()"
convertType _ _ "any" = "Int" -- TODO
convertType _ _ "array" = error "got array conversion" -- TODO
convertType _ _ "object" = error "got object type"
convertType _ _ "" = error "got empty type"
convertType ctx domain s = case T.splitOn "." s of
    [otherDomain, ty] -> if cn otherDomain == cn domain
            then tyNameHS otherDomain ty -- both domains are in the same component, so the type is not qualified
            else domainQualifiedName (cn otherDomain) (tyNameHS otherDomain ty)
    _ -> tyNameHS domain s 
  where
    cn dn = unComponentName . domainToComponentName ctx $ dn

----- Dependencies -----    
{- : Domain dependencies specified in the protocol seem inaccurate:
    https://bugs.chromium.org/p/chromium/issues/detail?id=1354980#c0

    So they are collected by inspecting each domain.
-}

domainToComponentName :: Context -> T.Text -> ComponentName
domainToComponentName ctx domainName = cName . (flip (Map.!) domainName) . ctxDomainComponents $ ctx 

newtype ComponentName = ComponentName { unComponentName :: T.Text }
    deriving (Show, Eq, Ord)

type DomainDependencies = Map.Map T.Text (D.DomainsElt, [T.Text])
data Component = Component {
  cName     :: ComponentName
, cDomDeps  :: DomainDependencies
}

type DomainComponents = Map.Map T.Text Component
    -- original domain name, component the domain belongs to
    -- domain name for each vertex is the modified domain name
type Vertex = (D.DomainsElt, T.Text, [T.Text]) -- domain, domain name, domain dependencies 

domainComponents :: [D.DomainsElt] -> DomainComponents
domainComponents delts = Map.fromList . concatMap verticesToDomainComponents $ vsc2
  where
    vsc2  = map removeSameComponentDependencies vsc1   
    vsc1  = map Graph.flattenSCC . Graph.stronglyConnCompR $ g
    g     = map (\delt -> (delt, domainName delt, deps delt)) delts
    deps  = Set.toList . domainDependencies

verticesToDomainComponents :: [Vertex] -> [(T.Text, Component)]
verticesToDomainComponents vs = map (,c) dns
  where
    dns = map vertexToDomainName vs 
    c   = verticesToComponent vs

vertexToDomainName :: Vertex -> T.Text
vertexToDomainName (_,dn,_) = dn 

verticesToComponent :: [Vertex] -> Component
verticesToComponent vs = Component cn . Map.fromList . map toComponent $ vs
  where
    toComponent (delt,dn,deps) = (dn, (delt, deps))
    cn = componentName vs

componentName :: [Vertex] -> ComponentName
componentName [v] = ComponentName $ vertexToDomainName v 
componentName vs  = ComponentName $ mconcat . map vertexToDomainName $ vs

removeSameComponentDependencies :: [Vertex] -> [Vertex]
removeSameComponentDependencies vs = map (go vs) vs
  where
    go vs (delt, dn, deps) = (delt,dn,) . Set.toList $ 
        (Set.fromList deps) `Set.difference` dns
    dns = Set.fromList $ map vertexToDomainName vs

domainDependencies :: D.DomainsElt -> Set.Set T.Text
domainDependencies delt = removeSelf . Set.fromList . concat $
    [ concatMap typeDependencies    . fromMaybe [] $ D.domainsEltTypes delt
    , concatMap commandDependencies $ D.domainsEltCommands delt
    , concatMap eventDependencies   . fromMaybe [] $ D.domainsEltEvents delt
    ]
  where
    removeSelf = Set.filter (/= dn)
    dn = domainName delt

typeDependencies :: D.TypesElt -> [T.Text]
typeDependencies telt = if not (isValidType telt) then [] else
    case D.typesEltEnum telt of 
        Just _  -> []
        Nothing -> case D.typesEltType telt of
            "array"  -> maybe [] pure . join $ itemDependencies . fromAltLeft <$> D.typesEltItems telt
            "object" -> fromMaybe [] $ catMaybes . map paramDependencies <$> D.typesEltProperties telt
            s -> maybe [] pure $ refTypeToDomain s

commandDependencies :: D.CommandsElt -> [T.Text]
commandDependencies celt = if not (isValidCommand celt) then [] else 
    concat
        [ catMaybes . map returnDependencies . fromMaybe [] $ D.commandsEltReturns celt
        , catMaybes . map paramDependencies  . fromMaybe [] $ D.commandsEltParameters celt
        ]

eventDependencies :: D.EventsElt -> [T.Text]
eventDependencies evelt = if not (isValidEvent evelt) then [] else 
    catMaybes . map paramDependencies . fromMaybe [] $ D.eventsEltParameters evelt

paramDependencies :: D.ParametersElt -> Maybe T.Text
paramDependencies pelt = if not (isValidParam pelt) then Nothing else 
    case D.parametersEltEnum pelt of 
        Just _  -> Nothing
        Nothing -> case D.parametersEltRef pelt of
            Just r  -> refTypeToDomain . fromAltLeft $ r
            Nothing -> case fromAltLeft <$> D.parametersEltType pelt of
                Just "object" -> Nothing
                Just "array"  -> itemDependencies =<< fromAltLeft <$> D.parametersEltItems pelt
                Just s -> refTypeToDomain s
                _      -> Nothing

returnDependencies :: D.ReturnsElt -> Maybe T.Text
returnDependencies relt = if not (isValidReturn relt) then Nothing else
    case D.returnsEltRef relt of
        Just r  -> refTypeToDomain . fromAltLeft $ r
        Nothing -> case fromAltLeft <$> D.returnsEltType relt of
            Just "object" -> Nothing
            Just "array"  -> itemDependencies =<< fromAltLeft <$> D.returnsEltItems relt
            Just s -> refTypeToDomain s
            _     -> Nothing

itemDependencies :: D.Items -> Maybe T.Text
itemDependencies itelt = refTypeToDomain =<< fromAltLeft <$> D.itemsRef itelt

----- Validity -----
validDomains :: [D.DomainsElt] -> [D.DomainsElt]
validDomains ds = filter isValidDomain $ ds
  
isValidDomain :: D.DomainsElt -> Bool
isValidDomain delt = (not . isTrue . D.domainsEltDeprecated $ delt)

isValidType :: D.TypesElt -> Bool
isValidType telt = (not . isTrue . D.typesEltDeprecated $ telt)

isValidEvent :: D.EventsElt -> Bool
isValidEvent evelt = (not . isTrue . D.eventsEltDeprecated $ evelt)

isValidCommand :: D.CommandsElt -> Bool
isValidCommand celt = (not . isTrue . D.commandsEltDeprecated $ celt)

isValidParam :: D.ParametersElt -> Bool
isValidParam pelt = (not . isTrue . D.parametersEltDeprecated $ pelt)

isValidReturn :: D.ReturnsElt -> Bool
isValidReturn relt = (not . isTrue . D.returnsEltDeprecated $ relt)

validTypes :: D.DomainsElt -> [D.TypesElt]
validTypes = filter isValidType . fromMaybe [] . D.domainsEltTypes

validEvents :: D.DomainsElt -> [D.EventsElt]
validEvents = filter isValidEvent . fromMaybe [] . D.domainsEltEvents

validCommands :: D.DomainsElt -> [D.CommandsElt]
validCommands = filter isValidCommand . D.domainsEltCommands

----- Names -----
eventName   :: T.Text -> D.EventsElt -> T.Text
eventName   domainName ev = (domainName <>) . ("." <>) . D.eventsEltName $ ev
eventNameHS :: T.Text -> D.EventsElt -> T.Text
eventNameHS domainName ev = tyNameHS domainName (D.eventsEltName ev)

commandFnName :: T.Text -> T.Text
commandFnName = uncapitalizeFirst
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

refTypeToDomain :: T.Text -> Maybe T.Text
refTypeToDomain r = case T.splitOn "." r of
    [domain, _] -> Just domain
    _           -> Nothing

-----
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
guardEmptyList f v = go =<< (filter f <$> v)
  where
    go [] = Nothing
    go xs = Just xs 

space :: Int -> T.Text
space n = T.unwords $ replicate n ""

fromMaybeAltLeft :: Maybe (a :|: b) -> a
fromMaybeAltLeft (Just al) = fromAltLeft al 

fromAltLeft :: a :|: b -> a
fromAltLeft (AltLeft a) = a

isTrue :: Eq a => Maybe (Bool:|:a) -> Bool
isTrue = (== (Just $ AltLeft True))

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t = maybe t (\(first, rest) -> T.singleton (toUpper first) `T.append` rest) . T.uncons $ t

uncapitalizeFirst :: T.Text -> T.Text
uncapitalizeFirst t = maybe t (\(first, rest) -> T.singleton (toLower first) `T.append` rest) . T.uncons $ t
