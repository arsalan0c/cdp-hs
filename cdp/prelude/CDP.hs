type ClientApp b = Handle Event -> IO b

runClient   :: Config -> ClientApp a -> IO a
runClient config app = runClient' config (app . Handle)

subscribe   :: forall a. FromEvent Event a => Handle Event -> Maybe SessionId -> (a -> IO ()) -> IO ()
subscribe (Handle handle) sessionId eventHandler = subscribe' handle sessionId eventHandler

unsubscribe :: forall a. FromEvent Event a => Handle Event -> Maybe SessionId -> Proxy a -> IO ()
unsubscribe (Handle handle) sessionId p = unsubscribe' handle sessionId p