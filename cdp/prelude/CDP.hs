type ClientApp b = Handle Event -> IO b

runClient   :: Config -> ClientApp a -> IO a
runClient config app = runClient' config (app . Handle)

subscribe   :: forall a. FromEvent Event a => Handle Event -> (a -> IO ()) -> IO ()
subscribe (Handle handle) eventHandler = subscribe' handle eventHandler

unsubscribe :: forall a. FromEvent Event a => Handle Event -> Proxy a -> IO ()
unsubscribe (Handle handle) p = unsubscribe' handle p