module Common.Utils exposing (withCmdNone)


withCmdNone : a -> ( a, Cmd msg )
withCmdNone value =
    ( value, Cmd.none )
