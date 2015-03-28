import ListProc
import Text(plainText)

-- main = plainText <| ListProc.log "Hello, World!"

main = plainText <| ListProc.takeWhileString ((/=) ' ') "Hello, World!"