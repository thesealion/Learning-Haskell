import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.IORef
import Data.Time.Clock
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Environment (getArgs)
import System.Posix.Process (getProcessID)
import Text.Printf (printf)


echo_request = 8
echo_reply = 0
max_recv = 2048

putHeader :: Word8 -> Word8 -> Word16 -> Word16 -> Word16 -> BL.ByteString -> Put
putHeader icmp_type code checksum ident seq icmp_data = do
    putWord8 icmp_type
    putWord8 code
    putWord16be checksum
    putWord16be ident
    putWord16be seq
    putLazyByteString icmp_data

getHeader :: Get (Word8, Word8, Word16, Word16, Word16)
getHeader = do
    icmp_type <- getWord8
    code <- getWord8
    checksum <- getWord16be
    ident <- getWord16be
    seq <- getWord16be
    return (icmp_type, code, checksum, ident, seq)

listOfWord16 :: Get [Word16]
listOfWord16 = do
  empty <- isEmpty
  if empty
     then return []
     else do v <- getWord16be
             rest <- listOfWord16
             return (v : rest)

checksum :: BL.ByteString -> Word16
-- Calculate checksum of an ICMP packet.
-- Implementation stolen from http://programatica.cs.pdx.edu/House/
checksum bs = let bs' = (if (BL.length bs) `mod` 2 == 0 then bs else BL.snoc bs 0)
                  ws = runGet listOfWord16 bs'
                  total = sum (map fromIntegral ws) :: Word32
              in complement (fromIntegral total + fromIntegral (total `shiftR` 16))

buildPacket :: Word16 -> Word16 -> BL.ByteString -> BL.ByteString
-- First build a header with checksum=0 then calculate the checksum and
-- put it into the header.
buildPacket ident seq icmp_data = buildPacket' $ checksum $ buildPacket' 0
    where buildPacket' cs = runPut $ putHeader echo_request 0 cs ident seq icmp_data

pingForever :: String -> SockAddr -> Socket -> Word16 -> Word16
    -> BL.ByteString -> IORef (Int, Int) -> IO a
pingForever host addr sock ident seq icmp_data ref = do
    (sent, received) <- readIORef ref
    -- Convert a lazy bytestring returned by buildPacket into a strict one.
    let packet = B.concat $ BL.toChunks $ buildPacket ident seq icmp_data
        (SockAddrInet _ addr_host) = addr
    start <- getCurrentTime
    sendTo sock packet addr
    writeIORef ref (sent + 1, received)

    (received_packet, address) <- recvFrom sock max_recv
    stop <- getCurrentTime
    let delay = (*1000) $ realToFrac $ diffUTCTime stop start
        (ip_header, ip_data) = BL.splitAt 20 (BL.fromChunks [received_packet])
        (SockAddrInet _ from_host) = address
        (icmp_header, icmp_data) = BL.splitAt 8 ip_data
        (icmp_type, code, checksum, r_ident, r_seq) = runGet getHeader icmp_header
    ip <- inet_ntoa from_host
    let hostname_to_show = if from_host == addr_host && host /= ip then
                               host ++ " (" ++ ip ++ ")"
                           else ip
    if r_ident == ident && icmp_type == echo_reply then do
        putStr $ (show $ BL.length ip_data) ++ " bytes from " ++
            hostname_to_show ++ ": icmp_req=" ++ (show r_seq) ++ " time="
        printf "%.1f" (delay :: Float)
        putStrLn ""
        writeIORef ref (sent + 1, received + 1)
        else return ()
    threadDelay $ 10^6
    pingForever host addr sock ident (seq + 1) icmp_data ref

printStats :: String -> IORef (Int, Int) -> IO ()
printStats hostname ref = do
    (sent, received) <- readIORef ref
    putStrLn $ "\n--- " ++ hostname ++ " ping statistics ---"
    putStr $ (show sent) ++ " packets transmitted, " ++ (show received) ++ " received, "
    let ratio = (fromIntegral received) / (fromIntegral sent) :: Float
    printf "%.0f" $ 100 - (ratio * 100)
    putStrLn "% packet loss"

main = do
    pid' <- getProcessID
    args <- getArgs
    if length args == 0 then error "destination required" else return ()

    let hostname = head args
        pid = fromIntegral pid'
        data_size = 56

    addrinfos <- getAddrInfo Nothing (Just hostname) Nothing
    let addr = addrAddress $ head addrinfos
        icmp_data = BL.pack $ map (.&. 255) [1..data_size]
        (SockAddrInet _ host) = addr
    ip <- inet_ntoa host

    let hostname_to_show = if hostname == ip then ip else hostname ++ " (" ++ ip ++ ")"
    putStrLn $ "PING " ++ hostname_to_show ++ " " ++ (show data_size) ++
        "(" ++ (show $ data_size + 28) ++ ") bytes of data."

    sock <- socket AF_INET Raw 1 -- raw socket, protocol=1 (ICMP)
    ref <- newIORef (0,0)
    pingForever hostname addr sock pid 1 icmp_data ref `finally` do
        printStats hostname ref
        sClose sock

