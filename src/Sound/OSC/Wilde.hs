
module Sound.OSC.Wilde where

import           Data.ByteString.Char8            (ByteString (..))
import qualified Data.ByteString.Char8            as BS
import qualified Sound.OSC                        as OSC
import qualified Sound.OSC.Transport.FD
import qualified Sound.PortMidi                   as MIDI
import Data.Bits
import Control.Concurrent (threadDelay)
import Control.Monad

data IntP = IntLT Int | IntGT Int | IntEQ Int | IntPCo [IntP]

type Key      = IntP
type Velocity = IntP

data Event
  = MIDINoteOn  Key Velocity
  | MIDINoteOff Key Velocity
  | MIDIPedal   Velocity

data Action
  = SendOSC
      String [String] -- address port pattern args (i.e. "/fluent/play" ["n1","1"])







listen :: Env -> [(Event, Action)] -> IO ()
listen env pats = forever $ do
  threadDelay (1000*5) -- ms/1000
  -- putStrLn "<<<<<<<< Callback!"
  readRes <- MIDI.readEvents (_midiIn env)
  messages <- return $ case readRes of
    -- Backwards in PortMIDI API...
    -- Right e -> putStrLn $ "Could not read MIDI: " ++ show e
    Right _ -> []
    Left x -> x
  forM_ messages $ \msg -> do
    forM_ pats $ \(event, action) -> do
      if (compileEvent event (MIDI.message msg)) then compileAction env action else return ()
    
  return () 

-- Implementation
data Env = Env {
    _oscOut :: OSC.UDP,
    _midiIn :: MIDI.PMStream
  }


compileIntP :: IntP -> Int -> Bool
compileIntP = go
  where
    go (IntLT a) x = x <  a
    go (IntGT a) x = x >  a
    go (IntEQ a) x = x == a
    go (IntPCo ps) x = all id $ map (($ x) . compileIntP) ps

-- TODO capture values in patterns
compileEvent :: Event -> MIDI.PMMsg -> Bool
compileEvent = go
  where
    go (MIDINoteOn k v) (MIDI.PMMsg s d1 d2) = 
      s `shiftR` 4 == 0x09 && compileIntP k (fromIntegral d1) && compileIntP v (fromIntegral d2)
    go (MIDINoteOff k v) (MIDI.PMMsg s d1 d2) = 
      s `shiftR` 4 == 0x08 && compileIntP k (fromIntegral d1) && compileIntP v (fromIntegral d2)
    go (MIDIPedal v) (MIDI.PMMsg s d1 d2) = 
      s `shiftR` 4 == 0x11 && d1 == 0x07 && compileIntP v (fromIntegral d2) -- is it in d1?

compileAction :: Env -> Action -> IO ()
compileAction env = go
  where
    go (SendOSC patt args) = do
      let msg = OSC.Message patt (fmap (OSC.ASCII_String . BS.pack) args)
      putStrLn $ "Sending " ++ show msg
      Sound.OSC.Transport.FD.sendMessage (_oscOut env) $ msg
-- End implementation

printDevs = do
  n <- MIDI.countDevices
  forM_ [0..n-1] $ \i -> do
    info <- MIDI.getDeviceInfo i
    putStrLn $ show i ++ ": " ++ show info
  
runWilde = do
  MIDI.initialize
  -- Print devices
  printDevs
  
  oscOut <- let (addr, port) = kOSCDest in OSC.openUDP addr port
  midiInRes <- MIDI.openInput kMIDIIn
  midiIn <- return $ case midiInRes of
    -- Backwards in PortMIDI API...
    Right e -> error $ "Could not start MIDI: " ++ show e
    Left x -> x
  let env = Env oscOut midiIn
  
  printDevs
  listen env $
    [ (MIDINoteOn (IntEQ 60) (IntGT 0), SendOSC "/fluent/play" ["n1","60"])
    , (MIDINoteOn (IntEQ 60) (IntEQ 0), SendOSC "/fluent/stop" ["n1"])
    ]

  putStrLn $ "Wilde is running!"
  putStrLn $ "Sending OSC to" ++ show kOSCDest


  
kOSCDest = ("127.0.0.1", 54321)
kMIDIIn  = 0

