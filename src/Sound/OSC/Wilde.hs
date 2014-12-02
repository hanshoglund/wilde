
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
      -- putStrLn $ "Sending " ++ show msg
      Sound.OSC.Transport.FD.sendMessage (_oscOut env) $ msg
-- End implementation

printDevs = do
  putStrLn "Available devices: "
  n <- MIDI.countDevices
  forM_ [0..n-1] $ \i -> do
    info <- MIDI.getDeviceInfo i
    putStrLn $ show i ++ ": " ++ show (MIDI.name info)
  putStrLn ""

printDevName i = do
  n <- MIDI.countDevices
  if i < 2 then do
    info <- MIDI.getDeviceInfo i
    putStrLn $ show i ++ ": " ++ show (MIDI.name info)
  else
    return ()
  
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

  putStrLn $ "Wilde is running!"
  putStr $ "Listening for MIDI from "
  printDevName kMIDIIn
  putStrLn $ "Sending OSC to" ++ show kOSCDest
  
  -- printDevs
  listen env $
    [ (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n1"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n2"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n3"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n4"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n5"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n6"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n7"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n8"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n1x"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n2x"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n3x"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n4x"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n5x"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n6x"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n7x"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n8x"])
    , (MIDINoteOn (IntGT 95) (IntGT 0), SendOSC "/fluent/stop" ["n0"])

    , (MIDINoteOn (IntEQ 60) (IntGT 0), SendOSC "/fluent/play" ["n1","chord1_begin_rev"])
    , (MIDINoteOn (IntEQ 61) (IntGT 0), SendOSC "/fluent/play" ["n2","chord2_begin_rev"])
    , (MIDINoteOn (IntEQ 62) (IntGT 0), SendOSC "/fluent/play" ["n3","chord3_begin_rev"])
    , (MIDINoteOn (IntEQ 63) (IntGT 0), SendOSC "/fluent/play" ["n4","chord4_begin_rev"])
    , (MIDINoteOn (IntEQ 64) (IntGT 0), SendOSC "/fluent/play" ["n5","chord5_begin_rev"])
    , (MIDINoteOn (IntEQ 65) (IntGT 0), SendOSC "/fluent/play" ["n6","chord6_begin_rev"])
    , (MIDINoteOn (IntEQ 66) (IntGT 0), SendOSC "/fluent/play" ["n7","chord7_begin_rev"])
    , (MIDINoteOn (IntEQ 67) (IntGT 0), SendOSC "/fluent/play" ["n8","chord8_begin_rev"])

    , (MIDINoteOn (IntEQ 95) (IntGT 0), SendOSC "/fluent/play" ["n0","crossing_bass"])

    -- End
    , (MIDINoteOn (IntEQ 69) (IntGT 0), SendOSC "/fluent/play" ["n1","chord1_begin_rev"])
    , (MIDINoteOn (IntEQ 70) (IntGT 0), SendOSC "/fluent/play" ["n2","chord2_begin_rev"])
    , (MIDINoteOn (IntEQ 71) (IntGT 0), SendOSC "/fluent/play" ["n3","chord3_begin_rev"])
    , (MIDINoteOn (IntEQ 72) (IntGT 0), SendOSC "/fluent/play" ["n4","chord4_begin_rev"])
    , (MIDINoteOn (IntEQ 73) (IntGT 0), SendOSC "/fluent/play" ["n5","chord5_begin_rev"])
    , (MIDINoteOn (IntEQ 74) (IntGT 0), SendOSC "/fluent/play" ["n6","chord6_begin_rev"])
    , (MIDINoteOn (IntEQ 75) (IntGT 0), SendOSC "/fluent/play" ["n7","chord7_begin_rev"])
    , (MIDINoteOn (IntEQ 76) (IntGT 0), SendOSC "/fluent/play" ["n8","chord8_begin_rev"])

    , (MIDINoteOn (IntEQ 69) (IntGT 0), SendOSC "/fluent/play" ["n1x","mono_crotarc_a_rev"])
    , (MIDINoteOn (IntEQ 70) (IntGT 0), SendOSC "/fluent/play" ["n2x","mono_crotarc_a_rev"])
    , (MIDINoteOn (IntEQ 71) (IntGT 0), SendOSC "/fluent/play" ["n3x","mono_crotarc_d_rev"])
    , (MIDINoteOn (IntEQ 72) (IntGT 0), SendOSC "/fluent/play" ["n4x","mono_crotarc_e_rev"])
    , (MIDINoteOn (IntEQ 73) (IntGT 0), SendOSC "/fluent/play" ["n5x","mono_crotarc_e_rev"])
    , (MIDINoteOn (IntEQ 74) (IntGT 0), SendOSC "/fluent/play" ["n6x","mono_crotarc_a_rev"])
    , (MIDINoteOn (IntEQ 75) (IntGT 0), SendOSC "/fluent/play" ["n7x","mono_crotarc_d_rev"])
    , (MIDINoteOn (IntEQ 76) (IntGT 0), SendOSC "/fluent/play" ["n8x","mono_crotarc_e_rev"])
-- mono_crotarc_d_rev

    ]



  
kOSCDest = ("127.0.0.1", 54321)
kMIDIIn  = 1

