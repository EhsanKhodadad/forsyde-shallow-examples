import ForSyDe.Shallow

dspFilter :: Signal Double -> Signal Double
dspFilter = mapSY (\x -> if x > 1.5 then 1.5 
                         else if x < -1.5 then -1.5 
                         else x)

{-|
    This function accepts a signal and a window size and generates a list of signals with the given size, spanning the values inside the original input signal.
    In the initial state, the output signal is filled with zeros; the values come out individually. For example, if we take @[1..5]@ as the input signal and 3 as the window size, the output is @[[0,0,0],[0,0,1],[0,1,2],[1,2,3],[2,3,4],[3,4,5]]@.
    First, its @initState@ fills the window with zero by replicating 0. Then, its @nextState@ appends the new value to the current window by @++@ operator, removes the old one by @drop 1@, and ensures its size remains correct by @take@.
    Finally, its @output@ outputs the current window, ignoring the index by @_@.
-}

windowSY :: Int -> Signal Double -> Signal [Double]
windowSY winSize = mooreSY nextState output initState where
    initState = (replicate winSize 0, 0) -- (win, index)
    nextState (currWin, currIndex) newVal =
      let nextWin = take winSize (drop 1 (currWin++[newVal]))
          nextIndex  = currIndex + 1 in (nextWin, nextIndex)
    output (currWin, _) = currWin

dspSMA :: Int -> Signal Double -> Signal Double
dspSMA n input = mapSY (\xs -> sum xs / fromIntegral n) (windowSY n input)

dspRMS :: Int -> Signal Double -> Signal Double
dspRMS n input = mapSY(\xs->sqrt (sum(map(^2) xs) / fromIntegral n)) (windowSY n input)

dspGain :: Double -> Signal Double -> Signal Double
dspGain g = mapSY (* g) 

data Level = Underload | Nominal | Overload deriving (Show, Eq, Ord)

dspMonitor :: Signal Double -> Signal Level
dspMonitor = mapSY (\level ->
    if level > 3.0 then Overload
    else if level < -3.0 then Underload
    else Nominal)

dspTag :: Signal Double -> Signal Level -> Signal (Double, Level)
dspTag = zipSY

dspAudio :: Double -> Int -> Signal Double -> Signal (Double, Level)
dspAudio gain n input = tagged where
    filtered   = dspFilter input         
    smoothed   = dspSMA n filtered       
    rms        = dspRMS n smoothed       
    amplified  = dspGain gain rms        
    monitored  = dspMonitor amplified    
    tagged     = dspTag amplified monitored


adjGain :: Double -> Double -> Double
adjGain currentGain level
    | level > 1.0  = max 0.0 (currentGain - 0.1) 
    | level < -1.0 = min 5.0 (currentGain + 0.1) 
    | otherwise    = currentGain -- No change

dspFB :: Double -> Int -> Signal Double -> Signal (Double, Level)
dspFB initGain n input = tagged where
  filtered  = dspFilter input 
  smoothed  = dspSMA n filtered 
  rms       = dspRMS n smoothed 
  adjusted  = mooreSY adjGain id initGain rms
  amplified = zipWithSY (*) adjusted rms
  monitored = dspMonitor amplified
  tagged    = dspTag amplified monitored  

-- This is a simple example of how to use the dspFB function with a signal.
audio :: Signal Double
audio = signal [1.4, -3.2, 0.1, 2.0, -1.5, 0.3, -0.7, 2.4, 0, -1.8, 
                -0.3, 1.7, 1.9, 0.2, -3.1, -0.4, 0.6, 1.9, -2.1, 
                -2.9, 0.5, 2.2, -0.2]
gain :: Double
gain = 2.5

winSize :: Int
winSize = 4

-- >>> dspFilter audio

-- >>> dspGain gain $ dspFilter audio

-- >>> dspSMA winSize $ dspFilter audio
-- {0.0,0.35,-2.5000000000000022e-2,-2.0816681711721685e-17,0.375,-0.35,0.10000000000000002,-9.999999999999999e-2,-9.999999999999998e-2,0.275,-0.175,-7.5e-2,-7.500000000000001e-2,0.3,0.7250000000000001,0.42500000000000004,-5.000000000000002e-2,-0.275,5.0000000000000044e-2,4.999999999999999e-2,-0.22499999999999998,-0.25,-0.25,7.5e-2}

-- >>> dspRMS winSize $ dspSMA winSize $ dspFilter audio

-- >>> dspAudio gain winSize audio

-- >>> dspFB gain winSize audio
