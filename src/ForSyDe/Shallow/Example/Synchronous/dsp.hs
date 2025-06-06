import ForSyDe.Shallow

dspFilter :: Signal Double -> Signal Double
dspFilter = mapSY (\x -> if x > 1.5 then 1.5 
                         else if x < -1.5 then -1.5 
                         else x)

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

-- >>> dspRMS winSize $ dspSMA winSize $ dspFilter audio

-- >>> dspAudio gain winSize audio

-- >>> dspFB gain winSize audio