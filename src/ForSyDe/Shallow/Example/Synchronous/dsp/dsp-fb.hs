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
dspSMA n input = mapSY (\xs -> sum xs / fromIntegral n) 
                 (windowSY n 1 input)

dspRMS :: Int -> Signal Double -> Signal Double
dspRMS n input = mapSY(\xs->sqrt (sum(map(^2) xs)/fromIntegral n)) 
                (windowSY n 1 input)

dspGain :: Double -> Signal Double -> Signal Double
dspGain g = mapSY (* g) 

adjGain :: Double -> Double -> Double
adjGain currentGain level
    | level > 1.0  = max 0.0 (currentGain - 0.1) 
    | level < -1.0 = min 5.0 (currentGain + 0.1) 
    | otherwise    = currentGain -- No change
    
data Level = Underload | Nominal | Overload deriving (Show, Eq, Ord)

dspMonitor :: Signal Double -> Signal Level
dspMonitor = mapSY (\level ->
    if level > 3.0 then Overload
    else if level < -3.0 then Underload
    else Nominal)

dspTag :: Signal Double -> Signal Level -> Signal (Double, Level)
dspTag = zipSY

dspFB :: Double -> Int -> Signal Double -> Signal (Double, Level)
dspFB initGain n input = tagged where
  filtered  = dspFilter input 
  smoothed  = dspSMA n filtered 
  rms       = dspRMS n smoothed 
  adjusted  = mooreSY adjGain id initGain rms
  amplified = zipWithSY (*) adjusted rms
  monitored = dspMonitor amplified
  tagged    = dspTag amplified monitored  