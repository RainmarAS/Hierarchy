module WorkingMain2 (main) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
background :: Color
background = white


-- TODO : MOVE TO GraphicsUtils.hs ------------------
drawing :: Int -> Int -> Int -> Picture
drawing width height n  = color (greyN 0.4) $ scale scaleX scaleY $ separateScreen radiusV n
       where 
              area = fromIntegral $ width * height :: Float
              radiusV =  ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2) :: Float
              scaleX = (fromIntegral width / fromIntegral height) ** (1/2)
              scaleY = 1/scaleX
              


separateScreen radius n = rotate (90) $ pics
       where 
              pics =  pictures $ circles ++ lines
              circles =  centerCircle : coloredSectors
              thArc =       let    r = 400 
                                   thickness = r - l * cos (realToFrac radians/2)
                            in thickArc 0 degrees r (2*thickness)
              coloredSectors  =   zipWith (color) colors sectors
              sectors = zipWith (<>) littleCircles thickArcs
              ithThickArc i = rotate (degrees * fromIntegral (negate i) ) thArc -- (4 - i) stands for correct segment indexes
              thickArcs = map ithThickArc [0..n-1]
              degrees = 360/fromIntegral n
              centerCircle = circleSolid radius 
              l = radius / (1 - sin (realToFrac radians/2 )) :: Float
              littleCircle = circleSolid (l * sin (realToFrac radians/2))
              littleCircles = zipWith (\(x,y) -> translate x y) (map ithCircleCenter [0..n-1]) (replicate n littleCircle)
              lines = map (\i -> line [(0,0),mulSV 1000 $ ithCosSin i]) [0..n-1]
              radians = 2*(pi::Double) /  fromIntegral n
              ithCosSin i = (realToFrac $ cos $ radians * fromIntegral i,realToFrac $ sin $ radians * fromIntegral i) 
              ithCircleCenter i = mulSV l $ ithCosSinForCircles i
              ithCosSinForCircles i = (realToFrac $ cos $ radians *(1/2+ fromIntegral i) ,realToFrac $ sin $ radians *(1/2+ fromIntegral i) ) 


-----------------------------------------------------

colors = [red,green,blue,yellow, magenta, rose, orange, chartreuse, aquamarine, azure, violet]

main :: IO ()
main =  do 
              (width, height) <- getScreenSize
              putStrLn $ "width: " ++ (show width) ++ " height: " ++ (show height)
              display FullScreen cyan (drawing width height (length colors))

              