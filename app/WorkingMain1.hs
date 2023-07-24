module WorkingMain1 (main) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
background :: Color
background = white


-- TODO : MOVE TO GraphicsUtils.hs ------------------
drawing :: Int -> Int -> Int -> Picture
drawing width height n  = color (greyN 0.4) $ scale scaleX scaleY  $ separateScreen radiusV n
       where 
              area = fromIntegral $ width * height :: Float
              radiusV =  ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2) :: Float
              scaleX = (fromIntegral width / fromIntegral height) ** (1/2)
              scaleY = 1/scaleX
              

separateScreen radius n = rotate (90) $ pics
       where 
              pics = pictures (sCircle : lines)
              sCircle = circleSolid radius 
              lines = map (\i -> line [(0,0),mulSV 1000 $ ithCosSin i]) [0..n-1]
              radians = 2*(pi::Double) /  fromIntegral n
              ithPoint i = mulSV radius $ ithCosSin i
              ithCosSin i = (realToFrac $ cos $ radians * fromIntegral i,realToFrac $ sin $ radians * fromIntegral i) 


-----------------------------------------------------



main :: IO ()
main =  do 
              (width, height) <- getScreenSize
              putStrLn $ "width: " ++ (show width) ++ " height: " ++ (show height)
              display FullScreen cyan (drawing width height 7)

              