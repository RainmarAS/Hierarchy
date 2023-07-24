module WorkingMain3 (main) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
import System.IO.Unsafe
import Data.List
import Data.Word
import Data.ByteString (ByteString, pack)


-- TODO : MOVE TO GraphicsUtils.hs ------------------
drawing :: Int -> Int -> Int -> Picture
drawing width height n  = color rose $ scale scaleX scaleY $ separateScreen $ ActorsPositionsOnScreen radiusV n --radiusV n
       where 
              area = fromIntegral $ width * height :: Float
              radiusV = ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2)  :: Float
              scaleX = (fromIntegral width / fromIntegral height) ** (1/2)
              scaleY = 1/scaleX
              
data ActorsPositionsOnScreen = ActorsPositionsOnScreen {radius :: Float, parts_quantity :: Int} deriving (Eq, Show)
distanceToLittleCircle :: ActorsPositionsOnScreen -> Float
distanceToLittleCircle actsPosOnScreen =  radius' / (1 - sin (realToFrac radians/2 ))
       where
              radius' = radius actsPosOnScreen
              radians = 2*(pi::Double) /  fromIntegral (parts_quantity actsPosOnScreen)
arcThickness :: ActorsPositionsOnScreen -> Float
arcThickness actsPosOnScreen  =2* ( arcRadius - l * cos (realToFrac radians/2))
       where 
              l = distanceToLittleCircle actsPosOnScreen
              radians = 2*(pi::Double) /  fromIntegral (parts_quantity actsPosOnScreen)
arcRadius ::  Float
arcRadius = 10000



separateScreen actsPosOnScreen = rotate (90) $ pics
       where 
              radius' = radius actsPosOnScreen
              n = parts_quantity actsPosOnScreen
              pics =  pictures $ circles -- ++ lines    ------> not needed yet
              --lines = map (\i -> line [(0,0),mulSV 1000 $ ithCosSin i]) [0..n-1]
              circles =    coloredSectors ++ [centerCircle] 
              coloredSectors  = drawSectors actsPosOnScreen
              degrees = 360/fromIntegral n
              centerCircle = circleSolid (radius' )
              radians = 2*(pi::Double) /  fromIntegral n
              ithCosSin i = (realToFrac $ cos $ radians * fromIntegral i,realToFrac $ sin $ radians * fromIntegral i) 


drawSectors actsPosOnScreen = coloredSectors
       where  
              radius' = radius actsPosOnScreen
              n = parts_quantity actsPosOnScreen
              coloredSectors  =   zipWith (color) colors sectors
              sectors = zipWith (<>) littleCircles thickArcs
              radians = 2*(pi::Double) /  fromIntegral n
              littleCircle = circleSolid (l * sin (realToFrac radians/2))
              littleCircles = zipWith (\(x,y) -> translate x y) (map ithCircleCenter [0..n-1]) (replicate n littleCircle)
              l =  distanceToLittleCircle actsPosOnScreen--radius' / (1 - sin (realToFrac radians/2 )) :: Float
              degrees = 360/fromIntegral n
              thickArcs = map ithThickArc [0..n-1]
              ithThickArc i = rotate (degrees * fromIntegral (negate i) ) thArc
              ithCircleCenter i = mulSV l $ ithCosSinForCircles i
              ithCosSinForCircles i = (realToFrac $ cos $ radians *(1/2+ fromIntegral i) ,realToFrac $ sin $ radians *(1/2+ fromIntegral i) ) 
              thArc = thickArc 0 degrees arcRadius (arcThickness actsPosOnScreen)
-----------------------------------------------------

colors = map (dark)  [red,green,rose, yellow, chartreuse, rose,  chartreuse, aquamarine, azure, violet]
bitmapData = pack $ take 40000 (cycle [200,10,10,55])
main :: IO ()
main = do 
              imgs <- mapM loadBMP paths
              (width, height) <- getScreenSize
              putStrLn $ "width: " ++ (show width) ++ " height: " ++ (show height)
              display FullScreen (dark blue) (drawing width height (length colors))
              --display FullScreen (white) (bitmapOfByteString 100 100 (BitmapFormat TopToBottom PxRGBA) bitmapData True)
              --display FullScreen (white) (pictures $ imgsTrans imgs)
       where 
              paths = ["images\\"++show i ++".bmp" | i <-  [1..9]]
              imgsTrans imgs = [translate (i*300) 0 img | (img,i) <- zip imgs [1..9]] :: [Picture]
