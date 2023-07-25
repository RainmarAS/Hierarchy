module GraphicsUtils (drawWorld,World) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
import Data.List
import Data.Word
import Data.ByteString (ByteString, pack)


data World = World {width :: Int, height :: Int, segments_quantity :: Int} deriving (Eq, Show)
colors = map (dark) $  cycle [greyN 0.3]--,red,green,rose, yellow,chartreuse, rose,  chartreuse, aquamarine, azure, violet]

drawWorld :: World-> Picture
drawWorld  world@(World width height n) = separateScreen world -- scale scaleX scaleY $
       where 
              scaleX = (fromIntegral width / fromIntegral height) ** (1/2)
              scaleY = 1/scaleX
              



separateScreen actsPosOnScreen = rotate (90) $ pics
       where 
              n = segments_quantity world
              pics =  pictures $ circles -- ++ lines    -- ----> not needed yet
              --lines =  map (\i -> color white $ line [(0,0),mulSV 1000 $ ithCosSin i]) [0..n-1]
              circles =    coloredSectors ++ [centerCircle] 
              coloredSectors  = drawSectors actsPosOnScreen
              degrees = 360/fromIntegral n
              centerCircle = circleSolid (radius' )
              radians = 2*(pi::Double) /  fromIntegral n
              ithCosSin i = (realToFrac $ cos $ radians * fromIntegral i,realToFrac $ sin $ radians * fromIntegral i) 


drawSectors world =    coloredSectors
       where  
              n = segments_quantity world
              --lines =  map (\i -> color white $ line [(0,0),mulSV 1000 $ ithCosSinForCircles i]) [0..n-1]
              coloredSectors  =   zipWith (color) colors sectors
              sectors = zipWith (<>) littleCircles thickArcs
              radians = 2*(pi::Double) /  fromIntegral n
              segment_icon = color red $ circleSolid $ (min 0.9 $ (fromIntegral n)/10)*(l * sin (realToFrac radians/2) )
              littleCircle_ = circleSolid $ (degrees - border_angle)/degrees*(l * sin (realToFrac radians/2) )
              littleCircle = littleCircle_ <> segment_icon
              littleCircles = zipWith (\(x,y) -> translate x y) (map ithCircleCenter [0..n-1]) (replicate n littleCircle)
              l =  distanceToLittleCircle world
              degrees = 360 /fromIntegral n
              thickArcs = map ithThickArc [0..n-1]
              ithCircleCenter i = mulSV l $ ithCosSinForCircles i
              ithCosSinForCircles i = (realToFrac $ cos $  radians *(1/2+ fromIntegral i) ,realToFrac $ sin $  radians *(1/2+ fromIntegral i) ) 
              ithThickArc i = thickArc (degrees * i'+border_angle/2) (degrees*(i'+1)-border_angle/2) arcRadius (arcThickness world)
                     where i' = fromIntegral i

radius (World width height n) =  ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2)  :: Float
          where area = fromIntegral $ width * height :: Float
distanceToLittleCircle :: World -> Float
distanceToLittleCircle world =  (radius world) / (1 - sin (realToFrac radians/2 ))
       where
              radians = 2*(pi::Double) /  fromIntegral (segments_quantity world)
arcThickness :: World -> Float
arcThickness world  =2* ( arcRadius - l * cos (realToFrac radians/2))
       where 
              l = distanceToLittleCircle world
              radians = 2*(pi::Double) /  fromIntegral (segments_quantity world)
