module GraphicsUtils (drawWorld,World(..)) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
import Data.List
import Data.Word
import Subject
import Data.ByteString (ByteString, pack)

border_angle  = 1/2 :: Float
arcRadius = 10000 :: Float


data World = World {width :: Int, height :: Int, segments_quantity :: Int, subjects :: [Subject]} deriving (Eq, Show)
colors = map (dark) $  cycle [greyN 0.3]--,red,green,rose, yellow,chartreuse, rose,  chartreuse, aquamarine, azure, violet]

drawWorld :: World-> Picture
drawWorld  world@(World width height n _)  =  separateScreen world -- scale scaleX scaleY $
       where 
              area = fromIntegral $ width * height :: Float
              radiusV = ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2)  :: Float
              scaleX = (fromIntegral width / fromIntegral height) ** (1/2)
              scaleY = 1/scaleX
              



separateScreen world = rotate (90) $ pics
       where 
              n = segments_quantity world
              pics =  pictures $ circles 
              circles =    coloredSectors ++ [centerCircle] 
              coloredSectors  = drawSectors world
              degrees = 360/fromIntegral n
              centerCircle =  color (greyN 0.2) $ circleSolid (radius world)
              radians = 2*(pi::Double) /  fromIntegral n
              ithCosSin i = (realToFrac $ cos $ radians * fromIntegral i,realToFrac $ sin $ radians * fromIntegral i) 


drawSectors world = coloredSectors ++ lines ++ squares
       where  
              n = segments_quantity world
              lines =  map (\i -> color white $line [mulSV l $ ithCosSinForCircles i,mulSV 1000 $ ithCosSinForCircles i]) [0..n-1]
              squares = concat [[ let (x,y) = mulSV (l*j) $ ithCosSinForCircles i  in color orange $ translate x y $ rotate(-90) $ scale 0.3 0.3 $ Text "Yeah"  --  rectangleSolid (40*j) (55*j)
                                   |i <- [0..n-1] ] | j<-[1.5,2..5] ]
              coloredSectors  =   zipWith (color) colors sectors
              sectors = zipWith (<>) thickArcs littleCircles
              radians = 2*(pi::Double) /  fromIntegral n
              semgent_icon = color red $ circleSolid $ (min 0.9 $ (fromIntegral n)/10)*(l * sin (realToFrac radians/2) )
              littleCircle_ = circleSolid $ (degrees - border_angle)/degrees*(l * sin (realToFrac radians/2) )
              littleCircle = littleCircle_ <> semgent_icon
              littleCircles = zipWith (\(x,y) -> translate x y) (map ithCircleCenter [0..n-1]) (replicate n littleCircle)
              l =  distanceToLittleCircle world
              degrees = 360 /fromIntegral n
              thickArcs = map ithThickArc [0..n-1]
              ithCircleCenter i = mulSV l $ ithCosSinForCircles i
              ithCosSinForCircles i = (realToFrac $ cos $  radians *(1/2+ fromIntegral i) ,realToFrac $ sin $  radians *(1/2+ fromIntegral i) ) 
              ithThickArc i = thickArc (degrees * i'+border_angle/2) (degrees*(i'+1)-border_angle/2) arcRadius (arcThickness world)
                     where i' = fromIntegral i

radius (World width height n []) =  ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2)  :: Float
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
