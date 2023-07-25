module GraphicsUtils (drawWorld,World(..),radius) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
import Data.List
import Data.Word
import Subject
import Data.ByteString (ByteString, pack)

border_angle  = 1/2 :: Float
arcRadius = 10000 :: Float


data World = World {width :: Int, height :: Int,  main_character :: Subject , subjects :: [Subject]} deriving (Eq, Show)

colors = map (dark) $  cycle [greyN 0.3]--,red,green,rose, yellow,chartreuse, rose,  chartreuse, aquamarine, azure, violet]

drawWorld :: World-> Picture
drawWorld  world@(World width height  _ _)  =  separateScreen world -- scale scaleX scaleY $
       where 
              n = segments_quantity world
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
              centerCircle = let text = scale 0.3 0.3 $ rotate (-90) $ translate (-400) 0 $ Text $ name $ main_character world 
                                 cCircle = color (greyN 0.2) $ circleSolid (radius world)
                             in cCircle <> text 
              radians = 2*(pi::Double) /  fromIntegral n
              --ithCosSin i = polar2decart (1,i*radians) --(realToFrac $ cos $ radians * fromIntegral i,realToFrac $ sin $ radians * fromIntegral i) 


drawSectors world = coloredSectors ++ lines -- ++ squares
       where  
              n = segments_quantity world
              lines = let fstpoint i =  polar2decart (l,realToFrac $ radians *(1/2+ fromIntegral i))
                      in [color white $ line [fstpoint i,mulSV 1000 $ fstpoint i] | i<- [0..n-1]]
              squares = concat [[  let (x,y) = polar2decart (l*j,realToFrac $ radians *(1/2+ fromIntegral i))
                                       trans = (color orange) . (translate x y) . rotate(-90) . (scale 0.3 0.3) . Text . name . main_character  --  rectangleSolid (40*j) (55*j)
                                   in  trans world 
                                   | i <- [0..n-1] ]
                                   | j<-[1.5,2..5] ]
              coloredSectors  =   zipWith (color) colors sectors
              sectors = zipWith (<>) thickArcs littleCircles
              radians = 2*(pi::Double) /  fromIntegral n
              semgent_icon = color red $ circleSolid $ (min 0.9 $ (fromIntegral n)/10)*(littleCircleRadius world)
              littleCircle_ = circleSolid $ (littleCircleRadius world) * (degrees - border_angle)/degrees
              littleCircle = littleCircle_ <> semgent_icon
              --littleCircles = zipWith (\(x,y) -> translate x y) (map ithCircleCenter [0..n-1]) (replicate n littleCircle)
              littleCircles = [ let (x,y) = polar2decart (l,realToFrac $ radians *(1/2+ fromIntegral i))
                                in translate x y $ littleCircle <> (rotate (-90) $ (translate (-25) (-25)) .  Text . name $ (subjects world) !! i)
                                | i <- [0..n-1] ]
              l =  distanceToLittleCircle world
              degrees = 360 /fromIntegral n
              thickArcs = map ithThickArc [0..n-1]
              ithThickArc i = thickArc (degrees * i'+border_angle/2) (degrees*(i'+1)-border_angle/2) arcRadius (arcThickness world)
                     where i' = fromIntegral i

segments_quantity world = length $ subjects world
radius world@(World width height _ _) =  ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2)  :: Float
          where
              n = segments_quantity world 
              area = fromIntegral $ width * height :: Float

segmentInRadians world = 2*pi / (fromIntegral $ segments_quantity world)
segmentInDegrees world = 360 / (fromIntegral $ segments_quantity world)
littleCircleRadius world =  l * sin (realToFrac radians/2)
       where l = distanceToLittleCircle world
             radians = segmentInRadians world


distanceToLittleCircle :: World -> Float
distanceToLittleCircle world =  (radius world) / (1 - sin (realToFrac radians/2 ))
       where
              radians = 2*(pi::Double) /  fromIntegral (segments_quantity world)

arcThickness :: World -> Float
arcThickness world  =2* ( arcRadius - l * cos (realToFrac radians/2))
       where 
              l = distanceToLittleCircle world
              radians = 2*(pi::Double) /  fromIntegral (segments_quantity world)

polar2decart :: (Float,Float) -> (Float,Float)
polar2decart (r,theta) =  (r * cos theta ,r* sin theta)
decart2polar :: (Float,Float) -> (Float,Float)
decart2polar (x,y) = ((x**2+y**2)**(1/2), atan2 x y)