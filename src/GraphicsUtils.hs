module GraphicsUtils (drawScreen
                     ,ScreenState(..)
                     ,FocusTarget(..)
                     ,segments_quantity
                     ,distanceToLittleCircle
                     ,littleCircleRadius
                     ,segmentInDegrees
                     ,segmentInRadians
                     ,centerCircleRadius
                     ,polar2decart
                     ,decart2polar
                     ,ClickableObject(..)
                     ,clickedObject) where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
import Data.List
import Data.Word
import Data.Maybe
import Data.List
import Subject
import Data.ByteString (ByteString, pack)

data ClickableObject = SubjectButton { subject :: Subject}



clickedObject :: Float -> Float -> ScreenState -> Maybe ClickableObject
clickedObject x_ y_ screen_state= fmap (\s -> SubjectButton s) subj
       where
              (dx,dy) = focusPoint screen_state
              (x,y) = ( 1 / scaleX screen_state * x_+dx , 1 / scaleY screen_state * y_+dy )
              --subj = (subjects screen_state) !! ((index +1) `mod` segments_quantity screen_state)
              subj = if x**2+y**2 < (centerCircleRadius screen_state) ** 2 then  Just (Subject "" Blank)
                     else fmap (\i -> (subjects screen_state) !! ((i +1) `mod` segments_quantity screen_state)) index
              index = (findIndex (\(xc,yc) -> (x-xc)**2 + (y-yc)**2 <= (littleCircleRadius screen_state)**2) 
                     [ polar2decart (distanceToLittleCircle screen_state, -pi/2 +(1/2 + fromIntegral i ) * ( segmentInRadians screen_state) )  
                     | i <- [1..segments_quantity screen_state]]  )

border_angle  = 1/2 :: Float
arcRadius = 10000 :: Float

data FocusTarget = SubjectInCenter | SubjectsAround | Balanced deriving (Eq, Show)

data ScreenState = ScreenState {width :: Int, height :: Int, main_character :: Subject , subjects :: [Subject], subjectInCenter :: Subject, focusTarget :: FocusTarget } deriving (Eq, Show)

scaleX :: ScreenState -> Float
scaleX screen_state =  (fromIntegral (width screen_state) / fromIntegral (height screen_state)) ** (1/2)
scaleY screen_state =  (fromIntegral (height screen_state) / fromIntegral (width screen_state) ) ** (1/2)

colors = map (dark) $  cycle [greyN 0.3]--,red,green,rose, yellow,chartreuse, rose,  chartreuse, aquamarine, azure, violet]

drawScreen :: ScreenState-> Picture
drawScreen  screen_state@(ScreenState width height  _ _ subj_in_center _) = scale (scaleX screen_state) (scaleY screen_state) $ translate (-1*x) (-1*y)$ separateScreen screen_state -- 
       where 
              (x,y) = focusPoint screen_state
              n = segments_quantity screen_state
              area = fromIntegral $ width * height :: Float
              radiusV = ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2)  :: Float
              
focusPoint screen_state = if subj_in_center_idx < 0 then (0,0) else  (x,y) --(x * scaleY screen_state,y * scaleX screen_state)
       where 
              (x,y) = polar2decart (distanceToLittleCircle screen_state,-pi/2 + (segmentInRadians screen_state) *(1/2+ fromIntegral subj_in_center_idx))
              subj_in_center_idx = fromMaybe (-1) (findIndex (==subj_in_center) $ subjects screen_state)
              subj_in_center = subjectInCenter screen_state

separateScreen screen_state = rotate (90) $ pictures $ sectors
       where 
              sectors =  coloredSectors ++ [drawCenterCircle screen_state] 
              coloredSectors  = drawSectors screen_state


              
              --ithCosSin i = polar2decart (1,i*radians) --(realToFrac $ cos $ radians * fromIntegral i,realToFrac $ sin $ radians * fromIntegral i) 
drawCenterCircle screen_state = arcSolOrange <> arcSol <> triangle <> border <> (getMainChPic screen_state)  <> fstrectangle <> sndrectangle --text = scale 0.3 0.3 $ rotate (-90) $ translate (-400) 0 $ Text $ name $ main_character screen_state 
              where            
                     r = centerCircleRadius screen_state
                     border = color (greyN 0.1) $ thickCircle (r-3) 6
                     arcSol = color (greyN 0.2) $ arcSolid 195 345 r
                     arcSolOrange = (color orange) $ circleSolid r
                     triangle = color orange $ polygon [(0,0) , (r*cos angle,-r* sin angle),(-r* cos angle,-r* sin angle)]
                     (x,y) = polar2decart (2*r/(-3), 3*pi/8)
                     getMainChPic = (translate x y ). (scale (r*0.001) (r*0.001))  . (rotate (-90)) . picture . main_character
                     angle = pi/12
                     fstrectangle = (color$  greyN 0.1) $ translate 0 (-r*(1+sin angle)/2) $ rectangleSolid 25 (r*(1 - sin angle))
                     sndrectangle = (color$  greyN 0.1) $ translate 0 (-r*(sin angle)) $ rectangleSolid (2*(cos angle)*centerCircleRadius screen_state) 15

drawSectors screen_state =  coloredSectors ++ lines -- ++ squares
       where  
              n = segments_quantity screen_state
              lines = let fstpoint i =  polar2decart (l + littleCircleRadius screen_state,realToFrac $ radians *(1/2+ fromIntegral i))
                      in [color white $ line [fstpoint i,mulSV 3 $ fstpoint i] | i<- [0..n-1]]
              squares = concat [[  let (x,y) = polar2decart (l*(1.4 + (j/2)**2),realToFrac $ radians *(1/2+ fromIntegral i))
                                       trans = (color orange) . (translate x y) . rotate(-90) . (scale 0.3 0.3) . Text $ "Info"  -- 
                                       unitOrangeSquare =  ( color (dark orange)$ rectangleSolid 1 1) <> ( color orange $ scale 0.9 0.9 $ rectangleSolid 1 1)
                                       square =  (translate x y) . rotate(-90) . (scale (j*100*scaleX screen_state) (j*200*scaleY screen_state)) $ unitOrangeSquare 
                                   in  square
                                   | i <- [0..n-1] ]
                                   | j<-[3] ]
              coloredSectors  =  zipWith (color) colors  backgroundSectors
              backgroundSectors = zipWith (<>) backgroundSectors_ (sectorContentCircles screen_state)  
              backgroundSectors_ = zipWith (<>) (sectorBackgroundCircles screen_state)  thickArcs
              radians = segmentInRadians screen_state -- 2*(pi::Double) /  fromIntegral n
              l =  distanceToLittleCircle screen_state
              degrees = 360 /fromIntegral n
              thickArcs = map ithThickArc [0..n-1]
              ithThickArc i = thickArc (degrees * i'+border_angle/2) (degrees*(i'+1)-border_angle/2) arcRadius (arcThickness screen_state)
                     where i' = fromIntegral i

sectorBackgroundCircles screen_state = [ let (x,y) = polar2decart (l,realToFrac $ radians *(1/2+ fromIntegral i))
                                         in translate x y $ littleCircle
                                          | i <- [0..n-1] ]
       where
              contentCircle = if focusTarget screen_state == SubjectInCenter
                              then color (dark . dark $ green) $ circleSolid $ 0.9 * (littleCircleRadius screen_state)
                              else Blank
              backgroundCircle = circleSolid $ (littleCircleRadius screen_state) * (degrees - border_angle)/degrees
              littleCircle = backgroundCircle <> contentCircle -- <> characterLittleCircle
              l =  distanceToLittleCircle screen_state
              n = segments_quantity screen_state
              degrees = 360 /fromIntegral n
              radians = segmentInRadians screen_state

sectorContentCircles screen_state = [ let (x,y) = ithCircleCoordinates i
                                          r = littleCircleRadius screen_state
                                          scalar =  0.0015 * r
                                          scalseS = scale scalar scalar
                                          txt = color white $ (  rotate (-90) $ (translate (-0.15*r) (-0.4*r)) . scalseS.  Text . name $ (subjects screen_state) !! i)
                                          subjPic = scalseS . rotate (-90) $ picture $ (subjects screen_state) !! i 
                                          in translate x y $ littleCircle <> subjPic <> txt
                                          | i <- [0..n-1] ]
       where
              contentCircle = if focusTarget screen_state `elem` [SubjectsAround,Balanced] 
                              then color (dark . dark $ green) $ circleSolid $ 0.9 * (littleCircleRadius screen_state)
                              else Blank
              characterLittleCircle = color red $ circleSolid $ (min 0.9 $ (fromIntegral n)/25)*(littleCircleRadius screen_state)
              littleCircle = contentCircle <> characterLittleCircle
              ithCircleCoordinates i = case focusTarget screen_state of
                     SubjectInCenter -> polar2decart (l,realToFrac $ radians *(1/2+ fromIntegral i))
                     Balanced -> polar2decart (l,realToFrac $ radians *(1/2+ fromIntegral i))
                     SubjectsAround -> polar2decart (4*l,realToFrac $ radians *(1/2+ fromIntegral i))

              l =  distanceToLittleCircle screen_state
              n = segments_quantity screen_state
              degrees = 360 /fromIntegral n
              radians = segmentInRadians screen_state


centerCircleRadius :: ScreenState -> Float
centerCircleRadius screen_state@(ScreenState width height _ _ _ focTar) = focusScalar * ( area / (2*pi*(fromIntegral n+1)) ) ** (1/2)  :: Float
          where
              focusScalar = case focTar of 
                     SubjectInCenter -> 1.8
                     SubjectsAround -> 1/2
                     Balanced -> 1                 
              n = segments_quantity screen_state 
              area = fromIntegral $ width * height :: Float

segments_quantity screen_state = length $ subjects screen_state
segmentInRadians screen_state = 2*pi / (fromIntegral $ segments_quantity screen_state)
segmentInDegrees screen_state = 360 / (fromIntegral $ segments_quantity screen_state)

littleCircleRadius :: ScreenState -> Float
littleCircleRadius screen_state = l * sin (realToFrac radians/2)
       where l = distanceToLittleCircle screen_state
             radians = segmentInRadians screen_state

distanceToLittleCircle :: ScreenState -> Float
distanceToLittleCircle screen_state = (centerCircleRadius screen_state) / (1 - sin (realToFrac radians/2 ))
       where
              radians = 2*(pi::Double) /  fromIntegral (segments_quantity screen_state)

arcThickness :: ScreenState -> Float
arcThickness screen_state  =2* ( arcRadius - l * cos (realToFrac radians/2))
       where 
              l = distanceToLittleCircle screen_state
              radians = 2*(pi::Double) /  fromIntegral (segments_quantity screen_state)

polar2decart :: (Float,Float) -> (Float,Float)
polar2decart (r,theta) =  (r * cos theta ,r* sin theta)
decart2polar :: (Float,Float) -> (Float,Float)
decart2polar (x,y) = ((x**2+y**2)**(1/2), atan2 x y)