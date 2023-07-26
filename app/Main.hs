module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Environment
import GraphicsUtils 
import Subject
import Data.ByteString (ByteString, pack)


paths = ["D:\\Hierarchy\\images\\"++show i ++".bmp" | i <-  [1..9]]
imgsTrans imgs = [translate dx 0 img | (img,dx) <- zip imgs [0,300..9*300]] :: [Picture]



updateWorld :: Event -> World -> World
--updateWorld (EventKey (MouseButton WheelUp) Down _ _) world = world{segments_quantity = 1 + segments_quantity world }
--updateWorld (EventKey (MouseButton WheelDown) Down _ _) world = world{segments_quantity = (-1) +  segments_quantity world}
updateWorld (EventKey (MouseButton LeftButton) Down _ (x,y)) world = if any_circle_touched x y world
                                                                     then world{subjects = [Subject "a",Subject "b",Subject "c"]} 
                                                                     else world
updateWorld _ world = world

any_circle_touched x y world = x**2+y**2 < (radius world) ** 2 || (any (\(xc,yc) -> (x-xc)**2 + (y-yc)**2 < (littleCircleRadius world)**2) [ polar2decart (distanceToLittleCircle world,fromIntegral i * segmentInRadians world)  | i <- [1..segments_quantity world]])
                                        

mainCharacter = Subject "Main Character"
initSubjects =  [Subject "a"
                ,Subject "b"
                ,Subject "c"
                ,Subject "d"
                ,Subject "e"              
                ,Subject "c"
                ,Subject "d"
                ,Subject "e"
                ,Subject "l"] :: [Subject]
--colors = map (dark)  [red,green,rose, yellow, chartreuse, rose,  chartreuse, aquamarine, azure, violet]
bitmapData = pack $ take 40000 (cycle [200,10,10,55])
main :: IO ()
main = do 
        imgs <- mapM loadBMP paths
        (width, height) <- getScreenSize
        putStrLn $ "width: " ++ (show width) ++ " height: " ++ (show height)
        play FullScreen (greyN 0.3) 60  (World width height mainCharacter initSubjects) drawWorld updateWorld (\_ -> id)
        --display FullScreen (dark blue) ( coloredDrawing width height  ) 
        --display FullScreen (white) (bitmapOfByteString 100 100 (BitmapFormat TopToBottom PxRGBA) bitmapData True)
        --display FullScreen (white) (pictures $ imgsTrans imgs)

{- for prototype with faces
                                        ( scale 0.7 0.7 $ imgs!!0)   <>
                                        ( translate 500 150 $ scale 0.7 0.7 $ imgs!!1) <>
                                        ( translate (-500) 150 $ scale 0.7 0.7 $ imgs!!2) <>
                                        ( translate 0 (350) $ scale 0.7 0.7 $ imgs!!3 ) <>
                                        ( translate 400 (-250) $ scale 0.7 0.7 $ imgs!!4 ) <>
                                        ( translate (-400) (-250) $ scale 0.7 0.7 $ imgs!!5 )-} 