module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Environment
import GraphicsUtils 
import Data.Maybe
import Data.List
import Subject
import Data.ByteString (ByteString, pack)


paths = ["D:\\Hierarchy\\images\\"++show i ++".bmp" | i <-  [1..9]]
imgsTrans imgs = [translate dx 0 img | (img,dx) <- zip imgs [0,300..9*300]] :: [Picture]



updateScreenState :: Event -> ScreenState -> ScreenState
updateScreenState (EventKey (MouseButton WheelUp) Down _ _) screen_state = screen_state{subjectInCenter = subjects screen_state !! newIndex}
        where newIndex =( 1 + fromMaybe 0 (findIndex (==subjectInCenter screen_state) $ subjects screen_state) ) `mod` len
              len = (length $ subjects screen_state) :: Int
updateScreenState (EventKey (MouseButton WheelDown) Down _ _) screen_state = screen_state{subjectInCenter = subjects screen_state !! newIndex}
        where newIndex =( -1 + fromMaybe 0 (findIndex (==subjectInCenter screen_state) $ subjects screen_state) ) `mod` len
              len = (length $ subjects screen_state) :: Int
updateScreenState (EventKey (MouseButton LeftButton) Down _ (x,y)) screen_state = screen_state {subjectInCenter = subj}
        where subj = subject $ fromMaybe (SubjectButton (subjectInCenter screen_state)) $ clickedObject x y screen_state
updateScreenState (EventKey (Char 'r') Down _ (x,y)) screen_state = screen_state {subjectInCenter = Subject ""}        
updateScreenState _ screen_state = screen_state

                                


mainCharacter = Subject "Main Character"
initSubjects = take 26 $ cycle $ map (\s -> Subject [s]) ['a','b'..'z'] :: [Subject]
--colors = map (dark)  [red,green,rose, yellow, chartreuse, rose,  chartreuse, aquamarine, azure, violet]
bitmapData = pack $ take 40000 (cycle [200,10,10,55])
main :: IO ()
main = do 
        imgs <- mapM loadBMP paths
        (width, height) <- getScreenSize
        putStrLn $ "width: " ++ (show width) ++ " height: " ++ (show height)
        play FullScreen (greyN 0.3) 60  (ScreenState width height mainCharacter initSubjects mainCharacter) drawScreen updateScreenState (\_ -> id)
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