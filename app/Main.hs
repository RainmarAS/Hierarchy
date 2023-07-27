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
--updateScreenState (EventKey (MouseButton LeftButton) Down _ (x,y)) screen_state = screen_state {subjectInCenter = subj}
--        where subj = subject $ fromMaybe (SubjectButton (subjectInCenter screen_state)) $ clickedObject x y screen_state
updateScreenState (EventMotion (x,y)) screen_state = screen_state {subjectInCenter = subj}
        where subj = subject $ fromMaybe (SubjectButton (subjectInCenter screen_state)) $ clickedObject x y screen_state
updateScreenState (EventKey (Char 'r') Down _ (x,y)) screen_state = screen_state {subjectInCenter = Subject "" Blank}        
updateScreenState _ screen_state = screen_state

                                



--initSubjects = take 7 $ cycle $ map (\s -> Subject [s] Blank) ['a','b'..'z'] :: [Subject]
--colors = map (dark)  [red,green,rose, yellow, chartreuse, rose,  chartreuse, aquamarine, azure, violet]
bitmapData = pack $ take 40000 (cycle [200,10,10,55])
main :: IO ()
main = do 
        imgs <- mapM loadBMP paths
        (width, height) <- getScreenSize
        putStrLn $ "width: " ++ (show width) ++ " height: " ++ (show height)
        play FullScreen (greyN 0.3) 60  (ScreenState width height (mainCharacter (imgs !! 0) ) (initSubjects $ tail imgs) (mainCharacter (imgs !! 0))) drawScreen updateScreenState (\_ -> id)
        --display FullScreen (dark blue) ( coloredDrawing width height  ) 
        --display FullScreen (white) (bitmapOfByteString 100 100 (BitmapFormat TopToBottom PxRGBA) bitmapData True)
        --display FullScreen (white) (pictures $ imgsTrans imgs)
        where
                initSubjects imgs = take 7 $  cycle $ zipWith (\s img -> Subject s img) combinations (cycle imgs) :: [Subject]
                combinations = [ [c,b,a] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']]
                mainCharacter img = Subject "Main Character" img
