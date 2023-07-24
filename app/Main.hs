module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import GraphicsUtils 
import Data.ByteString (ByteString, pack)


paths = ["D:\\Hierarcy\\images\\"++show i ++".bmp" | i <-  [1..9]]
imgsTrans imgs = [translate (i*300) 0 img | (img,i) <- zip imgs [1..9]] :: [Picture]


--colors = map (dark)  [red,green,rose, yellow, chartreuse, rose,  chartreuse, aquamarine, azure, violet]
bitmapData = pack $ take 40000 (cycle [200,10,10,55])
main :: IO ()
main = do 
              imgs <- mapM loadBMP paths
              (width, height) <- getScreenSize
              putStrLn $ "width: " ++ (show width) ++ " height: " ++ (show height)
              display FullScreen (dark blue) ( ( coloredDrawing width height) <> 
                                               ( scale 0.7 0.7 $ imgs!!0)   <>
                                               ( translate 500 150 $ scale 0.7 0.7 $ imgs!!1) <>
                                               ( translate (-500) 150 $ scale 0.7 0.7 $ imgs!!2) <>
                                               ( translate 0 (350) $ scale 0.7 0.7 $ imgs!!3 ) <>
                                               ( translate 400 (-250) $ scale 0.7 0.7 $ imgs!!4 ) <>
                                               ( translate (-400) (-250) $ scale 0.7 0.7 $ imgs!!5 ) ) 
              --display FullScreen (white) (bitmapOfByteString 100 100 (BitmapFormat TopToBottom PxRGBA) bitmapData True)
              --display FullScreen (white) (pictures $ imgsTrans imgs)

