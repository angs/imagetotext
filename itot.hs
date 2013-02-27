import Codec.Picture
import Codec.Picture.Types
import System.Environment
import Control.Monad
import System.Console.ANSI
import TermSize
import Data.List

pixelAspectX = 8
pixelAspectY = 17

scaleH = True

main = do
	(termh', termw') <- getTermSize
	when (termh' < 1 || termw' < 1) $ error "Terminal too small"
	a <- getArgs
	when (null a) $ error "Usage: itot filename"
	imagefile <- readImage $ head a

	let kuva@(Image w h dat) = case imagefile of
		Left str -> error str
		Right im -> case im of
			ImageYCbCr8 i -> i
			ImageRGB8 i -> convertImage i :: Image PixelYCbCr8
			ImageRGBA8 i -> convertImage $ dropAlphaLayer i
			_ -> error "Pixel format not yet supported"

	let (termw, termh) = case scaleH of
		True -> case (termw'*pixelAspectX*h < termh'*pixelAspectY*w) of
			True -> (termw', div (termh'*h*termw'*pixelAspectX) (w*termh'*pixelAspectY))
			_ -> (div (termw'*w*termh'*pixelAspectY) (h*termw'*pixelAspectX), termh')
		False -> (termw', div (h*termw'*pixelAspectX) (w*pixelAspectY))

	let (limitsx,limitsy) = 
		( map (\i -> div (w*i) termw) [0..termw]
		, map (\i -> div (h*i) termh) [0..termh]
		)::([Int],[Int])
	let thumbnail@(Image iW iH iD) = convertImage (generateImage pr termw termh) :: Image PixelRGB8
		where 
		--For terminal-sized thumbnail just take average of Y, Cb, Cr channels for each corresponding square.
		--Some filtering would be smarter
		pr :: Int -> Int -> PixelYCbCr8
		pr x y = means $ foldl'
			(\(s1,s2,s3,t) (PixelYCbCr8 y c b) -> (fromIntegral y + s1, fromIntegral c + s2, fromIntegral b + s3, t+1)) 
			(0::Int, 0::Int, 0::Int, 0::Int) 
			[pixelAt kuva x y | x <- [limitsx!!x..limitsx!!(x+1)-1], y <- [limitsy!!y..limitsy!!(y+1)-1]] 
		means :: (Int, Int, Int, Int) -> PixelYCbCr8
		means (y,b,r,t) = PixelYCbCr8 (fromIntegral $ div y t) (fromIntegral $ div b t) (fromIntegral $ div r t)

	forM_ [(y,x) | y<-[0..termh-1], x<-[0..termw-1]] (\(y,x) -> do
		when (x==0) $ putStr "\n"
		putStr $ pixToANSI $ pixelAt thumbnail x y) 
	putChar '\n'

--pixToANSI: Convert RGB pixel to HSV and pick approximately corresponding ANSI color using hue and 
pixToANSI :: PixelRGB8 -> String
pixToANSI (PixelRGB8 r g b) =
	setSGRCode [SetColor Foreground (fst letter) c] ++ [snd letter]
	where
	-- Not used: col = " ░▒▓█"
	cols = [(Dull, ' '), (Dull, '░'), (Vivid, '░'), (Dull, '▒'), (Vivid, '▒'), (Dull, '▓'), (Dull, '█'), (Vivid, '▓'), (Vivid, '█')]
	l = fromIntegral $ length cols
	-- Not used: col2 = " ▘▝▀▖▌▞▛▗▚▐▜▄▙▟█"
	r' = (fromIntegral r)/256.0
	g' = (fromIntegral g)/256.0
	b' = (fromIntegral b)/256.0
	ma = maximum [r',g',b']
	mi = minimum [r',g',b']
	alpha = 0.5*(2.0*r' - g' - b')
	beta = sqrt 3 / 2 * (g'-b')
	hue = floor $ (/pi) $ (*3) $ (\a -> if (a<0.0) then a+2.0*pi else a) $ (+(pi/6)) $ atan2 beta alpha
	chroma = sqrt $ alpha*alpha + beta*beta
	s = chroma / value
	value = ma
	c = if s < 0.36 then White else [Red,Yellow,Green,Cyan,Blue,Magenta]!!hue
	letter = cols!!(floor $ value * l)

