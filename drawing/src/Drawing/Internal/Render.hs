
{-# LANGUAGE OverloadedStrings #-}

module Drawing.Internal.Render where
import           Drawing.Internal.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder as TL

-- 'viewXMax' and 'viewYMax' are in pixels
renderSvgText :: Int -> Int -> Drawing -> (Double, TL.Text)
renderSvgText viewXMax viewYMax draw =
    toLazyText <$> renderSvgText' viewXMax viewYMax draw

renderSvgText' :: Int -> Int -> Drawing -> (Double, TL.Builder)
renderSvgText' viewXMax viewYMax draw =
    ( unitSize
    , pprDrawingSvg viewXMax viewYMax draw
    )


unitSize :: Double
unitSize = 20.0 -- in pixels

pprDrawingSvg :: Int -> Int -> Drawing -> TL.Builder
pprDrawingSvg viewXMax viewYMax draw =
    "<svg width='" <> fromInt (viewXMax*2) <> "' height='" <> fromInt (viewYMax*2) <>
                "' viewBox='" <> fromInt (-viewXMax) <> " " <> fromInt (-viewYMax) <> " " <> fromInt (viewXMax*2) <> " " <> fromInt (viewYMax*2) <>
                "' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>\n<g font-size='14px' pointer-events='none'>\n"
        <> "<rect vector-effect='non-scaling-stroke' fill='none' stroke='gray' stroke-width='2'" <>
                " x='" <> fromInt (-viewXMax) <> "' y='" <> fromInt (-viewYMax) <>
                "' width='" <> fromInt (viewXMax*2) <> "' height='" <> fromInt (viewYMax*2) <> "'/>\n"
        <> pprDrawing viewXMax viewYMax mempty (RGB 0 0 0) draw
        <> "</g>\n</svg>\n"

-- 'fromInt' and 'fromDouble' seem more efficient than 'Tl.decimal' and 'Tl.realFloat'.
fromInt :: Int -> TL.Builder
fromInt x = fromString $ show x

fromDouble :: Double -> TL.Builder
fromDouble x = fromString $ show x

pprDrawing :: Int -> Int -> Transform -> Color -> Drawing -> TL.Builder
pprDrawing _ _ tr color (Fill shape) =
    pprShape tr (pprFill color) shape
pprDrawing _ _ tr color (Outline shape) =
    pprShape tr (pprOutline color) shape
pprDrawing viewXMax viewYMax tr color (Transformed tr2 draw) =
    pprDrawing viewXMax viewYMax (tr <> tr2) color draw
pprDrawing viewXMax viewYMax tr _ (Colored color2 draw) =
    pprDrawing viewXMax viewYMax tr color2 draw
pprDrawing _ _ _ _ Blank =
    mempty
pprDrawing viewXMax viewYMax tr color (Many draws) =
    mconcat (pprDrawing viewXMax viewYMax tr color <$> draws [])
pprDrawing viewXMax viewYMax tr _ CoordinatePlane =
    let atr = pprTransform tr
    in "<g" <> atr <> ">\n"
          <> "<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='2' x1='-" <> fromInt viewXMax <> "' y1='0' x2='" <> fromInt viewXMax <> "' y2='0'/>\n"
          <> "<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='2' x1='0' y1='-" <> fromInt viewYMax <> "' x2='0' y2='" <> fromInt viewYMax <> "'/>\n"
          <> mconcat (hlines <$> [1 .. round (viewYMax' / unitSize)])
          <> mconcat (vlines <$> [1 .. round (viewXMax' / unitSize)])
          <> "</g>\n"
    where
        viewXMax' = fromIntegral viewXMax
        viewYMax' = fromIntegral viewYMax
        hlines, vlines :: Int -> TL.Builder
        hlines n =
          let c = fromIntegral n * unitSize
          in line (-viewXMax') c    viewXMax' c          -- horitzontal line at y = -n
          <> line (-viewXMax') (-c) viewXMax' (-c)       -- horitzontal line at y = n
          <> number "end" 0 c            (-n)
          <> number "end" 0 (-c)         n
        vlines n =
          let c = fromIntegral n * unitSize
          in line c (-viewYMax')    c viewYMax'          -- vertical line at x = n
          <> line (-c) (-viewYMax') (-c) viewYMax'       -- vertical line at x = -n
          <> number "middle" c    (unitSize*0.5) n
          <> number "middle" (-c) (unitSize*0.5) (-n)
        line x1 y1 x2 y2 =
            "<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='1' x1='" <> fromDouble x1 <> "'  y1='" <> fromDouble y1
                <> "' x2='" <> fromDouble x2 <> "' y2='" <> fromDouble y2 <> "'/>\n"
        number anchor x y n =
            "<text fill='gray' text-anchor='" <> anchor
                <> "' transform='translate(" <> fromDouble x <> "," <> fromDouble y <> ") scale(0.7,0.7)' x='0' y='0'>" <> fromInt n <> "</text>\n"

pprFill :: Color -> TL.Builder
pprFill c =
    " fill='" <> pprColor c <> "'"

pprOutline :: Color -> TL.Builder
pprOutline c =
    " fill='none' vector-effect='non-scaling-stroke' stroke='" <> pprColor c <> "' stroke-width='1'"

pprColor :: Color -> TL.Builder
pprColor (RGB r g b) =
    "rgb(" <> fromComp r <> "," <> fromComp g <> "," <> fromComp b <> ")"
    where
        fromComp n = fromInt $ round $ n * 255

pprShape :: Transform -> TL.Builder -> Shape -> TL.Builder
pprShape tr attrs (Path closed ps) =
    pprShape' 0 0 attrs (Path closed (transformPoint tr <$> ps))
pprShape tr attrs sh =
    case isTranslation tr of
        Just (tx, ty) ->
            pprShape' tx ty attrs sh
        Nothing ->
            pprShape' 0 0 (pprTransform tr <> attrs) sh

pprTransform :: Transform -> TL.Builder
pprTransform (Transform m11 m12 m21 m22 m31 m32) =
    " transform='matrix(" <> fromDouble m11 <> "," <> fromDouble (-m12) <> ","
                          <> fromDouble (-m21) <> "," <> fromDouble m22 <> ","
                          <> fromDouble (m31*unitSize) <> "," <> fromDouble (-m32*unitSize) <> ")'"
             {--     [ u  0 0 ]      H x Tapp = Tsvg x H
                 H = [ 0 -u 0 ]      Tsvg = H x Tapp x H^(-1)
                     [ 0  0 1 ]
                          [ 1/u  0  0 ]
                 H^(-1) = [  0 -1/u 0 ]
                          [  0   0  1 ]

                            [ m11 m21 m31 ]            [ m11*u  m21*u  m31*u  ]            [ m11  -m21 m31*u  ]
                 Tsvg = H x [ m12 m22 m32 ] x H^(-1) = [ -m12*u -m22*u -m32*u ] x H^(-1) = [ -m12 m22  -m32*u ]
                            [ 0   0   1   ]            [ 0      0      1      ]            [ 0    0    1      ]
             --}

pprShape' :: Double -> Double -> TL.Builder -> Shape -> TL.Builder
pprShape' x0 y0 attrs (Rect w h) =
    let x' = (x0-w/2)*unitSize; y' = (-y0-h/2)*unitSize; w' = w*unitSize; h' = h*unitSize
    in "<rect" <> attrs <> " x='" <> fromDouble x' <> "' y='" <> fromDouble y'
               <> "' width='" <> fromDouble w' <> "' height='" <> fromDouble h' <> "'/>\n"
pprShape' x0 y0 attrs (Ellipse rx ry) =
    let x' = x0*unitSize; y' = -y0*unitSize; rx' = rx*unitSize; ry' = ry*unitSize
    in "<ellipse" <> attrs <> " cx='" <> fromDouble x' <> "' cy='" <> fromDouble y' <> "' rx='" <> fromDouble rx' <> "' ry='" <> fromDouble ry' <> "'/>\n"
pprShape' _ _ _ (Path _closed []    ) = mempty
pprShape' x0 y0 attrs (Path closed (p:ps)) =
    "<path" <> attrs <> " d='M" <> point p <> go ps
    where
        point (x, y) =
            let x' = (x0+x)*unitSize; y' = -(y0+y)*unitSize
            in fromDouble x' <> " " <> fromDouble y'
        go (p':ps') = " L" <> point p' <> go ps'
        go []     = if closed then " Z'/>\n" else "'/>\n"
pprShape' x0 y0 attrs (TextS anchor s) =
    let x' = x0*unitSize; y' = -y0*unitSize
    in "<text" <> attrs <> " text-anchor='" <> fromText anchor <> "' x='" <> fromDouble x' <> "' y='" <> fromDouble y' <> "'>" <> escapeHtml s <> "</text>\n"

escapeHtml :: T.Text -> TL.Builder
escapeHtml =
    let convert '<'  = "&lt;"
        convert '&'  = "&amp;"
        convert '>'  = "&gt;"
        convert '\"' = "&quot;"
        convert '\'' = "&apos;"
        convert c    = T.singleton c
    in fromText . T.concatMap convert

