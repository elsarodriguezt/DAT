
function caseDrawing(drw, fFill, fOutl, fCooP, fTrans, fCol, fBlnk, fMany) {
    var tag = drw[0];
    if (tag == 'Fll') return fFill(drw[1]);
    else if (tag == 'Otl') return fOutl(drw[1]);
    else if (tag == 'Crd') return fCooP();
    else if (tag == 'Trn') return fTrans(drw[1], drw[2]);
    else if (tag == 'Col') return fCol(drw[1], drw[2]);
    else if (tag == 'Bln') return fBlnk();
    else /*(tag == 'Mny')*/ return fMany(drw[1]);
}

function caseShape(shape, fRect, fPath, fEllip, fText) {
    var tag = shape[0];
    if (tag == 'Rct') return fRect(shape[1], shape[2]);
    else if (tag == 'Pth') return fPath(shape[1], shape[2]);
    else if (tag == 'Ell') return fEllip(shape[1], shape[2]);
    else /*(tag == 'Txt')*/ return fText(shape[1], shape[2]);
}


//unitSize :: Double
const unitSize = 20.0; // in pixels

const trUnit = [1, 0, 0, 1, 0, 0];

function trMult (a, b) {
/* [a11 a21 a31]   [b11 b21 b31]   [a11*b11+a21*b12 a11*b21+a21*b22 a11*b31+a21*b32+a31]
   [a12 a22 a32] x [b12 b22 b32] = [a12*b11+a22*b12 a12*b21+a22*b22 a12*b31+a22*b32+a32]
   [0   0   1  ]   [0   0   1  ]   [0               0               1                  ]
*/
    return [ a[0]*b[0]+a[2]*b[1],      a[1]*b[0]+a[3]*b[1],
             a[0]*b[2]+a[2]*b[3],      a[1]*b[2]+a[3]*b[3],
             a[0]*b[4]+a[2]*b[5]+a[4], a[1]*b[4]+a[3]*b[5]+a[5] ];
}

function isTranslation(tr) {
    if (tr[0] == 1 && tr[1] == 0 && tr[2] == 0 && tr[3] == 1) return [tr[4], tr[5]];
    else return null;
}

// 'viewXMax' and 'viewYMax' are in pixels
//renderSvgText :: Int -> Int -> Drawing -> String
function renderSvgText(viewXMax, viewYMax, draw) {
  return "<svg width='" + (viewXMax*2) + "' height='" + (viewYMax*2) + "' viewBox='"
    +   (-viewXMax) + " " + (-viewYMax) + " " + (viewXMax*2) + " " + (viewYMax*2)
    + "' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'"
    +   " buffered-rendering='dynamic'>\n<g font-size='14px' pointer-events='none'>\n"
    + "<rect vector-effect='non-scaling-stroke' fill='none' stroke='gray' stroke-width='2' x='"
    +   (-viewXMax) + "' y='" + (-viewYMax) + "' width='" + (viewXMax*2) + "' height='" + (viewYMax*2) + "'/>\n"
    + pprDrawing(viewXMax, viewYMax, trUnit, "black", draw)
    + "</g>\n</svg>\n";
}

//pprDrawing :: Int -> Int -> Transform -> Drawing -> String
function pprDrawing(viewXMax, viewYMax, tr, co, draw) {
    return caseDrawing(draw,
        function(shape)     { return pprShape(tr, attrsFill(co), shape); },             // Fill
        function(shape)     { return pprShape(tr, attrsOutline(co), shape); },          // Outline
        function()          { return pprCP(viewXMax, viewYMax, tr); },                  // CoordinatePlane
        function(tr2, d)    { return pprDrawing(viewXMax,viewYMax, trMult(tr, tr2), co, d); }, // Transformed
        function(color2, d) { return pprDrawing(viewXMax, viewYMax, tr, color2, d); },  // Colored
        function()          { return ""; },                                             // Blank
        function(ds)        { return ds.map(function(d){ return pprDrawing(viewXMax, viewYMax, tr, co, d); }).join(""); } // Many
    );
}

function pprCP(viewXMax, viewYMax, tr) {
    function hlines(n) {
        var c = n * unitSize;
        return line(-viewXMax, c, viewXMax, c)      // horitzontal line at y = -n
          + line(-viewXMax, -c, viewXMax, -c)       // horitzontal line at y = n
          + number("end", 0, c, -n)
          + number("end", 0, -c, n);
    };
    function vlines(n) {
        var c = n * unitSize;
        return line(c, -viewYMax, c, viewYMax)      // vertical line at x = n
          + line(-c, -viewYMax, -c, viewYMax)       // vertical line at x = -n
          + number("middle", c, unitSize*0.5, n)
          + number("middle", -c, unitSize*0.5, -n);
    };
    function line(x1, y1, x2, y2) {
        return "<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='1' x1='" + x1 + "'  y1='" + y1
                + "' x2='" + x2 + "' y2='" + y2 + "'/>\n";
    };
    function number(anchor, x, y, n) {
        return "<text fill='gray' text-anchor='" + anchor
                + "' transform='translate(" + x + "," + y + ") scale(0.7,0.7)' x='0' y='0'>" + n + "</text>\n";
    };
    return "<g" + attrTransform(tr) + ">\n"
            + "<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='2' x1='" + (-viewXMax) + "' y1='0' x2='" + viewXMax + "' y2='0'/>\n"
            + "<line vector-effect='non-scaling-stroke' stroke='gray' stroke-width='2' x1='0' y1='" + (-viewYMax) + "' x2='0' y2='" + viewYMax + "'/>\n"
            + mapRange(1, Math.round(viewYMax/unitSize), hlines).join("")
            + mapRange(1, Math.round(viewXMax/unitSize), vlines).join("")
         + "</g>\n";
}

function mapRange (n1, n2, f) {
    var out = [];
    for (var n = n1; n <= n2; n++) {
        out.push(f(n));
    }
    return out;
}

//attrsFill :: Color -> String
function attrsFill(c) { return " fill='" + c + "'"; }

//attrsOutline :: Color -> String
function attrsOutline(c) { return " fill='none' vector-effect='non-scaling-stroke' stroke='" + c + "' stroke-width='1'"; }

//pprShape :: Transform -> String -> Shape -> String
function pprShape(tr, attrs, sh) {
    var mbt = isTranslation(tr);
    if (mbt) return pprShape2(mbt[0], mbt[1], attrs, sh);
    else return "<g" + attrTransform(tr) + ">\n" + pprShape2(0, 0, attrs, sh) + "</g>\n";
}

//attrTransform :: Transform -> String
function attrTransform (tr) {
             /*      [ u  0 0 ]      H x Tapp = Tsvg x H
                 H = [ 0 -u 0 ]      Tsvg = H x Tapp x H^(-1)
                     [ 0  0 1 ]
                          [ 1/u  0  0 ]
                 H^(-1) = [  0 -1/u 0 ]
                          [  0   0  1 ]

                            [ m11 m21 m31 ]            [ m11*u  m21*u  m31*u  ]            [ m11  -m21 m31*u  ]
                 Tsvg = H x [ m12 m22 m32 ] x H^(-1) = [ -m12*u -m22*u -m32*u ] x H^(-1) = [ -m12 m22  -m32*u ]
                            [ 0   0   1   ]            [ 0      0      1      ]            [ 0    0    1      ]
             */
    return " transform='matrix(" + tr[0] + "," + (-tr[1]) + ","
                                 + (-tr[2]) + "," + tr[3] + ","
                                 + (tr[4]*unitSize) + "," + (-tr[5]*unitSize) + ")'";
}

//pprShape2 :: Double -> Double -> String -> Shape -> String
function pprShape2 (x0, y0, attrs, shape) {
    return caseShape(shape,
        function(w, h) { // Rect
            var x2 = (x0-w/2)*unitSize, y2 = (-y0-h/2)*unitSize, w2 = w*unitSize, h2 = h*unitSize;
            return "<rect" + attrs + " x='" + x2 + "' y='" + y2 + "' width='" + w2 + "' height='" + h2 + "'/>\n";
        },
        function(closed, ps) { // Path
            var n = ps.length;
            function point (p) {
                return ((x0+p[0])*unitSize) + " " + (-(y0+p[1])*unitSize);
            };
            function go (i) {
                if (i < n) return " L" + point(ps[i]) + go(i+1);
                else if (closed) return " Z'/>\n"; else return "'/>\n";
            };
            if (n > 0) return "<path" + attrs + " d='M" + point(ps[0]) + go(1);
            else return "";
        },
        function(rx, ry) { // Ellipse
            var x2 = x0*unitSize, y2 = -y0*unitSize, rx2 = rx*unitSize, ry2 = ry*unitSize;
            return "<ellipse" + attrs + " cx='" + x2 + "' cy='" + y2 + "' rx='" + rx2 + "' ry='" + ry2 + "'/>\n";
        },
        function(anchor, s) { // Text
            var x2 = x0*unitSize, y2 = -y0*unitSize;
            return "<text" + attrs + " text-anchor='" + anchor + "' x='" + x2 + "' y='" + y2 + "'>" + escapeHtml(s) + "</text>\n"; 
        }
    );
}

//escapeHtml :: String -> String
function escapeHtml (s) {
    var out = "";
    for (var x = 0, c = ""; c = s.charAt(x); x++) {
        if (c == '<') out += "&lt;";
        else if (c == '&') out += "&amp;";
        else if (c == '>') out += "&gt;";
        else if (c == '\"') out += "&quot;";
        else if (c == '\'') out += "&apos;";
        else out += c;
    }
    return out;
}

