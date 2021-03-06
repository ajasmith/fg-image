; docformat='rst'
;
; :NAME:
;   HECKBERT
;
;
;
; :COPYRIGHT:
;
;   The MIT License (MIT)
;
;   Copyright (c) 2015 Andy Smith <aja.smith (at) gmail.com>
;
;   Permission is hereby granted, free of charge, to any person obtaining a copy
;   of this software and associated documentation files (the "Software"), to deal
;   in the Software without restriction, including without limitation the rights
;   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;   copies of the Software, and to permit persons to whom the Software is
;   furnished to do so, subject to the following conditions:
;
;   The above copyright notice and this permission notice shall be included in all
;   copies or substantial portions of the Software.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;   SOFTWARE.
;
;
 FUNCTION heckbert_nicenum, x, round
;+
;
; Find a "nice" number approximately equal to x.
; Round the number if round is set, otherwise
; take the ceiling.
;
; :PARAMS:
;    x: in, required, type=float;
;    round: in, required, type=boolean
;
;
; :HIDDEN:
;-

   COMPILE_OPT hidden, idl2

   e = FLOOR( ALOG10( x ) )
   f = x / 10.0^e
  
   IF round THEN BEGIN
      IF (f LT 1.5) THEN nf = 1 $ 
      ELSE IF (f LT 3) THEN nf = 2 $
      ELSE IF (f LT 7 ) THEN nf = 5 $
      ELSE nf=10
   ENDIF ELSE BEGIN
      IF f LE 1 THEN nf = 1 $
      ELSE IF (f LE 2 ) THEN nf = 2 $
      ELSE IF (f LE 5 ) THEN nf = 5 $
      ELSE nf = 10
   ENDELSE

   RETURN, nf * (10^e)

 END



;+
; Implementation of Heckbert's labeling algorithm which can be
; found at `http://tog.acm.org/resources/GraphicsGems/gems/Label.c`
;
;
; :Categories:
;     Function graphics, mappoints
;
; :PARAMS:
; 
;     range_in: in, required, type=FLTARR(2)
;          The range of values.
;
;     n: in, required, type=integer
;          The number of ticks wanted in the given range.
;
;
; :KEYWORDS:
;    help: in, optional, type=boolean
;          Load the help page for this routine.
;
;
; :RETURNS:
;     An array of `n` tick points for the given `range_in`.
;     Tick points may be outside the range if this improves
;     the aesthetic. If the range is invalid, then `NaN` is returned.
;
; :EXAMPLES:
;     Pick 4 numbers for a scale between -0.1 and 2.4::
; 
;          IDL> PRINT, HECKBERT( [-0.1, 2.4], 4 )
;                    -1           0           1           2
;
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
; :HISTORY:
;    04 MAR 2014 (AJAS) Created.
;
;    12 DEC 2014 (AJAS) Check for finite range before starting.
;
;
;-
 FUNCTION HECKBERT, range_in, n, help=help

   IF KEYWORD_SET(help) THEN BEGIN
      FG_HELP, 'heckbert'
      RETURN, 0
   ENDIF


   IF FINITE(range_in[0]) AND FINITE(range_in[1]) THEN BEGIN

      ;; If the range is in reverse order, then call in reverse!
      IF range_in[1]-range_in[0] LT 0 THEN BEGIN
         RETURN,REVERSE( HECKBERT(REVERSE(range_in), n ) )
      ENDIF

      ;; Get a nice number range
      range = HECKBERT_NICENUM( range_in[1]-range_in[0], 0b )
      lstep = HECKBERT_NICENUM( range / (n-1), 1b )

      IF lstep EQ 0 THEN BEGIN
         RETURN, HECKBERT( range_in*10, n ) * 0.1
      ENDIF
      lmin = FLOOR( range_in[0]/lstep ) * lstep
      lmax = CEIL(  range_in[1]/lstep ) * lstep

      RETURN, lmin + INDGEN(n)*lstep
   ENDIF ELSE BEGIN
      
      MESSAGE,'Range values are not appropriate',/CONTINUE
      RETURN, !VALUES.F_NAN
   ENDELSE
END

;
