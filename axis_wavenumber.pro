; docformat='rst'
;
; :NAME:
;   overplot_second_axis
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
;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO AXIS_WAVENUMBER, plot_object, XAXIS=xaxis_dummy, YAXIS=yaxis, $
                     WL_TICKS=wlt, NANOMETRES=nm, VERBOSE=verb,$
                     MAX_TICKS=max_ticks, $
                     NO_UNITS=no_units, HELP=help
;+
;
; Converts the second x- or y- axis to wavelengths, assuming that
; the main x- or y- axis is in wavenumber units.
;
; :PARAMS:
;    plot_object: in, required, type=objref
;      A plot object that has already been created.
;
; :KEYWORDS:
;    XAXIS: in, optional, default=true, type=boolean
;      Define the x-axis as the axis of wavenumber (the default).
;
;    YAXIS: in, optional, default=false, type=boolean
;      Define the y-axis as the axis of wavenumber.
;
;    WL_TICKS: in, optional, type=fltarr
;      Define the wavelength values at which we show the wavelength.
;
;    NANOMETRES: in, optional, type=boolean, default=false
;      Write wavelengths in nm instead of the default microns.
;
;    VERBOSE: in, optional, type=boolean, hidden
;      Leave messages.
;
;    MAX_TICKS: in, optional, type=integer
;      The maximum number of ticks permitted.
;
;    NO_UNITS: in, optional, type=boolean, default=false
;      Hide the units from the wavelength axis.
;
;    HELP: in, optional, type=boolean, default=false, hidden
;
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;
; :HISTORY:
;
;    19 Mar 2015 (AJAS) Created.
;
;- 


   ON_ERROR, 2
   COMPILE_OPT idl2

   IF KEYWORD_SET( help ) THEN BEGIN
      FG_HELP, 'axis_wavenumber'
      RETURN
   ENDIF

   ;; If YAXIS not set, then default XAXIS is used.
   xaxis = KEYWORD_SET(yaxis) ? 0b : 1b


   ;; Attempt to get the axes values. If they don't exist, then
   ;; attempting to overplot a second axis is probably a bad idea.
   CATCH, axisError
   IF axisError THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, 'The objref passed does not have valid AXIS object.'
      RETURN
   ENDIF

   ;; The idl documentation 'Axis References in IDL' reliably informs me that
   ;; you can use the  plot object as a hash and get back axis1 (always the left
   ;; y-axis for 2D plot) and axis3 (always right y-axis for a 2D plot).
   axWn = xaxis ? plot_object[ 'axis2' ] : plot_object[ 'axis3' ]

   ;; Get the range we want.
   wn_range = xaxis ? plot_object.xrange : plot_object.yrange

   ;; If we've got this far, stop worrying about axes.
   CATCH, /CANCEL
 

   ; Convert to wl in microns 
   wl_range = (KEYWORD_SET(nm) ? 1e7 : 1e4) / REVERSE(wn_range*1.0)


   ;; Unit string
   unit = KEYWORD_SET(no_units) ? "" : (KEYWORD_SET(nm) ? " nm" : " $\mu$m")
   

   IF ~ KEYWORD_SET(max_ticks) THEN max_ticks = 20

   IF KEYWORD_SET(wlt) THEN BEGIN
       wl_int_wl = wlt
       n_int_wl  = N_ELEMENTS(wlt)
       tickname  = MAX(wlt-ROUND(wlt)) LT 0.001 ? $
                   STRING(ROUND(wl_int_wl),FORMAT='(i0,"'+unit+'")') : $
                   STRING(wl_int_wl,FORMAT='(f0.2,"'+unit+'")')
   ENDIF ELSE BEGIN
       ;; Get the minimum integer values of wl.
       min_int_wl = CEIL(  wl_range[0] )
       max_int_wl = FLOOR( wl_range[1] )
       n_int_wl   = max_int_wl - min_int_wl + 1l

       ;; Make a list of every wn where wl is integer
       wl_int_wl  = reverse(findgen(n_int_wl)+min_int_wl)
       WHILE N_ELEMENTS(wl_int_wl) GT max_ticks DO BEGIN
          n_int_wl = N_ELEMENTS(wl_int_wl)/2
          odd = n_int_wl MOD 2 EQ 0 ? 0 : 1
          wl_int_wl = wl_int_wl[ INDGEN(n_int_wl)*2 + odd]
       ENDWHILE
       tickname = STRING(ROUND(wl_int_wl),FORMAT='(i0,"'+unit+'")')
   ENDELSE

   wn_int_wl  = (KEYWORD_SET(nm) ? 1e7 : 1e4) / wl_int_wl

   IF KEYWORD_SET(verb) THEN PRINT, tickname,wl_int_wl, wn_int_wl

   ; Number of required ticks.
   ;nticks = (keyword_set(nticks) ? nticks : 6l) < n_int_wl
   nticks = n_int_wl

   
   axWn.TICKVALUES = wn_int_wl
   axWn.TICKNAME = tickname
   axWn.TITLE = ''
   axWn.MINOR = 0
   axWn.SHOWTEXT = 1


END
