; docformat = 'rst'
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
FUNCTION OVERPLOT_SECOND_AXIS, plot_object, x, y, y_error, $
                               DONT_REDRAW=dont_redraw, $
                               YERROR=yerr_kw, $
                               YRANGE=yrange, $
                               YLOG=ylog, $
                               YTITLE=ytitle, $
                               COLOR=color, $
                               HELP=help, $
                               _EXTRA=extra
;+
;    
;    .. image:: overplot_second_axis.png
;
;    Overplot data with a different y-range by replacing the right y-axis.
;
;
; :RETURNS:
;
;    An object reference to a plot.
;
;
; :PARAMS:
;
;    plot_object: in, required, type=objref
;       The plot we will be adding a second axis to.
;
;    x: in, required, type=numeric
;
;    y: in, optional, type=numeric
;
;    y_error: in, optional, type=numeric
;       Optional error bar on the y-data.
;
;
; :KEYWORDS:
;
;    DONT_REDRAW: in, optional, type=boolean
;       If the second axis is already to your liking, don't redraw.
;
;    YERROR: in, optional, type=boolean
;       Plot error bars.
;
;    YRANGE: in, optional, type="fltarr(2)"
;
;    YLOG: in, hidden, type=boolean
;       This keyword is not functional yet.
;
;    YTITLE: in, optional, type=string
;
;    COLOR: in, optional, type="string / bytarr(3)"
;
;    HELP: in, optional, hidden, type=boolean
;
;    _EXTRA: optional
;       Additional keyword options to pass to the `PLOT` command.
;       
;    
; :EXAMPLES:
;
;     Create some exciting data with different ranges::
;
;       IDL> t  = FINDGEN( 1718 ) * !PI / 180.0
;       IDL> y1 = SIN( t ) + COS( t * 2.1 )
;       IDL> y2 = 6.3 - t * 0.1 * (1 - SIN( t*1.4 ))
;       IDL> y3 = 6.4 + COS( t * 1.4 ) * 0.4
;       
;
;     And then plot it::
;
;       IDL> p1 = PLOT( t, y1, YRANGE=[-2,2], COLOR='Dodger blue', $
;                       YTITLE='Left axis range', $
;                       DIMENSION=[700, 300], $
;                       NAME='Left axis data' )
;       IDL> p2 = OVERPLOT_SECOND_AXIS( p1, t, y2, COLOR='Tomato', THICK=2, $
;                                       YTITLE='Right axis range', $
;                                       YRANGE=[1,7], NAME='Right axis data' )
;
;
;     Adding extra data to the new axis range can be carried out using the `DONT_REDRAW` keyword::
;      
;       IDL> p3 = OVERPLOT_SECOND_AXIS( p1, t, y3, /DONT_REDRAW, COLOR='Orange', $
;                                       NAME='Right axis data with DONT_REDRAW keyword' )
;
;       IDL> l = LEGEND( TARGET=[p1,p2,p3], FONT_SIZE=10, $
;                        HORIZONTAL_ALIGNMENT='Left', $
;                        VERTICAL_ALIGNMENT='Bottom', $
;                        POSITION=[1, -1.8], /DATA ) 
;
;
;
; :BUGS:
;    Cannot currently cope with logarithmic y-axes. An error will be
;    returned.
;
;    Additional overplotting (not by this routine) will wipe the text
;    from the right hand axis. You will then need to re-show it by
;    setting the SHOWTEXT value to 1 on the correct axis.
;    
;
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;
; :HISTORY:
;
;    26 Feb 2015 (AJAS) Created initial test version.
;
;    02 Mar 2015 (AJAS) Bug with /DONT_REDRAW fixed. Documentation added.
;
;-


   ;ON_ERROR, 2
   COMPILE_OPT IDL2

   IF KEYWORD_SET( help ) THEN BEGIN
      FG_HELP, 'overplot_second_axis'
      RETURN, !NULL
   ENDIF

   IF KEYWORD_SET( ylog ) THEN MESSAGE, $
      'Plotting on a log axis not currently supported.'



   ;; Attempt to get the axes values. If they don't exist, then
   ;; attempting to overplot a second axis is probably a bad idea.
   CATCH, axisError
   IF axisError THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, 'The objref passed does not have valid AXES.'
      RETURN, !NULL
   ENDIF
   ax = plot_object.axes
   CATCH, /CANCEL
   
   ;; Get the left and right y-axis
   aloc = []
   FOREACH a, ax DO aloc = [ aloc, ISA(a,'Axis') ? TOTAL((a.location GT 0)*[1,2,4],/INT) : -1 ]
   iyLeft = ( WHERE( aloc EQ 2, nLeft ) )[0]
   iyRight= ( WHERE( aloc EQ 1, nRight) )[0]
   IF nLeft EQ 0 || nRight EQ 0 THEN MESSAGE, 'Valid y-axes not found.'


   ;; Get the x and y values.
   yplot = N_ELEMENTS( y ) GE 1 ? y : x
   xplot = N_ELEMENTS( y ) GE 1 ? x : LINDGEN(N_ELEMENTS(x))
   eplot = KEYWORD_SET(yerr_kw) && N_ELEMENTS( y_error) GE 1 ? y_error : !NULL


   IF KEYWORD_SET( dont_redraw ) THEN BEGIN
      ;; If we don't redraw, then we will be getting all of
      ;; our information from the axes already present.
      ;; We assume that the right axis is what we are using
      ;; for our new scale.
      yr1 = ax[ iYLeft ].YRANGE
      
      ;; The forward transform is taken from the right y-axis.
      ct_forward = ax[iYRight].coord_transform

      ;; The second apparent y-range is then given by.
      yr2 = ax[ iYRight].YRANGE * ct_forward[1] + ct_forward[0]

      ;; So the reverse transform is:
      ct_backward = [yr1[0] - yr2[0]/ct_forward[1], 1.0/ct_forward[1] ]


      ;; Get the colour of the points if it's not been defined.
      IF ~ KEYWORD_SET( color ) THEN color = ax[iYRight].color
   ENDIF ELSE BEGIN
      ;; Otherwise, we define our new right axis by the relative
      ;; ranges required for the old data (left axis) and the 
      ;; new data.

      ;; The ranges of the new and old axes.
      yr1 = ax[ iYLeft ].YRANGE
      yr2 = KEYWORD_SET( yrange ) ? yrange : [MIN(yplot,MAX=maxY), maxY]

      ;; Coordinate transform from old to new and back again...
      ratio = (yr2[1]-yr2[0])/(yr1[1]-yr1[0])
      ct_forward = [yr2[0] - yr1[0] * ratio,   ratio  ]
      ct_backward= [yr1[0] - yr2[0] / ratio, 1.0/ratio]

      ;; Get the y-axis on the right, remove it, and replace it.
      
      ;; Remove old y-axis on the right and replace it.
      ax[iYRight] ->  DELETE

      ;; Create new one. The object is now done, so overwrite the ref.
      ax[iYRight] = AXIS('Y',LOCATION='Right', COLOR=color, $
                         TICKFONT_SIZE=plot_object.font_size, $
                         TICKFONT_NAME=plot_object.font_name, $
                         TICKFONT_STYLE=plot_object.font_style,$
                         TARGET=plot_object, $
                         COORD_TRANSFORM=ct_forward, $
                         TITLE=ytitle )

   ENDELSE

   ;; Now do our plotting of our new data.
   IF ~ KEYWORD_SET( yerr_kw ) THEN BEGIN

      p = PLOT(OVERPLOT=plot_object, /CURRENT, $
               COLOR=color, _STRICT_EXTRA=extra, $
               xplot, ct_backward[0] + yplot*ct_backward[1]  )

   ENDIF ELSE BEGIN
      
      ;; If we want error bars, these need to be scaled by the 
      ;; value of the coordinate transform as well (but not translated).
      p = ERRORPLOT(OVERPLOT=plot_object, /CURRENT, $
                    COLOR=color, _STRICT_EXTRA=extra, $
                    xplot, ct_backward[0] + yplot*ct_backward[1], $
                    eplot*ct_backward[1]  )

   ENDELSE

   ;; The axis for some reason has is text hidden. Put it back!
   ax[iYRight].SHOWTEXT = 1

   RETURN, p
END






;p1a = PLOT( [0,4,3], YTITLE='Test data 1',CURRENT=FG_CURRENT(p1a),NAME='black 1'  )
;p2a = PLOT( [0,1.5,2], [1,1,2.5], /CURRENT, /OVERPLOT, COLOR='grey',NAME='black 2' )
;p3a = OVERPLOT_SECOND_AXIS( p1a, [10,5,5],COLOR='Dodger blue',YTITLE='Test data 2', NAME='blue 1' )
;p4a = OVERPLOT_SECOND_AXIS( p1a, [7,6,6], /DONT_REDRAW, COLOR='Blue', NAME='blue 2' )

;l = LEGEND( TARGET=[p1a,p2a,p3a,p4a], POSITION=[0.98,0.98] )



t  = FINDGEN( 1718 ) * !PI / 180.0
y1 = SIN( t ) + COS( t * 2.1 )
y2 = 6.3 - t * 0.1 * (1 - SIN( t*1.4 ))
y3 = 6.4 + COS( t * 1.4 ) * 0.4


p1 = PLOT( t, y1, YRANGE=[-2,2], COLOR='Dodger blue', $
           YTITLE='Left axis range', $
           DIMENSION=[700, 300], $
           NAME='Left axis data', CURRENT=FG_CURRENT(p1) )

p2 = OVERPLOT_SECOND_AXIS( p1, t, y2, COLOR='Tomato', THICK=2, $
                           YTITLE='Right axis range', $
                           YRANGE=[1,7], NAME='Right axis data' )

p3 = OVERPLOT_SECOND_AXIS( p1, t, y3, /DONT_REDRAW, COLOR='Orange', $
                           NAME='Right axis data with DONT_REDRAW keyword' )

l = LEGEND( TARGET=[p1,p2,p3], FONT_SIZE=10, $
            HORIZONTAL_ALIGNMENT='Left', $
            VERTICAL_ALIGNMENT='Bottom', $
            POSITION=[1, -1.8], /DATA ) 


END
