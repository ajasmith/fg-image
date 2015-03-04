; docformat = 'rst'
;
; :NAME:
;   ip_roi_draw
;
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


PRO iprd_obj::setProperty, x=x, y=y
;+
; Set the properties of the ROI. Once this is done, no changes.
; X and Y must be set simultanaeously.
;
; :HIDDEN:
;-
  COMPILE_OPT idl2, hidden
  ON_ERROR, 2

  IF self.done THEN MESSAGE, 'The ROI can only be set once.'
  IF KEYWORD_SET(x) && KEYWORD_SET(y) && N_ELEMENTS(x) EQ N_ELEMENTS(y) THEN BEGIN
     self.x = PTR_NEW( /ALLOCATE )
     *self.x= REFORM( x, N_ELEMENTS( x ) )
     self.y = PTR_NEW( /ALLOCATE )
     *self.y= REFORM( y, N_ELEMENTS( y ) )

     self.done = 1B
  ENDIF ELSE MESSAGE, 'You must set X and Y simultaneously. They must have identical dimensions.'
END

PRO iprd_obj::getProperty, x=x, y=y, polygon=poly, roi=roi
;+
; We can only get polygon once the ROI has been defined.
;
; :HIDDEN:
;-
  IF self.done THEN BEGIN
     IF ARG_PRESENT( x ) THEN x= *self.x
     IF ARG_PRESENT( y ) THEN y= *self.y
     IF ARG_PRESENT( poly ) THEN poly = [ [*self.x], [*self.y] ]
     IF ARG_PRESENT( roi ) THEN roi = OBJ_NEW( 'IDLanROI', *self.X, *self.Y )
  ENDIF ELSE MESSAGE,/INFO, 'The ROI has not been set yet!'
END

PRO iprd_obj::cleanup
;+
; :HIDDEN:
;-
  COMPILE_OPT idl2, hidden
  PTR_FREE, self.x, self.y
END

FUNCTION iprd_obj::init
;+
; No need to do anything. Our object just sits about waiting for
; data from the main code.
;
; :HIDDEN:
;-
  COMPILE_OPT idl2, hidden
  RETURN, 1
END

PRO iprd_obj__define
;+
;  
;    Very simple object to pass back a roi once we have it.
;    If the `done` flag is not set, then we will not get x and y.
;
; :HIDDEN:
;-
  COMPILE_OPT idl2, hidden
  define = { iprd_obj, inherits IDL_OBJECT, x: PTR_NEW(), y: PTR_NEW(), done: 0B }
END









FUNCTION iprdMouseDown, oWin, x, y, iButton, KeyMods, nClicks
;+
; Mouse down code to pass to the image window.
;
; :HIDDEN:
;
;-

  state = oWin.UVALUE
  state.buttonDown = 1B

  ;; Check that the mouse is on top of the image.
  im = oWin.hitTest(x,y)

  ;; Only add the point if it lies within the image.
  IF ISA( im, 'IMAGE' ) THEN BEGIN


     ;; Left click = add tick
     IF iButton EQ 1 THEN BEGIN

        ;; Check that we're not at the maximum.
        IF state.n GT N_ELEMENTS(state.x)-2 THEN MESSAGE, 'Too many points for the region.'

        v = im.getValueAtLocation(x,y,/INTERPOLATE,/DEVICE)
        state.x[ state.n ] = v[0]
        state.y[ state.n ] = v[1]

        ;; Increment the counter.
        state.n ++

        ;; If we're at the 1st point, stop hiding and define two
        ;; data points.
        IF state.n EQ 1 THEN BEGIN
           state.line.hide = 0
           state.newline.hide = 0
           state.line -> setData, state.x[[0,0]], state.y[[0,0]]
        ENDIF ELSE BEGIN
           state.line -> setData, state.x[0:state.n-1], state.y[0:state.n-1]
        ENDELSE

        ;; Middle button = remove last tick
     ENDIF ELSE IF iButton EQ 2 THEN BEGIN
        state.n --
        CASE state.n OF 
          -1:    state.n = 0
           0:    state.line.hide = 1
           1:    state.line -> setData, state.x[ [0,0] ],   state.y[ [0,0] ]
           ELSE: state.line -> setData, state.x[0:state.n-1], state.y[0:state.n-1]
        ENDCASE

     ENDIF ELSE IF iButton EQ 4 THEN BEGIN
        
        ;; Loop back to the start
        state.n ++
        state.x[ state.n-1 ] = state.x[ 0 ]
        state.y[ state.n-1 ] = state.y[ 0 ]

        state.line -> setData, state.x[0:state.n-1], state.y[0:state.n-1]

        ;; Now that we've defined our polygon, save it to the object.
        state.object -> setProperty, X=state.x[0:state.n-1], Y=state.y[0:state.n-1]

        ;; Kill the graphics window
        oWin -> CLOSE

        ;; And message our success.
        HELP, state.object, OUTPUT=output
        PRINT, 'ROI is ready as '+output
        RETURN, 0
     ENDIF

  ENDIF

  oWin.UVALUE = state

  RETURN, 0 ;; Skip default event handling


END

FUNCTION iprdMouseMotion, oWin, x, y, KeyMods
;+
; Code for mouse motion. This is supposed to show where the next
; line goes, but since it seems to override the click handler, it's 
; not really helping at all. It all goes wrong when the POLYLINE
; is updated. Up until then, it's ok.
;
; :HIDDEN:
;-
  state = oWin.uvalue
  IF state.buttonDown THEN RETURN, 0
  ;; Check that the mouse is on top of the image.
  im = oWin.hitTest(x,y)

  ;; Only add the point if it lies within the image and we're not finished.
  IF ISA( im, 'IMAGE' ) && state.n GT 0 THEN BEGIN
     state.newline.hide = 0
     state.newline.GetData, prevX, prevY
     v = im.getValueAtLocation(x,y,/INTERPOLATE,/DEVICE)
     ;HELP, prevX[-1], v[0], prevY[-1], v[1]
     IF prevX[-1] NE v[0] && prevY[-1] NE v[1] THEN BEGIN
        lx = [ state.x[ state.n-1 ], v[0] ]
        ly = [ state.y[ state.n-1 ], v[1] ]
        ;state.newline.SetData, lx, ly
     ENDIF
  ENDIF


  RETURN, 0 ;; Skip default event handling

END

FUNCTION iprdMouseUp, oWin, x, y, iButton
;+
; Reset the buttonDown value.
;
; :HIDDEN:
;-
  state = oWin.uvalue
  state.buttonDown = 0B
  oWin.uvalue = state
  RETURN, 0
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 FUNCTION ip_roi_draw, image_object, HELP=help
;+
;
;  Open a dialogue window to select a region of interest for an image
;  file defined by `IMAGE_PLOT_F`. The returned variable will be an
;  object that becomes populated with a polygon once we have finished
;  defining the ROI.
;
;  :PARAMS:
;     image_object: in, required, type="IMAGE object reference"
;
;  :KEYWORDS:
;     HELP: in, optional, hidden, type=boolean
;
;  :EXAMPLES:
;
;     Generate an image, and then define a region of interest::
;
;        IDL> f = FILEPATH('nyny.dat', SUBDIRECTORY = ['examples', 'data'])
;
;        IDL> nyny = READ_BINARY(f, DATA_DIMS = [768, 512] )
;
;        IDL> ipf = IMAGE_PLOT_F( nyny, FINDGEN(768), FINDGEN(512) )
;
;        IDL> roi = IP_ROI_DRAW( ipf )
;
;     We must wait until after having defined the polygon to continue::
;
;        IDL> p = POLYGON( roi.polygon, /DATA, TARGET=ipf, COLOR='Red', THICK=3, FILL_TRANSPARENCY=70 )
;
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;   
; :HISTORY:
;   04 MAR 2015 (AJAS) Created.
;
;
;
;
;-

  IF KEYWORD_SET( help ) THEN FG_HELP, 'ip_roi_draw'

  ;; Get the data from our image, having checked that it is an image.
  IF ~ ISA( image_object, 'IMAGE' ) THEN MESSAGE, 'You must provide a valid IMAGE!'

  image_object -> getData, iZ, iX, iY

  ;; And then create our own new space to plot it in.
  theImage = IMAGE( iZ, iX, iY, AXIS_STYLE=0, POSITION=[0,0.05,1,1] )
  theDescription = TEXT( 0.5,0.025, 'Left button to mark point; middle to erase previous point; right button to close', ALIGN=0.5,VERTICAL_ALIGN=0.5 )


  ;; The outputted roi is passed as an object that will be populated
  ;; at the end of the window's lifetime.
  output = OBJ_NEW( 'IPRD_OBJ' )


  ;; Our polygon will have a maximum of 200 points. We will pass 
  ;; the roi polygon around with is.
  nmax = 200

  ;; Our line will show the current shape.
  theLine = POLYLINE( [0,0], [0,0], /HIDE, /DATA, $
                      THICK=3, COLOR='Red', TARGET=theImage )
  newLine = POLYLINE( [0,0], [0,0], /HIDE, /DATA, $
                      THICK=2, COLOR='Orange', TARGET=theImage)

  theImage.window.UVALUE={x:FLTARR(nmax), y:FLTARR(nmax), n:0L, $
                          buttonDown:0B, previousPosition:[0.0,0.0], $
                          line:theLine, newLine:newLine, object:output}

  theImage.window.MOUSE_DOWN_HANDLER='iprdMouseDown'
  ;; Currently can't make motion and up handlers work,
  ;; so simplify by not using.
;  theImage.window.MOUSE_MOTION_HANDLER='iprdMouseMotion'
;  theImage.window.MOUSE_UP_HANDLER='iprdMouseUp'
  theImage -> SELECT

  RETURN, output

END























;; Test 
x = (RESTORE_TO_STRUCTURE( '/home/jupiter/eodg/smithan/iasi/ash/2014-02-14-Kelut/flag_20140214_1.sav')).flag_ash
aod = REFORM(x.x[0,*])
lat = x.lat
lon = x.lon

mp1 = MAPPOINTS(  aod, lat, lon, RANGE=[-0.5,2], $
                  LAYOUT=[1,2,1], $
                  DIMENSION=[500, 800], $
                  LIMIT=[-40,80,10,135], $
                  /COLOURBAR, CURRENT=FG_CURRENT(mp1), $
                  COLOUR_BACKGROUND='Light Cyan', COLOUR_LAND='Light green', $
                  COLOUR_TOO_HIGH='Red', COLOUR_TOO_LOW='Dark Grey',$
                  MAP_PROJECTION='Mollweide' )




;; Get the region of interest
ir = ip_roi_draw( mp1 )


;; Now we stop until we have defined the ROI.
PRINT, 'Type .CONTINUE after ROI is defined'
STOP

;; Get all the points within the ROI.
q = WHERE( ir.roi -> ContainsPoints( lon, lat ) )

;; And now plot those on the same scale.
mp2 = MAPPOINTS( aod[q], lat[q], lon[q], RANGE=[-0.5,2], LAYOUT=[1,2,2], $
                 LIMIT=[-40, 80, 10, 135], /COLOURBAR, $
                 COLOUR_BACKGROUND='Light Cyan', COLOUR_LAND='Light green', $
                 COLOUR_TOO_HIGH='Red', COLOUR_TOO_LOW='Dark Grey',$
                 MAP_PROJECTION='Mollweide', CURRENT=1 )





END
