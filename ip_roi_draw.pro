; docformat = 'rst'
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
;+
;
;   An interactive method to generate regions of interest (ROI).
;
; :PROPERTIES:
;
;    x
;     The x locations of the ROI polygon.
;    y
;     The y locations of the ROI polygon.
;    roi
;     An `IDLanROI` object containing the polygon locations.
;    polygon
;     An array containing the vertices of the ROI polygon.
;
; 
;-

PRO iprd_roi::setProperty, x=x, y=y
;+
; Set the properties of the ROI. Once this is done, no changes.
; X and Y must be set simultanaeously. This code should not accessed by the user.
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

PRO iprd_roi::getProperty, x=x, y=y, polygon=poly, roi=roi
;+
; 
; Get the region of interest that has been defined by `ip_roi_draw`. This is
; necessary, because after the graphics window we use to draw our ROI has been
; defined, control is returned to the main routine. As a result, at the time 
; the function returns, we don't yet have the ROI defined.
;
; 
; 
;-
  ON_ERROR, 2
  COMPILE_OPT idl2, hidden

  IF self.done THEN BEGIN
     IF ARG_PRESENT( x ) THEN x= *self.x
     IF ARG_PRESENT( y ) THEN y= *self.y
     IF ARG_PRESENT( poly ) THEN poly = [ [*self.x], [*self.y] ]
     IF ARG_PRESENT( roi ) THEN roi = OBJ_NEW( 'IDLanROI', *self.X, *self.Y )
  ENDIF ELSE MESSAGE,/INFO, 'The ROI has not been set yet!'
END


FUNCTION iprd_roi::_overloadHelp, varname
;+
; Overload help so that we know whether our object is ready for use.
;
; :HIDDEN:
;-
  ON_ERROR, 2
  COMPILE_OPT idl2, hidden

  id = STRTRIM(OBJ_VALID(self,/GET_HEAP_IDENTIFIER),2)
  s0 = 'OBJREF    = <ObjHeapVar'+id+'(IPRD_ROI): '
  s1 = self.done ? 'ready> '+STRTRIM(N_ELEMENTS(*self.x),2)+' vertices' : 'waiting>'

  finalString = STRLEN( varname ) GE 15 ? $
               [varname,STRING(s0+s1,FORMAT='(16X,A)')]  : $
               STRING(varname,s0+s1,FORMAT='(A-15,1X,A)')

  RETURN, finalString
END



PRO iprd_roi::cleanup
;+
; :HIDDEN:
;-
  ON_ERROR, 2
  COMPILE_OPT idl2, hidden
  PTR_FREE, self.x, self.y
END

FUNCTION iprd_roi::init
;+
; No need to do anything. Our object just sits about waiting for
; data from the main code.
;
; :HIDDEN:
;-
  ON_ERROR, 2
  COMPILE_OPT idl2, hidden
  RETURN, 1
END

PRO iprd_roi__define
;+
;  
;    Very simple object to pass back a roi once we have it.
;    If the `done` flag is not set, then we will not get x and y.
;
; :HIDDEN:
;-
  ON_ERROR, 2
  COMPILE_OPT idl2, hidden
  define = { iprd_roi, inherits IDL_OBJECT, x: PTR_NEW(), y: PTR_NEW(), done: 0B }
END












FUNCTION iprdMouseDown, oWin, x, y, iButton, KeyMods, nClicks
;+
; Mouse down code to pass to the image window. This is where the
; bulk of the thinking is done. For left clicks, we check that the
; click is over the image portion of the window.
; 
; Left click - Get the position of the click in image data coords.
;              Add it to the polygon vertices list.
;              Update the overplotted polyline.
;              
; Middle click - Remove the most recent vertice from the polygon.
;                Update the overplotted polyline.
;
; Right click - Close the polygon by making a final vertice that
;               is the same as the 1st vertice.
;
; :HIDDEN:
;
;-
  ON_ERROR, 2
  COMPILE_OPT idl2, hidden

  state = oWin.UVALUE


  ;; Left click = add tick
  IF iButton EQ 1 THEN BEGIN

     ;; Check that the mouse is on top of the image.
     hit = oWin.hitTest(x,y)

     ;; Select the image object.
     yep = BYTARR(N_ELEMENTS(hit))
     FOREACH h, hit,i DO yep[i] = ISA( h, 'IMAGE' )
     qImage = (WHERE( yep ))[0]
     ;; If we don't have an image object to play with then return.
     IF qImage GE 0 THEN im = hit[qImage] ELSE RETURN, 0



     ;; Check that we're not at the maximum.
     IF state.n GT N_ELEMENTS(state.x)-2 THEN MESSAGE, 'Too many points for the region.'

     ;; And get the location in the image.
     v = im.getValueAtLocation(x,y,/INTERPOLATE,/DEVICE)
        
     ;; If there's a map-projection, we need to move from proj metres to lat/lon.
     mp = im.MAPPROJECTION
     IF ISA(mp, 'MAPPROJECTION') THEN v = mp -> mapInverse( v[0:1] )

     state.x[ state.n ] = v[0]
     state.y[ state.n ] = v[1]

     ;; Increment the counter.
     state.n ++

     ;; If we're at the 1st point, stop hiding and define two
     ;; data points.
     IF state.n EQ 1 THEN BEGIN
        state.line.hide = 0
        state.line -> setData, state.x[[0,0]], state.y[[0,0]]
     ENDIF ELSE BEGIN
        state.line -> setData, state.x[0:state.n-1], state.y[0:state.n-1]
     ENDELSE

     ;; Middle button = remove last tick
  ENDIF ELSE IF iButton EQ 2 THEN BEGIN
     state.n --
     CASE state.n OF 
        -1:   state.n = 0
        0:    state.line.hide = 1
        1:    state.line -> setData, state.x[ [0,0] ],   state.y[ [0,0] ]
        ELSE: state.line -> setData, state.x[0:state.n-1], state.y[0:state.n-1]
     ENDCASE

     ;; Right button = finish by adding new tick = first tick.
  ENDIF ELSE IF iButton EQ 4 THEN BEGIN
     
     IF state.n LT 3 THEN BEGIN
        ;; Warn by leaving message across the screen for a second.
        oText = TEXT( /NORMAL, 0.5, 0.5, 'Minimum 3 points required!', $
                      ALIGN=0.5, VERTICAL_ALIGN=0.5, COLOR='RED',$
                      FILL_BACKGROUND=1, FILL_COLOR='White', FONT_SIZE=30 )
        WAIT, 0.8
        oText -> DELETE
     ENDIF ELSE BEGIN
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
        PRINT, 'ROI variable is ready to use.'
        RETURN, 0
     ENDELSE
  ENDIF

  ;; Save the new state.
  oWin.UVALUE = state

  RETURN, 0 ;; Skip default event handling


END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 FUNCTION ip_roi_draw, image_object, CONTINENTS=continents, MAP=onMap, $
                       HELP=help
;+
;
;  Open a dialogue window to select a region of interest for an image
;  file defined by `IMAGE_PLOT_F`. The returned variable will be an
;  object that becomes populated with a polygon once we have finished
;  defining the ROI.
;
;
; :Categories:
;    Function graphics, image_plot

;  :Returns:
;     An `iprd_roi` object that will contain the ROI polygons once
;      they have been defined.
;
;  :Params:
;     image_object: in, required, type="IMAGE object reference"
;      An image to define an ROI on.
;
;  :Keywords:
;     MAP: in, optional, type=boolean
;      Use the image's map projection (if it exists) to warp the image
;       we are defining the ROI in.
;     CONTINENTS: in, optional, type=boolean
;      If the `MAP` keyword is set, also put continents over the top.
;     HELP: in, optional, hidden, type=boolean
;
;  :Examples:
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
;     .. image:: example_images/ip_roi_ny.png
;
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;   
; :HISTORY:
;
;   04 MAR 2015 (AJAS) Created.
;
;   05 MAR 2015 (AJAS) Added support for map projections.
;
;
;-
  ON_ERROR, 2
  COMPILE_OPT idl2

  IF KEYWORD_SET( help ) THEN FG_HELP, 'ip_roi_draw'

  ;; Get the data from our image, having checked that it is an image.
  IF ~ ISA( image_object, 'IMAGE' ) THEN MESSAGE, 'You must provide a valid IMAGE!'
  image_object -> getData, iZ, iX, iY

  ;; Fill as much of the window as is possible.
  position = [0,0.04,1,1]

  ;; And then create our own new space to plot it in.
  IF KEYWORD_SET( onMap ) THEN BEGIN
     mp = image_object.MapProjection
     IF ~KEYWORD_SET( mp ) THEN MESSAGE, 'No map projection available!'
     oImage = IMAGE( iZ, iX, iY, AXIS_STYLE=0, POSITION=position, $
                     MAP_PROJECTION=mp.map_projection, GRID_UNITS=2, $
                     CENTER_LATITUDE=mp.center_latitude, $
                     CENTER_LONGITUDE=mp.center_longitude )
     IF KEYWORD_SET( continents ) THEN continents=MAPCONTINENTS()
     oImage.mapgrid.hide = 1
  ENDIF ELSE $
     oImage = IMAGE( iZ, iX, iY, AXIS_STYLE=0, POSITION=position )

  ;; Leave a helpful message!
  oDescription = TEXT( 0.5,0.02, FONT_SIZE=8, ALIGN=0.5,VERTICAL_ALIGN=0.5, $
        'Left button to mark point; middle to erase previous point; right button to close; scroll-wheel to zoom'  )


  ;; The outputted roi is passed as an object that will be populated
  ;; at the end of the window's lifetime.
  output = OBJ_NEW( 'IPRD_ROI' )


  ;; Our polygon will have a maximum of 1000 points. We will pass 
  ;; the roi polygon around with us.
  nmax = 1000

  ;; Our line will show the current shape.
  theLine = POLYLINE( [0,0], [0,0], /HIDE, /DATA, $
                      THICK=3, COLOR='Red', TARGET=oImage )

  ;; Use the user value to pass the polyline and the vertices back and forth.
  oImage.window.UVALUE={x:FLTARR(nmax), y:FLTARR(nmax), n:0L, $
                        line:theLine, object:output}

  ;; Define a mouse handler for clicks.
  oImage.window.MOUSE_DOWN_HANDLER='iprdMouseDown'

  ;; Select the image so that we can zoom in and out by scrolling.
  oImage -> SELECT

  RETURN, output

END
