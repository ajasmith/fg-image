; docformat = 'rst'
;
; :NAME:
;   mappoints
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Routines to preserve the input device graphics state and return to
 ; this after we are done (or if there's an error).
 FUNCTION mpf_plot_begin_save
;+
;
; Preserve the input device graphics state and so that we can return to 
; this after we are done.
;
; :RETURNS:
;    A structure containing the `!X`, `!Y`, `!P`, device name, and current
;    colour table::
;
;        IDL> HELP, mpf_plot_begin_save()
;        ** Structure <89a3f48>, 5 tags, length=4912, data length=4880, refs=1:
;        X               STRUCT    -> !AXIS Array[1]
;        Y               STRUCT    -> !AXIS Array[1]
;        P               STRUCT    -> !PLT Array[1]
;        MY_DEVICE       STRING    'X'
;        CT              BYTE      Array[256, 3]
; 
;
; :HIDDEN:
;
; 
;-
   COMPILE_OPT hidden, idl2
   ON_ERROR, 2
   ;; Save the current device colours
   TVLCT, ct, /GET
   ;; And the !X, !Y, !P and device.
   RETURN, {x:!x,y:!y,p:!p,my_device:!D.NAME,ct:ct}
 END


 PRO mpf_plot_reset, deviceGraphicStruct
;+
;
; Used to reset the device graphics to it's prior state (minus a wiped
;  z-buffer. It takes as input the structure created by 
;  `mpf_plot_begin_save`
;
; :PARAMS:
;    deviceGraphicStruct: in, type=structure
;
; :HIDDEN:
;-
   COMPILE_OPT hidden, idl2
   ;; Back to the original device.
   SET_PLOT, deviceGraphicStruct.my_device
   ;; Put the !X, !Y, and !P back how they were.
   !P = deviceGraphicStruct.p
   !X = deviceGraphicStruct.x
   !Y = deviceGraphicStruct.y
   ;; Put the old colour table back.
   TVLCT, deviceGraphicStruct.ct
 END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 PRO mpf_point, value, lat, lon, radius, SHAPE=shape
;+
;  Draws a single point in the z-buffer using POLYFILL.
;
; :PARAMS:
;    value: in, type=byte
;      A byte value representing a colour index.
;    lat: in, type=float
;      A latitude for the data point.
;    lon: in, type=float
;      A longitude for the data point.
;    radius: in, optional, type=float, default=120.0
;      The radius of the data point (in km)
;
; :KEYWORDS:
;    shape: in, optional, type='string', default='circle'
;      The shape of the plotted point.
;    
; :HIDDEN:
;-
   COMPILE_OPT hidden, idl2

   IF ~KEYWORD_SET( radius ) THEN radius = 120D0
   ;; Shape will be a circle, with radius in km, assuming spherical
   ;; earth. Default is IASI at nadir * 10.

   radius_earth = 6371D0
   radius_lat = radius / radius_earth * 2D0 * !DPI
   radius_lon = radius_lat / COS( lat * !DTOR )



   IF SIZE( /TYPE, shape ) EQ 7 THEN BEGIN
      CASE STRMID(STRUPCASE( shape[0] ),4 )  OF
         'CIRCLE': ishape=0
         'SQUARE': ishape=1
         'RECTANGLE': ishape=2
         ELSE: ishape=0
      ENDCASE
   ENDIF ELSE ishape=0

   IF ishape EQ 0 THEN BEGIN
      t_circle = DINDGEN( 33 ) *0.0625 * !DPI
      poly_lat = radius_lat * COS( t_circle ) + lat
      poly_lon = radius_lon * SIN( t_circle ) + lon
   ENDIF
   IF ishape EQ 1 THEN BEGIN
      poly_lat = lat + [-1,1,1,-1,-1] * radius_lat * 0.5
      poly_lon = lon + [-1,-1,1,1,-1] * radius_lon * 0.5
   ENDIF
   IF ishape GT 1 THEN MESSAGE,'Shape '+shape+' not implemented'


   POLYFILL, poly_lon, poly_lat, COLOR=value
 END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 FUNCTION MAPPOINTS, pts, lat, lon, $
                     ;; Data keywords
                     LOG=log, $
                     RANGE=range, $
                     RADIUS=radius, $
                     SQUARE=square, $
                     DPCM=dpcm, $
                     ;; Colour bar keywords
                     SIDEBAR=sidebar, $
                     COLOURBAR=colourbar, $
                     CBAR_VALUES=cbar_values, $
                     CBAR_LABELS=cbar_labels, $
                     HIDE_COLOURBAR=hide_colourbar, $
                     ;; Mapping keywords
                     COLOUR_BACKGROUND=colour_background, $
                     COLOUR_LAND=colour_land, $
                     CENTER_LATITUDE=clat_in, $
                     CENTER_LONGITUDE=clon_in, $
                     HIRES=hires, $
                     LIMIT=limit_in, $
                     NOCONTINENTS=nocontinents, $
                     HIDE_LL=hide_ll, $
                     ;; Window keywords.
                     BUFFER=buffer, $
                     CURRENT=current, $
                     DIMENSIONS=dimensions, $
                     LAYOUT=layout, $
                     WINDOW_TITLE=window_title, $
                     ;; Everything else.
                     OBJECTS=object, $
                     HELP=help, $
                     DEBUG=debug, $
                     STOP=stop, $
                     TEST=test, $
                     _EXTRA=extra
;+
;
;     .. image:: example_images/mappoints.png
;
;
;
;    Created a MAPPOINTS style plot using function graphics. Specifically,
;     an image is created in the z-buffer, and then projected into map
;     coordinates as an image using `IMAGE_PLOT_F`. The majority of
;     `IMAGE_PLOT_F` keywords are applicable, along with the following
;     options.
;
; :Categories:
;    Function graphics, mappoints
;
;
; :Returns:
;    An `IMAGE` object.
;
;
; :PARAMS:
;
;    pts: in, required, type=numeric
;        The data field to be plotted.
;    lat: in, required, type=float
;        The latitude of each data point.
;    lon: in, required, type=float
;        The longitude of each data point.
;
;
;   pts, lat, and lon must have the same number of elements
;   and must all be specified.
;
;
;
; :KEYWORDS:
;    LOG: in, optional, type=boolean
;      Represent the data on a logarithmic scale.
;    RANGE: in, optional
;           A 2-element array giving the data range to be plotted. 
;    RADIUS: in, optional, type=numeric, default=120
;      The radius of each pixel point in km (or width if the `SQUARE` 
;       keyword is set).
;    SQUARE: in, optional, type=boolean, default=0
;      Make each point a square instead of a circle.
;    DPCM: in, optional, type=float, default=2
;      Dots per cm. The resolution of the output image.
;
;
;  Colourbar and colour options
;  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;    SIDEBAR: in, optional, type=boolean, default=0
;        Include a colourbar to the left of the image.
;    COLOURBAR: in, optional, type=boolean, default=0
;        Include a colourbar below the image.
;    HIDE_COLOURBAR: in, optional, type=boolean, default=0
;        Hide the colourbar, even though a space is left for it.
;    CBAR_VALUES: in, optional, type=fltarr()
;      Values on the colourbar at which to add ticks.
;    CBAR_LABELS: in, optional, type=strarr()
;      Labels for the colourbar. Only sensible when also defining
;      `CBAR_VALUES`. It must have the same number of elements.
;    COLOUR_BACKGROUND: in, optional, type=string/bytarr(3), default='white'
;      A colour for areas with no data.
;    COLOUR_LAND: in, optional, type=string/bytarr(3), default='transparent'
;      A colour for areas with no data which are land.
;
;
;  Map options
;  ~~~~~~~~~~~
; 
;   These keywords work identically to the same keywords in the `MAP` function.
;    CENTER_LATITUDE: in, optional, type=numeric
;    CENTER_LONGITUDE:in, optional, type=numeric
;    HIRES: in, optional, type=boolean, default=0
;    LIMIT: in, optional, type=fltarr(4)
;      A 4-element array containing [lat_min,lon_min,lat_max,lon_max].
;    NOCONTINENTS: in, optional, type=boolean, default=0
;      Don't draw the outline of continents on the plot.
;    HIDE_LL: in, optional, type=boolean, default=0
;      Hide the latitude and longitude labels.
;
;  Window Options
;  ~~~~~~~~~~~~~~
;  
;   These keywords work identically to the same keywords in the 
;   `WINDOW` function.
;
;
;    BUFFER: in, optional, type=boolean
;
;    CURRENT: in, optional, type=boolean
;
;    DIMENSIONS: in, optional
;
;    LAYOUT: in, optional
;
;    WINDOW_TITLE: in, optional
;
;  Miscellaneous options
;  ~~~~~~~~~~~~~~~~~~~~~
;    
;    OBJECTS: out, optional, type=objref
;      A structure containing objects created by the routine.
;      Useful for subsequent plot modification.
;    HELP: in, optional, type=boolean
;          Load the help page for this routine.
;
;    TEST: in, optional, type=boolean
;      An example plot.
;    DEBUG: in, optional, type=boolean
;      Don't leave the routine on error.
;    STOP: in, optional, type=boolean
;      Stop immediately before returning from the routine.
;    _EXTRA:
;      Additional keywords to pass to `IMAGE_PLOT_F`. Most things should work 
;       sensibly.
;
;
; :USES:
;    `IMAGE_PLOT_F`
;    `HECKBERT`
;
;
; :POST:
;    The z-buffer will be wiped.
;
;
;
; :BUGS:
;
;    * The background colour extends beyond the outside of the map
;      for funky shaped projections (see example figure above).
;
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;
; :HISTORY:
;
;    04 Mar 2014 (AJAS) Created.
;
;    02 Sep 2014 (AJAS) Documented and added to EODG library.
;
;    10 Oct 2014 (AJAS) `COLOUR_LAND` AND `TEST` keywords added.
;
;    14 Oct 2014 (AJAS) Fixed bug when using `CBAR_VALUES` keyword.
;
;    20 Nov 2014 (AJAS) Modified land colour so that clon is no longer
;         passed to map_proj_init unless defined by the user.
;
;    11 Dec 2014 (AJAS) Added `LAYOUT` keyword explicitly to reduce overly
;         large images generated for many plots in the same window.
;         Reduced default image resolution.
;
;    12 Dec 2014 (AJAS) Check that range is finite before plotting: otherwise
;         we get stuck in an infinite loop inside `HECKBERT` function.
;         Disabled refreshing until the very end. 
;
;    06 Jan 2015 (AJAS) Modified colour usage: missing colour is now dealt
;         with by being transparent. As a result, it is actually the back-
;         ground colour defined in `IMAGE_PLOT_F`. Similarly, this makes the
;         `COLOUR_LAND` keyword set the mapcontinents object's colour.
;
;    28 Jan 2015 (AJAS) Found work-around for crossing the International date
;         line.
;
;    05 Feb 2015 (AJAS) `WINDOW_TITLE` keyword now works.
;
;    25 Feb 2015 (AJAS) Fixed bug with `HIDE_COLOURBAR` keyword.
;
; :REQUIRES:
;    8.3
; 
;-

   ON_ERROR, KEYWORD_SET( debug ) ? 0 : 2
   COMPILE_OPT idl2

   ;; Make sure we're using at least IDL 8.3.
   IF FLOAT(!VERSION.release) LT 8.3 THEN MESSAGE,'Requires IDL 8.3 or higher'

   ;; Load help if required.
   IF KEYWORD_SET(help) THEN BEGIN
      FG_HELP, 'mappoints'
      RETURN, 0
   ENDIF


   ;; Data for the test keyword.
   IF KEYWORD_SET( test ) THEN BEGIN
      pts = (FINDGEN(33)-1)/3.0
      lat = REPLICATE(50.0,33)
      lon = FINDGEN(33)*0.4-4
      IF ~KEYWORD_SET(range) THEN range=[0,10]
      IF ~KEYWORD_SET(limit_in) THEN limit_in=[40,-5,60,10]
      IF ~KEYWORD_SET(colour_background) THEN colour_background='Light Blue'
      IF ~KEYWORD_SET(colour_land) THEN colour_land='Pale Green'
      IF ~KEYWORD_SET(hires) THEN hires=1B
      IF ~KEYWORD_SET(colourbar) THEN colourbar=1B
      IF ~KEYWORD_SET(dimensions) THEN dimensions=[640,770]
   ENDIF
      
   ;; Check for input errors.

   ;; Test that pts, lat, and lon have the same number of elements.
   npts = N_ELEMENTS( pts )
   IF N_ELEMENTS( lat ) NE npts || N_ELEMENTS( lon ) NE npts THEN $
      MESSAGE, 'Data, lat and lon must have the same number of elements.'




   ;; Make a plot in the z-buffer, and then pass it to IMAGE_PLOT_F, 
   ;; where it will be warped onto the correct map grid.
   deviceGraphicStruct = MPF_PLOT_BEGIN_SAVE()


   ;; Error handler.
   CATCH, deviceError
   IF deviceError NE 0 THEN BEGIN
      CATCH, /CANCEL
      ;; Get out of the !Z buffer, and put plotting back how it was.
      MPF_PLOT_RESET, deviceGraphicStruct
      ;; And get the error message.
      HELP, /LAST_MESSAGE, OUTPUT=deviceErrMessage
      MESSAGE, /INFO, 'Plotting error caught'
      FOR i=0,N_ELEMENTS(deviceErrMessage)-1 DO PRINT,deviceErrMessage[i]
      RETURN, {ERROR:deviceErrMessage}
   ENDIF

   ; Have a maximum of 250 colours.
   ncolours = KEYWORD_SET(ncolours) ? (0 > LONG(ncolours) < 250) : 30l

   ;---------------------------------------------------------------------
   ;---------------------------------------------------------------------
   ;; Set up the range.
   ;---------------------------------------------------------------------
   CASE N_ELEMENTS( range ) OF
      0: BEGIN
         range_ = KEYWORD_SET( log ) ? [MIN(ALOG10(pts),MAX=mmm,/NAN), mmm] : $
                  [MIN(pts,MAX=mmm,/NAN), mmm]
         
         ;; If there is no range set, then we don't need a taper.
         IF ~KEYWORD_SET(hide_taper) THEN hide_taper=1b
      ENDCASE
      2: range_ = KEYWORD_SET( log ) ? ALOG10( range ) : range
      ELSE: MESSAGE,'Range must be a 2 element array'
   ENDCASE
   ;; Add a 0.1% cushion on either end of the range.
   range_ += [-1,1]*0.001*( range_[1] - range_[0] )

   ;; Check that our range is finite. Otherwise, issues with
   ;; Heckbert tick positions later...
   IF ~FINITE(range_[0]) || ~FINITE(range_[1]) THEN $
      MESSAGE,'Range is not finite.'+ (KEYWORD_SET(log) ? $
              ' Did you take the log of 0 or a negative number?' : '')


   ;; Limits of the plotting.
   IF N_ELEMENTS( limit_in ) EQ 4 THEN $
         limit = limit_in ELSE BEGIN
      
      ;; If we are only near the international date line, we have to
      ;; be a bit tricky.
         IF MIN( ABS(lon) ) GE 130 && MIN( lon ) LT 0 THEN BEGIN
            qmaxlon = WHERE( lon LT 0, COMPLEMENT=qminlon )
            min_lon = MIN( lon[qminlon] ) ;; Smallest longitude greater than 0
            max_lon = MAX( lon[qmaxlon] ) ;; Largest longitude less than 0
            limit = [MIN(lat,MAX=max_lat),min_lon, max_lat, max_lon]
         ENDIF ELSE BEGIN
            limit = [MIN(lat,MAX=max_lat),MIN(lon,MAX=max_lon),max_lat, max_lon]
         ENDELSE
         
         ;; Add a buffer of 0.5 degrees because otherwise, the points right 
         ;; on the edge will get obscured by the box axes.
         limit += ( [-1,-1,1,1] * 0.5 > [-90,-180,-90,-180] ) < [90,180,90,180]
   ENDELSE



   ;; Set up our window.
   win = WINDOW( CURRENT=current, DIMENSIONS=dimensions, $
                 BUFFER=buffer, $
                 WINDOW_TITLE=KEYWORD_SET(window_title) ? $
                              window_title : 'Mappoints'  )

   
   ;; Disable refreshing to save rendering effort before the final output.
   win.REFRESH, /DISABLE

   ;; Move into the Z buffer with correct dimensions.
   SET_PLOT, 'Z'

   window_dim = win.DIMENSIONS
   IF ~KEYWORD_SET( dpcm ) THEN dpcm=2.0
   ;; Shrink the window dimensions if there is more than
   ;; one plot in the space.
   IF KEYWORD_SET( layout ) THEN BEGIN
      IF layout[0] GT 1 THEN window_dim[0] /= layout[0]
      IF layout[1] GT 1 THEN window_dim[1] /= layout[1]
   ENDIF
      

   ;; And use colourtable 0.
   LOADCT,0, /SILENT

   ;; Indices for low, high and missing values.
   i_too_low = 0l
   i_too_high= ncolours+2
   i_missing = 253


   ;; We plot in greyscale so the value of each pixel will 
   ;; be scaled by IMAGE_PLOT_F to get colours, not by us.
   DEVICE, SET_RES=window_dim*dpcm, Z_BUFFER=0,SET_PIXEL_DEPTH=8
   !P.BACKGROUND = i_missing
   ERASE


   xrange = [ limit[1], limit[3] ]
   yrange = [ limit[0], limit[2] ]

   lon360 = lon
   ;; Check for a limit crossing the international date line.
   IF xrange[1] LT xrange[0] THEN BEGIN
      crossIDL=1B
      xrange[1] += 360
      lon360[WHERE( lon LT xrange[0], /NULL )] += 360
   ENDIF ELSE crossIDL=0B




   ;; Centre latitude and longitude.
   clat = KEYWORD_SET( clat_in ) ? clat_in : 0.5*TOTAL(yrange )
   clon = KEYWORD_SET( clon_in ) ? clon_in : 0.5*TOTAL(xrange )

   
   ;; Now make a plot with no markings
   PLOT, xrange, yrange, XSTYLE=5, YSTYLE=5, /NODATA, $
         XMARGIN=[0,0], YMARGIN=[0,0]



   ;; We only need the points that will be within the ranges.
   ok = WHERE( lat    GE yrange[0] AND $
               lat    LE yrange[1] AND $
               lon360 GE xrange[0] AND $
               lon360 LE xrange[1], nok )



   ;; Conversion factor from data value to colour index.
   ct_convert = FLOAT( ncolours ) / FLOAT(range_[1] - range_[0])

   ;; Use array operations to scale all elements to colour indices.
   points = KEYWORD_SET( log ) ? ALOG10( pts ) : pts
   ;; The main index runs from 1 -> ncolours+1.
   index = 0l > ( FLOOR(  (points-range_[0]) * ct_convert )+1.5 ) < (ncolours+2)


   IF KEYWORD_SET( test ) THEN BEGIN
      PRINT, points, FORMAT='(40F6.2)'
      PRINT, index,  FORMAT='(40F6.2)'
   ENDIF


   ;; Loop over the valid points.
   IF nok GT 0 THEN BEGIN
      FOR iok=0,nok-1 DO BEGIN
         j = ok[ iok ]
         MPF_POINT, index[j], lat[j], lon360[j], radius, $
                    SHAPE=KEYWORD_SET(square) ? 'Square' : 'Circle'
      ENDFOR
      
   ENDIF

   ;; Grab the 8-bit data from the z-buffer.
   our_data = TVRD( )

   ;; Now that we're done, it's time to reset everything.
   MPF_PLOT_RESET, deviceGraphicStruct 


   ;; Dimensions of the image.
   nx = N_ELEMENTS( our_data[*,0] )
   ny = N_ELEMENTS( our_data[*,1] )

   ;; Converted into a lat-lon grid.
   lon_grid = DINDGEN(nx) * (xrange[1]-xrange[0])/ DOUBLE(nx-1) + xrange[0]
   lat_grid = DINDGEN(ny) * (yrange[1]-yrange[0])/ DOUBLE(ny-1) + yrange[0]


   missing_colour = IP_GET_BYTE_COLOUR( KEYWORD_SET(colour_background) ? $
                                        colour_background : 'White' )
   


   ;; Finally pass the image to IMAGE_PLOT_F.
   ip = IMAGE_PLOT_F( our_data, lon_grid, lat_grid, $
                      CURRENT=1, $
                      SIDE=sidebar, COLOURBAR=colourbar, $
                      HIDE_COLOURBAR=KEYWORD_SET(hide_colourbar), $
                      RANGE=[1,ncolours],MISSING=i_missing, $
                      /TRANSPARENT_MISSING, $
                      CENTER_LATITUDE=clat, $
                      CENTER_LONGITUDE=clon, $
                      HIRES=KEYWORD_SET(hires), $
                      CONTINENTS=~KEYWORD_SET(NOCONTINENTS), $
                      LAYOUT=layout, /DONT_REFRESH, $
                      OBJECTS=object, _STRICT_EXTRA=extra)


   ;; If the background is black, do the continents in white.
   IF TOTAL( missing_colour, /INTEGER ) EQ 0 THEN BEGIN
      object.continents.color = IP_GET_BYTE_COLOUR( 'White' )
      object.mapgrid.color = IP_GET_BYTE_COLOUR( 'White' )
      object.mapgrid.box_color = IP_GET_BYTE_COLOUR( 'Black' )
   ;; Otherwise colour in the background now.
   ENDIF ELSE object.image_plot.background_color=missing_colour

   ;; If we want to colour in the land, do that here:
   IF KEYWORD_SET( colour_land ) THEN BEGIN
      object.continents.fill_color = IP_GET_BYTE_COLOUR( colour_land )
      object.continents.fill_background = 1B
      object.continents -> ORDER, /SEND_TO_BACK
   ENDIF
   

   ;; Use box axes.
   object.mapgrid.box_axes=1
   object.mapgrid.label_position=1
   object.mapgrid.linestyle=1
   object.mapgrid.transparency=40
   object.mapgrid.label_color=IP_GET_BYTE_COLOUR('Black')
   FOREACH l, object.mapgrid.longitudes DO l.LABEL_ANGLE=0
   FOREACH l, object.mapgrid.latitudes  DO l.LABEL_ANGLE=270

   IF KEYWORD_SET( hide_ll ) THEN object.mapgrid.label_show = 0

   ;; Do the colourbar after the fact by changing the integer 
   ;; scaling sent to IMAGE_PLOT_F into the actual values.
   cbar_yes = 0b
   IF KEYWORD_SET( sidebar ) THEN BEGIN
      cbar_i = 1
      cbar_yes = 1b
   ENDIF ELSE IF KEYWORD_SET( colourbar ) THEN BEGIN
      cbar_i = 0 
      cbar_yes = 1b
   ENDIF
   IF KEYWORD_SET( hide_colourbar ) THEN cbar_yes = 0b



   IF cbar_yes THEN BEGIN

      IF N_ELEMENTS( cbar_values ) GT 0 THEN BEGIN
         nice_range= KEYWORD_SET(log) ? ALOG10(cbar_values) : cbar_values
         nice_int = (nice_range-range_[0])/(range_[1]-range_[0])*(ncolours-1)+1
      ENDIF ELSE BEGIN
         ;; Pick a sensible set of values for the colour bar axes.
         nq=0
         nn=5
         WHILE nq LT 2 DO BEGIN
            nice_range = HECKBERT( range_,nn )
            nice_int = (nice_range-range_[0])/(range_[1]-range_[0])*(ncolours-1)+1
            q = WHERE( nice_int LE ncolours AND nice_int GE 1, nq )
            nice_int   = nice_int[ q ] 
            nice_range = nice_range[ q ]
            nn++
         ENDWHILE

      ENDELSE

      IF KEYWORD_SET(log) THEN nice_range=10.0^nice_range

      ;; Labels for the colourbar.
      IF KEYWORD_SET( cbar_labels ) THEN BEGIN
         IF N_ELEMENTS(cbar_labels) NE N_ELEMENTS(nice_int) THEN MESSAGE, $
            "Number of cbar labels doesn't match numbe of values in colour bar."
         label = STRING( cbar_labels )
      ENDIF ELSE BEGIN
         ;; Get the number of decimal places.
         dp = FLOOR( ALOG10( nice_range[1]-nice_range[0] ) )
         format = (dp LT 0) ? '(F0.'+STRTRIM(-dp,2)+')' : '(I0)'
         label = STRING(nice_range,FORMAT=format)
      ENDELSE

      cbar_ax = object.colour_bar.axes
      cbar_ax[cbar_i  ].tickvalues = nice_int
      cbar_ax[cbar_i+2].tickvalues = nice_int
      cbar_ax[cbar_i  ].tickname   = label

   ENDIF

   ;; Now that all modifications have been carried out, refresh the window.
   win.REFRESH

   IF KEYWORD_SET( stop ) THEN STOP

   ;; Return the image object.
   RETURN, ip

 END
   
