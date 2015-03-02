; docformat = 'rst'
;
; :NAME:
;   image_plot_f
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
FUNCTION IMAGE_PLOT_F, z, x, y, $
                        ;; BASIC AXIS STUFF
                        LOG = log, RANGE= range, TITLE= title, $
                        XLOG=xlog, XTITLE=xtitle, $
                        YLOG=ylog, YTITLE=ytitle, $
                        FONT_NAME=font_name, $
                        FONT_STYLE=font_style, $
                        FONT_SIZE=font_size, $
                        ;; HATCH THIS support
                        HATCH_THIS=hatch_this, $
                        ;; HANDLING COLOURS
                        MISSING=missing, COLOURTABLE=colourtable,$
                        REVERSE_COLOURS=reverse_colours, $
                        COLOUR_TOO_LOW=ctl, $
                        COLOUR_TOO_HIGH=cth,$
                        COLOUR_MISSING=cm, $
                        TRANSPARENT_MISSING=transparent_missing, $
                        NCOLOURS=ncolours, CBAR_TITLE=cbar_title,$
                        SIDEBAR=sidebar, COLOURBAR=colourbar, $
                        HIDE_TAPER=hide_taper, $
                        HIDE_COLOURBAR=hide_cbar, $
                        SHOW_KEY_MISSING=show_key_missing, $
                        BWRDIFF=bwrdiff, BWR2DIFF=bwr2diff, $
                        BGRDIFF=bgrdiff, GREEN=green, RED=red, BLUE=blue,$
                        ;; MAP PROJECTION STUFF
                        CONTINENTS=continents, HIRES=hires, $
                        MAP_PROJECTION=map_projection, $
                        CENTER_LATITUDE=clat, $
                        CENTER_LONGITUDE=clon, $
                        LIMIT=limit, $
                        ;; WINDOW STUFF
                        PLOT_POSITION=plot_position_in, $
                        CBAR_POSITION=cbar_position_in, $
                        DONT_REFRESH=dont_refresh, $
                        BUFFER=buffer, $
                        CURRENT=current, $
                        DIMENSIONS=dimensions, $
                        LAYOUT=layout, $
                        NAME=name, $
                        WINDOW_TITLE=window_title, $
                        ;; RELEVANT OBJECTS e.g. map, colourbar, etc.
                        OBJECTS=objects, $
                        HELP=help, $
                        ;; Example call
                        TEST=test, $
                        ;; Debugging
                        MAP_BUG_TEST=map_bug_test, $
                        DEBUG=debug, STOP=stop;, IMAGE=im


;+
;
;
;    .. image:: image_plot_f.png
;
;    Recreates the functionality of `IMAGE_PLOT` using function
;    graphics (using the IDL function `IMAGE` instead of Coyote's
;     `TVSCALE`). This routine is only suitable for IDL versions 8.2 
;    and above.
;
;
; :Categories:
;    Function graphics, image_plot
;
;
;
; :Returns:
;    An `IMAGE` object.
;
;
;
; :Params:
;    z: in, required, type=numeric
;      Either a 2D data field, or a 3D image (with a dimension of
;      size 3). If more than one dimension has size 3, then the
;      final dimension of size 3 is the RGB dimension.
;
;
;    x: in, optional, type=numeric 
;    y: in, optional, type=numeric 
;         Regular x and y values corresponding to the centre of
;         each data field in z. If x and y values are not on a
;         regular grid, then an error will be thrown. For
;         irregular values, re-label the axes after plotting.
;
;
; :Keywords:
;
;
;    RANGE: in, optional
;          The range of values of z.
;    LOG: in, optional, type=boolean, default=0
;       Plot the z data on a logarithmic scale (2D only).
;    TITLE: in, optional, type=string, default=''
;       The title of the plot.
;    XTITLE: in, optional, type=string, default=''
;       The title of the x-axis
;    XLOG: in, optional, type=boolean, default=0
;       Is the x-axis on a logarithmic scale?
;    YTITLE: in, optional, type=string, default=''
;       The title of the y-axis
;    YLOG: in, optional, type=boolean, default=0
;       Is the y-axis on a logarithmic scale?
;    FONT_NAME: in, optional, type=string
;       e.g. 'Times', 'Helvetica', 'Courier'.
;    FONT_STYLE:in, optional, type=string
;       e.g. 'Bold', 'Italic', 'Normal'.
;    FONT_SIZE:in, optional, type=float
;        e.g. 5, 11, 14.
;    HATCH_THIS: in, optional, type=long
;       An array of image element indices to hatch.
;
;
; 
;   Colourbar Keywords (2D only)
;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;
;    MISSING: in, optional, type=float, default=NaN
;        A value that indicates missing data. (NaNs are
;        automatically assumed missing values).
;    TRANSPARENT_MISSING: in, optional, type=boolean, default=0
;        Makes missing data transparent. This overides the 
;        COLOUR_MISSING keyword.
;    NCOLOURS: in, optional, type=integer, default=30
;        The number of colours used in the scale.
;    COLOURTABLE: in, optional, type="integer/strarr(N)/bytarr(N,3)"
;        Either a single colourtable number, an [N,3]
;        array of colours (which overrules NCOLOURS), 
;        or an N-length array of string names (again
;        overuling NCOLOURS).
;    COLOURBAR: in, optional, type=boolean, default=0
;        Include a colourbar under the image.
;    REVERSE_COLOURS: in, optional, type=boolean, default=0
;        Reverse the order of colours in the table.
;    SIDEBAR: in, optional, type=boolean, default=0
;        Include a colourbar to the left of the image.
;    CBAR_TITLE: in, optional, type=string
;        Include a title on the colourbar.
;    HIDE_TAPER: in, optional, type=boolean, default=0
;        Hide the tapers at the top and bottom of a colourbar
;        which show the too-high and too-low colours.
;    HIDE_COLOURBAR: in, optional, type=boolean, default=0
;        Hide the colourbar, but leave the main 
;        plot as if the colourbar was set.
;    SHOW_KEY_MISSING: in, optional, type=boolean/string, default=0
;        Add a key next to the colour bar showing
;        the colour for missing data. This keyword
;        has no effect if there is no colour bar.
;        If a string is passed, this becomes the description.
;
; 
;    COLOUR_TOO_LOW: in, optional, type="string/bytarr(3)"
;        The colour used for z data below range[0].
;        Can be defined by a string or an RGB 3-byte array.
;    COLOUR_TOO_HIGH: in, optional, type="string/bytarr(3)"
;        The colour used for z data above range[1].
;        Can be defined by a string or an RGB 3-byte array.
;    COLOUR_MISSING: in, optional, type="string/bytarr(3)"
;        The colour used for missing z data.
;        Can be defined by a string or an RGB 3-byte array.
;
;
;    BWRDIFF: in, optional, type=boolean
;       A blue -> white -> red colourbar (for differences).
;        Overides COLOURTABLE keyword.
;    BWR2DIFF: in, optional, type=boolean
;       An alternate blue -> white -> red colourbar.
;        Overides COLOURTABLE keyword.
;    BGRDIFF: in, optional, type=boolean
;       Blue -> grey -> red colourbar.
;        Overides COLOURTABLE keyword.
;    RED: in, optional, type=boolean
;       White -> red -> black colourbar.
;        Overides COLOURTABLE keyword.
;    GREEN: in, optional, type=boolean
;       White -> green -> black colourbar.
;        Overides COLOURTABLE keyword.
;    BLUE: in, optional, type=boolean
;       White -> blue -> black colourbar.
;        Overides COLOURTABLE keyword.
;      
;
;
;   Mapping Keywords (require x and y to be set)
;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;
;      
;
;       .. image:: image_plot_f_map_example.png
;
;       For further details of map projections, consult the IDL
;         help documention for `MAP`. Alterations to the map,
;         grid, and continents can be achieved after plotting by
;         using the OBJECTS structure passed back by keyword.
;
;    CONTINENTS: in, optional, type=boolean
;       Overplot continent outlines.
;    HIRES: in, optional, type=boolean
;       Make the outlines high-resolution.
;    MAP_PROJECTION: in, optional, type=string
;       Warp the image to named map-projection.
;    CENTER_LATITUDE: in, optional, type=float
;       Centre the map on this latitude
;    CENTER_LONGITUDE: in, optional, type=float
;       Centre the map on this longitude.
;    LIMIT: in, optional, type=float
;       A 4-element array giving [lat_min, lon_min, lat_max, lon_max]
;
;
;
;   Position Keywords
;   ~~~~~~~~~~~~~~~~~
; 
;    PLOT_POSITION: in, optional, type="fltarr(4)"
;        The relative position of the image.
;          Position values should be 4 element floats with values
;          ranging from 0->1 [left,bottom,right,top]. Unlike
;          the position keyword for `PLOT` and `IMAGE`, this
;          position is relative to the layout keyword if set.
;
;    CBAR_POSITION: in, optional, type="fltarr(4)"
;        The relative position of the colourbar.
;          Position values should be 4 element floats with values
;          ranging from 0->1 [left,bottom,right,top]. Unlike
;          the position keyword for `PLOT` and `IMAGE`, this
;          position is relative to the layout keyword if set.
;
;   Window Keywords
;   ~~~~~~~~~~~~~~~
;
;    DONT_REFRESH: in, optional, type=boolean, default=0
;      Don't refresh the window before exiting.
;
;
;
;   Window Keywords passed to IMAGE()
;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;
;   The following keywords are passed in exactly the same way 
;   as to the `IMAGE` IDL function, so consult the documentation 
;   for that.
;
;    BUFFER: in, optional, type=boolean
;
;    CURRENT: in, optional, type=boolean
;
;    DIMENSIONS: in, optional
;
;    LAYOUT: in, optional
;
;    NAME: in, optional
;
;    WINDOW_TITLE: in, optional
;
;
;   Miscellaneous Keywords
;   ~~~~~~~~~~~~~~~~~~~~~~
;   
;    OBJECTS: out, optional, type=structure
;      A structure containing objects created by the routine.
;      Useful for subsequent plot modification. e.g.::
;
;               IDL> HELP, objects
;               ** Structure <232f4d8>, 6 tags, length=24, data length=22, refs=2:
;                  IMAGE_PLOT      OBJREF    <ObjHeapVar633433(IMAGE)>
;                  WINDOW          OBJREF    <ObjHeapVar2766(GRAPHICSWIN)>
;                  MAPGRID         OBJREF    <ObjHeapVar633434(MAPGRID)>
;                  CONTINENTS      OBJREF    <ObjHeapVar633491(MAPCONTINENTS)>
;                  COLOUR_BAR      OBJREF    <ObjHeapVar635928(IMAGE)>
;                  FLAG_CB         INT              2
;
;
;    HELP: in, optional, type=boolean
;          Load the help page for this routine.
;      
;    TEST: in, optional, type=boolean
;      Create a test plot showing off fancy things.
;
;    MAP_BUG_TEST: in, optional, type=boolean
;      Test to see if bug in map code is ok.
;
;    DEBUG: in, optional, type=boolean
;      Don't leave routine on error.
;
;    STOP: in, optional, type=boolean
;      Stop immediately before returning from code.
; 
;    
;
;
;
; :Examples:
;
;    ::
;      ip = IMAGE_PLOT_F( /TEST )
;
;
;    .. image:: image_plot_f_test.png
;
;
; :USES:
;    `HATCH_THIS_FG`
;    `IP_GET_POSITION`
;    `IP_GET_BYTE_COLOUR`
;    
;
;
; :BUGS:
;
;    * Mapping with the 'Geographic' projection causes problems if
;       the data crosses the international date line.
;
;    * Hatching can be problematic for fancy projections such as
;       Interupted Goode.
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
; :HISTORY:
;    08 NOV 2013 (AJAS) First attempt at simple version. Only 2d fields.
;
;    11 NOV 2013 (AJAS) Map projections added. Simple image tests seem ok.
;
;    13 NOV 2013 (AJAS) Images ok as well.
;
;    14 NOV 2013 (AJAS) Documentation added.
;
;    22 JAN 2014 (AJAS) Only apply taper to colourbar if range is set.
;                       Added a work-around for map projection errors.
;
;    31 JAN 2014 (AJAS) Added `SHOW_KEY_MISSING` keyword.
;                       Can now cope with reversed axes order e.g. [1 -> -1].
;                       Attempted to fix map plotting across the international
;                        date line. Sort of works, but can lead to white space.
;
;    03 FEB 2014 (AJAS) Removed `XRANGE` and `YRANGE` keywords, since image 
;                        doesn't deal correctly with ranges that aren't exactly
;                        on the boundary of a pixel. Corrected bug in reverse 
;                        axes.
;
;    12 FEB 2014 (AJAS) Speeded up the colour indexing for 2D arrays using
;                        array operations instead of looping pixel by pixel.
;
;    05 MAR 2014 (AJAS) Moved position keyword into a separate routine (incase
;                        wrapping routines (e.g. future `mappoints_f`) want to
;                        know the image dimensions before generating data.
;
;    18 AUG 2014 (AJAS) Added support for hatching pixels using `HATCH_THIS`
;                        keyword.
;
;    20 AUG 2014 (AJAS) Forced array passed to `IMAGE` to be of type `BYTE` when
;                        using maps since the interpolation could do strange
;                        things to colours close to white otherwise.
;
;    10 OCT 2014 (AJAS) Added `MAPPOINTS_TWO_MISSING` keyword to allow a second
;                        missing value (can be used as a land mask). Designed
;                        only to be called from `MAPPOINTS` so will remain
;                        undocumented.
;
;    15 OCT 2014 (AJAS) Set the colour dimension of the image to be the 0th
;                        dimension, so that images with an x- or y-dimension
;                        of 3 or 4 don't have the wrong elements selected as
;                        the RGB or RGBA channel.
;
;    22 OCT 2014 (AJAS) Added `BUFFER` keyword.
;
;    04 DEC 2014 (AJAS) Disabled refreshing during modifications to the image
;                        object so save rendering time.
;
;    12 DEC 2014 (AJAS) Added `DONT_REFRESH` keyword.
;
;    06 JAN 2015 (AJAS) Added `TRANSPARENT_MISSING` keyword. The keyword
;                        `MAPPOINTS_TWO_MISSING` is depreciated. Removed calls
;                        to `IDL_VERSION_GE`.
;
;    19 JAN 2015 (AJAS) Changed documentation to rst format.
;
;    20 JAN 2015 (AJAS) Removed the `MAPPOINTS_TWO_MISSING` keyword.
;
;
;
; :REQUIRES:
;    8.1
; 
;-

    ON_ERROR, KEYWORD_SET( debug ) ? 0 : 2


    IF FLOAT(!VERSION.release) LT 8.1 THEN MESSAGE,'Requires IDL 8.1 or higher'

    IF KEYWORD_SET(help) THEN BEGIN
       FG_HELP, 'image_plot_f'
       RETURN, 0
    ENDIF


    IF KEYWORD_SET( test ) THEN BEGIN
;+
; EXAMPLE CALLS USED BY THE /TEST FLAG.
;
;  
       READ_JPEG, FILEPATH('Day.jpg',SUBDIR=['examples','data']), day
       lat=(FINDGEN(512)+0.5)*0.351562-90 & lon=(FINDGEN(1024)+0.5)*0.351562-180

       fin = FINDGEN( 1024, 512 ) - 200000.0

       glon = lon #  REPLICATE(1,512)
       glat = lat ## REPLICATE(1,1024)
       qhatch = WHERE( glon GT -50 AND glon LT 20 AND glat GT 20 AND glat LT 40 )


       ex1 = IMAGE_PLOT_F(day,lon,lat,MAP_PROJECTION='Orthographic',$
                          CENTER_LAT=60,CENTER_LON=30,LAYOUT=[2,2,1],$
                          TITLE='Warping a jpeg image with orthographic projection',$
                          DIMENSIONS=[800,800], OBJECTS=o1, DEBUG=debug )

       ex2 = IMAGE_PLOT_F(fin, COLOURTABLE=20,/SIDEBAR, $
                          RANGE=[10000,300000], /LOG, $
                          SHOW_KEY_MISSING='Negative (missing)', $
                          TITLE='Logarithmic scale + hatching', $
                          HATCH_THIS=qhatch, $
                          /CURRENT, LAYOUT=[2,2,2], OBJECTS=o2, DEBUG=debug )

       ex3 = IMAGE_PLOT_F(fin,lon,lat, /BWRDIFF, /COLOURBAR, $
                          COLOUR_TOO_LOW='Blue', $
                          COLOUR_TOO_HIGH='Red', $
                          RANGE=[-1,1]*350000, /CONTINENTS, $
                          TITLE='Difference colour bar and continents',$
                          /CURRENT, LAYOUT=[2,2,3], OBJECTS=o3, DEBUG=debug )

       ex4 = IMAGE_PLOT_F(fin,lon,lat, COLOURTABLE=39, $
                          MAP_PROJECTION='Interrupted Goode', $
                          /CONTINENTS,  $
                          TITLE='Warping data with Interrupted Goode projection', $
                          /HIDE_TAPER, /COLOURBAR, $
                          /CURRENT, LAYOUT=[2,2,4], OBJECTS=o4, DEBUG=debug )
;
;-
                          
       objects = {objects_1:o1, objects_2:o2, objects_3:o3, objects_4:o4}
       RETURN, [ex1, ex2, ex3, ex4]
    ENDIF

    IF KEYWORD_SET( map_bug_test ) THEN BEGIN
       ;; Bug in the map_projection code.
       zz = BINDGEN(3,5,6)*21b 
       xx = FINDGEN(5)*20-50
       yy = FINDGEN(6)*20-50

       mbt1 = IMAGE_PLOT_F( zz, xx, yy, LAYOUT=[2,2,1], DIMENSIONS=[700,700], $
                           TITLE='Image', DEBUG=debug )

       mbt2 = IMAGE_PLOT_F( zz, xx, yy, LAYOUT=[2,2,2], /CURRENT, $
                            MAP_PROJECTION='Geographic',$
                            TITLE="+MAP_PROJECTION='Geographic'", DEBUG=debug)

       mbt3 = IMAGE_PLOT_F( zz, xx, yy, LAYOUT=[2,2,3], /CURRENT, $
                            MAP_PROJECTION='Sinusoidal',$
                            TITLE="+MAP_PROJECTION='Sinusoidal'", DEBUG=debug)

       mbt4 = IMAGE_PLOT_F( zz, xx, yy, LAYOUT=[2,2,4], /CURRENT, $
                            MAP_PROJECTION='Interrupted Goode',$
                            TITLE="+MAP_PROJECTION='Interrupted Goode'", DEBUG=debug)
       RETURN, [mbt1, mbt2, mbt3, mbt4]
    ENDIF




    ;; Initial variable checking and setup.
    im_size = SIZE( z )

    IF im_size[0] LT 2 OR im_size[0] GT 3 THEN MESSAGE,$ 
       'Image has incorrect number of dimensions (2 or 3).'

    ;; Set missing value if not there.
    IF N_ELEMENTS( missing ) NE 1 THEN missing = !VALUES.F_NAN

    ;; If no window title is given, make it 'Image Plot'
    IF ~ KEYWORD_SET( window_title ) THEN window_title='Image Plot'

    ;; Get the dimensions.
    IF im_size[0] EQ 2 THEN BEGIN
       nx = im_size[ 1 ]
       ny = im_size[ 2 ]
       ;; If we generate the colours ourselves, then they will be in the 1st
       ;; dimension. This saves misery with low dimension images e.g. [4,3]
       ;; where the 1st dimension with size 3 OR 4 will be interpreted as the
       ;; RGB or RGBA dimension.
       colour_dim = 0l
    ENDIF ELSE BEGIN
       colour_dim = WHERE( im_size[1:3] EQ 3 OR im_size[1:3] EQ 4, ntmp )
       CASE ntmp OF ;; If more than one dimension has 3 elements
          ;; pick the first one.
          0: MESSAGE,'Incorrect dimensions for image.'
          1: colour_dim = colour_dim[0]
          ELSE: colour_dim=colour_dim[0]
       ENDCASE
       nx = im_size[ colour_dim EQ 0 ? 2 : 1 ]
       ny = im_size[ colour_dim EQ 2 ? 2 : 3 ]
    ENDELSE



    ;; Set up the x and y axes and ranges.
    IF KEYWORD_SET( x ) THEN BEGIN
       x_in = KEYWORD_SET( xlog ) ? ALOG10( x ) : x
       ;; IMAGE() wont allow irregular bin distances, so that's fine, 
       ;; but it does index the bin at the lhs, so we shift by half a bin.
       ;; We wont go looking for irregular distances. It will just fuck
       ;; up the plotting range if they're there.
       xbin = x_in[1] - x_in[0]
       xrange = [ x_in[0]-xbin*0.5, x_in[-1]+xbin*0.5 ]
    ENDIF ELSE xrange=[0,nx]-0.5

    ;; Repeat for y-axis.
    IF KEYWORD_SET( y ) THEN BEGIN
       y_in = KEYWORD_SET( ylog ) ? ALOG10( y ) : y
       ybin = y_in[1] - y_in[0]
       yrange = [ y_in[0]-ybin*0.5, y_in[-1]+ybin*0.5 ]
    ENDIF ELSE yrange=[0,ny]-0.5


    ;; Do we need to call the mapping code?
    earth = KEYWORD_SET( continents ) OR KEYWORD_SET( map_projection )

    IF earth AND ( N_ELEMENTS( x ) EQ 0 OR N_ELEMENTS( y ) EQ 0) THEN $
       MESSAGE,'Must supply lon/lat coordinates for map plotting.'




    ;----------------------------------------------------------------------
    ;----------------------------------------------------------------------
    ;; For 2D field, we need to assign a colour to each data point.
    ;----------------------------------------------------------------------
    IF im_size[0] EQ 2 THEN BEGIN



       ;----------------------------------------------------------------------
       ;----------------------------------------------------------------------
       ;; Get the colours going.
       ;----------------------------------------------------------------------
       ncolours = KEYWORD_SET(ncolours) ? (0 > LONG(ncolours) < 250) : 30l

       IF KEYWORD_SET( bwrdiff ) THEN BEGIN

          b = [255,255,  0]
          g = [  0,255,  0]
          r = [  0,255,255]

          xx0 = FINDGEN(3) / 2.0
          xx1 = FINDGEN(ncolours) / (ncolours-1.0)

          rgb = BYTE([ [INTERPOL(r,xx0,xx1)], $
                       [INTERPOL(g,xx0,xx1)], $
                       [INTERPOL(b,xx0,xx1)] ] )


       ENDIF ELSE IF KEYWORD_SET( bwr2diff ) THEN BEGIN

          b = [ 51,204,204,255, 77,  0,  0]
          g = [  0,  0,  0,255,204,  0,  0]
          r = [ 51,153,  0,255,255,255, 77]

          xx0 = FINDGEN(7) / 6.0
          xx1 = FINDGEN(ncolours) / (ncolours-1.0)

          rgb = BYTE([ [INTERPOL(r,xx0,xx1)], $
                       [INTERPOL(g,xx0,xx1)], $
                       [INTERPOL(b,xx0,xx1)] ] )


       ENDIF ELSE IF KEYWORD_SET( bgrdiff ) THEN BEGIN

          b = [  0,229,255]
          g = [  0,229,  0]
          r = [255,229,  0]

          xx0 = FINDGEN(3) / 2.0
          xx1 = FINDGEN(ncolours) / (ncolours-1.0)

          rgb = BYTE([ [INTERPOL(r,xx0,xx1)], $
                       [INTERPOL(g,xx0,xx1)], $
                       [INTERPOL(b,xx0,xx1)] ] )


       ENDIF ELSE IF KEYWORD_SET( green ) THEN BEGIN

          b = [  0,  0,  0,  0,  0,  0, 34,144,251]
          g = [  1, 31, 63, 95,127,159,191,223,254]
          r = [  0,  0,  0,  0, 48,100,151,203,253]

          xx0 = FINDGEN(9) / 8.0
          xx1 = FINDGEN(ncolours) / (ncolours-1.0)

          rgb = BYTE([ [INTERPOL(r,xx0,xx1)], $
                       [INTERPOL(g,xx0,xx1)], $
                       [INTERPOL(b,xx0,xx1)] ] )


       ENDIF ELSE IF KEYWORD_SET( red ) THEN BEGIN

          b = [  0,  0,  0,  0,  0,  0,  3,129,251]
          g = [  0,  0,  0,  0, 13, 73,134,194,253]
          r = [  1, 44, 91,137,184,230,255,255,255]

          xx0 = FINDGEN(9) / 8.0
          xx1 = FINDGEN(ncolours) / (ncolours-1.0)

          rgb = BYTE([ [INTERPOL(r,xx0,xx1)], $
                       [INTERPOL(g,xx0,xx1)], $
                       [INTERPOL(b,xx0,xx1)] ] )


       ENDIF ELSE IF KEYWORD_SET( blue ) THEN BEGIN

          r = [  0,  0,  0,  0,  0,  0,  3,129,251]
          g = [  0,  0,  0,  0, 13, 73,134,194,253]
          b = [  1, 44, 91,137,184,230,255,255,255]

          xx0 = FINDGEN(9) / 8.0
          xx1 = FINDGEN(ncolours) / (ncolours-1.0)

          rgb = BYTE([ [INTERPOL(r,xx0,xx1)], $
                       [INTERPOL(g,xx0,xx1)], $
                       [INTERPOL(b,xx0,xx1)] ] )


       ENDIF ELSE BEGIN
          ;; 3 Options for colour table. Not set, number, or rgb table.
          CASE( N_ELEMENTS(colourtable) ) OF
             ;; Default is colour table 39.
             0:  ct = 39
             ;; If we have a single number, then we're requesting a colour table.
             1:  ct = LONG( colourtable )
             ELSE: BEGIN
                sct = SIZE( colourtable )
                ;; Looking for an [Nx3] colour table.
                IF sct[0] EQ 2 && sct[2] EQ 3 && $
                   ISA(colourtable,/NUMBER) THEN BEGIN
                   ct = -1
                   rgb = BYTE( colourtable )
                   ncolours = N_ELEMENTS( rgb[*,0] )
                   ;; Or a string list of colours...
                ENDIF ELSE IF sct[0] EQ 1 && ISA(colourtable,'STRING') THEN BEGIN
                   ct = -1
                   rgb = BYTARR(sct[1],3)
                   FOR ict=0, sct[1]-1 DO rgb[ict,*]=$
                      IP_GET_BYTE_COLOUR(colourtable[ict])
                   ncolours = N_ELEMENTS(rgb[*,0])
                ENDIF ELSE MESSAGE,$
                   'Colour tables must be single numbers, [n x 3] BYTE arrays, '+$
                   'or n element STRING arrays.'
             ENDELSE
          ENDCASE

          IF ~ KEYWORD_SET( rgb ) THEN BEGIN
             LOADCT, /SILENT, ct, RGB_TABLE=rgb, NCOLORS=ncolours+2
             rgb = rgb[ 1:ncolours, * ]
          ENDIF


       ENDELSE


       IF KEYWORD_SET(reverse_colours) THEN rgb=REVERSE(rgb,1)

       c_too_low = KEYWORD_SET(ctl) ? IP_GET_BYTE_COLOUR(ctl) : [130b,130b,130b]
       c_too_high= KEYWORD_SET(cth) ? IP_GET_BYTE_COLOUR(cth) : [190b,190b,190b]
       c_missing = KEYWORD_SET(cm ) ? IP_GET_BYTE_COLOUR(cm ) : [ 50b, 50b, 50b]
       ;---------------------------------------------------------------------
   


       ;---------------------------------------------------------------------
       ;---------------------------------------------------------------------
       ;; Set up the range.
       ;---------------------------------------------------------------------
       CASE N_ELEMENTS( range ) OF
          0: BEGIN
             range_ = KEYWORD_SET( log ) ? [ MIN(ALOG10(z),MAX=mmm,/NAN), mmm] : $
                                           [ MIN(       z, MAX=mmm,/NAN), mmm]

             ;; If there is no range set, then we don't need a taper.
             IF ~KEYWORD_SET(hide_taper) THEN hide_taper=1b
          ENDCASE
          2: range_ = KEYWORD_SET( log ) ? ALOG10( range ) : range
          ELSE: MESSAGE,'Range must be a 2 element array'
       ENDCASE
       ;;; Add a 0.1% cushion on either end of the range.
       range_ += [-1,1]*0.001*( range_[1] - range_[0] )


      

       ;---------------------------------------------------------------------
       ;---------------------------------------------------------------------
       ;; Make an image by picking the correct colour.
       ;---------------------------------------------------------------------


       ;; Conversion factor from data value to colour index.
       ct_convert = FLOAT( ncolours ) / FLOAT(range_[1] - range_[0])



       ;; We will offset the indices by one, and place upper and lower
       ;; caps on the values.
       r = [ c_too_low[0], rgb[*,0], c_too_high[0], c_missing[0] ]
       g = [ c_too_low[1], rgb[*,1], c_too_high[1], c_missing[1] ]
       b = [ c_too_low[2], rgb[*,2], c_too_high[2], c_missing[2] ]

       ;; Set up the alpha mask in case it is needed.
       m = [ REPLICATE(255B,ncolours+2), 0B ]


       ;; Use array operations to scale all elements to colour indices.
       points = KEYWORD_SET( log ) ? ALOG10( z ) : z
       index = 0 > ( FLOOR(  (points - range_[0]) * ct_convert ) +1 ) < (ncolours+1)

       ;; Look for missing data and change colour.
       q_missing = WHERE( points EQ missing OR ~FINITE(points), n_missing )
       IF n_missing GT 0 THEN index[q_missing] = ncolours+2


       ;; Build the image.
       im = KEYWORD_SET( transparent_missing ) ? $
        [ [[ r[index] ]], [[ g[index] ]], [[ b[index] ]], [[ m[index] ]] ] : $
        [ [[ r[index] ]], [[ g[index] ]], [[ b[index] ]] ]

       ;; Transpose it so that the colour index comes first just 
       ;; in case nx or ny have values 3 or 4.
       im = TRANSPOSE( im, [2,0,1] )
       im_new = im



    ENDIF ELSE BEGIN 
       ;; If we have an image, then we don't really do any work.
       im = z

    ENDELSE

    ;; If xbin is negative, then we need to reverse indices.
    IF KEYWORD_SET( xbin ) && xbin LT 0 THEN BEGIN
       reverse_dim = (colour_dim EQ 0) ? 2 : 1
       im = REVERSE( TEMPORARY( im ), reverse_dim )
    ENDIF
    ;; Similarly for ybin.
    IF KEYWORD_SET( ybin ) && ybin LT 0 THEN BEGIN
       reverse_dim = (colour_dim EQ 2) ? 2 : 3
       im = REVERSE( TEMPORARY( im ), reverse_dim )
    ENDIF



    ;;---------------------------------------------------------------------
    ;;---------------------------------------------------------------------
    ;; Deal with position of main plot and colourbar.
    ;;---------------------------------------------------------------------
    flag_cb = KEYWORD_SET( sidebar ) + $
              KEYWORD_SET( colourbar ) * 2 + $
              KEYWORD_SET(show_key_missing) * 4


    position = IP_GET_POSITION(flag_cb,layout,$
                               plot_position_in,$
                               cbar_position_in)


    ;; If there is a colourbar, then the missing key can be set. We alter 
    ;; the colourbar position so that the bottom / left-most 6% contains
    ;; this key. This is done in IP_GET_POSITION. Here we tinker with the
    ;; text alignment.
    IF (flag_cb AND 4) EQ 4 THEN BEGIN

       IF N_ELEMENTS( show_key_missing ) GT 1 THEN MESSAGE, $
          'Only one label for "Missing" key is permitted.'

       ;; If the keyword is a string, use it as the title for missing data.
       miss_title = SIZE(show_key_missing,/TYPE) EQ 7 ? show_key_missing : 'Missing'

       IF (flag_cb AND 1) EQ 1 THEN BEGIN ; Sidebar
          miss_title = miss_title + ' '
          v_align_miss = 0.5
          h_align_miss = 1.0
       ENDIF ELSE BEGIN ; Colourbar
          v_align_miss = 1.3
          h_align_miss = 0.5
       ENDELSE

    ENDIF




    ;; PLOT!
    IF KEYWORD_SET( earth ) THEN BEGIN

       ;; Instead of the [x/y]range and [x/y] values, we will
       ;; use the IMAGE_LOCATION and IMAGE_DIMENSIONS to define
       ;; the edges of our picture. Currently, IDL likes to
       ;; truncate the first row and column, so we'll also have
       ;; to account for that.
       image_loc = [ MIN(xrange), MIN(yrange) ]
       image_dim = [ MAX(xrange), MAX(yrange) ] - image_loc



       limit_pass = KEYWORD_SET( limit ) ? limit : $
                       [ image_loc[1],$
                         image_loc[0],$
                         image_loc[1]+image_dim[1], $
                         image_loc[0]+image_dim[0]   ]

       
       ;; Now work around IDL map bug by adding an extra element to 
       ;; the arrays and painting white...
       dim_fudged = SIZE( /DIM, im )
       FOR idim=0,2 DO IF idim NE colour_dim THEN dim_fudged[idim]+=1
       im_fudged = BYTARR( dim_fudged )*0 + 255b
       CASE colour_dim OF
          0: im_fudged[*,1:*,1:*] = im
          1: im_fudged[1:*,*,1:*] = im
          2: im_fudged[1:*,1:*,*] = im
       ENDCASE

       im=im_fudged
       ipf = IMAGE( BYTE(im_fudged), $
                    XLOG=xlog, YLOG=ylog, $
                    AXIS_STYLE=2, $
                    ASPECT_RATIO=0, $
                    TITLE=title, $
                    INTERPOLATE=0, $
                    NAME=name, $
                    WINDOW_TITLE=window_title, $
                    FONT_NAME=font_name, $
                    FONT_STYLE=font_style,$
                    FONT_SIZE=font_size, $
                    POSITION=position.plot, $
                    CURRENT=current, $
                    DIMENSIONS=dimensions, $
                    BUFFER=buffer, $
                    ;; Use the image width and bottom left corner to define image.
                    IMAGE_LOCATION=image_loc, $
                    IMAGE_DIMENSIONS=image_dim,$
                    LIMIT=limit_pass, $
                    ;; IF no map projection is set, use cartesian projection.
                    MAP_PROJECTION=KEYWORD_SET(map_projection) ? $
                                           map_projection : 'EQUIRECTANGULAR',$
                    CENTER_LATITUDE=  KEYWORD_SET(clat) ? clat : !NULL, $
                    CENTER_LONGITUDE= KEYWORD_SET(clon) ? clon : !NULL, $
                    GRID_UNITS=2 )




    ENDIF ELSE BEGIN

       ;; Don't use x and y. Image location and dimensions should do
       ;; the same thing with less effort.
       ipf = IMAGE( im, $ ; x, y, $
                    IMAGE_LOCATION=[xrange[0], yrange[0]],$
                    IMAGE_DIMENSIONS=[xrange[1]-xrange[0],yrange[1]-yrange[0]],$
                    XRANGE=xrange, $
                    YRANGE=yrange, $
                    XTITLE=xtitle, $
                    YTITLE=ytitle, $
                    XLOG=xlog, YLOG=ylog, $
                    AXIS_STYLE=2, $
                    ASPECT_RATIO=0, $
                    TITLE=title, $
                    NAME=name, $
                    BUFFER=buffer, $
                    WINDOW_TITLE=window_title, $
                    FONT_NAME=font_name, $
                    FONT_STYLE=font_style,$
                    FONT_SIZE=font_size, $
                    POSITION=position.plot, $
                    CURRENT=current, $
                    DIMENSIONS=dimensions, $
                    GRID_UNITS=0 )
    ENDELSE


    ;; At this point, stop refreshing the window to save redraws.
    ipf.refresh, /DISABLE


    ;; The first two plotting objects.
    objects = {image_plot:ipf, window:ipf.window}


    ;; Add the map or grid if necessary.
    IF earth THEN BEGIN
       mapgrid = ipf.MAPGRID
       mapgrid.LINESTYLE = "solid"
       mapgrid.TRANSPARENCY = 70
       ;; For some reason, we have to turn the clipping off to turn it on!
       ;; This only works in IDL 8.2 and above.
       IF FLOAT(!VERSION.release) GE 8.2 THEN BEGIN
          mapgrid.clip = 0
          mapgrid.clip = 1
       ENDIF
       ;; Add the mapgrid structure.
       objects = CREATE_STRUCT( objects, 'mapgrid', mapgrid )

       IF KEYWORD_SET(continents) THEN BEGIN
          continents = MAPCONTINENTS( HIRES=hires, LIMIT=limit_pass  )
          ;; Add the continents structure.
          objects = CREATE_STRUCT( objects, 'continents', continents)
       ENDIF
    ENDIF



    ;; Add colourbar!
    IF (KEYWORD_SET(sidebar) OR KEYWORD_SET(colourbar)) AND $
       im_size[0] EQ 2 AND ~KEYWORD_SET(hide_cbar) $
    THEN BEGIN
       ;; The image that will be the colourbar.
       cbar_im = [   [[TRANSPOSE( rgb )]], [[TRANSPOSE( rgb )]] ]
       ;; And the associated values.
       cbar_val= FINDGEN(ncolours) * (range_[1]-range_[0])/FLOAT(ncolours)+range_[0]


       ;; Set up the axes.
       cxtitle = !NULL
       cytitle = !NULL
       cx = [0,1]
       cy = [0,1]
       cxlog = 0
       cylog = 0
       show_x = 0b
       show_y = 0b
       CASE (flag_cb MOD 4) OF
          1: BEGIN     ; SIDEBAR
             ;; We always want the colour index first where possible so that low
             ;; dimension images don't confuse IMAGE(). e.g. BYTARR(4,5,3) will 
             ;; be interpreted as a 5x3 image with an alpha channel whereas 
             ;; BYTARR(3,4,5) will be a 4x5 RGB image without an alpha channel.
             ;; Currently, cbar_im is [3,ncol,2]. We want to transpose so that
             ;; we get [3,2,ncol].
             cbar_im = TRANSPOSE( cbar_im, [0,2,1] )
             cytitle = KEYWORD_SET(cbar_title) ? cbar_title : ''
             cy      = cbar_val
             show_y  = 1b
             cylog   = KEYWORD_SET( log )
          END
          2: BEGIN ; COLOURBAR
             cxtitle = KEYWORD_SET(cbar_title) ? cbar_title : ''
             cx      = cbar_val
             show_x  = 1b
             cxlog   = KEYWORD_SET( log )
          END
       ENDCASE



       ;; Was experimenting with the COLORBAR() function, but it's
       ;; not very flexible so went back for a second IMAGE() call.

       ipf_cbar = IMAGE( cbar_im, cx, cy, /CURRENT, $
                         POSITION=position.colourbar, $
                         AXIS_STYLE=2, $
                         ASPECT_RATIO=0, $
                         XTRANSPARENCY=show_y*100, $
                         YTRANSPARENCY=show_x*100, $
                         XLOG=cxlog, $
                         YLOG=cylog, $
                         FONT_NAME=font_name, $
                         FONT_SIZE=font_size, $
                         FONT_STYLE=font_style, $
                         XTITLE=cxtitle, $
                         YTITLE=cytitle   )

       ;; Get rid of the axis we don't care about.
       cbar_axes = ipf_cbar.axes
       cbar_axes[(flag_cb MOD 4)-1].HIDE = 0
       cbar_axes[2-(flag_cb MOD 4)].subticklen = 0.2
       cbar_axes[4-(flag_cb MOD 4)].subticklen = 0.2

       ;; Get the values at the top and bottom of the colourbar.
       ;; Then, we add a small arrow if required. Use /DATA
       ;; coordinates so that changing the window shape doesn't
       ;; mess things up. Get the min and max values of the cbar.
       cb_bot = cbar_val[0]
       cb_top = cbar_val[-1]+cbar_val[1]-cbar_val[0]
       

       IF KEYWORD_SET( hide_taper ) THEN BEGIN

          ;; If there's no tapering, we need to draw lines at top and
          ;; bottom of the colourbar or it looks odd.
          IF KEYWORD_SET( log ) THEN BEGIN
             cb_bot = 10.0^( cb_bot )
             cb_top = 10.0^( cb_top )
          ENDIF
          IF (flag_cb MOD 4) EQ 1 THEN BEGIN
             tmp=PLOT(/OVERPLOT, [0,2], cb_bot*[1,1], CLIP=0 )
             tmp=PLOT(/OVERPLOT, [0,2], cb_top*[1,1], CLIP=0 )
          ENDIF ELSE BEGIN
             tmp=PLOT(/OVERPLOT, cb_bot*[1,1], [0,2], CLIP=0 )
             tmp=PLOT(/OVERPLOT, cb_top*[1,1], [0,2], CLIP=0 )                
          ENDELSE

       ENDIF ELSE BEGIN


          ;; Length of the taper will be 3% of the cbar length.
          cb_taper_length = ( cb_top - cb_bot ) * 0.03
       
          CASE (flag_cb MOD 4) OF
             1: BEGIN           ; SIDEBAR
                cb_taper_x0 = [0,2,1,0]
                cb_taper_x1 = cb_taper_x0
                cb_taper_y0 = cb_bot - [0,0,cb_taper_length,0]
                cb_taper_y1 = cb_top + [0,0,cb_taper_length,0]
                IF KEYWORD_SET( log ) THEN BEGIN
                   cb_taper_y0 = 10.0^cb_taper_y0
                   cb_taper_y1 = 10.0^cb_taper_y1
                ENDIF
             END
             2: BEGIN           ; COLOURBAR
                cb_taper_x0 = cb_bot - [0,0,cb_taper_length,0]
                cb_taper_x1 = cb_top + [0,0,cb_taper_length,0]
                cb_taper_y0 = [0,2,1,0]
                cb_taper_y1 = cb_taper_y0
                IF KEYWORD_SET( log ) THEN BEGIN
                   cb_taper_x0 = 10.0^( cb_taper_x0 )
                   cb_taper_x1 = 10.0^( cb_taper_x1 )
                ENDIF
             END
          ENDCASE
          ;; Draw the tapers.
          cbar_taper0 = POLYGON(/DATA,TARGET=ipf_cbar,CLIP=0,$
                                cb_taper_x0,cb_taper_y0,FILL_COLOR=c_too_low )
          cbar_taper1 = POLYGON(/DATA,TARGET=ipf_cbar,CLIP=0,$
                                cb_taper_x1,cb_taper_y1,FILL_COLOR=c_too_high)
       ENDELSE 


       objects = CREATE_STRUCT(objects, 'colour_bar',ipf_cbar, 'flag_cb',flag_cb)

       IF (flag_cb AND 4) EQ 4 THEN BEGIN
          miss_x = position.missing[ [0,0,2,2,0] ]
          miss_y = position.missing[ [1,3,3,1,1] ]
          text_x = (flag_cb AND 1) ? miss_x[0] : (miss_x[0]+miss_x[2])*0.5
          text_y = (flag_cb AND 1) ? (miss_y[0]+miss_y[2])*0.5 : miss_y[0]
          cbar_missing= POLYGON(/NORMAL, miss_x, miss_y, $
                                FILL_COLOR=c_missing)



          text_missing= TEXT( /NORMAL, text_x, text_y, miss_title, $
                              FONT_NAME=font_name, $
                              FONT_STYLE=font_style,$
                              FONT_SIZE=font_size, $
                              ALIGNMENT=h_align_miss, $
                              VERTICAL_ALIGNMENT=v_align_miss )

          objects = CREATE_STRUCT(objects, 'text_missing',text_missing)

       ENDIF

    ENDIF


    ;; IF the LAYOUT keyword is set, we should change font sizes.
    IF KEYWORD_SET( layout ) AND NOT(KEYWORD_SET(font_size)) THEN BEGIN
       ;; See what IDL would do for a standard plot and copy it.
       itmp = IMAGE(im,/NODATA, AXIS_STYLE=2,LAYOUT=layout,$
                    IMAGE_LOCATION=[xrange[0], yrange[0]],$
                    IMAGE_DIMENSIONS=[xrange[1]-xrange[0], yrange[1]-yrange[0]],$
                    XTRANSPARENCY=100, YTRANSPARENCY=100, $
                   /CURRENT, ASPECT_RATIO=0,TITLE=' ')

       IF KEYWORD_SET( title ) THEN ipf.title.font_size = itmp.title.font_size

       it_axes = itmp.axes
       ip_axes = ipf.axes
       ipf.font_size = itmp.font_size

       FOR i=0,3 DO BEGIN
          tfs = it_axes[i].tickfont_size
          ip_axes[i].tickfont_size = tfs
          IF flag_cb GT 0 AND ~ KEYWORD_SET(hide_cbar) THEN cbar_axes[i].tickfont_size=tfs
          it_axes[i].delete
       ENDFOR
       IF (flag_cb AND 4) EQ 4 AND ~KEYWORD_SET(hide_cbar) THEN text_missing.font_size=tfs
       itmp.delete
    ENDIF
 

    ;; If we want to hatch the image, then do it now.
    IF KEYWORD_SET( HATCH_THIS ) THEN BEGIN
       ht = HATCH_THIS_FG( nx, ny, LONG(hatch_this), ipf )
       objects = CREATE_STRUCT(objects, 'hatch_this',ht)
    ENDIF


    ;; Turn refreshing back on and exit.
    IF ~ KEYWORD_SET(dont_refresh) THEN ipf.refresh



    IF KEYWORD_SET( stop ) THEN STOP


    ;; Return the image object.
    RETURN, ipf

 END
 
