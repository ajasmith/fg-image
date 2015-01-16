;+
; NAME:
;  FUNCTION HATCH_THIS_FG, nx, ny, qhatch, image_object
;
; PURPOSE:
;  Given a regular [nx, ny] 2D array, hatch the areas marked by 
;   qhatch in an IMAGE_PLOT_FG image.
;
; INPUT VARIABLES:
;  nx       - Number of elements in x dimension.
;  ny       - Number of elements in y dimension.
;  qhatch   - Elements to hatch (as given by WHERE() ).
;  image_object - An IMAGE object.
;
; OUTPUT VARIABLES:
;  NONE.
; 
; EXAMPLE CALL:
;    
;    IDL> z = RANDOMU(seed,[15,15]) - 0.5
;    IDL> i = IMAGE_PLOT_F( z,/SIDE,/BWRDIF)
;    IDL> ;; Hatch all positive values of z.
;    IDL> q = WHERE( z GT 0 )
;    IDL> h = HATCH_THIS_FG( 15, 15, q, i )
;
; HISTORY:
;  18 Aug 2014 (AJAS) Modified HATCH_THIS procedure for
;                      use with IMAGE() function graphics.
;
;- 

FUNCTION hatch_this_fg, nx, ny, q, ipo

  ON_ERROR, 2

  ; WHERE( ) returns -1 for no values found.
  IF q[0] EQ -1 THEN RETURN, -1
  ; Unless you use WHERE(/NULL) in which case
  ; a null pointer with size 0.
  IF N_ELEMENTS( q ) EQ 0 THEN RETURN, -1


  ;; Turn into integers
  nx = LONG( nx[0] )
  ny = LONG( ny[0] )
  q  = LONG(   q   )
  nq = N_ELEMENTS( q )

  ;; We will be overplotting on the image object so
  ;; should probably test that we are dealing with one of those.
  IF ~ISA(ipo,'IMAGE') THEN MESSAGE, $
     'Can only hatch over images. You must pass an image object reference.'




  ;; If there is a map projection active, then we'll need
  ;; to convert lat-lon to projected metres.
  isMap = KEYWORD_SET( ipo.MAP_PROJECTION )
  IF isMap THEN BEGIN
     thisMap = ipo.MAPPROJECTION

     ;; Set up plotting scales in degrees.
     limit = thisMap.LIMIT
     x0 = limit[ 1 ]
     dx = ( limit[ 3 ] - limit[ 1 ] ) / DOUBLE( nx )
     
     y0 = limit[ 0 ]
     dy = ( limit[ 2 ] - limit[ 0 ] ) / DOUBLE( ny )
     
  ENDIF ELSE BEGIN
     ;; Set up the plotting scales.
     x0 = ( ipo.XRANGE )[0]
     dx = ( ( ipo.XRANGE )[1] - x0 ) / DOUBLE( nx )
     
     y0 = ( ipo.YRANGE )[0]
     dy = ( ( ipo.YRANGE )[1] - y0 ) / DOUBLE( ny )
  ENDELSE
     

  ; Loop through the values of q, defining the set of 
  ; polygons we shall use for hatching the area.
  xf = DBLARR( nq*5 )
  yf = DBLARR( nq*5 )
  ;; Use the connectivity property to separate the shapes.
  cf = LONARR( nq * 6 )

  ;; Loop!
  FOR i=0L, N_ELEMENTS( q )-1 DO BEGIN

     thisX = x0 + ( [0,0,1,1,0] + (q[i] MOD nx) ) * dx
     thisY = y0 + ( [0,1,1,0,0] + (q[i]  /  nx) ) * dy

     IF isMap THEN BEGIN
        projMetres = thisMap -> MapForward( thisX, thisY )
        thisX = REFORM( projMetres[ 0, * ] )
        thisY = REFORM( projMetres[ 1, * ] )
     ENDIF 

     xf[i*5: i*5+4] = thisX
     yf[i*5: i*5+4] = thisY
     cf[i*6: i*6+5] = [5, i*5 + LINDGEN(5) ]

     ;IF TOTAL(FINITE(thisX),/INT) LT 5 OR TOTAL(FINITE(thisY),/INT) LT 5 THEN $
     ;   PRINT,thisX,thisY,i,FORMAT='(2("[",4(F0,","),F0,"], "),I10 )'
  ENDFOR

  p0 = POLYGON(xf, yf, CONNECTIVITY=cf, TARGET=ipo, /DATA, $
               LINESTYLE=6, FILL_COLOR='Black', $
               FILL_PATTERN=OBJ_NEW('IDLgrPattern',1,$
                                    ORIENTATION=40, $
                                    SPACING=10) $
               )

  p1 = POLYGON(xf, yf, CONNECTIVITY=cf, TARGET=ipo, /DATA, $
               LINESTYLE=6, FILL_COLOR='Black', $
               FILL_PATTERN=OBJ_NEW('IDLgrPattern',1,$
                                    ORIENTATION=320, $
                                    SPACING=10) $
               )

  RETURN, [p0, p1]

END










;; Test hatch_this_fg

;; Only generate data the first time so comparisons between
;; repeated runs are clearer.
IF ~ARRAY_EQUAL(SIZE(z),[2,15,15,4,225]) THEN $
   z = SMOOTH( RANDOMU(seed,[15,15])*100-50, 5, /EDGE_WRAP )
x = FINDGEN(15)*20-140
y = FINDGEN(15)*10-70
q = WHERE( z GT 0 )
r = [-1,1]*MAX(ABS(z))


;; First test
i0 = IMAGE_PLOT_F( z, x, y, RANGE=r, /HIDE_TAPER, /SIDE, /BWRDIF,$
                   TITLE='Hatching positive values', $
                   LAYOUT=[1,2,1],DIMENSION=[700,800],$ 
                   CURRENT=FG_CURRENT(i0) )
h = HATCH_THIS_FG( 15, 15, q, i0 )


;; Compare to map projected version, using the keyword in IMAGE_PLOT_F().
i1 = IMAGE_PLOT_F( z, x, y, RANGE=r, /SIDE, /BWRDIF, $
                   LAYOUT=[1,2,2], /CURRENT, /HIDE_TAPER, $
                   TITLE='Hatching positive values on map projection', $
                   /CONTINENTS,MAP_PROJECTION='Mercator',HATCH_THIS=q )



END