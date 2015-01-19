; docformat='rst'
;
; :NAME:
;   fg_colours
;
FUNCTION FG_COLOURS, width, HELP=help
;+
;
; 
;    Creates a function graphics window with a list of all of the
;    colours stored in the !COLOR variable.
;
;
;
; :Categories:
;    Function graphics
;
; :Returns:
;    A `window` object.
;
; :Params:
;    width: in, optional, type=integer, default=600
;           The width of the window.
;
; :Keywords:
;    help: in, optional, type=boolean
;          Load the help page for this routine.
;
; :Examples:
;   Create a list of all colours:: 
;       IDL> f = FG_COLOURS()
;
; .. image:: fg_colours.png
;  
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
; 
;-
  ON_ERROR, 2
  
  IF KEYWORD_SET(help) THEN BEGIN
     FG_HELP, 'fg_colours'
     RETURN, 0
  ENDIF


  c = TAG_NAMES( !COLOR )
  n = N_ELEMENTS( c )

  dim = [1,SQRT(2)]* (KEYWORD_SET(width) ? width[0] : 600 )
  w = WINDOW( DIMENSION=dim, WINDOW_TITLE='Function graphics colors' )

  nx = 3
  ny = (n+1) / nx - 1

  dx = 1.0 / FLOAT( nx )
  dy = 1.0 / FLOAT( ny )


  FOR i=0, n-1 DO BEGIN

     x = ( [1,0,0,1,1]*0.3 + (i MOD nx)      ) * dx
     y = ( [1,1,0,0,1]     - (i  /  nx) + ny ) * dy

     p = POLYGON( x, y, /NORMAL, FILL_COLOR=c[i], COLOR=c[i] )
     p -> ORDER, /SEND_TO_BACK

     t = TEXT( x[0], y[0]-dy*0.5, ' '+c[i], /NORMAL, $
               FONT_SIZE=9, VERTICAL_ALIGNMENT=0.5 )


  ENDFOR

  RETURN, w


END

;; Generate colours
f = fg_colours()
f -> save, 'fg_colours.png', width=600
print,     'fg_colours.png created.'
END
