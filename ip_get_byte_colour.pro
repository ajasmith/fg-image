; docformat = 'rst'
;
;
 FUNCTION IP_GET_BYTE_COLOUR, colour
;+
;
;
; Takes a colour name, or a 3-element array and returns a valid
; 3-element byte array valid for RGB colours in IDL. Errors return
; black. [0b, 0b, 0b]. This is designed for use with IMAGE_PLOT_F.
;
; :Categories:
;    Function graphics, image_plot
;
; :Examples:
; 
;    Get the RGB values of hot pink::
;
;        IDL> PRINT, IP_GET_BYTE_COLOUR( 'hot pink' )
;        255 105 180
;
;    Make sure that the RGB values are correct type.
;        IDL> PRINT, IP_GET_BYTE_COLOUR( [34.2, 15.8, 200.0] )
;        34  15 200
;
;
;
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;-
   
  ; Get the variable type. We only deal with integers, floats and strings.
  s = SIZE( /TYPE, colour )

  ; Output when problem.
  out = [0b,0b,0b]

  ; If it's a string, compare to the IDL !COLOR system variable.
  IF s EQ 7 AND N_ELEMENTS(colour) EQ 1 THEN BEGIN

     ; Get list of colours.
     clist = TAG_NAMES(!COLOR)
     ; And find the right one.
     q = WHERE(STRUPCASE(STRJOIN(STRSPLIT(/EXTRACT,colour),'_')) EQ clist, n )

     ; If the colour exists, use it.
     IF n EQ 1 THEN out = !COLOR.( q[0] ) ELSE BEGIN
        MESSAGE,/CONTINUE,'Colour name '+colour+' not recognised. '+$
                'Try PRINT, TAG_NAMES(!COLOR) for full list of valid options.'
     ENDELSE
     
  ; If it's an integer or float, with 3 elements, then it's an RGB colour.
  ENDIF ELSE IF N_ELEMENTS(colour) EQ 3 AND s GE 1 AND s LE 5 THEN BEGIN
     IF MIN(colour) LT 0 OR MAX(colour) GT 255 THEN $
        MESSAGE,/CONTINUE,'Colour outside of byte range [0,255]. Limiting...'
     out =   BYTE( 0b > REFORM( colour, 3) < 255b )
  ENDIF ELSE BEGIN
     HELP, colour
     MESSAGE,'Unknown colour format', /CONTINUE
  ENDELSE

  RETURN, out

END
