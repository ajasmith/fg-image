; docformat = 'rst'
;
; :NAME:
;   IP_GET_POSITION
;
 FUNCTION IP_GET_POSITION, flag_cb, layout, plot_position_in, cbar_position_in
;+
;
;
; Get the normalised position in a function graphics window that will include
;  the main plot, colour bar, and missing value mark for an IMAGE_PLOT_F call.
;
; This has been shunted into a seperate routine so that other routines that
; might use `IMAGE_PLOT_F` can obtain the dimensions of a window without
; having to make a fake plot and then delete it.
;
;
; :CATEGORIES:
;    Function graphics, image_plot
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;
; :HISTORY:
;
;      05 MAR 2014 (AJAS) Created by moving code from `IMAGE_PLOT_F`.
;-

    CASE (flag_cb MOD 4) OF
       0: BEGIN ;; No colourbar
          plot_position = [0.15, 0.15, 0.96, 0.91]
          cbar_position = [0.00, 0.00, 0.00, 0.00]
          flag_cb = 0
       END
       1: BEGIN ;; Sidebar
          plot_position = [0.34, 0.10, 0.96, 0.91]
          cbar_position = [0.14, 0.10, 0.19, 0.91]
       END
       2: BEGIN ;; Colourbar
          plot_position = [0.10, 0.27, 0.96, 0.91]
          cbar_position = [0.10, 0.12, 0.96, 0.17]
       END
       ELSE: MESSAGE,'/COLOURBAR and /SIDEBAR cannot both be set.'
    ENDCASE

    ;; And then override if necessary.
    IF N_ELEMENTS( plot_position_in ) EQ 4 THEN plot_position=plot_position_in
    IF N_ELEMENTS( cbar_position_in ) EQ 4 THEN cbar_position=cbar_position_in
    
    ;; If there is a colourbar, then the missing key can be set. We alter 
    ;; the colourbar position so that the bottom / left-most 6% contains
    ;; this key.
    miss_position = cbar_position
    IF (flag_cb AND 4) EQ 4 THEN BEGIN

       IF (flag_cb AND 1) EQ 1 THEN BEGIN ; Sidebar
          miss_position[3] = cbar_position[3]*0.06 + cbar_position[1]*0.94
          cbar_position[1] = cbar_position[3]*0.12 + cbar_position[1]*0.88
       ENDIF ELSE BEGIN ; Colourbar
          miss_position[2] = cbar_position[2]*0.06 + cbar_position[0]*0.94
          cbar_position[0] = cbar_position[2]*0.12 + cbar_position[0]*0.88
       ENDELSE

    ENDIF



    ;; Position overrides the layout keyword, so we need to account for this.
    IF KEYWORD_SET( layout ) THEN BEGIN
       IF N_ELEMENTS( layout ) NE 3 THEN MESSAGE,'LAYOUT must have 3 elements.'
       
       ;; Now deal with layout issues... (Make position relative to layout)
       lx = LONG( layout[0] ) > 1
       ly = LONG( layout[1] ) > 1
       ln = LONG( layout[2] )

       IF ln GT lx*ly OR ln LT 1 THEN MESSAGE,'LAYOUT index outside bounds'

       ;; Bottom left corner of individual plot is [lxpos, lypos]
       lxpos = ( (ln-1) MOD lx ) / FLOAT( lx )
       lypos = 1 - ( ((ln-1)  /  lx) + 1 ) / FLOAT( ly )

       ;; Move from relative to absolute positions.
       plot_position[0] = plot_position[0] / FLOAT (lx) + lxpos
       plot_position[1] = plot_position[1] / FLOAT (ly) + lypos
       plot_position[2] = plot_position[2] / FLOAT (lx) + lxpos
       plot_position[3] = plot_position[3] / FLOAT (ly) + lypos
       
       cbar_position[0] = cbar_position[0] / FLOAT (lx) + lxpos
       cbar_position[1] = cbar_position[1] / FLOAT (ly) + lypos
       cbar_position[2] = cbar_position[2] / FLOAT (lx) + lxpos
       cbar_position[3] = cbar_position[3] / FLOAT (ly) + lypos

       miss_position[0] = miss_position[0] / FLOAT (lx) + lxpos
       miss_position[1] = miss_position[1] / FLOAT (ly) + lypos
       miss_position[2] = miss_position[2] / FLOAT (lx) + lxpos
       miss_position[3] = miss_position[3] / FLOAT (ly) + lypos

    ENDIF

    RETURN, {plot:plot_position, $
             colourbar:cbar_position, $
             missing:miss_position}


END
