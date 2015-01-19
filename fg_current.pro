; docformat = 'rst'
;
;
 FUNCTION FG_CURRENT, id
;+
;
;
;
;   Clear an object graphics window if it exists, so that re-running
;    code doesn't created another set of plots in a new window. The
;    function is designed to be passed directly to a plotting command
;    via the CURRENT keyword.
;
;
; :CATEGORIES:
;   Function graphics
;
; :PARAMS:
;   id: in, optional, type=objref
;       Object reference for a function graphics plot or window.
;
;
; :RETURNS:
;   Flag to be passed to plotting object. If the output is 1, then
;    the object has been successfully cleared and selected so that
;    the /CURRENT keyword should be correct.
;
;
; :EXAMPLES:
;
;   ;; Create the same plot twice.
;   plot_object_0 = PLOT( [0, 5] )
;   plot_object_0 = PLOT( [0, 5] )
;
;   ;; Now use FG_CURRENT to clear instead of creating a new one.
;   plot_object_1 = PLOT( [0,5], CURRENT=FG_CURRENT(plot_object_1) )
;   plot_object_1 = PLOT( [0,5], CURRENT=FG_CURRENT(plot_object_1) )
;
;
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;   
; :HISTORY:
;   17 FEB 2014 (AJAS) Created.
;
;   10 OCT 2014 (AJAS) More sensible test for FG object, using ISA().
;
;-
   
    ;; If there's an error, return current=0.
    CATCH, Error_status
    IF Error_status NE 0 THEN BEGIN
       PRINT, 'FG_CURRENT: ', !ERROR_STATE.MSG
       CATCH, /CANCEL
       RETURN, 0b
    ENDIF


    ;; Unless the current window is appropriately set,
    ;; we don't flag it.
    current = 0b

    ;; Check that we are dealing with an object reference to a graphic.
    IF ISA(id, 'GRAPHIC' ) THEN BEGIN

       ;; Get the window holding the graphic.
       wid = id.WINDOW
       ;; Clear it.
       wid.ERASE
       ;; Then make sure it's the currently selected window.
       wid.SELECT
       current = 1b

    ENDIF


    RETURN, current
 END
