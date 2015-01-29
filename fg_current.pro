; docformat = 'rst'
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
 FUNCTION FG_CURRENT, id, HELP=help
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
; :KEYWORDS:
;   help: in, optional, type=boolean
;         Load the help page for this routine.
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
;    Create the same plot twice::
;
;        plot_object_0 = PLOT( [0,5] )
;        plot_object_0 = PLOT( [5,0] )
;
;
;    Now use `FG_CURRENT` to clear instead of creating a new one::
;
;        plot_object_1 = PLOT( [0,5], CURRENT=FG_CURRENT(plot_object_1) )
;        plot_object_1 = PLOT( [5,0], CURRENT=FG_CURRENT(plot_object_1) )
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
    ON_ERROR, 2

    IF KEYWORD_SET(help) THEN BEGIN
       FG_HELP, 'fg_current'
       RETURN, 0
    ENDIF
 
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
