; docformat = 'rst'
;
;
PRO FG_HELP, routine, HELP=help
;+
;
;  Launch the IDLdoc help page for the defined routine. If no routine
;  is specified then the index is opened. The index will be opened in
;  the default web browser.
;  
; :PARAMS:
;    routine: in, optional, type=string
;      The name of the routine for which help is required.
;      If no routine is provided, the index page is opened.
;
;
; :KEYWORDS:
;     help: in, optional, type=boolean
;           Load the help page for this routine.
;
; :AUTHOR:
;    Andy Smith  (smith [at] atm.ox.ac.uk / aja.smith [at] gmail.com)
;
;
; :HISTORY:
;    19 Jan 2015 (AJAS) Created.
;
;-

  ON_ERROR, 2

  IF KEYWORD_SET( help ) THEN BEGIN
     FG_HELP, 'fg_help'
     RETURN
  ENDIF

  ;; Get the path where this code is called from.
  ;; This should be in the same folder as the rest of
  ;; the FG code.
  st = (SCOPE_TRACEBACK(/STRUCTURE))
  
  ;; The final element of the scope traceback is the current routine.
  fg_path = FILE_DIRNAME(st[-1].filename)

  ;; If a routine is not defined, then we open the index.
  r = KEYWORD_SET( routine ) ? STRLOWCASE(routine[0]) : 'index'

  ;; Final help file to open. Should be the routine name + .html
  f = fg_path + '/doc/'+r+'.html'

  ;; Check that it exists.
  IF FILE_TEST( f ) THEN BEGIN
     ;; And spawn a browser.
     SPAWN, 'xdg-open '+f, listing, EXIT_STATUS=exit_status
  ENDIF ELSE exit_status=1

  IF exit_status NE 0 THEN $
     MESSAGE, 'Error opening '+f


  RETURN

END
