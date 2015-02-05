; docformat = 'rst'
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
;    30 Jan 2015 (AJAS) Made paths platform independent using `FILEPATH` 
;                       and added spawning in OSX and Windows (untested).
;
;
; :REQUIRES:
;    8.0
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
  f = FILEPATH(r+'.html', ROOT_DIR=fg_path, SUBDIRECTORY=['doc'] )


  ;; Check that it exists.
  IF FILE_TEST( f ) THEN BEGIN
     ;; And spawn a browser.
     CASE STRLOWCASE(STRMID(!VERSION.os,0,3)) OF
        'lin': SPAWN, 'xdg-open '+f, listing, EXIT_STATUS=exit_status
        'dar': SPAWN, 'open -a safari '+f, listing, EXIT_STATUS=exit_status
        'win': SPAWN, 'start "" "'+f+'"', listing, EXIT_STATUS=exit_status
        ELSE: MESSAGE, 'Unknown architecture.'
     ENDCASE
  ENDIF ELSE exit_status=1

  IF exit_status NE 0 THEN $
     MESSAGE, 'Error opening '+f


  RETURN

END
