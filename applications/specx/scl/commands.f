*  History:
*     03 Dec 1993 (hme):
*        Added CONVERT-VAX-FILE as COMMS(23).
*     30 Dec 1993 (rp)
*        Moved CONVERT-VAX-FILE to end (out of SCL block of commands)
*     03 March 1993 (timj)
*        Add SET-DATA-DIRECTORY
C-----------------------------------------------------------------------------

      BLOCK DATA COMMANDS

C   Sets up values of command strings

      INCLUDE 'COMMAND_TABLE'

      DATA NFUNC/178/
      DATA (COMMS(I),I=1,10) /
     &                        'IF',
     &                        'ELSEIF',
     &                        'ELSE',
     &                        'ENDIF',
     &                        'DO',
     &                        'BREAK',
     &                        'ENDDO',
     &                        'SET-JOURNAL',
     &                        'SHOW-SYMBOLS',
     &                        'SHOW-VARIABLES'/
      DATA (COMMS(I),I=11,20) /
     &                        'PRINT',
     &                        'DECLARE',
     &                        'ASK',
     &                        'RETURN',
     &                        'SET-ERROR-RETURN',
     &                        '$',
     &                        '@',
     &                        '=',
     &                        ':=',
     &                        'WRITE'/
      DATA (COMMS(I),I=21,30)/
     &                        'READ',
     &                        'LABEL',
     &                        'GO-TO',
     &                         6*' ',
     &                        'HELP'/
      DATA (COMMS(I),I=31,40)/
     &                        'PAUSE',
     &                        'DIAGNOSTICS',
     &                        'SHOW-NEWS',
     &                        'SHOW-COMMANDS',
     &                        'SET-DUMP',
     &                        'RESTART',
     &                        'DUMP',
     &                        'EXTERNAL-1',
     &                        'EXTERNAL-2',
     &                        'EXTERNAL-3'/
      DATA (COMMS(I),I=41,50)/
     &                        'EXTERNAL-4',
     &                        'EXTERNAL-5',
     &                        'EXTERNAL-6',
     &                        'EXTERNAL-7',
     &                        'EXTERNAL-8',
     &                        'EXTERNAL-9',
     &                        'EXTERNAL-10',
     &                        'EXIT',
     &                        'INITIALIZE-PARAMETERS',
     &                        'SET-SITE-PARAMETERS'/
      DATA (COMMS(I),I=51,60)/
     &                        'OPEN-FILE',
     &                        'CLOSE-FILE',
     &                        'SET-FILE-ACCESS',
     &                        'INDEX-FILE',
     &                        'DELETE-SPECTRUM-FROM-FILE',
     &                        'COMPRESS-FILE',
     &                        'LIST-OPEN-FILES',
     &                        'RECOVER-FILE',
     &                        'EDIT-FILE-HEADER',
     &                        'READ-SPECTRUM'/
      DATA (COMMS(I),I=61,70)/
     &                        'WRITE-SPECTRUM',
     &                        'REWRITE-SPECTRUM',
     &                        'WRITE-ASCII-SPECTRUM',
     &                        'SET-LIST-FILE',
     &                        'LIST-SPECTRUM',
     &                        'PRINT-SPECTRUM-HEADER',
     &                        'SET-TERMINAL-DEVICE',
     &                        'SET-HARDCOPY-DEVICE',
     &                        'CHANGE-SIDEBAND',
     &                        'ADD-SPECTRA'/
      DATA (COMMS(I),I=71,80)/
     &                        'SUBTRACT-SPECTRA',
     &                        'MULTIPLY-SPECTRUM',
     &                        'DIVIDE-SPECTRUM',
     &                        'AVERAGE-SPECTRA',
     &                        'FORM-QUOTIENT-SPECTRUM',
     &                        'SET-STACK',
     &                        'CLEAR-STACK',
     &                        'ROLL-STACK',
     &                        'XY-INTERCHANGE',
     &                        'PUSH-STACK-UP'/
      DATA (COMMS(I),I=81,90)/
     &                        'POP-STACK-DOWN',
     &                        'SHOW-STACK',
     &                        'STORE-SPECTRUM',
     &                        'RECALL-SPECTRUM',
     &                        'SHOW-STORE-REGISTERS',
     &                        'REMOVE-LINEAR-BASELINE',
     &                        'FIT-POLYNOMIAL-BASELINE',
     &                        'FIT-COMPOSITE-BASELINE',
     &                        'FIT-GAUSSIAN-MODEL',
     &                        'ENTER-GAUSSIAN-MODEL'/
      DATA (COMMS(I),I=91,100)/
     &                        'CALCULATE-GAUSSIAN-MODEL',
     &                        'SHOW-GAUSSIAN-MODEL',
     &                        'READ-GSD-DATA',
     &                        'INDEX-GSD-FILES',
     &                        'READ-GSD-MAP',
     &                        'SET-GSD-FILENAME',
     &                        'SET-INTERACTIVE',
     &                        'SET-PLOT-DEVICE',
     &                        'SET-PLOT-PARAMETERS',
     &                        'SET-PLOT-SCALES'/
      DATA (COMMS(I),I=101,110)/
     &                        'NEW-PLOT',
     &                        'OVERLAY-SPECTRUM',
     &                        'SEE-PLOT',
     &                        'DRAW-PLOT-USING-CURSOR',
     &                        'DELETE-LAST-PLOT',
     &                        'SET-PLOT-SIZE',
     &                        'CLOSE-PLOT',
     &                        'SMOOTH-SPECTRUM',
     &                        'HANN-SPECTRUM',
     &                        'CONVOLVE-SPECTRUM'/
      DATA (COMMS(I),I=111,120)/
     &                        'TRUNCATE-SPECTRUM',
     &                        'SHIFT-SPECTRUM',
     &                        'SET-CHANNELS',
     &                        'DIFFERENTIATE-SPECTRUM',
     &                        'SLIDE-QUADRANT',
     &                        'FOURIER-TRANSFORM',
     &                        'FOURIER-POWER-SPECTRUM',
     &                        'OFFSET-SPECTRUM',
     &                        'BIN-SPECTRUM',
     &                        'REMOVE-SPIKES'/
      DATA (COMMS(I),I=121,130)/
     &                        'INVERT-SPECTRUM',
     &                        'FOLD-SPECTRUM',
     &                        'DROP-CHANNELS',
     &                        'REGRID-SPECTRUM',
     &                        'SET-X-SCALE',
     &                        'SET-LINE-REST-FREQ',
     &                        'SHOW-X-SCALE',
     &                        'FIND-SPECTRUM-STATISTICS',
     &                        'FIND-MAXIMUM',
     &                        'FIND-CENTROID'/
      DATA (COMMS(I),I=131,140)/
     &                        'FIND-LINE-WIDTH',
     &                        'FIND-AZEL',
     &                        'FIND-INTEGRATED-INTENSITY',
     &                        'FIND-MOMENTS',
     &                        'FIND-SKEWNESS',
     &                        'SET-QUADRANT-DISPLAY',
     &                        'MERGE-QUADRANTS',
     &                        'EXTRACT-QUADRANT',
     &                        'OPEN-MAP-FILE',
     &                        'ADD-TO-MAP'/
      DATA (COMMS(I),I=141,150)/
     &                        'GET-SPECTRUM-FROM-MAP',
     &                        'LIST-MAP',
     &                        'INTERPOLATE-MAP',
     &                        'CHANGE-MAP',
     &                        'CLOSE-MAP',
     &                        'CONTOUR-MAP',
     &                        'CHANNEL-MAPS',
     &                        'GRAYSCALE-MAP',
     &                        'GREYSCALE-MAP',
     &                        'GRID-SPECTRA'/
      DATA (COMMS(I),I=151,160)/
     &                        'PLOT-LINE-PARAMETERS',
     &                        'SET-MAP-SCALES',
     &                        'SET-MAP-SIZE',
     &                        'SET-CONTOUR-LEVELS',
     &                        'SET-GREYSCALE',
     &                        'SET-GRAYSCALE',
     &                        'SET-MAP-PARAMETERS',
     &                        'SET-MAP-ACCEPT',
     &                        'DELETE-FROM-MAP',
     &                        'ROTATE-MAP'/
      DATA (COMMS(I),I=161,170)/
     &                        'WRITE-ASCII-MAP',
     &                        'WRITE-GILDAS-IMAGE',
     &                        'OPEN-FITS-FILE',
     &                        'READ-FITS-SPECTRUM',
     &                        'WRITE-FITS-SPECTRUM',
     &                        'WRITE-FITS-MAP',
     &                        'CLOSE-FITS-FILE',
     &                        'SET-VELOCITY-FRAME',
     &                        'CLIP-SPECTRUM',
     &                        'CONCATENATE-SPECTRA'/
      DATA (COMMS(I),I=171,180)/
     &                        'CONVERT-VAX-FILE',
     &                        'CONVERT-VAX-MAP',
     &                        'WRITE-FITS-CUBE',
     &                        'INFO-FILE',
     &			      'READ-GSD-RASTER',
     &                        'DAS-MERGE',
     &                        'MERGE-FILES',
     &                        'SET-DATA-DIRECTORY',
     &                         2*' '/

C  Rest of symbol table

      DATA (COMMS(I),I=181,256) /76*' '/

      END

C-----------------------------------------------------------------------------
