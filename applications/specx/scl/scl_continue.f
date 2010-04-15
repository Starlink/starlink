*  History:
*     15 Nov 1993 (hme):
*        Added () to LOGICAL*4 FUNCTION SCL_CONTINUE().
*     30 Nov 1993 (hme):
*        Hard code the error messages and severities for each error
*        code, instead of reading from an unformatted file.
*        Change the A<IM> descriptor to A with default width.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION SCL_CONTINUE ()

*  Function to determine whether error should be terminal in the current
*  context. If not terminal, resets the status to zero (good)

      IMPLICIT  NONE

*     Common

      INCLUDE 'ERRORS'

*  Ok? Go..

      ERROR_SET      = .FALSE.
      SCL_CONTINUE = .TRUE.

      IF (ERROR.NE.0) THEN

        LAST_ERROR =  ERROR
        CALL ERRMESS (ERROR, SEVERITY)

        IF (SEVERITY.GT.MAX_OK_ERROR) THEN
          SCL_CONTINUE = .FALSE.
        ELSE
          IF (SEVERITY.NE.1) CALL CLI_EMPTY
          ERROR = 0
        END IF

        ERROR_SET  =.TRUE.
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE ERRMESS (IERR,ISEV)

C  Routine to print out SPECX error messages and return severity.

C  The error messages are held in the file SPXDEF.UNF which is
C  produced by running the program Error_compile with the text file
C  SPXDEF.DAT as input. The messages can be changed at any time
C  by amending the text file and rerunning Error_compile, then rename
C  the result fine SPXDEF.UNF into SYS_SPECX:

      INTEGER*4 ISEV
      CHARACTER MESS*80
      CHARACTER SCHAR*4
      DATA      SCHAR /'IWEF' /

      INTEGER*4 GEN_ILEN

*  Ok, now go do it...

      IF (IERR.EQ.0) THEN
        ISEV = 0
        RETURN
      END IF

      LUN = 6

CD    PRINT *, '--- errmess ---'
CD    PRINT *, '    error # = ', IERR

      IF ( IERR .EQ.  1 ) THEN
         ISEV = 3
         MESS = 'No file opened with required access'
      ELSE IF ( IERR .EQ.  2 ) THEN
         ISEV = 2
         MESS = 'No such scan in file'
      ELSE IF ( IERR .EQ.  3 ) THEN
         ISEV = 3
         MESS = 'Inappropriate access set - use S-F-A'
      ELSE IF ( IERR .EQ.  4 ) THEN
         ISEV = 2
         MESS = 'No room in file - scan not inserted'
      ELSE IF ( IERR .EQ.  5 ) THEN
         ISEV = 3
         MESS = 'Insufficient points in scan'
      ELSE IF ( IERR .EQ.  6 ) THEN
         ISEV = 3
         MESS = 'Trouble getting logical unit for file'
      ELSE IF ( IERR .EQ.  7 ) THEN
         ISEV = 1
         MESS = 'No plot ready - use NEW-PLOT'
      ELSE IF ( IERR .EQ.  8 ) THEN
         ISEV = 3
         MESS = 'Not enough spectra in stack for this command'
      ELSE IF ( IERR .EQ.  9 ) THEN
         ISEV = 2
         MESS = 'Baseline removal unsuccessful'
      ELSE IF ( IERR .EQ. 10 ) THEN
         ISEV = 2
         MESS = 'Error opening file'
      ELSE IF ( IERR .EQ. 11 ) THEN
         ISEV = 2
         MESS = 'File already open'
      ELSE IF ( IERR .EQ. 12 ) THEN
         ISEV = 3
         MESS = 'Ambiguous file name'
      ELSE IF ( IERR .EQ. 13 ) THEN
         ISEV = 4
         MESS = 'Error reading from input file or terminal'
      ELSE IF ( IERR .EQ. 14 ) THEN
         ISEV = 4
         MESS = 'No default available'
      ELSE IF ( IERR .EQ. 15 ) THEN
         ISEV = 3
         MESS = 'Error forming quotient spectrum'
      ELSE IF ( IERR .EQ. 16 ) THEN
         ISEV = 3
         MESS = 'Illegal value - start again'
      ELSE IF ( IERR .EQ. 17 ) THEN
         ISEV = 3
         MESS = 'Unequal number of points'
      ELSE IF ( IERR .EQ. 18 ) THEN
         ISEV = 4
         MESS = 'Unknown error'
      ELSE IF ( IERR .EQ. 19 ) THEN
         ISEV = 3
         MESS = 'Invalid points'
      ELSE IF ( IERR .EQ. 20 ) THEN
         ISEV = 3
         MESS = 'No such file open'
      ELSE IF ( IERR .EQ. 21 ) THEN
         ISEV = 3
         MESS = 'Not a valid option'
      ELSE IF ( IERR .EQ. 22 ) THEN
         ISEV = 3
         MESS = 'Option cannot be repeated'
      ELSE IF ( IERR .EQ. 23 ) THEN
         ISEV = 3
         MESS = 'Not valid for multi-quadrant data: use EXTRACT-QUAD'
      ELSE IF ( IERR .EQ. 24 ) THEN
         ISEV = 2
         MESS = 'Scan too long for this file position - not inserted'
      ELSE IF ( IERR .EQ. 25 ) THEN
         ISEV = 2
         MESS = 'Stack registers too short for this scan - not read'
      ELSE IF ( IERR .EQ. 26 ) THEN
         ISEV = 2
         MESS = 'No points in range of X-axis or range < 1 point'
      ELSE IF ( IERR .EQ. 27 ) THEN
         ISEV = 3
         MESS = 'Insufficient quadrants in one spectrum'
      ELSE IF ( IERR .EQ. 28 ) THEN
         ISEV = 3
         MESS = 'No such quadrant'
      ELSE IF ( IERR .EQ. 29 ) THEN
         ISEV = 3
         MESS = 'This quadrant is not masked for processing/display'
      ELSE IF ( IERR .EQ. 30 ) THEN
         ISEV = 2
         MESS = 'Not suitable for MERGE - not done'
      ELSE IF ( IERR .EQ. 31 ) THEN
         ISEV = 2
         MESS = 'Can''t MERGE - only 1 quadrant'
      ELSE IF ( IERR .EQ. 32 ) THEN
         ISEV = 2
         MESS = 'Can''t MERGE - different parameters'
      ELSE IF ( IERR .EQ. 33 ) THEN
         ISEV = 2
         MESS = 'Can''t MERGE - quadrants do not overlap'
      ELSE IF ( IERR .EQ. 34 ) THEN
         ISEV = 2
         MESS = 'Can''t SLIDE - odd number of points'
      ELSE IF ( IERR .EQ. 35 ) THEN
         ISEV = 3
         MESS = 'Attempt to divide by zero'
      ELSE IF ( IERR .EQ. 36 ) THEN
         ISEV = 4
         MESS = 'Trouble reading GSD header'
      ELSE IF ( IERR .EQ. 37 ) THEN
         ISEV = 4
         MESS = 'Trouble listing GSD header'
      ELSE IF ( IERR .EQ. 38 ) THEN
         ISEV = 3
         MESS = 'Error reading file'
      ELSE IF ( IERR .EQ. 39 ) THEN
         ISEV = 3
         MESS = 'Not a valid string'
      ELSE IF ( IERR .EQ. 40 ) THEN
         ISEV = 3
         MESS = 'Error opening plot file'
      ELSE IF ( IERR .EQ. 41 ) THEN
         ISEV = 2
         MESS = 'Range zero for plot'
      ELSE IF ( IERR .EQ. 42 ) THEN
         ISEV = 1
         MESS = 'No contours plotted: SET-CONTOURS or INTERPOLATE-MAP?'
      ELSE IF ( IERR .EQ. 43 ) THEN
         ISEV = 2
         MESS = 'No dump file found'
      ELSE IF ( IERR .EQ. 44 ) THEN
         ISEV = 2
         MESS = 'Trouble editing header - try again!'
      ELSE IF ( IERR .EQ. 45 ) THEN
         ISEV = 4
         MESS = 'Command file execution halted'
      ELSE IF ( IERR .EQ. 46 ) THEN
         ISEV = 3
         MESS = 'Not plotting to terminal or interactive mode ' //
     :      'not enabled - use SET-INT'
      ELSE IF ( IERR .EQ. 47 ) THEN
         ISEV = 3
         MESS = 'Maximum radius for interpolating function ' //
     :      'exceeds 10 pixels - try again'
      ELSE IF ( IERR .EQ. 48 ) THEN
         ISEV = 3
         MESS = 'Error establishing plotting window for map'
      ELSE IF ( IERR .EQ. 49 ) THEN
         ISEV = 3
         MESS = 'Map has only one pixel in one or more dimensions'
      ELSE IF ( IERR .EQ. 50 ) THEN
         ISEV = 3
         MESS = 'Too many undetermined parameters'
      ELSE IF ( IERR .EQ. 51 ) THEN
         ISEV = 2
         MESS = 'Virtual memory problems - If they continue exit ' //
     :      'SPECX and restart'
      ELSE IF ( IERR .EQ. 52 ) THEN
         ISEV = 2
         MESS = 'No map open - use OPEN-MAP-FILE'
      ELSE IF ( IERR .EQ. 53 ) THEN
         ISEV = 2
         MESS = 'Plotting window has zero extent in one dimension'
      ELSE IF ( IERR .EQ. 54 ) THEN
         ISEV = 2
         MESS = 'Plotting window extends too far outside data cube'
      ELSE IF ( IERR .EQ. 55 ) THEN
         ISEV = 1
         MESS = 'Map file already exists'
      ELSE IF ( IERR .EQ. 56 ) THEN
         ISEV = 1
         MESS = 'Position outside map boundary or too far from ' //
     :      'sample point'
      ELSE IF ( IERR .EQ. 57 ) THEN
         ISEV = 2
         MESS = 'Attempt to extend map'
      ELSE IF ( IERR .EQ. 58 ) THEN
         ISEV = 1
         MESS = 'Duplicate spectrum'
      ELSE IF ( IERR .EQ. 59 ) THEN
         ISEV = 3
         MESS = 'Negative index position calculated'
      ELSE IF ( IERR .EQ. 60 ) THEN
         ISEV = 3
         MESS = 'Insufficient spectra in map file'
      ELSE IF ( IERR .EQ. 61 ) THEN
         ISEV = 3
         MESS = 'Map file not found'
      ELSE IF ( IERR .EQ. 62 ) THEN
         ISEV = 4
         MESS = 'Other error opening map file'
      ELSE IF ( IERR .EQ. 63 ) THEN
         ISEV = 4
         MESS = 'Map axes not selected - use SET-MAP-SCALES'
      ELSE IF ( IERR .EQ. 64 ) THEN
         ISEV = 2
         MESS = 'No measured data points in range of map'
      ELSE IF ( IERR .EQ. 65 ) THEN
         ISEV = 3
         MESS = '2D map file (mapplane.tmp) not found -- ' //
     :      'need to MAKE-MAP?'
      ELSE IF ( IERR .EQ. 66 ) THEN
         ISEV = 3
         MESS = 'Trouble opening 2D map file (mapplane.tmp)'
      ELSE IF ( IERR .EQ. 67 ) THEN
         ISEV = 3
         MESS = 'Trouble mapping 2D map file (mapplane.tmp) ' //
     :      'to virtual memory'
      ELSE IF ( IERR .EQ. 68 ) THEN
         ISEV = 3
         MESS = 'Cube not resident in memory;  ' //
     :      'OPEN-MAP and interpolate if necessary'
      ELSE IF ( IERR .EQ. 69 ) THEN
         ISEV = 1
         MESS = 'Map already interpolated? ' //
     :      '(no raw points in cube). OPEN-MAP and try again.'
      ELSE IF ( IERR .EQ. 70 ) THEN
         ISEV = 1
         MESS = 'Data too far from map sample point.'
      ELSE IF ( IERR .EQ. 71 ) THEN
         ISEV = 3
         MESS = 'Illegal value(s): Amplitude = 0 or Width .le. 0'
      ELSE IF ( IERR .EQ. 72 ) THEN
         ISEV = 1
         MESS = 'Map already rotated & interpolated: OPEN-MAP ' //
     :      'and try again.'
      ELSE IF ( IERR .EQ. 73 ) THEN
         ISEV = 3
         MESS = 'No plot-device with requested name.'
      ELSE IF ( IERR .EQ. 74 ) THEN
         ISEV = 3
         MESS = 'Defaults not allowed!'
      ELSE IF ( IERR .EQ. 75 ) THEN
         ISEV = 1
         MESS = 'Null plot device selected; no plot made.'
      ELSE IF ( IERR .EQ. 76 ) THEN
         ISEV = 1
         MESS = 'Error nesting IFs - no IF structure active'
      ELSE IF ( IERR .EQ. 77 ) THEN
         ISEV = 1
         MESS = 'Cannot use IF/ELSEIF/ELSE/ENDIF at command level'
      ELSE IF ( IERR .EQ. 78 ) THEN
         ISEV = 4
         MESS = 'IF-stack overflow: Reduce # of nested IFs'
      ELSE IF ( IERR .EQ. 79 ) THEN
         ISEV = 4
         MESS = 'Error pulling from IF-stack: Consult RP'
      ELSE IF ( IERR .EQ. 80 ) THEN
         ISEV = 4
         MESS = 'Do variable must be of integer type'
      ELSE IF ( IERR .EQ. 81 ) THEN
         ISEV = 3
         MESS = 'Command not found'
      ELSE IF ( IERR .EQ. 82 ) THEN
         ISEV = 3
         MESS = 'Ambiguous command name'
      ELSE IF ( IERR .EQ. 83 ) THEN
         ISEV = 3
         MESS = 'Command line error'
      ELSE IF ( IERR .EQ. 84 ) THEN
         ISEV = 4
         MESS = 'Unable to open command file'
      ELSE IF ( IERR .EQ. 85 ) THEN
         ISEV = 4
         MESS = 'Error reading numeric value from string'
      ELSE IF ( IERR .EQ. 86 ) THEN
         ISEV = 2
         MESS = 'Command not yet available'
      ELSE IF ( IERR .EQ. 87 ) THEN
         ISEV = 4
         MESS = '# data points in spectrum does not match that of map'
      ELSE IF ( IERR .EQ. 88 ) THEN
         ISEV = 4
         MESS = 'Error opening plot device'
      ELSE IF ( IERR .EQ. 89 ) THEN
         ISEV = 3
         MESS = 'Spectrum at this position previously deleted - no data'
      ELSE IF ( IERR .EQ. 90 ) THEN
         ISEV = 2
         MESS = 'User symbol memory exceeded'
      ELSE IF ( IERR .EQ. 95 ) THEN
         ISEV = 3
         MESS = 'No symbol table installed'
      ELSE IF ( IERR .EQ. 98 ) THEN
         ISEV = 4
         MESS = 'Must be a string variable'
      ELSE IF ( IERR .EQ. 99 ) THEN
         ISEV = 3
         MESS = 'String variable must be .le. 128 characters'
      ELSE IF ( IERR .EQ.100 ) THEN
         ISEV = 3
         MESS = 'Variable not declared'
      ELSE IF ( IERR .EQ.101 ) THEN
         ISEV = 1
         MESS = 'Variable already defined'
      ELSE IF ( IERR .EQ.102 ) THEN
         ISEV = 3
         MESS = 'Variable is readonly'
      ELSE IF ( IERR .EQ.103 ) THEN
         ISEV = 3
         MESS = 'Error setting variable in SPECX_SET_VALUE'
      ELSE IF ( IERR .EQ.104 ) THEN
         ISEV = 3
         MESS = 'Variable table full'
      ELSE IF ( IERR .EQ.105 ) THEN
         ISEV = 3
         MESS = 'Not a valid symbol name'
      ELSE IF ( IERR .EQ.106 ) THEN
         ISEV = 3
         MESS = 'Variable hash table is full'
      ELSE IF ( IERR .EQ.107 ) THEN
         ISEV = 3
         MESS = 'internal stack overflow: simplify expression'
      ELSE IF ( IERR .EQ.108 ) THEN
         ISEV = 3
         MESS = 'cannot translate string variable'
      ELSE IF ( IERR .EQ.109 ) THEN
         ISEV = 2
         MESS = 'error in PRINT command'
      ELSE IF ( IERR .EQ.110 ) THEN
         ISEV = 4
         MESS = 'Only available for position-position maps'
      ELSE IF ( IERR .EQ.111 ) THEN
         ISEV = 3
         MESS = 'No FITS output file open --- use OPEN-FITS'
      ELSE IF ( IERR .EQ.112 ) THEN
         ISEV = 2
         MESS = 'FITS output file already open'
      ELSE IF ( IERR .EQ.113 ) THEN
         ISEV = 4
         MESS = 'Too many sectors in resultant spectrum'
      ELSE IF ( IERR .EQ.114 ) THEN
         ISEV = 4
         MESS = 'Too many points in resultant spectrum'
      END IF

      IM = GEN_ILEN (MESS)

      WRITE (LUN, '('' -- SPECX#'', I3.3, '' -'', A1, ''- '','
     &        //  'A, '' --'')') IERR, SCHAR(ISEV:ISEV), MESS(:IM)

      RETURN
      END
