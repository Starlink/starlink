*+  PEEPSB - displays part of an image on a terminal

      SUBROUTINE PEEPSB ( INARR, LBND, DIM1, DIM2, XLL, YLL, XSIZE,
     :                    YSIZE, STATUS )
*
*    Description :
*
*     This routine displays a formatted listing of an XSIZE x YSIZE
*     section of an image on the terminal. The data are presented in a
*     pleasing format which depends on the value of the pixel, and the
*     number of pixels in each line. The lower left pixel of the box and
*     the box size are input parameters. The x size of the box is
*     constrained to be 9 or less pixels.
*
*    Invocation :
*
*     CALL PEEPSB( INARR, LBND, DIM1, DIM2, XLL, YLL, XSIZE, YSIZE,
*    :             STATUS )
*
*    Arguments :
*
*     INARR( DIM1, DIM2 )  =  REAL( READ )
*         The array which holds the image data
*     LBND( 2 ) = INTEGER ( READ )
*         Lower bounds of the array, thus the first element of the
*         array (1,1) is actually at (LBND(1),LBND(2)).
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     XLL  =  INTEGER( READ )
*         The x co-ordinate of the lower left pixel of the selected
*         region
*     YLL  =  INTEGER( READ )
*         The y co-ordinate of the lower left pixel of the selected
*         region
*     XSIZE  =  INTEGER( READ )
*         The x size of the box to be output
*     YSIZE  =  INTEGER( READ )
*         The y size of the box to be output
*     STATUS  =  INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Fix the x size of the box to be MAXNUM or less pixels
*     Output string to give x column index using CHR_ITOC
*     For all YSIZE lines in sub-array (in reverse order - top to
*      bottom)
*        Call CHR_ITOC to get string representing line index on left
*        For all XSIZE pixels in current line
*           If pixel lies off array then
*              Set up string to indicate this
*           Elseif pixel is invalid then
*              Set up string to indicate this
*           Else
*              Call CHR_RTOC to convert the current pixel value into
*               a pleasing string
*           Endif
*           Insert pixel string into correct position in current line
*             string
*        Endfor
*        Output current line string to terminal
*     Endfor
*     Return
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     11-06-1986 : First implementation - partly from KFH routine
*                  (REVA::MJM)
*     15-06-1986 : Generalised to give variable output box size
*                  (REVA::MJM)
*     1986 Aug 15: Renamed from PEEPSUB, tidied, nearly conformed to
*                  Starlink programming standards (RL.STAR::CUR).
*     1986 Sep 5 : Renamed parameters section to arguments, applied
*                  bad-pixel handling (RL.STAR::CUR).
*     1988 Jul 11: Parameterised line width and margin, and adjusted
*                  former to 78 from 70 (RL.STAR::CUR).
*     1989 Jul 27: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     1991 Jun 10 : Added LBND argument as temporary patch for INSPECT
*                   to use NDF (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE               ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'           ! SSE global parameters
      INCLUDE 'PRM_PAR'           ! PRIMDAT public constants

*    Import :

      INTEGER
     :    LBND( 2 ),
     :    DIM1, DIM2,
     :    XLL,
     :    YLL,
     :    XSIZE,
     :    YSIZE

      REAL
     :    INARR( DIM1, DIM2 )

*    Status :

      INTEGER STATUS

*    Local constants :

      INTEGER
     :    MARGIN,              ! width of margin to output line index
     :    LINLTH,              ! maximum number of characters in output
                               ! record
     :    MAXNUM,              ! maximum number of pixels across screen
     :    MAXSTR               ! maximum string length to hold formatted
                               ! pixel value
      PARAMETER( MARGIN  =  5 )
      PARAMETER( LINLTH  = 78 )
      PARAMETER( MAXNUM  =  9 )
      PARAMETER( MAXSTR  =  12 )

*    Local variables :

      INTEGER
     :    CURRX,               ! current x co-ord in sub-array
     :    CURRY,               !    "    y   "    "  "    "
     :    STRLEN,              ! length of string used for given format
     :    POINT,               ! pointer for indexing output string
     :    NCHAR,               ! dummy to hold return from CHR_RTOC
     :    I, J,                ! counter variables
     :    XSIZ                 ! constrained x size of the box

      REAL
     :    VALUE                ! value of current pixel in array

      CHARACTER*( MAXSTR )
     :    STRING,              ! string to hold output for current pixel
     :    XSTR,                ! string to hold column index
     :    YSTR                 !    "    "   "  line      "

      CHARACTER*( LINLTH )
     :    OUTLIN               ! string to hold output for current line

*-
*    check status on entry - return immediately if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    force the x size of the output array to be MAXNUM pixels across
*    at most

      XSIZ  =  MIN( MAXNUM, XSIZE )

*    work out the string length to be used for the requested number of x
*    pixels across the display, making sure the maximum output string
*    length for each line is fixed at LINLTH characters and leaving
*    MARGIN characters for the line index down the side

      STRLEN  =  MIN( MAXSTR, INT( ( LINLTH - MARGIN - XSIZ ) / XSIZ ) )

*    set the output line to be blanks initially

      OUTLIN  =  ' '

*    put a column index across the top line

      DO  I  =  1, XSIZ

*       Work out the current column.

         CURRX  =  XLL + I - 1

*       convert the current index to a left justified string of
*       fixed length 10 characters

         CALL CHR_ITOC( CURRX, XSTR, NCHAR )

*       work out where in the line output string we are - there are
*       blanks to be inserted at the beginning of the string

         POINT  =  ( ( I - 1 ) * ( STRLEN + 1 ) ) + 6

*       stick the current x index string in the right place in
*       the output string padding with one space to the right

         OUTLIN( POINT : POINT )  =  ' '
         OUTLIN( POINT + 1 : POINT + STRLEN )  =  XSTR( 1 : STRLEN )
      END DO

*    output the column index string

      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_OUT( 'COL_INDEX', OUTLIN, STATUS )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    loop round the XSIZ x YSIZE box outputting the relevant number or
*    symbols - first round all the lines in reverse order

      DO  J  =  1, YSIZE

*       clear the output string

         OUTLIN  =  ' '

*       Work out where the current line lies in the array including
*       the origin.  Work down from the top line to the bottom one.

         CURRY  =  YLL + YSIZE - J

*       convert the current line index into a string

         CALL CHR_ITOC( CURRY, YSTR, NCHAR )

*       Correct the line position from its true pixel index to its
*       index within the array supplied.

         CURRY  =  CURRY - LBND( 2 ) + 1

*       stick this string at the beginning of the output string

         OUTLIN( 2 : MARGIN )  =  YSTR( 1 : MARGIN-1 )

*       now loop round the XSIZ pixels in the current line

         DO  I  =  1, XSIZ

*          Work out where the current pixel lies in the line allowing
*          for the origin to give the pixel index with the array
*          supplied.

            CURRX  =  XLL + I - LBND( 1 )

*          now set up the appropriate string according to the current
*          pixel value - first check for a pixel not actually on the
*          array

            IF ( CURRX .LT. 1 .OR. CURRX .GT. DIM1 .OR.
     :           CURRY .LT. 1 .OR. CURRY .GT. DIM2 ) THEN

*             set up string to indicate off the array

               STRING  =  '............'

*          test if pixel is invalid

            ELSE IF ( INARR( CURRX, CURRY ) .EQ. VAL__BADR ) THEN

               STRING  =  'INVALID     '

*          else pixel is on array - format it nicely

            ELSE

*             get the current pixel value

               VALUE  =  INARR( CURRX, CURRY )

*             convert it into a string in a pleasing manner (!)

               CALL CHR_RTOC( VALUE, STRING( 1 : STRLEN ), NCHAR )

*          end of if-current-pixel-not-on-array check

            END IF

*          work out where to put the latest number in this line string

            POINT  =  ( ( I - 1 ) * ( STRLEN + 1 ) ) + 6

*          put the current number into the current line string, padding
*          with one additional space

            OUTLIN( POINT : POINT )  =  ' '
            OUTLIN( POINT + 1 : POINT + STRLEN )
     :                                      =  STRING ( 1 : STRLEN )

*       end of loop round pixels in current line

         END DO

*       output the current line string

         CALL MSG_OUT( 'CURRLINE', OUTLIN, STATUS )

*    end of loop round the lines

      END DO

*    output a blank line to finish

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

 999  CONTINUE

*    end and return

      END
