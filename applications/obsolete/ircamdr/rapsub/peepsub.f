
*+  PEEPSUB - displays part of an image on a terminal

      SUBROUTINE PEEPSUB ( ARRAY, DIMS1, DIMS2, XLL, YLL, XSIZE,
     :                     YSIZE, STATUS )

*    Description :
*
*     This routine displays a formatted listing of an XSIZE x YSIZE section
*     of an images on the terminal. The data is is presented in a pleasing
*     format which depends on the value of the pixel, and the number of pixels
*     in each row. The lower left pixel of the box and the box size are input
*     parameters. The x size of the box is constrained to be 8 or less pixels.
*
*    Invocation :
*
*     CALL PEEPSUB( ARRAY, DIMS, XLL, YLL, XSIZE, YSIZE, STATUS )
*
*    Parameters :
*
*     ARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( READ )
*           The array which holds the image data
*     DIMS( 2 )  =  INTEGER( READ )
*           The dimensions of the image
*     XLL  =  INTEGER( READ )
*           The x coordinate of the lower left pixel of the selected region
*     YLL  =  INTEGER( READ )
*           The y coordinate of the lower left pixel of the selected region
*     XSIZE  =  INTEGER( READ )
*           The x size of the box to be output
*     YSIZE  =  INTEGER( READ )
*           The y size of the box to be output
*     STATUS  =  INTEGER( READ, WRITE )
*           Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Fix the x size of the box to be MAXNUM or less pixels - call it DXSIZE
*     Output string to give x column index using CHR_ITOC
*     For all YSIZE rows in sub-array (in reverse order - top to bottom)
*        Call CHR_ITOC to get string representing row index on left
*        For all DXSIZE pixels in current row
*           If pixel lies off array then
*              Set up string to indicate this
*           Else
*              Call CHR_RTOC to convert the current pixel value into
*               a pleasing string
*           Endif
*           Insert pixel string into correct position in current row string
*        Endfor
*        Output current row string to terminal
*     Endfor
*     Return
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     11-06-1986 : First implementation - partly from KFH routine (REVA::MJM)
*     15-06-1986 : Generalised to give variable output box size (REVA::MJM)
*     24-11-1986 : Use internal variable to constrain size of box in x
*                : direction, rather than updating input XSIZE (HILO::MJM)
*     15-AUG-1994  Changed DIM arguments so routine will compile (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE                  ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'              ! SSE global parameters

*    Import :

      INTEGER
     :    DIMS1, DIMS2,              ! dimensions of input array
     :    XLL,                       ! x coord of lower left of box
     :    YLL,                       ! y   "    "   "     "   "  "
     :    XSIZE,                     ! x size of box to be output
     :    YSIZE                      ! y   "   "  "   "  "    "

      REAL
     :    ARRAY( DIMS1, DIMS2 )   ! array containing data

*    Status :

      INTEGER STATUS                 ! global status parameter

*    Local constants :

      INTEGER
     :    MAXNUM,                    ! maximum number of pixels across screen
     :    MAXSTR                     ! maximum string length to hold formatted
                                     ! pixel value
      PARAMETER( MAXNUM  =  9 )
      PARAMETER( MAXSTR  =  12 )

*    Local variables :

      INTEGER
     :    DXSIZE,                    ! dummy variable to hold x size of box
     :    CURRX,                     ! current x coord in sub-array
     :    CURRY,                     !    "    y   "    "  "    "
     :    STRLEN,                    ! length of string used for given format
     :    POINT,                     ! pointer for indexing output string
     :    NCHAR,                     ! dummy to hold return from CHR_RTOC
     :    I, J                       ! counter variables

      REAL
     :    VALUE                      ! value of current pixel in array

      CHARACTER*( MAXSTR )
     :    STRING,                    ! string to hold output for current pixel
     :    XSTR,                      ! string to hold column index
     :    YSTR                       !    "    "   "  row      "

      CHARACTER*70
     :    OUTLINE                    ! string to hold output for current row

*-
*    check status on entry - return immediately if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF


*    force the x size of the output array to be MAXNUM pixels across at most -
*    use a dummy variable instead of updating the input argument XSIZE
      DXSIZE  =  MIN( MAXNUM, XSIZE )

*    work out the string length to be used for the requested number of x
*    pixels across the display, making sure the maximum output string
*    length for each row is fixed at 70 characters and leaving 5 spaces
*    for the row index down the side
      STRLEN  =  MIN( MAXSTR, INT( ( 65 - DXSIZE ) / DXSIZE ) )

*    set the output line to be blanks initially
      OUTLINE  =  ' '

*    put a column index across the top row
      DO  I  =  1, DXSIZE

*       work out the current column
         CURRX  =  XLL + I - 1

*       convert the current index to a left justified string of
*       fixed length 10 characters
         CALL CHR_ITOC( CURRX, XSTR, NCHAR )

*       work out where in the line output string we are - there are blanks
*       to be inserted at the beginning of the string
         POINT  =  ( ( I - 1 ) * ( STRLEN + 1 ) ) + 6

*       stick the current x index string in the right place in
*       the output string padding with one space to the right
         OUTLINE( POINT : POINT )  =  ' '
         OUTLINE( POINT + 1 : POINT + STRLEN )  =  XSTR( 1 : STRLEN )

      END DO

*    output the column index string
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_OUT( 'COL_INDEX', OUTLINE, STATUS )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    loop round the DXSIZE x YSIZE box outputting the relevant number or
*    symbols - first round all the rows in reverse order
      DO  J  =  1, YSIZE

*       clear the output string
         OUTLINE  =  ' '

*       work out where the current row lies in the array - work down from
*       the top row to the bottom one
         CURRY  =  YLL + YSIZE - J

*       convert the current row index into a string
         CALL CHR_ITOC( CURRY, YSTR, NCHAR )

*       stick this string at the beginning of the output string
         OUTLINE( 2 : 5 )  =  YSTR( 1 : 4 )

*       now loop round the DXSIZE pixels in the current row
         DO  I  =  1, DXSIZE

*          work out where the current pixel lies in the row
            CURRX  =  XLL + I - 1

*          now set up the appropriate string according to the current
*          pixel value - first check for a pixel not actually on the
*          array
            IF ( CURRX .LT. 1 .OR. CURRX .GT. DIMS1 .OR.
     :           CURRY .LT. 1 .OR. CURRY .GT. DIMS2 ) THEN

*             set up string to indicate off the array
               STRING  =  '............'

*          else pixel is on array - format it nicely
            ELSE

*             get the current pixel value
               VALUE  =  ARRAY( CURRX, CURRY )

*             convert it into a string in a pleasing manner (!)
               CALL CHR_RTOC( VALUE, STRING( 1 : STRLEN ), NCHAR )

*          end of if-current-pixel-not-on-array check
            END IF

*          work out where to put the latest number in this row string
            POINT  =  ( ( I - 1 ) * ( STRLEN + 1 ) ) + 6

*          put the current number into the current row string, padding
*          with one additional space
            OUTLINE( POINT : POINT )  =  ' '
            OUTLINE( POINT + 1 : POINT + STRLEN )
     :                                      =  STRING ( 1 : STRLEN )

*       end of loop round pixels in current row
         END DO

*       output the current row string
         CALL MSG_OUT( 'CURRLINE', OUTLINE, STATUS )

*    end of loop round the rows
      END DO

*    output a blank line to finish
      CALL MSG_OUT( 'BLANK', ' ', STATUS )


*    end and return
      END
