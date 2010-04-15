
*+  LISTSUB - lists a specified section of an image to a file for printing

      SUBROUTINE LISTSUB ( ARRAY, DIMS1, DIMS2, XLOW, YLOW, XUPP, YUPP,
     :                     FILENAME, STATUS )

*    Description :
*
*     This routine takes an input image and lists out a specified section
*     of that image, as defined by the x and y lower and upper bounds, to
*     a file as specified by the input file name. The format chosen gives
*     ten numbers across a full printout page (132 chars), and a new page
*     is thrown at the right point.
*
*    Invocation :
*
*     CALL LISTSUB( ARRAY, DIMS, XLOW, YLOW, XUPP, YUPP, FILENAME, STATUS )
*
*    Parameters :
*
*     ARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( READ )
*         Image to be listed out
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of image
*     XLOW  =  INTEGER( READ )
*         x coord of lower left corner of sub-array to be listed out
*     YLOW  =  INTEGER( READ )
*         y coord of lower left corner of sub-array to be listed out
*     XUPP  =  INTEGER( READ )
*         x coord of upper right corner of sub-array to be listed out
*     YUPP  =  INTEGER( READ )
*         y coord of upper right corner of sub-array to be listed out
*     FILENAME  =  CHAR( READ )
*         Name of file to contain listing output
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get a free unit number from process pool
*     Open given file name as this unit number
*     If not successful on opening then
*        Set status to indicate error
*        Free unit number
*        Return
*     Endif
*     Loop round selected columns in strips of ten
*        Output new page and current strip number to file
*        Work out the x indices of strip start and end
*        For all columns in current strip
*           Convert column index into a string
*           Stick this string into the output string
*        Endfor
*        Output column index string to file
*        For all selected rows in current strip
*           Convert row index into string and put into output string
*           For all selected pixels in current row
*              If pixel does not lie on array
*                 Indicate with special string
*              Else
*                 Get value from array at this pixel location
*                 Convert value to a string
*                 Stick the string into the output string at the right point
*              Endif
*           Endfor
*           Output string corresponding to current row to file
*        Endfor
*     Endfor
*     Close file
*     Free unit number back into process pool
*     Return
*
*    Deficiencies :
*
*     Uses Fortran file i/o
*     Uses Vax run time library routines LIB$GET_LUN and LIB$FREE_LUN
*     Uses statement labels and format statements
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     14-06-1986 : First implementation - partly from KFH (REVA::MJM)
*     26-May-1994  Replaced lib$ calls with fio_ (SKL@JACH)
*     15-JUL-1994  Changed arguments to input dimensions separately
*                  so that routine will compile (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'FIO_PAR'

*    Import :

      INTEGER
     :    DIMS1,              ! dimensions of input array
     :    DIMS2,              ! dimensions of input array
     :    XLOW,                   ! x coord of lower left of sub-array
     :    YLOW,                   ! y   "    "   "     "   "  "    "
     :    XUPP,                   ! x   "    " upper right "  "    "
     :    YUPP                    ! y   "    "   "     "   "  "    "

      REAL
     :    ARRAY( DIMS1, DIMS2 )  ! array containing data

      CHARACTER*(*)
     :    FILENAME                ! name of VMS file to hold listed data

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    STAT,                   ! for IOSTAT
     :    LUN,                    ! file unit number
     :    STRIPNUM,               ! current strip number
     :    STRIPSTART,             ! start x coord of current strip
     :    STRIPEND,               ! end   "   "    "    "      "
     :    POINT,                  ! pointer into output line
     :    NCHAR,                  ! dummy to hold return from CHR_ calls
     :    I, J, JJ                ! counter variables

      REAL
     :    VALUE                   ! current pixel value

      CHARACTER*120
     :    OUTLINE                 ! output line for listing

      CHARACTER*10
     :    CURRX,                  ! string to hold x index value
     :    CURRY,                  !    "    "   "  y   "     "
     :    VALSTR                  !    "    "   "  current pixel value

      CHARACTER*5
     :    STRIPSTR                ! string to hold current strip number

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    get a free unit number for the file opening from the process pool
      CALL FIO_GUNIT( LUN, STATUS )

*    open the requested named file as the unit specified above
      OPEN( UNIT = LUN, FILE = FILENAME, STATUS = 'NEW',
     :      IOSTAT=STAT, ERR = 999 )

*    initialise the strip counter
      STRIPNUM  =  0

*    loop round all the requested columns in strips of 10 numbers
      DO  STRIPSTART  =  XLOW, XUPP, 10

*       increment the strip counter by one
         STRIPNUM  =  STRIPNUM + 1

*       reset the output string
         OUTLINE  =  ' '

*       convert the strip number to a fixed length string
         CALL CHR_ITOC( STRIPNUM, STRIPSTR, NCHAR )

*       stick this into the output line
         OUTLINE( 1  : 18 )  =  '     Strip number '
         OUTLINE( 19 : 24 )  =  STRIPSTR( 1 : 5 )

*       output a line to the file indicating which strip we are on
         WRITE( LUN, 100 ) OUTLINE
100      FORMAT( '1', A/ )

*       given the x index of the strip start, work out where the
*       strip ends, it either being 10 numbers wide, or earlier
*       if the requested sub-array finishes first
         STRIPEND  =  MIN( STRIPSTART + 9, XUPP )

*       reset the output line buffer
         OUTLINE  =  ' '

*       output the first line of the strip which is the running
*       x coordinate - loop round all the x coords in the current
*       strip
         DO  I  =  STRIPSTART, STRIPEND

*          convert the current index to a left justified string of
*          fixed length 10 characters
            CALL CHR_ITOC( I, CURRX, NCHAR )

*          work out where in the line output string we are - the
*          first ten characters are left blank
            POINT  =  ( ( I - STRIPSTART ) * 11 ) + 11

*          stick the current x index string in the right place in
*          the output string padding with one space to the right
            OUTLINE( POINT : POINT + 9 )  =  CURRX( 1 : 10 )
            OUTLINE( POINT + 10 : POINT + 10 )  =  ' '

         END DO

*       output the top line to the listing file with a blank line
*       following it
         WRITE( LUN, 200 ) OUTLINE
200      FORMAT( A/ )

*       now we want to loop round all the selected rows in the
*       current strip
         DO  J  =  YLOW, YUPP

*          we really want to come from the top down, so work out
*          which row we are on from the top
            JJ  =  YUPP - J + 1

*          reset the output line buffer
            OUTLINE  =  ' '

*          convert the row number to a fixed length string of 10
*          characters left justified
            CALL CHR_ITOC( JJ, CURRY, NCHAR )

*          stick this into the output string at the beginning, leaving
*          three spaces blank - if string started in first character,
*          then it would get interpreted as a page throw if the first
*          character was a 1
            OUTLINE( 3 : 10 )  =  CURRY( 1 : 7 )

*          now loop round the pixels in this row
            DO  I  =  STRIPSTART, STRIPEND

*             work out where in the output line we are, recalling that
*             the output line starts with 10 characters containing the
*             row index and padding spaces
               POINT  =  ( ( I - STRIPSTART ) * 11 ) + 11

*             check the current pixel is on the array
               IF ( I .LT. 1 .OR. I .GT. DIMS1 .OR.
     :              JJ .LT. 1 .OR. JJ .GT. DIMS2 ) THEN

*                set up string to indicate this
                  OUTLINE( POINT : POINT + 10 )  =  '.......... '

               ELSE

*                get the value held in the array at the current pixel
*                location
                  VALUE  =  ARRAY( I, JJ )

*                convert this to a fixed length string of 10 characters
*                justified to the left with spaces
                  CALL CHR_RTOC( VALUE, VALSTR, NCHAR )

*                stick the current pixel value string in the right place in
*                the output string padding with one space to the right
                  OUTLINE( POINT : POINT + 9 )  =  VALSTR( 1 : 10 )
                  OUTLINE( POINT + 10 : POINT + 10 )  =  ' '

*             end of if-pixel-not-on-array check
               END IF

*          end of loop round pixels in current row
            END DO

*          output the current row line to the listing file
            WRITE( LUN, 300 ) OUTLINE
300         FORMAT( A )

*       end of loop round rows in current strip
         END DO

*    end of loop round all the necessary strips of ten numbers
      END DO

*    close the file
      CLOSE( UNIT = LUN )

*    free the unit number back to the process pool
      CALL FIO_PUNIT( LUN, STATUS )


*    return
      RETURN


*    come here if there is an error on opening the requested file -
*    return an error status and free the unit number
999   CALL FIO_REP( LUN, FILENAME, STAT, ' ', STATUS )
      CALL FIO_PUNIT( LUN, STATUS )
      RETURN


*    return and end
      END
