
*+  COMPRESSSUB - compresses an image by different amounts in x and y

      SUBROUTINE COMPRESSSUB ( INARRAY, IDIMS1, IDIMS2, OUTARRAY,
     :                         ODIMS1, ODIMS2, XCMPRS, YCMPRS,
     :                         STATUS )

*    Description :
*
*     This routine averages over a number of pixels in the input image
*     ( as defined by the two integer compression factors ), and creates
*     a smaller image containing the averaged pixels. This routine is
*     designed for 2-d arrays only.
*
*    Invocation :
*
*     CALL COMPRESSSUB( INARRAY, IDIMS, OUTARRAY, ODIMS, XCMPRS,
*                       YCMPRS, STATUS )
*
*    Method :
*
*     The routine takes as input a data array and its dimensions,
*     and a new array with its dimensions. The compression factors
*     define the linear factors over which the input array is to
*     be averaged in each dimension to compress the input array
*     into the output array. Using a rolling array that equals the
*     length of the input array x dimension, each column in the
*     input is summed over the calculated number of rows, and then
*     the columns are summed over the calculated number of columns
*     and divided by the total number of pixels that were summed over
*     to form a row of output pixels. The rolling array is then re-
*     initialised and is used again to sum over the next set of
*     rows and so on, until all the rows of the output image have
*     been created.
*
*    Deficiencies :
*
*     Maximum input array x dimension is 2048, not adjustable.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     21-10-1985 : First implementation
*                : (REVA::MJM)
*     15-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE               ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'           ! global SSE definitions

*    Import :

      INTEGER
     :    IDIMS1, IDIMS2,      ! dimensions of input array
     :    ODIMS1, ODIMS2,      !      "      " output  "
     :    XCMPRS,          ! x compression factor to be used
     :    YCMPRS           ! y      "         "    "  "   "

      REAL
     :    INARRAY( IDIMS1, IDIMS2 )    ! input data array

*    Export :

      REAL
     :    OUTARRAY( ODIMS1, ODIMS2 )   ! output data array

*    Status :

      INTEGER  STATUS          ! global status parameter

*    Local variables :

      INTEGER
     :    BOX,                 ! number of input pixels to one output
     :    STARTROW,            ! current start row in input image
     :    ENDROW,              !    "     end   "   "   "     "
     :    STARTCOL,            !    "    start column in "    "
     :    ENDCOL,              !    "     end    "     " "    "
     :    I, J, K, L, M, N     ! counters

      REAL
     :    ROW( 2048 ),         ! rolling row array for sub-totals
     :    RUNTOT               ! running total for summing


*-
*    Error check on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    work out the size of box which we average over in the input
*    image to create one new pixel in the output image - having used
*    integer arithmetic in to calculate the output dimensions from
*    the input dimensions and the compression factor, we can be
*    sure that an integer number of such boxes will fit into the
*    input array

      BOX  =  XCMPRS * YCMPRS

*    loop around all the output pixels - outer loop is in y dimension

      DO  J  =  1, ODIMS2

*       first re-initialise the row-summing array

         DO  N  =  1, IDIMS1
            ROW( N )  =  0.0
         END DO

*       calculate next start and end rows in  the input image which
*       the current output row will come from

         STARTROW  =  ( ( J - 1 ) * YCMPRS ) + 1
         ENDROW    =  STARTROW + YCMPRS - 1

*       now loop round these rows and all the input columns to get
*       a new row compressed in the y direction but not x

         DO  L  =  STARTROW, ENDROW
            DO  K  =  1, IDIMS1
               ROW( K )  =  ROW( K ) + INARRAY( K, L )
            END DO
         END DO

*       now loop round the number of columns in the output image,
*       compressing the rolling row in the x direction

         DO  I  =  1, ODIMS1

*          first re-initialise the running total

            RUNTOT  =  0.0

*          now work out the start and end columns in the input image
*          that go to make up the current output image column

            STARTCOL  =  ( ( I - 1 ) * XCMPRS ) + 1
            ENDCOL    =  STARTCOL + XCMPRS - 1

*          loop around the appropriate columns in the input image
*          which make up the current column of the output image

            DO  M  =  STARTCOL, ENDCOL
               RUNTOT  =  RUNTOT + ROW( M )
            END DO

*          finally divide the total by the number of pixels that went
*          to make it up to find the average

            OUTARRAY( I, J )  =  RUNTOT / BOX

         END DO
      END DO

*    end and return

      END
