
*+  COMPADDSUB - compresses an image by adding together several input pixels

      SUBROUTINE COMPADDSUB ( INARRAY, IDIMS1, IDIMS2, OUTARRAY, ODIMS1,
     :                        ODIMS2, COMPRESS, STATUS )

*    Description :
*
*     This routine adds together a number of pixels in the input image
*     ( as defined by the integer compression factor ), and creates a
*     a smaller image containing the binned-up pixels. This routine is
*     designed for 2-d arrays only.
*
*    Invocation :
*
*     CALL COMPADDSUB( INARRAY, IDIMS, OUTARRAY, ODIMS, COMPRESS, STATUS )
*
*    Parameters :
*
*     INARRAY( IDIMS( 1 ), IDIMS( 2 ) ) = REAL( READ )
*           The input data array
*     IDIMS( 2 ) = INTEGER( READ )
*           The dimensions of the input array
*     OUTARRAY( ODIMS( 1 ), ODIMS( 2 ) ) = REAL( WRITE )
*           The output compressed data array
*     ODIMS( 2 ) = INTEGER( READ )
*           The dimensions of the output array
*     COMPRESS = INTEGER( READ )
*           The linear compression factor applied
*
*    Method :
*
*     The routine takes as input a data array and its dimensions,
*     and a new array with its dimensions. The compression factor
*     defines the linear factor over which the input array is to
*     be added up in each dimension to compress the input array
*     into the output array. Using a rolling array that equals the
*     length of the input array x dimension, each column in the
*     input is summed over the calculated number of rows, and then
*     the columns are summed over the same calculated number to
*     form a row of output pixels. The rolling array is then re-
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
*     19-09-1985 : First implementation ( mainly copied from COMPAVESUB )
*                : (REVA::MJM)
*     15-AUG-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE

*    Global constants :

      INCLUDE  'SAE_PAR'           ! global SSE definitions

*    Import :

      INTEGER
     :    IDIMS1, IDIMS2,      ! dimensions of input array
     :    ODIMS1, ODIMS2,      !      "      " output  "
     :    COMPRESS         ! the linear compression factor to be used

      REAL
     :    INARRAY( IDIMS1, IDIMS2 )    ! input data array

*    Export :

      REAL
     :    OUTARRAY( ODIMS1, ODIMS2 )   ! output data array

*    Status :

      INTEGER  STATUS          ! global status parameter

*    Local variables :

      INTEGER
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


*    loop around all the output pixels - outer loop is in y dimension

      DO  J  =  1, ODIMS2

*       first re-initialise the row-summing array

         DO  N  =  1, IDIMS1
            ROW( N )  =  0.0
         END DO

*       calculate next start and end rows in  the input image which
*       the current output row will come from

         STARTROW  =  ( ( J - 1 ) * COMPRESS ) + 1
         ENDROW    =  STARTROW + COMPRESS - 1

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

            STARTCOL  =  ( ( I - 1 ) * COMPRESS ) + 1
            ENDCOL    =  STARTCOL + COMPRESS - 1

*          loop around the appropriate columns in the input image
*          which make up the current column of the output image

            DO  M  =  STARTCOL, ENDCOL
               RUNTOT  =  RUNTOT + ROW( M )
            END DO

*          finally make the output pixel equal to the current value
*          of RUNTOT, it being the sum of the required number of
*          pixels from the input array

            OUTARRAY( I, J )  =  RUNTOT

         END DO
      END DO

*    end and return

      END
