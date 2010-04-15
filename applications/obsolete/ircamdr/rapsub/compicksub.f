
*+  COMPICKSUB - compresses an image by selecting pixels at intervals

      SUBROUTINE COMPICKSUB ( INARRAY, IDIMS1, IDIMS2, OUTARRAY, ODIMS1,
     :                        ODIMS2, COMPRESS, STATUS )

*    Description :
*
*     This routine compresses an input image by selecting evenly spaced
*     pixels ( where the pixel spacing is defined by the compression
*     factor ), and using those as the pixels for the smaller output
*     image. This routine is designed for 2-d arrays only.
*
*    Invocation :
*
*     CALL COMPICKSUB( INARRAY, IDIMS, OUTARRAY, ODIMS, COMPRESS, STATUS )
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
*     defines the linear interval in both x and y dimensions
*     between the pixels to be selected from the input image to
*     form the output. Starting with the pixel 1,1 in the input
*     image, pixels are selected at the calculated interval and
*     put into the output image array. This process is repeated
*     until the output array has been filled.
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
     :    ROWPIX,              ! current y index in input image
     :    COLPIX,              !    "    x   "    "   "     "
     :    I, J                 ! counters


*-
*    Error check on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    loop around all the output pixels - outer loop is in y dimension

      DO  J  =  1, ODIMS2

*       calculate the y index of the next pixels to be selected from
*       the input image - starts at 1

         ROWPIX  = ( ( J - 1 ) * COMPRESS ) + 1

*       loop round the output image x dimension

         DO  I  =  1, ODIMS1

*          calculate the x index of the next pixel to be selected from
*          the input image - starts at 1

            COLPIX  =  ( (  I - 1 ) * COMPRESS ) + 1

*          set the current output array pixel to be the calculated
*          input array pixel

            OUTARRAY( I, J )  =  INARRAY( COLPIX, ROWPIX )

         END DO
      END DO


*    end and return

      END
