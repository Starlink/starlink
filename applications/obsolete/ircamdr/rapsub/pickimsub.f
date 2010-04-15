
*+  PICKIMSUB - creates an output image from a subset of another

      SUBROUTINE PICKIMSUB ( INARRAY, IDIMS1, IDIMS2, XSTART, XFINISH,
     :                       YSTART, YFINISH, OUTARRAY, ODIMS1, ODIMS2,
     :                       STATUS )

*    Description :
*
*     This routine copies a specified subset of the input 2d image into
*     the relevant pixels of the output image, so that the output image
*     comprises a section of the input.
*
*    Invocation :
*
*     CALL PICKIMSUB( INARRAY, IDIMS, XSTART, XFINISH, YSTART,
*                     YFINISH, OUTARRAY, ODIMS, STATUS )
*
*    Parameters :
*
*     INARRAY( IDIMS( 1 ), IDIMS( 2 ) )  =  REAL( READ )
*           Input image from which the subset is to be taken
*     IDIMS( 2 )  =  INTEGER( READ )
*           Dimensions of input image
*     XSTART  =  INTEGER( READ )
*           First column to be included in subset
*     XFINISH  =  INTEGER( READ )
*           Last column to be included in subset
*     YSTART  =  INTEGER( READ )
*           First row to be included in subset
*     YFINISH  =  INTEGER( READ )
*           Last row to be included in subset
*     OUTARRAY( ODIMS( 1 ), ODIMS( 2 ) )  =  REAL( WRITE )
*           Output image containing subset of input
*     ODIMS( 2 )  =  INTEGER( READ )
*           Dimensions of output image
*     STATUS  =  INTEGER( READ )
*           Global status variable
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     For all included rows of input image
*        For all included pixels in current row
*           Copy input image pixel into relevant output image pixel
*        Endfor
*     Endfor
*     Return
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
*     26-11-1986 : First implementation (UKTH::MJM)
*     15-Aug-1994  Changed DIM arguments so routine will compile (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    IDIMS1, IDIMS2,             ! dimensions of input image
     :    ODIMS1, ODIMS2,             !      "      " output  "
     :    XSTART,                 ! first column to be included in output
     :    XFINISH,                ! last     "    "  "     "     "    "
     :    YSTART,                 ! first row     "  "     "     "    "
     :    YFINISH                 ! last   "      "  "     "     "    "

      REAL
     :    INARRAY( IDIMS1, IDIMS2 )   ! input image

*    Export :

      REAL
     :    OUTARRAY( ODIMS1, ODIMS2 )  ! output image

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    I, J, II, JJ            ! counters

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    loop round all included rows of input image
      DO  J  =  YSTART, YFINISH

*       work out corresponding row in output image
         JJ  =  J - YSTART + 1

*       loop round all included pixels of current row of input image
         DO  I  =  XSTART, XFINISH

*          work out corresponding column in output image
            II  =  I - XSTART + 1

*          copy input image pixel into relevant output image pixel
            OUTARRAY( II, JJ )  =  INARRAY( I, J )

*        end of loop round all included pixels of current row
          END DO

*    end of loop round all included rows of input image
      END DO


*    return and end
      END
