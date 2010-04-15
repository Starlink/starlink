
*+  SUBARR2D - subtract two 2-d arrays

      SUBROUTINE SUBARR2D ( INARRAY1, INARRAY2, OUTARRAY, DIMS1, DIMS2,
     :                      STATUS )

*    Description :
*
*     This routine fills the output array pixels with the results
*     of subtracting the pixels of the second input array from the
*     first input array one at a time.
*
*    Invocation :
*
*     CALL SUBARR2D( INARRAY1, INARRAY2, OUTARRAY, DIMS, STATUS )
*
*    Method :
*
*     Check for error on entry - if not o.k. return immediately
*     For all pixels of the output array
*        Outarray pixel = Inarray1 pixel - Inarray2 pixel
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
*     09-01-1986 : First implementation (REVA::MJM)
*     30-06-1986 : Renamed from SUBARRAY to SUBARR2D (REVA::MJM)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :   DIMS1, DIMS2                ! dimensions of arrays

      REAL
     :   INARRAY1( DIMS1, DIMS2 ),   ! first input array
     :   INARRAY2( DIMS1, DIMS2 )    ! second  "     "

*    Export :

      REAL
     :   OUTARRAY( DIMS1, DIMS2 )   ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :   I, J             ! counter variables

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    loop round all rows of the output array
      DO  J  =  1, DIMS2

*       loop round all pixels of the current row
         DO  I  =  1, DIMS1

*          set output pixel to be first input array pixel minus
*          second input array pixel
            OUTARRAY( I, J )  =  INARRAY1( I, J ) - INARRAY2( I, J )

         END DO

      END DO

*    return

      END
