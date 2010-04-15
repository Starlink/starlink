
*+  DIVSCA2D - divide every pixel of a 2-d array by a scalar

      SUBROUTINE DIVSCA2D ( INARRAY, DIMS1, DIMS2, SCALAR, OUTARRAY,
     :                      STATUS )

*    Description :
*
*     This routine fills the output array pixels with the results
*     of dividing each pixel of the input array by the given scalar.
*     Error checking is done to prevent division by zero.
*
*    Invocation :
*
*     CALL DIVSCA2D( INARRAY, DIMS, SCALAR; OUTARRAY, STATUS )
*
*    Method :
*
*     Check for error on entry - if not o.k. return immediately
*     If scalar is too small then
*        For all pixels of output array
*           Outarray pixel = Inarray pixel
*        Endfor
*        Status set to indicate error on return
*     Else
*        For all pixels of the output array
*           Outarray pixel = Inarray pixel / Scalar
*        Endfor
*     Endif
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
*     27-06-1986 :  First implementation (REVA::MJM)
*     12-Aug-1994   Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :   DIMS1, DIMS2                ! dimensions of arrays

      REAL
     :   INARRAY( DIMS1, DIMS2 ),   ! input array
     :   SCALAR           ! number to be used in pixel division

*    Export :

      REAL
     :   OUTARRAY( DIMS1, DIMS2 )  ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local constants :

      REAL
     :   MINIMUM                  ! smallest scalar allowable
      PARAMETER( MINIMUM  =  1.0E-20 )  ! will do for now

*    Local variables :

      INTEGER
     :   I, J             ! counter variables

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    check scalar to see if it is too small
      IF ( ABS( SCALAR ) .LT. MINIMUM ) THEN

*       too small - fill output up with input and set status to indicate
*       the error - first loop round all rows of the output array
         DO  J  =  1, DIMS2

*          loop round all pixels of the current row
            DO  I  =  1, DIMS1

*             set output pixel to be input array pixel times scalar
               OUTARRAY( I, J )  =  INARRAY( I, J )

            END DO

         END DO

*       set status to indicate an error
         STATUS  =  SAI__ERROR

*    else scalar value is ok
      ELSE

*       loop round all rows of the output array
         DO  J  =  1, DIMS2

*          loop round all pixels of the current row
            DO  I  =  1, DIMS1

*             set output pixel to be input array pixel divided by scalar
               OUTARRAY( I, J )  =  INARRAY( I, J ) / SCALAR

            END DO

         END DO

*    end of if-scalar-is-too-small check
      END IF


*    return
      END
