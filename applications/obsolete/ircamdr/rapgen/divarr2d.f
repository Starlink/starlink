
*+  DIVARR2D - divide two 2-d arrays together pixel by pixel

      SUBROUTINE DIVARR2D ( INARRAY1, INARRAY2, OUTARRAY, DIMS1, DIMS2,
     :                      STATUS )

*    Description :
*
*     This routine fills the output array pixels with the results
*     of dividing the pixels of the first input array by those
*     of the second input array one at a time.
*
*    Invocation :
*
*     CALL DIVARR2D( INARRAY1, INARRAY2, OUTARRAY, DIMS, STATUS )
*
*    Method :
*
*     Check for error on entry - if not o.k. return immediately
*     For all pixels of the output array
*        If Inarray2 pixel has absolute value less than specified minimum then
*           Outarray pixel = Bad value
*        Else
*           Outarray pixel = Inarray1 pixel / Inarray2 pixel
*        Endif
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
     :   INARRAY1( DIMS1, DIMS2 ),   ! first input array
     :   INARRAY2( DIMS1, DIMS2 )    ! second  "     "

*    Export :

      REAL
     :   OUTARRAY( DIMS1, DIMS2 )   ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local constants :

      REAL
     :    MINIMUM,                ! absolute value of smallest allowable
                                  ! divisor
     :    BADVAL                  ! 'bad pixel' value to be used when a
                                  ! division by too small a number was
                                  ! trapped
      PARAMETER( MINIMUM  =  1.0E-20 )  ! will do for now
      PARAMETER( BADVAL   =  0.0 )      ! ditto

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

*          check divisor pixel value for an absolute value less than the
*          specified minimum
            IF ( ABS( INARRAY2( I, J ) ) .LT. MINIMUM ) THEN

*             too small a divisor - stick the 'bad pixel' value into
*             the output pixel
               OUTARRAY( I, J )  =  BADVAL

*          else a valid division may take place
            ELSE

*             set output pixel to be first input array pixel divided
*             by second input array pixel
               OUTARRAY( I, J )  =  INARRAY1( I, J ) / INARRAY2( I, J )

*          end of if-divisor-pixel-value-too-small check
            END IF

         END DO

      END DO

*    return
      END
