*+ SETVALSUB - sets pixels in array equal to value to another value

      SUBROUTINE SETVALSUB( INARRAY, OUTARRAY, DIMS1, DIMS2, INVAL,
     :                      OUTVAL, STATUS )

*    Description :
*
*    Invocation :
*
*     CALL SETVALSUB( INARRAY, OUTARRAY, DIMS, INVAL, OUTVAL, STATUS )
*
*    Parameters :
*
*     INARRAY( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded
*     OUTARRAY( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of input and output data arrays
*     INVAL  =  REAL( READ )
*         Value to be changed
*     OUTVAL  =  REAL( READ )
*         New value
*
*    Method :
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*    History :
*
*     28-02-1987 : First implementation (UKTH::CAA)
*     11-AUG-1994  Changed DIM arguments so that routine will compile (SKL@JACH)
*
*
*    Type Definitions :

      IMPLICIT  NONE                 ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'             ! SSE global definitions

*    Import :

      INTEGER
     :    DIMS1, DIMS2                  ! dimensions of data arrays

      REAL
     :    INARRAY( DIMS1, DIMS2 ),  ! input data array
     :    INVAL,                            ! input value
     :    OUTVAL                            ! new value

*    Export :

      REAL
     :    OUTARRAY( DIMS1, DIMS2 )  ! output data array

*    Status :

      INTEGER  STATUS                ! global status parameter

*    Local Constants :

      REAL  TOLERANCE		     ! tolerance an real comparison

      PARAMETER ( TOLERANCE = 1.0E-5)

*    Local variables :

      INTEGER  I, J                  ! counters

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN

         RETURN

      ENDIF

*    loop round all rows in input image

      DO  J  =  1, DIMS2

*       loop round all pixels in current row

         DO  I  =  1, DIMS1

*          check input array value and act accordingly

            IF ( INARRAY( I, J) .GT. ( INVAL - TOLERANCE) .AND.
     :	         INARRAY( I, J) .LT. ( INVAL + TOLERANCE)) THEN

*             input pixel value is to be changed - set output pixel to
*             given replacement value

               OUTARRAY( I, J )  =  OUTVAL

            ELSE

*             input pixel value is not the value to be changed so just copy
*             it into output pixel

               OUTARRAY( I, J )  =  INARRAY( I, J )

*          end of check to see where input pixel value lies in range

            END IF

*       end of loop round all pixels in current row

         END DO

*    end of loop round all rows in input image

      END DO

*    end

      END
