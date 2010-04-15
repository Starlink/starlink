
*+ THRESHSUB - sets pixels in array to defined new values outside limits

      SUBROUTINE THRESHSUB( INARRAY, OUTARRAY, DIMS1, DIMS2, THRLO,
     :                      THRHI, NEWLO, NEWHI, STATUS )

*    Description :
*
*     Takes an array of data and sets all values above a defined upper
*     threshold to a new defined value, and sets all those below a
*     defined lower threshold to another defined value. In practice,
*     all values outside the two thresholds may be set to zero, for
*     example.
*
*    Invocation :
*
*     CALL THRESHSUB( INARRAY, OUTARRAY, DIMS, THRLO, THRHI, NEWLO,
*    :                NEWHI, STATUS )
*
*    Parameters :
*
*     INARRAY( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded
*     OUTARRAY( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of input and output data arrays
*     THRLO  =  REAL( READ )
*         Upper threshold level
*     THRHI  =  REAL( READ )
*         Lower threshold level
*     NEWLO  =  REAL( READ )
*         Value to which pixels below THRLO will be set
*     NEWHI  =  REAL( READ )
*         Value to which pixels above THRHI will be set
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     For all pixels of input array
*        If value less than lower threshold THRLO
*           Set new value to NEWLO
*        Else if value higher than upper threshold THRHI
*           Set new value to NEWHI
*        Else
*           Copy input value straight into output value
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
*     B.D.Kelly (ROE::BDK)
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     29-01-1982 : First implementation (ROE::BDK)
*     04-06-1985 : Revised to take status parameter, to take different
*                : new values for upper and lower thresholds, and
*                : redocumented SSE / ADAM style (REVA::MJM)
*                : (Also changed last section - seemed crazy)
*     02-09-1985 : Renamed THRESHSUB (REVA::MJM)
*     03-07-1986 : Revised implementation and documentation (REVA::MJM)
*     15-Aug-1994  Changed DIM arguments so routine will compile (SKL@JACH)
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
     :    THRLO,                     ! lower threshold
     :    THRHI,                     ! upper     "
     :    NEWLO,                     ! new value for pixels below THRLO
     :    NEWHI                      ! new value for pixels above THRHI

*    Export :

      REAL
     :    OUTARRAY( DIMS1, DIMS2 )  ! output data array

*    Status :

      INTEGER  STATUS                ! global status parameter

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
            IF ( INARRAY( I, J ) .GT. THRHI ) THEN

*             input pixel value is greater than upper threshold - set
*             output pixel to given replacement value
               OUTARRAY( I, J )  =  NEWHI

            ELSE IF ( INARRAY( I, J ) .LT. THRLO ) THEN

*             input pixel value is less than lower threshold - set
*             output pixel to given replacement value
               OUTARRAY( I, J )  =  NEWLO

            ELSE

*             input pixel value lies between thresholds - just copy
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
