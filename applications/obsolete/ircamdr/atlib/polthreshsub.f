*+ POLTHRESHSUB - sets pixels in array to defined new values outside limits

	SUBROUTINE POLTHRESHSUB( INARRAY1, INARRAY2, INARRAY3, INARRAY4,
     :	                         OUTARRAY1, OUTARRAY2, OUTARRAY3,
     :                           OUTARRAY4, DIMS1, DIMS2, THRLO, NUMPTY,
     :                           STATUS )

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
*     CALL POLTHRESHSUB( INARRAY1, INARRAY2, INARRAY3, INARRAY4,
*                        OUTARRAY1, OUTARRAY2, OUTARRAY3, OUTARRAY4,
*                        DIMS, THRLO, NUMPTY, STATUS )
*
*    Parameters :
*
*     INARRAY1( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - 0 degrees
*     INARRAY2( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - 45 degrees
*     INARRAY3( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - 22.5 degrees
*     INARRAY4( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - 67.5 degrees
*     OUTARRAY1( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - 0 degrees
*     OUTARRAY2( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - 45 degrees
*     OUTARRAY3( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - 22.5 degrees
*     OUTARRAY4( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - 67.5 degrees
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of input and output data arrays
*     THRLO  =  REAL( READ )
*         Upper threshold level
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     For all pixels of input arrays
*        If one is value less than lower threshold THRLO
*           Set new value to0.0
*        Else
*           Copy input values straight into output values
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
*     Colin Aspin (JACH::CAA)
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
*     10-17-1988 : created this from threshsub (JACH::CAA)
*     11-AUG-1994  Changed DIM arguments so that routine will compile (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE                 ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'             ! SSE global definitions

*    Import :

      INTEGER
     :    DIMS1,                  ! dimensions of data arrays
     :    DIMS2                  ! dimensions of data arrays

      REAL
     :    INARRAY1( DIMS1, DIMS2 ),  ! input data array
     :    INARRAY2( DIMS1, DIMS2 ),  ! input data array
     :    INARRAY3( DIMS1, DIMS2 ),  ! input data array
     :    INARRAY4( DIMS1, DIMS2 ),  ! input data array
     :    THRLO                      ! lower threshold

*    Export :

      INTEGER
     :	  NUMPTY

      REAL
     :    OUTARRAY1( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY2( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY3( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY4( DIMS1, DIMS2 )   ! output data array

*    Status :

      INTEGER  STATUS                ! global status parameter

*    Local variables :

      INTEGER  I, J                  ! counters

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    initialize numpty to zero
      NUMPTY = 0

*    loop round all rows in input image
      DO  J  =  1, DIMS2

*       loop round all pixels in current row
         DO  I  =  1, DIMS1

            IF ( INARRAY1( I, J ) .LT. THRLO .OR.
     :	         INARRAY2( I, J ) .LT. THRLO .OR.
     :	         INARRAY3( I, J ) .LT. THRLO .OR.
     :	         INARRAY4( I, J ) .LT. THRLO ) THEN

*             one of input pixel values is less than lower threshold - set
*             all output pixel to given replacement value
               OUTARRAY1( I, J )  =  0.0
               OUTARRAY2( I, J )  =  0.0
               OUTARRAY3( I, J )  =  0.0
               OUTARRAY4( I, J )  =  0.0

	       NUMPTY = NUMPTY + 1

            ELSE

*             input pixel values lies between thresholds - just copy
*             it into output pixel
               OUTARRAY1( I, J )  =  INARRAY1( I, J )
               OUTARRAY2( I, J )  =  INARRAY2( I, J )
               OUTARRAY3( I, J )  =  INARRAY3( I, J )
               OUTARRAY4( I, J )  =  INARRAY4( I, J )

*          end of check to see where input pixel value lies in range
            END IF

*       end of loop round all pixels in current row
         END DO

*    end of loop round all rows in input image
      END DO

*    end
      END
