*+ POLTHRESHSUB2 - sets pixels in array to defined new values outside limits

	SUBROUTINE POLTHRESH2SUB ( INARRAY1, INARRAY2, INARRAY3, INARRAY4,
     :	                           INARRAY5, INARRAY6, INARRAY7, INARRAY8,
     :	                           OUTARRAY1, OUTARRAY2, OUTARRAY3,
     :                             OUTARRAY4, OUTARRAY5, OUTARRAY6,
     :	                           OUTARRAY7, OUTARRAY8, DIMS1, DIMS2,
     :	                           THRLO, NUMPTY, STATUS )

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
*     CALL POLTHRESH2SUB( INARRAY1, INARRAY2, INARRAY3, INARRAY4,
*                         INARRAY5, INARRAY6, INARRAY7, INARRAY8,
*                         OUTARRAY1, OUTARRAY2, OUTARRAY3, OUTARRAY4,
*                         OUTARRAY5, OUTARRAY6, OUTARRAY7, OUTARRAY8,
*                         DIMS1, DIMS2, THRLO, NUMPTY, STATUS )
*
*    Parameters :
*
*     INARRAY1( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - o- 0 degrees
*     INARRAY2( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - e- 0 degrees
*     INARRAY3( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - o- 45 degrees
*     INARRAY4( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - e- 45 degrees
*     INARRAY5( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - o- 22.5 degrees
*     INARRAY6( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - e- 22.5 degrees
*     INARRAY7( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - o- 67.5 degrees
*     INARRAY8( DIMS( 1 ), DIMS( 2 )  =  REAL( READ )
*         Input data to be thresholded - e- 67.5 degrees
*     OUTARRAY1( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - o- 0 degrees
*     OUTARRAY2( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - e- 0 degrees
*     OUTARRAY3( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - o- 45 degrees
*     OUTARRAY4( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - e- 45 degrees
*     OUTARRAY5( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - o- 22.5 degrees
*     OUTARRAY6( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - e- 22.5 degrees
*     OUTARRAY7( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - o- 67.5 degrees
*     OUTARRAY8( DIMS( 1 ), DIMS( 2 )  =  REAL( WRITE )
*         Output thresholded data - e- 67.5 degrees
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
*           Set new value to 0
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
*     27-Nov-1995: made this version from polthreshsub (caa@jach)
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
     :    INARRAY5( DIMS1, DIMS2 ),  ! input data array
     :    INARRAY6( DIMS1, DIMS2 ),  ! input data array
     :    INARRAY7( DIMS1, DIMS2 ),  ! input data array
     :    INARRAY8( DIMS1, DIMS2 ),  ! input data array
     :    THRLO                      ! lower threshold

*    Export :

      INTEGER
     :	  NUMPTY

      REAL
     :    OUTARRAY1( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY2( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY3( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY4( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY5( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY6( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY7( DIMS1, DIMS2 ),  ! output data array
     :    OUTARRAY8( DIMS1, DIMS2 )   ! output data array

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
     :	         INARRAY4( I, J ) .LT. THRLO .OR.
     :	         INARRAY5( I, J ) .LT. THRLO .OR.
     :	         INARRAY6( I, J ) .LT. THRLO .OR.
     :	         INARRAY7( I, J ) .LT. THRLO .OR.
     :	         INARRAY8( I, J ) .LT. THRLO ) THEN

*             one of input pixel values is less than lower threshold - set
*             all output pixel to given replacement value
               OUTARRAY1( I, J )  =  0.0
               OUTARRAY2( I, J )  =  0.0
               OUTARRAY3( I, J )  =  0.0
               OUTARRAY4( I, J )  =  0.0
               OUTARRAY5( I, J )  =  0.0
               OUTARRAY6( I, J )  =  0.0
               OUTARRAY7( I, J )  =  0.0
               OUTARRAY8( I, J )  =  0.0

	       NUMPTY = NUMPTY + 1

            ELSE

*             input pixel values lies between thresholds - just copy
*             it into output pixel
               OUTARRAY1( I, J )  =  INARRAY1( I, J )
               OUTARRAY2( I, J )  =  INARRAY2( I, J )
               OUTARRAY3( I, J )  =  INARRAY3( I, J )
               OUTARRAY4( I, J )  =  INARRAY4( I, J )
               OUTARRAY5( I, J )  =  INARRAY5( I, J )
               OUTARRAY6( I, J )  =  INARRAY6( I, J )
               OUTARRAY7( I, J )  =  INARRAY7( I, J )
               OUTARRAY8( I, J )  =  INARRAY8( I, J )

*          end of check to see where input pixel value lies in range
            END IF

*       end of loop round all pixels in current row
         END DO

*    end of loop round all rows in input image
      END DO

*    end
      END
