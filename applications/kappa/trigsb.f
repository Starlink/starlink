*+  TRIGSB - transforms input to output using a trig function

      SUBROUTINE TRIGSB ( INARR, DIMS, TRGFNC, OUTARR, STATUS )
*
*    Description :
*
*     According to the input value given for the string TRGFNC,
*     the input image array INARR is transformed using the
*     given function to the output image OUTARR.
*     Depending on the string passed, radian or degree trigonometric
*     functions will be used.
*
*    Invocation :
*
*     CALL TRIGSB( INARR, DIMS, TRGFNC, OUTARR, STATUS )
*
*    Arguments :
*
*     INARR( DIMS ) = REAL( READ )
*         Input array to be operated on
*     DIMS = INTEGER( READ )
*         Dimension of input and output arrays
*     TRGFNC = CHARACTER*(*)( READ )
*         Chosen trignometrical function to be applied
*     OUTARR( DIMS ) = REAL( WRITE )
*         Result of trignometrical operation
*     STATUS = INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     The string TRGFNC is examined, and depending on what value
*     it contains, the relevant loop is executed, setting the output
*     array pixel values to the corresponding input pixels, transformed
*     by the requested trigonometrical function if the input pixel is
*     valid. Checking is employed in the TAN and TAND loops, as they may
*     potentially cause overflow if an input pixel is set very close
*     to 90.0 or 270.0 degrees.
*     If pixels are found during arcsine or arccos transforms
*     which are beyond the range -1 to 1, then an error is reported.
*     If an invalid trig function is input, the output array is
*     merely copied from the input, an error is reported also.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     10-12-1985 :  First implementation (UKTH::MARK)
*     1986 Aug 18:  Renamed TRIGSB, prologue completed, Vax-specific
*                   degree-trignometrical function calls removed,
*                   generalised to an array, nearly conformed to
*                   Starlink programming standards (RL.STAR::CUR).
*     1986 Sep 5 :  Renamed parameters section to arguments, applied
*                   bad-pixel handling, replaced UPCASE by CHR_UCASE
*                   (RL.STAR::CUR).
*     1988 May 31:  Reporting of error context, so TRGFNC is import only
*                   (RL.STAR::CUR).
*     1988 Jun 22 : Added identification to error reporting
*                   (RL.STAR::CUR).
*
*    Type definitions :

      IMPLICIT  NONE            ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'        ! SSE global definitions
      INCLUDE 'PRM_PAR'         ! PRIMDAT public constants

*    Import :

      INTEGER
     :     DIMS

      REAL
     :     INARR( DIMS )

      CHARACTER*(*)
     :     TRGFNC

*    Export :

      REAL
     :     OUTARR( DIMS )

*    Status :

      INTEGER  STATUS

*    Local Constants :

      REAL
     :     PI,                  ! pi
     :     DGTORD,              ! degrees-to-radians conversion factor
     :     R90,                 ! radian equivalent of pi/2
     :     R90M,                ! radian overflow value just below pi/2
     :     R90P,                !    "       "      "     "  above pi/2
     :     D90M,                ! degree     "      "     "  below 90 
     :     D90P                 !    "       "      "     "  above 90 

      PARAMETER( PI  =  3.141592653 )
      PARAMETER( DGTORD = PI/180.0 )
      PARAMETER( R90  =  PI / 2.0 )
      PARAMETER( R90M  =  R90 - 0.0000001 )
      PARAMETER( R90P  =  R90 + 0.00000001 )
      PARAMETER( D90M  =  90.0 - 0.00001 )
      PARAMETER( D90P  =  90.0 + 0.00001 )

*    Local variables :

      INTEGER
     :     J                    ! general array counter variable

      REAL
     :     DUMMY                ! dummy variable used when checking
                                ! for overflow with TAN and TAND

      LOGICAL                   ! true if:
     :     TANPOS,              ! radian tangent will positive overflow
     :     TANNEG,              !    "      "      "  negative     "
     :     TANDPS,              ! degree    "      "  positive     "
     :     TANDNG,              !    "      "      "  negative     "
     :     TRGOUT               ! input scalar value is out-of-range for
                                ! chosen trig. function

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      TRGOUT  =  .FALSE.

*    start by forcing input function string to uppercase

      CALL CHR_UCASE( TRGFNC )

*    now check through all valid trigonometrical function options
*    and filling output array with transformed version of input
*    array

*    sine (radians)

      IF( TRGFNC .EQ. 'SIN' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE
               OUTARR( J )  =  SIN( INARR( J ) )
            END IF
         END DO

*    cosine (radians)

      ELSEIF( TRGFNC .EQ. 'COS' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE
               OUTARR( J )  =  COS( INARR( J ) )
            END IF
         END DO

*    tangent (radians)

      ELSEIF( TRGFNC .EQ. 'TAN' ) THEN

         DO  J  =  1, DIMS

            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE

*             check for pixel values that could cause overflow

               DUMMY  =  MOD( INARR( J ), PI )

*             evaluate logicals that allow checking to see if 
*             tangent functions will overflow
*             if value is just below or equal to pi/2 radians

               TANPOS  =  ( DUMMY .GT. R90M .AND. DUMMY .LE. R90 )

*             if value is just above pi/2 radians

               TANNEG  =  ( DUMMY .GE. R90 .AND. DUMMY .LT. R90P )

               IF( TANPOS .OR. TANNEG ) THEN
                  OUTARR( J )  =  VAL__BADR
               ELSE
                  OUTARR( J )  =  TAN( INARR( J ) )
               END IF
            END IF
         END DO

*    sine (degrees)

      ELSEIF( TRGFNC .EQ. 'SIND' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE
               OUTARR( J )  =  SIN( DGTORD * INARR( J ) )
            END IF
         END DO

*    cosine (degrees)

      ELSEIF( TRGFNC .EQ. 'COSD' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE
               OUTARR( J )  =  COS( DGTORD * INARR( J ) )
            END IF
         END DO

*    tangent (degrees)

      ELSEIF( TRGFNC .EQ. 'TAND' ) THEN

         DO  J  =  1, DIMS

            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE

*             check for pixel values that could cause overflow

               DUMMY  =  MOD( INARR( J ), 180.0 )

*             evaluate logicals that allow you to see if 
*             tangent functions will overflow
*             if value is just below or equal to 90 degrees

               TANDPS =  ( DUMMY .GT. D90M .AND. DUMMY .LE. 90.0 )

*             if value is just above 90 degrees

               TANDNG =  ( DUMMY .GE. 90.0 .AND. DUMMY .LT. D90P )

               IF( TANDPS .OR. TANDNG ) THEN
                  OUTARR( J )  =  VAL__BADR
               ELSE
                  OUTARR( J )  =  TAN( DGTORD * INARR( J ) )
               END IF
            END IF
         END DO

*    arcsine (radians)

      ELSEIF( TRGFNC .EQ. 'ASIN' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE IF( INARR( J ) .GT. 1.0 ) THEN
               OUTARR( J )  =  R90
               TRGOUT  =  .TRUE.
            ELSEIF( INARR( J ) .LT. -1.0 ) THEN
               OUTARR( J )  =  -R90
               TRGOUT  =  .TRUE.
            ELSE
               OUTARR( J )  =  ASIN( INARR( J ) )
            END IF
          END DO

*    arccosine (radians)

      ELSEIF( TRGFNC .EQ. 'ACOS' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSEIF( INARR( J ) .GT. 1.0 .OR. INARR( J ) .LT. -1.0 ) THEN
               OUTARR( J )  =  0.0
               TRGOUT  =  .TRUE.
            ELSE
               OUTARR( J )  =  ACOS( INARR( J ) )
            END IF
         END DO

*    arctangent (radians)

      ELSEIF( TRGFNC .EQ. 'ATAN' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE
               OUTARR( J )  =  ATAN( INARR( J ) )
            END IF
         END DO

*    arcsine (degrees)

      ELSEIF( TRGFNC .EQ. 'ASIND' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE IF( INARR( J ) .GT. 1.0 ) THEN
               OUTARR( J )  =  90.0
               TRGOUT  =  .TRUE.
            ELSEIF( INARR( J ) .LT. -1.0 ) THEN
               OUTARR( J )  =  -90.0
               TRGOUT  =  .TRUE.
            ELSE
               OUTARR( J )  =  ASIN( INARR( J ) ) / DGTORD
            END IF
         END DO

*    arccosine (degrees)

      ELSEIF( TRGFNC .EQ. 'ACOSD' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSEIF( INARR( J ) .GT. 1.0 .OR. INARR( J ) .LT. -1.0 ) THEN
               OUTARR( J )  =  0.0 
               TRGOUT  =  .TRUE.
            ELSE
               OUTARR( J )  =  ACOS( INARR( J ) ) / DGTORD
            END IF
         END DO

*    arctangent (degrees)

      ELSEIF( TRGFNC .EQ. 'ATAND' ) THEN

         DO  J  =  1, DIMS
            IF ( INARR( J ) .EQ. VAL__BADR ) THEN
               OUTARR( J )  =  INARR( J )
            ELSE
               OUTARR( J )  =  ATAN( INARR( J ) ) / DGTORD
            END IF
         END DO

*    unrecognised type - set output same as input and report error

      ELSE 

         STATUS  =  SAI__ERROR
         CALL ERR_REP( 'ERR_TRIGSB_BAD',
     :     'TRIGSB: Bad trig function passed - output array set equal '/
     :     /'to input array.', STATUS )

         DO  J  =  1, DIMS
            OUTARR( J )  =  INARR( J )
         END DO

      END IF

      IF ( TRGOUT ) THEN
         STATUS  =  SAI__ERROR
         CALL ERR_REP( 'ERR_TRIGSB_OUT',
     :     'TRIGSB: At least one input pixel was outside function '/
     :     /'range', STATUS )
      END IF

*    that's it - return

      END
