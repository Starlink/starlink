*+  TRGSCL - transforms scalar input to output using a trig function

      SUBROUTINE TRGSCL ( SCALAR, TRGFNC, RESULT, STATUS )
*
*    Description :
*
*     According to the input value given for the string TRGFNC,
*     the input scalar is transformed using the given function
*     and is returned in the result argument.
*     Depending on the string passed, radian or degree trig
*     functions will be used.
*
*    Invocation :
*
*     CALL TRGSCL( SCALAR, TRGFNC, RESULT, STATUS )
*
*    Arguments :
*
*     SCALAR = REAL( READ )
*         Input scalar value to be operated on
*     TRGFNC = CHARACTER*(*)( READ )
*         Chosen trignometrical function to be applied
*     RESULT = REAL( WRITE )
*         Result of trignometrical operation
*     STATUS = INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     The string TRGFNC is examined, and depending on what value
*     it contains, the relevant loop is executed, operating on the
*     input scalar value with the requested trigonometrical function. 
*     Checking is employed in the TAN and TAND loops, as they may
*     potentially cause overflow if the input scalar is very close
*     to 90.0 or 270.0 degrees.
*     If the scalar given to arcsine or arccos transforms is outside
*     the valid range of these functions an error is reported.
*     If an invalid trig function is input, the output array is
*     merely copied from the input, an error message is also reported.
*     Numbers input larger than 360 degrees and its radian equivalent
*     are merely taken as mod(180) or mod(pi) accordingly.
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
*     08-01-1986  : First implementation (REVA::MJM)
*     1986 Aug 18 : Renamed TRGSCL, prologue completed, Vax-specific
*                   degree trignometrical function calls removed,
*                   nearly conformed to Starlink programming standards
*                   (RL.STAR::CUR).
*     1986 Sep 5  : Renamed parameters section in prologue to arguments
*                   bad-pixel handling, replaced UPCASE by CHR_UCASE
*                   (RL.STAR::CUR).
*     1988 May 31 : Reporting of error context, so TRGFNC is import only
*                   (RL.STAR::CUR).
*     1988 Jun 22 : Added identification to error reporting
*                   (RL.STAR::CUR).
*
*    Type definitions :

      IMPLICIT  NONE            ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'        ! SSE global definitions

*    Import :

      REAL
     :     SCALAR

      CHARACTER*(*)
     :     TRGFNC

*    Export :

      REAL
     :     RESULT

*    Status :

      INTEGER  STATUS

*    Local Constants :

      REAL
     :     PI,                  ! pi
     :     DGTORD,              ! degrees-to-radians conversion factor
     :     DATMAX,              ! value for pixel when overflow occurs
     :     R90,                 ! radian equivalent of pi/2
     :     R90M,                ! radian overflow value just below pi/2
     :     R90P,                !    "       "      "     "  above pi/2
     :     D90M,                ! degree     "      "     "  below 90 
     :     D90P                 !    "       "      "     "  above 90 

      PARAMETER( PI  =  3.141592653 )
      PARAMETER( DGTORD = PI/180.0 )
      PARAMETER( DATMAX  =  1.0E20 )
      PARAMETER( R90  =  PI / 2.0 )
      PARAMETER( R90M  =  R90 - 0.0000001 )
      PARAMETER( R90P  =  R90 + 0.00000001 )
      PARAMETER( D90M  =  90.0 - 0.00001 )
      PARAMETER( D90P  =  90.0 + 0.00001 )

*    Local variables :

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

         RESULT  =  SIN( SCALAR )

*    cosine (radians)

      ELSE IF( TRGFNC .EQ. 'COS' ) THEN

         RESULT  =  COS( SCALAR )

*    tangent (radians)

      ELSE IF( TRGFNC .EQ. 'TAN' ) THEN

*       check for a scalar that could cause overflow

         DUMMY  =  MOD( SCALAR, PI )

*       evaluate logicals that allow checking to see if 
*       tangent functions will overflow

*       if value is just below or equal to pi/2 radians

         TANPOS  =  ( DUMMY .GT. R90M .AND. DUMMY .LE. R90 )

*       if value is just above pi/2 radians

         TANNEG  =  ( DUMMY .GE. R90 .AND. DUMMY .LT. R90P )

         IF( TANPOS ) THEN
            RESULT  =  DATMAX
         ELSE IF( TANNEG ) THEN
            RESULT  =  -DATMAX
         ELSE
            RESULT  =  TAN( SCALAR )
         END IF

*    sine (degrees)

      ELSE IF( TRGFNC .EQ. 'SIND' ) THEN

         RESULT  =  SIN( DGTORD * SCALAR )

*    cosine (degrees)

      ELSE IF( TRGFNC .EQ. 'COSD' ) THEN

         RESULT  =  COS( DGTORD * SCALAR )

*    tangent (degrees)

      ELSE IF( TRGFNC .EQ. 'TAND' ) THEN

*       check for pixel values that could cause overflow

         DUMMY  =  MOD( SCALAR, 180.0 )

*       evaluate logicals that allow you to see if 
*       tangent functions will overflow
*       if value is just below or equal to 90 degrees

         TANDPS =  ( DUMMY .GT. D90M .AND. DUMMY .LE. 90.0 )

*       if value is just above 90 degrees

         TANDNG =  ( DUMMY .GE. 90.0 
     :                      .AND. DUMMY .LT. D90P )

        IF( TANDPS ) THEN
           RESULT  =  DATMAX
         ELSE IF( TANDNG ) THEN
           RESULT  =  -DATMAX
         ELSE
           RESULT  =  TAN( DGTORD * SCALAR )
         END IF

*    arcsine (radians)

      ELSE IF( TRGFNC .EQ. 'ASIN' ) THEN

         IF( SCALAR .GT. 1.0 ) THEN
            RESULT  =  R90
            TRGOUT  =  .TRUE.
         ELSE IF( SCALAR .LT. -1.0 ) THEN
            RESULT  =  -R90
            TRGOUT  =  .TRUE.
         ELSE
            RESULT  =  ASIN( SCALAR )
         END IF

*    arccosine (radians)

      ELSE IF( TRGFNC .EQ. 'ACOS' ) THEN

         IF( SCALAR .GT. 1.0 .OR. SCALAR .LT. -1.0 ) THEN
            RESULT  =  0.0
            TRGOUT  =  .TRUE.
         ELSE
            RESULT  =  ACOS( SCALAR )
         END IF

*    arctangent (radians)

      ELSE IF( TRGFNC .EQ. 'ATAN' ) THEN

         RESULT  =  ATAN( SCALAR )

*    arcsine (degrees)

      ELSE IF( TRGFNC .EQ. 'ASIND' ) THEN

         IF( SCALAR .GT. 1.0 ) THEN
            RESULT  =  90.0
            TRGOUT  =  .TRUE.
         ELSE IF( SCALAR .LT. -1.0 ) THEN
            RESULT  =  -90.0
            TRGOUT  =  .TRUE.
         ELSE
            RESULT  =  ASIN( SCALAR ) / DGTORD
         END IF

*    arccosine (degrees)

      ELSE IF( TRGFNC .EQ. 'ACOSD' ) THEN

         IF( SCALAR .GT. 1.0 .OR. SCALAR .LT. -1.0 ) THEN
            RESULT  =  0.0
            TRGOUT  =  .TRUE.
         ELSE
            RESULT  =  ACOS( SCALAR ) / DGTORD
         END IF

*    arcsine (degrees)

      ELSE IF( TRGFNC .EQ. 'ATAND' ) THEN

         RESULT  =  ATAN( SCALAR ) / DGTORD

*    unrecognised type - set output same as input and report error

      ELSE

         STATUS  =  SAI__ERROR
         CALL ERR_REP( 'ERR_TRGSCL_BAD',
     :     'TRGSCL: Bad trig. function passed  - no result available '/
     :     /'(result = input)', STATUS )

         RESULT  =  SCALAR

      END IF

      IF( TRGOUT ) THEN
         STATUS  =  SAI__ERROR
         CALL ERR_REP( 'ERR_TRGSCL_OUT',
     :     'TRGSCL: Input scalar was outside function range - no '/
     :     /'result available', STATUS )
      END IF

*    that's it - return

      END

