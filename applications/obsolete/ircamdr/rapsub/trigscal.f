
*+  TRIGSCAL - transforms scalar input to output using a trig function

      SUBROUTINE TRIGSCAL ( SCALAR, TRIGFUNC, RESULT, STATUS )

*    Description :
*
*     According to the input value given for the string TRIGFUNC,
*     the input scalar is transformed using the given function
*     and is returned in the result argument.
*     Depending on the string passed, radian or degree trig
*     functions will be used.
*
*    Invocation :
*
*     CALL TRIGSCAL( SCALAR, TRIGFUNC, RESULT, STATUS )
*
*    Method :
*
*     The string TRIGFUNC is examined, and depending on what value
*     it contains, the relevant loop is executed, operating on the
*     input scalar value with the requested trigonometrical function.
*     Checking is employed in the TAN and TAND loops, as they may
*     potentially cause overflow if the input scalar is very close
*     to 90.0 or 270.0 degrees.
*     If the scalar given to arcsine or arccos transforms is outside
*     te valid range of these functions, the string TRIGFUNC is
*     set to 'OUT' on return.
*     If an invalid trig function is input, the output array is
*     merely copied from the input, and TRIGFUNC is set to the
*     value 'BAD'.
*     Numbers input larger than 360 degrees and its radian equivalent
*     are merely taken as mod(180) or mod(pi) accordingly.
*
*    Deficiencies :
*
*     Uses Vax Fortran extensions for degree trig functions.
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
*     08-01-1986 :  First implementation
*                :  (REVA::MJM)
*     17-July-1994  Converted angles to/from radians to avoid VAX-specific
*                   trig functions (SKL@JACH)
*
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      REAL
     :     SCALAR           ! input scalar value to be operated on

*    Import-Export :

      CHARACTER*(*)
     :     TRIGFUNC         ! chosen trig function to be applied

*    Export :

      REAL
     :     RESULT           ! result of trig operation

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      REAL
     :     PI,              ! pi
     :     DATAMAX,         ! value for pixel when overflow occurs
     :     R90,             ! radian equivalent of pi/2
     :     R90M,            ! radian overflow value just below pi/2
     :     R90P,            !    "       "      "     "  above pi/2
     :     D90M,            ! degree     "      "     "  below 90
     :     D90P,            !    "       "      "     "  above 90
     :     DTOR             ! factor for converting degrees to radians

      PARAMETER ( DTOR = 3.141592 / 180.0 )

      PARAMETER( PI  =  3.141592653 )
      PARAMETER( DATAMAX  =  1.0E20 )
      PARAMETER( R90  =  PI / 2.0 )
      PARAMETER( R90M  =  R90 - 0.0000001 )
      PARAMETER( R90P  =  R90 + 0.00000001 )
      PARAMETER( D90M  =  90.0 - 0.00001 )
      PARAMETER( D90P  =  90.0 + 0.00001 )

*    Local variables :

      REAL
     :     ANGLE,           ! angle in radians
     :     DUMMY            ! dummy variable used when checking
                            ! for overflow with TAN and TAND

      LOGICAL
     :     TAN_HIGH_POS,    ! if radian tangent will positive overflow
     :     TAN_HIGH_NEG,    !  "    "      "      "  negative     "
     :     TAND_HIGH_POS,   !  " degree    "      "  positive     "
     :     TAND_HIGH_NEG    !  "    "      "      "  negative     "

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    start by forcing input function string to uppercase

      CALL UPCASE( TRIGFUNC, TRIGFUNC, STATUS )


*    now check through all valid trigonometrical function options
*    and filling output array with transformed version of input
*    array

      IF( TRIGFUNC .EQ. 'SIN' ) THEN       ! sine (radians)

         RESULT  =  SIN( SCALAR )

      ELSEIF( TRIGFUNC .EQ. 'COS' ) THEN      ! cosine (radians)

         RESULT  =  COS( SCALAR )

      ELSEIF( TRIGFUNC .EQ. 'TAN' ) THEN      ! tangent (radians)

*       check for a scalar that could cause overflow
         DUMMY  =  MOD( SCALAR, PI )

*       evaluate logicals that allow checking to see if
*       tangent functions will overflow

*       if value is just below or equal to pi/2 radians
         TAN_HIGH_POS  =  ( DUMMY .GT. R90M .AND. DUMMY .LE. R90 )

*       if value is just above pi/2 radians
         TAN_HIGH_NEG  =  ( DUMMY .GE. R90 .AND. DUMMY .LT. R90P )

         IF( TAN_HIGH_POS ) THEN
            RESULT  =  DATAMAX
         ELSEIF( TAN_HIGH_NEG ) THEN
            RESULT  =  -DATAMAX
         ELSE
            RESULT  =  TAN( SCALAR )
         END IF

      ELSEIF( TRIGFUNC .EQ. 'SIND' ) THEN      ! sine (degrees)

*       first convert degrees to radians
         ANGLE = SCALAR * DTOR
         RESULT  =  SIN( ANGLE )

      ELSEIF( TRIGFUNC .EQ. 'COSD' ) THEN      ! cosine (degrees)

*       first convert degrees to radians
         ANGLE = SCALAR * DTOR
         RESULT  =  COS( ANGLE )

      ELSEIF( TRIGFUNC .EQ. 'TAND' ) THEN      ! tangent (degrees)

*       check for pixel values that could cause overflow
         DUMMY  =  MOD( SCALAR, 180.0 )

*       evaluate logicals that allow you to see if
*       tangent functions will overflow

*       if value is just below or equal to 90 degrees
         TAND_HIGH_POS =  ( DUMMY .GT. D90M
     :                      .AND. DUMMY .LE. 90.0 )

*       if value is just above 90 degrees
         TAND_HIGH_NEG =  ( DUMMY .GE. 90.0
     :                      .AND. DUMMY .LT. D90P )

        IF( TAND_HIGH_POS ) THEN
           RESULT  =  DATAMAX
         ELSEIF( TAND_HIGH_NEG ) THEN
           RESULT  =  -DATAMAX
         ELSE
*         first convert degrees to radians
           ANGLE = SCALAR * DTOR
           RESULT  =  TAN( ANGLE )
         END IF

      ELSEIF( TRIGFUNC .EQ. 'ASIN' ) THEN      ! arcsine (radians)

         IF( SCALAR .GT. 1.0 ) THEN
            RESULT  =  R90
            TRIGFUNC  =  'OUT'
         ELSEIF( SCALAR .LT. -1.0 ) THEN
            RESULT  =  -R90
            TRIGFUNC  =  'OUT'
         ELSE
            RESULT  =  ASIN( SCALAR )
         END IF

      ELSEIF( TRIGFUNC .EQ. 'ACOS' ) THEN      ! arccosine (radians)

         IF( SCALAR .GT. 1.0 .OR. SCALAR .LT. -1.0 ) THEN
            RESULT  =  0.0
            TRIGFUNC  =  'OUT'
         ELSE
            RESULT  =  ACOS( SCALAR )
         END IF

      ELSEIF( TRIGFUNC .EQ. 'ATAN' ) THEN      ! arctangent (radians)

         RESULT  =  ATAN( SCALAR )

      ELSEIF( TRIGFUNC .EQ. 'ASIND' ) THEN      ! arcsine (degrees)

         IF( SCALAR .GT. 1.0 ) THEN
            RESULT  =  90.0
            TRIGFUNC  =  'OUT'
         ELSEIF( SCALAR .LT. -1.0 ) THEN
            RESULT  =  -90.0
            TRIGFUNC  =  'OUT'
         ELSE
            RESULT  =  ASIN( SCALAR )
*          convert radians to degrees
            RESULT = RESULT / DTOR
         END IF

      ELSEIF( TRIGFUNC .EQ. 'ACOSD' ) THEN      ! arccosine (degrees)

         IF( SCALAR .GT. 1.0 .OR. SCALAR .LT. -1.0 ) THEN
            RESULT  =  0.0
            TRIGFUNC  =  'OUT'
         ELSE
            RESULT  =  ACOS( SCALAR )
*          convert radians to degrees
            RESULT = RESULT / DTOR
         END IF

      ELSEIF( TRIGFUNC .EQ. 'ATAND' ) THEN      ! arctangent (degrees)

         RESULT  =  ATAN( SCALAR )
*       convert radians to degrees
         RESULT = RESULT / DTOR


      ELSE             ! unrecognised type - set output same as input
                       ! and set TRIGFUNC to error

         RESULT  =  SCALAR
         TRIGFUNC  =  'BAD'

      END IF


*    that's it - return

      END
