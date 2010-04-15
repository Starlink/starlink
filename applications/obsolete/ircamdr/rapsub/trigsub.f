
*+  TRIGSUB - transforms input to output using a trig function

      SUBROUTINE TRIGSUB ( INARRAY, DIMS1, DIMS2, TRIGFUNC, OUTARRAY,
     :                     STATUS )

*    Description :
*
*     According to the input value given for the string TRIGFUNC,
*     the input image array INARRAY is transformed using the
*     given function to the output image OUTARRAY.
*     Depending on the string passed, radian or degree trig
*     functions will be used.
*
*    Invocation :
*
*     CALL TRIGSUB( INARRAY, DIMS, TRIGFUNC, OUTARRAY, STATUS )
*
*    Method :
*
*     The string TRIGFUNC is examined, and depending on what value
*     it contains, the relevant loop is executed, setting the output
*     array pixel values to the corresponding input pixels, transformed
*     by the requested trigonometrical function.
*     Checking is employed in the TAN and TAND loops, as they may
*     potentially cause overflow if an input pixel is set very close
*     to 90.0 or 270.0 degrees.
*     If pixels are found during arcsine or arccos transforms
*     which are outwith the range -1 to 1, then TRIGFUNC is
*     set to 'OUT' on return.
*     If an invalid trig function is input, the output array is
*     merely copied from the input, and TRIGFUNC is set to the
*     value 'BAD'.
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
*     10-12-1985 :  First implementation
*                :  (UKTH::MARK)
*     17-July-1994  Converted angles to radians to avoid VAX-specific
*                   trig functions, changed arguments to input DIMS
*                   separately so that routine will still compile (SKL@JACH)
*
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :     DIMS1,        ! dimensions of input and output arrays
     :     DIMS2         ! dimensions of input and output arrays

      REAL
     :     INARRAY( DIMS1, DIMS2 )   ! input array

*    Import-Export :

      CHARACTER*(*)
     :     TRIGFUNC         ! chosen trig function to be applied

*    Export :

      REAL
     :     OUTARRAY( DIMS1, DIMS2 )  ! output array

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

      INTEGER
     :     I, J             ! general array counter variables

      REAL
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

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               OUTARRAY( I, J )  =  SIN( INARRAY( I, J ) )
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'COS' ) THEN      ! cosine (radians)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               OUTARRAY( I, J )  =  COS( INARRAY( I, J ) )
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'TAN' ) THEN      ! tangent (radians)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1

*             check for pixel values that could cause overflow
               DUMMY  =  MOD( INARRAY( I, J ), PI )

*             evaluate logicals that allow checking to see if
*             tangent functions will overflow

*             if value is just below or equal to pi/2 radians
               TAN_HIGH_POS  =  ( DUMMY .GT. R90M .AND. DUMMY .LE. R90 )

*             if value is just above pi/2 radians
               TAN_HIGH_NEG  =  ( DUMMY .GE. R90 .AND. DUMMY .LT. R90P )

               IF( TAN_HIGH_POS ) THEN
                  OUTARRAY( I, J )  =  DATAMAX
               ELSEIF( TAN_HIGH_NEG ) THEN
                  OUTARRAY( I, J )  =  -DATAMAX
               ELSE
                  OUTARRAY( I, J )  =  TAN( INARRAY( I, J ) )
               END IF
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'SIND' ) THEN      ! sine (degrees)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               OUTARRAY( I, J )  =  SIN( INARRAY( I, J ) * DTOR )
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'COSD' ) THEN      ! cosine (degrees)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               OUTARRAY( I, J )  =  COS( INARRAY( I, J ) * DTOR )
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'TAND' ) THEN      ! tangent (degrees)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1

*             check for pixel values that could cause overflow
               DUMMY  =  MOD( INARRAY( I, J ), 180.0 )

*             evaluate logicals that allow you to see if
*             tangent functions will overflow

*             if value is just below or equal to 90 degrees
               TAND_HIGH_POS =  ( DUMMY .GT. D90M
     :                            .AND. DUMMY .LE. 90.0 )

*             if value is just above 90 degrees
               TAND_HIGH_NEG =  ( DUMMY .GE. 90.0
     :                            .AND. DUMMY .LT. D90P )

               IF( TAND_HIGH_POS ) THEN
                  OUTARRAY( I, J )  =  DATAMAX
               ELSEIF( TAND_HIGH_NEG ) THEN
                  OUTARRAY( I, J )  =  -DATAMAX
               ELSE
                  OUTARRAY( I, J )  =  TAN( INARRAY( I, J ) * DTOR )
               END IF
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'ASIN' ) THEN      ! arcsine (radians)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               IF( INARRAY( I, J ) .GT. 1.0 ) THEN
                  OUTARRAY( I, J )  =  R90
                  TRIGFUNC  =  'OUT'
               ELSEIF( INARRAY( I, J ) .LT. -1.0 ) THEN
                  OUTARRAY( I, J )  =  -R90
                  TRIGFUNC  =  'OUT'
               ELSE
                  OUTARRAY( I, J )  =  ASIN( INARRAY( I, J ) )
               END IF
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'ACOS' ) THEN      ! arccosine (radians)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               IF( INARRAY( I, J ) .GT. 1.0 .OR.
     :             INARRAY( I, J ) .LT. -1.0 ) THEN
                  OUTARRAY( I, J )  =  0.0
                  TRIGFUNC  =  'OUT'
               ELSE
                  OUTARRAY( I, J )  =  ACOS( INARRAY( I, J ) )
               END IF
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'ATAN' ) THEN      ! arctangent (radians)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               OUTARRAY( I, J )  =  ATAN( INARRAY( I, J ) )
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'ASIND' ) THEN      ! arcsine (degrees)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               IF( INARRAY( I, J ) .GT. 1.0 ) THEN
                  OUTARRAY( I, J )  =  90.0
                  TRIGFUNC  =  'OUT'
               ELSEIF( INARRAY( I, J ) .LT. -1.0 ) THEN
                  OUTARRAY( I, J )  =  -90.0
                  TRIGFUNC  =  'OUT'
               ELSE
                  OUTARRAY( I, J )  =  ASIN( INARRAY( I, J ) )
                  OUTARRAY( I, J )  =  OUTARRAY( I, J ) / DTOR
               END IF
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'ACOSD' ) THEN      ! arccosine (degrees)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               IF( INARRAY( I, J ) .GT. 1.0 .OR.
     :             INARRAY( I, J ) .LT. -1.0 ) THEN
                  OUTARRAY( I, J )  =  0.0
                  TRIGFUNC  =  'OUT'
               ELSE
                  OUTARRAY( I, J )  =  ACOS( INARRAY( I, J ) )
                  OUTARRAY( I, J )  =  OUTARRAY( I, J ) / DTOR
               END IF
            END DO
         END DO

      ELSEIF( TRIGFUNC .EQ. 'ATAND' ) THEN      ! arctangent (degrees)

         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               OUTARRAY( I, J )  =  ATAN( INARRAY( I, J ) )
               OUTARRAY( I, J )  =  OUTARRAY( I, J ) / DTOR
            END DO
         END DO

      ELSE             ! unrecognised type - set output same as input
                       ! and set TRIGFUNC to error

         TRIGFUNC  =  'BAD'
         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               OUTARRAY( I, J )  =  INARRAY( I, J )
            END DO
         END DO

      END IF


*    that's it - return

      END
