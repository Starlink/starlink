      SUBROUTINE KPG1_AXLIR( EL, ARRAY, LVAL, UVAL, LINEAR, STATUS )
*+
*  Name:
*     KPG1_AXLIx
 
*  Purpose:
*     Determines whether an array's values are equally spaced.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_AXLIx( EL, ARRAY, LVAL, UVAL, LINEAR, STATUS )
 
*  Description:
*     This routine determines whether or not adjacent elements of a
*     1-d array have values that are equally spaced, i.e. it tests for
*     linearity.  It simply checks if the intervals between all
*     successive pairs of elements are the same within the machine
*     precision.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the array.  It must be at least
*        two.
*     ARRAY( EL ) = ? (Given)
*        The array to be tested.
*     LVAL = ? (Returned)
*        Value of the first array element.  If this is bad an estimated
*        value is substituted when the array is linear.
*     UVAL = ? (Returned)
*        Value of the last array element.  If this is bad an estimated
*        value is substituted when the array is linear.
*     LINEAR = LOGICAL (Returned)
*        True if the array is linear.
*     STATUS = INTEGER (Given)
*        The global status.
 
*  Notes:
*     -  There is a routine for most numeric data types: replace "x" in
*     the routine name by D, R, I, W, or UW as appropriate.  The array
*     (and the variables for the first and last array elements) supplied
*     to the routine must have the data type specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 April 4 (MJC):
*        Original version based on JM's CON_LNEAR.
*     1992 March 13 (MJC):
*        Improved the test for linearity by using a longer baseline.
*        Allowed for bad values.  Performed all tests in double
*        precision.  Set the maximum difference between the actual
*        and predicted values to be 0.5 for integer data.
*     1993 may 27 (MJC):
*        Used improved algorithm for the linearity test scaling the
*        maximum deviation by maximum absolute value in the array.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT    NONE         ! No implicit typing
 
*  Global Constants:
      INCLUDE    'SAE_PAR'     ! Standard SAE constants
      INCLUDE    'PRM_PAR'     ! PRIMDAT constants
 
*  Arguments Given:
      INTEGER    EL            ! Array size
      REAL     ARRAY( EL )   ! Array to be tested.
 
*  Arguments Returned:
      LOGICAL    LINEAR        ! True if array is linear
      REAL     LVAL          ! Value of first array element.
      REAL     UVAL          ! Value of last array element.
 
*  Status:
      INTEGER    STATUS        ! Global status
 
*  Local Variables:
      DOUBLE PRECISION CURENT  ! Current array value
      DOUBLE PRECISION DIFLIM  ! Tolerance used for comparing intervals
      DOUBLE PRECISION FIRST   ! First non-bad array value
      INTEGER    HIGH          ! Index of last non-bad array value
      INTEGER    I             ! Loop variable
      DOUBLE PRECISION INCREM  ! Value of step size
      INTEGER    LOW           ! Index of first non-bad array value
      DOUBLE PRECISION MAXVAL  ! Maximum absolute value in the array
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions
 
*.
 
*    Check the inherited global status.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*    Assume that the array is not linear until proven otherwise.
 
      LINEAR = .FALSE.
 
*    Set start, and end values using first and last elements assuming
*    these are not bad.
 
      LVAL = ARRAY( 1 )
      UVAL = ARRAY( EL )
 
*    Find the increment assuming for the moment that it is a linear
*    array by using the longest baseline.  Exclude any bad data at
*    the ends of the array.
 
      LOW = 1
      DO WHILE ( ARRAY( LOW ) .EQ. VAL__BADR .AND. LOW .LT. EL )
         LOW = LOW + 1
      END DO
      HIGH = EL
      DO WHILE ( ARRAY( HIGH ) .EQ. VAL__BADR .AND. HIGH .GT. LOW )
         HIGH = HIGH - 1
      END DO
 
*    Report an error when there is one or no good values.
 
      IF ( LOW .GE. HIGH ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_AXLIx_BADAT',
     :     'The test for linear spacing within an array has failed '/
     :     /'because the array has one or no good values.', STATUS )
         GOTO 999
      END IF
 
*    Evaluate the linear increment.
 
      INCREM = ( NUM_RTOD( ARRAY( HIGH ) )
     :         - NUM_RTOD( ARRAY( LOW ) ) ) / NUM_ITOR( HIGH - LOW )
 
*    Find the biggest absolute value in the array.
 
      MAXVAL = -1.0D0
      DO I = LOW, HIGH
         IF ( ARRAY( I ) .NE. VAL__BADR ) THEN
            MAXVAL = MAX( MAXVAL, ABS( NUM_RTOD( ARRAY( I ) ) ) )
         END IF
      END DO
 
*    Find the smallest allowed difference in the intervals.  Integer
*    intervals need only be different by half of one---the machine
*    precision by definition---for non-linearity.  Floating-point uses
*    an arbitrary factor times the floating-point machine precision in
*    units of the increment to test for linearity.  The factor should
*    allow for rounding errors and a full decade of values.
 
      IF ( VAL__EPSR .EQ. 1.0E0 ) THEN
         DIFLIM = 0.5D0
      ELSE
         DIFLIM = 11.0E0 * VAL__EPSR * MAXVAL
      END IF
 
*    Check that each pair of successive elements are the same interval
*    apart in value as the previous pair.  If not, the array is
*    non-linear.  There must be at least two valid values to test for
*    linearity.
 
      FIRST = NUM_RTOD( ARRAY( LOW ) )
      IF ( ( HIGH - LOW ) .GT. 1 ) THEN
         DO I = LOW + 1, HIGH
 
*          Ignore bad values from the test.
 
            IF ( ARRAY( I ) .NE. VAL__BADR ) THEN
 
*             The tolerance for deciding that the intervals are the
*             same is achieved by comparing the difference between the
*             predicted value---given linear data---and the actual
*             value, against a few times the machine precision at the
*             maximum absolute value.
 
               CURENT = NUM_RTOD( ARRAY( I ) )
               IF ( ABS( CURENT - FIRST - INCREM * DBLE( I - LOW ) )
     :              .GT. DIFLIM ) GO TO 999
 
            END IF
         END DO
      END IF
 
*    The loop was completed therefore the interval in array values is
*    constant.
 
      LINEAR = .TRUE.
 
*    Set the limiting values if either was bad by extrapolating the
*    linear array.
 
      IF ( ARRAY( 1 ) .EQ. VAL__BADR ) THEN
         LVAL = NUM_DTOR( NUM_RTOD( ARRAY( LOW ) ) -
     :          INCREM * DBLE( LOW - 1 ) )
      END IF
 
      IF ( ARRAY( EL ) .EQ. VAL__BADR ) THEN
         UVAL = NUM_DTOR( NUM_RTOD( ARRAY( HIGH ) ) +
     :          INCREM * DBLE( EL - HIGH ) )
      END IF
 
  999 CONTINUE
 
      END
