      SUBROUTINE RTD_AXLID( EL, ARRAY, LVAL, UVAL, REFPIX, REFVAL, 
     :                      LINEAR, STATUS )
*+
*  Name:
*     RTD_AXLID
 
*  Purpose:
*     Determines whether an array's values are equally spaced.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL_ RTD_AXLID( EL, ARRAY, LVAL, UVAL, REFPIX, REFVAL, 
*                      LINEAR, STATUS )
 
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
*     ARRAY( EL ) = DOUBLE PRECISION (Given)
*        The array to be tested.
*     LVAL = DOUBLE PRECISION (Returned)
*        Value of the first array element.  If this is bad an estimated
*        value is substituted when the array is linear.
*     UVAL = DOUBLE PRECISION (Returned)
*        Value of the last array element.  If this is bad an estimated
*        value is substituted when the array is linear.
*     REFPIX = INTEGER (Returned)
*        Position of a pixel near the middle of the array. This can
*        be used as a reference point.
*     REFVAL = DOUBLE PRECISION (Returned)
*         Value at reference pixel.
*     LINEAR = LOGICAL (Returned)
*        True if the array is linear.
*     STATUS = INTEGER (Given)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PWD: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}
 
*  History:
*     1992 September 16 (MJC):
*        Original version renamed from KAPPA's KPG1_AXLIX
*     1996 March 7 (PWD):
*        Renamed RTD_AXLID and no longer generic.
*     1997 November 21 (PWD):
*        Added reference pixel arguments.
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
      INTEGER    EL             ! Array size
      DOUBLE PRECISION     ARRAY( EL ) ! Array to be tested.
 
*  Arguments Returned:
      LOGICAL    LINEAR         ! True if array is linear
      DOUBLE PRECISION     LVAL ! Value of first array element.
      DOUBLE PRECISION     UVAL ! Value of last array element.
      INTEGER REFPIX            ! Reference pixel position
      DOUBLE PRECISION REFVAL   ! Reference pixel value
 
*  Status:
      INTEGER    STATUS         ! Global status
 
*  Local Variables:
      DOUBLE PRECISION CURENT   ! Current array value
      DOUBLE PRECISION DELLIM   ! Rounding increment to the tolerance
      DOUBLE PRECISION DIFLIM   ! Tolerance used for comparing intervals
      INTEGER HIGH              ! Index of last non-bad array value
      INTEGER I                 ! Loop variable
      DOUBLE PRECISION INCREM   ! Value of step size
      DOUBLE PRECISION LAST     ! Previous array value
      INTEGER LOW               ! Index of first non-bad array value
      DOUBLE PRECISION PREDCT   ! Predicted value if it is linear
      INTEGER STEP              ! Difference in pixel index of the two
                                ! array elements being compared.
      
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
      DO WHILE ( ARRAY( LOW ) .EQ. VAL__BADD .AND. LOW .LT. EL )
         LOW = LOW + 1
      END DO
      HIGH = EL
 10   CONTINUE
      IF ( ARRAY( HIGH ) .EQ. VAL__BADD .AND. HIGH .GT. LOW ) THEN
         HIGH = HIGH - 1
         GO TO 10
      END IF
 
*    Report an error when there is one or no good values.
      IF ( LOW .GE. HIGH ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CON_AXLIx_BADAT',
     :     'The test for linear spacing within an array has failed '/
     :     /'because the array has one or no good values.', STATUS )
         GOTO 999
      END IF
 
*    Evaluate the linear increment.
      INCREM = ( ARRAY( HIGH )- ARRAY( LOW ) ) / DBLE( HIGH - LOW )
 
*    Find the smallest allowed difference in the intervals.  Integer
*    intervals need only be different by half of one---the machine
*    precision by definition---for non-linearity.  Floating-point uses
*    an arbitrary factor times the floating-point machine precision in
*    units of the increment to test for linearity.  This ought be
*    enough even for double precision and caters for the case where
*    single precision axis data have been converted to double
*    precision.  The factor should allow for rounding errors and a full
*    decade of values.
      IF ( VAL__EPSD .EQ. 1.0D0 ) THEN
         DIFLIM = 0.5D0
         DELLIM = 0.0D0
      ELSE
         DIFLIM = 11.0D0 * DBLE( VAL__EPSR )
         DELLIM = DBLE( VAL__EPSR )
      END IF
 
*    Check that each pair of successive elements are the same interval
*    apart in value as the previous pair.  If not, the array is
*    non-linear.
      LAST = ARRAY( LOW )
      STEP = 1
      IF ( ( HIGH - LOW ) .GT. 1 ) THEN
         DO 11 I = LOW + 1, HIGH
            CURENT = ARRAY( I )
 
*    Count groups of bad pixels so that the expected increment can be
*    derived.
            IF ( ARRAY( I ) .EQ. VAL__BADD ) THEN
               STEP = STEP + 1
            ELSE
 
*    The tolerance for deciding that the intervals are the same is
*    achieved by comparing the difference between the predicted
*    value---given linear data---and the actual value, against a few
*    times the machine precision at the current value.  Note for
*    floating-point data, the rounding errors accumulate and so the test
*    has to be eased along the array.
               PREDCT = LAST + DBLE( STEP ) * INCREM
               DIFLIM = DIFLIM + DELLIM
               IF ( ABS( CURENT - PREDCT ) .GE. DIFLIM *
     :              ABS( CURENT ) * DBLE( STEP ) ) GO TO 999
 
*    Reset the number of steps as the test has been made.  Also make the
*    current value the last value ready for the next comparison.
               STEP = 1
               LAST = CURENT
            END IF
 11      CONTINUE
      END IF
 
*    The loop was completed therefore the interval in array values is
*    constant.
      LINEAR = .TRUE.
 
*    Set the limiting values if either was bad by extrapolating the
*    linear array.
      IF ( ARRAY( 1 ) .EQ. VAL__BADD ) THEN
         LVAL = ARRAY( LOW ) - INCREM * DBLE( LOW - 1 )
      END IF
 
      IF ( ARRAY( EL ) .EQ. VAL__BADD ) THEN
         UVAL = ARRAY( HIGH ) + INCREM * DBLE( EL - HIGH )
      END IF

*     Determine a suitable reference pixel position and return its
*     value. 
      REFPIX = MAX( 1, MIN( EL, EL / 2 ) )
      REFVAL = ARRAY( REFPIX )
 
  999 CONTINUE
      END
