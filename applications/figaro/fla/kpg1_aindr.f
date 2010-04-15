      SUBROUTINE KPG1_AINDR( LBND, UBND, AXIS, EL, VALUE, INDEX,
     :                        STATUS )
*+
*  Name:
*     KPG1_AINDx

*  Purpose:
*     Obtains for an axis the equivalent index co-ordinates given axis
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AINDx( LBND, UBND, AXIS, EL, VALUE, INDEX, STATUS )

*  Description:
*     This routine determines the indices within an axis array for a
*     series of axis values.  It assumes that the array is
*     monotonically increasing or decreasing, and is approximately
*     linear.  This routine may be used for arbitrary 1-d arrays
*     in addition to axes, provided these criteria are met.
*
*     A Newton's approximation method is used comparing the actual value
*     with a linear approximation.  The upper and lower bounds used to
*     define the increment are adjusted given the deviation from the
*     linear axis.  Once the value lies between adjacent array elements
*     the nearer (by linear interpolation) becomes the required index.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis array.
*     AXIS( LBND:UBND ) = ? (Given)
*        The axis array.
*     EL = INTEGER (Given)
*        The number of values whose indices in the axis are to be found.
*     VALUE( EL ) = ? (Given)
*        The axis-array values.
*     INDEX( EL ) = ? (Returned)
*        The pixel indices of the values in the axis array.
*        Notice that this is floating as fractional positions may be
*        required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for the data types real or double precision:
*     replace "x" in the routine name by R or D respectively, as
*     appropriate.  The axis array and values, and the returned indices
*     should have this data type as well.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 April 29 (MJC):
*        Original version.
*     1991 May 31 (MJC):
*        Converted to generic.
*     1992 July 15 (MJC):
*        Added protection against adjacent axis values being the same.
*     1992 November 19 (MJC):
*        Added protection against lower- and upper-bound axis values
*        being equal.
*     1994 December 6 (MJC):
*        Added protection for values at the end of linear arrays,
*        where the residual is zero.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER
     :  LBND,
     :  UBND,
     :  EL

      REAL
     :  AXIS( LBND:UBND ),
     :  VALUE( EL )

*  Arguments Returned:
      REAL
     :  INDEX( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  CINDEX,                  ! Current pixel index
     :  I,                       ! Loop counter
     :  LB,                      ! Lower bound of the elements
                                 ! containing a data value
     :  LBC,                     ! Corrected lower bound
     :  UB                       ! Upper bound of the elements
                                 ! containing a data value

      DOUBLE PRECISION
     :  DIFF,                    ! Difference used for interpolation
     :  FRAC,                    ! Fractional increment in array index
     :  INCREM,                  ! The increment of axis centres from
                                 ! the lower to the upper bounds
     :  RESID,                   ! Residual of the data value less
                                 ! the linear approximation
     :  SLOPE                    ! The increment of the linear
                                 ! approximation

      LOGICAL                    ! True if:
     :  LOOP                     ! Continuing refining the approximation

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Endeavour to find an axis increment.  This protects against
*    identical axis-centre values at the ends of the axis from making
*    a zero denominator.  Shift the lower bound until a value can be
*    reached or there are insufficient axis values remaining.
      INCREM = 0.0D0
      LBC = LBND

      DO WHILE ( ABS( INCREM ) .LT. VAL__EPSD .AND.
     :           ( UBND - LBC ) .GT. 0 )
         INCREM = NUM_RTOD( AXIS( UBND ) - AXIS( LBC ) )
         IF ( ABS( INCREM ) .LT. VAL__EPSD ) LBC = LBC + 1
      END DO

*    Check that the lower and upper axis centres are still identical.
*    Abort if necessary.
      IF ( ABS( INCREM ) .LT. VAL__EPSD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_AINDX_NONLIN',
     :     'Unable to convert an axis co-ordinate to a pixel index '/
     :     /'because the axis centres have the same value.', STATUS )
         GOTO 999
      END IF

      DO  I = 1, MAX( EL, 1 )

*       Check that the value lies within bounds.

         SLOPE = INCREM

         IF ( ( ( VALUE( I ) .GT. AXIS( LBC ) .OR.
     :            VALUE( I ) .LT. AXIS( UBND ) ) .AND.
     :          SLOPE .LT. 0.0D0 ) .OR. ( SLOPE. GT. VAL__EPSD .AND.
     :        ( VALUE( I ) .LT. AXIS( LBC ) .OR.
     :          VALUE( I ) .GT. AXIS( UBND ) ) ) ) THEN

*          Report the error.

            STATUS = SAI__ERROR
            CALL MSG_SETR( 'VALUE', VALUE( I ) )
            CALL MSG_SETR( 'LVAL', AXIS( LBC ) )
            CALL MSG_SETR( 'UVAL', AXIS( UBND ) )

            CALL ERR_REP( 'KPG1_AINDX_OUTBOUND',
     :        'The data co-ordinate ^VALUE lies outside the '/
     :        /'bounds of the axis (^LVAL to ^UVAL).', STATUS )

*          Want to exit so skip the loop.

            GOTO 999
         END IF

*       Initialise bounds of the search for the "loop until".

         LB = LBC
         UB = UBND
         LOOP = .TRUE.

*       Loop until the pixel matching the data co-ordinate is found.

         DO WHILE( LOOP )

*          Find the slope of the axis, assuming approximately linear
*          co-ordinates.

            SLOPE = NUM_RTOD( AXIS( UB ) - AXIS( LB ) ) /
     :              DBLE( UB - LB )

*          Obtain a guess at the pixel index.

            CINDEX = INT( ( VALUE( I ) - AXIS( LB ) ) / SLOPE ) + LB

*          Find how far is value at the index from the true value.  Stop
*          if the shift is less than the slope.

            RESID = VALUE( I ) - AXIS( CINDEX )

*          If the residual is zero, then we have found the value.  So
*          assign the index to the returned floating-point value, and
*          exit the loop.

            IF ( ABS( RESID ) .LT. VAL__EPSD ) THEN
               INDEX( I ) = NUM_ITOR( CINDEX )
               LOOP = .FALSE.

*          Alter the upper bound.   Form the new loop criterion.
*          Determine whether or not the required value lies between the
*          upper bound and the lower adjacent element.  The loop will
*          stop if it does. There is protection when adjacent axis
*          elements have the same value (probably due to rounding),
*          where the average slope is used to interpolate rather than
*          the difference in axis values.

            ELSE IF ( RESID / SLOPE .LT. 0.0D0 ) THEN
               UB = MIN( CINDEX + 1, UBND )
               DIFF = NUM_RTOD( AXIS( UB - 1 ) - AXIS( UB ) )
               IF ( ABS( DIFF ) .LT. NUM_RTOD( VAL__EPSR ) ) THEN
                  DIFF = SLOPE
               END IF
               FRAC = NUM_RTOD( VALUE( I ) - AXIS( UB ) ) / DIFF

               LOOP = NINT( ABS( FRAC ) ) .GE. 2

*             Evaluate the required index.

               IF ( .NOT. LOOP )
     :           INDEX( I ) = NUM_DTOR( FRAC ) + NUM_ITOR( UB )

*          Alter the lower bound.   Form the new loop criterion.
*          Determine whether or not the required value lies between the
*          lower bound and the upper adjacent element.  The loop will
*          stop if it does. There is protection when adjacent axis
*          elements have the same value (probably due to rounding),
*          where the average slope is used to interpolate rather than
*          the difference in axis values.

            ELSE
               LB = MAX( CINDEX, LBND )
               DIFF = NUM_RTOD( AXIS( LB + 1 ) - AXIS( LB ) )
               IF ( ABS( DIFF ) .LT. NUM_RTOD( VAL__EPSR ) ) THEN
                  DIFF = SLOPE
               END IF
               FRAC = NUM_RTOD( VALUE( I ) - AXIS( LB ) ) / DIFF

               LOOP = NINT( ABS( FRAC ) ) .GE. 2

*             Evaluate the required index.

               IF ( .NOT. LOOP )
     :           INDEX( I ) = NUM_DTOR( FRAC ) + NUM_ITOR( LB )
            END IF
         END DO
      END DO

  999 CONTINUE

      END
