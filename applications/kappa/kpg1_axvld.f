      SUBROUTINE KPG1_AXVLD( LBND, UBND, AXIS, EL, PIXCO, DATCO,
     :                       STATUS )
*+
*  Name:
*     KPG1_AXVLx
 
*  Purpose:
*     Obtains the axis-array values given their corresponding pixel
*     co-ordinates.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_AXVLx( LBND, UBND, AXIS, EL, PIXCO, DATCO, STATUS )
 
*  Description:
*     This routine determines values within an axis array for a series
*     of non-integer pixel co-ordinates.  It assumes that the array is
*     monotonic and approximately linear, since it uses linear
*     interpolation to derive the axis values. This routine may be
*     used for arbitrary one-dimensional arrays in addition to axes,
*     provided this criterion is met.
*
*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis array.
*     AXIS( LBND:UBND ) = ? (Given)
*        The axis array.
*     EL = INTEGER (Given)
*        The number of pixel co-ordinates whose values in the axis
*        array are to be found.
*     PIXCO( EL ) = ? (Given)
*        The pixel co-ordinates of the values in the axis array.
*     DATCO( EL ) = ? (Returned)
*        The axis-array values.  A value is set to the bad value when
*        its input co-ordinate lies outside the range of co-ordinates
*        in the axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for the data types real or double precision:
*     replace "x" in the routine name by R or D respectively, as
*     appropriate.  The axis array and indices, and the returned values
*     should have this data type as well.
*     -  A pixel co-ordinate that lies within the lower-bound or
*     upper-bound element of the axis array but not between elements,
*     and hence cannot have interpolation (i.e. PIXCO is less than LBND
*     - 0.5 or greater than UBND + 0.5) return with the centre data
*     co-ordinate of the lower-bound or upper-bound pixel respectively.
*     -  An error report is made and bad status returned should any
*     input index lie outside the range of the bounds of the axis.
*     Processing will continue through the list.
 
*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 April 29 (MJC):
*        Original version.
*     1991 May 31 (MJC):
*        Converted to generic.
*     1993 March 2 (MJC):
*        Made to continue when an input index is out of bounds.
*     1994 December 8 (MJC):
*        Made to use pixel co-ordinates consistently.  Prevously there
*        was ambiguity whether this routine expect input pixel indices
*        or co-ordinates.  Allowed for the special cases where the
*        value lies within the limiting elements of the axis array, but
*        cannot be interpolated.
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
      INTEGER LBND
      INTEGER UBND
      INTEGER EL
 
      DOUBLE PRECISION AXIS( LBND:UBND )
      DOUBLE PRECISION PIXCO( EL )
 
*  Arguments Returned:
      DOUBLE PRECISION DATCO( EL )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      LOGICAL BAD                ! True if an input value is out of
                                 ! bounds
 
      DOUBLE PRECISION FRAC               ! Fractional array index
      INTEGER I                  ! Loop counter
      INTEGER LB                 ! Truncated pixel index
      DOUBLE PRECISION PIXIND             ! Input co-ordinate as a pixel index
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Set the flag to indicate that none of the input indices are out of
*  range.
      BAD = .FALSE.
 
      DO  I = 1, MAX( EL, 1 )
 
*  Check the pixel co-ordinate does not lie outside the bounds.
         IF ( PIXCO( I ) .LT. NUM_ITOD( LBND ) - 1.0D0 .OR.
     :        PIXCO( I ) .GT. NUM_ITOD( UBND ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'PIXCO', PIXCO( I ) )
            CALL MSG_SETI( 'LBND', LBND - 1 )
            CALL MSG_SETI( 'UBND', UBND )
 
            CALL ERR_REP( 'KPG1_AXVLx_OUTBOUND',
     :        'The pixel co-ordinate ^PIXCO lies outside the '/
     :        /'bounds of the axis (^LBND to ^UBND).', STATUS )
 
*  Temporarily set the status to good so that the co-ordinates of the
*  remaining points may be found.  Record the fact so that a bad status
*  can be reset before exiting.
            STATUS = SAI__OK
            BAD = .TRUE.
 
*  Set the co-ordinate value to be bad.
            DATCO( I ) = VAL__BADD
 
*  Deal with a co-ordinate within the axis lower-bound element, but
*  not between pixels, and so cannot be interpolated.
         ELSE IF ( PIXCO( I ) .LE. NUM_ITOD( LBND ) -
     :             NUM_RTOD( 0.5 ) ) THEN
            DATCO( I ) = AXIS( LBND )
 
*  Deal with a co-ordinate within the axis upper-bound element, but
*  not between pixels, and so cannot be interpolated.
         ELSE IF ( PIXCO( I ) .GE. NUM_ITOD( UBND ) -
     :             NUM_RTOD( 0.5 ) ) THEN
            DATCO( I ) = AXIS( UBND )
 
         ELSE
 
*  Find the next lower integer pixel index.
            PIXIND = PIXCO( I ) + 0.5
            LB = NINT( PIXCO( I ) )
            FRAC = ( PIXIND - NUM_ITOD( LB ) )
 
*  Derive the data value via linear interpolation.
            DATCO( I ) = FRAC * AXIS( LB + 1 ) +
     :                   ( 1.0D0 - FRAC ) * AXIS( LB )
         END IF
      END DO
 
*  Reset the status should any of the values be bad.
      IF ( BAD ) STATUS = SAI__ERROR
 
      END
