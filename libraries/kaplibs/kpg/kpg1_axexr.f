      SUBROUTINE KPG1_AXEXR( EL, CENTRE, USEERR, ERROR, USEWID, WIDTH,
     :                         NSIGMA, ASTART, AEND, STATUS )
*+
*  Name:
*     KPG1_AXEXx
 
*  Purpose:
*     Calculates the extent of an NDF along an axis.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_AXEXx( EL, CENTRE, USEERR, ERROR, USEWID, WIDTH,
*                      NSIGMA, ASTART, AEND, STATUS )
 
*  Description:
*     This routine calculates the starting and ending positions of an
*     NDF's pixels along an axis, optionally taking account of the
*     axis width and error values.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the axis arrays.
*     CENTRE( EL ) = ? (Given)
*        The centres of the pixels on the axis.
*     USEERR = LOGICAL (Given)
*        Use the error array and NSIGMA in the calculation of the
*        extent of the axis.
*     ERROR( EL ) = ? (Given)
*        The errors of the pixel centres on the axis.  It is only
*        required (accessed) when USEERR = .TRUE..
*     USEWID = LOGICAL (Given)
*        Use the width array in the calculation of the extent of the
*        axis.
*     WIDTH( EL ) = ? (Given)
*        The widths of the pixels on the axis.
*     NSIGMA = ? (Given)
*        Number of multiples of the error to use in the calculation
*     ASTART = ? (Returned)
*        If the axis centre positions increase with NDF pixel index,
*        this argument returns the axis position of the edge of the
*        first pixel which has the lower co-ordinate. Otherwise it
*        returns the axis position of the edge with the higher
*        co-ordinate.
*     AEND = ? (Returned)
*        If the axis centre positions increase with NDF pixel index,
*        this argument returns the axis position of the edge of the
*        last pixel which has the higher co-ordinate. Otherwise it
*        returns the axis position of the edge with the lower
*        co-ordinate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1996 October 2 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
 
*  Arguments Given:
      INTEGER EL
      REAL CENTRE( EL )
      LOGICAL USEERR
      REAL ERROR( EL )
      LOGICAL USEWID
      REAL WIDTH( EL )
      REAL NSIGMA
 
*  Arguments Returned:
      REAL ASTART
      REAL AEND
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      REAL MAXE               ! Maximum axis centre plus error
      INTEGER MAXPOS             ! Index of maximum centre plus error
      REAL MINE               ! Minimum axis centre plus error
      INTEGER MINPOS             ! Index of minimum centre plus error
      INTEGER NINVAL             ! Number of bad centres and/or errors
      REAL THRESH( 2 )        ! Thresholds for finding the range
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  The widths must not overlap but the extreme axis centre plus or
*  minus an error (especially if NSIGMA is greater than one) can.
*  So work through the whole axis centre and error arrays to find the
*  maximum and minimum.
      IF ( USEERR ) THEN
         THRESH( 1 ) = VAL__MINR
         THRESH( 2 ) = VAL__MAXR
         CALL KPG1_MXMER( .TRUE., EL, CENTRE, ERROR, NSIGMA, THRESH,
     :                      NINVAL, MAXE, MINE, MAXPOS, MINPOS, STATUS )
      END IF
 
*  If the axis centre values increase with pixel index, then calculate
*  the extent.
      IF ( CENTRE( 1 ) .LE. CENTRE( EL ) ) THEN
 
*  Include both the width and the errors in the calculation.
         IF ( USEERR .AND. USEWID ) THEN
            ASTART = MIN( CENTRE( 1 ) - ABS( WIDTH( 1 ) ) / 2.0E0,
     :               MINE )
            AEND = MAX( CENTRE( EL ) + ABS( WIDTH( EL ) ) / 2.0E0,
     :             MAXE )
 
*  Allow for the errors.
         ELSE IF ( USEERR ) THEN
            ASTART = MINE
            AEND = MAXE
 
*  Allow for the widths.
         ELSE IF ( USEWID ) THEN
            ASTART = CENTRE( 1 ) - ABS( WIDTH ( 1 ) ) / 2.0E0
            AEND = CENTRE( EL ) +  ABS( WIDTH ( EL ) ) / 2.0E0
 
*  Just return the limiting axis centres.
         ELSE
            ASTART = CENTRE( 1 )
            AEND = CENTRE( EL )
 
         END IF
 
*  Otherwise calculate the extent appropriate to decreasing axis centre
*  values.
      ELSE
         IF ( USEERR .AND. USEWID ) THEN
            ASTART = MAX( CENTRE( 1 ) + ABS( WIDTH( 1 ) ) / 2.0E0,
     :               MAXE )
            AEND = MAX( CENTRE( EL ) - ABS( WIDTH( EL ) ) / 2.0E0,
     :             MINE )
 
*  Allow for the errors.
         ELSE IF ( USEERR ) THEN
            ASTART = MAXE
            AEND = MINE
 
*  Allow for the widths.
         ELSE IF ( USEWID ) THEN
            ASTART = CENTRE( 1 ) + ABS( WIDTH ( 1 ) ) / 2.0E0
            AEND = CENTRE( EL ) - ABS( WIDTH ( EL ) ) / 2.0E0
 
*  Just return the limiting axis centres.
         ELSE
            ASTART = CENTRE( 1 )
            AEND = CENTRE( EL )
 
         END IF
      END IF
 
      END
