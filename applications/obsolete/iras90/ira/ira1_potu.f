      SUBROUTINE IRA1_POTU( XLO, YLO, XHI, YHI, INCNT, MAXLAB, LABS,
     :                      NLABS, STATUS )
*+
*  Name:
*     IRA1_POTU

*  Purpose:
*     See if the stored "end" label info can be used.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_POTU( XLO, YLO, XHI, YHI, INCNT, MAXLAB, LABS, NLABS,
*                     STATUS )

*  Description:
*     The labels are sorted so that the longitude or latitude values are
*     in increasing order. A limit is then put on how close together
*     labels can appear on the screen (to avoid labels merging), and
*     labels which are too close to each other are removed from
*     the list. In order to be usable, at least a third of the meridians
*     or parallels which intersect the plotting space must have end
*     labels. An absolute minimum of 2 end labels is imposed.

*  Arguments:
*     XLO = REAL (Given)
*        The low X bound of the plotting space.
*     YLO = REAL (Given)
*        The low Y bound of the plotting space.
*     XHI = REAL (Given)
*        The high X bound of the plotting space.
*     YHI = REAL (Given)
*        The high Y bound of the plotting space.
*     INCNT = INTEGER (Given)
*        The number of meridians or parallels which intersected the
*        plotting space.
*     MAXLAB = INTEGER (Given and Returned)
*        The size of the LABS array.
*     LABS( MAXLAB, 5 ) = DOUBLE PRECISION (Given and Returned)
*        The end label information. On exit, close pairs of labels are
*        purged.
*     NLABS = INTEGER (Given and Returned)
*        The number of labels stored in LABS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink constants

*  Arguments Given:
      REAL XLO
      REAL YLO
      REAL XHI
      REAL YHI
      INTEGER INCNT
      INTEGER MAXLAB

*  Arguments Given and Returned:
      DOUBLE PRECISION LABS( MAXLAB, 5 )
      INTEGER NLABS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER J
      DOUBLE PRECISION SEPLIM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill unused elements of LABS with bad values.
      DO I = NLABS + 1 , MAXLAB
         DO J = 1, 5
            LABS( I, J ) = VAL__BADD
         END DO
      END DO

*  Order the sky coordinate values into increasing order.
      CALL IRA1_SORTD( .TRUE., 1, MAXLAB, 5, LABS, NLABS, STATUS )

*  Remove labels which are too close together.
      SEPLIM = DBLE( 0.03*MAX( XHI - XLO, YHI - YLO ) )

      DO I = 2, NLABS

         IF( ABS( LABS( I, 3 ) - LABS( I - 1, 3 ) ) .LT.
     :       SEPLIM .AND.
     :       ABS( LABS( I, 2 ) - LABS( I - 1, 2 ) ) .LT.
     :       SEPLIM ) THEN

            LABS( I - 1, 1 ) = VAL__BADD
            LABS( I, 1 ) = VAL__BADD

         END IF

      END DO

*  Re-order the sky coordinate values to remove the bad values.
      CALL IRA1_SORTD( .TRUE., 1, MAXLAB, 5, LABS, NLABS, STATUS )

*  If less than 1 third of the curves could be labeled using the
*  remaining label information, then another method will have to be
*  used.
      IF( NLABS .LT. MAX( 2, INCNT/3 ) ) NLABS = 0

      END
