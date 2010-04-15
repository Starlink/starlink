      SUBROUTINE IRM_PTAST( PICID, IDA, STATUS )
*+
*  Name:
*     IRM_PTAST

*  Purpose:
*     Associate an astrometry structure with a picture in the AGI
*     database.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_PTAST( PICID, IDA, STATUS )

*  Description:
*     The astrometry information identified by IDA is copied to the
*     MORE structure associated with a given picture in the AGI
*     database.  If an astrometry structure already exists in MORE,
*     then the routine returns without doing anything.

*  Arguments:
*     PICID = INTEGER (Given)
*        The AGI identifier for the picture with which to associate the
*        astrometry information.
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Arguments Given:
      INTEGER PICID
      INTEGER IDA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to component of MORE.
      CHARACTER MLOC*(DAT__SZLOC)! Locator to MORE.
      CHARACTER TYPE*(DAT__SZTYP)! Component type.

      INTEGER I                  ! Component index.
      INTEGER NCOMP              ! No. of components in MORE.

      LOGICAL AGAIN              ! True if more components remain to be
                                 ! checked.
      LOGICAL THERE              ! True if an object exists.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the picture already has an associated MORE structure.
      CALL AGI_IMORE( PICID, THERE, STATUS )

*  If so, get a locator to it.
      IF( THERE ) THEN
         CALL AGI_MORE( PICID, 'UPDATE', MLOC, STATUS )

*  Find the number of components stored in the MORE structure.
         CALL DAT_NCOMP( MLOC, NCOMP, STATUS )

*  Loop round the list of components in order to see if there is already
*  a component with HDS type "IRAS_ASTROMETRY".
         I = 1
         THERE = .FALSE.

         AGAIN = .TRUE.
         DO WHILE( AGAIN .AND. STATUS .EQ. SAI__OK )

*  Get the HDS type of the I'th component.
            CALL DAT_INDEX( MLOC, I, CLOC, STATUS )
            CALL DAT_TYPE( CLOC, TYPE, STATUS )

*  Set the flag and end the loop if this is an IRAS astrometry
*  structure.
            IF( TYPE .EQ. IRA__HDSTY ) THEN
               THERE = .TRUE.
               AGAIN = .FALSE.

*  Otherwise, increment to the next component if any components are
*  left.
            ELSE

               IF( I .LT. NCOMP ) then
                  I = I + 1
               ELSE
                  AGAIN = .FALSE.
               END IF

            END IF

*  Annul the locator to the component.
            CALL DAT_ANNUL( CLOC, STATUS )

         END DO

*  If the picture does not currently have an associated MORE structure,
*  get a locator to a new one.
      ELSE
         CALL AGI_MORE( PICID, 'WRITE', MLOC, STATUS )

      END IF

*  If the MORE structure does not currently contain an IRAS astrometry
*  structure, create one now.
      IF( .NOT. THERE ) THEN
         CALL DAT_NEW( MLOC, 'ASTROMETRY', IRA__HDSTY, 0, 0, STATUS )
         CALL DAT_FIND( MLOC, 'ASTROMETRY', CLOC, STATUS )
         CALL IRA_WRITE( IDA, CLOC, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  Annul the locator to the MORE structure.
      CALL DAT_ANNUL( MLOC, STATUS )

      END
