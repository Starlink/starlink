      SUBROUTINE PREPC4( II, NITEM, SIZE, WORK, PAIR, INOISE, IDATA,
     :                   INFO, STATUS )
*+
*  Name:
*     PREPC4

*  Purpose:
*     Identify pairs of associated noise and data grids.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPC4( II, NITEM, SIZE, WORK, PAIR, INOISE, IDATA, INFO,
*                  STATUS )

*  Description:
*     The information stored in the workspace is searched to find an
*     NDF which has information matching that stored at index II, but
*     with the opposite grid type (noise or data). If one is found, PAIR
*     is returned true, INOISE is returned holding the index of the
*     noise grid and IDATA is returned holding the index of the data
*     grid. If no match is found, then IDATA is returned equal to II.
*     Once the information stored at a given index has been used, it is
*     marked by the first item being set to "ERASED". Such indices are
*     excluded from further searches.

*  Arguments:
*     II = INTEGER (Given)
*        The index of the NDF for which a match is to be found.
*     NITEM = INTEGER (Given)
*        The maximum number of items of information stored about each
*        NDF.
*     SIZE = INTEGER (Given)
*        The total number of NDFs in the input group.
*     WORK( NITEM, SIZE ) = CHARACTER * ( * ) (Given and Returned)
*        The workspace in which items of information describing each NDF
*        are stored.
*     PAIR = LOGICAL (Returned)
*        True if a match was found for the NDF with index given by II.
*     INOISE = INTEGER (Returned)
*        The index of the NDF holding the noise grid which forms one of
*        the members of a pair. Only significant if PAIR is returned
*        true.
*     IDATA = INTEGER (Returned)
*        The index of the NDF holding the data grid which forms one of
*        the members of a pair. If PAIR is returned false, IDATA is
*        returned equal to II.
*     INFO( NITEM ) = CHARACTER * ( * ) (Returned)
*        Workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Arguments Given:
      INTEGER II
      INTEGER NITEM
      INTEGER SIZE
      CHARACTER WORK( NITEM, SIZE )*(*)

*  Arguments Returned:
      LOGICAL PAIR
      INTEGER INOISE
      INTEGER IDATA
      CHARACTER INFO( NITEM )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if two strings are equal apart
                                 ! from case.

*  Local Variables:
      INTEGER INDEX              ! NDF index.
      INTEGER J                  ! Item count.

*.

*  Ensure that no pair is indicated if an error occurs.
      PAIR = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the type of map to be searched for (noise or data). It is the
*  opposite of the current map type given in the first element of the
*  work array.
      IF( WORK( 1, II ) .EQ. 'NOISE' ) THEN
         INFO( 1 ) = 'DATA'

      ELSE IF( WORK( 1, II ) .EQ. 'DATA' ) THEN
         INFO( 1 ) = 'NOISE'

*  If the current NDF has already been included in a pair, return with
*  IDATA equal to zero.
      ELSE IF( WORK( 1, II ) .EQ. 'USED' ) THEN
         IDATA = 0
         GO TO 999

*  If the current NDF is neither a data or a noise map, no pair exists.
*  Return with IDATA equal to the index of the input NDF.
      ELSE
         IDATA = II
         GO TO 999
      END IF

*  Store the values of the other items of information being searched
*  for.
      DO J = 2, NITEM
         INFO( J ) = WORK( J, II )
      END DO

*  Loop through the work array until a match is found.
      INDEX = 0
      PAIR = .FALSE.
      DO WHILE( .NOT. PAIR .AND. INDEX .LT. SIZE )
         INDEX = INDEX + 1

*  Loop round each of the items of information until the a difference is
*  found between the work values and the current NDF values.
         J = 0
         PAIR = .TRUE.
         DO WHILE( PAIR .AND. J .LT. NITEM )
            J = J + 1
            PAIR = CHR_SIMLR( INFO( J ), WORK( J, INDEX ) )
         END DO

      END DO

*  If a match was found, return the index of each member of the pair.
*  and remove the information about both members from the workspace.
      IF( PAIR ) THEN

         IF( INFO( 1 ) .EQ. 'NOISE' ) THEN
            INOISE = INDEX
            IDATA = II

         ELSE
            INOISE = II
            IDATA = INDEX
         END IF

         DO J = 1, NITEM
            WORK( J, II ) = 'USED'
            WORK( J, INDEX ) = 'USED'
         END DO

*  If no pair was found, remove the information for the current NDF from
*  the workspace, and ensure that the returned data map index is the
*  index of the current NDF.
      ELSE

         DO J = 1, NITEM
            WORK( J, II ) = 'USED '
         END DO

         IDATA = II

      END IF

*  Finish
 999  CONTINUE

      END
