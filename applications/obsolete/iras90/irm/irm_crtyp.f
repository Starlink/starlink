      SUBROUTINE IRM_CRTYP( NCRDD, NDFID, TYPE, STATUS )
*+
*  Name:
*     IRM_CRTYP

*  Purpose:
*     Check the type of a group of CRDD NDF files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_CRTYP( NCRDD, NDFID, TYPE, STATUS )

*  Description:
*     This subroutine is used to check whether a group of CRDD NDF files
*     come from the same CRDD type. If not, the status will be set.

*  Arguments:
*     NCRDD = INTEGER (Given)
*        The number of CRDD NDF files.
*     NDFID( NCRDD ) = INTEGER (Given)
*        The IDs of the CRDD NDF files.
*     TYPE = CHARACTER (Returned)
*        The type of the CRDD files if they are from the same type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     19-NOV-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCRDD
      INTEGER NDFID( NCRDD )

*  Argument Returned:
      CHARACTER*( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER I                  ! Do loop index
      CHARACTER*( 50 ) LAB1      ! Label of the first CRDD NDF
      INTEGER LAB1LN             ! Used length of LAB1
      CHARACTER*( 50 ) LAB       ! Label of present CRDD NDF
      INTEGER LABLN              ! Used length of LAB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Only do the check when there are more than 1 input CRDD files.
      IF ( NCRDD .GT. 1 ) THEN

*  Get the label of the first CRDD NDF.
         CALL NDF_CGET( NDFID( 1 ), 'Label', LAB1, STATUS )
         CALL CHR_UCASE( LAB1 )
         CALL CHR_LDBLK( LAB1 )
         LAB1LN = CHR_LEN( LAB1 )

*  Check other NDFs one by one.
         DO I = 2, NCRDD
            CALL NDF_CGET( NDFID( I ), 'Label', LAB, STATUS )
            CALL CHR_UCASE( LAB )
            CALL CHR_LDBLK( LAB )
            LABLN = CHR_LEN( LAB )

*  If this NDF has different type from that of the first, set status
*  and exit.
            IF ( LAB( : LABLN ) .NE. LAB1( : LAB1LN ) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'IRM_CRTYP_ERR1', 'IRM_CRTYP: '/
     :                      /'Input CRDD files are from different '/
     :                      /'types.', STATUS )
               GOTO 999
            END IF
         END DO
      END IF

 999  CONTINUE

*  If the routine finishes sucessfully, return the type of CRDD files
      IF ( STATUS .EQ. SAI__OK ) THEN
         TYPE = LAB1

*  Otherwise, return a blank string.
      ELSE
         TYPE = ' '
      END IF

      END
