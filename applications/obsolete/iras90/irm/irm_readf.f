      SUBROUTINE IRM_READF( PARAM, REJECT, IGRP, SIZE, STATUS )
*+
*  Name:
*     IRM_READF

*  Purpose:
*     Read the contents of a text file into a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_READF( PARAM, REJECT, IGRP, SIZE, STATUS )

*  Description:
*     The supplied parameter is used to obtain the name of a text file
*     from the environment. The contents of this text file are read and
*     stored in a GRP group. The default GRP control characters are used
*     to interpret the contents of the file. For instance, this means
*     that fields separated by commas are placed in adjacent elements
*     of the group, strings starting with # are ignored as comments,
*     strings starting with ^ are treated as indirection elements, etc.
*     If REJECT is supplied .TRUE., all blanks elements are removed from
*     the group before it is returned. The group is set
*     case-insensitive.

*  Arguments:
*     PARAM = INTEGER (Given)
*        The name of the parameter used to get the name of the text
*        file.
*     REJECT = LOGICAL (Given)
*        If true, blanks are removed from the group.
*     IGRP = INTEGER (Returned)
*        The GRP identifier for the returned group.
*     SIZE = INTEGER (Returned)
*        The number of elements in the returned group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1993 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'GRP_ERR'          ! GRP_ error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL REJECT

*  Arguments Returned:
      INTEGER IGRP
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER INDCC*1          ! Groups indirection control character.
      CHARACTER FILE*(GRP__SZFNM) ! Input file name.
      CHARACTER GRPEXP*(GRP__SZGEX) ! Group expression.

      INTEGER ADDED              ! No. of elements added to the group.
      INTEGER IPAR               ! SUBPAR parameter identifier.
      INTEGER ITMP               ! GRP identifier for a temporary group

      LOGICAL FLAG               ! True if group expression was flagged.


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new empty group to hold the contents of the file.
      CALL GRP_NEW( 'IRM_READF group', ITMP, STATUS )

*  Get the indirection character.
      CALL GRP_GETCC( ITMP, 'INDIRECTION', INDCC, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the file name from the environment.
 10   CONTINUE

      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, FILE, STATUS )

*  Set the file name blank and abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         FILE= ' '
         GO TO 999
      END IF

*  Create a group expression which will read the contents of the file
*  into a group.
      GRPEXP = INDCC//FILE

*  Attempt to read the contents of the file into the group.
      CALL GRP_GRPEX( GRPEXP, GRP__NOID, ITMP, SIZE, ADDED, FLAG,
     :                STATUS )

*  If a fortran I/O error has occurred, flush it, set the group size to
*  zero and go round for a new file name.
      IF( STATUS .EQ. GRP__FIOER ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL GRP_SETSZ( ITMP, 0, STATUS )
         CALL SUBPAR_CANCL( IPAR, STATUS )
         GO TO 10
      END IF

*  If required, create a group from which all blanks have been removed.
      IF( REJECT ) THEN
         CALL GRP_REMOV( ITMP, ' ', IGRP, STATUS )

*  Delete the original group.
         CALL GRP_DELET( ITMP, STATUS )

*  Return the size of the group without blanks.
         CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  If blanks are to be left in, return the "temporary" group.
      ELSE
         IGRP = ITMP
      END IF

*  Set the group case insensitive.
      CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  If an error has occurred, delete the group and add a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP, STATUS )

         IF( FILE .NE. ' ' ) THEN
            CALL MSG_SETC( 'F', FILE )
            CALL ERR_REP( 'IRM_READF_ERR1',
     :    'IRM_READF: Unable to read information from file ^F',
     :                 STATUS )

         ELSE
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'IRM_READF_ERR2',
     :    'IRM_READF: Unable to read information from a file using '//
     :    'parameter %^P', STATUS )

         END IF

      END IF

      END
