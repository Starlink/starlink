      SUBROUTINE IRG_GET( GID, INDEX, DEVICE, DIRN, NAME, SLICE, AMODE,
     :                    OUT, STATUS )
*+
*  Name:
*     IRG_GET

*  Purpose:
*     Return information about an NDF named in a specified group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG_GET( GID, INDEX, DEVICE, DIRN, NAME, SLICE, AMODE, OUT,
*                   STATUS )

*  Description:
*     This routine returns information about an NDF stored in a
*     specified group at a specified index. It also returns the
*     access mode of the specified group, and whether it is an input or
*     output group.

*  Arguments:
*     GID = INTEGER (Given)
*        An IRG group identifier.
*     INDEX = INTEGER (Given)
*        An index within the group. An error is reported if the index
*        lies outside the bounds of the group. The exception to this is
*        that no error is reported if a value of zero is supplied, but
*        in this case only the arguments AMODE and OUT are returned.
*     DEVICE = CHARACTER (Returned)
*        The name of the device on which the NDF container file resides.
*        Returned blank if INDEX is zero.
*     DIRN = CHARACTER (Returned)
*        The name of the directory in which the NDF container file
*        resides.  Returned blank if INDEX is zero.
*     NAME = CHARACTER (Returned)
*        The name of the NDF container file.  Returned blank if INDEX
*        is zero.
*     SLICE = CHARACTER (Returned)
*        Any user-supplied NDF slice specification included in the
*        stored NAME. Returned blank if no slice was specified or if
*        INDEX is zero.
*     AMODE = CHARACTER (Returned)
*        The access mode of the group; READ, WRITE or UPDATE.
*     OUT = LOGICAL (Returned)
*        Returned true if the group holds the names of NDF which are
*        to be created by the application, and returned false if the
*        group holds the names of NDFs which already exist.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-FEB-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRG_PAR'          ! IRG constants

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_AMODE( IRH__MAXG ) = CHARACTER (Read)
*           Access mode (READ, WRITE or UPDATE) for each group. 
*           the corresponding GROUP strcuture.
*        GCM_OUT( IRH__MAXG ) = LOGICAL (Read)
*           If true, then the group is an output group. Otherwise it is
*           an input group.

*  Arguments Given:
      INTEGER GID
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER DEVICE*(*)
      CHARACTER DIRN*(*)
      CHARACTER NAME*(*)
      CHARACTER SLICE*(*)
      CHARACTER AMODE*(*)
      LOGICAL OUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISLICE             ! Position of start of slice
                                 ! specification within stored string.
      CHARACTER STRING*(IRH__SZNAM) ! Stored string.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that any group identified by GID is a group created by IRG.
*  This is assumed to be true if the group title starts with the string
*  given by symbolic constant IRG__PREFX, and if the access mode is
*  legal.
      CALL IRG1_CHECK( GID, .FALSE., STATUS )

*  Return the group access mode in AMODE.
      AMODE = GCM_AMODE( GID )

*  Return true in OUT if the group is an output group.
      OUT = GCM_OUT( GID )

*  If the supplied index is not zero...
      IF( INDEX .NE. 0 ) THEN

*  Get the full string stored at the requested index.
         CALL IRH_GET( GID, INDEX, 1, STRING, STATUS )

*  If the string contains a slice specification, remove it from the
*  string and return it in argument SLICE.
         CALL IRG1_SLICE( STRING, SLICE, ISLICE, STATUS )

*  Return the container file name in NAME.
         CALL IRG1_FSPEC( STRING, IRG__NDFTP, 'NAME', NAME, STATUS )
   
*  Return the directory in DIRN.
         CALL IRG1_FSPEC( STRING, IRG__NDFTP, 'DIRECTORY', DIRN,
     :                    STATUS )

*  Return the device in DEVICE.
         CALL IRG1_FSPEC( STRING, IRG__NDFTP, 'DEVICE', DEVICE, STATUS )

      END IF
   
      END
* $Id$
