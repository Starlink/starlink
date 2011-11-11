      SUBROUTINE NDG_MOREG( INDF, IGRP, SIZE, STATUS )
*+
*  Name:
*     NDG_MOREG

*  Purpose:
*     Search for NDFs within the extensions of a supplied NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_MOREG( INDF, IGRP, SIZE, STATUS )

*  Description:
*     Each extension within the supplied NDF is searched to see if it
*     contains any NDFs. The paths to any such NDFs are appended to the
*     end of the supplied group (a new group is created if none is
*     supplied). NDF identifiers for particular members of the group can
*     be obtained using NDG_NDFAS.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF to be searched.
*     IGRP = INTEGER (Given and Returned)
*        The identifier of the group in which NDF names are to be
*        stored. A new group is created if the supplied value is GRP__NOID.
*        It should be deleted when no longer needed using GRP_DELET.
*     SIZE = INTEGER (Returned)
*        The total number of NDF names in the returned group IGRP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The NDFs within the returned group are ordered by increasing
*     depth within the supplied NDF. That is, if integer J is larger than
*     integer I, then the NDF stored at index I may contain the NDF
*     stored at index J, but the reverse will never be true (i.e. the NDF
*     stored at index J will never contain the NDF stored at index I).
*     -  Each element in the returned group contains a full specification
*     for an NDF. Several other groups are created by this routine, and
*     are associated with the returned group by means of a GRP "owner-slave"
*     relationship. These supplemental groups are automatically deleted
*     when the returned group is deleted using GRP_DELET. The returned
*     group should not be altered using GRP directly because corresponding
*     changes may need to be made to the supplemental groups. Routines
*     NDG_SETSZ, NDG_GTSUP and NDG_PTSUP are provided to manipulate the
*     entire chain of groups. The full chain (starting from the head) is
*     as follows:
*
*        - NDF slice specifications
*        - HDS paths
*        - File types
*        - Base file names
*        - Directory paths
*        - Full NDF specification (this is the returned group IGRP)

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-FEB-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDG_CONST'        ! NDG constants.
      INCLUDE 'NDG_ERR'          ! NDG error constants.
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      INTEGER INDF

*  Arguments Given and Returned:
      INTEGER   IGRP

*  Arguments Returned:
      INTEGER   SIZE

*  Status:
      INTEGER   STATUS             ! Global status

*  Local Variables:
      CHARACTER DIR*(GRP__SZNAM)   ! Directory field
      CHARACTER NAM*(GRP__SZNAM)   ! File base name field
      CHARACTER NDFNAM*255         ! Spec. for supplied NDF
      CHARACTER SEC*(GRP__SZNAM)   ! File NDF/HDS section
      CHARACTER SPEC*(GRP__SZNAM)  ! The file spec of the matching file
      CHARACTER TYP*(GRP__SZNAM)   ! File type field
      CHARACTER TYPE*(GRP__SZTYP)  ! The group type string
      CHARACTER XLOC*(DAT__SZLOC)  ! HDS locator for extension
      CHARACTER XNAME*(DAT__SZNAM) ! Extension name
      INTEGER ADDED                ! No. of names added to group
      INTEGER I                    ! Extension index
      INTEGER IGRPB                ! Group holding base name fields
      INTEGER IGRPD                ! Group holding directory fields
      INTEGER IGRPH                ! Group holding data path fields
      INTEGER IGRPS                ! Group holding NDF slice fields
      INTEGER IGRPT                ! Group holding file type fields
      INTEGER NDFLEN               ! Used length of NDFNAM
      INTEGER NEXTN                ! Number of extensions
      INTEGER SIZE0                ! Size of group on entry to this routine
      LOGICAL FOUND                ! Was an NDF found?
      LOGICAL STRUCT               ! Is extension a structure?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  If the supplied value of IGRP is GRP__NOID, create a new group to
*  hold the names of the NDFs. Set the group case insensitive if the
*  host file system is case insensitive.
      IF( IGRP .EQ. GRP__NOID ) THEN
         TYPE = ' '
         TYPE = 'A list of existing data sets'
         CALL GRP_NEW( TYPE, IGRP, STATUS )
         SIZE0 = 0

*  If a group identifier was supplied, get the original size of the
*  group.
      ELSE
         CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )

      END IF

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Ensure the group uses "\" as its escape character.
      CALL GRP_SETCC( IGRP, 'ESC', NDG__BKSLH, STATUS )

*  Ensure that supplemental groups are associated with the returned group
*  holding extra information. These groups are deleted automatically when
*  the returned group is deleted.
*
*  The returned group is owned by a group holding the directory
*  specifications for each file. Check to see if the group exists. If
*  not, create it and establish it as the owner of the returned group.
      CALL GRP_OWN( IGRP, IGRPD, STATUS )
      IF( IGRPD .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Directory', IGRPD, STATUS )
         CALL GRP_SOWN( IGRP, IGRPD, STATUS )
      END IF

*  The directories group is owned by a group holding the file base names.
      CALL GRP_OWN( IGRPD, IGRPB, STATUS )
      IF( IGRPB .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Base name', IGRPB, STATUS )
         CALL GRP_SOWN( IGRPD, IGRPB, STATUS )
      END IF

*  The base names group is owned by a group holding the file types.
      CALL GRP_OWN( IGRPB, IGRPT, STATUS )
      IF( IGRPT .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'File type', IGRPT, STATUS )
         CALL GRP_SOWN( IGRPB, IGRPT, STATUS )
      END IF

*  The file types group is owned by a group holding the data paths.
      CALL GRP_OWN( IGRPT, IGRPH, STATUS )
      IF( IGRPH .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Data path', IGRPH, STATUS )
         CALL GRP_SOWN( IGRPT, IGRPH, STATUS )
      END IF

*  The data paths group is owned by a group holding the NDF slices.
      CALL GRP_OWN( IGRPH, IGRPS, STATUS )
      IF( IGRPS .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'NDF slice', IGRPS, STATUS )
         CALL GRP_SOWN( IGRPH, IGRPS, STATUS )
      END IF

*  Get the full specification of the supplied NDF.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', NDFNAM, NDFLEN, STATUS )

*  Split the file spec into directory, basename, suffix and section.
      CALL NDG1_FPARS( NDFNAM( : NDFLEN ), 0, DIR, NAM, TYP, SEC,
     :                 STATUS )

*  We want the whole of each NDF, not just a section matching the
*  supplied NDF, so blank out the section string obtained form the supplied
*  NDF.
      SEC = ' '

*  Loop round every extension in the supplied NDF.
      CALL NDF_XNUMB( INDF, NEXTN, STATUS )
      DO I = 1, NEXTN

*  Get the name of the current NDF extension.
         CALL NDF_XNAME( INDF, I, XNAME, STATUS )

*  Get a locator for the extension.
         CALL NDF_XLOC( INDF, XNAME, 'READ', XLOC, STATUS )

*  Skip over extensions that are not structures.
         CALL DAT_STRUC( XLOC, STRUCT, STATUS )
         IF( STRUCT ) THEN

*  Search the structure recursively for NDFs, adding paths for any found
*  to the group. The NDFs are stored in order of increasing depth within
*  the supplied NDF.
            CALL NDG1_SDFEX( IGRP, .TRUE., IGRPD, IGRPB, IGRPT, IGRPH,
     :                       IGRPS, XLOC, DIR, NAM, TYP, SEC, FOUND,
     :                       STATUS )

         END IF

*  Annul the extension.
         CALL DAT_ANNUL( XLOC, STATUS )
      END DO

*  Update the SIZE argument to take account of the new group members
*  produced above. This needs to happen even if an error has been
*  reported, so do it in a new error reporting context.
      CALL ERR_BEGIN( STATUS )
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      CALL ERR_END( STATUS )

*  If an error has been reported set the group back to its original size.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )

         SIZE = SIZE0
         CALL NDG_SETSZ( IGRP, SIZE0, STATUS )

         CALL ERR_END( STATUS )
      END IF

*  If an error occured give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'N', INDF )
         CALL ERR_REP( 'NDG_MOREG_ERR2', 'Error searching for '//
     :                 'extension NDFs contained within ''^N''.',
     :                 STATUS )
      END IF

*  Release the current error context.
      CALL ERR_RLSE

      END
