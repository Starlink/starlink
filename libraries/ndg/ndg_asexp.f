      SUBROUTINE NDG_ASEXP( GRPEXP, VERB, IGRP1, IGRP2, SIZE, FLAG,
     :                      STATUS )
*+
*  Name:
*     NDG_ASEXP

*  Purpose:
*     Store names of existing NDFs supplied as a group expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ASEXP( GRPEXP, VERB, IGRP1, IGRP2, SIZE, FLAG, STATUS )

*  Description:
*     The supplied group expression is parsed (using the facilities of
*     the GRP routine GRP_GROUP, see SUN/150) to produce a list of
*     explicit names for existing NDFs which are appended to the end of
*     the supplied group (a new group is created if none is supplied).
*     NDF identifiers for particular members of the group can be obtained
*     using NDG_NDFAS.
*
*     If any of the NDFs specified by the group expression cannot be
*     accessed, an error is reported and STATUS is returned equal to
*     NDG__NOFIL. If this happens strings holding the name of each
*     bad NDF are appended to the group identified by IGRP1 (so long
*     as IGRP1 is not equal to GRP__NOID).

*  Arguments:
*     GRPEXP = CHARACTER * ( * ) (Given)
*        The group expression specifying the NDF names to be stored
*        in the group.
*     VERB = LOGICAL (Given)
*        If TRUE then errors which occur whilst accessing supplied NDFs
*        are flushed so that the user can see the details ("verbose" mode).
*        Otherwise, they are annulled and a general "Cannot access file xyz"
*        message is reported instead.
*     IGRP1 = INTEGER (Given)
*        The identifier of a group to which the names of any
*        inaccessable NDFs will be appended. The group should already
*        have been created by a call to GRP_NEW, and should be deleted
*        when no longer needed by a call to GRP_DELET. If IGRP1 is
*        supplied equal to symbolic constant GRP__NOID, then no
*        information is stored describing the bad NDFs.
*     IGRP2 = INTEGER (Given and Returned)
*        The identifier of the group in which the NDF names are to be
*        stored. A new group is created if the supplied value is GRP__NOID.
*        It should be deleted when no longer needed using GRP_DELET.
*     SIZE = INTEGER (Returned)
*        The total number of NDF names in the returned group IGRP2.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag
*        character", then FLAG is returned .TRUE. Otherwise it is
*        returned .FALSE. Returned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Any file names containing wildcards are expanded into a list of NDF
*     names. The supplied strings are intepreted by a shell (/bin/tcsh if
*     it exists, otherwise /bin/csh, otherwise /bin/sh), and so may
*     contain shell meta-characters (e.g. twiddle, $HOME, even command
*     substitution and pipes - but pipe characters "|" need to be escaped
*     using a backslash "\" to avoid them being interpreted as GRP
*     editing characters).
*     -  Each supplied name may include an HDS path. For instance,
*     "/home/dsb/mydata.a.c(1).b" refers to an NDF stored in component
*     "a.c(1).b" in the HDS container file /home/dsb/mydata.sdf. Note,
*     wild cards are not allowed within HDS component paths (i.e. they
*     are only allowed within the specification of the container file).
*     -  If an HDS object is specified which is not an NDF, then the
*     object will be searched for NDF components. This search is
*     recursive, in that any components of the specified object are also
*     searched. The supplied name will be expanded into a group of names,
*     one for each NDF found within the specified HDS object. Note, NDFs
*     are not themselves searched for other NDFs. That is, the expanded
*     group of names will not include any NDF which is contained within
*     another NDF (i.e. NDFs which are stored as an extension item of
*     another NDF are not included in the group). For instance, if the
*     string "fred" is given, the HDS file fred.sdf will be searched for
*     NDFs and the returned group will contain references for all NDFs
*     found within fred.sdf.
*     -  If the environment variable NDF_FORMATS_IN is defined (see
*     SSN/20) then all possible NDFs matching the supplied string are
*     included in the returned group. For instance, if the string "fred"
*     is supplied, then the returned group will contain references to all
*     files with basename fred which also have a file type specified in
*     NDF_FORMATS_IN. If a FITS file "fred.fit" exists, and HDS file
*     "fred.sdf" also exists (and contains an NDF), then supplying the
*     name "fred" will result in both being included in the returned
*     group. If the file "fred.sdf" contains a component called ".fit",
*     then this will be included in the returned group in place of
*     "fred.sdf".
*     -  NDFs contained within HDS files are opened in order to ensure
*     that they are valid NDFs. The NDF name is returned in IGRP1 if there
*     are no valid NDFs matching a supplied name. No check is made that any
*     foreign data files contain valid NDFs since this would involve a
*     potentially expensive data conversion. So, for instance, "*.fit" could
*     pick up FITS catalogues as well as FITS images. If a foreign data file
*     does not contain a valid NDF, an error will be reported when the NDF
*     is accessed using NDG_NDFAS.
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
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1997, 1999, 2000, 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1992 (DSB):
*        Original version.
*     29-AUG-1997 (DSB):
*        Modified to work with automatic NDF data conversion.
*     9-9-1999 (DSB):
*        Improved prologue.
*     10-APR-2000 (DSB):
*        Added argument VERB.
*     15-JUN-2001 (DSB):
*        Renamed as NDG_ASEXP (was NDG1_ASEXP).
*     15-APR-2005 (PWD):
*        Now uses NDG__BKSLH to improve portability
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
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      CHARACTER GRPEXP*(*)
      LOGICAL   VERB
      INTEGER   IGRP1

*  Arguments Given and Returned:
      INTEGER   IGRP2

*  Arguments Returned:
      INTEGER   SIZE
      LOGICAL   FLAG

*  Status:
      INTEGER   STATUS             ! Global status

*  Local Variables:
      CHARACTER TYPE*(GRP__SZTYP)  ! The group type string
      INTEGER   ADDED              ! No. of names added to group
      INTEGER   SIZE0              ! Size of group on entry to this routine
*.

*  Ensure a .FALSE. value for FLAG is returned if an error has already
*  occured.
      FLAG = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  If the supplied value of IGRP2 is GRP__NOID, create a new group to
*  hold the names of the NDFs. Set the group case insensitive if the
*  host file system is case insensitive.
      IF( IGRP2 .EQ. GRP__NOID ) THEN
         TYPE = ' '
         TYPE = 'A list of existing data sets'
         CALL GRP_NEW( TYPE, IGRP2, STATUS )
         SIZE0 = 0

*  If a group identifier was supplied, get the original size of the
*  group.
      ELSE
         CALL GRP_GRPSZ( IGRP2, SIZE0, STATUS )

      END IF

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP2, .FALSE., STATUS )

*  Ensure the group uses "\" as its escape character.
      CALL GRP_SETCC( IGRP2, 'ESC', NDG__BKSLH, STATUS )

*  Append the names to the end of the group.
      IF( GRPEXP .NE. ' ' ) THEN
         CALL GRP_GRPEX( GRPEXP, GRP__NOID, IGRP2, SIZE, ADDED, FLAG,
     :                   STATUS )
      ELSE
         SIZE = SIZE0
         ADDED = 0
         FLAG = .FALSE.
      END IF

*  Check the names added to the group as a result of the above call.
*  Each name may potentially be expanded into a list of names (e.g. because
*  of wild-cards, etc). These are appended to the end of the group and the
*  original name deleted. An error is reported if no accessable NDFs can
*  be found matching any one of the supplied names.
      CALL NDG1_NDFCH( VERB, IGRP2, SIZE0 + 1, IGRP1, STATUS )

*  Update the SIZE argument to take account of the new group
*  members produced as a result of the expansion of any wild cards. This
*  needs to happen even if an error has been reported, so do it in a new
*  error reporting context.
      CALL ERR_BEGIN( STATUS )
      CALL GRP_GRPSZ( IGRP2, SIZE, STATUS )
      CALL ERR_END( STATUS )

*  If an error has been reported (other than "some NDFs not accessible")
*  set the group back to its original size.
      IF( STATUS .NE. SAI__OK .AND. STATUS .NE. NDG__NOFIL ) THEN
         CALL ERR_BEGIN( STATUS )

         SIZE = SIZE0
         CALL NDG_SETSZ( IGRP2, SIZE0, STATUS )

         CALL ERR_END( STATUS )
      END IF

*  If an error occured give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', GRPEXP )
         CALL ERR_REP( 'NDG_ASEXP_ERR2', 'Error obtaining a group '//
     :                 'of existing NDFs using group expression "^P"',
     :                 STATUS )
      END IF

*  Release the current error context.
      CALL ERR_RLSE

      END
