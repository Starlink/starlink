      SUBROUTINE CCD1_GTLIH( NDFS, NDFGR, ITEM, PARAM, NFILES, FIOGR,
     :                       STATUS )
*+
*  Name:
*     CCD1_GTLIH

*  Purpose:
*     Opens a list of formatted files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTLIH( NDFS, NDFGR, ITEM, PARAM, NFILES, FIOGR,
*                      STATUS )

*  Description:
*     This routine opens a series of formatted files. The names of the
*     files may be accessed in two different (exclusive) fashions. If
*     the NDFS argument is set true then it is assumed that the names of
*     the lists are stored within the CCDPACK extensions of a list of
*     NDFs. The name of the file to be actually returned is stored in
*
*         ndf_name.more.ccdpack.ITEM
*
*     If NDFS is true then a GRP group identifier is passed for a group
*     containing the NDF names.
*
*     If NDFS is false then it is assumed that the names accessed
*     through the ADAM parameters PARAM using GRP are just the exact
*     names of the files.  The names of all the files are returned
*     in the group FIOGR.  If NDFS is true but some of the files do
*     not have the requisite item in their extensions, then no error
*     is generated and a blank string is returned as the corresponding
*     element of FIOGR.

*  Arguments:
*     NDFS = LOGICAL (Given)
*        Whether the names of the files to be opened are stored with the
*        extensions of NDFs or not.
*     NDFGR = INTEGER (Given)
*        If NDFS is true this gives the group of NDF names whose
*        extensions are to be interrogated for file names.  If NDFS
*        is false, this argument is not used.
*     ITEM = CHARACTER * ( * ) (Given)
*        Only used if NDFS is true. The name of the extension item
*        with contains the file name.
*     PARAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter name via which the list of file names is to
*        be accessed.  Only used if NDFS is false.
*     NFILES = INTEGER (Given)
*        The number of filenames to be returned.  If NDFS is true
*        this should be the same number as the size of the NDFGR group.
*     FIOGR = INTEGER (Returned)
*        A GRP group identifier for the names of the files which have
*        accessed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The calling routine must close all the files which are opened
*     before exit.
*     -  The calling routine must annul the group identifiers before exit.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-APR-2001 (MBT):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      LOGICAL NDFS
      INTEGER NDFGR
      CHARACTER * ( * ) ITEM
      CHARACTER * ( * ) PARAM
      INTEGER NFILES

*  Arguments Returned:
      INTEGER FIOGR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) FNAME ! Filename
      INTEGER I                  ! Loop variable
      INTEGER NDFID              ! NDF identfier
      INTEGER NRET               ! Dummy argument
      LOGICAL OK                 ! Flag showing extension ok

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  If we are getting the list from NDF extensions, do that.
      IF ( NDFS ) THEN

*  Create a GRP group to contain the name strings.
         CALL GRP_NEW( 'CCDPACK:FILELIST', FIOGR, STATUS )

*  Open each NDF in turn and locate the required name.
         DO I = 1, NFILES
            CALL NDG_NDFAS( NDFGR, I, 'READ', NDFID, STATUS )

*  Get the file name.
            CALL CCG1_FCH0C( NDFID, ITEM, FNAME, OK, STATUS )

*  Enter either the file name or a blank string into the group.
            IF ( .NOT. OK ) FNAME = ' '
            CALL GRP_PUT( FIOGR, 1, FNAME, I, STATUS )

*  Release the NDF.
            CALL NDF_ANNUL( NDFID, STATUS )
         END DO

*  If getting them directly, do that.
      ELSE
         CALL CCD1_STRGR( PARAM, GRP__NOID, NFILES, NFILES, FIOGR,
     :                    NRET, STATUS )
      END IF

*  Exit.
 99   CONTINUE
      END
* $Id$
