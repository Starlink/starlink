      SUBROUTINE CCD1_GTLIS( NDFS, ITEM, PARNAM, MINOPN, MAXOPN, NOPEN,
     :                       FDS, FIOGR, NDFGR, STATUS )
*+
*  Name:
*     CCD1_GTLIS

*  Purpose:
*     Opens a list of formatted files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTLIS( NDFS, ITEM, PARNAM, MINOPN, MAXOPN, NOPEN, FDS,
*                      FIOGR, NDFGR, STATUS )

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
*     through the ADAM parameters PARNAM using GRP are just the exact
*     names of the files.  All files are opened using FIO_OPEN and the
*     FIO file descriptors are returned in FDS. If an extension does
*     not exist (or the name within it) or the file cannot be opened
*     then an error is reported and status is set.
*
*     In either case a GRP identfier for the filename group is
*     returned.

*  Arguments:
*     NDFS = LOGICAL (Given)
*        Whether the names of the files to be opened are stored with the
*        extensions of NDFs are not. If true the input names are
*        expanded into an NDG group.
*     ITEM = CHARACTER * ( * ) (Given)
*        Only used if NDFS is true. The name of the extension item
*        with contains the file name.
*     PARNAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter name via which the list of file names is to
*        be accessed.
*     MINOPN = INTEGER (Given)
*        The minimum number of files which need to be opened.
*     MAXOPN = INTEGER (Given)
*        The maximum number of files which can be opened. This should be
*        not less than the dimensions of the FDS array.
*     NOPEN = INTEGER (Given)
*        The number of files which were opened.
*     FDS( MAXOPN ) = INTEGER (Returned)
*        FIO file system descriptors to the opened files.
*     FIOGR = INTEGER (Returned)
*        A GRP group identifier for the names of the files which have
*        opened. This group is intended for use as a modification group.
*     NDFGR = INTEGER (Returned)
*        An NDG group identifier for the names of the NDFs from which
*        the filenames were obtained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The calling routine must close all the files which are opened
*     before exit.
*     -  The calling routine must annul the group identifiers before exit.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUL-1992 (PDRAPER):
*        Original version.
*     28-JUL-1992 (PDRAPER):
*        Added minimum number of input files option.
*     22-JAN-1993 (PDRAPER):
*        Changed to look for names in NDF extensions.
*     11-JUL-1995 (PDRAPER):
*        Now gives up as expected on failure to open file (was trapped
*        in loop, eventually exceeding error stack).
*     3-MAR-1997 (PDRAPER):
*        Removed LOC argument from IRG_NDFEX call (and associated code).
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
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
      CHARACTER * ( * ) ITEM
      CHARACTER * ( * ) PARNAM
      INTEGER MINOPN
      INTEGER MAXOPN

*  Arguments Returned:
      INTEGER NOPEN
      INTEGER FDS( MAXOPN )
      INTEGER FIOGR
      INTEGER NDFGR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) FNAME ! Filename
      INTEGER FD                 ! FIO file descriptor
      INTEGER I                  ! Loop variable
      INTEGER INGRP              ! Dummy GRP identifier
      INTEGER NDFID              ! NDF identfier
      INTEGER NRET               ! Number of names in group
      LOGICAL OK                 ! Flag showing extension ok

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the lists of names. This may be a list of NDF names (which may
*  include wildcards) or just a straight-forward list of ordinary
*  file-names.
      IF ( NDFS ) THEN

*  Access a list of NDF names.
         CALL CCD1_NDFGL( PARNAM, MINOPN, MAXOPN, NDFGR, NRET, STATUS )
      ELSE

*  Not a list of NDFs, just get a group of names.
         INGRP = GRP__NOID
         CALL CCD1_STRGR( PARNAM, INGRP, MINOPN, MAXOPN, FIOGR, NRET,
     :                    STATUS )
         CALL MSG_SETI( 'NOPEN', NRET )
         CALL MSG_SETC( 'PARNAM', PARNAM )
         IF ( NRET .EQ. 1 ) THEN
            CALL MSG_OUT( ' ',
     :'  ^NOPEN names accessed using parameter %^PARNAM', STATUS )
         ELSE
            CALL MSG_OUT( ' ',
     :'  ^NOPEN names accessed using parameter %^PARNAM', STATUS )
         END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If all's well then proceed to either open the files, if NDFS is
*  false or start looking for the names in the NDF extensions.
      IF ( NDFS ) THEN

*  Create a GRP group to contain the name strings.
         CALL GRP_NEW( 'CCDPACK:FILELIST', FIOGR, STATUS )

*  Open each NDF in turn and locate the required name.
         DO 2 I = 1, NRET
            CALL NDG_NDFAS( NDFGR, I, 'UPDATE', NDFID, STATUS )

*  Get the file name.
            CALL CCG1_FCH0C( NDFID, ITEM, FNAME, OK ,STATUS )
            IF ( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN

*  Cannot locate the named extension item. Set status and exit.
               STATUS = SAI__ERROR
               CALL NDF_MSG( 'NDF', NDFID )
               CALL MSG_SETC( 'ITEM', ITEM )
               CALL ERR_REP( 'CCD1GTLISNOITEM',
     :'  The CCDPACK extension of NDF ^NDF does not exist or does'//
     :' not contain the item ^ITEM ', STATUS )
               GO TO 99
            END IF

*  Enter the file name into the new group, append to the end (0).
            CALL GRP_PUT( FIOGR, 1, FNAME, 0, STATUS )

*  Release the NDF.
            CALL NDF_ANNUL( NDFID, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
 2       CONTINUE
      END IF

*  Now at stage were we have a group of names which may belong to a
*  list of formatted files. Try to open them one by one, stop if one
*  does not exist.
      DO 3 I = 1, NRET
         CALL GRP_GET( FIOGR, I, 1, FNAME, STATUS )

*  Try to open the file.
         CALL FIO_OPEN( FNAME, 'READ', 'LIST', 0, FD, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Everything ok - file exists and is open.
            FDS( I ) = FD
         ELSE

*  Failed to open the file. Stop and issue error.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL ERR_REP( 'CCD1_GTLIS_FERR',
     :         '  Failed to open file ^FNAME', STATUS )
            GO TO 99
         END IF
 3    CONTINUE

*  Set number of output files.
      NOPEN = NRET

*  Exit.
 99   CONTINUE
      END
* $Id$
