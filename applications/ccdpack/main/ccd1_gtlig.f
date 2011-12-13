      SUBROUTINE CCD1_GTLIG( NDFS, ITEM, PARNAM, MINOPN, MAXOPN, SKIPMT,
     :                       NOPEN, FIOGR, NDFGR, NNOLIS, NLGR, STATUS )
*+
*  Name:
*     CCD1_GTLIG

*  Purpose:
*     Gets a GRP group of validated (formatted) file names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTLIG( NDFS, ITEM, PARNAM, MINOPN, MAXOPN, SKIPMT,
*                      NOPEN, FIOGR, NDFGR, NNOLIS, NLGR, STATUS )

*  Description:
*     This routine creates a GRP group of filenames. The files are
*     tested for existence before entry into the group. The names of
*     the files may be accessed in two different (exclusive) fashions.
*     If the NDFS argument is set true then it is assumed that the
*     names of the files are stored within the CCDPACK extensions of a
*     list of NDFs. The name of the file to be actually returned is
*     stored in
*
*         ndf_name.more.ccdpack.ITEM
*
*     If NDFS is true then a GRP group identifier is also returned
*     for a group containing the NDF names (NDFGR).
*
*     If NDFS is false then it is assumed that the names accessed
*     through the ADAM parameters PARNAM using GRP are just the exact
*     names of the files.  All files are then opened using FIO_OPEN to
*     test for their existence.

*  Arguments:
*     NDFS = LOGICAL (Given)
*        Whether the names of the files to be opened are stored with the
*        extensions of NDFs are not. It true the input names are
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
*        The maximum number of files which can be opened.
*     SKIPMT = LOGICAL (Given)
*        If true, then position list files which exist but contain no
*        positions will be treated in the same way as non-existent
*        position list files (added to the NLGR group).  If false,
*        they will be treated the same as position list files which
*        do contain positions.
*     NOPEN = INTEGER (Returned)
*        The number of files which were opened (size of FIOGR and NDFGR).
*     FIOGR = INTEGER (Returned)
*        A GRP group identifier for the names of the files which have
*        opened. This group is intended for use as a modification group.
*     NDFGR = INTEGER (Returned)
*        An NDG group identifier for the names of the NDFs from which
*        the filenames were obtained.
*     NNOLIS = INTEGER (Returned)
*        The size of NLGR.
*     NLGR = INTEGER (Returned)
*        An NDG group identifier for the names of lists/NDFs which were
*        specified by the user, but have no position list file or,
*        if SKIPMT is true, have empty lists.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The calling routine must close all the files which are opened
*     before exit.
*     -  The calling routine must annul the group identifiers before exit.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1997, 1999-2001 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     25-JAN-1993 (PDRAPER):
*        Changed to not return the FIO identifiers.
*     3-MAR-1997 (PDRAPER):
*        Removed LOC argument and associated code from IRG_NDFEX call.
*     26-APR-1999 (MBT):
*        Modified so that failing to find ITEM in the CCDPACK extension
*        is no longer fatal.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     16-MAR-2001 (MBT):
*        Added NNOLIS and NLGR arguments.
*     22-MAY-2001 (MBT):
*        Added SKIPMT argument.
*     01-JUL-2005 (BEC):
*        Replaced MSG_OUT with CCD1_MSG to respect user's preference of
*        output logging.
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
      INCLUDE 'CCD1_PAR'         ! CCDPACK private constants

*  Arguments Given:
      LOGICAL NDFS
      CHARACTER * ( * ) ITEM
      CHARACTER * ( * ) PARNAM
      INTEGER MINOPN
      INTEGER MAXOPN
      LOGICAL SKIPMT

*  Arguments Returned:
      INTEGER NOPEN
      INTEGER NNOLIS
      INTEGER FIOGR
      INTEGER NDFGR
      INTEGER NLGR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) FNAME ! Filename
      CHARACTER * ( CCD1__BLEN ) LINE ! Line buffer
      CHARACTER * ( GRP__SZNAM ) NNAME ! NDF name
      INTEGER FD                 ! FIO file descriptor
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER FIO1GR             ! GRP identifier for group fo all PARNAM FIOs
      INTEGER INGRP              ! Dummy GRP identifier
      INTEGER NC                 ! Number of characters in line (dummy)
      INTEGER NDF1GR             ! GRP identifier for group of all PARNAM NDFs
      INTEGER NDFID              ! NDF identfier
      INTEGER NL                 ! Number of line in file (dummy)
      INTEGER NRET               ! Number of names in group
      LOGICAL OK                 ! Flag showing extension ok
      LOGICAL SKIP               ! Do we skip this list?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the lists of names. This may be a list of NDF names (which may
*  include wildcards) or just a straight-forward list of ordinary
*  file-names.
      IF ( NDFS ) THEN

*  Access a list of NDF names.
         CALL CCD1_NDFGL( PARNAM, MINOPN, MAXOPN, NDF1GR, NRET, STATUS )
      ELSE

*  Not a list of NDFs, just get a group of names.
         INGRP = GRP__NOID
         CALL CCD1_STRGR( PARNAM, INGRP, MINOPN, MAXOPN, FIO1GR, NRET,
     :                    STATUS )
         CALL MSG_SETI( 'NOPEN', NRET )
         CALL MSG_SETC( 'PARNAM', PARNAM )
         IF ( NRET .EQ. 1 ) THEN
            CALL CCD1_MSG( ' ',
     :'  ^NOPEN name accessed using parameter %^PARNAM', STATUS )
         ELSE
            CALL CCD1_MSG( ' ',
     :'  ^NOPEN names accessed using parameter %^PARNAM', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Initialise number of names successfully entered in the group.
      NOPEN = 0
      NNOLIS = 0

*  Create GRP groups to contain the name strings.
      CALL GRP_NEW( 'CCDPACK:FILELIST', FIOGR, STATUS )
      CALL GRP_NEW( 'CCDPACK:NOLIST', NLGR, STATUS )

*  If all's well then proceed to either open the files, if NDFS is
*  false or start looking for the names in the NDF extensions.
      IF ( NDFS ) THEN

*  Create a new group to hold the NDF names.
         CALL GRP_NEW( 'CCDPACK:NDFLIST', NDFGR, STATUS )

*  Open each NDF in turn and locate the required name.
         DO 2 I = 1, NRET
            CALL NDG_NDFAS( NDF1GR, I, 'UPDATE', NDFID, STATUS )

*  Get the NDF name and file name.
            CALL GRP_GET( NDF1GR, I, 1, NNAME, STATUS )
            CALL CCG1_FCH0C( NDFID, ITEM, FNAME, OK ,STATUS )
            IF ( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN

*  Cannot locate the named extension item.  Add this name to the group
*  of NDFs with no associated list.
               CALL GRP_PUT( NLGR, 1, NNAME, 0, STATUS )
               NNOLIS = NNOLIS + 1

*  Report that this NDF will be ignored.
               CALL MSG_SETC( 'NDF', NNAME )
               CALL CCD1_MSG( ' ',
     :'  There is no associated list for NDF ^NDF', STATUS )
            ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  We have a file name.  See if the named file exists.
               CALL FIO_OPEN( FNAME, 'READ', 'LIST', 0, FD, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN

*  Failed to open the file. Stop and issue error.
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'FNAME', FNAME )
                  CALL ERR_REP( 'CCD1_GTLIS_FERR',
     :                          '  Failed to open file ^FNAME', STATUS )
                  GO TO 99
               END IF

*  The file exists.  If asked to, check it for being empty.
               SKIP = .FALSE.
               IF ( SKIPMT ) THEN
                  NL = 0
                  CALL CCD1_RDLIN( FD, CCD1__BLEN, LINE, NC, NL, SKIP,
     :                             STATUS )
               END IF

*  Close the file.
               CALL FIO_CLOSE( FD, STATUS )

*  If we are to discard it, store it in the no-list group.
               IF ( SKIP ) THEN
                  CALL GRP_PUT( NLGR, 1, NNAME, 0, STATUS )
                  NNOLIS = NNOLIS + 1

*  Report that this NDF will be ignored.
                  CALL MSG_SETC( 'NDF', NNAME )
                  CALL CCD1_MSG( ' ',
     :'  The associated list for NDF ^NDF is empty', STATUS )
               ELSE

*  Everything is all right, we have a usable list.  Enter the file name
*  and the NDF name into the new groups, appending to the end (0).
                  CALL GRP_PUT( FIOGR, 1, FNAME, 0, STATUS )
                  CALL GRP_PUT( NDFGR, 1, NNAME, 0, STATUS )
                  NOPEN = NOPEN + 1
               END IF
            END IF

*  Release the NDF.
            CALL NDF_ANNUL( NDFID, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
 2       CONTINUE

*  Annul the original NDF group identifier, since it is no longer required.
         CALL CCD1_GRDEL( NDF1GR, STATUS )
      ELSE

*  Position lists are given directly.  We just need to check that the
*  named files exist and possibly that they are non-empty.
         DO I = 1, NRET

*  Get the name of the file.
            CALL GRP_GET( FIO1GR, I, 1, FNAME, STATUS )

*  Try to open it.
            CALL FIO_OPEN( FNAME, 'READ', 'LIST', 0, FD, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN

*  Failed to open the file.  Stop and issue error.
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'FNAME', FNAME )
               CALL ERR_REP( 'CCD1_GTLIG_FERR',
     :         'CCD1_GTLIG: Failed to open file ^FNAME', STATUS )
            ELSE

*  The file exists.  If asked to, check for it being empty.
               SKIP = .FALSE.
               IF ( SKIPMT ) THEN
                  NL = 0
                  CALL CCD1_RDLIN( FD, CCD1__BLEN, LINE, NC, NL, SKIP,
     :                             STATUS )
               END IF

*  Close the file.
               CALL FIO_CLOSE( FD, STATUS )

*  If we are to discard it, store it in the no-list group.
               IF ( SKIP ) THEN
                  CALL GRP_PUT( NLGR, 1, FNAME, 0, STATUS )
                  NNOLIS = NNOLIS + 1

*  Report that this list will be ignored.
                  CALL MSG_SETC( 'LIST', FNAME )
                  CALL CCD1_MSG( ' ', '  The list ^LIST is empty',
     :                           STATUS )
               ELSE

*  Everything is all right, we have a usable list.  Enter the file name
*  into the new group, appending to the end (0).
                  CALL GRP_PUT( FIOGR, 1, FNAME, 0, STATUS )
                  NOPEN = NOPEN + 1
               END IF
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 99
         END DO

*  Relase the input group.
         CALL CCD1_GRDEL( FIO1GR, STATUS )
      END IF

*  Ensure that we have an acceptable number of lists.  We may have
*  arrived here without that if an acceptable number of NDFs were
*  supplied but they did not all have associated lists.
      IF ( NOPEN .LT. MINOPN ) THEN
         STATUS = SAI__ERROR
         IAT = 0
         CALL CHR_PUTC( 'Only ', LINE, IAT )
         CALL CHR_PUTI( NOPEN, LINE, IAT )
         IF ( SKIPMT ) CALL CHR_PUTC( ' non-empty', LINE, IAT )
         IF ( NOPEN .EQ. 1 ) THEN
            CALL CHR_PUTC( ' file was', LINE, IAT )
         ELSE
            CALL CHR_PUTC( ' files were', LINE, IAT )
         END IF
         CALL CHR_PUTC( ' supplied - need at least ', LINE, IAT )
         CALL CHR_PUTI( MINOPN, LINE, IAT )
         CALL ERR_REP( 'CCD1_GTLIG_TOOFEW', LINE( 1:IAT ), STATUS )
      ELSE IF ( NOPEN .GT. MAXOPN ) THEN
         STATUS = SAI__ERROR
         IAT = 0
         CALL CHR_PUTI( NOPEN, LINE, IAT )
         IF ( SKIPMT ) CALL CHR_PUTC( ' non-empty', LINE, IAT )
         IF ( NOPEN .EQ. 1 ) THEN
            CALL CHR_PUTC( ' file was', LINE, IAT )
         ELSE
            CALL CHR_PUTC( ' files were', LINE, IAT )
         END IF
         CALL CHR_PUTC( ' supplied - maximum is ', LINE, IAT )
         CALL CHR_PUTI( MAXOPN, LINE, IAT )
         CALL ERR_REP( 'CCD1_GTLIG_TOOMANY', LINE( 1:IAT ), STATUS )
      END IF

*  Exit.
 99   CONTINUE
      END
* $Id$
