      SUBROUTINE CCD1_NDFPL( PARAM, NPLACE, PRENAM, NAMGRP, PLACE,
     :                       FILNAM, STATUS )
*+
*  Name:
*     CCD1_NDFPL

*  Purpose:
*     Creates NDF placeholders using the NDG system.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_NDFPL( PARAM, NPLACE, PRENAM, NAMGRP, PLACE, FILNAM,
*                      STATUS )

*  Description:
*     This routine creates a set of NDF placeholders from an ADAM
*     parameter representing a single name, via the NDG system.
*     The NDG system is used to keep a uniformity of interface
*     between all NDG command specs and prompts (e.g. can be enclosed
*     in quotes, read from a file etc).  Note this routine is not
*     designed for the situation in which the parameter value refers
*     to a list of files in the filesystem.
*
*     A number NPLACE of placeholders are created.  If NPLACE is
*     unity, a normal NDF file is created as per the name given by
*     PARAM.  However, if NPLACE is greater than unity, the name
*     given by PARAM is used as the name of an HDS container file,
*     and placeholders are generated for NDFs within that container.
*     In addition, the name of the file (either the HDS container
*     or the normal NDF file) which was created is returned.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter from which is to be read an
*        NDG expression representing a single location for an HDS
*        object (e.g. a filename).
*     NPLACE = INTEGER (Given)
*        The number of placeholders to return.
*     PRENAM = CHARACTER * ( * ) (Given)
*        If this string is non-blank it gives a prefix to prepend to the
*        values of the NAMGRP entries.  Only used if NPLACE is greater
*        than unity.  Trailing blanks are ignored.
*     NAMGRP = INTEGER (Given)
*        A GRP identifier for tails of names of the NDF structures within
*        an HDS container file.  This is only accessed if NPLACE is greater
*        than unity.
*     PLACE( NPLACE ) = INTEGER (Returned)
*        The new placeholders are returned in this array.
*     FILNAM = CHARACTER * ( * ) (Returned)
*        The name of the file which was created to contain the
*        placeholders.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     8-FEB-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS system constants
      INCLUDE 'GRP_PAR'          ! GRP system constants
      INCLUDE 'PAR_ERR'          ! PAR system error values

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) PRENAM
      INTEGER NPLACE
      INTEGER NAMGRP

*  Arguments Returned:
      INTEGER PLACE( NPLACE )
      CHARACTER * ( * ) FILNAM

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Used length of string

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! Number of extra NDFs this time
      INTEGER GID                ! NDG identifier for new file list
      INTEGER I                  ! Loop counter
      INTEGER NNDF               ! Number of NDFs in list
      INTEGER NTRY               ! Number of user attempts
      INTEGER ONNDF              ! Number of NDFs last time
      LOGICAL AGAIN              ! Do it again?
      LOGICAL TERM               ! Termination flag
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF structure in container
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the new name through NDG.  Set GID to no previous entries.
*  Set the termination character to '-' if this is added to any lines
*  of data it will be stripped and ignored.
      GID = GRP__NOID
      NNDF = 0
      NTRY = 0
 1    CONTINUE                   ! start of repeat until loop
         TERM = .FALSE.
         ONNDF = NNDF
         CALL NDG_CREAT( PARAM, GRP__NOID, GID, NNDF, TERM, STATUS )
         ADDED = NNDF - ONNDF

*  Get out if a given a par_abort. Also quit after an unreasonble
*  number of attempts.
         IF ( STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'CCD1_NDFPR',
     :      '  Unable to obtain valid NDF name using parameter %^PARAM',
     :      STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned is in the permitted range. If
*  the continuation character has been used ignore it NDG has stripped
*  it off.
         AGAIN = .FALSE.

*  One NDF only is allowed.
         IF ( NNDF .GT. 1 ) THEN
             CALL MSG_SETI( 'MAXVAL', 1 )
             CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXVAL '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL CCD1_GRDEL( GID, STATUS )
             NNDF = 0
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( PARAM, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued ignore it.
            CALL PAR_CANCL( PARAM, STATUS )
            AGAIN = .TRUE.

*  Try to trap a special case of ' ' string return. This should leave
*  added unmodified. This will be taken as a request to exit. The normal
*  stop entry request from an blank line will be `!' .
         ELSE IF ( ( ADDED .EQ. 0 ) .AND. ( .NOT. TERM ) ) THEN
            AGAIN = .FALSE.

*  Only tested if the checks for number of NDFs etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.
         END IF
      IF ( AGAIN ) GO TO 1

*  Ensure that we've got a valid list.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the single name to use.
      CALL GRP_GET( GID, 1, 1, FILNAM, STATUS )

*  If we just need one placeholder, create an NDF as usual.
      IF ( NPLACE .EQ. 1 ) THEN
         CALL NDF_PLACE( DAT__ROOT, FILNAM, PLACE( 1 ), STATUS )

*  If we need multiple placeholders, then create an HDS container file
*  and put them in it.
      ELSE
         CALL HDS_NEW( FILNAM, 'NDFS', 'NDF_CONTAINER', 0, 0, LOC,
     :                 STATUS )
         DO I = 1, NPLACE
            CALL GRP_GET( NAMGRP, I, 1, NDFNAM, STATUS )
            NDFNAM = PRENAM( 1:CHR_LEN( PRENAM ) ) // NDFNAM
            CALL NDF_PLACE( LOC, NDFNAM, PLACE( I ), STATUS )
         END DO
         CALL DAT_ANNUL( LOC, STATUS )
      END IF

*  Error exit label.
 99   CONTINUE

*  Release GRP resources.
      CALL CCD1_GRDEL( GID, STATUS )

      END
* $Id$
