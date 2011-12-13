      SUBROUTINE CCD1_NGLIS( PARAM, FILNAM, MAXNDF, ECHO, NNDF, STATUS )
*+
*  Name:
*     CCD1_NGLIS

*  Purpose:
*     Write a list of NDF names got from the environment to a text file.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_NGLIS( PARAM, FILNAM, MAXNDF, ECHO, NNDF, STATUS )

*  Description:
*     This routine gets a list of NDF names from the user via the ADAM
*     parameter system and writes their names to a text file, and
*     optionally through the CCDPACK logging system.  The NDG routines
*     are used so that the user may specify NDFs with the normal
*     wildcarding conventions.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter via which to get the NDF names.
*     FILNAM = CHARACTER * ( * ) (Given)
*        Name of the text file to which the names are to be written.
*     MAXNDF = INTEGER (Given)
*        The maximum number of NDFs which can be specified.
*     ECHO = LOGICAL (Given)
*        If true, the NDFs specified will be written through the CCDPACK
*        logging system as well as to the named text file.
*     NNDF = INTEGER (Returned)
*        The number of NDF names written to FILNAM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     29-AUG-2000 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'PAR_ERR'          ! Standard PAR system constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) FILNAM
      INTEGER MAXNDF
      LOGICAL ECHO

*  Arguments Returned:
      INTEGER NNDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER FD                 ! FIO descriptor for the output file
      INTEGER GID                ! Group identifier for group of NDFs
      INTEGER I                  ! Loop variable
      CHARACTER * ( GRP__SZNAM ) NAME ! Buffer for NDF names

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a list of NDFs from the user.
      NNDF = 0
      CALL CCD1_NDFGL( PARAM, 1, MAXNDF, GID, NNDF, STATUS )

*  Open a text file into which to write the results.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CCD1_OPFIO( FILNAM, 'WRITE', 'LIST', 0, FD, STATUS )

*  Write the list of NDFs to the screen and to the file.
         IF ( ECHO ) CALL CCD1_MSG( ' ', ' ', STATUS )
         DO I = 1, NNDF

*  Get the NDF name from the group.
            CALL GRP_GET( GID, I, 1, NAME, STATUS )

*  Write the NDF name to the text file.
            CALL FIO_WRITE( FD, NAME( : CHR_LEN( NAME ) ), STATUS )

*  Write the NDF name through the CCDPACK logging system.
            IF ( ECHO ) THEN
               CALL MSG_SETC( 'NAME', NAME )
               CALL MSG_SETI( 'I', I )
               CALL CCD1_MSG( ' ', '  ^I)  ^NAME', STATUS )
            END IF
         END DO
         IF ( ECHO ) CALL CCD1_MSG( ' ', ' ', STATUS )

*  Close the text file.
         CALL FIO_CLOSE( FD, STATUS )

*  A null response is OK, just annul the error and continue.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Annul group resources.
      CALL CCD1_GRDEL( GID, STATUS )

      END
* $Id$
