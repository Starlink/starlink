      SUBROUTINE CCD1_LNAMM( PARAM, INDXLO, INDXHI, TITLE, GRPID,
     :                       VALID, COMMEN, STATUS )
*+
*  Name:
*     CCD1_LNAMM

*  Purpose:
*     Lists the contents of a GRP group to a file via an ADAM
*     parameter. Empty files are not recorded.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LNAMM( PARAM, INDXHI, INDXLO, TITLE, GRPID, VALID,
*                      COMMEN, STATUS )

*  Description:
*     This routine writes the names of the elements in the input GRP
*     group into a text file. An integer array is also given which
*     indicates whether the associated name should be written. Array
*     elements with value 0 are not written.
*
*     The output text file is opened via the ADAM parameter PARAM.  The
*     names in the group are taken from the index range INDXLO to
*     INDXHI. A title is written to the first line of the file.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter used to obtain the text file.
*     INDXLO = INTEGER (Given)
*        The index of the first element of the group which is to be
*        written out.
*     INDXHI = INTEGER (Given)
*        The index of the last element of the group which is to be
*        written out.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title for the first line of the file. This must contain the
*        character # first (i.e. '#  then the actual comment').
*     GRPID = INTEGER (Given)
*        The GRP identifier of the group.
*     VALID( INDXHI ) = INTEGER (Given)
*        Array of integer values indicating whether the associated name
*        should be added to the output file or not. If the value
*        associated with a name is 0 then it will not be written.
*     COMMEN = _LOGICAL (Given)
*        Whether to write a comment to the user about the name of the
*        output file or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2000 Central Laboratory of the Research
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
*     12-NOV-1998 (PDRAPER):
*        Original version. Based on CCD1_LNAM, just added integer array
*        which really indicates the number of detections associated with
*        the name
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
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'GRP_PAR'          ! GRP system constants

*  Arguments Given:
      CHARACTER *  ( * ) PARAM
      INTEGER INDXLO
      INTEGER INDXHI
      CHARACTER * ( * ) TITLE
      INTEGER GRPID
      INTEGER VALID( * )
      LOGICAL COMMEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) NAME ! Buffer for name
      CHARACTER * ( FIO__SZFNM ) FNAME ! Buffer for file name
      INTEGER I                  ! Loop variable
      INTEGER FD                 ! File descriptor
      LOGICAL OPEN               ! File is open flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the file via the named adam parameter.
      OPEN = .FALSE.
      CALL CCD1_ASFIO( PARAM, 'WRITE', 'LIST', GRP__SZNAM, FD, OPEN,
     :                 STATUS )

*  Write the title
      CALL FIO_WRITE( FD, TITLE( : CHR_LEN( TITLE ) ), STATUS )

*  Loop over the required index extracting the names and then writting
*  them into the file.
      DO 1 I = INDXLO, INDXHI
         IF ( VALID( I ) .NE. 0 ) THEN
            NAME = ' '
            CALL GRP_GET( GRPID, I, 1, NAME, STATUS )

*  Now write out the name.
            CALL FIO_WRITE( FD, NAME( : CHR_LEN( NAME ) ), STATUS )
         END IF
 1    CONTINUE

      IF ( COMMEN .AND. STATUS .EQ. SAI__OK ) THEN

*  Write a comment about the name of the list.
         CALL FIO_FNAME( FD, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL CCD1_MSG( ' ',
     :   '  Namelist written to file: ^FNAME', STATUS )
      END IF

*  Close the file if it is open.
      IF ( OPEN ) CALL FIO_CLOSE( FD, STATUS )
      END
* $Id$
