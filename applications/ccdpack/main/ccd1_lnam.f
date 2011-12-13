      SUBROUTINE CCD1_LNAM( PARAM, INDXLO, INDXHI, TITLE, GRPID, COMGID,
     :                      COMMEN, STATUS )
*+
*  Name:
*     CCD1_LNAM

*  Purpose:
*     Lists the contents of an GRP group to a file via an ADAM
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LNAM( PARAM, INDXHI, INDXLO, TITLE, GRPID, COMGID,
*                     COMMEN, STATUS )

*  Description:
*     This routine writes the names of the elements in the input GRP
*     group into a text file. The text file is opened via the ADAM
*     parameter PARAM.  The names in the group are taken from the index
*     range INDXLO to INDXHI. A title is written to the first line of
*     the file.
*
*     If the COMGID argument is not equal to GRP__NOID then it indicates
*     a GRP group which contains comment strings to be written
*     alongside the names.  These comments will be appended to each
*     line following a comment character (the GRP COMMENT character
*     for GRPID, which by default is '#').  If COMGID is GRP__NOID, no
*     comment will be written.

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
*     COMGID = INTEGER (Given)
*        The GRP identifier for a group giving a comment string to
*        append to each line of the output.  It should have the same
*        number of elements as the GRPID group.  If supplied equal to
*        GRP__NOID, no comments will be written.
*     COMMEN = _LOGICAL (Given)
*        Whether to write a comment to the user about the name of the
*        output file or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2000-2001 Central Laboratory of the Research
*     Copyright (C) 2008 Science and Technology Facilities Council
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
*     25-MAY-1993 (PDRAPER):
*        Original version.
*     16-JUN-1993 (PDRAPER):
*        Added COMMEN argument as $PARAMETER does not return name.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     19-JUN-2001 (MBT):
*        Added COMGID parameter.
*     8-FEB-2008 (PWD):
*        Do nothing if the file is not opened.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'GRP_PAR'          ! GRP sysetem constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK private constants

*  Arguments Given:
      CHARACTER *  ( * ) PARAM
      INTEGER INDXLO
      INTEGER INDXHI
      CHARACTER * ( * ) TITLE
      INTEGER GRPID
      INTEGER COMGID
      LOGICAL COMMEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) NAME ! Buffer for name
      CHARACTER * ( FIO__SZFNM ) FNAME ! Buffer for file name
      CHARACTER * ( CCD1__BLEN ) COMTXT ! Buffer for comment text
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for output line
      CHARACTER * ( 1 ) COMCHR   ! Comment character
      INTEGER FD                 ! File descriptor
      INTEGER I                  ! Loop variable
      INTEGER LENG               ! Line length
      LOGICAL OPEN               ! File is open flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the group's COMMENT character if we will need it.
      IF ( COMGID .NE. GRP__NOID ) THEN
         CALL GRP_GETCC( GRPID, 'COMMENT', COMCHR, STATUS )
      END IF

*  Open the file via the named adam parameter.
      OPEN = .FALSE.
      CALL CCD1_ASFIO( PARAM, 'WRITE', 'LIST', GRP__SZNAM, FD, OPEN,
     :                 STATUS )
      IF ( OPEN ) THEN

*  Write the title
         CALL FIO_WRITE( FD, TITLE( : CHR_LEN( TITLE ) ), STATUS )

*  Loop over the required index extracting the names and then writing
*  them into the file.
         DO 1 I = INDXLO, INDXHI
            NAME = ' '

*  Get the item name.
            CALL GRP_GET( GRPID, I, 1, NAME, STATUS )
            CALL MSG_SETC( 'NAME', NAME )

*  Construct the output line.
            IF ( COMGID .EQ. GRP__NOID ) THEN

*  No comments to write - just use the name.
               CALL MSG_LOAD( ' ', '^NAME', LINE, LENG, STATUS )
            ELSE

*  If we have comments to write, extract them from the comment group.
               CALL MSG_SETC( 'COMMENTCHR', COMCHR )
               CALL GRP_GET( COMGID, I, 1, COMTXT, STATUS )
               CALL MSG_SETC( 'COMMENT', COMTXT )
               CALL MSG_LOAD( ' ', '^NAME ^COMMENTCHR ^COMMENT', LINE,
     :                        LENG, STATUS )
            END IF

*  Now write out the constructed line.
            CALL FIO_WRITE( FD, LINE( 1:LENG ), STATUS )
 1       CONTINUE

         IF ( COMMEN .AND. STATUS .EQ. SAI__OK ) THEN

*  Write a comment about the name of the list.
            CALL FIO_FNAME( FD, FNAME, STATUS )
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL CCD1_MSG( ' ',
     :                  '  Namelist written to file: ^FNAME', STATUS )
         END IF
         CALL FIO_CLOSE( FD, STATUS )
      END IF

      END
* $Id$
