      SUBROUTINE CCD1_DECL2( LINE, LINNUM, HAVIND, NUMVAL, STATUS )
*+
*  Name:
*     CCD1_DECL2

*  Purpose:
*     Decodes a line of characters returning the number of values after
*     the index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DECL2( LINE, LINNUM, HAVIND, NUMVAL, STATUS )

*  Description:
*     This routine decodes a line of characters into words.  The line
*     must only contains words separated by blanks (lines read in by
*     CCD1_RDLIN are recommended).  The number of words are counted. If
*     at least one word is present then HAVIND is set true and a index
*     value is assumed present. If more than 1 word is present the
*     excess is returned as NUMVAL. This routine is expected to be used
*     in determining the memory requirements to decode the data into its
*     numeric values.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        Line of blank separated words to be decoded into words.
*     LINNUM = INTEGER (Given)
*        A line number to be issued with error messages.
*     HAVIND = LOGICAL (Returned)
*        Set true it at least one value is present.
*     NUMVA = INTEGER (Returned)
*        The number of values after the index.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUL-1992 (PDRAPER):
*        Original Version.
*     17-MAR-1995 (PDRAPER):
*        Removed unused NCHAR argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LINE
      INTEGER LINNUM

*  Arguments Returned:
      LOGICAL HAVIND
      INTEGER NUMVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FIRST
      INTEGER LAST
      LOGICAL NOTFND
      INTEGER IAT
      INTEGER NVAL

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop counting the number of space seperated words.
      NVAL = 0
      NOTFND = .FALSE.
      IAT = 1

*  Loop while STATUS is ok and a word has been found
 1    CONTINUE                ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. NOTFND ) THEN
         CALL KPG_NXWRD( LINE, IAT, FIRST, LAST, NOTFND, STATUS )

*  Change search to new position for next time around.
         IAT = LAST + 1
         IF ( STATUS .EQ. SAI__OK .AND. .NOT. NOTFND ) THEN

*  Increment word count.
            NVAL = NVAL + 1
         END IF
         GO TO 1
      END IF

*  How many words have we found? Need at least one.
      IF ( NVAL .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         HAVIND = .FALSE.
         CALL MSG_SETI( 'LINNUM', LINNUM )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_DECL2ERR1',
     :   '  File contains no values - line ^LINNUM' ,STATUS )
      ELSE

*  Ok set the number of values and HAVIND.
         HAVIND  = .TRUE.
         NUMVAL = MAX( 0, NVAL - 1 )
      END IF

      END
* $Id$
