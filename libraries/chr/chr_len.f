      INTEGER FUNCTION CHR_LEN( STRING )
*+
*  Name:
*     CHR_LEN

*  Purpose:
*     Return the length of a string, ignoring trailing blanks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_LEN( STRING )

*  Description:
*     Find length of string, ignoring trailing blanks.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string whose length is to be determined.

*  Returned Value:
*     CHR_LEN = INTEGER
*        Returns the used length of the string.

*  Algorithm:
*     Portable version:
*        Start from string's declared size and work back to start
*        until first non-blank character is found.
*     VMS-specific version:
*        This has been found to be slower unless there are around 150
*        trailing spaces.
*        The utility copies the given string to a local string, removing
*        trailing blank and tab characters in the process, and returns
*        the number of characters copied into the local string.
*        The local string, of course, has to be able to hold anything up
*        to the largest string possible on the Vax. This may be rather
*        expensive in its use of space. If it is, a shorter local string
*        could be used and the status returned from the VAX utility
*        could be used to check for truncation and if truncation has
*        occured then the standard Fortran method used.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1990 Science & Engineering Research Council.
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
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     2-OCT-1984 (ACD):
*        Documentation improved.
*     13-SEP-1988 (AJC):
*        Add VAX VMS version in comments (slower unless the tail is
*        very long).
*     3-OCT-1988 (AJC):
*        Improve documentation.
*     26-JAN-1990 (AJC):
*        Improve Method comments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Portable version.
*  Local Variables:
      INTEGER TOTLEN             ! Declared length of the string
      INTEGER IPOSN              ! Current position in the string

*.

*  Get the declared length of the string.
      TOTLEN = LEN( STRING )

*  Loop to find the position of the last non-blank character.
      DO 10 IPOSN = TOTLEN, 1, -1
         IF ( STRING( IPOSN : IPOSN ) .NE. ' ' ) GO TO 20
 10   CONTINUE
 20   CONTINUE

      CHR_LEN = IPOSN

*  VMS-specific version.
*  Local Constants:
*     INTEGER MAXLEN             ! Maximum string length that can be handled
*     PARAMETER ( MAXLEN = 65535 )

*  Local Variables:
*     CHARACTER * ( MAXLEN ) LOCAL ! To contain STRING with trailing
*                                  ! blanks and tabs removed
*
*     INTEGER TOTLEN             ! Declared length of the string

*  Get the declared length of the string to be truncated.
*     TOTLEN = LEN( STRING )

*  Copy STRING to LOCAL removing trailing blanks and tabs and return
*  the number of characters copied in CHR_LEN. If truncation occurs,
*  CHR_LEN will correspond to the maximum number of characters that
*  can be held in LOCAL.
*     CALL STR$TRIM( LOCAL( 1 : TOTLEN ), STRING, CHR_LEN )

      END
