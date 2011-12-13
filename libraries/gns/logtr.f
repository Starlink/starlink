      SUBROUTINE GNS_1LOGTR ( LNAME, EQNAM, LENEQ, STATUS )

*+
*  Name:
*     GNS_1LOGTR

*  Purpose:
*     Perform one logical translation of the given name

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine translates the given environment variable into an
*     equivalence name.
*     If the translation fails then a zero length string is returned.

*  Arguments:
*     LNAME = CHAR (Given)
*         Name to translate
*     EQNAM = CHAR (Returned)
*         Equivalence name
*     LENEQ = INTEGER (Returned)
*         Length of name
*     STATUS = INTEGER (Given & Returned)
*         Inherited status

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     DLT: D L Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     15-Jan-1991 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      CHARACTER *(*) LNAME
      CHARACTER *(*) EQNAM
      INTEGER LENEQ
      INTEGER STATUS

      INTEGER STRLEN
      CHARACTER*255 STRING, STRING1

*   Initialise the output arguments
      LENEQ = 0
      EQNAM = ' '
      IF ( STATUS .EQ. 0 ) THEN

*   Copy the input string into a local variable
         CALL GNS_1TRIM( LNAME, STRING, STRLEN )

*   Translate the name
         CALL GETENV(STRING(:STRLEN), STRING1)

*   Trim the resulting string
         CALL GNS_1TRIM( STRING1, EQNAM, LENEQ )

      ENDIF

      END
