      SUBROUTINE NDG1_FPARS( SPEC, NDOT, DIR, BN, SUF, SEC, STATUS )
*+
*  Name:
*     NDG1_FPARS

*  Purpose:
*     Extracts fields from a full NDF specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_FPARS( SPEC, NDOT, DIR, BN, SUF, SEC, STATUS )

*  Description:
*     This routine extracts and returned fields from the supplied NDF
*     specification.

*  Arguments:
*     SPEC = CHARACTER * ( * ) (Given)
*        The full NDF file spec.
*     NDOT = INTEGER (Given)
*        The number of dots expected within the file basename. This should
*        be zero for all native NDFs, but may be non-zero for foreign files.
*     DIR = CHARACTER * ( * ) (Returned)
*        The directory path (ending at the final "\" in the spec).
*     BN = CHARACTER * ( * ) (Returned)
*        The file base name. Ends with the character preceeding the first
*        "." or "(" or "[" following the directory path.
*     SUF = CHARACTER * ( * ) (Returned)
*        Any string following the file base name, and preceeding any
*        opening parenthesis. SUF will either be blank, or begin with a
*        dot or a "[".
*     SEC = CHARACTER * ( * ) (Returned)
*        Any parenthesised string following the suffix.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2000, 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-SEP-1999 (DSB):
*        Original version.
*     18-JUL-2000 (DSB):
*        Use "[" as an additional delimiter for the basename, in order to
*        allow for foreign extension specifiers.
*     27-FEB-2001 (DSB):
*        Restructure to so that a "[..]" string is only treated as a
*        foreign extension specified if it ocurs at the end of the file basename
*        or after the file type. Such strings occurring within the file
*        basename will be treated as a globbing wildcard pattern.
*     11-NOV-2011 (DSB):
*        Added NDOT argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER SPEC*(*)
      INTEGER NDOT

*  Arguments Returned:
      CHARACTER DIR*(*)
      CHARACTER BN*(*)
      CHARACTER SUF*(*)
      CHARACTER SEC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      INTEGER BNBEG              ! Index of first character in base name
      INTEGER BNEND              ! Index of last character in base name
      INTEGER DIRBEG             ! Index of first character in directory
      INTEGER DIREND             ! Index of last character in directory
      INTEGER DOT                ! Position of next dot in basename
      INTEGER IDOT               ! Index of next dot in basename
      INTEGER LSPEC              ! Length of spec
      INTEGER SECBEG             ! Index of first character in section
      INTEGER SECEND             ! Index of last character in section
      INTEGER SUFBEG             ! Index of first character in suffix
      INTEGER SUFEND             ! Index of last character in suffix
*.

*  Initialise
      DIR = ' '
      BN = ' '
      SUF = ' '
      SEC = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the used length of the supplied spec.
      LSPEC = CHR_LEN( SPEC )

*  The directory path starts at the first character.
      DIRBEG = 1

*  Find the end of the directory path. This is the last "/" in the string.
*  DIREND will be zero if there is no "/".
      CALL CHR_LASTO( SPEC, '/', DIREND )

*  The first character in the file basename follows the last "/".
      BNBEG = DIREND + 1

*  If present the NDF section string must be the last field in the spec.
*  Check to see if the last character in the string is a closing parenthesis.
*  If so find the last opening parenthesis (this is the start of the section).
      SECBEG = 0
      SECEND = -1
      IF( SPEC( LSPEC : LSPEC ) .EQ. ')' ) THEN
         CALL CHR_LASTO( SPEC, '(', SECBEG )
         IF( SECBEG .GT. 0 ) THEN
            SECEND = LSPEC
            LSPEC = SECBEG - 1
         END IF
      END IF

*  Find the first dot following the start of the basename.
      DOT = 0
      DO IDOT = 0, NDOT
         IF( DOT .GE. 0 ) THEN
            SUFBEG = INDEX( SPEC( BNBEG + DOT : LSPEC ), '.' )
            IF( SUFBEG .GT. 0 ) THEN
               DOT = DOT + SUFBEG
            ELSE
               DOT = -1
            END IF
         END IF
      END DO

      SUFBEG = DOT
      SUFEND = -2

*  If a dot was found it marks the start of the suffix, which extends to
*  the end of the remaining string.
      IF( SUFBEG .GT. 0 ) THEN
         SUFBEG = BNBEG + SUFBEG - 1
         SUFEND = LSPEC
         LSPEC = SUFBEG - 1

*  Otherwise, if the last remaining character is a "]" the suffix
*  consists of a foreign extension specifier.
      ELSE IF( SPEC( LSPEC : LSPEC ) .EQ. ']' ) THEN

*  Find the last "[" in the string. This marks the start of the suffix.
         CALL CHR_LASTO( SPEC, '[', SUFBEG )
         IF( SUFBEG .GT. 0 ) THEN
            SUFEND = LSPEC
            LSPEC = SUFBEG - 1
         END IF

      END IF

*  The end of the remaining string is the end of the basename.
      BNEND = LSPEC

*  Extract the fields into separate variables.
      IF( DIRBEG .LE. DIREND ) DIR = SPEC( DIRBEG : DIREND )
      IF( BNBEG .LE. BNEND ) BN = SPEC( BNBEG : BNEND )
      IF( SUFBEG .LE. SUFEND ) SUF = SPEC( SUFBEG : SUFEND )
      IF( SECBEG .LE. SECEND ) SEC = SPEC( SECBEG : SECEND )

      END
