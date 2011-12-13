      SUBROUTINE CTG1_FPARS( SPEC, DIR, BN, SUF, EXT, STATUS )
*+
*  Name:
*     CTG1_FPARS

*  Purpose:
*     Extracts fields from a full catalogue specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_FPARS( SPEC, DIR, BN, SUF, EXT, STATUS )

*  Description:
*     This routine extracts and returned fields from the supplied
*     catalogue specification.

*  Arguments:
*     SPEC = CHARACTER * ( * ) (Given)
*        The full catalogue file spec.
*     DIR = CHARACTER * ( * ) (Returned)
*        The directory path (ending at the final "\" in the spec).
*     BN = CHARACTER * ( * ) (Returned)
*        The file base name. Ends with the character preceeding the first
*        "." or "(" following the directory path.
*     SUF = CHARACTER * ( * ) (Returned)
*        Any string following the file base name, and preceeding any
*        opening parenthesis. SUF will either be blank, or begin with a
*        dot.
*     EXT = CHARACTER * ( * ) (Returned)
*        Any string enclosed in curly braces following the suffix.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 CLRC
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-SEP-1999 (DSB):
*        Original version.
*     27-DEC-2005 (TIMJ):
*        Call CHR_LASTO rather than private CTG version.
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

*  Arguments Returned:
      CHARACTER DIR*(*)
      CHARACTER BN*(*)
      CHARACTER SUF*(*)
      CHARACTER EXT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      INTEGER BNBEG              ! Index of first character in base name
      INTEGER BNEND              ! Index of last character in base name
      INTEGER DIRBEG             ! Index of first character in directory
      INTEGER DIREND             ! Index of last character in directory
      INTEGER DOT                ! Index of dot
      INTEGER PAR                ! Index of opening parenthesis
      INTEGER SUFBEG             ! Index of first character in suffix
      INTEGER SUFEND             ! Index of last character in suffix
      INTEGER LSPEC              ! Length of spec
*.

*  Initialise
      DIR = ' '
      BN = ' '
      SUF = ' '
      EXT = ' '

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

*  Find the first dot following the start of the basename.
      DOT = INDEX( SPEC( BNBEG : ), '.' )

*  Find the first "{" following the start of the basename (a potential
*  FITS extension specification).
      PAR = INDEX( SPEC( BNBEG : ), '{' )

*  The end of the current file basename is marked by the earlier of the two.
      IF( DOT .EQ. 0 ) THEN
         BNEND = PAR - 1
      ELSE IF( PAR .EQ. 0 ) THEN
         BNEND = DOT - 1
      ELSE
         BNEND = MIN( PAR, DOT ) - 1
      END IF

*  If no end marker was found use the whole string.
      IF( BNEND .EQ. -1 ) THEN
         BNEND = LSPEC

*  Otherwise, correct for the start of the string.
      ELSE
         BNEND = BNEND + BNBEG - 1
      END IF

*  The file suffix is any string following the basename, extending
*  to the first "{", or the end of the string (if there is no "{" ).
      SUFBEG = BNEND + 1
      IF( PAR .GT. 0 ) THEN
         SUFEND = PAR + BNBEG - 2
      ELSE
         SUFEND = LSPEC
      END IF

*  Extract the fields into separate variables.
      IF( DIRBEG .LE. DIREND ) DIR = SPEC( DIRBEG : DIREND )
      IF( BNBEG .LE. BNEND ) BN = SPEC( BNBEG : BNEND )
      IF( SUFBEG .LE. SUFEND ) SUF = SPEC( SUFBEG : SUFEND )
      IF( SUFEND .LT. LSPEC ) EXT = SPEC( SUFEND + 1 : LSPEC )

      END
