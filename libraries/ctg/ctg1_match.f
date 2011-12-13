      LOGICAL FUNCTION CTG1_MATCH( TEMPLT, TEST, STATUS )
*+
*  Name:
*     CTG_MATCH

*  Purpose:
*     See if a given string matches a wild-card template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     MATCH = CTG1_MATCH( TEMPLT, TEST, STATUS )

*  Description:
*     This routine returns .TRUE. if the supplied test string matches the
*     supplied wild-card template.

*  Arguments:
*     TEMPLT = CHARACTER*(*) (Given)
*        The wild-card template.
*     TEST = CHARACTER*(*) (Given)
*        The test string to be compared to the template.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     CTG1_MATCH = LOGICAL
*        .TRUE. if the test string matches the template.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David Berry (STARLINK)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1999 (DSB):
*        Original version.
*     15-APR-2005 (PWD):
*        Parameterise backslash use for better portability.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'CTG_CONST'        ! CTG private constants.

*  Local Constants:
      CHARACTER ESC*(1)          ! Single backslash
*  Some compilers need '\\' to get '\', which isn't a problem as Fortran
*  will truncate the string '\\' to '\' on the occasions when that isn't
*  needed.
      PARAMETER( ESC = '\\' )

*  Arguments Given:
      CHARACTER TEMPLT*(*)
      CHARACTER TEST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Externals:
      LOGICAL CHR_WILD
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER MAT*(GRP__SZFNM)   ! Wild-card match string
      CHARACTER TEXT*(GRP__SZFNM)  ! File type template
      CHARACTER TEXT2*(GRP__SZFNM) ! Temporary file type template
      INTEGER LTEST                ! Length of test string
      INTEGER LTEXT                ! Length of template text
      INTEGER NSUB                 ! No. of substitutions made
*.

*  Initialise the returned flag.
      CTG1_MATCH = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  We use CHR_WILD to compare the strings. CHR_WILD uses * for its
*  multi-character wild card, but % for its single character wild-card. If
*  these are not the same as those used by the native operating system, then
*  we need to put a CHR_WILD escape character "\" in front of any existing
*  CHR_WILD wild cards in the template, and translate the native wild-cards
*  into CHR_WILD wild-cards.
      IF( CTG__WILD1 .NE. '%' ) THEN
         CALL CTG1_SUBST( TEMPLT, '%', ESC//'%', .TRUE., TEXT, NSUB,
     :                    STATUS )
         CALL CHR_TRCHR( CTG__WILD1, '%', TEXT, STATUS )
      END IF

      IF( CTG__WILD2 .NE. '*' ) THEN
         CALL CTG1_SUBST( TEXT, '*', ESC//'*', .TRUE., TEXT2, NSUB,
     :                    STATUS )
         TEXT = TEXT2
         CALL CHR_TRCHR( CTG__WILD2, '*', TEXT, STATUS )
      END IF

*  Now match the file type template against the FMTS string, ignoring
*  trailing spaces.
      LTEST = CHR_LEN( TEST )
      LTEXT = CHR_LEN( TEXT )

      CTG1_MATCH = CHR_WILD( TEST( : LTEST ), TEXT( : LTEXT ), MAT )

      END
