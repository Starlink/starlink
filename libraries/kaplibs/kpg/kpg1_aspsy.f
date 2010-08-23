      SUBROUTINE KPG1_ASPSY( SYNON, TRAN, STATUS )
*+
*  Name:
*     KPG1_ASPSY

*  Purpose:
*     Establishes synonyms for AST attribute names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASPSY( SYNON, TRAN, STATUS )

*  Description:
*     This routine establishes synonyms for AST attribute names or
*     attribute qualifiers (qualifiers are strings in parenthesise following
*     the attribute name - the "graphical elements" used by some Plot
*     attributes are examples of qualifiers). The routine KPG1_ASCHP is
*     used to translated synonyms into values recognised by AST.
*
*     Note, substitutions for synonyms are performed in the order in
*     which they are defined, with later substitutions potentially
*     modifying the results of earlier substitutions. For this reason,
*     synonyms for specific name/qualifer pairs (eg"FORMAT(VEC*TOR)" )
*     should be defined *before* synonyms for qualifiers alone (eg
*     "(VEC*TOR)" ). If these two examples were defined in the opposite
*     order, then the qualifier in FORMAT(VEC) (i.e. "VEC") would get
*     replaced by the translation supplied for synonym "(VEC*TOR)", so
*     that the resulting string would then fail to match the synonym
*     "FORMAT(VEC*TOR)" and so would not result in the correct translation.

*  Arguments:
*     SYNON = CHARACTER * ( * ) (Given)
*        A synonym for an AST attribute name and/or qualifier. The
*        supplied value is converted to upper case, and stripped of spaces.
*        If the synonym contains just a qualifier (i.e. no attribute
*        name), then KPG1_ASCHP will substitute the qualifier from the
*        translation irespective of the attribute name. Minimum
*        abbreviations for attribute qualifiers may be given by including
*        an asterisk in the qualifier to mark the end of the minimum
*        abbreviation. If a blank value is supplied, then the resources used
*        to store the synonyms and translations are released.
*     TRAN = CHARACTER * ( * ) (Given)
*        The translation for the supplied synonym. This should be a legal
*        AST attribute name/qualifier combination (but no check is made
*        on this). The supplied value is converted to upper case, and
*        stripped of spaces.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-MAR-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants (needed by KPG_AST)
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      CHARACTER SYNON*(*)
      CHARACTER TRAN*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTING = INTEGER (Read and Write)
*           GRP identifier for group holding synonyms.
*        ASTNPS = INTEGER (Read and Write)
*           No. of defined synonyms.
*        ASTOUG = INTEGER (Read and Write)
*           GRP identifier for group holding corresponding AST attribute names.

*  Local Variables:
      CHARACTER TEXT*(GRP__SZNAM)! Cleaned text
      LOGICAL VALID1             ! Is the synonym group identifier valid?
      LOGICAL VALID2             ! Is the attribute name group identifier valid?
      LOGICAL VALID              ! Are both group identifiers valid?

*.

*  See if the groups holding names and synonyms have been created. Do
*  this in a new error reporting context since GRP_VALID checks the
*  inherited status, but we want a usable result even if the inherited
*  status is set.
      CALL ERR_BEGIN( STATUS )
      CALL GRP_VALID( ASTING, VALID1, STATUS )
      CALL GRP_VALID( ASTOUG, VALID2, STATUS )
      VALID = ( VALID1 .AND. VALID2 )
      CALL ERR_END( STATUS )

*  If we are freeing resources, just delete the groups (if valid) without
*  checking the inherited status value.
      IF( SYNON .EQ. ' ' ) THEN
         IF( VALID1 ) CALL GRP_DELET( ASTING, STATUS )
         IF( VALID2 ) CALL GRP_DELET( ASTOUG, STATUS )
         ASTNPS = 0

*  Otherwise, check the inherited global status.
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  If the groups holding names and synonyms have not yet been created,
*  create them now.
         IF( .NOT. VALID ) THEN
            IF( VALID1 ) CALL GRP_DELET( ASTING, STATUS )
            IF( VALID2 ) CALL GRP_DELET( ASTOUG, STATUS )
            CALL GRP_NEW( 'AST attribute names', ASTOUG, STATUS )
            CALL GRP_NEW( 'AST attribute syonyms', ASTING, STATUS )
            ASTNPS = 0
         END IF

*  Add the supplied name and synonym (after stripping spaces
*  and converting to upper case).
         TEXT = TRAN
         CALL CHR_UCASE( TEXT )
         CALL CHR_RMBLK( TEXT )
         CALL GRP_PUT( ASTOUG, 1, TEXT, 0, STATUS)

         TEXT = SYNON
         CALL CHR_UCASE( TEXT )
         CALL CHR_RMBLK( TEXT )
         CALL GRP_PUT( ASTING, 1, TEXT, 0, STATUS)
         ASTNPS = ASTNPS + 1

      END IF

      END
