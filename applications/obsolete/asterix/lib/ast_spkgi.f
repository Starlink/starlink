      SUBROUTINE AST_SPKGI( PKG )
*+
*  Name:
*     AST_SPKGI

*  Purpose:
*     Mark an ASTERIX sub-package as initialised

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AST_SPKGI( PKG )

*  Description:
*     Mark the specified package as loaded

*  Arguments:
*     PKG = INTEGER (given)
*        Package code, see AST_PKG include file

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     AST Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ast.html

*  Keywords:
*     package:ast, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Mar 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'AST_PKG'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'AST_PKG_CMN'                                 ! AST common block
*       AST__LPACK[AST__MXPKG] = LOGICAL (returned)
*         Package loaded array

*  Arguments Given:
      INTEGER			PKG
*.

*  Set the flag
      AST__LPACK(PKG) = .TRUE.

      END
