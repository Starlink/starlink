      LOGICAL FUNCTION AST_QPKGI( PKG )
*+
*  Name:
*     AST_QPKGI

*  Purpose:
*     Query whether the specified package is loaded

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = AST_QPKGI( PKG )

*  Description:
*     Returns true or false depending on whether the ASTERIX sub-package
*     known with code PKG has registered itself as initialised.

*  Arguments:
*     PKG = INTEGER (given)
*        Code of package to test

*  Returned Value:
*     AST_QPKGI = LOGICAL
*        Is package initialised?

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
*     Roughly 100 to 300 nanoseconds on typical machines at time of
*     original version

*  External Routines Used:
*     {facility_or_package}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     AST Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ast.html

*  Keywords:
*     package:ast, usage:public

*  Copyright:
*     {routine_copyright}

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
      INCLUDE 'AST_PKG'

*  Global Variables:
      INCLUDE 'AST_PKG_CMN'
*        AST__LPACK[AST__MXPKG] = LOGICAL (read)
*           Return value of PKG'th element

*  Arguments Given:
      INTEGER			PKG

*  Local Data:
      DATA			AST__LPACK/AST__MXPKG*.FALSE./
*.

*  Return result
      AST_QPKGI = AST__LPACK(PKG)

      END
