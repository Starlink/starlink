      SUBROUTINE AST_CLOSE()
*+
*  Name:
*     AST_CLOSE

*  Purpose:
*     Shut down ASTERIX sub-systems started by AST_INIT

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AST_CLOSE()

*  Description:
*     Close down the file system, dynamic memory management and the user
*     interface. IEEE inexact and underflow exceptions are cleared.

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
*     9 Jun 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*.

*  User interface
      CALL USI_CLOSE()

*  Dynamic memory management
      CALL DYN_CLOSE()

*  Clear floating exceptions
      CALL AST_CLIEEE()

      END
