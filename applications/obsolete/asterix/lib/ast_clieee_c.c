/*
*+
*  Name:
*     AST_CLIEEE

*  Purpose:
*     Clear unimportant floating point exceptions

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL AST_CLIEEE()

*  Description:
*    Clear IEEE floating point exceptions on those machines which generate
*    them. These the floating underflow and floating inexact exceptions.

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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  References:
*     ast Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/ast.html

*  Keywords:
*     package:ast, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Jun 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*
 *  Include files
 */
#include "f77.h"

/*
 *  Body of code
 */
F77_SUBROUTINE(ast_clieee)()
  {
#if defined(sun4_Solaris) || defined(sun4)
  char *out;

  ieee_flags("clear","exception","inexact",&out);
  ieee_flags("clear","exception","underflow",&out);
#endif
  }
