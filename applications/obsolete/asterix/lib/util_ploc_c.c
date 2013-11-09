/*
*+
*  Name:
*     UTIL_PLOC

*  Purpose:
*     Returns address of its argument

*  Language:
*     Starlink ANSI C

*  Invocation:
*     INTEGER ADDR
*     ADDR = UTIL_PLOC( ARG )

*  Description:
*     Returns the address of its argument. Used by Fortran programs which
*     want to construct pointers from variables. Function is identical to
*     the intrinsic %LOC supported on many architectures, but can be used
*     with EXTERNAL data types and on some machines which don't support
*     %LOC.

*  Arguments:
*     ARG = anytype (given)
*        The address of the Fortran item whose address is required

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

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     util Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/util.html

*  Keywords:
*     package:util, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Dec 1992 (DJA):
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
F77_POINTER_TYPE *F77_EXTERNAL_NAME(util_ploc)( INTEGER(arg) )
  {
  GENPTR_INTEGER(arg)

  return (F77_POINTER_TYPE *) arg;
  }
