/*
*+
*  Name:
*     BIT_NOTUB

*  Purpose:
*     Returns the bit-wise NOT of its UNSIGNED BYTE argument

*  Language:
*     Starlink ANSI C

*  Invocation:
*     RESULT = BIT_NOTUB( A )

*  Description:
*     Provides portable bit-wise NOT of unsigned bytes.

*  Arguments:
*     A = BYTE (given)
*        First argument

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
*     bit Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bit.html

*  Keywords:
*     package:bit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Jun 1995 (DJA):
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

F77_UBYTE_FUNCTION(bit_notub)( UBYTE(a) )
  {
  GENPTR_UBYTE(a)

  return ((*a) ^ 255);
  }
