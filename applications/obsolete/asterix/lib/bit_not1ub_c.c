/*
*+
*  Name:
*     BIT_NOT1UB

*  Purpose:
*     Returns the bit-wise NOT of array of UNSIGNED BYTEs.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL BIT_NOT1UB( N, ARRAY, STATUS )

*  Description:
*     Provides portable bit-wise NOT of unsigned bytes.

*  Arguments:
*     N = INTEGER (given)
*        Number of values to mask
*     ARRAY = BYTE[] (given and returned)
*        The array to masked
*     STATUS = INTEGER (given)
*        The global status.

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
*     bit Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/bit.html

*  Keywords:
*     package:bit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     24 Feb 1994 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*
 *  Include files
 */
#include "sae_par.h"
#include "f77.h"

/*
 *  Body of code
 */

F77_SUBROUTINE(bit_not1ub)( INTEGER(n), UBYTE_ARRAY(array), INTEGER(status) )
  {
  GENPTR_INTEGER(n)
  GENPTR_UBYTE_ARRAY(array)
  GENPTR_INTEGER(status)

  int		i;			/* Loop over array */

/* Check inherited global status on entry */
  if ( *status != SAI__OK )
    return;

  for( i=0; i<(*n) ; i++ )
    array[i] ^= 255;
  }
