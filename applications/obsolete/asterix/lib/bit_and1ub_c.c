/*
*+
*  Name:
*     BIT_AND1UB

*  Purpose:
*     Returns the bit-wise AND of array of UNSIGNED BYTE with mask value

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL BIT_AND1UB( N, ARRAY, MASK, STATUS )

*  Description:
*     Provides portable bit-wise AND of unsigned bytes with a mask.

*  Arguments:
*     N = INTEGER (given)
*        Number of values to mask
*     ARRAY = BYTE[] (given and returned)
*        The array to masked
*     MASK = BYTE (given)
*        The mask value
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

F77_SUBROUTINE(bit_and1ub)( INTEGER(n), UBYTE_ARRAY(array),
                            UBYTE(mask), INTEGER(status) )
  {
  GENPTR_INTEGER(n)
  GENPTR_UBYTE_ARRAY(array)
  GENPTR_UBYTE(mask)
  GENPTR_INTEGER(status)

  int		i;			/* Loop over array */

/* Check inherited global status on entry */
  if ( *status != SAI__OK )
    return;

  for( i=0; i<(*n) ; i++ )
    array[i] &= (*mask);
  }
