/* Subroutine:  psx_srand( seed, status )
*+
*  Name:
*     PSX_SRAND

*  Purpose:
*     Set the seed for the random number generator

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_SRAND( SEED, STATUS )

*  Description:
*     The argument SEED is used to set a new seed for the sequence of
*     random numbers returned by the subroutine PSX_RAND. If PSX_SRAND
*     is called with the same value of SEED, then the values returned by
*     subsequent calls to PSX_RAND will be the same. If PSX_RAND is
*     called before calling PSX_SRAND, then the sequence of random
*     number returned by PSX_RAND will the the same as if PSX_SRAND had
*     been called with SEED set to one.

*  Arguments:
*     SEED = INTEGER (Given)
*        The seed for the random number generator.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The range of values allowed for SEED is not specified. It is
*        unlikely that values between 1 and the maximum integral value
*        that PSX_RAND can return will cause problems.

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.10.2.2

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

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
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/


/* Global Constants:							    */

#include <stdlib.h>		 /* Standard C library			    */
#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_srand)( INTEGER(seed), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(seed)
   GENPTR_INTEGER(status)

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Set the random number seed.						    */

   srand( *seed );
}
