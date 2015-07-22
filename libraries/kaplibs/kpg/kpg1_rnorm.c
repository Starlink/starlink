#include "f77.h"
#include "kaplibs.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg1_rnorm)( INTEGER(EL), DOUBLE_ARRAY(ARRAY), INTEGER(SEED),
                            INTEGER(STATUS) ) {
/*
*+
*  Name:
*     KPG1_RNORM

*  Purpose:
*     Returns a set of random samples from a normal distribution.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_RNORM( EL, ARRAY, SEED, STATUS )

*  Description:
*     This routine returns a  set of random samples from a normal
*     distribution with mean=0.0 and standard deviation=1.0. It uses
*     the default GSL random number generator type.

*  Arguments:
*     EL = INTEGER (Given)
*         The number of samples to return.
*     ARRAY(EL) = DOUBLE PRECISION (Returned)
*        The array in which to return the values.
*     SEED = INTEGER (Given)
*        The seed to use. If zero or negative, the value of environment
*        variable STAR_SEED is used if set, and a non-repeatable value
*        is used if STAR_SEED is not set.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     22-JUL-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/

   GENPTR_INTEGER(EL)
   GENPTR_DOUBLE_ARRAY(ARRAY)
   GENPTR_INTEGER(SEED)
   GENPTR_INTEGER(STATUS)

   kpg1Rnorm( *EL, ARRAY, *SEED, STATUS );

}
