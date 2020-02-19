#include <time.h>
#include "f77.h"
#include "prm_par.h"

F77_SUBROUTINE(kpg1_cputm)( INTEGER_ARRAY(CONTXT), DOUBLE(CPUTIM) ) {
/*
*+
*  Name:
*     KPG1_CPUTIME

*  Purpose:
*     Returns the CPU time used by the current process.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_CPUTM( CONTXT, CPUTIM )

*  Description:
*     This function allows the CPU time used by the current process to be
*     measured, in seconds. Each time it is called it returns the CPU time
*     used since the previous call. This includes CPU used by all threads
*     within the current process.

*  Arguments:
*     CONTXT( 4 ) = INTEGER (Given and Returned)
*        Workspace in which to store context information. It should not
*        be changed in any way between calls to this function.
*     CPUTIM = DOUBLE PRECISION (Returned)
*        The CPU used since the previous call to this function, in seconds.
*        An arbitrary value is returned on the first call. If CPUTIM  holds
*        VAL__BADD on entry, then no value is returned (but the current
*        CPU time is still recorded in CONTXT).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.
*     All Rights Reserved.

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
*     DSB: David S. Berry

*  History:
*     18-FEB-2020 (DSB):
*        Original version.

*-
*/
   GENPTR_INTEGER_ARRAY(CONTXT)
   GENPTR_DOUBLE(CPUTIM)

/* Get the current CPU time */
   clock_t cputime = clock();

/* Get a pointer to the CPU time in the supplied context structure. This
   will be junk on the first call to this function. */
      clock_t *old_cputime = (clock_t *) CONTXT;

/* If a CPUTIM value is to be returned.... */
   if( *CPUTIM != VAL__BADD ) {

/* Get the change in CPU time since the last call to this function. */
      *CPUTIM = cputime - *old_cputime;

/* Normalise to seconds. */
      *CPUTIM /= CLOCKS_PER_SEC;
   }

/* Store the current CPU time in the supplied context structure. */
   *old_cputime = cputime;
}
