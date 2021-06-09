#include <time.h>
#include "f77.h"
#include "prm_par.h"

F77_SUBROUTINE(kpg1_elptm)( INTEGER_ARRAY(CONTXT), DOUBLE(ELPTIM) ) {
/*
*+
*  Name:
*     KPG1_ELPTM

*  Purpose:
*     Returns the elapsed time since the previous call to this function.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_ELPTM( CONTXT, ELPTIM )

*  Description:
*     This function returns the elapsed time since the previous call to
*     this function, in seconds.

*  Arguments:
*     CONTXT( 4 ) = INTEGER (Given and Returned)
*        Workspace in which to store context information. It should not
*        be changed in any way between calls to this function.
*     ELPTIM = DOUBLE PRECISION (Returned)
*        The elpased time since the previous call to this function, in seconds.
*        An arbitrary value is returned on the first call. If ELPTIM  holds
*        VAL__BADD on entry, then no value is returned (but the current
*        time is still recorded in CONTXT).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory.
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
*     9-JUN-2021 (DSB):
*        Original version.

*-
*/
   GENPTR_INTEGER_ARRAY(CONTXT)
   GENPTR_DOUBLE(ELPTIM)
   struct timespec tp;
   struct timespec *old_tp;
   size_t dsec, dnsec;

/* Get the current time */
   clock_gettime( CLOCK_REALTIME, &tp );

/* Get a pointer to the time in the supplied context structure. This
   will be junk on the first call to this function. */
   old_tp = (struct timespec *) CONTXT;

/* If an ELPTIM value is to be returned.... */
   if( *ELPTIM != VAL__BADD ) {

/* Get the change in time since the last call to this function. */
      *ELPTIM = (tp.tv_sec - old_tp->tv_sec) +
                1.0E-9*(tp.tv_nsec - old_tp->tv_nsec);
   }

/* Store the current time in the supplied context structure. */
   *old_tp = tp;
}
