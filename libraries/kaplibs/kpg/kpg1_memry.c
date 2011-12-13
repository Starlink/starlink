#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "f77.h"
#include "sae_par.h"

#include <stdlib.h>
#include <stdio.h>
#include "mers.h"

F77_SUBROUTINE(kpg1_memry)( INTEGER(MEM), INTEGER(STATUS) ){
/*
*+
*  Name:
*     kpg1_memry

*  Purpose:
*     Returns memory currently being used by the current process.

*  Language:
*     C

*  Invocation:
*     CALL KPG1_MEMRY( MEM, STATUS )

*  Description:
*     This subroutine returns the maximum resident set size for the
*     current process.

*  Arguments:
*     MEM = INTEGER (Returned)
*        The memory currently being used by the current process, in
*        KB.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-NOV-2002 (DSB):
*        Original version.
*     13-MAY-2011 (TIMJ):
*        Use the C interface to errRep rather than the Fortran interface.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   GENPTR_INTEGER(MEM)
   GENPTR_INTEGER(STATUS)
   struct rusage usage;

   *MEM = 0;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

   if( getrusage( RUSAGE_SELF, &usage ) != 0 ) {
      *STATUS = SAI__ERROR;
      errRepf( "", "%s", STATUS, strerror( errno ) );
   } else {
      *MEM = (int) usage.ru_maxrss*getpagesize()/1024;
   }
}
