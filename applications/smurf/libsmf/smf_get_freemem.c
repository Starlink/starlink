/*
 *+
 *  Name:
 *     smf_get_freemem

 *  Purpose:
 *     Determine free memory on the system.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     C function

 *  Invocation:
 *     dim_t smf_get_freemem( double * mbytes, dim_t *pagesize,
 *                             int64_t *physsize, int *status );

 *  Arguments:
 *     mbytes = double * (Returned)
 *        Pointer to variable to hold the free memory in
 *        megabytes if required. Can be NULL.
 *     pagesize = dim_t * (Returned)
 *        Pagesize in bytes. Can be NULL.
 *     physsize = int64_t * (Returned)
 *        Total physical memory in bytes. Can be NULL.
 *     status = int* (Given and Returned)
 *        Pointer to inherited status.

 *  Returned Value:
 *     The amount of free memory in megabytes. Will return 0 if no value
 *     could be determined or if status is bad on entry.

 *  Description:
 *     Calculates the amount of free memory on the system and returns it
 *     in bytes.

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     EC: Ed Chapin (UBC)

 *  History:
 *     2009-10-12 (TIMJ):
 *       Initial version
 *     2009-10-16 (EC):
 *       Add physsize parameter.
 *     2011-01-25 (TIMJ):
 *        Change type of physsize to guarantee it is a 64-bit value.

 *  Copyright:
 *     Copyright (C) 2009-2011 Science & Technology Facilities Council.
 *     Copyright (C) 2009 University of British Columbia.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 3 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if HAVE_MACH_MACH_H
# define HAVE_MACH_VM 1
# include <mach/mach.h>
# include <mach/mach_host.h>
#endif

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"



dim_t smf_get_freemem ( double *mbytes, dim_t * pagesize,
                         int64_t * physsize, int * status ) {
  int64_t mem_used = 0;
  int64_t mem_free = 0;
  int64_t mem_total = 0;
  double freembytes = 0.0;

  if (*status != SAI__OK) return mem_free;


# if HAVE_MACH_VM
  {
    mach_port_t host_port;
    mach_msg_type_number_t host_size;
    vm_size_t vmpagesize;
    vm_statistics_data_t vm_stat;

    host_port = mach_host_self();
    host_size = sizeof(vm_statistics_data_t) / sizeof(integer_t);
    host_page_size(host_port, &vmpagesize);
    if (pagesize) *pagesize = vmpagesize;

    if (host_statistics(host_port, HOST_VM_INFO, (host_info_t)&vm_stat, &host_size) == KERN_SUCCESS) {
      /* Stats in bytes */
      mem_used = (vm_stat.active_count +
                  vm_stat.inactive_count +
                  vm_stat.wire_count) * vmpagesize;
      mem_free = vm_stat.free_count * vmpagesize;
      mem_total = mem_used + mem_free;
    }
  }
#else

#ifdef _SC_AVPHYS_PAGES
  /* Figure out available physical memory from sysconf */
  {
    dim_t mypagesize;
    mypagesize = sysconf(_SC_PAGE_SIZE);
    mem_free = sysconf(_SC_AVPHYS_PAGES) * mypagesize;
    mem_total = sysconf(_SC_PHYS_PAGES) * mypagesize;
    mem_used = mem_total - mem_free;
    if (pagesize) *pagesize = mypagesize;
  }
#endif /* SC_AVPHYS_PAGES */

#endif  /* MACH_VM */


  if (mem_free > 0) {
    freembytes = (double) mem_free / (double)SMF__MIB;
    msgOutiff( MSG__DEBUG, "", "Free memory: %g MB Used Memory: %g MB  Total Memory: %g MB", status,
               freembytes, (double)mem_used/(double)SMF__MIB, (double)mem_total/(double)SMF__MIB
               );
  } else {
    msgOutif( MSG__DEBUG,"", "Unable to determine free memory", status );
  }

  /* sort out return values */
  if (mbytes) *mbytes = freembytes;
  if (physsize) *physsize = mem_total;
  return mem_free;
}
