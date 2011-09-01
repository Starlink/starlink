#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>              /* Utility functions                       */
#include <stddef.h>

/* VMS version include files:                                               */
/* =========================                                                */
#if defined( vms )
#include <stsdef.h>              /* System status codes (VMS)               */
#endif

#include "star/mem.h"
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_deall_mem( size_t size, void **pntr )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_deall_mem                                                         */

/* Purpose:                                                                 */
/*    Deallocate memory.                                                    */

/* Invocation:                                                              */
/*    rec_deall_mem( size, pntr )                                           */

/* Description:                                                             */
/*    This function deallocates memory previously allocated by              */
/*    rec_alloc_mem.                                                        */

/* Parameters:                                                              */
/*    size_t size                                                           */
/*       The amount of memory allocated in bytes.                           */
/*    void **pntr                                                           */
/*       Address of a pointer to the allocated memory. A null pointer is    */
/*       returned.                                                          */

/* Returned Value:                                                          */
/*    int rec_deall_mem                                                     */
/*       The global status value current on exit.                           */

/* Notes:                                                                   */
/*    -  This routine attempts to execute even if the HDS global status is  */
/*    set on entry.                                                         */
/*    -  Care should be taken that the pointer supplied is either valid or  */
/*    null (in the latter case the routine has no effect), since an invalid */
/*    pointer value cannot be detected.                                     */

/* Copyright:                                                               */
/*    Copyright (C) 1991 Science & Engineering Research Council             */
/*    Copyright (C) 2006 Particle Physics and Astronomy Research Council    */

/*  Licence:                                                                */
/*     This program is free software; you can redistribute it and/or        */
/*     modify it under the terms of the GNU General Public License as       */
/*     published by the Free Software Foundation; either version 2 of       */
/*     the License, or (at your option) any later version.                  */

/*     This program is distributed in the hope that it will be              */
/*     useful, but WITHOUT ANY WARRANTY; without even the implied           */
/*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR              */
/*     PURPOSE. See the GNU General Public License for more details.        */

/*     You should have received a copy of the GNU General Public            */
/*     License along with this program; if not, write to the Free           */
/*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,       */
/*     MA 02111-1307, USA                                                   */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    6-FEB-1991 (RFWS):                                                    */
/*       Original version, rewrite for portability.                         */
/*    27-MAR-1991 (RFWS):                                                   */
/*       Improved prologue.                                                 */
/*    13-DEC-1991 (RFWS):                                                   */
/*       Changed VMS implementation to manage address space more            */
/*       effectively.                                                       */
/*    23-FEB-2006 (TIMJ):                                                   */
/*       use starmem                                                        */
/*    04-APR-2006 (TIMJ):                                                   */
/*       use size_t                                                         */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
#if defined( vms )               /* VMS version local variables:            */
      int npage;                 /* Number of pages allocated               */
      unsigned int base;         /* Base address of allocated pages         */
      unsigned int systat;       /* System status code                      */
#endif

/* External References:                                                     */
#if defined( vms )               /* VMS version system calls:               */
      unsigned int LIB$FREE_VM_PAGE
         ( int *npage,
           unsigned int *base );
#endif

/*.                                                                         */

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* If this was a "big" memory request, then calculate the number of pages   */
/* allocated and return them to the global page pool.                       */
      if ( size >= REC__BIGMEM )
      {
	npage = 1 + ( (int)size - 1 ) / 512;
         base = (unsigned int) *pntr;
         systat = LIB$FREE_VM_PAGE( &npage, &base );

/* If an error occurred, set the global status and report it. Do this       */
/* inside a new error reporting environment.                                */
         if ( !( systat & STS$M_SUCCESS ) )
         {
            emsBegin( &hds_gl_status );
            hds_gl_status = DAT__NOMEM;
            emsSeti( "NBYTES", (int)size );
            emsSyser( "MESSAGE", systat );
            emsRep( "REC_DEALL_MEM_1",
                       "Unable to release a block of ^NBYTES bytes of memory - \
^MESSAGE",
                       &hds_gl_status );
            emsEnd( &hds_gl_status );
         }
      }

/* If this was a small memory request (or not running on VMS)...            */
      else
#endif

/* Release the memory previously allocated by malloc.                       */
      {
         MEM_FREE( *pntr );
      }

/* Return a null pointer.                                                   */
      *pntr = NULL;

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
