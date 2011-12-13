#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>              /* Utility functions                       */
#include <string.h>              /* String built-ins                        */
#include <errno.h>               /* Error numbers                           */

/* VMS version include files:                                               */
/* =========================                                                */
#if defined( vms )
#include <stsdef.h>              /* System status codes (VMS)               */
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error codes                       */
#include "f77.h"                 /* Fortran <--> C interface facilities     */

   int rec_alloc_xmem( size_t size, void **pntr )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_alloc_xmem                                                        */

/* Purpose:                                                                 */
/*    Allocate exportable memory.                                           */

/* Invocation:                                                              */
/*    rec_alloc_xmem( size, pntr )                                          */

/* Description:                                                             */
/*    This function allocates memory for use as workspace. The memory is    */
/*    allocated using CNF facilities, so that it is exportable (e.g. can be */
/*    accessed from Fortran as well as C). This is necessary for memory     */
/*    pointers which will be returned to external code via the public       */
/*    interface.                                                            */

/* Parameters:                                                              */
/*    size_t size                                                           */
/*       The amount of memory required, in bytes.                           */
/*    void **pntr                                                           */
/*       Address of a pointer to the allocated workspace. A null pointer    */
/*       value is returned if an error occurs.                              */

/* Returned Value:                                                          */
/*    int rec_alloc_xmem                                                    */
/*       The global status value current on exit.                           */

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
/*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,       */
/*     MA 02110-1301, USA                                                   */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    16-FEB-1999 (RFWS):                                                   */
/*       Original version, adapted from rec_alloc_mem.                      */
/*    04-APR-2006 (TIMJ):                                                   */
/*       use size_t                                                         */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
#if defined( vms )               /* VMS version local variables:            */
      int npage;                 /* Number of pages required                */
      unsigned int base;         /* Base address of allocated pages         */
      unsigned int systat;       /* System status code                      */
#endif

/* External References:                                                     */
#if defined( vms )               /* VMS version system calls:               */
      unsigned int LIB$GET_VM_PAGE
         ( int *npage,
           unsigned int *base );
#endif

/*.                                                                         */

/* Set an initial null pointer value.                                       */
      *pntr = NULL;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* If this is a "big" memory request, then calculate the number of pages    */
/* required and allocate them from the global page pool.                    */
      if ( size >= REC__BIGMEM )
      {
	npage = 1 + ( (int)size - 1 ) / 512;
         systat = LIB$GET_VM_PAGE( &npage, &base );

/* If an error occurred, set the global status and report it.               */
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = DAT__NOMEM;
            emsSeti( "NBYTES", (int)size );
            emsSyser( "MESSAGE", systat );
            emsRep( "REC_ALLOC_XMEM_1",
                       "Unable to obtain a block of ^NBYTES bytes of memory - \
^MESSAGE",
                       &hds_gl_status );
         }

/* If OK, return a pointer to the allocated memory.                         */
         else
         {
            *pntr = (void *) base;
         }
      }

/* If this is a small memory request (or not running on VMS)...             */
      else
#endif

/* Allocate the required memory using cnfMalloc, so that it is exportable. */
      {
         *pntr = cnfMalloc( size );

/* If allocation failed, then report an error.                              */
         if ( *pntr == NULL )
         {
            hds_gl_status = DAT__NOMEM;
            emsSyser( "MESSAGE", errno );
            dat1emsSetBigu( "NBYTES", (UINT_BIG)size );
            emsRep( "REC_ALLOC_XMEM_2",
                       "Unable to obtain a block of ^NBYTES bytes of memory "
                       "- ^MESSAGE", &hds_gl_status );
         }
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
