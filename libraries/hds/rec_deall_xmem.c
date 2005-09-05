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

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "f77.h"                 /* Fortran <--> C interface facilities     */

   int rec_deall_xmem( int size, void **pntr )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_deall_xmem                                                        */

/* Purpose:                                                                 */
/*    Deallocate exportable memory.                                         */

/* Invocation:                                                              */
/*    rec_deall_xmem( size, pntr )                                          */

/* Description:                                                             */
/*    This function deallocates exportable memory previously allocated by   */
/*    rec_alloc_xmem.                                                       */

/* Parameters:                                                              */
/*    int size                                                              */
/*       The amount of memory allocated in bytes.                           */
/*    void **pntr                                                           */
/*       Address of a pointer to the allocated memory. A null pointer is    */
/*       returned.                                                          */

/* Returned Value:                                                          */
/*    int rec_deall_xmem                                                    */
/*       The global status value current on exit.                           */

/* Notes:                                                                   */
/*    -  This routine attempts to execute even if the HDS global status is  */
/*    set on entry.                                                         */
/*    -  Care should be taken that the pointer supplied is either valid or  */
/*    null (in the latter case the routine has no effect), since an invalid */
/*    pointer value cannot be detected.                                     */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    16-FEB-1999 (RFWS):                                                   */
/*       Original version, adapted from rec_deall_mem.                      */
/*    {@enter_changes_here@}                                                */

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
         npage = 1 + ( size - 1 ) / 512;
         base = (unsigned int) *pntr;
         systat = LIB$FREE_VM_PAGE( &npage, &base );

/* If an error occurred, set the global status and report it. Do this       */
/* inside a new error reporting environment.                                */
         if ( !( systat & STS$M_SUCCESS ) )
         {
            ems_begin_c( &hds_gl_status );
            hds_gl_status = DAT__NOMEM;
            ems_seti_c( "NBYTES", size );
            ems_syser_c( "MESSAGE", systat );
            ems_rep_c( "REC_DEALL_XMEM_1",
                       "Unable to release a block of ^NBYTES bytes of memory "
                       "- ^MESSAGE", &hds_gl_status );
            ems_end_c( &hds_gl_status );
         }
      }

/* If this was a small memory request (or not running on VMS)...            */
      else
#endif

/* Release the memory previously allocated by cnf_malloc.                   */
      {
         cnf_free( *pntr );
      }

/* Return a null pointer.                                                   */
      *pntr = NULL;

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
