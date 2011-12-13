#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void rec_stop( void )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_stop                                                              */

/* Purpose:                                                                 */
/*    Close down the rec_ facility.                                         */

/* Invocation:                                                              */
/*    rec_stop( )                                                           */

/* Description:                                                             */
/*    This function ensures that the rec_ facility is closed down. All      */
/*    cached data are written back to disk and associated resources are     */
/*    released. It returns without action if the rec_ facility is already   */
/*    inactive.                                                             */

/* Parameters:                                                              */
/*    void                                                                  */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    This routine attempts to execute even if the global HDS status is set */
/*    on entry, although no further error report will be made if it         */
/*    subsequently fails under these circumstances.                         */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
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
/*    22-APR-1991 (RFWS):                                                   */
/*       Added prologue and error handling and tidied.                      */
/*    12-JUN-1991 (RFWS):                                                   */
/*       Removed unnecessary flushing of the Working Page List - cached     */
/*       blocks are flushed when the File Control Vector slots are closed.  */
/*    30-OCT-1992 (RFWS):                                                   */
/*       Added deallocation of remaining wild-card file search contexts.    */
/*    25-NOV-1992 (RFWS):                                                   */
/*       Changed to deallocate the File Control Vector and to return a void */
/*       value.                                                             */
/*    31-JAN-2006 (TIMJ):                                                   */
/*        Free memory associated with the Block Control Packets             */
/*    01-FEB-2006 (TIMJ):                                                   */
/*        Free memory allocated by rec1_getcwd                              */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      INT slot;                  /* Loop counter for File Control Vector    */
      struct WLD *context;       /* Pointer to wild-card search context     */

/*.                                                                         */

/* Check that the rec_ facility is active. There is nothing to do if it is  */
/* not.                                                                     */
      if ( rec_gl_active )
      {

/* Begin a new error reporting context.                                     */
         emsBegin( &hds_gl_status );

/* Shut down all the open slots in the File Control Vector. Then deallocate */
/* the FCV itself.                                                          */
         for ( slot = 0; slot < rec_gl_endslot; slot++ )
         {
            rec1_close_slot( slot );
         }
         rec_deall_mem( rec_gl_mxslot * sizeof( struct FCV ),
                        (void **) &rec_ga_fcv );

/* Deallocate any remaining wild-card file search contexts.                 */
         while ( rec_gl_wldque != NULL )
         {
            context = rec_gl_wldque;
            rec_end_wild( &context );
         }

	 /* Deallocate the memory associated with the free page list */
	 /* The malloced memory area may not even be reachable in the
	    linked list so we obtain the pointer from the global that exists
	    for exactly this purpose */
	 if (rec_ga_fpl_malloced != NULL)
	   rec_deall_mem( hds_gl_maxwpl * sizeof( struct BCP ),
			  (void **)&rec_ga_fpl_malloced );

	 /* Free the getcwd buffer */
	 rec1_getcwd_free( );

/* Note that the rec_ facility is no longer active.                         */
         rec_gl_active = 0;

/* End the error reporting context.                                         */
         emsEnd( &hds_gl_status );
      }

/* Exit the routine.                                                        */
      return;
}
