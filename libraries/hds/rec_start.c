#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void rec_start( void )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_start                                                             */

/* Purpose:                                                                 */
/*    Start up the rec_ facility.                                           */

/* Invocation:                                                              */
/*    rec_start( )                                                          */

/* Description:                                                             */
/*    This function starts up the rec_ facility and should be called prior  */
/*    to calling any other rec_ functions. It returns without action if the */
/*    rec_ facility is already active.                                      */

/* Parameters:                                                              */
/*    void                                                                  */

/* Returned Value:                                                          */
/*    void                                                                  */

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
/*    15-APR-1991 (RFWS):                                                   */
/*       Added prologue and error handling.                                 */
/*    15-APR-1991 (RFWS):                                                   */
/*       Changed to return without action if already active.                */
/*    26-AUG-1992 (RFWS);                                                   */
/*       Removed auto-increment on macro argument (dangerous).              */
/*    25-NOV-1992 (RFWS):                                                   */
/*       Changed to handle new expandable File Control Vector and to return */
/*       a void value.                                                      */
/*    31-JAN-2006 (TIMJ):                                                   */
/*        Store pointer to malloced BCP                                     */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Constants:                                                         */
      const INT mxslot0 = 64;    /* Initial number of FCV slots to allocate */

/* Local Variables:                                                         */
      INT i;
      struct BCP *bcp;

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return;

/* Check if the rec_ facility is already active. There is nothing to do if  */
/* it is.                                                                   */
      if ( !rec_gl_active )
      {

/* Allocate space for the File Control Vector. If successful, initialise    */
/* the space to zero and record the number of FCV slots allocated.          */
         rec_alloc_mem( mxslot0 * sizeof( struct FCV ),
                        (void **) &rec_ga_fcv );
         if ( _ok( hds_gl_status ) )
         {
            (void) memset( (void *) rec_ga_fcv, 0,
                           (size_t) mxslot0 * sizeof( struct FCV ) );
            rec_gl_mxslot = mxslot0;
         }

/* Initialise the Working Page and Free Page lists.                         */
         rec_ga_wpl = NULL;
         rec_ga_fpl = NULL;

/* Allocate space for the required number of Block Control Packets.         */
         rec_alloc_mem( hds_gl_maxwpl * sizeof( struct BCP ), (void **) &bcp );
         if ( _ok( hds_gl_status ) )
         {
            (void) memset( (void *) bcp, 0,
                           (size_t) hds_gl_maxwpl * sizeof( struct BCP ) );
	    rec_ga_fpl_malloced = bcp;

/* Fill the Free Page List.                                                 */
            for ( i = 0; i < hds_gl_maxwpl; i++ )
            {
               _insque( bcp, rec_ga_fpl );
               bcp++;
            }
         }

/* If successful, note that the rec_ facility is now active.                */
         if ( _ok( hds_gl_status ) )
         {
            rec_gl_active = 1;
         }

/* Otherwise, deallocate the memory.                                        */
         else
         {
            rec_deall_mem( mxslot0 * sizeof( struct FCV ),
                           (void **) &rec_ga_fcv );
            rec_deall_mem( hds_gl_maxwpl * sizeof( struct BCP ),
                           (void **) &bcp );
         }
      }

/* Exit the routine.                                                        */
      return;
   }
