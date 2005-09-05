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

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
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
         ems_begin_c( &hds_gl_status );

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

/* Note that the rec_ facility is no longer active.                         */
         rec_gl_active = 0;

/* End the error reporting context.                                         */
         ems_end_c( &hds_gl_status );
      }

/* Exit the routine.                                                        */
      return;
}
