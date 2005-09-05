#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_lock( const struct HAN *han )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_lock                                                              */

/* Purpose:                                                                 */
/*    Lock a container file.                                                */

/* Invocation:                                                              */
/*    rec_lock( han )                                                       */

/* Description:                                                             */
/*    This function locks a container file for exclusive write access.  It  */
/*    returns without action if the file is already locked.                 */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for the top level   */
/*       record in the container file.                                      */

/* Returned Value:                                                          */
/*    int rec_lock                                                          */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    22-APR-1991 (RFWS):                                                   */
/*       Added prologue and error handling and tidied.                      */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Lock the slot if it is not already locked.                               */
      if ( !rec_ga_fcv[ han->slot ].locked )
      {
         rec1_lock_slot( han->slot );
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
