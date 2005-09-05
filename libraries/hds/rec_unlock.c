#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_unlock( const struct HAN *han )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_unlock                                                            */

/* Purpose:                                                                 */
/*    Unlock a container file.                                              */

/* Invocation:                                                              */
/*    rec_unlock( han )                                                     */

/* Description:                                                             */
/*    This function unlocks a container file which has previously been      */
/*    locked with rec_lock, thereby allowing other processes to write to    */
/*    the file.  It returns without action if the file is not locked.       */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle to the top level    */
/*       record in the container file.                                      */

/* Returned Value:                                                          */
/*    int rec_unlock                                                        */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* Notes:                                                                   */
/*    This routine attempts to execute even if the global HDS status is set */
/*    on entry, although no further error report will be made if it         */
/*    subsequently fails under these circumstances.                         */

/* History:                                                                 */
/*    22-APR-1991 (RFWS):                                                   */
/*       Added prologue and error reporting and tidied.                     */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Begin a new error reporting context.                                     */
      ems_begin_c( &hds_gl_status );

/* Unlock the container file's slot in the File Control Vector.             */
      rec1_unlock_slot( han->slot );

/* End the error reporting context and return the current global status     */
/* value.                                                                   */
      ems_end_c( &hds_gl_status );
      return hds_gl_status;
   }
