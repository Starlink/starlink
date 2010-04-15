#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_release_block ( int slot, INT_BIG bloc )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_release_block                                                     */

/* Purpose:                                                                 */
/*    Release a Logical Record Block.                                       */

/* Invocation:                                                              */
/*    rec_release_bloc( slot, bloc )                                        */

/* Description:                                                             */
/*    This function decrements the reference count for a Logical Record     */
/*    Block.  It should be used in conjunction with rec_locate_block, with  */
/*    each call to rec_locate_block being matched by a corresponding call   */
/*    to rec_release_block. This indicates that the Logical Record Block is */
/*    no longer in use. The block may be removed from the Working Page List */
/*    (block cache) when its reference count reaches zero.                  */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       Slot number of the container file in the File Control Vector.      */
/*    int bloc                                                              */
/*       Number of the Logical Record Block being released (the first block */
/*       in the file is no. 1 ).                                            */

/* Notes:                                                                   */
/*    This routine attempts to execute even if the global HDS status is set */
/*    on entry, although no further error report will be made if it         */
/*    subsequently fails under these circumstances.                         */

/* Returned Value:                                                          */
/*    int rec_release_block                                                 */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    9-APR-1991 (RFWS):                                                    */
/*       Changed name, added prologue and error handling.                   */
/*    27-JUN-1991 (RFWS):                                                   */
/*       Removed use of ems_begin_c and ems_end_c. These are not needed     */
/*       because the routine cannot fail and they appeared to consume most  */
/*       of the execution time. Also removed use of unnecessary local       */
/*       variables.                                                         */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      struct BCP *bcp;           /* Pointer to Block Control Packet         */

/*.                                                                         */

/* Determine if the required BCP was the last one used.                     */
      bcp = rec_ga_lastbcp;
      if ( ( bcp->bid.slot != slot ) ||
           ( bcp->bid.bloc != bloc ) )
      {

/* If not, then search the Working Page List for the relevant Block Control */
/* Packet.                                                                  */
         bcp = rec_ga_wpl;
         while ( ( bcp->bid.slot != slot ) ||
                 ( bcp->bid.bloc != bloc ) )
         {
            bcp = bcp->flink;
         }
      }

/* Decrement the BCP reference count.                                       */
      bcp->count--;

/* Return the global status value.                                          */
      return hds_gl_status;
   }
