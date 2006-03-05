#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_flush_block( struct BCP *bcp )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_flush_block                                                      */

/* Purpose:                                                                 */
/*    Flush the contents of a cached Logical Record Block.                  */

/* Invocation:                                                              */
/*    rec1_flush_block( bcp )                                               */

/* Description:                                                             */
/*    This function flushes the contents of a Logical Record Block which is */
/*    held in the Working Page List (block cache). The block is written     */
/*    back to its container file if it has been modified.                   */

/* Parameters:                                                              */
/*    struct BCP *bcp                                                       */
/*       Pointer to the Working Page List Block Control Packet for the      */
/*       block to be flushed. The modified block flag in this structure     */
/*       will be cleared if the block is successfully written back to its   */
/*       container file.                                                    */

/* Returned Value:                                                          */
/*    int rec1_flush_block                                                  */
/*       The global status value current on exit.                           */

/* Notes:                                                                   */
/*    This routine attempts to execute even if the HDS global status is set */
/*    on entry, although no further error report will be made if it         */
/*    subsequently fails under these circumstances.                         */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    9-APR-1991 (RFWS):                                                    */
/*       Changed name and added error handling and prologue.                */
/*    15-APR-1991 (RFWS):                                                   */
/*       Removed use of local variables. Changed to clear the modified      */
/*       block flag once the block has been successfully written back to    */
/*       the container file.                                                */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Check if the block has been modified. There is nothing to do if it has   */
/* not.                                                                     */
      if ( bcp->modify )
      {

/* Begin a new error reporting context.                                     */
         emsBegin( &hds_gl_status );

/* Write the block back to the container file and reset the modified block  */
/* flag.                                                                    */
         rec1_write_file( bcp->bid.slot, 1, bcp->bloc, bcp->bid.bloc );
         if ( _ok( hds_gl_status ) )
         {
            bcp->modify = 0;
         }

/* End the error reporting context.                                         */
         emsEnd( &hds_gl_status );
      }

/* Return the global status value.                                          */
      return hds_gl_status;
   }
