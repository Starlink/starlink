#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int rec_delete_record( const struct HAN *han )
{
/*+                                                                         */
/* Name:                                                                    */
/*    rec_delete_record                                                     */

/* Purpose:                                                                 */
/*    Delete an existing record.                                            */

/* Invocation:                                                              */
/*    rec_delete_record( han )                                              */

/* Description:                                                             */
/*    This function deletes an existing record from a container file and    */
/*    makes its space available for re-use.                                 */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for the record to   */
/*       be deleted.                                                        */

/* Returned Value:                                                          */
/*    int rec_delete_record                                                 */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    22-APR-1991 (RFWS):                                                   */
/*       Added prologue and error handling and tidied.                      */
/*    14-JUNE-1991 (RFWS):                                                  */
/*       Changed access mode to 'U' in rec_locate_block.                    */
/*    27-JUN-2000  (BKM):                                                   */
/*       Revise for extended format (64-bit) HDS files.                     */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
   INT_BIG bloc;                 /* Block number of frame of PDBs           */
   int spare;                    /* No. blocks or chips to release          */
   struct RCL rcl;               /* Record Control Label                    */
   unsigned char *cbm;           /* Pointer to Chip Bitmap                  */
   unsigned char *cdom;          /* Pointer to Control Domain               */
   unsigned char *ddom;          /* Pointer to Dynamic Domain               */
   unsigned char *lrb;           /* Pointer to Logical Record Block         */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( hds_gl_status ) )
      return hds_gl_status;

/* Locate the Logical Record Block containing the record.                   */
   rec_locate_block( han->slot, han->rid.bloc, 'U', &lrb );

/* Find the Chip Bitmap.                                                    */
   cbm = lrb;

/* Find the record's Control Domain and unpack the Record Control Label.    */
   cdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP );
   rec1_unpack_rcl( cdom, &rcl );

/* If the Dynamic Domain is chained, then find it and unpack the block      */
/* number of the first block in the associated frame of Pure Data Blocks.   */
   if ( rcl.chain )
   {
      spare = _nblocs( rcl.dlen );
      ddom = cdom + ( rcl.extended ? REC__SZRCL : REC__SZORCL ) + rcl.slen;
      rec1_unpack_chain( ddom, rcl.extended, &bloc );

/* Release the PDBs.                                                        */
      if ( _ok( hds_gl_status ) )
      {
         rec1_deall_frame( han->slot, spare, bloc );
      }
   }

/* Clear the appropriate bits in the Chip Bitmap and update the free space  */
/* stack.                                                                   */
   spare = rcl.size;
   rec1_clear_cbm( cbm, spare, han->rid.chip );
   rec1_update_free( han->slot, han->rid.bloc, cbm );

/* Fill the reclaimed space in the Logical Record Block with zeros.         */
   if ( _ok( hds_gl_status ) )
   {
      (void) memset( (void *) cdom, 0, (size_t) ( spare * REC__SZCHIP ) );
   }

/* Release the Logical Record Block.                                        */
   if ( lrb != NULL )
      rec_release_block( han->slot, han->rid.bloc );

/* Return the current global status value.                                  */
   return hds_gl_status;
}
