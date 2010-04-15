#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int rec_create_record( const struct HAN *par, struct RCL *rcl,
                       struct HAN *han )
{
/*+                                                                         */
/* Name:                                                                    */
/*    rec_create_record                                                     */

/* Purpose:                                                                 */
/*    Create a new record.                                                  */

/* Invocation:                                                              */
/*    rec_create_record( par, rcl, han )                                    */

/* Description:                                                             */
/*    This routine creates a new record in the context of a parent record.  */
/*    An attempt is first made to allocate space in the Logical Record      */
/*    Block which contains the parent record (to reduce the overheads in    */
/*    subsequent access). If this fails, then the free space stack is       */
/*    scanned for an LRB with the appropriate sequence of free chips.  If   */
/*    none are found, then a virgin LRB is allocated from the free space    */
/*    stack.                                                                */

/* Parameters:                                                              */
/*    const struct HAN *par                                                 */
/*       Pointer to a handle for the parent record.                         */
/*    struct RCL *rcl                                                       */
/*       Pointer to a record control label which describes the record to be */
/*       created.                                                           */
/*    struct HAN *han                                                       */
/*       Pointer to a handle which will be filled in to identify the new    */
/*       record.                                                            */

/* Returned Value:                                                          */
/*    int rec_create_record                                                 */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    27-MAR-1991 (RFWS):                                                   */
/*       Added prologue.                                                    */
/*    10-APR-1991 (RFWS):                                                   */
/*       Added call to rec1_pack_rcl to pack the Record Control Label       */
/*       information into the record's Control Domain.                      */
/*    16-APR-1991 (RFWS):                                                   */
/*       Improved the error handling.                                       */
/*    17-APR-1991 (RFWS):                                                   */
/*       Further improved the error handling and eliminated unnecessary     */
/*       local variables.                                                   */
/*    11-JUN-1991 (RFWS):                                                   */
/*       Removed reference to RCL .reserve field.                           */
/*    14-JUNE-1991 (RFWS):                                                  */
/*       Changed access mode to 'U' in rec_locate_block.                    */
/*    19-APR-2004 (BKM):                                                  */
/*       Revise for 64-bit HDS files.                                       */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
   INT_BIG bloc;                 /* Block number of record                  */
   INT_BIG pdb;                  /* Block number of frame of PDBs           */
   int chip;                     /* Chip number of record                   */
   int i;                        /* Loop counter for free space stack       */
   int size;                     /* Record size in chips                    */
   struct HCB *hcb;              /* Pointer to Header Control Block         */
   struct STK *stk;              /* Pointer to free space stack             */
   unsigned char *cbm=NULL;      /* Pointer to LRB chip bitmap              */
   unsigned char *cdom;          /* Pointer to Control Domain               */
   unsigned char *ddom;          /* Pointer to Dynamic Domain               */
   unsigned char *lrb;           /* Pointer to Logical Record Block         */
   unsigned char *pntr;          /* Pointer to mapped frame of PDBs         */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( hds_gl_status ) )
      return hds_gl_status;

/* Locate the Header Control Block.                                         */
   rec1_locate_hcb( par->slot, 'R', &hcb );
   if ( _ok( hds_gl_status ) )
   {

/* Locate the free space stack.                                             */
      stk = hcb->stk;

/* Set the entended format flag                                             */
      if( hds_gl_64bit )
            rcl->extended = 1;
      else
            rcl->extended = 0;

/* Calculate the number of chips required for the new record and determine  */
/* if its dynamic domain needs chaining. (If it does, then it becomes a     */
/* pointer to a frame of Pure Data Blocks).                                 */
      size = _nchips( rcl->slen + _max( rcl->dlen, SZCHAIN(rcl->extended) ) );
      rcl->chain = ( size > REC__MXCHIP );
      if ( rcl->chain )
      {
         size = _nchips( rcl->slen + SZCHAIN(rcl->extended) );
      }
      rcl->size = size;

/* Save the parent record ID and ensure that the active and modify flags    */
/* are clear.                                                               */
      rcl->parent = par->rid;
      rcl->active = 0;
      rcl->modify = 0;

/* Locate the parent Logical Record Block, if it exists.                    */
      chip = 0;
      bloc = 0;
      if ( ( par->rid.bloc != 0 ) || ( par->rid.chip != 0 ) )
      {
         rec_locate_block( par->slot, par->rid.bloc, 'U', &lrb );

/* See if there is enough space in the parent LRB to create the new record. */
/* If not, then release the block.                                          */
         if ( _ok( hds_gl_status ) )
         {
            cbm = lrb;
            if ( rec1_scan_cbm( cbm, size, &chip ) )
            {
               bloc = par->rid.bloc;
            }
            else
            {
               rec_release_block( par->slot, par->rid.bloc );
            }
         }
      }

/* If there is insufficient room in the parent LRB, then search the free    */
/* space stack for an LRB with enough spare chips to accommodate the new    */
/* record.                                                                  */
      if ( _ok( hds_gl_status ) )
      {
         if ( bloc == 0 )
         {
            for ( i = 0; ; i++ )
            {
               if ( stk[ i ].spare == -1 )
               {
                  break;
               }

/* If a suitable block is found, locate it, and determine the chip number   */
/* at which the record should start.                                        */
               else if ( size <= stk[ i ].spare )
               {
                  bloc = stk[ i ].bloc;
                  rec_locate_block( par->slot, bloc, 'U', &lrb );
                  cbm  = lrb;
                  rec1_scan_cbm( cbm, size, &chip );
                  break;
               }
            }
         }
      }

/* If no space of the required size is available, then allocate a new       */
/* Logical Record Block and locate it, filling it with zeros.               */
      if ( _ok( hds_gl_status ) )
      {
         if ( bloc == 0 )
         {
            rec1_alloc_frame( par->slot, 1, &bloc );
            rec_locate_block( par->slot, bloc, 'Z', &lrb );
            cbm = lrb;
         }
      }

/* Set the appropriate bits in the Chip Bitmap and update the free space    */
/* stack.                                                                   */
      if ( _ok( hds_gl_status ) )
      {
         rec1_set_cbm( cbm, size, chip );
         rec1_update_free( par->slot, bloc, cbm );
      }

/* Save the File Control Vector slot number and the new record ID in the    */
/* returned handle.                                                         */
      if ( _ok( hds_gl_status ) )
      {
         han->slot = par->slot;
         han->rid.bloc = bloc;
         han->rid.chip = chip;
         han->read = 0;

/* Determine the location of the new record's Control Domain and pack the   */
/* Record Control Label into it.                                            */
         cdom = lrb + REC__SZCBM + ( chip * REC__SZCHIP );
         rec1_pack_rcl( rcl, cdom );

/* If the dynamic domain needs chaining, then allocate an associated frame  */
/* of Pure Data Blocks.                                                     */
         if ( rcl->chain )
         {
            size = _nblocs( rcl->dlen );
            rec1_alloc_frame( par->slot, size, &pdb );

/* Clear the frame if the Dynamic Domain is demand-zero.                    */
            if ( rcl->zero )
            {
               rec1_map_frame( par->slot, pdb, size * REC__SZBLK, 0, 'Z',
                               &pntr );
               rec1_unmap_frame( par->slot, pdb, size * REC__SZBLK, 0, 'Z',
                                 &pntr );
            }

/* Pack the block number of the frame into the record's Dynamic Domain.     */
            ddom = cdom + SZRCL + rcl->slen;
            rec1_pack_chain( pdb, ddom );
         }
      }

/* Release the Logical Record Block containing the new record.              */
      if ( lrb != NULL ) rec_release_block( par->slot, bloc );
   }

/* Return the current global status value.                                  */
   return hds_gl_status;
}
