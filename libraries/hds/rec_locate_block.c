#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include <string.h>
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_locate_block( int slot, int bloc, char mode, unsigned char **lrb )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_locate_block                                                      */

/* Purpose:                                                                 */
/*    Locate a specified Logical Record Block.                              */

/* Invocation:                                                              */
/*    rec_locate_block( slot, bloc, mode, lrb )                             */

/* Description:                                                             */
/*    This function locates a specified Logical Record Block in a container */
/*    file and returns a pointer to it. The block will be read from the     */
/*    file if necessary, although often it will be present in the Working   */
/*    Page List (block cache).                                              */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       Slot number of the container file in the File Control Vector.      */
/*    int bloc                                                              */
/*       Number of the Logical Record Block required (the first block in    */
/*       the file is no. 1).                                                */
/*    char mode                                                             */
/*       Symbol indicating the access mode required for the block: 'R' for  */
/*       read, 'U' or update, 'W' for write and 'Z' for demand-zero.        */
/*    unsigned char **lrb                                                   */
/*       Pointer to an unsigned char pointer which will be set to point at  */
/*       the first byte of the Logical Record Block. A null pointer will be */
/*       returned under error conditions.                                   */

/* Returned Value:                                                          */
/*    int rec_locate_block                                                  */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    9-APR-1991 (RFWS):                                                    */
/*       Added prologue.                                                    */
/*    17-APR-1991 (RFWS):                                                   */
/*       Removed use of a return statement in the middle of the routine and */
/*       improved the error handling.                                       */
/*    14-JUN-1991 (RFWS):                                                   */
/*       Added support for 'W' access mode.                                 */
/*    19-JUN-1991 (RFWS):                                                   */
/*       Fixed bug causing "demand zero" initialisation to be omitted if    */
/*       the block is already present on the Working Page List.             */
/*    1-JUL-1991 (RFWS):                                                    */
/*       Removed use of unnecessary local variables.                        */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int hit;                   /* Block Control Packet found?             */
      int i;                     /* Loop counter for Working Page List      */
      int write;                 /* Block will be written to?               */
      struct BCP *bcp;           /* Pointer to Block Control Packet         */

/*.                                                                         */

/* Assign an initial null value to the returned pointer.                    */
      *lrb = NULL;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Initialise.                                                              */
      hit = 0;

/* Set the write flag if appropriate and lock the slot if not done already. */
      write = ( mode != 'R' );
      if ( write && ( !rec_ga_fcv[ slot ].locked ) )
      {
         rec1_lock_slot( slot );
      }

/* Check if there are any entries in the Working Page List.                 */
      if ( _ok ( hds_gl_status ) )
      {
         if ( rec_gl_wplsize > 0 )
         {

/* If so, then first determine if the last BCP accessed is the one          */
/* required.                                                                */
            bcp = rec_ga_lastbcp;
            hit = ( ( bcp->bid.slot == slot ) &&
                    ( bcp->bid.bloc == bloc ) );

/* If not, then search the Working Page List for the relevant BCP.          */
            if ( !hit )
            {
               bcp = rec_ga_wpl;
               for ( i = 0; i < rec_gl_wplsize; i++ )
               {
                  hit = ( ( bcp->bid.slot == slot ) &&
                          ( bcp->bid.bloc == bloc ) );
                  if ( hit )
                  {
                     break;
                  }
                  else
                  {
                     bcp = bcp->flink;
                  }
               }
            }

/* If the Block Control Packet was found on the Working Page List, then     */
/* return the pointer to the Logical Record Block, set the modified flag if */
/* appropriate, increment the reference count and adjust the current BCP    */
/* position.                                                                */
            if ( hit )
            {
               *lrb = bcp->bloc;
               bcp->modify = ( bcp->modify || write );
               bcp->count++;
               rec_ga_lastbcp = bcp;

/* If the access mode is "demand zero", then fill the block with zeros.     */
               if ( mode == 'Z' ) memset( (void *) *lrb, 0, REC__SZBLK );
            }
         }
      }

/* If the BCP was not on the Working Page List, then attempt to allocate a  */
/* BCP from the Free Page List.                                             */
      if ( !hit && _ok( hds_gl_status ) )
      {
         bcp = rec_ga_fpl;
         if ( bcp != NULL )
         {
            _remque( bcp, rec_ga_fpl );

/* If successful, allocate memory to hold the block and increment the       */
/* current Working Page List size.                                          */
            rec_alloc_mem( REC__SZBLK, (void **) &bcp->bloc );
            rec_gl_wplsize++;
         }                      

/* If the Free Page List is empty, then a BCP from the Working Page List    */
/* must be re-used.                                                         */
         else
         {

/* Scan backwards through the list until a BCP with a zero reference count  */
/* is found.                                                                */
            bcp = rec_ga_wpl->blink;
            while ( bcp->count > 0 )
            {
               bcp = bcp->blink;
            }

/* Flush the old block and, if successful, remove it from its present       */
/* position in the Working Page List.                                       */
            rec1_flush_block( bcp );
            if ( _ok( hds_gl_status ) )
            {
               _remque( bcp, rec_ga_wpl );
            }
         }

/* Insert the new BCP at the head of the Working Page List and update the   */
/* current BCP position.                                                    */
         if ( _ok( hds_gl_status ) )
         {
            _insque( bcp, rec_ga_wpl );
            bcp->bid.bloc = bloc;
            bcp->bid.slot = slot;
            bcp->modify = write;
            bcp->count = 1;
            rec_ga_lastbcp = bcp;

/* Return a pointer to the block.                                           */
            *lrb = bcp->bloc;

/* If the access mode is demand-zero, then zero the block.                  */
            if ( mode == 'Z' )
            {
               memset( (void *) *lrb, 0, REC__SZBLK );
            }

/* Otherwise, if the access mode is not write, then read the block from the */
/* container file.                                                          */
            else if ( mode != 'W' )
            {
               rec1_read_file( slot, bloc, 1, *lrb );
            }
         }
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
