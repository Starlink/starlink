#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_release_data( const struct HAN *han, INT_BIG length,
                         INT_BIG offset, char mode, unsigned char **pntr )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_release_data                                                      */

/* Purpose:                                                                 */
/*    Release a specified region of the data stored in a record.            */

/* Invocation:                                                              */
/*    rec_release_data( han, length, offset, mode, pntr )                   */

/* Description:                                                             */
/*    This function releases a specified segment of the data associated     */
/*    with a record which has previously been located with rec_locate_data. */
/*    These two functions should always be called in pairs, with            */
/*    rec_release_data being used to cancel the access request implied by   */
/*    rec_locate_data.                                                      */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for the record.     */
/*    INT_BIG length                                                        */
/*       The length (in unsigned chars) of the data segment to be released. */
/*    INT_BIG offset                                                        */
/*       The (zero based) offset of the start of the data segment (in       */
/*       unsigned chars) from the start of the record's data region.        */
/*    char mode                                                             */
/*       The access mode originally used for accessing the data segment:    */
/*       'R' for read, 'U' for update, 'W' for write, or 'Z' for            */
/*       demand-zero.                                                       */
/*    unsigned char **pntr                                                  */
/*       Pointer to an unsigned char pointer which points at the start of   */
/*       the data segment in memory. A null pointer value will be returned. */

/* Returned Value:                                                          */
/*    int rec_release_data                                                  */
/*       The global status value current on exit.                           */

/* Notes:                                                                   */
/*    -  This routine attempts to execute even if the HDS global status is  */
/*    set on entry, although no further error report will be made if it     */
/*    subsequently fails under these circumstances.                         */
/*    -  This routine returns without action if the pntr argument points to */
/*    a null pointer on entry.                                              */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    15-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    16-APR-1991 (RFWS):                                                   */
/*       Improved prologue and error handling.                              */
/*    17-APR-1991 (RFWS):                                                   */
/*       Return a null pointer.                                             */
/*    13-JUN-1991 (RFWS):                                                   */
/*       Fixed bug in calculation of offset into a chained data domain.     */
/*    14-JUN-1991 (RFWS):                                                   */
/*       Added support for 'U' and 'Z' access modes.                        */
/*    18-JUN-1991 (RFWS):                                                   */
/*       Added deallocation of memory allocated by rec_locate_data to       */
/*       ensure conservative memory alignment.                              */
/*    16-FEB-1999 (RFWS):                                                   */
/*       Deallocate exportable memory.                                      */
/*    23-JUN-2000 (BKM):                                                    */
/*       Revise for extended format (64-bit) HDS files.                     */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      INT_BIG bloc;              /* Block containing start of mapped data   */
      int modify;                /* Data segment was modified?              */
      struct RCL rcl;            /* Record Control Label                    */
      unsigned char *cdom;       /* Pointer to Control Domain               */
      unsigned char *ddom;       /* Pointer to Dynamic Domain               */
      unsigned char *lrb;        /* Pointer to Logical Record Block         */

/*.                                                                         */

/* Check that the pointer supplied is not null. There is nothing to do if   */
/* it is.                                                                   */
      if ( *pntr != NULL )
      {

/* Begin a new error reporting context.                                     */
         emsBegin( &hds_gl_status );

/* See if the data segment was modified.                                    */
         modify = ( mode != 'R' );

/* Locate the block containing the record.                                  */
         rec_locate_block( han->slot, han->rid.bloc, ( modify ? 'U' : 'R' ),
                           &lrb );

/* Derive the location of the Control Domain and unpack the Record Control  */
/* Label.                                                                   */
         cdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP );
         rec1_unpack_rcl( cdom, &rcl );

/* Obtain a pointer to the dynamic domain.                                  */
         ddom = cdom + ( rcl.extended ? REC__SZRCL : REC__SZORCL ) + rcl.slen;

/* If the dynamic domain is not chained and the data segment was accessed   */
/* for modification, then copy the new data into the appropriate part of    */
/* the dynamic domain.                                                      */
         if ( !rcl.chain )
         {
            if ( modify )
            {
               memcpy( (void *) ( ddom + offset ), (void *) *pntr, length );
            }

/* Deallocate the associated exportable memory.                             */
            rec_deall_xmem( length, (void **) pntr );
         }

/* If the Dynamic Domain is chained, then unpack the starting block number  */
/* of the associated frame of Pure Data Blocks.                             */
         else
         {
            rec1_unpack_chain( ddom, rcl.extended, &bloc );

/* Derive the block number in which the mapped section of data begins and   */
/* unmap it.                                                                */
            if ( _ok( hds_gl_status ) )
            {
               bloc += ( offset / REC__SZBLK );
               offset = offset % REC__SZBLK;
               rec1_unmap_frame( han->slot, bloc, length, offset, mode, pntr );
            }
         }

/* If the data segment was originally accessed for modification, then clear */
/* the RCL modify flag and re-pack the modified Record Control Label.       */
         if ( modify )
         {
            rcl.modify = 0;
            rec1_pack_rcl( &rcl, cdom );
         }

/* Release the Logical Record Block.                                        */
         if ( lrb != NULL ) rec_release_block( han->slot, han->rid.bloc );

/* Return a null pointer.                                                   */
         *pntr = NULL;

/* End the error reporting context.                                         */
         emsEnd( &hds_gl_status );
      }

/* Return the global status value.                                          */
      return hds_gl_status;
   }
