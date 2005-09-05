#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_where( const struct HAN *han, INT_BIG length, 
                  INT_BIG offset, INT_BIG *bloc, int *bytoff )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_where                                                             */

/* Purpose:                                                                 */
/*    Determine where a record is stored in a container file.               */

/* Invocation:                                                              */
/*    rec_where( han, length, offset, bloc, bytoff )                        */

/* Description:                                                             */
/*    This function determines where in a container file a record's dynamic */
/*    domain is stored and returns the block number and byte offset within  */
/*    that block of a specified offset within the domain.                   */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for the record.     */
/*    INT_BIG length                                                        */
/*       Length (in bytes) of the required data segment within the record's */
/*       dynamic domain. The required data must lie entirely within the     */
/*       domain.                                                            */
/*    INT_BIG offset                                                        */
/*       The (zero based) byte offset into the dynamic domain.              */
/*    INT_BIG *bloc                                                         */
/*       Pointer to an 64-bit integer to receive the container file block   */
/*       number in which the requested data segment begins (the first block */
/*       in the file is no. 1 ).                                            */
/*    int *bytoff                                                           */
/*       Pointer to an integer to receive the (zero based) byte offset      */
/*       within the block at which the requested data segment begins.       */

/* Returned Value:                                                          */
/*    int rec_where                                                         */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    3-APR-1991 (RFWS):                                                    */
/*       Added error handling and prologue.                                 */
/*    9-APR-1991 (RFWS):                                                    */
/*       Improved the error handling.                                       */
/*    16-APR-1991 (RFWS):                                                   */
/*       Further improved the error handling, added call to                 */
/*       rec_unpack_chain and eliminated unnecessary local variables.       */
/*    22-JUN-1995 (RFWS):                                                   */
/*       Improved error message when record length is exceeded.             */
/*    27-JUN-2000 (BKM):                                                    */
/*        Revise for 64-bit extended HDS files.                             */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      struct RCL rcl;            /* Record Control Label                    */
      unsigned char *cdom;       /* Pointer to Control Domain               */
      unsigned char *ddom;       /* Pointer to Dynamic Domain               */
      unsigned char *lrb;        /* Pointer to Logical Record Block         */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;
               
/* Find the Logical Record Block containing the record.                     */
      rec_locate_block( han->slot, han->rid.bloc, 'R', &lrb );

/* Determine the location of the record's Control Domain and unpack it to   */
/* obtain the Record Control Label.                                         */
      cdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP );
      rec1_unpack_rcl( cdom, &rcl );
      if ( _ok( hds_gl_status ) )
      {

/* Report an error if the Dynamic Domain bounds have been exceeded.         */
         if ( offset + length > rcl.dlen )
         {
            hds_gl_status = DAT__INCHK;
            ems_seti_c( "DLEN", rcl.dlen );
            rec1_fmsg( "FILE", han->slot );
            ems_rep_c( "REC_WHERE_1",
                       "Requested data extends beyond the end of the record; \
record length is ^DLEN bytes (possible corrupt HDS container file ^FILE).",
                       &hds_gl_status );
         }
      }

/* Determine the location of the Dynamic Domain.                            */
      if ( _ok( hds_gl_status ) )
      {
         ddom = cdom + ( rcl.extended ? REC__SZRCL : REC__SZORCL ) + rcl.slen;

/* If the record's data are chained (i.e. contained in a frame of Pure Data */
/* Blocks), then the block number of the start of the frame is stored in    */
/* the Dynamic Domain, so unpack it.                                        */
         if ( rcl.chain )
         {
            rec1_unpack_chain( ddom, rcl.extended, bloc );

/* Adjust the block number to account for the requested offset into the     */
/* data and derive the byte offset into the block.                          */
            *bloc += ( offset / REC__SZBLK );
            *bytoff = offset % REC__SZBLK;
         }

/* If the record's data are not chained, then the block number is that of   */
/* the LRB and the offset can be calculated from the pointer to the start   */
/* of the block.                                                            */
         else
         {
            *bloc = han->rid.bloc;
            *bytoff = ( ddom - lrb ) + offset;
         }
      }

/* Drop the Logical Record Block.                                           */
      if ( lrb != NULL ) rec_release_block( han->slot, han->rid.bloc );

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
