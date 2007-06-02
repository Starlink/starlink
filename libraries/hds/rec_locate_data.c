#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_locate_data( const struct HAN *han, INT_BIG length, 
                        INT_BIG offset, char mode, unsigned char **pntr )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_locate_data                                                       */

/* Purpose:                                                                 */
/*    Locate a specified region of the data stored in a record.             */

/* Invocation:                                                              */
/*    rec_locate_data( han, length, offset, mode, pntr )                    */

/* Description:                                                             */
/*    This function returns an exportable pointer to a specified segment of */
/*    the data held in a record. The record's data will be brought into     */
/*    memory from the container file if necessary and held there until a    */
/*    corresponding call to rec_release_data (these two functions should    */
/*    always be used in matching pairs).                                    */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for the record      */
/*       whose data segment is required.                                    */
/*    INT_BIG length                                                        */
/*       Length (in unsigned chars) of the region of the records's data     */
/*       which is required.                                                 */
/*    INT_BIG offset                                                        */
/*       The (zero based) offset (in unsigned chars) of the first element   */
/*       of the record's data which is required. The returned pointer will  */
/*       point at this element.                                             */
/*    char mode                                                             */
/*       A symbol indicating the required mode of access to the data        */
/*       segment: 'R' for read, 'U' for update, 'W' for write, or 'Z' for   */
/*       demand-zero.                                                       */
/*    unsigned char **pntr                                                  */
/*       Pointer to an unsigned char pointer which will be set to point at  */
/*       the first requested data element. The data will be conservatively  */
/*       aligned in memory. A null pointer value will be returned under     */
/*       error conditions.                                                  */

/* Returned Value:                                                          */
/*    int rec_locate_data                                                   */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    11-APR-1991 (RFWS):                                                   */
/*       Added prologue and error handling, changed name and made portable. */
/*    3-MAY-1991 (RFWS):                                                    */
/*       Changed to remove the assumption of page-aligned data by calling   */
/*       a new version of rec1_map_frame.                                   */
/*    13-JUNE-1991 (RFWS):                                                  */
/*       Corrected bug in calculation of offset into a chained data domain. */
/*    14-JUNE-1991 (RFWS):                                                  */
/*       Added support for 'U' and 'Z' access modes.                        */
/*    18-JUN-1991 (RFWS):                                                   */
/*       Added memory allocation to ensure that non-chained data are        */
/*       returned conservatively aligned in memory.                         */
/*    22-JUN-1995 (RFWS):                                                   */
/*       Improved error message when record length is exceeded.             */
/*    16-FEB-1999 (RFWS):                                                   */
/*       Allocate exportable memory.                                        */
/*    20-JUN-2000 (BKM):                                                    */
/*       Revise for extended format (64-bit) HDS files.                     */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      INT_BIG bloc;              /* First block in frame to be mapped       */
      int modify;                /* Data will be modified?                  */
      struct RCL rcl;            /* Record Control Label                    */
      unsigned char *cdom;       /* Pointer to Control Domain               */
      unsigned char *ddom;       /* Pointer to Dynamic Domain               */
      unsigned char *lrb;        /* Pointer to Logical Record Block         */

/*.                                                                         */

/* Set an initial null value for the returned pointer.                      */
      *pntr = NULL;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* See if the data segment will be modified.                                */
      modify = ( mode != 'R' );

/* Locate the Logical Record Block containing the record.                   */
      rec_locate_block( han->slot, han->rid.bloc, ( modify ? 'U' : 'R' ),
                        &lrb );

/* Derive a pointer to the record's Control Domain and unpack the Record    */
/* Control Label.                                                           */
      cdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP );
      rec1_unpack_rcl( cdom, &rcl );
      if ( _ok( hds_gl_status ) )
      {

/* Check that the requested data segment does not extend beyond the size of */
/* the record's Dynamic Domain. Report an error if it does.                 */
	if ( (UINT_BIG)(offset + length) > rcl.dlen )
         {
            hds_gl_status = DAT__INCHK;
            dat1emsSetBigu( "DLEN", rcl.dlen );
            rec1_fmsg( "FILE", han->slot );
            emsRep( "REC_LOCATE_DATA_1",
                       "Requested data extends beyond the end of the record; \
record length is ^DLEN bytes (possible corrupt HDS container file ^FILE).",
                       &hds_gl_status );
         }
      }

/* If the record's data are to be modified, then ensure that there is no    */
/* other current access which would also cause modification. Report an      */
/* error if there is.                                                       */
      if ( _ok( hds_gl_status ) )
      {
         if ( modify && rcl.modify )
         {
            hds_gl_status = DAT__ACCON;
            emsRep( "REC_LOCATE_DATA_2",
                       "Attempt to modify an object which is already being \
modified (possible programming error).",
                       &hds_gl_status );
         }
      }

/* Set the Record Control Lable active and modify flags if the data are to  */
/* be written to.                                                           */
      if ( _ok( hds_gl_status ) )
      {
         rcl.modify = ( rcl.modify || modify );
         rcl.active = ( rcl.active || modify );

/* Obtain a pointer to the record's Dynamic Domain.                         */
         ddom = cdom + (rcl.extended ? REC__SZRCL : REC__SZORCL) + rcl.slen;

/* If the Dynamic Domain is not chained, then allocate memory to hold the   */
/* data. Note that this is done (rather than return a pointer to the        */
/* dynamic domain directly) to ensure that the data are conservatively      */
/* aligned in memory. We allocate exportable memory, since the pointer may  */
/* be returned via the public interface.                                    */
         if ( !rcl.chain )
         {
	   rec_alloc_xmem( (size_t)length, (void **) pntr );
            if ( _ok( hds_gl_status ) )
            {

/* If the access mode is "demand zero", then fill the memory with zeros.    */
               if ( mode == 'Z' )
               {
                  memset( (void *) *pntr, 0, length );
               }

/* Otherwise, if the the access mode is not write, copy the required data   */
/* segment into the allocated memory.                                       */
               else if ( mode != 'W' )
               {
                  memcpy( (void *) *pntr, (void *) ( ddom + offset ), length );
               }
            }
         }

/* If the Dynamic Domain is chained (i.e. the record's data are contained   */
/* in an associated frame of Pure Data Blocks), then the starting block     */
/* number will be held in the Dynamic Domain. Extract this block number.    */
         else
         {
            rec1_unpack_chain( ddom, rcl.extended, &bloc );

/* Determine the number of the block in which the start of the required     */
/* data segment resides and derive the offset into this block.              */
            bloc += ( offset / REC__SZBLK );
            offset = offset % REC__SZBLK;

/* Map the required frame of Pure Data Blocks.                              */
            rec1_map_frame( han->slot, bloc, length, offset, mode, pntr );
         }
      }

/* If the record's data are being modified, then pack the (possibly         */
/* modified) Record Control Label back into the Control Domain.             */
      if ( modify ) rec1_pack_rcl( &rcl, cdom );
      
/* Release the Logical Record Block.                                        */
      if ( lrb != NULL ) rec_release_block( han->slot, han->rid.bloc );

/* If an error occurred, then return a null pointer value.                  */
      if ( !_ok( hds_gl_status ) ) *pntr = NULL;

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
