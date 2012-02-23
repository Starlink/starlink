#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */
#include "rec1.h"

   int rec1_unpack_rcl( const unsigned char prcl[], struct RCL *rcl )
   {
/*+									    */
/* Name:								    */
/*    rec1_unpack_rcl							    */

/* Purpose:								    */
/*    Unpack Record Control Label information.				    */

/* Invocation:								    */
/*    rec1_unpack_rcl( prcl, rcl )					    */

/* Description:								    */
/*    This function unpacks the information in a Record Control Label and   */
/*    puts it into an RCL structure.  This is done so that the Record	    */
/*    Control Lable format need not depend on the details of the way that   */
/*    an RCL structure is stored in memory.				    */

/* Parameters:								    */
/*    const unsigned char prcl[ REC__SZRCL  or REC__SZORCL]	     	    */
/*	 Pointer to an array of packed Record Control Label characters      */
/*	 containing the information to be unpacked.			    */
/*    struct RCL *rcl							    */
/*	 Pointer to an RCL structure to receive the unpacked information.   */

/* Returned Value:							    */
/*    int rec1_unpack_rcl						    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    12-FEB-1991 (RFWS):						    */
/*	 Original version.						    */
/*    8-APR-1991 (RFWS):						    */
/*       Changed to make the parent field of type struct RID.		    */
/*    11-JUN-1991 (RFWS):						    */
/*	 Removed unpacking of the RCL reserve field (no longer present in   */
/*	 the internal representation of the data structure).		    */
/*    21-JUN-2000 (BKM):                                                    */
/*       Revise to cope with both HDS Version 3 and the 64-bit HDS Version  */
/*       4 RCL structures.                                           */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local variables:                                                         */
      INT_BIG  temp;      /* Temporary variable.                            */
      int shift;          /* Bit-shift value                                */
      int byte;           /* Current byte in packed record                  */
      int i;              /* Loop variable                                  */
/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* First unpack the RCL elements common to the two current formats. These   */
/* include the flag-bit to allow correct further decoding.                  */

/* Extract the RCL active, zero, modify and extended-fromat fields from     */
/* the third packed byte.                                                   */
      rcl->active = prcl[ 3 ] & 0x1;
      rcl->zero =     ( prcl[ 3 ] >> 1 ) & 0x1;
      rcl->modify =   ( prcl[ 3 ] >> 2 ) & 0x1;
      rcl->extended = ( prcl[ 3 ] >> 3 ) & 0x1;

/* Extract the RCL size field (4 bits), the class field (3 bits) and the    */
/* chain field (1 bit) from the next char.				    */
      rcl->size =  (prcl[ 4 ] & 0xf);
      rcl->class = ( prcl[ 4 ] >> 4 ) & 0x7;
      rcl->chain = ( prcl[ 4 ] >> 7 ) & 0x1;

/* Obtain the RCL slen field from the next char.			    */
      rcl->slen = prcl [ 5 ];

/* Test which HDS file format (3 or 4) we have.                             */
      if( !rcl->extended )
      {

/* Unpack the RCL parent.bloc field from the first 20 bits, and the RCL	    */
/* parent.chip field from the following 4 bits.				    */
         rcl->parent.bloc = ( ( ( ( prcl[ 2 ] & 0xf ) << 8 ) |
				    prcl[ 1 ] ) << 8 ) |
				 prcl[ 0 ];
         rcl->parent.chip =  ( prcl[ 2 ] >> 4 ) & 0xf;


/* Unpack the dlen field from the final 4 char.				    */
         rcl->dlen = ( ( ( ( ( ( prcl[ 9 ] ) << 8 ) |
                                 prcl[ 8 ] ) << 8 ) |
                                 prcl[ 7 ] ) << 8 ) |
			         prcl[ 6 ];
      }
      else      /* Extended (64-bit addressing) HDS (File format 4)         */
      {
/* Unpack the first two bytes of the Parent RID bloc field and the RID chip */
/* field from the first 3 bytes.                                            */
         rcl->parent.bloc = ( prcl[ 1 ] << 8 ) |
                              prcl[ 0 ];
         rcl->parent.chip = prcl[ 2 ] & 0xf;

/* Extract the 64-bit value for rcl->dlen                                   */
         byte = 6;
         shift = 0;
         rcl->dlen = 0;
         for( i = 0; i < 8; i++ )
         {
            temp = prcl[ byte ++ ];
            rcl->dlen |= ( temp << shift );
            shift += 8;
         }
/* And the remaining 5 bytes for the parent RID bloc field.                 */
         shift = 16;
         for( i = 0; i <5; i++ )
         {
            temp = prcl[ byte++ ];
            rcl->parent.bloc |= ( temp << shift );
            shift += 8;
         }
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
