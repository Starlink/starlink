#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */
#include "rec1.h"

   int rec1_pack_rcl( const struct RCL *rcl,
		      unsigned char prcl[] )
   {
/*+									    */
/* Name:								    */
/*    rec1_pack_rcl							    */

/* Purpose:								    */
/*    Pack Record Control Label information.				    */

/* Invocation:								    */
/*    rec1_pack_rcl( rcl, prcl )					    */

/* Description:								    */
/*    This function packs the information in an RCL structure and puts it   */
/*    into an array of characters suitable for writing to the Record	    */
/*    Control Label held in the Control Domain of a record.  This is done   */
/*    so that the Record Control Lablel format need not depend on the	    */
/*    details of the way that an RCL structure is stored in memory.	    */

/* Parameters:								    */
/*    const struct RCL *hcb						    */
/*	 Pointer to an RCL structure containing the information to be	    */
/*	 packed.							    */
/*    unsigned char phcb[ REC__SZRCL or REC__SZORCL]	 		    */
/*	 Pointer to *EITHER* an array of REC__SZRCL *OR* REC__SZORCL Record */
/*       Control Lable characters depending on the rcl->extended flag       */
/*	 into which the information is to be packed.			    */

/* Returned Value:							    */
/*    int rec1_pack_rcl							    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    10-APR-1991 (RFWS):						    */
/*       Original version.						    */
/*    11-JUN-1991 (RFWS):						    */
/*	 Removed use of the RCL reserve field (no longer present in the	    */
/*	 internal representation of the data structure).		    */
/*    23-JUN-2000: (BKM)                                                    */
/*       Revise packing logic for HDS Version 4.                            */
/*    16-AUG-2000: (BKM)                                                    */
/*       Correct packing logic to pack BOTH HDS3 and HDS4 RCLs              */
/*       Correct prcl[3] packing on V4 files                                */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local variables:                                                         */
      INT_BIG temp;       /* Temporary variable.                            */
      int byte;           /* Current position in the prcl array.            */
      int i;              /* Loop index variable                            */
/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Pack the common part of the RCL parent field into the first 2 chars.     */
      prcl[ 0 ] = rcl->parent.bloc & 0xff;
      prcl[ 1 ] = ( rcl->parent.bloc >> 8 ) & 0xff;

      if ( !hds_gl_64bit )      /* 32-bit HDS files (V3)                    */
/* The RCL parent field is packed into the first 3 chars. This is a RID	    */
/* structure; the first 2 chars hold part of the 20-bit RID bloc field and  */
/* the third char holds the remaining 4 bits plus the 4-bit RID chip field. */
      {
         prcl[ 2 ] = ( ( rcl->parent.bloc >> 16 ) & 0xf ) |
		     ( ( rcl->parent.chip & 0xf ) << 4 );

/* Pack the active (1 bit), zero (1 bit) and modify (1 bit) fields into the */
/* next char, leaving the remaining bits at zero.			    */
         prcl[ 3 ] = ( ( ( ( rcl->modify & 0x1 ) << 1 ) |
			   ( rcl->zero & 0x1 ) ) << 1 ) |
			   ( rcl->active & 0x1 );

/* Pack the size (4 bits), class (3 bits) and chain (1 bit) fields into the */
/* next char.								    */
         prcl[ 4 ] = ( ( ( ( rcl->chain & 0x1 ) << 3 ) |
                           ( rcl->class & 0x7 ) ) << 4 ) |
		           ( rcl->size & 0xf );

/* The next char holds the 8-bit slen field.				    */
         prcl[ 5 ] = rcl->slen & 0xff;

/* The final 4 chars hold the 32-bit dlen field.			    */
         prcl[ 6 ] = rcl->dlen & 0xff;
         prcl[ 7 ] = ( rcl->dlen >> 8 ) & 0xff;
         prcl[ 8 ] = ( rcl->dlen >> 16 ) & 0xff;
         prcl[ 9 ] = ( rcl->dlen >> 24 ) & 0xff;

      }
      else               /* 64-bit HDS files HDS (V4)                       */
/* Part of the RCL parent field is packed into the first 3 chars. This      */
/* consists of the first two bytes of the RID bloc field plus the 4-bit chip*/
/* field.                                                                   */
      {
         prcl[ 2 ] = rcl->parent.chip & 0xf;

/* Pack the active (1 bit), zero (1 bit), modify (1 bit) and extended (1    */
/* bit) fields into the next char, and the domain length in bytes in the    */
/* top 4 bits.                                                              */
         prcl[ 3 ] = ( ( ( ( rcl->modify & 0x1 ) << 1  )  |
			    ( rcl->zero & 0x1 ) ) << 1  )  |
			    ( rcl->active & 0x1 );
         prcl[ 3 ] |= 0x08;     /* rcl->extended bit always set             */

/* Pack the size (4 bits), class (3 bits) and chain (1 bit) fields into the */
/* next char.								    */
         prcl[ 4 ] = ( ( ( ( rcl->chain & 0x1 ) << 3 ) |
                           ( rcl->class & 0x7 ) ) << 4 ) |
		           ( rcl->size & 0xf );

/* The next char holds the 8-bit slen field.				    */
         prcl[ 5 ] = rcl->slen & 0xff;

/* The next 8 bytes hold the the dlen field.                                */
         byte = 6;
         temp = rcl->dlen;
         for ( i = 0; i < 8; i++ )
         {
            prcl[ byte++ ] = temp & 0xff;
            temp >>= 8;
         }

/* The next 5 bytes hold the high order bytes of the parent bloc field      */
         temp = rcl->parent.bloc >> 16;
         for ( i = 0; i < 5; i++ )
         {
            prcl[ byte ++ ] = temp & 0xff;
            temp >>= 8;
         }
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
