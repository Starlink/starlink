#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec1_pack_hcb( const struct HCB *hcb,
		      unsigned char phcb[ REC__SZBLK ] )
   {
/*+									    */
/* Name:								    */
/*    rec1_pack_hcb							    */

/* Purpose:								    */
/*    Pack Header Control Block information.				    */

/* Invocation:								    */
/*    rec1_pack_hcb( hcb, phcb )					    */

/* Description:								    */
/*    This function packs the information in an HCB structure and puts it   */
/*    into an array of characters suitable for writing to the Header	    */
/*    Control Block of an HDS container file. This is done so that the	    */
/*    Header Control Block format need not depend on the details of the way */
/*    that an HCB structure is stored in memory.			    */

/* Parameters:								    */
/*    const struct HCB *hcb						    */
/*	 Pointer to an HCB structure containing the information to be	    */
/*	 packed.							    */
/*    unsigned char phcb[ REC__SZBLK ]					    */
/*	 Pointer to an array of REC__SZBLK Header Control Block characters  */
/*	 into which the information is to be packed.			    */

/* Returned Value:							    */
/*    int rec1_pack_hcb							    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    6-FEB-1991 (RFWS):						    */
/*       Original version.						    */
/*    21-MAY-1991 (RFWS):						    */
/*       Removed use of HCB "reserve" field: no longer present.		    */
/*    23-MAY-1991 (RFWS):						    */
/*	 Added checks for flag values of -1 in the STK .bloc and .spare	    */
/*	 fields. This is now needed because these fields are stored	    */
/*	 internally as normal integers rather than as 20 bit fields.	    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      int bloc;			 /* STK .bloc field value		    */
      int i;			 /* Loop counter for packing arrays	    */
      int j;			 /* Position of start of STK data	    */
      int spare;		 /* STK .spare field value		    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Pack the stamp field into the first 3 chars.				    */
      phcb[ 0 ] = hcb->stamp & 0xff;
      phcb[ 1 ] = ( hcb->stamp >> 8 ) & 0xff;
      phcb[ 2 ] = ( hcb->stamp >> 16 ) & 0xff;

/* Insert the version number into the 4th char.				    */
      phcb[ 3 ] = hcb->version;

/* Pack the end-of-file block number into the following 4 chars.	    */
      phcb[ 4 ] = hcb->eof & 0xff;
      phcb[ 5 ] = ( hcb->eof >> 8 ) & 0xff;
      phcb[ 6 ] = ( hcb->eof >> 16 ) & 0xff;
      phcb[ 7 ] = ( hcb->eof >> 24 ) & 0xff;

/* The next 24 chars are not used, so set them to zero.			    */
      for ( i = 0; i < 24; i++ )
      {
         phcb[ i + 8 ] = 0;
      }

/* Loop to pack all the STK elements, each of which occupies a further 5    */
/* chars.								    */
      for ( i = 0, j = 32; i < REC__MXSTK; i++, j += 5 )
      {

/* Obtain the values of the STK .bloc and .spare fields, checking for any   */
/* flag values of -1 and translating them into all 20 bits of the packed    */
/* value being set.							    */
         bloc = ( hcb->stk[ i ].bloc == -1 ) ? 0xfffff : hcb->stk[ i ].bloc;
         spare = ( hcb->stk[ i ].spare == -1 ) ? 0xfffff : hcb->stk[ i ].spare;

/* Pack the STK .bloc and .spare fields into each group of 5 chars, 20 bits */
/* per value.								    */
         phcb[ j ] = bloc & 0xff;
	 phcb[ j + 1 ] = ( bloc >> 8 ) & 0xff;

/* The middle char contains contributions from both values...		    */
	 phcb[ j + 2 ] = ( ( bloc >> 16 ) & 0xf ) | ( ( spare << 4 ) & 0xf0 );
         phcb[ j + 3 ] = ( spare >> 4 ) & 0xff;
	 phcb[ j + 4 ] = ( spare >> 12 ) & 0xff;
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
