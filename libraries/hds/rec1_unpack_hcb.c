#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec1_unpack_hcb( const unsigned char phcb[ REC__SZBLK ],
			struct HCB *hcb )
   {
/*+									    */
/* Name:								    */
/*    rec1_unpack_hcb							    */

/* Purpose:								    */
/*    Unpack Header Control Block information.				    */

/* Invocation:								    */
/*    rec1_unpack_hcb( phcb, hcb )					    */
/*									    */
/* Description:								    */
/*    This function unpacks the information in the Header Control Block of  */
/*    an HDS container file and puts it into an HCB structure. This is done */
/*    so that the Header Control Block format need not depend on the	    */
/*    details of the way that an HCB structure is stored in memory.	    */

/* Parameters:								    */
/*    const unsigned char phcb[ REC__SZBLK ]				    */
/*	 Pointer to an array of REC__SZBLK Header Control Block characters  */
/*	 to be unpacked.						    */
/*    struct HCB *hcb							    */
/*	 Pointer to an HCB structure to receive the unpacked information.   */

/* Returned Value:							    */
/*    int rec1_unpack_hcb						    */
/*       The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    25-JUL-1990 (RFWS):						    */
/*       Original version.						    */
/*    25-MAR-1991 (RFWS):						    */
/*	 Added unpacking of the reserve field and fixed bug in STK	    */
/*	 structure unpacking.						    */
/*    21-MAY-1991 (RFWS):						    */
/*	 Removed reserve field: no longer present.			    */
/*    23-MAY-1991 (RFWS):						    */
/*	 Added test for all bits set in STK bloc and spare fields. This is  */
/*	 now needed because these HCB components are stored internally as   */
/*	 normal integers, not as 20-bit fields as they are in their packed  */
/*	 form.								    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      int bloc;			 /* Value of STK bloc field		    */
      int i;			 /* Loop counter for STK elements	    */
      int j;			 /* Position of start of STK data	    */
      int spare;		 /* Value of STK spare field		    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Extract the stamp field from the first 3 chars.			    */
      hcb->stamp = ( ( ( ( phcb[ 2 ] ) << 8 ) |
                           phcb[ 1 ] ) << 8 ) |
			   phcb[ 0 ];

/* Extract the version number from the 4th char.			    */
      hcb->version = phcb[ 3 ];

/* Extract the end-of-file block number from the following 4 chars.	    */
      hcb->eof = ( ( ( ( ( ( phcb[ 7 ] ) << 8 ) |
                             phcb[ 6 ] ) << 8 ) |
                             phcb[ 5 ] ) << 8 ) |
			     phcb[ 4 ];

/* Loop to extract all the STK elements, each of which occupies 5 chars.    */
      for ( i = 0, j = 32; i < REC__MXSTK; i++, j += 5 )
      {

/* Extract the STK bloc field from the first 20 bits of each element. If    */
/* all 20 bits are set, then use a flag value of -1 instead.		    */
         bloc = ( ( ( ( phcb[ j + 2 ] & 0xf ) << 8 ) |
                        phcb[ j + 1 ] ) << 8 ) |
			phcb[ j ];
         hcb->stk[ i ].bloc = ( bloc == 0xfffff ) ? -1 : bloc;

/* Similarly extract the STK spare field from the second 20 bits, again	    */
/* testing if all bits are set and using a flag value of -1 if they are.    */
         spare = ( ( ( phcb[ j + 4 ] << 8 ) |
                       phcb[ j + 3 ] ) << 4 ) |
                     ( ( phcb[ j + 2 ] >> 4 ) & 0xf );
         hcb->stk[ i ].spare = ( spare == 0xfffff ) ? -1 : spare;
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
