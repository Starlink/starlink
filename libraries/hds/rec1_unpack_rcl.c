#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec1_unpack_rcl( const unsigned char prcl[ REC__SZRCL ],
		        struct RCL *rcl )
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
/*    const unsigned char prcl[ REC__SZRCL  ]				    */
/*	 Pointer to an array of REC__SZRCL Record Control Label characters  */
/*	 containing the information to be unpacked.			    */
/*    struct RCL *rcl							    */
/*	 Pointer to an RCL structure to receive the unpacked information.   */

/* Returned Value:							    */
/*    int rec1_unpack_rcl						    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    12-FEB-1991 (RFWS):						    */
/*	 Original version.						    */
/*    8-APR-1991 (RFWS):						    */
/*       Changed to make the parent field of type struct RID.		    */
/*    11-JUN-1991 (RFWS):						    */
/*	 Removed unpacking of the RCL reserve field (no longer present in   */
/*	 the internal representation of the data structure).		    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Unpack the RCL parent.bloc field from the first 20 bits, and the RCL	    */
/* parent.chip field from the following 4 bits.				    */
      rcl->parent.bloc = ( ( ( ( prcl[ 2 ] & 0xf ) << 8 ) |
				 prcl[ 1 ] ) << 8 ) |
				 prcl[ 0 ];
      rcl->parent.chip =  ( prcl[ 2 ] >> 4 ) & 0xf;

/* Extract the RCL active, zero and modify fields from each of the	    */
/* following 3 bits.							    */
      rcl->active = prcl[ 3 ] & 0x1;
      rcl->zero = ( prcl[ 3 ] >> 1 ) & 0x1;
      rcl->modify = ( prcl[ 3 ] >> 2 ) & 0x1;

/* Extract the RCL size field (4 bits), the class field (3 bits) and the    */
/* chain field (1 bit) from the next char.				    */
      rcl->size = prcl[ 4 ] & 0xf;
      rcl->class = ( prcl[ 4 ] >> 4 ) & 0x7;
      rcl->chain = ( prcl[ 4 ] >> 7 ) & 0x1;

/* Obtain the RCL slen field from the next char.			    */
      rcl->slen = *( (const char *) ( prcl + 5 ) );

/* Unpack the dlen field from the final 4 char.				    */
      rcl->dlen = ( ( ( ( ( ( prcl[ 9 ] ) << 8 ) |
                              prcl[ 8 ] ) << 8 ) |
                              prcl[ 7 ] ) << 8 ) |
			      prcl[ 6 ];

/* Return the current global status value.				    */
      return hds_gl_status;
   }
