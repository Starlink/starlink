#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int dat1_unpack_srv( const unsigned char psrv[ 4 ], struct RID *rid )
   {
/*+									    */
/* Name:								    */
/*    dat1_unpack_srv							    */

/* Purpose:								    */
/*    Unpack a Structure Record Vector element.				    */

/* Invocation:								    */
/*    dat1_unpack_srv( psrv, rid )					    */
/*									    */
/* Description:								    */
/*    This function unpacks the information in an element of a Structure    */
/*    Record Vector, putting it into a RID structure (which then identifies */
/*    the structure element's Component Record).  This is done so that the  */
/*    Structure Record Vector format need not depend on the details of the  */
/*    way that a RID structure is stored in memory.			    */

/* Parameters:								    */
/*    const unsigned char psrv[ 4 ]					    */
/*	 Pointer to an array of 4 Structure Record Vector bytes to be	    */
/*	 unpacked.							    */
/*    struct RID *rid							    */
/*	 Pointer to a RID structure to receive the unpacked information.    */

/* Returned Value:							    */
/*    int dat1_unpack_srv						    */
/*       The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    16-APR-1991 (RFWS):						    */
/*       Original version.						    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Unpack the RID .bloc field from the first 20 bits, and the RID .chip	    */
/* field from the following 4 bits.					    */
      rid->bloc = ( ( ( ( psrv[ 2 ] & 0xf ) << 8 ) |
			  psrv[ 1 ] ) << 8 ) |
			  psrv[ 0 ];
      rid->chip =  ( psrv[ 2 ] >> 4 ) & 0xf;

/* Return the current global status value.				    */
      return hds_gl_status;
   }
