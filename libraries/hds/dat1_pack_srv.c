#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int dat1_pack_srv( const struct RID *rid, unsigned char psrv[ 4 ] )
   {
/*+									    */
/* Name:								    */
/*    dat1_pack_srv							    */

/* Purpose:								    */
/*    Pack a Structure Record Vector element.				    */

/* Invocation:								    */
/*    dat1_pack_srv( rid, psrv )					    */
/*									    */
/* Description:								    */
/*    This function packs information from a RID structrure (which	    */
/*    identifies a structure element's Component Record) into an element of */
/*    a Structure Record Vector.  This is done so that the Structure Record */
/*    Vector format need not depend on the details of the way that a RID    */
/*    structure is stored in memory.					    */

/* Parameters:								    */
/*    const struct RID *rid						    */
/*	 Pointer to a RID structure containing the information to be	    */
/*	 packed.							    */
/*    unsigned char psrv[ 4 ]						    */
/*	 Pointer to an array of 4 Structure Record Vector bytes into which  */
/*	 the information is to be packed.				    */

/* Returned Value:							    */
/*    int dat1_pack_srv							    */
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

/* Pack the RID .bloc field into the first 20 bits, and the RID .chip field */
/* into the following 4 bits.						    */
      psrv[ 0 ] = rid->bloc & 0xff;
      psrv[ 1 ] = ( rid->bloc >> 8 ) & 0xff;
      psrv[ 2 ] = ( ( rid->bloc >> 16 ) & 0xf ) |
		  ( rid->chip << 4 );

/* The following byte is not used, so set it to zero.			    */
      psrv[ 3 ] = 0;

/* Return the current global status value.				    */
      return hds_gl_status;
   }
