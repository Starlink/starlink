#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int dat1_pack_odl( const struct ODL *odl, unsigned char *podl )
   {
/*+									    */
/* Name:								    */
/*    dat1_pack_odl							    */

/* Purpose:								    */
/*    Pack an Object Descriptor Label for an Object Record.		    */

/* Invocation:								    */
/*    dat1_pack_odl( odl, podl )					    */
/*									    */
/* Description:								    */
/*    This function packs Object Descriptor Label information held in an    */
/*    ODL structure into the form needed for storage in the Static Domain   */
/*    of an Object Record. This is done so that the Static Domain format    */
/*    need not depend on the details of the way that an ODL structure is    */
/*    stored in memory.							    */

/* Parameters:								    */
/*    const struct ODL *odl						    */
/*	 Pointer to an ODL structure containing the information to be	    */
/*	 packed.							    */
/*    unsigned char *podl						    */
/*	 Pointer to an array of Static Domain characters into which the	    */
/*	 information is to be packed.					    */

/* Returned Value:							    */
/*    int dat1_pack_odl							    */
/*       The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    11-APR-1991 (RFWS):						    */
/*       Original version.						    */
/*    12-JUN-1991 (RFWS):						    */
/*       Changed packing of naxes field to relect a new internal type of    */
/*	 int.								    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      int axis;			 /* Axis size				    */
      int i;		         /* Loop counter; unpacked array index	    */
      int j;			 /* Loop counter; packed array index	    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Pack the first 15 chars which contain the ODL type field.		    */
      for ( i = 0; i < 15; i++ )
      {
         podl[ i ] = *( (unsigned char *) ( odl->type + i ) );
      }

/* Pack the number of axes into the next char.				    */
      podl[ 15 ] = odl->naxes & 0xff;

/* Loop to pack the axis sizes, each of which occupies a further 4 chars.   */
      for ( i = 0, j = 16; i < odl->naxes; i++, j += 4 )
      {
         axis = odl->axis[ i ];
         podl[ j ] = axis & 0xff;
	 podl[ j + 1 ] = ( axis >> 8 ) & 0xff;
	 podl[ j + 2 ] = ( axis >> 16 ) & 0xff;
	 podl[ j + 3 ] = ( axis >> 24 ) & 0xff;
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
