#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */

   int rec_same_file( const struct HAN *han1, const struct HAN *han2 )
   {
/*+									    */
/* Name:								    */
/*    rec_same_file							    */

/* Purpose:								    */
/*    Determine if two records reside in the same file.			    */

/* Invocation:								    */
/*    rec_same_file( han1, han2 )					    */

/* Description:								    */
/*    This function compares two records and determines whether they reside */
/*    in the same container file.					    */

/* Parameters:								    */
/*    const struct HAN *han1						    */
/*       Pointer to a HAN structure containing a handle to the first	    */
/*	 record.							    */
/*    const struct HAN *han2						    */
/*	 Pointer to a HAN structure containing a handle to the second	    */
/*	 record.							    */

/* Returned Value:							    */
/*    int rec_same_file							    */
/*	 Returns 1 if both records reside in the same file, otherwise 0.    */

/* Notes:								    */
/*    This routine executes regardless of the HDS global status value.	    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    24-APR-1991 (RFWS):						    */
/*       Added prologue and changed to return a logical result.		    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/*.									    */

/* Compare the File Control Vector slot numbers for each handle.	    */
      return ( han1->slot == han2->slot );
   }
