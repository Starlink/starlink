#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec_get_handle( const struct RID *rid, const struct HAN *kin,
		       struct HAN *han )
   {
/*+									    */
/* Name:								    */
/*    rec_get_handle							    */

/* Purpose:								    */
/*    Obtain a handle for a record in a container file.			    */

/* Invocation:								    */
/*    rec_get_handle( rid, kin, han )					    */

/* Description:								    */
/*    This function returns a handle for a record in a container file whose */
/*    record ID is known. A handle for another record in the same file must */
/*    also be supplied.							    */

/* Parameters:								    */
/*    const struct RID *rid						    */
/*       Pointer to a RID structure containing the record ID for the	    */
/*	 record.							    */
/*    const struct HAN *kin						    */
/*       Pointer to a HAN structure containing a handle for any other	    */
/*	 record in the same container file.				    */
/*    struct HAN *han							    */
/*       Pointer to a HAN structure in which the new handle will be	    */
/*	 returned.							    */

/* Returned Value:							    */
/*    int rec_get_handle						    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    BKM:  B.K, McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    14-APR-1991 (RFWS):						    */
/*       Added prologue and error handling and tidied.			    */
/*    19-APR-2004 (BKM):                                                    */
/*       Revise for 64-bit HDS files.                                       */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      struct RCL rcl;
/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Copy the File Control Vector slot number and read flag from the handle   */
/* supplied and insert the record ID into the new handle.		    */
      han->slot = kin->slot;
      han->read = kin->read;
      han->rid = *rid;
      rec_get_rcl(han, &rcl);

/* Return the current global status value.				    */
      return hds_gl_status;
   }
