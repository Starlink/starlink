#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   void rec_close_file( const struct HAN *han )
   {
/*+									    */
/* Name:								    */
/*    rec_close_file							    */

/* Purpose:								    */
/*    Close an HDS container file.					    */

/* Invocation:								    */
/*    rec_close_file( han )						    */

/* Description:								    */
/*    This function closes an HDS container file. It will be deleted at	    */
/*    this point it it has previously been marked for deletion, otherwise   */
/*    it will be kept.							    */

/* Parameters:								    */
/*    const struct HAN *han						    */
/*	 Pointer to a HAN structure containing a handle to any record in    */
/*	 the file.							    */

/* Returned Value:							    */
/*    void								    */

/* Notes:								    */
/*    This routine attempts to execute even if the global HDS status is set */
/*    on entry, although no further error report will be made if it	    */
/*    subsequently fails under these circumstances.			    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    22-APR-1991 (RFWS):						    */
/*       Added prologue and error handling and tidied.			    */
/*    11-SEP-1992 (RFWS):						    */
/*	 Routine no longer examines the file's reference count; it closes   */
/*	 the file unconditionally. Handling the reference count is now the  */
/*	 caller's responsibility.					    */
/*    23-SEP-1992 (RFWS):						    */
/*       Removed the disp argument; marking a file for deletion is now a    */
/*	 separate operation. Made this a void function and renamed it.	    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/*.									    */

/* Begin a new error reporting context.					    */
      ems_begin_c( &hds_gl_status );

/* Close the File Control Vector slot.					    */
      rec1_close_slot( han->slot );

/* End the error reporting context and return.				    */
      ems_end_c( &hds_gl_status );
      return;
   }
