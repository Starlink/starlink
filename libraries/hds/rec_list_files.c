#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include <stdio.h>
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec_list_files( void )
   {
/*+									    */
/* Name:								    */
/*    rec_list_files							    */

/* Purpose:								    */
/*    List open file statistics.					    */

/* Invocation:								    */
/*    rec_list_files( )							    */

/* Description:								    */
/*    This function lists statistics about the currently open container	    */
/*    files on the standard output device.				    */

/* Parameters:								    */
/*    void								    */

/* Returned Value:							    */
/*    int rec_list_files						    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    14-APR-1991 (RFWS):						    */
/*       Added prologue and error handling and tidied.			    */
/*    26-APR-1991 (RFWS):						    */
/*       Changed to cater for a null-terminate file name.		    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      const char *disp;		 /* Pointer to file disposition string	    */
      const char *mode;		 /* Pointer to access mode string	    */
      int slot;			 /* Loop counter for File Control Vector    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Scan through the File Control Vector looking for open slots.		    */
      for ( slot = rec_gl_endslot - 1; slot >= 0; slot-- )
      {
         if ( rec_ga_fcv[ slot ].open )
         {

/* For each open slot, display the fully expanded file name, the	    */
/* disposition and access mode, and the reference count.		    */
            disp = ( rec_ga_fcv[ slot ].dele ) ? "[D]" : "[K]";
            mode = ( rec_ga_fcv[ slot ].write) ? "[U]" : "[R]";
            printf( "%s, disp=%s, mode=%s, refcnt=%-d\n",
                    rec_ga_fcv[ slot ].name, disp, mode,
		    rec_ga_fcv[ slot ].count );
         }
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
