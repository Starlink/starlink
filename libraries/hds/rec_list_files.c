#if HAVE_CONFIG_H
#  include <config.h>
#endif

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
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    14-APR-1991 (RFWS):						    */
/*       Added prologue and error handling and tidied.			    */
/*    26-APR-1991 (RFWS):						    */
/*       Changed to cater for a null-terminate file name.		    */
/*    12-MAY-2004: (BKM)                                                    */
/*       Add 64-bit item to report                                          */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      const char *disp;		 /* Pointer to file disposition string	    */
      const char *mode;		 /* Pointer to access mode string	    */
      const char *bit64;         /* Pointer to 64-bit type string           */
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
            disp  = ( rec_ga_fcv[ slot ].dele ) ? "[D]" : "[K]";
            mode  = ( rec_ga_fcv[ slot ].write) ? "[U]" : "[R]";
            bit64 = ( rec_ga_fcv[ slot ].hds_version > REC__VERSION3 ) ?
                    "[Y]" : "[N]";
            printf( "%s, disp=%s, mode=%s, refcnt=%-d, 64-bit=%s\n",
                    rec_ga_fcv[ slot ].name, disp, mode,
		    rec_ga_fcv[ slot ].count, bit64 );
         }
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
