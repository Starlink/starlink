#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT_ error codes			    */

   void dat1_intune( int *status )
   {
/*+									    */
/* Name:								    */
/*    dat1_intune							    */

/* Purpose:								    */
/*    Initialise default HDS tuning profile.				    */

/* Language:								    */
/*    ANSI C								    */

/* Invocation:								    */
/*    dat1_intune( status )						    */

/* Description:								    */
/*    This routine should be called to set up the default HDS tuning	    */
/*    profile prior to making any other use of HDS. This default profile    */
/*    may be configured by external "environment variables", whose values   */
/*    are obtained by this routine and used to initialise appropriate	    */
/*    global variables. No other aspects of HDS are initialised by this	    */
/*    routine.								    */

/* Parameters:								    */
/*    int *status							    */
/*       Pointer to the inherited global status.			    */

/* Returned Value:							    */
/*    void								    */

/* Notes:								    */
/*    This routine performs its function once only.  If it is called again  */
/*    (after a previous successful invocation), it will return without	    */
/*    action.								    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    25-FEB-1992 (RFWS):						    */
/*       Original version.						    */
/*    30-NOV-1992 (RFWS):						    */
/*       Added HDS_SHELL tuning parameter.				    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      static int done = 0;	 /* Routine called at least once?	    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( *status ) ) return;

/* Ignore all invocations after the first successful one.		    */
      if ( !done )
      {

/* Initialise the HDS tuning parameters, applying suitable constraints on   */
/* their values. For tuning parameters which have a "one-shot" mode (set    */
/* through a call to HDS_TUNE), also initialise the one-shot value to the   */
/* default value.							    */

/* Initial file allocation quantity: constrain this to be at least 2	    */
/* blocks.								    */
	 dat1_getenv( "HDS_INALQ", HDS__INALQ, &hds_gl_inalq0 );
	 if ( hds_gl_inalq0 < 2 ) hds_gl_inalq0 = 2;
	 hds_gl_inalq = hds_gl_inalq0;

/* Use file mapping if available?					    */
	 dat1_getenv( "HDS_MAP", HDS__MAP, &hds_gl_map );
         switch ( hds_gl_map )
	 {

/* If -1 was specified, set file mapping if it is the best mode for	    */
/* sequential file access.						    */
	    case -1:
	       hds_gl_map = HDS__MAPSEQ;
	       break;

/* If -2 was specified, set file mapping if it is the best mode for sparse  */
/* file access.								    */
	    case -2:
	       hds_gl_map = HDS__MAPSPARSE;
	       break;

/* If -3 was specified, set file mapping if it is the best mode for	    */
/* minimising memory usage.						    */
	    case -3:
	       hds_gl_map = HDS__MAPMEM;
	       break;

/* Convert all other non-zero values to 1.				    */
	    default:
	       hds_gl_map = ( hds_gl_map != 0 );
	       break;
         }

/* Turn mapping off if it is not implemented.				    */
	 hds_gl_map = ( hds_gl_map && HDS__CANMAP );

/* Maximum size of the "working page list": constrain this to be at least   */
/* the default value (HDS__MAXWPL).					    */
	 dat1_getenv( "HDS_MAXWPL", HDS__MAXWPL, &hds_gl_maxwpl );
	 if ( hds_gl_maxwpl < HDS__MAXWPL ) hds_gl_maxwpl = HDS__MAXWPL;

/* Size of the internal "transfer buffer": constrain this to be at least    */
/* the default value (HDS__NBLOCKS).					    */
	 dat1_getenv( "HDS_NBLOCKS", HDS__NBLOCKS, &hds_gl_nblocks );
	 if ( hds_gl_nblocks < HDS__NBLOCKS ) hds_gl_nblocks = HDS__NBLOCKS;

/* Optimum number of structure components: constrain this to be at least    */
/* one.									    */
	 dat1_getenv( "HDS_NCOMP", HDS__NCOMP, &hds_gl_ncomp0 );
         if ( hds_gl_ncomp0 < 1 ) hds_gl_ncomp0 = 1;
         hds_gl_ncomp = hds_gl_ncomp0;

/* Shell used for file name expansion (UNIX & POSIX systems only): if the   */
/* supplied value lies outside the supported range, then use the default    */
/* shell.								    */
	 dat1_getenv( "HDS_SHELL", HDS__SHELL, &hds_gl_shell );
	 if ( ( hds_gl_shell < HDS__NOSHELL ) ||
	      ( hds_gl_shell > HDS__MXSHELL ) )
	 {
	    hds_gl_shell = HDS__SHELL;
	 }

/* System wide lock flag: zero implies no system wide locking, everything   */
/* else implies system wide locking.					    */
	 dat1_getenv( "HDS_SYSLCK", HDS__SYSLCK, &hds_gl_syslck );
	 hds_gl_syslck = ( hds_gl_syslck != 0 );

/* Wait for locked files? Zero implies don't wait, everything else implies  */
/* wait.								    */
	 dat1_getenv( "HDS_WAIT", HDS__WAIT, &hds_gl_wait );
         hds_gl_wait = ( hds_gl_wait != 0 );

/* Note when the routine has completed successfully, so that it will not    */
/* execute again.							    */
	 if ( _ok( hds_gl_status ) )
	 {
            done = 1;
         }
      }

/* Exit the routine.							    */
      return;
   }
