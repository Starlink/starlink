#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */

#if defined( vms )
void rec1_getcwd( void ){};	 /* This routine is not used on VMS systems */
#else

/* SUN4 version include files:						    */
/* ==========================						    */
#if defined( sun4 )
#include <string.h>
#include <sys/param.h>

/* Portable version include files:					    */
/* ==============================					    */
#else
#include <errno.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#endif

/* Other include files:							    */
/* ===================							    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "ems_par.h"		 /* EMS__ public constants		    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   void rec1_getcwd( char **cwd, INT *lcwd )
   {
/*+									    */
/* Name:								    */
/*    rec1_getcwd							    */

/* Purpose:								    */
/*    Obtain a path name for the current working directory.		    */

/* Invocation:								    */
/*    rec1_getcwd( cwd, lcwd )						    */
/*									    */
/* Description:								    */
/*    This function returns a pointer to a null-terminated string	    */
/*    containing a path name for the current working directory, together    */
/*    with the length of the path name in characters.			    */

/* Parameters:								    */
/*    char **cwd							    */
/*	 Pointer to a char pointer which will be set to point at a null	    */
/*	 terminated character string containing a path name for the current */
/*	 working directory.						    */
/*    INT *lcwd								    */
/*	 Pointer to an integer to receive the length of the path name in    */
/*	 characters (excluding the terminating null).			    */

/* Returned Value:							    */
/*    void								    */

/* Notes:								    */
/*   -  A null pointer will be returned in *cwd if an error occurs or if    */
/*   the routine is called with the global status set.			    */
/*   -  The returned path name string occupies space which is managed by    */
/*   rec1_getcwd and should not be deallocated by the caller. This space    */
/*   may be over-written or deallocated by a subsequent call to		    */
/*   rec1_getcwd.							    */
/*   -  This routine exists simply to overcome the lack of dynamic string   */
/*   allocation in the standard POSIX getcwd function.			    */
/*   -  This routine is not implemented on VMS systems.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    23-NOV-1992 (RFWS):						    */
/*       Original version.						    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Constants:							    */
#if defined( sun4 )		 /* No local constants for SUN4 version	    */

#else				 /* Portable version local constants:	    */
#if defined( PATH_MAX )
      const INT mxwd0 = PATH_MAX + 1; /* Initial amount of space for path   */
#else
      const INT mxwd0 = _POSIX_PATH_MAX + 1;
#endif
#endif

/* Local Variables:							    */
#if defined( sun4 )		 /* SUN4 version local variables:	    */
      static char wd[ MAXPATHLEN ]; /* Static buffer for path name	    */

#else				 /* Portable version local variables:	    */
      static INT mxwd;		 /* Amount of space allocated		    */
      static char *wd = NULL;	 /* Pointer to working directory string.    */
#endif

/* External references:							    */
#if defined( sun4 )		 /* SUN4 version system routines:	    */
      char *getwd( char pathname[ MAXPATHLEN ] ); /* Get working directory  */
#endif

/*.									    */

/* Set initial default returned values.					    */
      *cwd = NULL;
      *lcwd = 0;

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return;

/* SUN4 version:							    */
/* ============								    */
/* Implemented separately for better performance.			    */
#if defined( sun4 )

/* Copy the working directory name into a static buffer and check for	    */
/* errors.								    */
      if ( getwd( wd ) == (char *) 0 )
      {
	 hds_gl_status = DAT__FATAL;
	 ems_setc_c( "MESSAGE", wd, EMS__SZTOK );
	 ems_rep_c( "REC1_GETCWD_1",
	            "Unable to determine a path name for the current \
working directory - ^MESSAGE",
		    &hds_gl_status );
      }

/* Portable version:							    */
/* ================							    */
#else
/* If no space has yet been allocated for the working directory path, then  */
/* allocate an initial amount.						    */
      if ( wd == NULL )
      {
         rec_alloc_mem( mxwd0, (void **) &wd );
	 if ( _ok( hds_gl_status ) ) mxwd = mxwd0;
      }

/* Obtain the working directory path string and check for errors.	    */
      while ( _ok( hds_gl_status ) )
      {
         if ( getcwd( wd, (size_t) mxwd ) != NULL )
	 {
	    break;
	 }

/* If the buffer supplied was too short, then double its length and try	    */
/* again.								    */
         else if ( errno == ERANGE )
	 {
	    rec_reall_mem( mxwd * 2, (void **) &wd );
	    if( _ok( hds_gl_status ) ) mxwd *= 2;
	 }

/* If any other error occurred, then report it.				    */
	 else
	 {
	    hds_gl_status = DAT__FATAL;
	    ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZTOK );
	    ems_rep_c( "REC1_GETCWD_2",
	               "Unable to determine a path name for the current \
working directory - ^MESSAGE",
		       &hds_gl_status );
	 }
      }
#endif

/* If OK, return the path name pointer and the length of the string it	    */
/* points at.								    */
      if ( _ok( hds_gl_status ) )
      {
         *cwd = wd;
	 *lcwd = (INT) strlen( wd );
      }
      return;
   }
#endif
