#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if !HAVE_GETWD && !HAVE_GETCWD
/* use a void function on the rare (VMS) chance that neither getwd not
   getcwd are available */
void rec1_getcwd( void ){};      /* This routine is not used on VMS systems */
#else

/* This routine includes code for both getwd and getcwd. The trick
   is to decide which one to prefer. In the past, solaris always
   used getwd (for "speed") and everything else used getcwd.
   We prefer to use getcwd if it is available since in most cases
   getwd is a wrapper around getcwd
 */

#if HAVE_GETCWD
# define USE_GETCWD 1
#elif HAVE_GETWD
# define USE_GETWD 1
#else
   error unable to find either getwd or getcwd
#endif


/* System include files */

#include <string.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if USE_GETCWD
#  include <errno.h>
#  include <limits.h>
#else
/* GETWD requires sys/param.h for constants */
#  include <sys/param.h>
#endif

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* File globals - declare them outside the function so that we can
   clean up the malloc for valgrind on exit. */
#if USE_GETWD
      static char wd[ MAXPATHLEN ]; /* Static buffer for path name          */

#else                            /* getcwd local variables:                 */
      static INT mxwd;           /* Amount of space allocated               */
      static char *wd = NULL;    /* Pointer to working directory string.    */
#endif


   void rec1_getcwd( char **cwd, INT *lcwd )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_getcwd                                                           */

/* Purpose:                                                                 */
/*    Obtain a path name for the current working directory.                 */

/* Invocation:                                                              */
/*    rec1_getcwd( cwd, lcwd )                                              */
/*                                                                          */
/* Description:                                                             */
/*    This function returns a pointer to a null-terminated string           */
/*    containing a path name for the current working directory, together    */
/*    with the length of the path name in characters.                       */

/* Parameters:                                                              */
/*    char **cwd                                                            */
/*       Pointer to a char pointer which will be set to point at a null     */
/*       terminated character string containing a path name for the current */
/*       working directory.                                                 */
/*    INT *lcwd                                                             */
/*       Pointer to an integer to receive the length of the path name in    */
/*       characters (excluding the terminating null).                       */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*   -  A null pointer will be returned in *cwd if an error occurs or if    */
/*   the routine is called with the global status set.                      */
/*   -  The returned path name string occupies space which is managed by    */
/*   rec1_getcwd and should not be deallocated by the caller. This space    */
/*   may be over-written or deallocated by a subsequent call to             */
/*   rec1_getcwd.                                                           */
/*   -  This routine exists simply to overcome the lack of dynamic string   */
/*   allocation in the standard POSIX getcwd function.                      */
/*   -  This routine is not implemented on VMS systems.                     */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
/*    Copyright (C) 2005-2006 Particle Physics and Astronomy Research       */
/*                 Council. All Rights Reserved.                            */

/*  Licence:                                                                */
/*     This program is free software; you can redistribute it and/or        */
/*     modify it under the terms of the GNU General Public License as       */
/*     published by the Free Software Foundation; either version 2 of       */
/*     the License, or (at your option) any later version.                  */

/*     This program is distributed in the hope that it will be              */
/*     useful, but WITHOUT ANY WARRANTY; without even the implied           */
/*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR              */
/*     PURPOSE. See the GNU General Public License for more details.        */

/*     You should have received a copy of the GNU General Public            */
/*     License along with this program; if not, write to the Free           */
/*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,       */
/*     MA 02110-1301, USA                                                   */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    23-NOV-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    06-SEP-2005 (TIMJ):                                                   */
/*       Use autoconf to determine getwd availability                       */
/*    01-FEB-2006 (TIMJ):                                                   */
/*       Add rec1_getcwd_free                                               */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Constants:                                                         */
#if USE_GETCWD
#  if defined( PATH_MAX )
      const INT mxwd0 = PATH_MAX + 1; /* Initial amount of space for path   */
#  else
      const INT mxwd0 = _POSIX_PATH_MAX + 1;
#  endif
#endif

/* Local Variables:                                                         */

/* External references:                                                     */
#if USE_GETWD && !HAVE_DECL_GETWD
      char *getwd( char pathname[ MAXPATHLEN ] ); /* Get working directory  */
#endif

/*.                                                                         */

/* Set initial default returned values.                                     */
      *cwd = NULL;
      *lcwd = 0;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return;

/* getwd version:                                                           */
/* ============                                                             */
/* Implemented separately for better performance.                           */
#if USE_GETWD

/* Copy the working directory name into a static buffer and check for       */
/* errors.                                                                  */
      if ( getwd( wd ) == (char *) 0 )
      {
         hds_gl_status = DAT__FATAL;
         emsSetnc( "MESSAGE", wd, EMS__SZTOK );
         emsRep( "REC1_GETCWD_1",
                    "Unable to determine a path name for the current \
working directory - ^MESSAGE",
                    &hds_gl_status );
      }

/* getcwd version:                                                          */
/* ================                                                         */
#else
/* If no space has yet been allocated for the working directory path, then  */
/* allocate an initial amount.                                              */
      if ( wd == NULL )
      {
         rec_alloc_mem( mxwd0, (void **) &wd );
         if ( _ok( hds_gl_status ) ) mxwd = mxwd0;
      }

/* Obtain the working directory path string and check for errors.           */
      while ( _ok( hds_gl_status ) )
      {
         if ( getcwd( wd, (size_t) mxwd ) != NULL )
         {
            break;
         }

/* If the buffer supplied was too short, then double its length and try     */
/* again.                                                                   */
         else if ( errno == ERANGE )
         {
            rec_reall_mem( mxwd * 2, (void **) &wd );
            if( _ok( hds_gl_status ) ) mxwd *= 2;
         }

/* If any other error occurred, then report it.                             */
         else
         {
            hds_gl_status = DAT__FATAL;
            emsSyser( "MESSAGE", errno );
            emsRep( "REC1_GETCWD_2",
                       "Unable to determine a path name for the current \
working directory - ^MESSAGE",
                       &hds_gl_status );
         }
      }
#endif

/* If OK, return the path name pointer and the length of the string it      */
/* points at.                                                               */
      if ( _ok( hds_gl_status ) )
      {
         *cwd = wd;
         *lcwd = (INT) strlen( wd );
      }
      return;
   }
#endif

/* simple routine called from rec_stop to free up the memory that may
   have been malloced by rec1_getcwd */
int
rec1_getcwd_free( void ) {
  /* GETWD uses a static buffer which we should not free */
#if ! USE_GETWD
  if (wd != NULL) {
    rec_deall_mem(mxwd*sizeof(char), (void**)&wd);
    if (!_ok(hds_gl_status)) return hds_gl_status;
  }
#endif
  return DAT__OK;
}
