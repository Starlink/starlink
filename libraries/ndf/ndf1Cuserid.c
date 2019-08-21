#include "ndf1.h"
#include "sae_par.h"
#include <config.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Prefer getpwuid */

#if HAVE_GETPWUID && HAVE_GETEUID
#   define USE_GETPWUID
#   include <pwd.h>
#   include <unistd.h>
#   include <sys/types.h>
#else
#  if HAVE_GETLOGIN
#     define USE_GETLOGIN
#     include <unistd.h>
#  else
#     if HAVE_CUSERID
#        define USE_CUSERID
#        if !HAVE_DECL_CUSERID
/* cuserid is no longer in 1003.1-1990 */
extern char *cuserid( const char *);
#        endif
#     else
/* We may be trying to build under MinGW, in which case we use the
   Windows function GetUserName */
#        if defined HAVE_DECL_GETUSERNAME && HAVE_DECL_GETUSERNAME
#           define USE_GETUSERNAME
#           include <windows.h>
#           define L_cuserid MAX_PATH
#        else
#           error "No supported cuserid equivalent on this system"
#        endif
#     endif
#  endif
#endif

#if !HAVE_DECL_CUSERID
# ifndef L_cuserid
#   define L_cuserid 64
# endif
#endif

/* Cache storage to minimize system calls. This routine is used
   a lot by NDF when history writing is enabled and we can assume
   that the information is invariant between calls within a single
   process. Access to this cache is serialised using a mutex. */
static char user_cache[L_cuserid];
static char *user_cache_ptr = NULL;
static pthread_mutex_t Ndf_cuserid_mutex = PTHREAD_MUTEX_INITIALIZER;


void ndf1Cuserid( char *user, size_t user_length, int *status ){
/*
*+
*  Name:
*     ndf1Cuserid

*  Purpose:
*     Get the username

*  Synopsis:
*     void ndf1Cuserid( char *user, size_t user_length, int *status )

*  Description:
*     This function gets a username associated with the effective
*     user ID of the current process. If the username cannot be found,
*     a blank string is returned.

*  Arguments:
*     user
*        A buffer in which to return the null-terminated user name.
*     user_length
*        The length of the buffer, including roiom for a terminating null.
*     *status
*        The global status.

*  Notes:
*     -  On a Unix system the translation from effective user ID to
*        username is performed. Since there can be several usernames
*        associated with a user ID, there is no guarantee that the value
*        returned will be unique.
*     -  The Unix function cuserid is no longer in the IEEE 1003.1-1990
*        standard, so an alternative to this routine is used if available.
*        with getpwuid being preferred over getlogin over cuserid.
*     -  If the system call can not obtain a user ID, the value of LOGNAME
*        environment variable is returned as a last resort. And if that is
*        not there we use $USER. Only relevant currently if getenv() function
*        exists.
*     -  If the first attempt to get the username fails, one more attempt
*        is made. This overcomes an occasional (timing?) problem on Linux.
*
*     -  Under MinGW (Windows) the GetUserName function is used.

*  References:
*     -  POSIX standard (1988), section 4.2.4
*     -  POSIX standard (1990), section B.4.2.4

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (EAO)

*  History:
*     3-AUG-2018 (DSB):
*        Original NDF version, a modified copy of psx_cuserid by PMA et al.

*-
*/

/* Local Variables: */
#ifdef USE_CUSERID
   char tempuser[L_cuserid];	 /* Array to hold username */
#else
#ifdef USE_GETUSERNAME
   char tempuser[L_cuserid];	 /* Array to hold username                    */
   DWORD len = L_cuserid;        /* Maximum length of username */
#else
   char tempuser[2];             /* Array to hold defaulted string */
#endif
#endif
   char *p_tempuser = NULL;         /* Return value of calling cuserid            */
#ifdef USE_GETPWUID
   struct passwd * pw;
#endif


/* Check the inherited global status. */
   if( *status != SAI__OK ) return;

/* Lock a mutex to ensure the current thread has exclusive access to the
   static cache. */
   pthread_mutex_lock( &Ndf_cuserid_mutex );

/* Get the username.Only try a system call if user_cache is not defined. */
   if( user_cache_ptr == NULL ) {

/* Use getpwuid(geteuid) if we have it, else use getlogin, else use cuserid.
   Try using LOGNAME environment variable as last resort. */

#ifdef USE_GETPWUID
      pw = getpwuid( geteuid() );
      p_tempuser = pw->pw_name;
#endif

#ifdef USE_GETLOGIN
      p_tempuser = getlogin();
#endif

#ifdef USE_CUSERID

/* Try twice with cuserid since on some linux system uses NIS+ there
   can be a delay which will cause an initial failure */
      p_tempuser = cuserid( tempuser );
      if( p_tempuser == NULL ) {
         p_tempuser = cuserid( tempuser );
      }
#endif

#ifdef USE_GETUSERNAME
/* Use Windows function when operating under MinGW */
      p_tempuser = tempuser;
      if( ! GetUserName( tempuser, &len ) ) {
         p_tempuser = NULL;
      }
#endif

#if HAVE_GETENV
/* Use LOGNAME then USER env var if we do not have a string */
      if(p_tempuser == NULL) {

/* do not use PSX_GETENV since we only want to use a simple command without
   lots of temp char arrays. */
         p_tempuser = getenv( "LOGNAME");
         if(p_tempuser == NULL) {
            p_tempuser = getenv( "USER" );
         }
      }
#endif

/* Last gasp protection from NULL pointer. If we got a NULL value, just
   copy in empty string */
      if(p_tempuser == NULL ) {
         tempuser[0] = '\0';
         p_tempuser = tempuser;
      }

/* Store p_tempuser in the cache and set static pointer */
      strncpy( user_cache, p_tempuser, L_cuserid );
      user_cache_ptr = user_cache;
   }

/* Export the username to the supplied string. */
   ndf1Ccpy( user_cache, user, user_length, status );

/* Unlock the mutex so that other threads can access the static cache. */
   pthread_mutex_unlock( &Ndf_cuserid_mutex );

}
