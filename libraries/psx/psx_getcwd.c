/*
*+
*  Name:
*     PSX_GETCWD

*  Purpose:
*     Obtain current working directory

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL PSX_GETCWD( CWD, STATUS )

*  Description:
*     Provides a Fortran interface to obtain the current working directory.
*     Some Fortran implementations provide a GETCWD builtin but this is provided
*     for compatibility.

*  Arguments:
*     CWD = CHARACTER * ( * ) (Returned)
*        On exit contains the name of the current directory. Status will be set to
*        PSX__TRUNC if the string is too short to hold the directory.
*     STATUS = INTEGER (Given & Returned)
*        The global status.

*  References:
*     - POSIX Standard ISO/IEC 9945-1:1990

*  Copyright:
*     Copyright (C) Particle Physics and Astronomy Research Council 2006

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Internally, may use getwd or getcwd.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  History:
*     15-FEB-2006 (TIMJ):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* System include files */
#if HAVE_UNISTD_H
#  include <unistd.h>
#endif

/* Starlink includes */
#include "f77.h"
#include "ems.h"
#include "sae_par.h"
#include "psx_err.h"
#include "psx1.h"

/* Decide which implementation to use */
#if HAVE_GETCWD
#  define USE_GETCWD 1
#elif HAVE_GETWD
#  define USE_GETWD 1
#else
   error Unable to find either getwd or getcwd
#endif

#if USE_GETWD
#  include <sys/param.h>
#endif

#include <string.h>
#include <errno.h>

F77_SUBROUTINE(psx_getcwd)( CHARACTER(CWD), INTEGER(STATUS) TRAIL(CWD) )
{
  GENPTR_CHARACTER(CWD)
  GENPTR_INTEGER(STATUS)

  int  err;        /* Local copy of errno */
  char * result;   /* Result of getcwd/getwd */
#if USE_GETCWD
  size_t size;     /* Size of string for getcwd */
  char * tempbuf = NULL;      /* Local copy of result */
#else
  char tempbuf[MAXPATHLEN];   /* somewhere to store the result from getwd */
#endif

  if (*STATUS != SAI__OK) {
    cnfExprt( " ", CWD, CWD_length ); /* initialise result */
    return;
  }

#if USE_GETCWD

  /* We need to create a temporary string of length CWD_length + 1
     rather than using the supplied buffer directly. If we did use
     the buffer directly we would never be able to completely fill it.
  */
  size = CWD_length + 1;
  tempbuf = cnfCreat( size );
  if (!tempbuf) {
    err = ENOMEM;
    goto ERROR;
  }

  /* get the current working directory */
  result = getcwd( tempbuf, size );

#else  /* USE_GETWD */
  printf("Using getwd\n");
  result = getwd( tempbuf );

#endif

  if (!result) {
    err = errno;
    goto ERROR;
  }

  /* check for truncation */
  if (strlen(result) > (size_t)CWD_length) {
    err = ERANGE;
    goto ERROR;
  }

  /* if everything is okay copy the result to the waiting buffer */
  cnfExprt( result, CWD, CWD_length );

#if USE_GETCWD
  /* Free result if we malloced it */
  cnfFree(tempbuf);
#endif

  return;

 ERROR:
  /* fill output string with blank */
  cnfExprt( " ", CWD, CWD_length );

  /* Report the error */
  emsSyser( "REASON", err );
  if (err == ERANGE) {
    *STATUS = PSX__TRUNC;
  } else if (err == ENOMEM) {
    *STATUS = PSX__NOMEM;
  } else {
    *STATUS = PSX__ERRNO;
  }
  psx1_rep_c( "PSX_GETCWD_ERR", "Error obtaining current working directory - ^REASON", STATUS);
  return;
}
