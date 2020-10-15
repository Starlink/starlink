/*
*+
*  Name:
*     smf_smfFile_msg

*  Purpose:
*     Convert a smfFile to a message token

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_smfFile_msg( const smfFile * file, const char token[],
*                      int strip, const char fallback[] );

*  Arguments:
*     file = const smfFile * (Given)
*        smfFile to be read.
*     token = const char [] (Given)
*        Name of message token.
*     strip = int (Given)
*        If true path information will be stripped.
*     fallback = const char [] (Given)
*        Default value for token if a filename can not be obtained.
*        Token will not be set if this is a null pointer.

*  Description:
*     Store the filename associated with a smfFile into a message token.
*     Preference is given to the "name" component of the struct. If that is
*     empty the NDF identifier will be used. Can optionally strip path information.

*  Notes:
*     Not thread-safe if the ndfMsg path is used.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-03-16 (TIMJ):
*        Initial version.
*     2011-02-17 (TIMJ):
*        Should work if status is bad
*     2011-03-04 (TIMJ):
*        Set new error level after the code that might return early.
*     2011-03-11 (TIMJ):
*        More fixes now that status is not used. No longer use a
*        new ERR context regardless and only use status in one call.

*  Copyright:
*     Copyright (C) 2010,2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
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

*  Bugs:
*     {note_any_bugs_here}
*-
*/


#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "ndf.h"
#include "mers.h"
#include "sae_par.h"
#include "msg_par.h"
#include "star/util.h"

#include "smf.h"
#include "smurf_par.h"

void
smf_smfFile_msg( const smfFile * file, const char token[],
                 int strip, const char fallback[] ) {

  int have_set = 0;         /* have we set the token yet? */
  char path[GRP__SZNAM+1];  /* Full path information */
  int status = SAI__OK;

  if (!file) {
    if (fallback) msgSetc( token, fallback );
    return;
  }

  path[0] = '\0';

  if (file->name && strlen(file->name) > 0 ) {
    if (strip) {
      star_strellcpy( path, file->name, sizeof(path) );
    } else {
      /* set directly */
      msgSetc( token, file->name );
      have_set = 1;
    }
  } else if ( file->ndfid != NDF__NOID ) {
    if (strip) {
      int oplen = 0;
      /* we need to get the value from the NDF subsystem
         and we need to do it without clearing tokens */

      errMark();
      ndfMsg( "LOCALTOK", file->ndfid );
      msgLoad( "", "^LOCALTOK", path, sizeof(path), &oplen, &status );
      if (status != SAI__OK) errAnnul(&status);
      errRlse();
    } else {
      /* set directly */
      ndfMsg( token, file->ndfid );
      have_set = 1;
    }
  } else {
    msgSetc( token, fallback );
  }

  /* if "path" has stuff in it we are meant to strip it and we have not
     set the token yet */
  if (strlen(path) > 0 ) {
    dim_t length = strlen(path);
    char *endp = &path[length-1];

    /* Do not use the POSIX basename() function here because it does not
       seem to be thread safe (but then again ndfMsg is not thread safe above).
       Could do with a one_basename based on the GNU basename implementation. Does
       not handle all the cases handled by the generic routines. Very naive. */
    while ( *endp != '/' && endp != path) endp--;

    /* gone one too far */
    if (endp != path) endp++;

    /* set the token */
    msgSetc( token, endp);
    have_set = 1;

  }

  if (!have_set) {
    msgSetc(token, fallback );
  }

  return;

}
