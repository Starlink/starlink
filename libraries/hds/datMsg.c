
#include <string.h>

#include "ems_par.h"
#include "ems.h"
#include "hds1.h"
#include "hds.h"

/*
*+
*  Name:
*     datMsg

*  Purpose:
*     Assign the name of an HDS object to a message token.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     datMsg( const char * token, const HDSLoc * loc );
*     CALL DAT_MSG( TOKEN, LOC )

*  Description:
*     The routine assigns the full name (including the file name) of an
*     HDS object to a message token for use with the ERR_ and MSG_
*     routines (SUN/104) or with the EMS_ routines (SSN/4). Appropriate
*     syntax is used to represent file names which do not have the
*     standard (.sdf) file extension.

*  Arguments:
*     token = const char * (Given)
*        Name of the message token.
*     loc = const HDSLoc * (Given)
*        Locator to the HDS object.

*  Notes:
*     -  This routine has no STATUS argument and does not perform
*     normal error checking. If it should fail, then no value will be
*     assigned to the message token and this will be apparent in the
*     final message.

*  Machine-specific features used:
*     -  This routine makes assumptions about the form of VMS and Unix
*     file names.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1990 (RFWS):
*        Original version.
*     26-NOV-1990 (RFWS):
*        Moved the call to EMS_SETC outside the local error context to
*        prevent the message token value from being lost.
*     2-JAN-1991 (RFWS):
*        Fixed problem with undefined path name length.
*     7-AUG-1991 (RFWS):
*        Renamed from the original NDF routine to become an HDS (DAT)
*        routine. Also added handling of Unix file names.
*     8-AUG-1991 (RFWS):
*        Allow subscripts on top-level objects.
*     9-AUG-1991 (RFWS):
*        Modified the handling of the error flag.
*     9-AUG-1991 (RFWS):
*        Modified to call EMS_ instead of MSG_.
*     16-AUG-1991 (RFWS):
*        Changed to make the tests for file extensions case sensitive.
*     20-AUG-1991 (RFWS):
*        Removed dependence on PSX_ routines - use global constant
*        instead.
*     24-SEP-1991 (RFWS):
*        Changed to call EMS_ routines instead of ERR_ routines.
*     01-DEC-2005 (TIMJ):
*        Rewrite in C.
*     28-DEC-2005 (TIMJ):
*        Use DAT__FLEXT instead of hard-coded ".sdf"
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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

*  Bugs:
*     {note_any_bugs_here}

*-
*/

void datMsg( const char * token, const HDSLoc * loc ) {

  /*  Local Variables: */
  char buff[EMS__SZMSG+1];  /* Buffer */
  char file[EMS__SZMSG+1];  /* Container file name */
  char path[EMS__SZMSG+1];  /* Object path name */

  char *bra;   /* Position of '(' character */
  char *dot;   /* Position of '.' */
  size_t ncf;   /* Number of characters in filename */
  int nlev; /* Object level in HDS */
#if defined( vms )
  char *semi;  /* Position of ';' */
#endif
  char *start; /* Position to start in path name */
  int status = DAT__OK; /* local status variable */
  int odd;   /* Is the filename odd? */
  int ok;    /* No error occurred? */

  /*  Mark the error stack */
  emsMark();

  /*  Obtain the data object path and container file name. */
  hdsTrace( loc, &nlev, path, file, &status, sizeof(path), sizeof(file) );

  if ( status == DAT__OK ) {

    /*  If necessary, handle VAX/VMS file names.  Locate the semicolon which
     *  delimits the version number in the file name. */

#if defined( vms )

    semi = strchr( file, ';' );

    /*  If found, then select the file name prior to it by replacing
	it with a nul. */
    if (semi != NULL ) {
      *semi = '\0';
    }

#endif

    /* Find the filename length */
    ncf = strlen( file );

    /*  See if the file is "odd". Check to see if it has the default file
     *  extension of '.SDF' with at least one character preceding it. */

    odd = 1;
    if ( ncf >= 5 ) {
      if ( strcmp( &file[ncf-DAT__SZFLX], DAT__FLEXT ) == 0) {
	odd = 0;
      } else {
	odd = 1;
      }
    }

    /*  If the file name is not odd, then omit the file extension. */
    if (!odd) file[ncf-4] = '\0';

    /*  Enter the file name into the buffer, surrounding it in quotes if it
     *  is odd. */

    *buff = '\0';
    if (odd) strcat(buff, "\"" );
    strcat( buff, file );
    if (odd) strcat(buff, "\"" );

    /*  If the object is not a top-level object, then find the position of
     *  the first '.' in its pathname, which marks the start of the first
     *  component name. */

    if (nlev > 1 ) {

      dot = strchr( path, '.' );

      /*  If successful, see if the '.' is preceded by a '(' indicating that
       *  the top-level object is subscripted. Derive the starting position in
       *  the path name so that the subscript is used if present. */

      if (dot != NULL) {

	bra = strchr( path, '(' );
	if (bra != NULL && bra < dot) {
	  start = bra;
	} else {
	  start = dot;
	}

	/*  Add the required part of the path name to the buffer. */
	strcat( buff, start );

      }

    } else {
      /*  If the object is a top-level object, then see if it is subscripted.
       *  If so, then add the subscript to the buffer. */

      start = strchr( path, '(' );

      if ( start != NULL ) {
	strcat( buff, start );
      }

    }

  }

  /*  Note if any error has occurred. */
  ok = ( status == DAT__OK  ? 1 : 0 );

  /*  If an error occurred, then annul it. Release the error stack. */
  if ( status != DAT__OK ) emsAnnul( &status );
  emsRlse();

  /*  If no error occurred, then assign the resulting buffer contents to
   *  the message token. */
  if (ok) emsSetc( token, buff );

}
