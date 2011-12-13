
#include <string.h>

#include "ems_par.h"
#include "ems.h"
#include "hds1.h"
#include "hds.h"
#include "dat_err.h"

/*
*+
*  Name:
*     datRef

*  Purpose:
*     Obtain a reference for an HDS object.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     datRef( const HDSLoc * locator, char * ref, size_t reflen, int * status);
*     CALL DAT_REF( LOC, REF, LREF, STATUS )

*  Description:
*     The routine returns a "reference name" for an HDS object whose
*     locator is supplied. This name identifies the object uniquely by
*     including both the name of the container file and the "path name"
*     which locates the object within this file. If a locator to a cell
*     or a slice is supplied, then subscript information will also be
*     included.  Appropriate syntax is used to represent file names
*     which do not have the standard (.sdf) file extension.

*  Arguments:
*     loc = const HDSLoc * (Given)
*        Locator to the HDS object.
*     ref = char * (Returned)
*        Pointer to character buffer to receive the reference. It must be
*        preallocated by the caller. The reference will be truncated
*        to fit if the buffer is not large enough, and status will be
*        set to DAT__TRUNC.
*     reflen = size_t (Given)
*        Size of buffer pointed to by ref. Size includes space for
*        trailing nul.
*     status = int* (Given and Returned)
*        Global status

*  Machine-specific features used:
*     -  This routine makes assumptions about the form of VMS and Unix
*     file names.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-AUG-1991 (RFWS):
*        Original version, derived from the DAT_MSG routine.
*     16-AUG-1991 (RFWS):
*        Changed to make tests for file extensions case sensitive. Also
*        append an extra '.' to Unix file names when necessary.
*     20-AUG-1991 (RFWS):
*        Removed dependence on PSX_ routines - use global constant
*        instead.
*     25-SEP-1991 (RFWS):
*        Added the LREF argument.
*     1-OCT-1991 (RFWS):
*        Set an initial "safe" value for the LREF argument.
*     03-DEC-2005 (TIMJ):
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

#define MAX_PATH_LEN 512

int datRef( const HDSLoc * locator, char * ref, size_t reflen, int * status ) {

  /*  Local Variables: */
  char buff[MAX_PATH_LEN+1];  /* Buffer */
  char file[MAX_PATH_LEN+1];  /* Container file name */
  char path[MAX_PATH_LEN+1];  /* Object path name */

  char *bra;   /* Position of '(' character */
  char *dot;   /* Position of '.' */
  size_t ncf;   /* Number of characters in filename */
  int i;    /* Loop counter */
  int nlev; /* Object level in HDS */
#if defined( vms )
  char *semi;  /* Position of ';' */
#endif
  char *start; /* Position to start in path name */
  int add_dot; /* Do we need to add a '.' ? */
  int odd;   /* Is the filename odd? */

  /* Terminate the buffer so that it is safe*/
  *ref = '\0';

  /* Check intial global status */
  if ( *status != DAT__OK ) return *status;


  /*  Obtain the data object path and container file name. */
  /*  Lie about the size of the supplied buffer to account for quotes
      and dots that may be added */
  hdsTrace( locator, &nlev, path, file, status, MAX_PATH_LEN-3, MAX_PATH_LEN-1 );

  if ( *status == DAT__OK ) {

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
      if ( strcmp( &file[ncf-DAT__SZFLX], DAT__FLEXT) == 0) {
	odd = 0;
      } else {
	odd = 1;
      }
    }

    /*  If the file name is odd, then we must also decide whether to append
     *  a '.' to the end of it. This is done to counteract the removal of a
     *  terminating '.' which HDS performs on all Unix file names (to permit
     *  the creation of files without a '.' in their names if required).
     *  Initially assume an extra '.' is needed.
     */

    if (odd) {

      add_dot = 1;

      /*  If the file name already ends with a '.'. then another '.' will be
       *  needed to protect it.  Otherwise, search backwards through the final
       *  field of the file name (stopping when a '/' is encountered) to see
       *  whether there is already a '.' present.
       */

      if ( file[ncf-1] != '.' ) {

	/* search back through the string */
	for ( i = 1; i <= ncf ; i++ ) {
	  if ( file[ncf-i] == '/' ) break;


	  /*  If a '.' is present, then note that another one need not be added
	   *  (otherwise one must be added to prevent the default ".sdf" extension
	   *  being appended if HDS re-opens the file using this name).
	   */

	  if ( file[ncf-i] == '.' ) {
	    add_dot = 0;
	    break;
	  }
	}


	/*  If an extra '.' is needed, then append it to the file name (note
	 *  that an extra character is reserved in FILE for this purpose).
	 */
	if (add_dot) {
	  strcat( file, "." );
	}

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

    /*  If the length of the reference name exceeded the length of the output
     *  argument, then append an ellipsis.
     */

    if ( strlen(buff) > reflen -1 ) {
      strncpy( ref, buff, reflen - 4 );
      ref[reflen-4] = '\0';
      strcat(ref, "xyz");

      /* Report an error showing the truncated character string */
      *status = DAT__TRUNC;

      emsSetc( "STRING", ref );
      emsRep( "DAT_REF_1",
	      "Character string truncated: '^STRING'.",
	      status );
      emsRep( "DAT_REF_2",
	      "Output character variable is too short "
	      "to accommodate the returned result.",
	      status );

    } else {
      strcpy( ref, buff );
    }

  }

  /* If an error occurred report contextual information */
  if ( *status != DAT__OK ) {
    emsRep( "DAT_REF_ERR",
	    "DAT_REF: Error obtaining a reference name "
	    "for an HDS object.", status );
  }

  return *status;
}
