/*
*+
*  Name:
*     PSX_RENAME

*  Purpose:
*     Rename a file

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL PSX_RENAME( INFIL, OUTFIL, STATUS )

*  Description:
*     Provides a Fortran interface to rename files. The file with the
*     name specified by the first argument is renamed to the second
*     name.

*  Arguments:
*     INFIL = CHARACTER*(*) (Given)
*        The name of the file to rename
*     OUTFIL = CHARACTER*(*) (Given)
*        The new name of the file
*     STATUS = INTEGER (Given & Returned)
*        The global status.

*  References:
*     - POSIX Standard

*  Copyright:
*     Copyright (C) University of Birmingham, 1995
*     Copyright (C) Council for the Central Laboratory of the Research Councils 2001
*     Copyright (C) Particle Physics and Astronomy Research Council 2006

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     AJC: Alan J. Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

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
*     22 Jan 1993 (RDS):
*        Original version.
*     14 Dec 1993 (DJA):
*        Error handling improved.
*      5-Dec-2001 (AJC):
*        New form of EMS and CNF routine name
*     14-FEB-2006 (TIMJ):
*        Integrate into PSX.
*     15-FEB-2006 (TIMJ):
*        Set status to PSX__ERRNO rather than generic SAI__ERROR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*
 *  Include files
 */
#include "sae_par.h"
#include "f77.h"
#include "cnf.h"
#include "ems.h"          /* Error handling */
#include "psx_err.h"
#include <stdio.h>
#include <errno.h>

/*
 * Prototype the rename function in VMS
 */
#if defined(VAX)
F77_INTEGER_FUNCTION(lib$rename_file)( CHARACTER(arg1), CHARACTER(arg2)
                                       TRAIL(arg1) TRAIL(arg2) );
#endif



/*
 *  Body of code
 */
F77_SUBROUTINE(psx_rename)( CHARACTER(infil), CHARACTER(outfil),
                             INTEGER(status) TRAIL(infil) TRAIL(outfil) )
  {
  GENPTR_CHARACTER(infil)
  GENPTR_CHARACTER(outfil)
  GENPTR_INTEGER(status)

  char          *instr, *outstr;	/* CNF temporary strings */
#if defined(VAX)
  int  		lstat;			/* Status from system routine */
#endif

/* Check inherited global stratus on entry */
  if ( *status != SAI__OK )
    return;

#if defined(VAX)
  lstat = F77_EXTERNAL_NAME(lib$rename_file)( CHARACTER_ARG(infil),
                  CHARACTER_ARG(outfil)
                  TRAIL_ARG(infil) TRAIL_ARG(outfil) );

  if ( lstat != 1 ) {
    emsSyser( "REASON", lstat );
    *status = SAI__ERROR;
    }
#else

/* Import Fortran strings to C */
  instr = cnfCreim( infil, infil_length);
  outstr = cnfCreim( outfil, outfil_length);

/* Status renaming file */
  if ( rename(instr,outstr) ) {
    emsSyser( "REASON", errno );
    *status = PSX__ERRNO;
    }

  if ( instr )				/* Free temporary strings */
    cnfFree( instr );
  if ( outstr )
    cnfFree( outstr );
#endif

/* Output message if rename failed */
  if ( *status != SAI__OK ) {
    emsSetnc( "INP", infil, infil_length );
    emsSetnc( "OUT", outfil, outfil_length );
    emsRep(" ","Rename of ^INP to ^OUT failed - ^REASON", status);
    }
  }
