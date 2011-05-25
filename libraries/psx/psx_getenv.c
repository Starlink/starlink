/* Subroutine:  psx_getenv( name, trans, status )
*+
*  Name:
*     PSX_GETENV

*  Purpose:
*     Translate an environment variable

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_GETENV( NAME, TRANS, STATUS )

*  Description:
*     The routine tries to get the translation of the environment
*     variable NAME. If it succeeds, it returns the translation in
*     TRANS. If it fails, it sets STATUS to PSX__NOENV and reports an error.
*
*     If the value of the environment variable does not fit in TRANS
*     it is truncated and STATUS is set to PSX__TRUNC.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the environment variable to be translated.
*     TRANS = CHARACTER * ( * ) (Returned)
*        The translation of the environment variable.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     CALL PSX_GETENV( 'USER', TRANS, STATUS )
*        This will return the value of the environment variable USER,
*        i.e. the username of the current process.

*  External Routines Used:
*     cnf: cnfCreim, cnfExprt, cnfFree

*  References:
*     -  POSIX standard (1988), section 4.6.1
*     -  ANSI C standard (1989), section 4.10.4.4

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-JAN-1991 (PMA):
*        Original version.
*     15-APR-1991 (PMA):
*        Changed calls to ems to calls to psx1.
*      3-MAY-1991 (PMA):
*        Ensure that the output argument is set, even on an error.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     16-AUG-1999 (DLT):
*        Fix memory leaks when no translation exists.
*     23-JUN-2000 (AJC):
*        Tidy refs to CNF routines
*        Remove refs to VMS in prologue
*     19-SEP-2005 (TIMJ):
*        Should use 'free' not 'cnfFree' when freeing string returned
*        by cnfCreim.
*     13-FEB-2006 (TIMJ):
*        Use cnfFree again since this is easier to control when changing
*        malloc library.
*     23-FEB-2006 (TIMJ):
*        Use starMalloc
*     28-JUL-2009 (TIMJ):
*        Report an error if the environment value won't fit in the return buffer.
*     25-MAY-2011 (TIMJ):
*        Simplify error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/


/* Global Constants:		.					    */

#include <stdlib.h>		 /* Standard library			    */
#include <string.h>		 /* String handling library		    */
#include <stdio.h>
#include "f77.h"		 /* C - Fortran interface		    */
#include "psx_err.h"		 /* PSX error values			    */
#include "psx1.h"		 /* Internal PSX routines		    */
#include "sae_par.h"		 /* ADAM constants			    */
#include "star/mem.h"            /* Starlink memory routines                */

F77_SUBROUTINE(psx_getenv)( CHARACTER(name),
                            CHARACTER(trans),
                            INTEGER(status)
                            TRAIL(name)
                            TRAIL(trans)
                          )
{

/* Pointers to Arguments:						    */

   GENPTR_CHARACTER(name)
   GENPTR_CHARACTER(trans)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   char *temp_name = NULL;     	 /* Pointer to local copy of name 	    */
   char *ptr = NULL;		 /* Pointer to environment variable	    */
#if defined(vms)
   char errmsg1[] = "There is no translation for logical name or symbol ";
#else
   const char errmsg1[] = "There is no translation for environment variable ";
#endif

/* Check inherited global status.					    */

   cnfExprt( " ", trans, trans_length ); /* defensive initialisation */
   if( *status != SAI__OK ) return;

/* If this is a VMS system, initialize the VAX C run time library.	    */

#if defined(vms)
   psx1_init_rtl();
#endif

/* Import name into the C string temp_name.                                 */

   temp_name = cnfCreim( name, name_length );

/* Get a pointer to the environment variable.				    */

   if ( temp_name ) ptr = getenv( temp_name );

   if( ptr != 0 )
   {

/* Export the translation of the environment variable to the Fortran string */
/* trans. Checking for truncation. */

      cnfExprt( ptr, trans, trans_length );
      if ( cnfLenc(ptr) > trans_length ) {

          *status = PSX__TRUNC;
          psx1_rep_c( "PSX_GETENV_TRUNC",
                      "Unable to copy environment variable into Fortran buffer. "
                      "Requires %d characters but return string only has %d",
                      status, cnfLenc(ptr), trans_length);

     }

/* Free the temporary space.						    */

      cnfFree( temp_name );

   }

   else

/* No translation found. Set the status to indicate this, report an error   */
/* and return a blank string.						    */

   {
      cnfExprt( " ", trans, trans_length );
      *status = PSX__NOENV;

      psx1_rep_c( "PSX_GETENV_NOENV", "%s \"%s\"",
                  status, errmsg1, temp_name );

      if (temp_name) cnfFree( temp_name );
   }


}
