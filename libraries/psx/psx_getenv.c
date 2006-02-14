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
*     Copyright (C) 1991 Science & Engineering Research Council

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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/


/* Global Constants:		.					    */

#include <stdlib.h>		 /* Standard library			    */
#include <string.h>		 /* String handling library		    */
#include "f77.h"		 /* C - Fortran interface		    */
#include "psx_err.h"		 /* PSX error values			    */
#include "psx1.h"		 /* Internal PSX routines		    */
#include "sae_par.h"		 /* ADAM constants			    */

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

   char *temp_name;     	 /* Pointer to local copy of name 	    */
   char *ptr;			 /* Pointer to environment variable	    */
   char *errmsg_p;		 /* Pointer to complete error message	    */
#if defined(vms)
   char errmsg1[] = "There is no translation for logical name or symbol ";
#elif defined(sun)
   char errmsg1[] = "There is no translation for environment variable ";
#else
   char *errmsg1; errmsg1 = "There is no translation for environment variable ";
#endif

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* If this is a VMS system, initialize the VAX C run time library.	    */

#if defined(vms)
   psx1_init_rtl();
#endif

/* Import name into the C string temp_name.                                 */

   temp_name = cnfCreim( name, name_length );
   
/* Get a pointer to the environment variable.				    */

   ptr = getenv( temp_name );

   if( ptr != 0 )
   {

/* Export the translation of the environment variable to the Fortran string */
/* trans.								    */

      cnfExprt( ptr, trans, trans_length );

/* Free the temporary space.						    */

      cnfFree( temp_name );

   }

   else

/* No translation found. Set the status to indicate this, report an error   */
/* and return a blank string.						    */

   {
      cnfExprt( " ", trans, trans_length );
      *status = PSX__NOENV;

/* Build the error message. The space allocated is enough for the beginning */
/* of the message, the name of the environment variable, two quotes and the */
/* trailing null .							    */

      errmsg_p = malloc( strlen( errmsg1 ) + strlen( temp_name ) + 3 );
      strcpy( errmsg_p, errmsg1 );
      strcat( errmsg_p, "\"" );
      strcat( errmsg_p, temp_name );
      strcat( errmsg_p, "\"" );
      psx1_rep_c( "PSX_GETENV_NOENV", errmsg_p, status );
      free( errmsg_p );
      cnfFree( temp_name );
   }


}
