/*+
 *  Name:
 *     util_delete_c.c
 *
 *  Purpose:
 *     Deletes one or more files from the file system.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Description:
 *
 *
 *  Invokation:
 *
 *     CALL UTIL_DELETE( FILENAME, STATUS )
 *
 *  Authors:
 *
 *     David J. Allan (ROSAT,BHVAD::DJA)
 *
 *  History:
 *
 *     26-Nov-1993 (DJA):
 *        Original version.
 *- */

/* Include Statements: */
#include "sae_par.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"          /* Error handling */

/* VMS definitions
 */
#if defined(VAX)
F77_INTEGER_FUNCTION(lib$delete_file)( CHARACTER(arg) TRAIL(arg) );

/* UNIX definitions
 */
#else
#include <errno.h>
#include <string.h>

#endif


F77_SUBROUTINE(util_delete)( CHARACTER(spec), INTEGER(status) TRAIL(spec) )
  {
  GENPTR_CHARACTER(spec)
  GENPTR_INTEGER(status)

  char		*cspec;			/* C string holding file to delete */
  int		lstat;			/* Delete status */

  if ( *status != SAI__OK )		/* Check status on entry */
    return;

#if defined(VAX)
  lstat = F77_EXTERNAL_NAME(lib$delete_file)( CHARACTER_ARG(spec) TRAIL_ARG(spec) );
  if ( lstat != 1 )
    {
    ems_syser_c( "REASON", lstat );
    *status = SAI__ERROR;
    }
#else
  cspec = cnf_creim( spec, 		/* Import Fortran string to C */
            spec_length );

  if ( remove(cspec) ) 			/* Try to remove file */
    {
    ems_syser_c( "REASON", errno );
    *status = SAI__ERROR;
    }

  cnf_free( cspec );			/* Free temporary sting */
#endif

  if (*status != SAI__OK )		/* Report errors */
    {
    ems_setc_c( "FILE", spec, spec_length );
    ems_rep_c( " ", "Error deleting the file ^FILE/ - ^REASON", status );
    ems_rep_c( " ", "...from UTIL_DELETE", status );
    }
  }
