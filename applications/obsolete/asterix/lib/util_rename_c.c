/*+
 *  Name:
 *     util_rename_c.c
 *
 *  Purpose:
 *     Renames a file.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Invokation :
 *
 *     CALL UTIL_RENAME( FILE1, FILE2, STATUS )
 *
 *  Description:
 *
 *  Authors:
 *
 *     Richard D. Saxton (LTVAD::RDS)
 *
 *  History:
 *
 *     22-Jan-1993 (RDS):
 *        Original version.
 *     14-Dec-1993 (DJA):
 *        Error handling improved.
 *- */

/* Include Statements: */
#include "sae_par.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"          /* Error handling */
#include <errno.h>

#if defined(VAX)
F77_INTEGER_FUNCTION(lib$rename_file)( CHARACTER(arg1), CHARACTER(arg2)
                                       TRAIL(arg1) TRAIL(arg2) );

/* UNIX definitions
 */
#else
#include <errno.h>
#endif

F77_SUBROUTINE(util_rename)( CHARACTER(infil), CHARACTER(outfil),
                             INTEGER(status) TRAIL(infil) TRAIL(outfil) )
  {
  GENPTR_CHARACTER(infil)
  GENPTR_CHARACTER(outfil)
  GENPTR_INTEGER(status)

  char          *instr, *outstr;	/* CNF temporary strings */
  int  		lstat;			/* Status from system routine */

  if ( *status != SAI__OK )		/* Check status on entry */
    return;

#if defined(VAX)
  lstat = F77_EXTERNAL_NAME(lib$rename_file)( CHARACTER_ARG(infil),
                  CHARACTER_ARG(outfil)
                  TRAIL_ARG(infil) TRAIL_ARG(outfil) );

  if ( lstat != 1 )
    {
    ems_syser_c( "REASON", lstat );
    *status = SAI__ERROR;
    }
#else
  instr = cnf_creim( infil, 		/* Import Fortran strings to C */
                     infil_length);
  outstr = cnf_creim( outfil,
                     outfil_length);

  if ( rename(instr,outstr) )		/* Status renaming file */
    {
    ems_syser_c( "REASON", errno );
    *status = SAI__ERROR;
    }

  if ( instr )				/* Free temporary strings */
    cnf_free( instr );
  if ( outstr )
    cnf_free( outstr );
#endif

  if ( *status != SAI__OK ) 		/* Output message if rename failed */
    {
    ems_setc_c( "INP", infil, infil_length );
    ems_setc_c( "OUT", outfil, outfil_length );
    ems_rep_c(" ","Rename of ^INP to ^OUT failed - ^REASON", status);
    }
  }
