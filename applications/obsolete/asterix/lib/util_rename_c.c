/*
*+
*  Name:
*     util_rename_c

*  Purpose:
*     Rename a file

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL UTIL_RENAME_C( INFIL, OUTFIL, STATUS )

*  Description:
*     Provides a Fortran interface to rename files. The file with the
*     name specified by the first argument is renamed to the second
*     name.

*  Arguments:
*     INFIL = CHARACTER*(*) (given)
*        The name of the file to rename
*     OUTFIL = CHARACTER*(*) (given)
*        The new name of the file
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  References:
*     util Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/util.html

*  Keywords:
*     package:util, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     22 Jan 1993 (RDS):
*        Original version.
*     14 Dec 1993 (DJA):
*        Error handling improved.
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
F77_SUBROUTINE(util_rename_c)( CHARACTER(infil), CHARACTER(outfil),
                               INTEGER(status) TRAIL(infil) TRAIL(outfil) )
  {
  GENPTR_CHARACTER(infil)
  GENPTR_CHARACTER(outfil)
  GENPTR_INTEGER(status)

  char          *instr, *outstr;	/* CNF temporary strings */
  int  		lstat;			/* Status from system routine */

/* Check inherited global stratus on entry */
  if ( *status != SAI__OK )
    return;

#if defined(VAX)
  lstat = F77_EXTERNAL_NAME(lib$rename_file)( CHARACTER_ARG(infil),
                  CHARACTER_ARG(outfil)
                  TRAIL_ARG(infil) TRAIL_ARG(outfil) );

  if ( lstat != 1 ) {
    ems_syser_c( "REASON", lstat );
    *status = SAI__ERROR;
    }
#else

/* Import Fortran strings to C */
  instr = cnf_creim( infil, infil_length);
  outstr = cnf_creim( outfil, outfil_length);

/* Status renaming file */
  if ( rename(instr,outstr) ) {
    ems_syser_c( "REASON", errno );
    *status = SAI__ERROR;
    }

  if ( instr )				/* Free temporary strings */
    cnf_free( instr );
  if ( outstr )
    cnf_free( outstr );
#endif

/* Output message if rename failed */
  if ( *status != SAI__OK ) {
    ems_setc_c( "INP", infil, infil_length );
    ems_setc_c( "OUT", outfil, outfil_length );
    ems_rep_c(" ","Rename of ^INP to ^OUT failed - ^REASON", status);
    }
  }
