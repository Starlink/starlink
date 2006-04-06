#include "sae_par.h"
#include "f77.h"                 
#include "mers.h"
#include "cupid.h"
#include <string.h>

/* Declare global variables used throughout cupid */
/* ---------------------------------------------- */

/* A pointer to the global status variable.*/
int *cupid_global_status;

/* PixelSet cache used by the ClumpFind algorithm. */
CupidPixelSet **cupid_ps_cache = NULL;
int cupid_ps_cache_size = 0;


extern F77_SUBROUTINE(task_get_name)( CHARACTER(name), INTEGER(status) TRAIL(name) );


void cupid_mon( int *adam_status ) {
/*
*+
*  Name:
*     cupid_mon

*  Purpose:
*     Top-level CUPID function for A-task monolith on UNIX.

*  Type of Module:
*     ADAM A-task

*  Synopsis:
*     void cupid_mon( int *adam_status );

*  Parameters:
*     adam_status 
*        Pointer to the global status variable used by the ADAM fixed part.

*  Description:
*     This is the top-level A-task monolith function for the CUPID
*     suite of A-tasks.  Each CUPID command is an alias to a softlink
*     that points to this monolith.  The chosen command is obtained
*     from the ADAM routine TASK_GET_NAME.  The command may be specified
*     from the shell or ICL.  Given the command, the requested A-task
*     is called after a successful matching of the input string with a
*     valid task name.  If there is no match, an error report is made.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-2005 (DSB):
*        Original version.

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local variables: */
   DECLARE_INTEGER(fstatus);      /* Fortran status variable */
   DECLARE_CHARACTER_DYN(fname);  /* Fortran character variable to hold name */
   char name[16];                 /* C character variable to hold name */
   int ast_caching;               /* Initial value of AST MemoryCaching tuning parameter */

/* Check the inherited status. */
   if( *adam_status != SAI__OK ) return;

/* Obtain the command from the environment.  This returns uppercase names. */
   F77_EXPORT_INTEGER( SAI__OK, fstatus );
   F77_CREATE_CHARACTER( fname, 15 );
   F77_CALL(task_get_name) ( CHARACTER_ARG(fname), INTEGER_ARG(&fstatus)
                             TRAIL_ARG(fname) );
   cnfImprt( fname, fname_length, name );
   F77_FREE_CHARACTER(fname);
   F77_IMPORT_INTEGER( &fstatus, adam_status );

/* Store an external pointer to the adam status variable so that other CUPID 
   functions can access it without needing it to be passed as a function 
   parameter. */
   cupid_global_status = adam_status;

/* Make AST use the same variable for its inherited status. */
   astWatch( adam_status );

/* Tell AST to re-cycle memory when possible. */
   ast_caching = astTune( "MemoryCaching", 1 );

/* Check the string against valid A-task names---if matched then call
   the relevant A-task. Since these functions do not have an expliciti
   "status" parameter, they cannot be built as stand-alone atasks using
   "alink". */

/* Identifies emission clumps within a 2- or 3D NDF. */
   if( !strcmp( name, "FINDCLUMPS" ) ) {
      findclumps();

/* Give help on CUPID commands. */
   } else if( !strcmp( name, "CUPIDHELP" ) ) {
      cupidhelp();

/* Create simulated data containing clumps and noise. */
   } else if( !strcmp( name, "MAKECLUMPS" ) ) {
      makeclumps();

/* Create a signal-to-noise NDF. */
   } else if( !strcmp( name, "MAKESNR" ) ) {
      makesnr();




/* Add new commands here... */




/* Report an error if the command name is not recognised. */
   } else if( *adam_status == SAI__OK ) {
      *adam_status = SAI__ERROR;
      errRep( "CUPID_MON_NOCOM", "CUPID: No such command ^CMD.", adam_status );
   }

/* Re-instate the original value of the AST ObjectCaching tuning
   parameter. */
   astTune( "MemoryCaching", ast_caching );

/* Make AST use its own internal variable for its inherited status. */
   astWatch( NULL );

/* Clear out any remaining memory allocated by AST and report
   unintentional leaks. */
   astFlushMemory( 1 );

}


