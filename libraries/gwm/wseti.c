#include <X11/Xlib.h>
#include <ctype.h>
#include <string.h>
#include "sae_par.h"
#include "gwm_err.h"
#include "gwm_for.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"

/******************************************************************************/

F77_SUBROUTINE(gwm_wseti) ( CHARACTER(option), INTEGER(value),
                            INTEGER(status) TRAIL(option) )

/*
*+
*  Name :
*     GWM_WSETI
*
*  Purpose :
*     Set an integer window option
*
*  Language :
*     C
*
*  Invocation :
*     CALL GWM_WSETI( OPTION, VALUE, STATUS )
*
*  Description :
*     The window options are used to control the characteristics of
*     the GWM window and to override the default values. These must
*     be set before the window is created with GWM_CRWIN. The
*     'COLOURS' option is an example of an integer option.
*
*  Arguments :
*     OPTION = CHARACTER * ( * ) (Given)
*        The option name.
*     VALUE = INTEGER (Given)
*        The option value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
*     11-OCT-1991 (NE):
*        Orignal version
*      7-NOV-1991 (DLT):
*        global variable definitions moved to function body
*     20-OCT-1993 (DLT):
*        eliminate assignment of pointers to int
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

{
#include "gwm_global.h"

GENPTR_CHARACTER(option)
GENPTR_INTEGER(value)
GENPTR_INTEGER(status)

/* Local variables */
int i, noption;
char *loption;

/* Check status on entry */
if ( *status != SAI__OK ) return;

/* Copy the FORTRAN string to a local C string */
loption = cnf_creim( option, option_length );

/* Convert the string to upper case */
for ( i = 0; i < option_length; i++ )
   loption[i] = (char)toupper( loption[i] );

/* Compare the string to the defined option names */
noption = -1;
for ( i = 0; i < wc_nchars; i++ )
   {
   if ( strcmp( loption, GWM_wc.names[i] ) == 0 )
      {
      noption = i;
      break;
      }
   }

/* Check that the option was recognised */
if ( noption == -1 )
   {
   *status = GWM__INOPT;
   ems_setc_c( "TOKEN", loption, option_length );
   ems_rep_c( "GWM_WSETL_INOPT", "Invalid integer option : ^TOKEN", status );
   return;
   }

/* Check that the type is correct */
if ( strcmp( "I", GWM_wc.types[noption] ) != 0 )
   {
   *status = GWM__INOPT;
   ems_setc_c( "TOKEN", loption, option_length );
   ems_rep_c( "GWM_WSETL_INOPT", "Invalid integer option : ^TOKEN", status );
   return;
   }

/* Store the value in the structure */
GWM_wc.yesno[noption] = 1;
GWM_wc.values[noption].ival = *value;
/* Free the CNF resources */
cnf_free( loption );

return;
}
