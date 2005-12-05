#include "sae_par.h"
#include "ast.h"
#include "prm_par.h"
#include "cupid.h"

int cupidConfigI( AstKeyMap *config, const char *name, int def ){
/*
*  Name:
*     cupidConfigI

*  Purpose:
*     Get the value of a configuration parameter.

*  Synopsis:
*     int cupidConfigI( AstKeyMap *config, const char *name, int def );

*  Description:
*     This function returns a named value from the supplied KeyMap. If
*     the KeyMap does not contaain the named value, the supplied default 
*     value is returned, and is also stored in the KeyMap.

*  Parameters:
*     config
*        An AST KeyMap holding the configuration parameters. If NULL is
*        supplied, the default value is returned without error.
*     name
*        The name of the value to extract from the KeyMap.
*     def
*        The default value to use.

*  Returned Value:
*     The required value. The supplied default value is returned if an
*     error occurs.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-OCT-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int ret;       /* The returned value */

/* Initialise */
   ret = def;

/* Abort if an error has already occurred, or if no KeyMap was supplied. */
   if( *status != SAI__OK || !config ) return ret;

/* Attempt to extract the named value from the supplied KeyMap. */
   if( !astMapGet0I( config, name, &ret ) ) {

/* If the value was not found in the KeyMap, return the default value and
   also store it in the KeyMap, if it is good. */
      ret = def;
      if( def != VAL__BADI ) astMapPut0I( config, name, def, NULL );
   }

/* Return the result. */
   return ret;
}
