#include "sae_par.h"
#include "ast.h"
#include "cupid.h"

double cupidConfigD( AstKeyMap *config, const char *name, double def ){
/*
*  Name:
*     cupidConfigD

*  Purpose:
*     Get the value of a configuration parameter.

*  Synopsis:
*     double cupidConfigD( AstKeyMap *config, const char *name, double def );

*  Description:
*     This function returns a named value from the supplied KeyMap. If
*     the KeyMap does not contaain the named value, the supplied default 
*     value is returned, and is also stored in the KeyMap.

*  Parameters:
*     config
*        An AST KeyMap holding the configuration parameters.
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
   double ret;       /* The returned value */

/* Initialise */
   ret = def;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Attempt to extract the named value from the supplied KeyMap. */
   if( !astMapGet0D( config, name, &ret ) ) {

/* If the value was not found in the KeyMap, return the default value and
   also store it in the KeyMap. */
      ret = def;
      astMapPut0D( config, name, def, NULL );
   }

/* Return the result. */
   return ret;
}
