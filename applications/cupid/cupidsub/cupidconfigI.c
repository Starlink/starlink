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
*     the KeyMap does not contain the named value, an attempt is made to
*     obtain a value from a secondary KeyMap which should be stored
*     within the supplied KeyMap, using a key equal to the constant
*     string CUPID__CONFIG. If the secondary KeyMap does not contain a
*     value, then the supplied default value is returned. In either case,
*     the returned value is stored in the KeyMap.

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
   AstObject *sconfig; /* Object pointer obtained from KeyMap */
   int ret;            /* The returned value */

/* Initialise */
   ret = def;

/* Abort if an error has already occurred, or if no KeyMap was supplied. */
   if( *status != SAI__OK || !config ) return ret;

/* Attempt to extract the named value from the supplied KeyMap. */
   if( !astMapGet0I( config, name, &ret ) ) {

/* If the value was not found in the KeyMap, see if the KeyMap contains a
   secondary KeyMap. */
      if( astMapGet0A( config, CUPID__CONFIG, &sconfig ) ) {

/* If it does, see if the secondary KayMap contains a value for the named
   entry. If it does, remove the value from the KeyMap so it does not
   appear in the CUPID NDF extension. */
         if( astMapGet0I( (AstKeyMap *) sconfig, name, &ret ) ) {
            astMapRemove(  (AstKeyMap *) sconfig, name );

/* If the value was not found in either KeyMap, return the default value. */
         } else {
            ret = def;
         }

/* Free the pointer to the secondary KeyMap. */
         sconfig = astAnnul( sconfig );

/* If no secondary KeyMap was found, return the default value. */
      } else {
         ret = def;
      }

/* Store the returned value in the supplied KeyMap if it is good. */
      if( ret != VAL__BADI ) astMapPut0I( config, name, ret, NULL );
   }

/* Return the result. */
   return ret;
}
