#include "sae_par.h"
#include "ast.h"
#include "ast_err.h"
#include "mers.h"
#include "prm_par.h"
#include "cupid.h"

double cupidConfigD( AstKeyMap *config, const char *name, double def,
                     int *status ){
/*
*+
*  Name:
*     cupidConfigD

*  Purpose:
*     Get the value of a configuration parameter.

*  Language:
*     Starlink C

*  Synopsis:
*     double cupidConfigD( AstKeyMap *config, const char *name, double def,
*        int *status )

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
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The required value. The supplied default value is returned if an
*     error occurs.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-OCT-2005 (DSB):
*        Original version.
*     26-MAR-2008 (DSB):
*        Re-report a more friendly message iof the supplied text could
*        not be interpreted as a numerical value.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstObject *sconfig; /* Object pointer obtained from KeyMap */
   const char *text;   /* Pointer to uninterprted text value */
   double ret;         /* The returned value */

/* Initialise */
   ret = def;

/* Abort if an error has already occurred, or if no KeyMap was supplied. */
   if( *status != SAI__OK || !config ) return ret;

/* Attempt to extract the named value from the supplied KeyMap. */
   if( !astMapGet0D( config, name, &ret ) ) {

/* If the value was not found in the KeyMap, see if the KeyMap contains a
   secondary KeyMap. */
      if( astMapGet0A( config, CUPID__CONFIG, &sconfig ) ) {

/* If it does, see if the secondary KayMap contains a value for the named
   entry. If it does, remove the value from the KeyMap so it does not
   appear in the CUPID NDF extension. */
         if( astMapGet0D( (AstKeyMap *) sconfig, name, &ret ) ) {
            astMapRemove(  (AstKeyMap *) sconfig, name );

/* If the text supplied by the user could not be interpreted as a
   floating point value, re-report the error. */
         } else if( *status == AST__MPGER ) {
            ret = def;
            errAnnul( status );
            if( astMapGet0C( config, name, &text ) ) {
               *status = SAI__ERROR;
               msgSetc( "T", text );
               msgSetc( "N", name );
               errRep( "", "Illegal value \"^T\" supplied for configuration "
                       "parameter ^N.", status );
            }

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
      if( ret != VAL__BADD ) astMapPut0D( config, name, ret, NULL );

/* If the text supplied by the user could not be interpreted as a
   floating point value, re-report the error. */
   } else if( *status == AST__MPGER ) {
      ret = def;
      errAnnul( status );
      if( astMapGet0C( config, name, &text ) ) {
         *status = SAI__ERROR;
         msgSetc( "T", text );
         msgSetc( "N", name );
         errRep( "", "Illegal value \"^T\" supplied for configuration "
                 "parameter ^N.", status );
      }
   }

/* Return the result. */
   return ret;
}
