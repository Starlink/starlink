#include "sae_par.h"
#include "ast.h"
#include "prm_par.h"
#include "cupid.h"
#include "mers.h"

double cupidConfigRMS( AstKeyMap *config, const char *name, double rms,
                       double def, int *status ){
/*
*+
*  Name:
*     cupidConfigRMS

*  Purpose:
*     Get the value of a configuration parameter, specified either as an
*     absolute value or as a mutiple of the RMS noise.

*  Language:
*     Starlink C

*  Synopsis:
*     double cupidConfigRMS( AstKeyMap *config, const char *name, double rms,
                             double def, int *status )

*  Description:
*     This function returns a named value from the supplied KeyMap. If
*     the KeyMap does not contain the named value, an attempt is made to
*     obtain a value from a secondary KeyMap which should be stored
*     within the supplied KeyMap, using a key equal to the constant
*     string CUPID__CONFIG. If the secondary KeyMap does not contain a
*     value, then the supplied default value is returned. In either case,
*     the returned value is stored in the KeyMap.
*
*     If the value obtained is a string of the form "[x]*RMS", where
*     "[x]" is a numerical value, then the returned function value is
*     [x]*rms  (where "rms" is the supplied value of the "rms" function
*     parameter).

*  Parameters:
*     config
*        An AST KeyMap holding the configuration parameters. If NULL is
*        supplied, the default value is returned without error.
*     name
*        The name of the value to extract from the KeyMap.
*     rms
*        The rms noise level.
*     def
*        The default absolute value to use.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The required value. The supplied default value is returned if an
*     error occurs.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
*     21-MAR-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstObject *sconfig; /* Object pointer obtained from KeyMap */
   const char *text;   /* Pointer to text string */
   double ret;         /* The returned value */
   int len;            /* Length of text */
   int nc;             /* Number oc characters read from text */

/* Initialise */
   ret = def;
   text = NULL;

/* Abort if an error has already occurred, or if no KeyMap was supplied. */
   if( *status != SAI__OK || !config ) return ret;

/* Attempt to extract the named value (as a string) from the supplied KeyMap. */
   if( !astMapGet0C( config, name, &text ) ) {

/* If the value was not found in the KeyMap, see if the KeyMap contains a
   secondary KeyMap. */
      if( astMapGet0A( config, CUPID__CONFIG, &sconfig ) ) {

/* If it does, see if the secondary KayMap contains a value for the named
   entry. If it does, remove the value from the KeyMap so it does not
   appear in the CUPID NDF extension. */
         if( astMapGet0C( (AstKeyMap *) sconfig, name, &text ) ) {
            astMapRemove(  (AstKeyMap *) sconfig, name );
         }

/* Free the pointer to the secondary KeyMap. */
         sconfig = astAnnul( sconfig );
      }
   }

/* If a text string was obtained, attempt to interpret it. */
   if( text ) {

/* Obtain the length of the text string. */
      len = strlen( text );

/* See if it is a numerical value followed by "*RMS" (case insensitive). If
   so, multiply the supplied RMS value by the numerical value extracted
   from the string. */
      if( nc = 0, 1 == astSscanf( text, " %lg*%*[Rr]%*[Mm]%*[Ss] %n",
                                  &ret, &nc ) && ( nc >= len ) ) {
         ret *= rms;

/* Otherwise, see if the string is just "RMS". If so, return the RMS
   value. */
      } else if( nc = 0, 0 == astSscanf( text, " %*[Rr]%*[Mm]%*[Ss] %n",
                                         &nc ) && ( nc >= len ) ) {
         ret = rms;

/* Otherwise, see if the string is a simple numerical value. If so, return
   it. */
      } else if( nc = 0, 1 == astSscanf( text, " %lg %n", &ret, &nc )
                                         && ( nc >= len ) ) {

/* Otherwise, return the default value. */
      } else {
         if( *status == SAI__OK ) *status = SAI__ERROR;
         msgSetc( "V", text );
         msgSetc( "N", name );
         errRep( "", "The value (^V) of configuration parameter \"^N\""
                 "cannot be used.", status );
         ret = def;
      }
   }

/* Store the returned value in the supplied KeyMap if it is good. */
   if( ret != VAL__BADD ) astMapPut0D( config, name, ret, NULL );

/* Return the result. */
   return ret;
}
