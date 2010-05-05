#include "star/grp.h"
#include "ast.h"
#include "kaplibs.h"
#include "sae_par.h"
#include "par_err.h"
#include "mers.h"

AstKeyMap *kpg1Config( const char *param, const char *def, int *status ){
/*
*+
*  Name:
*     kpg1Config

*  Purpose:
*     Create an AST KeyMap holding a set of configuration parameter values.

*  Language:
*     C.

*  Invocation:
*     AstKeyMap *kpg1Config( const char *param, const char *def, int *status )

*  Description:
*     This function first creates a KeyMap by reading the values from a specified
*     text file (argument "def"). This text files specifies the complete list
*     of all allowed config parameters and their default values. The KeyMap is then
*     locked by setting its MapLocked attribute to a true value. A GRP group is then
*     obtained from the environment using the specified parameter (argument "param").
*     The config settings thus obtained are stored in the KeyMap. Since the KeyMap
*     has been locked, an error will be reported if the user-supplied group refers to
*     any config parameters that were not read earlier from the default file.
*
*     The beneifts of using this function are that 1) the user gets to know if they
*     mis-spell a config parameter name, and 2) the default parameter values can be
*     defined in a single place, rather than hard-wiring them into application code
*     at each place where the coinfig parameter is used.

*  Arguments:
*     paramc = const char * (Given)
*        The name of the environment parameter to use.
*     def = const char * (Given)
*        The path to a file containing the default value for every allowaed config
*        parameter. For instance, "$SMURF_DIR/dimmconfig.def".
*     status = int * (Given & Returned)
*        The inherited status.

*  Notes:
*     - The KeyError attribute is set non-zero in the returned KeyMap so that an error
*     will be reported by astMapGet<X> if the requested key does not exist in the KeyMap.

*  Returned Value:
*     A pointer to the AST KeyMap, or NULL if an error occurrs.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry
*     TIMJ: Tim Jenness
*     {enter_new_authors_here}

*  History:
*     28-APR-2010 (DSB):
*        Original version.
*     4-MAY-2010 (DSB):
*        Set KeyError attribute non-zero in the returned KeyMap.
*     4-MAY-2010 (TIMJ):
*        Merge defaults with supplied values in this routine so that
*        we can correctly handle defaulting using <def>.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Varianles: */
   AstKeyMap *external = NULL;  /* Keymap of externally supplied values */
   AstKeyMap *result = NULL;    /* Returned KeyMap */
   char *value;                 /* Pointer to GRP element buffer */
   char buffer[ GRP__SZNAM ];   /* Buffer for GRP element */
   int added;                   /* Number of names added to group */
   int flag;                    /* Flag */
   Grp *grp;                    /* Group to hold config values */
   size_t size;                 /* Size of group */

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Attempt to read the specified defaults file into a GRP group. */
   grp = grpNew( "GRP", status );
   sprintf( buffer, "^%s", def );
   grpGrpex( buffer, NULL, grp, &size, &added, &flag, status );

/* Create a KeyMap from this group. */
   kpg1Kymap( grp, &result, status );

/* Delete the group. */
   grpDelet( &grp, status );

/* Read a group of configuration setting from the specified environment parameter. */
   kpg1Gtgrp( param, &grp, &size, status );

/* Lock the KeyMap so that an error will be reported if an attempt
   is made to add any new entries to it. */
   astSetI( result, "MapLocked", 1 );

/* If no group was supplied, just annul any PAR__NULL error. */
   if( *status == PAR__NULL || size == 0 ) {
      if( *status != SAI__OK ) errAnnul( status );

/* If a group was supplied, see if it consists of the single value "def".
   If so, we will leave the KeyMap unchanged. */
   } else {
      value = buffer;
      if( size == 1 ) {
         grpGet( grp, 1, 1, &value, sizeof(buffer), status );
      } else {
         strcpy( value, " " );
      }

/* Otherwise, store the configuration settings in the KeyMap. */
      if( ! astChrMatch( value, "DEF" ) ) kpg1Kymap( grp, &external, status );

/* Copy the overrides into the default. An error will be reported if a config
   parameter is specified that is not already present in the KeyMap. */
      astMapCopy( result, external );

/* Delete the external KeyMap */
      external = astAnnul( external );

   }

/* Ensure the KeyError attribute is non-zero in the returned KeyMap so that an
   error is reported by astMapGet<X> if the KeyMap does not contain the requested
   entry. */
   if( result ) astSetI( result, "KeyError", 1 );

/* Delete the group, if any. */
   if( grp ) grpDelet( &grp, status );

/* Return the KeyMap. */
   return result;
}
