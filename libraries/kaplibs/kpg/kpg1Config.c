#include "star/grp.h"
#include "star/ndg.h"
#include "ast.h"
#include "kaplibs.h"
#include "sae_par.h"
#include "par_err.h"
#include "mers.h"

/* internal helper routine */
static void kpg1__process_nesting( AstKeyMap * keymap, AstKeyMap * nested,
                                  int * status );

AstKeyMap *kpg1Config( const char *param, const char *def,
                       AstKeyMap * nested, int *status ){
/*
*+
*  Name:
*     kpg1Config

*  Purpose:
*     Create an AST KeyMap holding a set of configuration parameter values.

*  Language:
*     C.

*  Invocation:
*     AstKeyMap *kpg1Config( const char *param, const char *def,
*                   AstKeyMap * nested, int *status )

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
*     The "nested" keymap can be used to specify any nested keymaps that can be
*     provided by the user but which can only contain values specified in the defaults.
*     If the named items are present in the base keymap they will be copied in to the
*     base if the value in the "nested" keymap is true. If the value is false they will
*     simply be dropped. Both the default keymap and externally supplied overrides are
*     processed in this way before the two are merged. This allows you to have a single
*     config file with "x.a", "y.x.a" and "z.x.a" where the "y.x" will be copied to "x.a"
*     but "z.x" will be dropped without having to explicitly specify default values for
*     all of "z.x" and "y.x".
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
*     nested = AstKeyMap * (Given)
*        If non-NULL, used to determine which nested keys might be in the config
*        and which should be merged with the base keymap. The values in the keymap
*        should be true to indicate merging.
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
*     2010-05-05 (TIMJ):
*        Add "nested" keymap to allow merging.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
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

/* Handle nested entries */
   kpg1__process_nesting( result, nested, status );

/* Lock the KeyMap so that an error will be reported if an attempt
   is made to add any new entries to it. */
   astSetI( result, "MapLocked", 1 );

/* Read a group of configuration setting from the specified environment parameter. */
   kpg1Gtgrp( param, &grp, &size, status );

/* If no group was supplied, just annul any PAR__NULL error. */
   if( *status == PAR__NULL) {
      errAnnul( status );

/* If a group was supplied, see if it consists of the single value "def".
   If so, we will leave the KeyMap unchanged. */
   } else if (size > 0 ) {
      value = buffer;
      if( size == 1 ) {
         grpGet( grp, 1, 1, &value, sizeof(buffer), status );
      } else {
         strcpy( value, " " );
      }

/* Otherwise, store the configuration settings in the KeyMap. */
      if( ! astChrMatch( value, "DEF" ) ) kpg1Kymap( grp, &external, status );

/* Handle nested entries */
      kpg1__process_nesting( external, nested, status );

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

/* Store the merged keymap as the keymap to be associated with this parameter.
 * This is the one that will actually be used rather than the one that was given
 * by the user. */
   if ( result ) {
     Grp * mergedgrp = NULL;

     /* convert to a GRP */
     kpg1Kygrp( result, &mergedgrp, status );

     /* register it */
     ndgAddgh( param, mergedgrp, status );

     if (mergedgrp) grpDelet( &mergedgrp, status );
   }

/* Delete the group, if any. */
   if( grp ) grpDelet( &grp, status );

/* Return the KeyMap. */
   return result;
}

/*
 * This function handles the nesting. Does nothing if the supplied "nested" keymap
 * is NULL.
 * For each key in "nested" sees if a corresponding key is present in "keymap".
 * If it is present the value in "nested" is checked. If true the contents are
 * copied to the base keymap. The entry is then deleted.
 */

static void kpg1__process_nesting( AstKeyMap * keymap,
                                   AstKeyMap * nested, int * status ) {
  size_t i;
  size_t nnest;

  if (*status != SAI__OK) return;
  if (! nested ) return;

  nnest = astMapSize( nested );
  for (i=0; i < nnest; i++) {
    int keep = 0;
    const char * testkey = astMapKey( nested, i );

    /* see if that is present in "keymap". No problem if it is not present.
       It should itself be a keymap if it is there. */
    if ( astMapHasKey( keymap, testkey ) ) {
      int ktype = 0;

      /* Check its type */
      ktype = astMapType( keymap, testkey );
      if ( ktype == AST__OBJECTTYPE ) {

        /* should we keep these values and copy them to the parent? */
        astMapGet0I( nested, testkey, &keep );

        if (keep) {
          AstObject * obj = NULL;
          AstKeyMap * subkeymap = NULL;
          /* Get the named nested keymap from "keymap" */
          astMapGet0A( keymap, testkey, &obj );
          subkeymap = (AstKeyMap*)obj;
          obj = NULL;

          if (astIsAKeyMap( subkeymap ) ) {
            astMapCopy( keymap, subkeymap );
          } else {
            if (*status == SAI__OK) {
              *status = SAI__ERROR;
              errRepf( "", "Key '%s' in configuration should be a KeyMap",
                       status, testkey );
              subkeymap = astAnnul( subkeymap );
              break;
            }
          }
          subkeymap = astAnnul( subkeymap );
        }
      } else {
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          errRepf( "", "Key '%s' in configuration should be a KeyMap",
                   status, testkey );
          break;
        }
      }

      /* Now remove the nested item from "keymap" since we do not need it
         any more. */
      astMapRemove( keymap, testkey );

    }
  }

}
