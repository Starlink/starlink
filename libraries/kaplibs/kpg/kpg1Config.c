#include "star/grp.h"
#include "star/ndg.h"
#include "ast.h"
#include "kaplibs.h"
#include "sae_par.h"
#include "par_err.h"
#include "mers.h"


/* Prototypes for internal helper routines */
static void kpg1Config_ProcessNesting( AstKeyMap *keymap, AstKeyMap *nested,
                                       int *status );
static void kpg1Config_CheckNames( AstKeyMap *map1, AstKeyMap *map2, Grp *grp,
                                   const char *param, int *status );



AstKeyMap *kpg1Config( const char *param, const char *def,
                       AstKeyMap *nested, int *status ){
/*
*+
*  Name:
*     kpg1Config

*  Purpose:
*     Creates an AST KeyMap holding a set of configuration parameter values.

*  Language:
*     C.

*  Invocation:
*     AstKeyMap *kpg1Config( const char *param, const char *def,
*                            AstKeyMap *nested, int *status )

*  Description:
*     This function first creates a KeyMap by reading the values from a
*     specified text file (argument "def"). This text files specifies the
*     complete list of all allowed config parameters and their default
*     values. The KeyMap is then locked by setting its MapLocked attribute
*     to a true value. A GRP group is then obtained from the environment
*     using the specified parameter (argument "param"). The config
*     settings thus obtained are stored in the KeyMap. An error is reported
*     if the user-supplied group refers to any config parameters that were
*     not read earlier from the default file.
*
*     Both the defaults file and the user-supplied configuration may
*     contain several sets of "alternate" configuration parameter values,
*     and the set to use can be specified by the caller via the supplied
*     "nested" keymap. For instance, if a basic configuration contains
*     keys "par1" and "par2", the user could supply two alternate sets of
*     values for these keys by prepending each key name with the strings
*     "850" and "450" (say). Thus, the user could supply values for any
*     or all of: "par1", "par2", "450.par1", "450.par2", "850.par1",
*     "850.par2".
*
*     The "nested" keymap supplied by the caller serves two functions: 1)
*     it defines the known alternatives, and 2) it specifies which of the
*     aternatives is to be used. Each key in the supplied "nested" keymap
*     should be the name of an allowed alternative. In the above example,
*     a KeyMap containing keys "850" and "450" could be supplied. No
*     error is reported if the user-supplied configuration fails to
*     provide values for one or more of the alternatives. The value
*     associated with each key in the "nested" keymap should be an
*     integer - the alternative to be used should have a non-zero value,
*     and all other should be zero.
*
*     So first, the configuration parameters specified by the supplied
*     defaults file are examined. Any parameter that starts with the name
*     of the selected alternative (i.e. the key within "nested" that has
*     an associated value of 1) has the name of the alternative removed.
*     Any parameter that start with the name of any of the other
*     alternatives is simply removed from the configuration. Thus, using
*     the above example, if "nested" contains two entries - one with key
*     "850" and value "1", and the other with key "450" and value "0" -
*     the "850.par1" and "850.par2" entries in the defaults file would be
*     renamed as "par1" and "par2" (over-writing the original values for
*     "par1" and "par2"), and the "450.par1" and "450.par2" entries would
*     be deleted.
*
*     Next, the same process is applied to the user-supplied
*     configuration obtained via the specified environment parameter.
*
*     Finally, the values in the user-supplied configuration are used to
*     replace those in the defaults file.
*
*     The beneifts of using this function are that 1) the user gets to
*     know if they mis-spell a config parameter name, and 2) the default
*     parameter values can be defined in a single place, rather than
*     hard-wiring them into application code at each place where the
*     config parameter is used.

*  Arguments:
*     paramc = const char * (Given)
*        The name of the environment parameter to use.
*     def = const char * (Given)
*        The path to a file containing the default value for every allowed config
*        parameter. For instance, "$SMURF_DIR/dimmconfig.def".
*     nested = AstKeyMap * (Given)
*        If non-NULL, used to determine which nested keys might be in the config
*        and which should be merged with the base keymap. The values in the keymap
*        should be non-zero to indicate merging.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
*     19-MAY-2010 (DSB):
*        - In order to get better error messages, do explicit checks that all
*        user supplied keys are good, so that
*        - Expand on the documentation for "nested".
*     28-JAN-2011 (DSB):
*        Correct handling of case where the user supplied "def" to
*        indicate that the defaults should be accepted. In this case
*        there is no "external" keymap.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstKeyMap *external = NULL;  /* Keymap of externally supplied values */
   AstKeyMap *result = NULL;    /* Returned KeyMap */
   Grp *grp;                    /* Group to hold config values */
   char *value;                 /* Pointer to GRP element buffer */
   char buffer[ GRP__SZNAM ];   /* Buffer for GRP element */
   int added;                   /* Number of names added to group */
   int flag;                    /* Flag */
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
   kpg1Config_ProcessNesting( result, nested, status );

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
      if( ! astChrMatch( value, "DEF" ) ) {
         kpg1Kymap( grp, &external, status );

/* Handle alternate nested entries */
         kpg1Config_ProcessNesting( external, nested, status );

/* Test every entry in the user-supplied configuration keymap. If an
   entry is found which does not exist in the defaults keymap, report an
   error. We could rely on astMapCopy to do this (called below) but the
   wording of the error message created by astMapCopy is a bit too
   generalised to be useful. */
         kpg1Config_CheckNames( result, external, grp, param, status );

/* Copy the overrides into the default. */
         astMapCopy( result, external );

/* Delete the external KeyMap */
         external = astAnnul( external );
      }
   }

/* Ensure the KeyError attribute is non-zero in the returned KeyMap so that an
   error is reported by astMapGet<X> if the KeyMap does not contain the requested
   entry. */
   if( result ) astSetI( result, "KeyError", 1 );

/* Store the merged keymap as the keymap to be associated with this parameter.
   This is the one that will actually be used rather than the one that was given
   by the user. */
   if ( result ) {
      Grp *mergedgrp = NULL;

/* Convert to a GRP */
      kpg1Kygrp( result, &mergedgrp, status );

/* Register it */
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

static void kpg1Config_ProcessNesting( AstKeyMap * keymap, AstKeyMap * nested,
                                       int * status ) {
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

static void kpg1Config_CheckNames( AstKeyMap *map1, AstKeyMap *map2, Grp *grp,
                                   const char *param, int *status ){
/*
*  Name:
*     kpg1Config_CheckNames

*  Purpose:
*     Report an error if "map2" contains a key that is not in "map1".

*  Invocation:
*     void kpg1Config_CheckNames( AstKeyMap *map1, AstKeyMap *map2, Grp *grp,
*                                 const char *param, int *status )

*  Description:
*     This function is a private helper function for kpg1Config. It
*     checks that each entry in the "map2" KeyMap has a corresponding
*     entry in "map1". If not, an error is reported. It handles nested
*     KeyMaps by invoking itself recursively.

*  Arguments:
*     map1 = AstKeyMap * (Given)
*        Pointer to a KeyMap holding defaults for all known configuration
*        parameters.
*     map2 = AstKeyMap * (Given)
*        Pointer to a KeyMap holding user supplied configuration
*        parameter values.
*     grp = Grp * (Given)
*        Pointer to the GRP group obtained from the user.
*     param = const char * (Given)
*        The ADAM parameter used to obtain the group.
*     status = int * (Given & Returned)
*        The inherited status.

*  Authors:
*     DSB: David S. Berry

*  History:
*     19-MAY-2010 (DSB):
*        Original version.
*/

/* Local Variables: */
   AstObject *obj1;          /* Object pointer obtained from map1 */
   AstObject *obj2;          /* Object pointer obtained from map2 */
   char *match;              /* Pointer to matching string */
   char *up_badname;         /* Upper-case version of badname */
   char *up_elem = NULL;     /* Upper case version of "elem" */
   char elem[ GRP__SZNAM ];  /* Value of element in "grp" */
   char file[ GRP__SZFNM ];  /* File name from which element was read */
   char re[ 100 ];           /* Regular expression matching an assigment */
   const char *badname;      /* String holding unknwon parameter name */
   const char *key;          /* Key for entry being checked */
   int file_len;             /* Length of file name */
   int ielem;                /* Index of element in "grp" */
   int ikey;                 /* Index of of entry in map2 */
   int nelem;                /* Number of elements in "grp" */
   int nkey;                 /* Number of entries in map2 */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Indicate we have not yet found any bad names in "map2". */
   badname = NULL;

/* Loop round every entry in "map2". Break if we encounter a bad name in
   "map2", or if an error occurs. */
   nkey = astMapSize( map2 );
   for( ikey = 0; ikey < nkey && !badname && *status == SAI__OK; ikey++ ) {
      key = astMapKey( map2, ikey );

/* If an entry with the same name exists in "map1"... */
      if( astMapHasKey( map1, key ) ) {

/* ... and the entry is a KeyMap ... */
         if( astMapType( map1, key ) == AST__OBJECTTYPE ) {

/* ... and the corresponding "map2" entry is a KeyMap, call this function
   recursively to check the contents of the two KeyMaps. */
            if( astMapType( map2, key ) == AST__OBJECTTYPE ) {

               (void) astMapGet0A( map1, key, &obj1 );
               (void) astMapGet0A( map2, key, &obj2 );

               kpg1Config_CheckNames( (AstKeyMap *) obj1,
                                      (AstKeyMap *) obj2,
                                      grp, param, status );

/* Indicate we have found a bad name if the corresponding "map2" entry is not
   a KeyMap. */
            } else if( *status == SAI__OK ) {
               badname = key;
            }

/* Free the AST object pointers. */
            obj1 = astAnnul( obj1 );
            obj2 = astAnnul( obj2 );
         }

/* If no such entry exists in "map1", indicate we have found a bad name. */
      } else {
         badname = key;
      }
   }

/* If a bad name was found, search the group of user-supplied values for the
   bad name, and then report an error including extra info about the bad
   name. */
   if( badname && *status == SAI__OK ) {

/* Create an upper-case copy of the bad name. */
      up_badname = astStringCase( badname, 1 );

/* Create a regular expression that matches a string that assigns a value
   to the upper case bad name (with or without preceededing parent key
   names). */
      sprintf( re, "\\.%s *\\=|^%s *\\=", up_badname, up_badname );

/* Loop round all elements in the group. */
      nelem =  grpGrpsz( grp, status );
      for( ielem = 1; ielem <= nelem; ielem++ ) {

/* Get the text of the element. */
         grpInfoc( grp, ielem, "Name", elem, GRP__SZNAM, status );

/* Create an upper-case copy of the element. */
         up_elem = astStringCase( elem, 1 );

/* See if it looks like a string that assigns a value to the bad name. If
   so, break out of the loop with "up_elem" still set. */
         match = astChrSub( up_elem, re, NULL, 0 );
         if( match ) {
            match = astFree( match );
            break;
         }

/* Free resources. */
         up_elem = astFree( up_elem );
      }
      up_badname = astFree( up_badname );

/* If we found a grp element that assigns a value to the bad name, get the
   name of the file from which the element was read. */
      if( up_elem ) {
         grpInfoc( grp, ielem, "File", file, GRP__SZFNM, status );
      } else {
         file[ 0 ] = 0;
         file_len = 0;
      }

/* Create an error report, including the group element and the name of the file
   from which the element was read, if possible. */
      msgSetc( "M", badname );
      msgSetc( "P", param );

      *status = SAI__ERROR;
      errRep( " ", "Unknown configuration parameter '^M' specified via environment "
              "parameter ^P.", status );

      if( up_elem ) {
         msgSetc( "M", "Text was '" );
         msgSetc( "M", elem );
         msgSetc( "M", "'" );

         if( astChrLen( file ) > 0 ) {
            msgSetc( "M", " read from file '" );
            msgSetc( "M", file );
            msgSetc( "M", "'" );
         }
         msgSetc( "M", "." );
         errRep( " ", "^M", status );
      }

      up_elem = astFree( up_elem );
   }
}


