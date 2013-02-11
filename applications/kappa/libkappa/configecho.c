#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include <math.h>
#include <string.h>
#include <stdio.h>

static void DisplayKeyMap( AstKeyMap *km, int sort, const char *prefix, int *status );

F77_SUBROUTINE(configecho)( INTEGER(STATUS) ){
/*
*+
*  Name:
*     CONFIGECHO

*  Purpose:
*     Displays one or more configuration parameters.

*  Language:
*     C (designed to be called from Fortran)

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CONFIGECHO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays the name and value of one or more
*     configuration parameters, specified using Parameter CONFIG.
*     The value is also written to an output parameter. If the
*     parameter is not specified by the CONFIG or DEFAULTS parameter,
*     then the value supplied for DEFVAL is returned.

*  Usage:
*     configecho name config [defaults] [select] [defval]

*  ADAM Parameters:
*     CONFIG = GROUP (Read)
*        Specifies values for the configuration parameters. If the string
*        "def" (case-insensitive) or a null (!) value is supplied, a set
*        of default configuration parameter values will be used, as
*        specified by Parameter DEFAULTS.
*
*        The supplied value should be either a comma-separated list of
*        strings or the name of a text file preceded by an up-arrow
*        character "^", containing one or more comma-separated lists of
*        strings. Each string is either a "keyword=value" setting, or the
*        name of a text file preceded by an up-arrow character "^". Such
*        text files should contain further comma-separated lists which
*        will be read and interpreted in the same manner (any blank lines
*        or lines beginning with "#" are ignored). Within a text file,
*        newlines can be used as delimiters, as well as commas. Settings
*        are applied in the order in which they occur within the list,
*        with later settings overriding any earlier settings given for
*        the same keyword.
*
*        Each individual setting should be of the form "<keyword>=<value>".
*        If a non-null value is supplied for Parameter DEFAULTS, an error
*        will be reported if CONFIG includes values for any parameters
*        that are not included in DEFAULTS.
*     DEFAULTS = LITERAL (Read)
*        The path to a file containing the default value for every
*        allowed configuration parameter. If null (!) is supplied, no
*        defaults will be supplied for parameters that are not specified
*        by CONFIG, and no tests will be performed on the validity of
*        paramter names supplied by CONFIG. [!]
*     DEFVAL = LITERAL (Read)
*        The value to return if no value can be obtained for the named
*        parameter, or if the value is "<undef>".  [<***>]
*     NAME = LITERAL (Read)
*        The name of the configuration parameter to display. If null (!)
*	 then all parameters defined in the configuration are displayed.
*     SELECT = GROUP (Read)
*        A group that specifies any alternative prefixes that can be
*        included at the start of any parameter name. For instance, if
*        this group contains the two entries "450=1" and "850=0", then
*        DEFAULTS can specify two defaults for any single parameter--
*        one for the parameter prefixed by "450." and another for the
*        parameter prefixed by "850.". Thus if DEFAULTS defines a
*        parameter called "filter", it could include "450.filter=300"
*        and "850.filter=600". The CONFIG parameter could then either
*        set the filter parameter for a specific prefix (as in
*        "450.filter=234"); or it could leave the prefix unspecified,
*        in which case the prefix used is the first one with a
*        non-zero value in SELECT (450 in the case of this example - 850
*        has a value zero in SELECT). Thus the names of the items in
*        SELECT define the set of allowed alternative prefixes, and the
*        values indicate which one of these alternatives is to be used
*        (the first one with non-zero value). [!]
*     SORT = _LOGICAL (Read)
*        If TRUE then sort the listed parameters in to alphabetical order.
*        Otherwise, retain the order they have in the supplied
*	 configuration. Only used if a null (!) value is supplied for
*        parameter NAME. [FALSE]
*     VALUE = LITERAL (Write)
*        The value of the configuration parameter, or "<***>" if the
*        parameter has no value in CONFIG and DEFAULTS.

*  Examples:
*     configecho m81 ^myconf
*        Report the value of configuration parameter "m81" defined within
*        the file "myconf". If the file does not contain a value for
*        "m81", then "<***>" is displayed.
*     configecho flt.filt_edge_largescale \
*                config=^/star/share/smurf/dimmconfig.lis \
*                defaults=/star/bin/smurf/smurf_makemap.def \
*                select="450=1,850=0"
*        Report the value of configuration parameter "flt.filt_edge_largescale"
*        defined within the file "/star/share/smurf/dimmconfig.lis", using
*        defaults from the file "/star/bin/smurf/smurf_makemap.def". If
*        dimmconfig.lis does not contain a value for "flt.filt_edge_largescale"
*        then it is searched for "450.flt.filt_edge_largescale" instead. An
*        error is reported if dimmconfig.lis contains values for any
*        items that are not defined in smurf_makemap.def.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     10-DEC-2012 (DSB):
*        Original version.
*     6-FEB-2012 (DSB):
*        Added parameter DEFVAL.
*     11-FEB-2013 (DSB):
*        Added parameter SORT and allow all parameters to be listed by
*        providing a null value for NAME.
*     {enter_further_changes_here}

*-
*/

   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   AstKeyMap *keymap2;
   AstKeyMap *keymap;
   Grp *grp = NULL;
   char *dot;
   char *pname;
   char defs[250];
   char defval[250];
   char name[250];
   const char *value;
   int showall;
   int sort;
   size_t size;

/* Abort if an error has already occurred. */
   if( *STATUS != SAI__OK ) return;

/* Begin an AST context */
   astBegin;

/* Get the value to return if no value can be obtained for the named
   parameter, of it it has a value of <undef>. */
   parGet0c( "DEFVAL", defval, sizeof(defval), STATUS );

/* Get any defaults file, annuling the error if null (!) is supplied. */
   if( *STATUS == SAI__OK ) {
      parGet0c( "DEFAULTS", defs, sizeof(defs), STATUS );
      if( *STATUS == PAR__NULL ) {
         errAnnul( STATUS );
         defs[0] = 0;
      }
   }

/* If no defaults file was supplied, just get the CONFIG group and convert
   to an AST KeyMap. */
   if( ! defs[0] ) {
      kpg1Gtgrp( "CONFIG", &grp, &size, STATUS );
      kpg1Kymap( grp, &keymap, STATUS );
      grpDelet( &grp, STATUS );

/* If a defaults file was supplied, we also allow the user to define the
   allowed alternative prefixes and to select the default prefix, using
   parameter SELECT. */
   } else if( *STATUS == SAI__OK ){

      kpg1Gtgrp( "SELECT", &grp, &size, STATUS );
      if( *STATUS == PAR__NULL ) {
         grpDelet( &grp, STATUS );
         errAnnul( STATUS );
         keymap2 = NULL;
      } else {
         kpg1Kymap( grp, &keymap2, STATUS );
         grpDelet( &grp, STATUS );
      }

      keymap = kpg1Config( "CONFIG", defs, keymap2, STATUS );
   }

/* Abort if an error has occurred. */
   if( *STATUS != SAI__OK ) goto L999;

/* Get the name of the required parameter, and convert to upper case (if
   supplied). If not supplied, set a flag indicating that all parameters
   should be displayed. */
   parGet0c( "NAME", name, sizeof(name), STATUS );
   if( *STATUS == PAR__NULL ) {
      errAnnul( STATUS );
      showall = 1;
   } else {
      showall = 0;
      astChrCase( NULL, name, 1, 0 );
   }

   if( *STATUS == SAI__OK ) {

/* First deal with cases where we are displaying a single parameter
   value. */
      if( !showall ) {

/* Loop round each section of the name that ends with a dot. */
         value = defval;
         pname = name;

         dot = strchr( pname, '.' );
         while( dot && keymap ) {

/* Get a nested keymap with the name that occurs prior to the dot. If
   found, use it in place of the parent keymap. */
            pname[ dot - pname ] = 0;
            if( astMapGet0A( keymap, pname, &keymap2 ) ) {
               astAnnul( keymap );
               keymap = keymap2;
            } else {
               astAnnul( keymap );
            }

/* Re-instate the original dot, and move on to find the next dot. */
            pname[ dot - pname ] = '.';
            pname = dot + 1;
            dot = strchr( pname, '.' );
         }

/* Ensure no error is reported if the parameter is not found in the
   KeyMap. */
         if( keymap ) {
            astClear( keymap, "KeyError" );

/* Get the parameter value as a string. */
            astMapGet0C( keymap, pname, &value );
         }

/* Display it. */
         msgOut( "", value, STATUS );

/* Write it to the output parameter. */
         parPut0c( "VALUE", value, STATUS );

/* Now deal with cases were we are displaying all parameter values. */
      } else {

/* See if the values should be sorted. */
         parGet0l( "SORT", &sort, STATUS );

/* Display them. */
         DisplayKeyMap( keymap, sort, "", STATUS );
      }
   }

/* Tidy up. */
L999:;

/* End the AST context */
   astEnd;

/* If an error has occurred, issue another error report identifying the
   program which has failed (i.e. this one). */
   if( *STATUS != SAI__OK ) {
      errRep( "CONFIGECHO_ERR", "CONFIGECHO: Failed to echo configuration "
              "parameters.", STATUS );
   }

}





static void DisplayKeyMap( AstKeyMap *km, int sort, const char *prefix,
                           int *status ){
/*
*  Name:
*     DisplayKeyMap

*  Purpose:
*     Display the contents of a keymap.

*  Synopsis:
*     void DisplayKeyMap( AstKeyMap *km, int sort, const char *prefix,
*                         int *status )

*  Arguments:
*     km
*        Pointer to the KeyMaps containing the values to display.
*     sort
*        If non-zero, sort the values alphabetically by their keys.
*     prefix
*        A string to prepend to eack key.
*     status
*        Inherited status pointer.

*  Description:
*     This function displays the contents of a supplied KeyMap as
*     a series of "key = value" strings, one per line. It calls itself
*     recursively if a nested KeyMap is found, adding a suitable
*     prefix to the nested keys.
*/

/* Local Variables: */
   AstObject *avalue;
   char cbuffer[ 255 ];
   char newpref[ 255 ];
   const char *cvalue;
   const char *key;
   int ikey;
   int ival;
   int nc;
   int nkey;
   int nval;

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Sort the supplied KeyMap is required. */
   if( sort ) astSetC( km, "SortBy", "KeyUp" );

/* Loop round all keys in the supplied KeyMap. */
   nkey = astMapSize( km );
   for( ikey = 0; ikey < nkey; ikey++ ) {
      key = astMapKey( km, ikey );

/* If the current entry is a nest KeyMap, get a pointer to it and call
   this function recurisvely to display it, modifying the prefix to add
   to each key so that it includes the key associated with the nest keymap. */
      if( astMapType( km, key ) == AST__OBJECTTYPE ) {
         astMapGet0A( km, key, &avalue );
         sprintf( newpref, "%s%s.", prefix, key );
         DisplayKeyMap( (AstKeyMap *) avalue, sort, newpref, status );
         avalue = astAnnul( avalue );

/* If the current entry is not a nested keymap, we display it now. */
      } else {

/* Get the vector length of the entry. */
         nval = astMapLength( km, key );

/* If it is a scalar, just get its value as a character string using
   the automatic type conversion provided by the KeyMap class, and
   display it, putting the supplied prefix at the start of the key. */
         if( nval <= 1 ) {
            cvalue = "<undef>";
            astMapGet0C( km, key, &cvalue );
            msgOutf( "", "%s%s = %s", status, prefix, key, cvalue );

/* If it is a vector, we construct a string containing a comma-separated
   list of elements, enclosed in parentheses. */
         } else {
            nc = 0;
            cvalue = astAppendString( NULL, &nc, "(" );
            for( ival = 0; ival < nval; ival++ ) {
               if( astMapGetElemC( km, key, sizeof( cbuffer) - 1, ival,
                                   cbuffer ) ) {
                  cvalue = astAppendString( (char *) cvalue, &nc, cbuffer );
               }
               if( ival < nval - 1 ) cvalue = astAppendString( (char *) cvalue,
                                                                &nc, "," );
            }
            cvalue = astAppendString( (char *) cvalue, &nc, ")" );

/* Display the total string, with the current key prefix, and free the memory
   used to store it. */
            msgOutf( "", "%s%s = %s", status, prefix, key, cvalue );
            cvalue = astFree( (void *) cvalue );
         }
      }
   }
}



