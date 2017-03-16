#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "star/one.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

static void DisplayKeyMap( AstKeyMap *km, int sort, const char *prefix,
                           AstKeyMap *refkm, FILE *fd, int *status );
void HistoryKeyMap(int i, char* const text[], int* status);
AstKeyMap* historyConfig = NULL;

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
*     This application displays the name and value of one or all
*     configuration parameters, specified using Parameters CONFIG or
*     NDF. If a single parameter is displayed, its value is also
*     written to an output parameter. If the parameter value is not
*     specified by the CONFIG, NDF or DEFAULTS parameter, then the
*     value supplied for DEFVAL is displayed.
*
*     If an input NDF is supplied then configuration parameters
*     are read from its history (see Parameters NDF and APPLICATION).
*
*     If values are supplied for both CONFIG and NDF, then the
*     differences between the two sets of configuration parameters
*     are displayed (see Parameter NDF).

*  Usage:
*     configecho name config [defaults] [select] [defval]

*  ADAM Parameters:
*     APPLICATION = LITERAL (Read)
*        When reading configuration parameters from the history
*        of an NDF, this parameter specifies the name of the application
*        to find in the history. There must be a history component
*        corresponding to the value of this parameter, and it must
*        include a CONFIG group. [current value]
*     CONFIG = GROUP (Read)
*        Specifies values for the configuration parameters. If the string
*        "def" (case-insensitive) or a null (!) value is supplied, the
*        configuration parameters are obtained using Parameter NDF. If
*        a null value is also supplied for NDF, a set of default
*        configuration parameter values will be used, as specified by
*        Parameter DEFAULTS.
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
*     LOGFILE = LITERAL (Read)
*        The name of a text file in which to store the displayed
*        configuration parameters. [!]
*     NAME = LITERAL (Read)
*        The name of the configuration parameter to display.  If set to
*        null (!), then all parameters defined in the configuration are
*        displayed.
*     NDF = NDF (Read)
*        An NDF file containing history entries which include
*        configuration parameters. If not null (!) the history
*        of the NDF will be searched for a component corresponding
*        to the Parameter APPLICATION.  The Parameter CONFIG
*        is then optional, but if it too is not null (!) then
*        the output will show the differences between the configuration
*        stored in the NDF history and the given configuration:
*        new parameters and those different from the reference
*        configuration (given by Parameter CONFIG) are prefixed
*        with "+" and those which are the same as the reference
*        configuration are prefixed with "-". [!]
*     SELECT = GROUP (Read)
*        A group that specifies any alternative prefixes that can be
*        included at the start of any parameter name. For instance, if
*        this group contains the two entries "450=1" and "850=0", then
*        either CONFIG or DEFAULTS can specify two values for any single
*        parameter -- one for the parameter prefixed by "450." and another
*        for the parameter prefixed by "850.". Thus, for instance, if
*        DEFAULTS defines a parameter called "filter", it could include
*        "450.filter=300" and "850.filter=600". The CONFIG parameter could
*        then either set the filter parameter for a specific prefix (as
*        in "450.filter=234"); or it could leave the prefix unspecified,
*        in which case the prefix used is the first one with a
*        non-zero value in SELECT (450 in the case of this example - 850
*        has a value zero in SELECT). Thus the names of the items in
*        SELECT define the set of allowed alternative prefixes, and the
*        values indicate which one of these alternatives is to be used
*        (the first one with non-zero value). [!]
*     SORT = _LOGICAL (Read)
*        If TRUE then sort the listed parameters in to alphabetical order.
*        Otherwise, retain the order they have in the supplied
*        configuration. Only used if a null (!) value is supplied for
*        Parameter NAME. [FALSE]
*     VALUE = LITERAL (Write)
*        The value of the configuration parameter, or "<***>" if the
*        parameter has no value in CONFIG and DEFAULTS.

*  Examples:
*     configecho m81 ^myconf
*        Report the value of configuration parameter "m81" defined within
*        the file "myconf". If the file does not contain a value for
*        "m81", then "<***>" is displayed.
*     configecho type ^myconf select="m57=0,m31=1,m103=0"
*        Report the value of configuration parameter "type" defined within
*        the file "myconf". If the file does not contain a value for
*        "type", then the value of "m31.type" will be reported instead. If
*        neither is present, then "<***>" is displayed.
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
*     configecho ndf=omc1 config=^/star/share/smurf/dimmconfig.lis \
*                defaults=/star/bin/smurf/smurf_makemap.def \
*                application=makemap name=! sort select="450=0,850=1"
*        Show how the configuration used to generate the 850um map
*        of OMC1 differs from the basic dimmconfig.lis file.

*  Copyright:
*     Copyright (C) 2012-3 Science & Technology Facilities Council.
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
*     GSB: Graham S. Bell
*     {enter_new_authors_here}

*  History:
*     10-DEC-2012 (DSB):
*        Original version.
*     6-FEB-2013 (DSB):
*        Added parameter DEFVAL.
*     11-FEB-2013 (DSB):
*        Added parameter SORT and allow all parameters to be listed by
*        providing a null value for NAME.
*     11-FEB-2013 (GSB):
*        Added ability to read configuration from history entries.
*     13-FEB-2013 (DSB):
*        Nullify AST object pointers when the objects are annulled,
*        to avoid re-use of dead pointers.
*     14-FEB-2013 (DSB):
*        Allow the SELECT feature to be used even if no DEFAULTS file is
*        supplied (see the new entry in the "Examples:" section).
*     15-FEB-2013 (DSB):
*        Expand the prologue docs, and use NULL in place of zero for pointers.
*     22-FEB-2013 (DSB):
*        Guard against seg fault in HistoryKeymap when the NDF does
*        not contain the required CONFIG entry in the History
*        component.
*     23-SEP-2014 (DSB):
*        Added parameter LOGFILE.
*     16-MAR-2017 (DSB):
*        Previously, echoing a vector-valued parameter such as makemap's
*        "modelorder" parameter only displayed the first element. The use
*        of astMapGet0C has been changed to astMapGetC to fix this bug.
*     {enter_further_changes_here}

*-
*/

   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   AstKeyMap *keymap2;
   AstKeyMap *keymap;
   FILE *fd = NULL;
   Grp *grp = NULL;
   char *dot;
   char *pname;
   char application[NDF__SZAPP];
   char applicationi[NDF__SZAPP];
   char defs[250];
   char defval[250];
   char logfile[250];
   char name[250];
   const char *historyValue = NULL;
   const char *value;
   int i;
   int indf = 0;
   int nrec;
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

/* Get the NDF identifier if requested. */
   ndfBegin();
   if (*STATUS == SAI__OK) {
      ndfAssoc("NDF", "READ", &indf, STATUS);
      if (*STATUS == PAR__NULL) {
         errAnnul(STATUS);
         indf = 0;
      }
      else {
         parGet0c("APPLICATION", application, sizeof(application), STATUS);
         /* Check now for error because the block below allowing an undefined
          * CONFIG clears this status otherwise. */
         if (*STATUS != SAI__OK) goto L999;
      }
   }

/* See if any alternate keyword prefixes are allowed, and if so determine
   which of the alternatices is to be displayed. */
   kpg1Gtgrp( "SELECT", &grp, &size, STATUS );
   if( *STATUS == PAR__NULL ) {
      grpDelet( &grp, STATUS );
      errAnnul( STATUS );
      keymap2 = NULL;
   } else {
      kpg1Kymap( grp, &keymap2, STATUS );
      grpDelet( &grp, STATUS );
   }

/* Create a KeyMap holding the selected alternative for each keyword, and
   also supply defaults for any missing values (if a defaults file was
   supplied by the user). */
   keymap = kpg1Config( "CONFIG", defs[0]?defs:NULL, keymap2, 0, STATUS );

/* Allow it to be NULL if we're reading an NDF because we'll replace
   keymap with historyConfig later if necessary. */
   if( indf && *STATUS == PAR__NULL ) {
      errAnnul(STATUS);
      keymap = NULL;
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

/* Attempt to find the NDF's corresponding history record. */
   if (indf && *STATUS == SAI__OK) {
      ndfHnrec(indf, &nrec, STATUS);
      for (i = 0; i < nrec; i ++) {
         ndfHinfo(indf, "APPLICATION", i + 1, applicationi,
                  sizeof(applicationi), STATUS);
         if (! strncasecmp(application, applicationi, strlen(application))) {
            ndfHout(indf, i + 1, HistoryKeyMap, STATUS);
            break;
         }
      }

      if (*STATUS == SAI__OK && ! historyConfig) {
         *STATUS = SAI__ERROR;

         errRepf("CONFIGECHO_ERR", "CONFIGECHO: Failed to find %s "
                 "configuration in NDF history.", STATUS, application);
      }
      else if (! keymap) {
         keymap = historyConfig;
         historyConfig = NULL;
      }
   }


/* Open a log file if required. */
   if( *STATUS == SAI__OK ) {
      parGet0c( "LOGFILE", logfile, sizeof(logfile), STATUS );
      if( *STATUS == PAR__NULL ) {
         errAnnul( STATUS );
      } else if( *STATUS == SAI__OK ) {
         fd = fopen( logfile, "w" );
         if( !fd ) {
            *STATUS = SAI__ERROR;
            errRepf( "", "Failed to create log file (%s): %s.", STATUS,
                     logfile, strerror(errno) );
         }
      }
   }

/* Check it is safe to proceed. */
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
               keymap = astAnnul( keymap );
            }

/* If historyConfig exists, do the same there. */
            if (historyConfig) {
               if (astMapGet0A(historyConfig, pname, &keymap2)) {
                  astAnnul(historyConfig);
                  historyConfig = keymap2;
               }
               else {
                  historyConfig = astAnnul(historyConfig);
               }
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
            astMapGetC( keymap, pname, &value );
         }

         if (historyConfig) {
            astClear(historyConfig, "KeyError");
            astMapGetC(historyConfig, pname, &historyValue);

/* In NDF history mode we only want to return a value if it
   was found in the configuration from the history. */

            if (historyValue) {
               if (strcmp(value, historyValue)) {
                  msgOutf("", "+ %s", STATUS, historyValue);
               }
               else {
                  msgOutf("", "- %s", STATUS, historyValue);
               }
               parPut0c("VALUE", historyValue, STATUS);
            }
         }
         else {
/* Display it. */
            msgOut( "", value, STATUS );

/* Write it to the output parameter. */
            parPut0c( "VALUE", value, STATUS );
         }

/* Now deal with cases were we are displaying all parameter values. */
      } else {

/* See if the values should be sorted. */
         parGet0l( "SORT", &sort, STATUS );

/* Display them. */
         if (historyConfig) {
            DisplayKeyMap( historyConfig , sort, "", keymap, fd, STATUS );
         }
         else {
            DisplayKeyMap( keymap, sort, "", NULL, fd, STATUS );
         }
      }
   }

/* Tidy up. */
L999:;

/* Close the log file. */
   if( fd ) fclose( fd );

/* End the AST context */
   astEnd;

/* Close the NDF if open. */
   ndfEnd(STATUS);

/* If an error has occurred, issue another error report identifying the
   program which has failed (i.e. this one). */
   if( *STATUS != SAI__OK ) {
      errRep( "CONFIGECHO_ERR", "CONFIGECHO: Failed to echo configuration "
              "parameters.", STATUS );
   }

}





static void DisplayKeyMap( AstKeyMap *km, int sort, const char *prefix,
                           AstKeyMap *refkm, FILE *fd, int *status ){
/*
*  Name:
*     DisplayKeyMap

*  Purpose:
*     Display the contents of a keymap.

*  Synopsis:
*     void DisplayKeyMap( AstKeyMap *km, int sort, const char *prefix,
*                         FILE *fd, int *status )

*  Arguments:
*     km
*        Pointer to the KeyMaps containing the values to display.
*     sort
*        If non-zero, sort the values alphabetically by their keys.
*     prefix
*        A string to prepend to eack key.
*     refkm
*        Reference key map (e.g. values from the supplied configuration
*        rather than the NDF history), or null if not required.
*     fd
*        Pointer to stdio descriptor for log file, or NULL. If supplied,
*        a copy of the displayed text is appended to the log file.
*     status
*        Inherited status pointer.

*  Description:
*     This function displays the contents of a supplied KeyMap as
*     a series of "key = value" strings, one per line. It calls itself
*     recursively if a nested KeyMap is found, adding a suitable
*     prefix to the nested keys.  If a reference key map is supplied then
*     the output shows how the main key map differs from it.
*/

/* Local Variables: */
   AstObject *avalue;
   AstObject *refavalue;
   char *text;
   char cbuffer[ 255 ];
   char newpref[ 255 ];
   const char *cvalue;
   const char *key;
   const char *refcvalue;
   int ikey;
   int ival;
   int nc;
   int nkey;
   int nval;

/* Check the inherited status */
   if( *status != SAI__OK ) return;
   if (refkm) astClear(refkm, "KeyError");

/* Sort the supplied KeyMap is required. */
   if( sort ) astSetC( km, "SortBy", "KeyUp" );

/* Loop round all keys in the supplied KeyMap. */
   nkey = astMapSize( km );
   for( ikey = 0; ikey < nkey; ikey++ ) {
      key = astMapKey( km, ikey );

/* If the current entry is a nest KeyMap, get a pointer to it and call
   this function recursively to display it, modifying the prefix to add
   to each key so that it includes the key associated with the nest keymap. */
      if( astMapType( km, key ) == AST__OBJECTTYPE ) {
         astMapGet0A( km, key, &avalue );
         if (refkm) {
            if (! astMapGet0A(refkm, key, &refavalue)) {
               refavalue = (AstObject*) astKeyMap("");
            }
         }
         else {
            refavalue = NULL;
         }
         sprintf( newpref, "%s%s.", prefix, key );
         DisplayKeyMap( (AstKeyMap *) avalue, sort, newpref,
                        (AstKeyMap *) refavalue, fd, status );
         avalue = astAnnul( avalue );
         if (refavalue) refavalue = astAnnul(refavalue);

/* If the current entry is not a nested keymap, we display it now. */
      } else {

/* Get the vector length of the entry. */
         nval = astMapLength( km, key );

/* Get its value as a character string using the automatic type conversion
   provided by the KeyMap class, and format it, putting the supplied prefix
   at the start of the key. Note, the astMapGetC function formats a vector
   KeyMap entry as a comma-separated list enclosed in parentheses. */
         if( nval <= 1 ) {
            text = NULL;
            nc = 0;
            cvalue = "<undef>";
            astMapGetC( km, key, &cvalue );
            if( refkm ) {
               refcvalue = "<undef>";
               if( astMapGetC( refkm, key, &refcvalue ) &&
                   !strcmp( cvalue, refcvalue ) ) {
                  text = astAppendString( text, &nc, "- " );
               } else {
                  text = astAppendString( text, &nc, "+ " );
               }
            }
            text = astAppendStringf( text, &nc, "%s%s = %s", prefix, key, cvalue );
         }

/* Display the total text on standard output. */
         msgOut( "", text, status );

/* If required, write the total text to the log file. */
         if( fd ) fprintf( fd, "%s\n", text );
      }
   }
}

/*
 * Process an NDF history element and construct a KeyMay.
 *
 * This is intended as a handler routine for use with ndfHout,
 * it reads the supplied text and sets the global historyConfig
 * variable.
 */
void HistoryKeyMap(int n, char* const text[], int* status) {
   char patt_group[] = "Group:";
   char patt_cont[] = "   ";
   char patt_name[] = "CONFIG";
   char* buff = NULL;
   int nc = 0;
   char line[NDF__SZHIS + 1];
   char* p;
   char* q;
   int i;
   int groupstarting = 0;
   int groupcontinuing = 0;
   Grp* grp;
   size_t grpsize;
   int grpadded, grpflag;

/* Check the inherited status */

   if (*status != SAI__OK) return;

/* Loop over history text lines, copying each into line for editing
   and setting p to point at the start of line. */

   for (i = 0; i < n; i ++) {
      one_strlcpy(line, text[i], NDF__SZHIS + 1, status);
      p = line;

/* If a group is in progess, check that it continues, and if so
   add the line to the buffer, otherwise end the group. Since we
   only extract one group, we can leave processing buff to the end. */

      if (groupcontinuing) {
         if (! strncmp(patt_cont, p, strlen(patt_cont))) {
            p += strlen(patt_cont);
            while (*p == ' ') p++;
            while (p[strlen(p) - 1] == ' ') p[strlen(p) - 1] = '\0';
            buff = astAppendString(buff, &nc, p);
            continue;
         }
         else {
            groupcontinuing = 0;
         }
      }

/* If we didn't already detect a group to be starting, see whether
   one is starting now. */

      if (! groupstarting) {
         if (strncmp(patt_group, p, strlen(patt_group))) {
            continue;
         }
         else {
            groupstarting = 1;
            p += strlen(patt_group);
         }
      }

      while (*p == ' ') p ++;

      if (! *p) continue;

/* Group is starting, so check whether the group name is the one
   for which we are looking, and if so, start collecting the
   text in buff. */

      if (! strncmp(patt_name, p, strlen(patt_name))) {
         p += strlen(patt_name);
         while (*p == ' ') p ++;
         if (*p == '=') {
            p++;
            if (*p == '"') p++;
            while (*p == ' ') p++;
            while (p[strlen(p) - 1] == ' ') p[strlen(p) - 1] = '\0';
            buff = astAppendString(astFree(buff), &nc, p);
            groupcontinuing = 1;
         }
      }

      groupstarting = 0;
   }

/* Convert buff to a KeyMap and set the global variable historyConfig. */

   if (buff && *buff) {
      if (buff[strlen(buff) - 1] == '"') buff[strlen(buff) - 1] = '\0';
      grp = grpNew("CONFIG", status);

      p = buff;
      while (*p) {
         q = p;
         i = 0;
         groupcontinuing = 0;
         for (q = p; *q; q ++) {
            if (*q == '(' || *q == '[' || (*q == '\'' && i == 0)) i ++;
            else if (*q == ')' || *q == ']' || (*q == '\'' && i > 0)) i --;
            else if (*q == ',' && i == 0) {
               *q = '\0';
               groupcontinuing = 1;
               break;
            }
         }
         grpGrpex(p, GRP__NOID, grp, &grpsize, &grpadded, &grpflag, status);
         if (!groupcontinuing) break;
         p = q + 1;
      }

      kpg1Kymap(grp, &historyConfig, status);
      grpDelet(&grp, status);
      buff = astFree(buff);
   }
}
