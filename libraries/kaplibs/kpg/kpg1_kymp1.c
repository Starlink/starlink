#include "f77.h"
#include "mers.h"
#include "sae_par.h"
#include "ast.h"
#include "star/grp.h"
#include "kaplibs_private.h"

void kpg1Kymp1( const Grp *igrp, AstKeyMap **keymap, int *status ){
/*
*+
*  Name:
*     kpg1Kymp1

*  Purpose:
*     Creates an AST KeyMap holding keyword/value pairs read from a GRP
*     group.

*  Language:
*     C.

*  Invocation:
*     void kpg1Kymp1( const Grp *igrp, AstKeyMap **keymap, int *status )

*  Description:
*     This function checks each non-comment, non-blank line in the supplied
*     GRP group. An error is reported if any such lines do not have the
*     form "keyword = value", where the keyword name can be a hierarchical
*     list of component names separated by dots. The returned KeyMap has
*     an entry for each component name found at the start of any keyword
*     name. The value associated with the entry will either be a
*     primitive value (if the keyword name contained no other components)
*     or another KeyMap (if the keyword name contained other components).
*
*     For example, consider a group containing the following lines:
*
*     gaussclumps.epsilon = (0.001,0.002)
*     gaussclumps.contrast = 2.3
*     clumpfind.naxis = 2
*     clumpfind.deltat = 2.0
*     method = gaussclumps
*
*     The returned KeyMap will contain 3 entries with keys "gaussclumps",
*     "clumpfind" and "method". The value associated with the "gaussclumps"
*     entry will be another KeyMap containing keys "epsilon" (a primitive
*     vector entry containing the values 0.001 and 0.002) and "contrast"
*     (a primitive scalar entry with value "2.3"). The value associated with
*     the "clumpfind" entry will be another KeyMap containing keys "naxis"
*     and "deltat", which will have primitive scalar values "2" and "2.0".
*     The value associated with the "method" entry will be the primitive
*     scalar value "gaussclumps".
*
*     Assigning the value "<def>" (case insensitive) to a keyword has the
*     effect of removing the keyword from the KeyMap. For example:
*
*     ^global.lis
*     method = <def>
*
*     reads keyword values from the file "global.lis", and then ensures
*     that the KeyMap does not contain a value for keyword "method". The
*     calling application should then usually use a default value for
*     "method".
*
*     Assigning the value "<undef>" (case insensitive) to a keyword
*     has the effect of forcing the value to be undefined. This can
*     be useful in defining defaults where the keymap is locked
*     after being populated.

*  Arguments:
*     igrp
*        A GRP identifier for the group of text strings to be analysed.
*     keymap
*        A location at which to return a pointer to the new KeyMap, or
*        NULL if an error occurs. A valid pointer to an empty KeyMap will be
*        returned if the supplied group contains nothing but comments and
*        blank lines.
*     status
*        Pointer to the global status variable.

*  Notes:
*     - Vector elements should be separated by commas and enclosed within
*     parentheses (commas and closing parentheses can be included
*     literally in a vector element by preceeding them with a backslash).
*     - This function provides a private implementation for the public
*     KPG1_KYMAP Fortran routine and kpg1Kymap C function.
*     - Component names must contain only alphanumerical characters,
*     underscores, plus and minus signs [a-zA-Z0-9_+\-],
*     - Any lower case characters contained in a component name will be
*     translated to the upper case equivalent.
*     - If the last non-blank character in a value is a backslash ("\"),
*     the backslash will be reomved, together with any white space
*     following it, and the entire next line will be appended to the value.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     15-JUL-2008 (TIMJ):
*        Tweak to GRP C API.
*     25-FEB-2010(DSB):
*        Document the "keyword=<def>" facility.
*     2010-05-05 (TIMJ):
*        Document the "keyword=<undef>" facility.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   char *accumulation;          /* Sum of concatenated strings */
   char *pname;                 /* Pointer to pass to grpGet */
   char name[ GRP__SZNAM + 1 ]; /* A single string from the group */
   int acclen;                  /* Length of accumulation exc. trailing null */
   int i;                       /* Index into supplied group */
   int *old_status;             /* Pointer to original status variable */
   int size;                    /* No. of strings in supplied group */
   int thislen;                 /* Length of current string */

/* Initialise the returned pointer */
   *keymap = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Make AST use the Fortran status variable. */
   old_status = astWatch( status );

/* Create a new empty KeyMap. */
   *keymap = astKeyMap( " " );

/* Get the number of strings in the group. */
   size = grpGrpsz( igrp, status );

/* We need to pass a pointer to the "name" variable to grpGet */
   pname = name;

/* Indicate the current accumulation of concatenated lines is null. */
   acclen = 0;
   accumulation = NULL;

/* Loop round all the strings in the group. */
   for( i = 0; i < size && *status == SAI__OK; i++ ) {

/* Get the i'th string from the group. Note, grp uses 1-based indices. */
      grpGet( igrp, i + 1, 1, &pname, GRP__SZNAM + 1, status );

/* Get the used length of this string. */
      thislen = astChrLen( name );

/* Skip blank lines */
      if( thislen > 0 ) {

/* Eliminate any trailing white space by terminating the new string after
   its last non-white character. */
         name[ thislen ] = 0;

/* Append this line to the current accumulation of lines (this will be a
   null string unless the previous line ended with a backslash). */
         accumulation = astAppendString( accumulation, &acclen, name );

/* If the last non-white character in the accumulation is "\", remove it and
   pass on to read the next string from the group. */
         if( accumulation[ acclen - 1 ] == '\\' ) {
            acclen--;
            accumulation[ acclen ] = 0;

/* Otherwise (i.e. if the accumulation does not end with a backslash) the
   accumulation of continuation lines is complete, so parse the total
   accumulation. */
         } else {
            kpg1Kymp2( accumulation, *keymap, status );

/* Reset the length of the accumulation back to zero in order to start a
   new accumulation with the next string form the group. We retain the
   memory since it can be used again for the next accumulation. */
            acclen = 0;
         }
      }
   }

/* If the current accumulation has not yet been parsed (e.g. because the
   last string in the group ended with a backslash), parse it now. */
   if( acclen ) kpg1Kymp2( accumulation, *keymap, status );

/* Free resources. */
   accumulation = astFree( accumulation );

/* If an error occurred, annul the returned KeyMap. */
   if( *status != SAI__OK ) *keymap = astAnnul( *keymap );

/* Make AST use its original status variable. */
   astWatch( old_status );

}
