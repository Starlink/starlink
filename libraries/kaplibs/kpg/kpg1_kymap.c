#include "f77.h"
#include "ast.h"
#include "star/grp.h"
#include "kaplibs_private.h"

F77_SUBROUTINE(kpg1_kymap)( INTEGER(IGRP), INTEGER(KEYMAP), INTEGER(STATUS) ) {
/*
*+
*  Name:
*     KPG1_KYMAP

*  Purpose:
*     Creates an AST KeyMap holding keyword/value pairs read from a GRP
*     group.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_KYMAP( IGRP, KEYMAP, STATUS )

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
*     The returned KeyMap will contain three entries with keys "gaussclumps",
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
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group of text strings to be analysed.
*     KEYMAP = INTEGER (Returned)
*        An AST pointer to the new KeyMap, or AST__NULL if an error occurs.
*        A valid pointer to an empty KeyMap will be returned if the supplied
*        group contains nothing but comments and blank lines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Vector elements should be separated by commas and enclosed within
*     parentheses (commas and closing parentheses can be included literally
*     in a vector element by preceeding them with a backslash).
*     - Component names must contain only alphanumerical characters,
*     underscores, plus and minus signs [a-zA-Z0-9_+\-]. White space
*     within keywords is ignored.
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
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     25-FEB-2010(DSB):
*        Document the "keyword=<def>" facility.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_INTEGER(IGRP)
   GENPTR_INTEGER(KEYMAP)
   GENPTR_INTEGER(STATUS)

   Grp *grp;
   AstKeyMap *keymap;
   int cstatus;

   F77_IMPORT_INTEGER( *STATUS, cstatus );

   grp = (Grp *) grpF2C( *IGRP, &cstatus );

   kpg1Kymp1( grp, &keymap, &cstatus );

   grp = grpFree( grp, &cstatus );

   F77_EXPORT_INTEGER( astP2I( keymap ), *KEYMAP );
   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
