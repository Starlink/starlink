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
*     Create an AST KeyMap holding keyword/value pairs read from a GRP
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
*     gaussclumps.epsilon = 0.001
*     gaussclumps.contrast = 2.3
*     clumpfind.naxis = 2
*     clumpfind.deltat = 2.0
*     method = gaussclumps
*
*     The returned KeyMap will contain 3 entries with keys "gaussclumps",
*     "clumpfind" and "method". The value associated with the "gaussclumps"
*     entry will be another KeyMap containing keys "epsilon" and "contrast", 
*     which will have primitive values "0.001" and "2.3". The value 
*     associated with the "clumpfind" entry will be another KeyMap 
*     containing keys "naxis" and "deltat", which will have primitive 
*     values "2" and "2.0". The value associated with the "method" entry 
*     will be the primitive value "gaussclumps".

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
*     - Component names must contain only alphanumerical characters,
*     underscores, plus and minus signs [a-zA-Z0-9_+\-]. White space
*     within keywords is ignored.
*     - Any lower case characters contained in a component name will be
*     translated to the upper case equivalent.
*     - If the last non-blank character in a value is a backslash ("\"),
*     the backslash will be reomved, together with any white space
*     following it, and the entire next line will be appended to the value.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_INTEGER(IGRP)
   GENPTR_INTEGER(KEYMAP)
   GENPTR_INTEGER(STATUS)

   Grp igrp;
   AstKeyMap *keymap;
   int cstatus;

   F77_IMPORT_INTEGER( *STATUS, cstatus );

   /* Copy grp id into blank Grp struct */
   grp1Setid( &igrp, *IGRP, &cstatus );

   kpg1Kymp1( &igrp, &keymap, &cstatus );

   F77_EXPORT_INTEGER( astP2I( keymap ), *KEYMAP );
   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
