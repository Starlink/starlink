/*+
* Name:
*   gethdstype

* Purpose:
*    To get the HDS type corresponding to an IDL type

* Language:
*    C

* Invocation:
*    hdstype = gethdstype( idltype )

* Arguments:
*    idltype = IDL_LONG (Given)
*       The IDL type

* Returned Value:
*    hdstype = char *
*       A pointer to a string containing the HDS type.

* Description:
*    For primitives the corresponding type string is returned
*    For structures, "STRUCTURE" is returned

* Pitfalls:
*    For strings, only "_CHAR" is returned - the size component of the
*    type must be added before it is used.

* Notes:
*    -  {noted_item}
*    [routine_notes]...

* External Routines Used:
*    None

* Implementation Deficiencies:
*    -  {deficiency}
*    [routine_deficiencies]...

* Copyright:
*    Copyright (C) 1999 Central Laboratory of the Research Councils

* Authors:
*    AJC: A.J.Chipperfield (Starlink, RAL)
*    {enter_new_authors_here}

* History:
*    13-OCT--1999 (AJC):
*       Original version.
*    {enter_further_changes_here}

* Bugs:
*    -  {description_of_bug}
*    {note_new_bugs_here}
*-
*/

#include <stdio.h>
#include "export.h"
static char *hdstypes[]={"UNDEFINED","_UBYTE","_WORD","_INTEGER",
   "_REAL","_DOUBLE","COMPLEX","_CHAR","",
   "DPCOMPLEX","POINTER","OBJECT"};

char *gethdstype( UCHAR idltype ) {

   return hdstypes[(int)idltype];

}
