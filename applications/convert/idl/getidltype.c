/*+
* Name:
*    getidltype

*  Purpose:
*     To get the IDL type corresponding to an HDS primitive type

*  Language:
*     C

*  Invocation:
*     idltype = getidltype( hdstype )

*  Arguments:
*     hdstype = char[DAT__SZTYP] (Given)
*        The HDS type

*  Returned Value:
*     idltype = UCHAR
*        The IDL type

*  Description:
*     The IDL type corresponding to the HDS type in the Starlink/IDL
*     data conversion process is returned as a UCHAR - that is the form
*     suitable to place in an IDL_VARIABLE structure.
*
*     IDL_TYP_UNDEF is returned if the type is not primitive or not handled.

*  Pitfalls:

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  External Routines Used:
*     None

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     29-FEB-2000(AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/

#include <stdio.h>
#include <string.h>
#include "export.h"

int i;   /* array index */
char *hdstypes[]={"_UBYTE", "_WORD", "_INTEGER", "_REAL",
                  "_LOGICAL", "_DOUBLE", "_CHAR", "_UWORD",
                  "_BYTE", NULL};
UCHAR idltypes[]={IDL_TYP_BYTE, IDL_TYP_INT, IDL_TYP_LONG, IDL_TYP_FLOAT,
                  IDL_TYP_LONG, IDL_TYP_DOUBLE, IDL_TYP_STRING, IDL_TYP_LONG,
                  IDL_TYP_INT};

UCHAR getidltype( char *hdstype ) {

   for ( i=0; hdstypes[i]!=NULL; i++ )
      if ( !strcmp( hdstypes[i], strtok( hdstype, "*" ) ) )
         return idltypes[i];

   return IDL_TYP_UNDEF;
}
