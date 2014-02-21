#include "f77.h"
#include "ast.h"
#include "star/grp.h"
#include "kaplibs.h"

F77_INTEGER_FUNCTION(kpg1_getoutline)( INTEGER(INDF), INTEGER(STATUS) ) {
/*
*+
*  Name:
*     KPG1_GETOUTLINE

*  Purpose:
*     Retrieve an STC polygon describing the spatial extent of an NDF.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     RESULT = KPG1_GETOUTLINE( Indf, STATUS )

*  Description:
*     If The NDF contains an OUTLINE extension, it is expected to be a
*     character array containing an STC-S description of a polygon. If
*     this is the case, the polygon is returned as the function value.
*     Otherwise a NULL pointer is returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     A pointer to an AST Region, or AST__NULL if no Region can be created.

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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
*     21-FEB-2014 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   AstRegion *result;
   int status, indf;

   F77_IMPORT_INTEGER( *STATUS, status );
   F77_IMPORT_INTEGER( *INDF, indf );

   result = kpgGetOutline( indf, &status );

   F77_EXPORT_INTEGER( status, *STATUS );
   return astP2I( result );

}
