#include "star/hds.h"
#include "star/hds_fortran.h"
#include "f77.h"
#include "kaplibs.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg1_ch2pm)( CHARACTER(LOC), INTEGER(POLYMAP),
                            INTEGER(STATUS) TRAIL(LOC) ) {
/*
*+
*  Name:
*     KPG1_CH2PM

*  Purpose:
*     Creates an AST PolyMap describing a Starlink POLYNOMIAL HDS structure.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_CH2PM( LOC, POLYMAP, STATUS )

*  Description:
*     This routine creates an AST PolyMap that implements the
*     polynomial transformation described by a supplied Starlink
*     POLYNOMIAL structure (see SGP/38).

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        An HDS locator for the POLYNOMIAL structure.
*     POLYMAP = INTEGER (Returned)
*        An AST pointer to the new PolyMap.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The returned PolyMap has a defined forward transformation
*     (equivalent to the supplied POLYNOMIAL), but no inverse
*     transformation.
*     - Both CHEBYSHEV and SIMPLE variants of the POLYNOMIAL structure
*     are supported. But currently only 1- or 2- dimensional Chebyshev
*     polynomials can be handled. An error is reported for Chebyshev
*     polynomials of higher dimensionality.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     11-NOV-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(POLYMAP)
   GENPTR_INTEGER(STATUS)

   HDSLoc *locator_c = NULL;
   AstPolyMap *polymap;
   int cstatus;

   F77_IMPORT_INTEGER( *STATUS, cstatus );
   datImportFloc( LOC, LOC_length, &locator_c, &cstatus );

   kpg1Ch2pm( locator_c, &polymap, &cstatus );

   F77_EXPORT_INTEGER( astP2I( polymap ), *POLYMAP );
   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
