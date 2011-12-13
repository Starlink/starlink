#include "f77.h"
#include "kaplibs.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg_ptfts)( INTEGER(INDF), INTEGER(FCHAN), INTEGER(STATUS) ) {
/*
*+
*  Name:
*     KPG_PTFTS

*  Purpose:
*     Stores FITS header information into an NDF.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG_PTFTS( INDF, FCHAN, STATUS )

*  Description:
*     The routine stores the contents of an AST FitsChan into an
*     NDF by creating (or replacing) the FITS extension in the NDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier of NDF to receive the .FITS extension.
*     FCHAN = INTEGER (Given)
*        An AST pointer to a FitsChan which contains information about
*        the FITS header to be associated with the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If a .MORE.FITS extension already exists it will be completely
*     replaced by this routine.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     4-JUL-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(FCHAN)
   GENPTR_INTEGER(STATUS)

   AstFitsChan *fchan;
   int cstatus, indf;

   F77_IMPORT_INTEGER( *STATUS, cstatus );
   F77_IMPORT_INTEGER( *INDF, indf );
   fchan = astI2P( *FCHAN );

   (void) kpgPtfts( indf, fchan, &cstatus );

   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
