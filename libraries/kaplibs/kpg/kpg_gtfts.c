#include "f77.h"
#include "kaplibs.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg_gtfts)( INTEGER(INDF), INTEGER(FCHAN), INTEGER(STATUS) ) {
/*
*+
*  Name:
*     KPG_GTFTS

*  Purpose:
*     Obtains FITS header information from an NDF.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG_GTFTS( INDF, FCHAN, STATUS )

*  Description:
*     The routine reads the FITS extension from an NDF and returns an
*     AST pointer to a FitsChan which contains this information. The
*     information may then be accessed using routines from the AST
*     library (SUN/211).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     FCHAN = INTEGER (Returned)
*        An AST pointer to a FitsChan which contains information about
*        the FITS headers associated with the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - It is the caller's responsibility to annul the AST pointer
*     issued by this routine (e.g. by calling AST_ANNUL) when it is no
*     longer required.
*     - If this routine is called with STATUS set, then a value of
*     AST__NULL will be returned for the FCHAN argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.
*     - Status is set to KPG__NOFTS if no FITS extension is found.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     4-JUL-2008 (DSB):
*        Original version.
*     15-JUL-2008 (TIMJ):
*        Pass in pointer to fchan
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(FCHAN)
   GENPTR_INTEGER(STATUS)

   AstFitsChan *fchan = NULL;
   int cstatus, indf;

   F77_IMPORT_INTEGER( *STATUS, cstatus );
   F77_IMPORT_INTEGER( *INDF, indf );

   (void) kpgGtfts( indf, &fchan, &cstatus );

   *FCHAN = astP2I( fchan );
   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
