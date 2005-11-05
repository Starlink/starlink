/*
*+
*  Name:
*     smf_fits_crchan

*  Purpose:
*     Helper routine for creating an AST FitsChan from FITS headers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smf_fits_crchan( int indf, AstFitsChan **fchan, int *status );

*  Arguments:
*     indf = int (Given)
*        NDF identifier
*     fchan = AstFitsChan** (Returned)
*        AST Fits Chan containing the FITS information
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine for reading FITS information from an NDF


*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-11-04 (AGG):
*        Initial test version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "ast.h"
#include "ndf.h"
#include "star/hds.h"
#include "sae_par.h"
#include <stdlib.h>
#include "mers.h"
#include <string.h>

int curindex = 0;
char *card = NULL;
int step = 0;
int maxfits;

const char* astsource ( );

void smf_fits_crchan( int nfits, char * headrec, AstFitsChan ** fits, int *status ) {


  if ( headrec[80] == '\0' ) {
    step = 81;
  } else {
    step = 80;
  }

  card = headrec;
  maxfits = nfits;

  *fits = astFitsChan( astsource, NULL, "");

  return;

}

const char* astsource () {

  char * hdritem = NULL;

  if (curindex == maxfits) {
    return NULL;
  }

  hdritem = astMalloc(80+1);
  if (hdritem == NULL) return NULL;

  strncpy( hdritem, card, 80 );
  curindex++;
  card += step;
  
  return hdritem;
}
