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
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-11-04 (AGG):
*        Initial test version
*     2005-11-07 (TIMJ):
*        Fix logic that ditinguishes sc2store from datGetc
*     2005-11-29 (TIMJ):
*        Use astPutFits rather than a callback (much easier)
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

const char* astsource ( );

void smf_fits_crchan( int nfits, char * headrec, AstFitsChan ** fits, int *status ) {
  size_t len;
  
  /* Sc2store header records are each null terminated at the last non-whitespace
     character so we need to search for a null in the first 81 characters to
     distinguish this from the "read a block" approach. The quick way to do this
     is to use strlen since we know that all the strings are terminated somewhere.
  */

  char *card = NULL;
  int i;
  int step = 0;

  len = strlen( headrec );

  if ( len <= 81 ) {
    /* individually terminated or a single string */
    step = 81;
  } else {
    step = 80;
  }

  /* Create the empty fitschan */
  *fits = astFitsChan( NULL, NULL, "" );

  /* Fill it */
  card = headrec;
  for (i = 0; i < nfits; i++ ) {
    astPutFits( *fits, card, 0 );
    card += step;
  }

  return;

}
