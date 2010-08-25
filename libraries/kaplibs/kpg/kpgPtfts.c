
#include <string.h>

#include "star/hds.h"
#include "ast.h"
#include "ndf.h"
#include "ems.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "dat_err.h"
#include "kaplibs.h"

#define SZFITSCARD 80      /* Size of a FITS header card */
#define FITSSTR "80"       /* string representation of size of FITS */


/*
*+
*  Name:
*     kpgPtfts

*  Purpose:
*     Stores FITS header information into an NDF.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     kpgPtfts( int indf, AstFitsChan * fchan, int * status );

*  Description:
*     The routine stores the contents of an AST FitsChan into an
*     NDF by creating (or replacing) the FITS extension in the NDF.

*  Arguments:
*     indf = int (Given)
*        Identifier of NDF to receive the .FITS extension.
*     fchan = const AstFitsChan * (Given)
*        An AST pointer to a FitsChan which contains information about
*        the FITS header to be associated with the NDF.
*     status = int * (Given and Returned)
*        The global status.

*  Return Value:
*     Returns the status.

*  Notes:
*     - If a .MORE.FITS extension already exists it will be completely
*     replaced by this routine.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the
*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
*    Boston, MA  02111-1307  USA

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-NOV-2005 (TIMJ):
*        Original version.
*     25-APR-2006 (TIMJ):
*        Finish.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

int kpgPtfts( int indf, const AstFitsChan * fchan, int * status ) {

  char card[SZFITSCARD+1];            /* A single FITS header card */
  int  fitsdim[1];  /* dimensions of FITS extension */
  HDSLoc * fitsloc = NULL;  /* Locator to FITS extension */
  char * fpntr;             /* Pointer to mapped FITS header */
  unsigned int i;           /* Loop counter */
  AstFitsChan * lchan; /* Local copy of FitsChan */
  unsigned int ncards;      /* Number of header cards */
  size_t nchars;            /* Actual size of FITS extension */
  int * oldstat;    /* Current status watched by AST */
  int result;               /* Result from astFindFits */
  int there = 0;    /* Is FITS extension there? */
  void * vpntr;     /* dummy void pointer */

  if ( *status != SAI__OK ) return *status;

  /* First need to look for a FITS extension */
  ndfXstat( indf, "FITS", &there, status );

  /* Remove it if it exists */
  if (there) {
    ndfXdel( indf, "FITS", status );
  }

  /* Make sure that we are checking AST status */
  oldstat = astWatch( status );

  /* Get local cloned copy of the FitsChan since we promised not
     to modify the supplied FitsChan */
  lchan = astCopy( fchan );

  /* Find out how many cards are present in the FitsChan */
  ncards = astGetI( lchan, "Ncard" );

  /* Rewind the FitsChan */
  astClear( lchan, "Card" );

  /* Create FITS extension */
  fitsdim[0] = ncards;
  ndfXnew(indf, "FITS", "_CHAR*" FITSSTR, 1, fitsdim, &fitsloc, status );

  /* Loop over all cards, inserting into extension -
     vpntr shenanigans fix strict-aliasing warning if casting
     &fpntr directly to void** */
  datMapV( fitsloc, "_CHAR*" FITSSTR, "WRITE", &vpntr, &nchars, status );
  fpntr = vpntr;

  if (*status == SAI__OK) {
    if ( ncards != nchars ) {
      *status = SAI__ERROR;
      emsSetu( "DM", nchars );
      emsSetu( "SZ",  ncards );
      emsRep("KPG_PTFTS_ERR",
	     "Bizarre error whereby number of cards in mapped FITS header (^DM) differs from number requested (^SZ)", status );
    }
  }

  if (*status == SAI__OK) {
    for (i = 1; i <= ncards; i++) {
      result = astFindFits( lchan, "%f", card, 1 );
      if (result) {
	strncpy( fpntr, card, SZFITSCARD );
	fpntr += SZFITSCARD;
      } else {
	break;
      }
    }
  }

  /* Cleanup */
  datUnmap( fitsloc, status );
  datAnnul( &fitsloc, status );

  /* Annul the local copy */
  astAnnul( lchan );

  /* Reset AST status */
  astWatch( oldstat );

  if (*status != SAI__OK)
    emsRep(" ", "kpgPtfts: Error writing FITS information to NDF", status );

  return *status;
}
