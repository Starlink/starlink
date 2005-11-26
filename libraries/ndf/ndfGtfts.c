
#include <string.h>

#include "star/hds.h"
#include "ast.h"
#include "ndf.h"
#include "ndf1.h"
#include "mers.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "dat_err.h"

const int SZFITSCARD = 80; /* Size of a FITS header card */
#define FITSSTR "80"       /* string representation of size of FITS */

/*
*+
*  Name:
*     ndfGtfts

*  Purpose:
*     Obtain FITS header information from an NDF

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL NDF_GTFTS( INDF, FCHAN, STATUS )
*     ndfGtfts( int indf, AstFitsChan ** fchan, int * status );

*  Description:
*     The routine reads the FITS extension from an NDF and returns an
*     AST pointer to a FitsChan which contains this information. The 
*     information may then be accessed using routines from the AST 
*     library (SUN/211).

*  Arguments:
*     indf = int (Given)
*        NDF identifier.
*     fchan = AstFitsChan ** (Returned)
*        An AST pointer to a FitsChan which contains information about
*        the FITS headers associated with the NDF.
*     status = int * (Given and Returned)
*        The global status.

*  Return Value:
*     Returns the status.

*  Notes:
*     - It is the caller's responsibility to annul the AST pointer
*     issued by this routine (e.g. by calling AST_ANNUL) when it is no
*     longer required. The NDF_ system will not perform this task
*     itself.
*     - If this routine is called with STATUS set, then a value of
*     AST__NULL will be returned for the FCHAN argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.
*     - Status is set to NDF__NOFTS if no FITS extension is found.

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-NOV-2005 (TIMJ):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

int ndfGtfts( int indf, AstFitsChan ** fchan, int * status ) {

  char   *card;               /* Pointer to start of current card */
  HDSLoc *fitsloc = NULL;     /* FITS HDS Locator in extension */
  hdsdim fitsdim[DAT__MXDIM]; /* Dimensionality of FITS extension */
  char   *fpntr;              /* Pointer to the mapped FITS header */
  int    i;                   /* Loop counter */
  size_t ncards;              /* Number of header cards in extension */
  size_t nchars;              /* Number of characters in extension */
  size_t nmapcards;           /* Number of cards mapped */
  int    ndim;                /* Number of dimensions in FITS array */
  int    *oldstat;            /* Current status watched by AST */
  int    there = 0;           /* Is FITS extension there? */
  char   type[DAT__SZTYP+1];  /* Data type of the FITS extension */

  /* make sure the fits chan is set to NULL on exit with bad status */
  *fchan = AST__NULL;

  if ( *status != SAI__OK ) return *status;

  /* First need to look for a FITS extension */
  ndfXstat( indf, "FITS", &there, status );

  if ( *status == SAI__OK ) {
    if (!there) {
      *status = NDF__NOFTS;
      errRep( "NDF_GTFTS_NOF", "FITS extension is not present in NDF",
	      status );
    }
  }

  /* Get the locator to the FITS extension */
  ndfXloc( indf, "FITS", "READ", &fitsloc, status );

  /* Get the data type */
  datType( fitsloc, type, status );

  if ( *status == SAI__OK ) {
    if (strcmp(type, "_CHAR*" FITSSTR) != 0 ) {
      *status = DAT__TYPIN;
      msgSetc( "TYP", type );
      errRep( "NDF_GTFTS_TYP", "Data type of FITS extension is '^TYP' not '_CHAR*" FITSSTR "'", status );
    }
  }

  /* Determine the dimensionality of the FITS extension */
  datShape( fitsloc, DAT__MXDIM, fitsdim, &ndim, status );

  if ( *status == SAI__OK ) {
    if ( ndim != 1 ) {
      *status = DAT__DIMIN;
      msgSeti( "NDIM", ndim );
      errRep( "NDF_GTFTS_DIM", "Number of dimensions in FITS extension = ^NDIM but should be 1", status );
    }
  }

  /* Get number of FITS entries - should match fitsdim[0] */
  datSize( fitsloc, &ncards, status );

  if ( *status == SAI__OK ) {
    if ( ncards != fitsdim[0] ) {
      *status = DAT__DIMIN;
      msgSeti( "DM", (int)fitsdim[0] );
      msgSeti( "SZ", (int)ncards );
      errRep( "NDF_GTFTS_SIZ","Bizarre error whereby the first dimension of the FITS extension (^DM) does not equal the size of the extension (^SZ)", status);
    }
  }

  /* Use datMapV to map the entire FITS array, then step through
     it 80 characters at a time until we have done all the cards.
     Note that there is no nul-terminator so we can not use
     astPutCards directly */
    
  datMapV( fitsloc, "_CHAR*" FITSSTR, "READ", &fpntr, &nchars, status );

  if ( *status == SAI__OK ) {
    if ( ncards != nchars ) {
      *status = DAT__DIMIN;
      msgSeti( "DM", (int)nchars);
      msgSeti( "SZ", (int)ncards );
      errRep( "NDF_GTFTS_SIZ2","Bizarre error whereby the number of elements mapped in the FITS extension (^DM) does not equal the size of the extension (^SZ)", status);
    }
  }

  /* Do not bother with AST stuff if status is bad */
  if ( *status == SAI__OK ) {

    /* Associate AST status with our status */
    oldstat = astWatch( status );

    /* Create a new FitsChan */
    *fchan = astFitsChan( NULL, NULL, "" );
  
    /* Extract headers 80 characters at a time. No nul-termination
       but astPutFits guarantees to only read 80 characters */
    card = fpntr;
    for (i = 0; i < ncards; i++ ) {
      astPutFits( *fchan, card, 0 );
      card += SZFITSCARD;
    }

    /* Rewind the FitsChan */
    astClear( *fchan, "Card" );

    /* if status is bad annul the Fits Chan */
    if ( *status != SAI__OK ) astAnnul( *fchan );

    /* Reset AST status */
    astWatch( oldstat );

  }

  /* Clean up */
  datUnmap( fitsloc, status );
  datAnnul( &fitsloc, status );

  printf("Status = %d\n", *status );

  /* Report wrapper error message */
  if ( *status != SAI__OK ) {
    errRep( "NDF_GTFTS_ERR",
	    "NDF_GTFTS: Error obtaining FITS header from an NDF.",
	    status );
    ndf1Trace( "NDF_GTFTS", status );
  }

  return *status;
}
