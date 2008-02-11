/*
*+
*  Name:
*     gsdac_wrtData

*  Purpose:
*     Write out the converted GSD2ACSIS file.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_wrtData ( const gsdac_gsd_struct *gsd,
*                     const unsigned int nSteps,
*                     char *directory, int *status );

*  Arguments:
*     gsd = const gsdac_gsd_struct* (Given)
*        GSD file access parameters
*     nSteps = const unsigned int* (Given)
*        Number of time steps
*     directory = char* (Given)
*        Directory to write the file
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Initializes the ACSIS directory for writing, writes a file for
*     each subsystem, then closes the files and writes the FITS 
*     headers.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-02-05 (JB):
*        Original.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "gsd.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libacsis/specwrite.h"
#include "libsmf/smf.h"
#include "libgsd/gsdac.h"
#include "libgsd/gsdac_struct.h"

#include "jcmt/state.h"

#define FUNC_NAME "gsdac_wrtData"

#define MAXFITS 80
#define MAXRECEP 8  
#define MAXSUBSYS 16

void gsdac_wrtData ( const struct gsdac_gsd_struct *gsd, 
                     const unsigned int nSteps,
                     const char *directory, int *status )
{

  /* Local variables */
  char backend[MAXFITS];      /* name of the backend */
  double centreFreq;          /* centre frequency */
  const AstFitsChan *fitschan[nSteps];  /* Array of FITS headers */
  char *focalStation = NULL;  /* focal station of the instrument */
  float fPlaneX[MAXRECEP]; 
  float fPlaneY[MAXRECEP];
  double gsdObsNum;           /* GSD observation number */
  double gsdUTDate;           /* GSD UT date */
  int i;                      /* loop counter */
  double mem;                 /* amount of memory for spectrum */
  unsigned int nChans;        /* number of channels in each subsystem */
  unsigned int nRecep;        /* number of receptors participating in 
                                 this observation */
  unsigned int nSubsys;       /* number of subsystems */
  unsigned int nTotChans;     /* total number of channels in observation */
  unsigned int obsNum;        /* current observation number */
  char *OCSConfig = NULL;     /* OCS configuration XML */
  JCMTState *record = NULL;   /* JCMT state information for the 
                                 current spectrum */
  char *recepNames[MAXRECEP]; /* names of the receptors */
  ACSISSpecHdr *specHdr;      /* ACSIS spectrum-specific information */
  unsigned long specIndex;    /* index into spectral data */
  float *spectrum = NULL;     /* single spectrum data */
  unsigned int spectrumSize;  /* size of spectrum data */
  unsigned int stepNum;       /* current step */
  unsigned int subsysNum;     /* subsystem used for the current spectrum */
  unsigned int utDate;        /* UT date in YYYYMMDD format */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the UTDate and convert to YYYYMMDD format. */
  msgOutif(MSG__VERB," ", 
	   "Checking UTDate to retrieve observation number", status);

  gsdac_get0d ( gsd, "C3DAT", &gsdUTDate, status );

  utDate = (int)( ( gsdUTDate + 0.00001 ) * 10000.0 );

  /* If the UTDate is prior to 20030202, prompt the user for the observation
     number.  Otherwise, use the value in NOBS. */
  if ( utDate < 20030202 ) {
    parGet0i ( "OBSNUM", &obsNum, status ); 
  } else {
    gsdac_get0d ( gsd, "C1SNO", &gsdObsNum, status );
    if ( *status == SAI__OK ) {
      obsNum = (int)gsdObsNum;
    }
  }

  /* Get the number of subsystems in this observation. */
  gsdac_get0i ( gsd, "C3NRS", &nSubsys, status );

  /* Get the total number of channels in this observation. */
  gsdac_get0i ( gsd, "C3NCH", &nTotChans, status );

  /* Get the number of receptors. */
  gsdac_get0i ( gsd, "C3NFOC", &nRecep, status );

  /* Get the centre frequency to determine receptor names. */
  gsdac_getElemd ( gsd, "C12CF", 0, &centreFreq, status );

  /* Get the size of the data array. */
  gsdac_getArraySize ( gsd, "C13DAT", &spectrumSize, status ); 

  /* Get the focal station. */
  focalStation = "DIRECT";

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( "", "Error retrieving GSD headers", status );
    return;
  }

  for ( i = 0; i < nRecep; i++ ) { 
    recepNames[i] = smf_malloc ( 2, sizeof( char ), 0, status );
    fPlaneX[i] = 0.0;
    fPlaneY[i] = 0.0;
  }

  /* Check to make sure we have the right number of receptors for
     this frontend, and copy the receptor names. */
  if ( centreFreq < 290.0 ) {

    if ( nRecep != 1 ) {
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Front end is receiver A but has seems to have more than 1 receptor", status );
      return;
    }

    strncpy ( recepNames[0], "A", 1 );

  } else if ( centreFreq < 395.0 ) {

    if ( nRecep != 2 ) {
      *status = SAI__ERROR;
      errRep ( "", "Front end is receiver B but does not have 2 receptors", 
               status ); 
      return; 
    }
    
    strncpy ( recepNames[0], "BA", 2 );
    strncpy ( recepNames[1], "BB", 2 );   

  } else if ( centreFreq < 600.0 ) {

    if ( nRecep != 2 ) {
      *status = SAI__ERROR;
      errRep ( "", "Front end is receiver C but does not have 2 receptors", 
               status ); 
      return; 
    }
    
    strncpy ( recepNames[0], "CA", 2 );
    strncpy ( recepNames[1], "CB", 2 ); 
   
  } else if ( centreFreq < 750 ) {

    if ( nRecep != 2 ) {
      *status = SAI__ERROR;
      errRep ( "", "Front end is receiver D but does not have 2 receptors", 
               status ); 
      return; 
    }

    strncpy ( recepNames[0], "DA", 2 );
    strncpy ( recepNames[1], "DB", 2 );

  } else {
    *status = SAI__ERROR;
    errRep ( "", "Couldn't obtain receptor names.", status ); 
    return; 
  }

  /* Determine how much memory we need and set the memory
     allocation in specwrite accordingly.  acsSpecSetMem wants
     to know how many bytes to allocate. */
  //mem = (double)spectrumSize * sizeof (float) / (double)( nSubsys );
  //acsSpecSetMem ( mem,status );   

  msgOutif(MSG__VERB," ", 
	     "Preparing file writing system", status); 

  acsSpecOpenTS ( directory, utDate, obsNum, nRecep, nSubsys, 
                  recepNames, focalStation, fPlaneX, fPlaneY,
                  OCSConfig, status );

  /* Get the name of the backend. */
  gsdac_get0c ( gsd, "C1BKE", backend, status );

  if ( *status == SAI__OK ) {

    /* Truncate the name of the backend. */
    cnfImprt ( backend, 16, backend );

    /* Set the backendFlag. */
    if ( strncmp ( backend, "DAS", 3 ) == 0 )
      acsSpecSetBackend ( ACS__BACKEND_DAS, status );
    else if ( strncmp ( backend, "AOSC", 4 ) == 0 )
      acsSpecSetBackend ( ACS__BACKEND_AOS, status );
    else {
      *status = SAI__ERROR;
      msgSetc ( "BACKEND", backend );
      errRep ( "", "Backend ^BACKEND not supported", status );
      return;

    } 
  }

  /* Allocate memory for JCMTState, SpecHdr, and data. */
  record = smf_malloc ( 1, sizeof ( *record ), 0, status );
  specHdr = smf_malloc ( 1, sizeof ( *specHdr ), 0, status );
                 
  /* Retrieve the full spectral data and flag bad values. */
  spectrum = smf_malloc ( spectrumSize, sizeof( float ), 0, status );
  gsdac_get1r ( gsd, "C13DAT", spectrum, status );

  if ( *status == SAI__OK ) {

    for ( i = 0; i < spectrumSize; i++ ) {
      if ( spectrum[i] == 9999 ) spectrum[i] = VAL__BADR;
    }
  }  

  /* Iterate through each time step. */
  for ( stepNum = 0; stepNum < nSteps; stepNum++ ) {

    specIndex = stepNum * ( spectrumSize / nSteps );

    /* Fill JCMTState. */
    gsdac_putJCMTStateC ( gsd, stepNum, record, status );  

    /* For each subsystem, write the files. */
    for ( subsysNum = 1; subsysNum <= nSubsys; subsysNum++ ) {

      /* Get the number of channels. */
      gsdac_getElemi ( gsd, "C3LSPC", subsysNum-1, &nChans, status );

      /* Get the subsystem-dependent JCMTState values. */
      gsdac_putJCMTStateS ( gsd, stepNum, subsysNum, record, status );

      /* Get the ACSIS SpecHdr. */
      gsdac_putSpecHdr ( gsd, nSteps, stepNum, subsysNum, record, specHdr, status );

      msgOutif(MSG__VERB," ", "Writing data", status); 

      /* Write a spectrum to the file. */
      acsSpecWriteTS( subsysNum, nChans, &(spectrum[specIndex]), 
                      record, specHdr, status );

      /* Initialize the astFitsChan for this file. */
      fitschan[ ( stepNum * nSubsys ) + ( subsysNum - 1 ) ] = astFitsChan ( NULL, NULL, "" );

      /* Fill the FITS headers. */
      gsdac_putFits ( gsd, nSubsys, subsysNum, obsNum, utDate, nChans, 
                      nSteps, backend, nRecep, recepNames, record, 
                      fitschan[ ( stepNum * nSubsys ) + ( subsysNum - 1 ) ], status ); 

      specIndex = specIndex + nChans;

    }

  }

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( "", "Error writing data", status );
    return;
  }  

  msgOutif(MSG__VERB," ", 
	   "Closing new file(s)", status);

  /* Close the file. */
  acsSpecCloseTS ( fitschan, 0, status );

  /* Free allocated memory. */
  smf_free ( record, status );
  smf_free ( spectrum, status );

  for ( i = 0; i < nRecep; i++ ) { 
    smf_free ( recepNames[i], status );
  }


}

