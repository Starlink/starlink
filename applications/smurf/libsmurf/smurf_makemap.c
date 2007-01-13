/*
*+
*  Name:
*     MAKEMAP

*  Purpose:
*     Top-level MAKEMAP implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_makemap( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the MAKEMAP task.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s)
*     PIXSIZE = REAL (Read)
*          Pixel size in output image, in arcsec
*     OUT = NDF (Write)
*          Output file

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (EC):
*        Clone from smurf_extinction
*     2005-12-16 (EC):
*        Working for simple test case with astRebinSeq 
*     2006-01-04 (EC):
*        Properly setting rebinflags
*     2006-01-13 (EC):
*        Automatically determine map size
*        Use VAL__BADD for pixels with no data in output map
*     2006-01-25 (TIMJ):
*        Replace malloc with smf_malloc.
*     2006-01-25 (TIMJ):
*        sc2head is now embedded in smfHead.
*     2006-01-27 (TIMJ):
*        - Try to jump out of loop if status bad.
*        - sc2head is now a pointer again
*     2006-02-02 (EC):
*        - Broke up mapbounds/regridding into subroutines smf_mapbounds and
*          smf_rebinmap
*        - fits header written to output using ndfputwcs
*     2006-03-23 (AGG):
*        Update to take account of new API for rebinmap
*     2006-03-23 (DSB):
*        Guard against null pointer when reporting error.
*     2006-04-21 (AGG):
*        Now calls sky removal and extinction correction routines.
*     2006-05-24 (AGG):
*        Check that the weights array pointer is not NULL
*     2006-05-25 (EC):
*        Add iterative map-maker + associated command line parameters
*     2006-06-24 (EC):
*        Iterative map-maker parameters given in CONFIG file
*     2006-08-07 (TIMJ):
*        GRP__NOID is not a Fortran concept.
*     2006-08-21 (JB):
*        Write data, variance, and weights using smfData structures
*     2006-08-22 (JB):
*        Add odata for output, add smf_close_file for odata.
*     2006-10-11 (AGG):
*        - Update to new API for smf_open_newfile, remove need for dims array
*        - Remove calls to subtract sky and correct for extinction
*     2006-10-12 (JB):
*        Use bad bolometer mask if supplied; add usebad flag
*     2006-12-18 (AGG):
*        Fix incorrect indf declaration, delete ogrp if it exists
*     2007-01-12 (AGG):
*        Add SYSTEM parameter for specifying output coordinate system
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council and the University of British Columbia. All Rights
*     Reserved.

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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"


/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "smurf_typ.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smurf_makemap"
#define TASK_NAME "MAKEMAP"
#define LEN__METHOD 20

void smurf_makemap( int *status ) {

  /* Local Variables */
  Grp *confgrp = NULL;       /* Group containing configuration file */
  smfData *data=NULL;        /* Pointer to SCUBA2 data struct */
  smfFile *file=NULL;        /* Pointer to SCUBA2 data file struct */
  int flag;                  /* Flag */
  dim_t i;                   /* Loop counter */
  Grp *igrp = NULL;          /* Group of input files */
  int indf;                  /* NDF identifier of output file */
  AstKeyMap *keymap=NULL;    /* Pointer to keymap of config settings */
  int ksize=0;               /* Size of group containing CONFIG file */
  int lbnd_out[2];           /* Lower pixel bounds for output map */
  void *map=NULL;            /* Pointer to the rebinned map data */
  char method[LEN__METHOD];  /* String for map-making method */
  smfData *odata=NULL;       /* Pointer to output SCUBA2 data struct */
  Grp *ogrp = NULL;          /* Group containing output file */
  int ondf;                  /* output NDF identifier */
  int outsize;               /* Number of files in output group */
  AstFrameSet *outfset=NULL; /* Frameset containing sky->output mapping */
  int parstate;              /* State of ADAM parameters */
  float pixsize=3;           /* Size of an output map pixel in arcsec */
  int size;                  /* Number of files in input group */
  int smfflags=0;            /* Flags for smfData */
  char system[10];           /* Celestial coordinate system for output image */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  int usebad;                /* Flag for whether to use bad bolos mask */
  void *variance=NULL;       /* Pointer to the variance map */
  smfData *wdata=NULL;       /* Pointer to SCUBA2 data struct for weights */
  void *weights=NULL;        /* Pointer to the weights map */
  HDSLoc *weightsloc=NULL;   /* HDS locator of weights array */


  /* Main routine */
  ndfBegin();
  
  /* Get group of input files */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get the celestial coordinate system for the output cube. */
  parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC,"
	    "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

  /* Get the user defined pixel size */
  parGet0r( "PIXSIZE", &pixsize, status );
  if ( pixsize <= 0 || pixsize > 60. ) {
    msgSetr("PIXSIZE", pixsize);
    *status = SAI__ERROR;
    errRep(" ", 
	   "Invalid pixel size, ^PIXSIZE (must be positive but < 60 arcsec)", 
	   status);
  }

  /* Determine whether or not to use the bad bolos mask.  If 
     unspecified, use the mask */
  parGtd0l ("USEBAD", 1, 1, &usebad, status);

  /* Get METHOD */
  parChoic( "METHOD", "REBIN", 
	    "REBIN, ITERATE.", 1,
	    method, LEN__METHOD, status);
  
  /* Calculate the map bounds */
  msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Determine map bounds", status);
  smf_mapbounds( igrp, size, system, 0, 0, 1, pixsize, lbnd_out, ubnd_out, 
		 &outfset, status );
  if (*status != SAI__OK) {
    errRep(FUNC_NAME, "Unable to determine map bounds", status);
  }

  /* Create an output smfData */
  ndgCreat ( "OUT", NULL, &ogrp, &outsize, &flag, status );
  smfflags = 0;
  smfflags |= SMF__MAP_VAR;

  smf_open_newfile ( ogrp, 1, SMF__DOUBLE, 2, lbnd_out, ubnd_out, smfflags, &odata, 
		     status );

  if ( *status == SAI__OK ) {

    file = odata->file;
    ondf = file->ndfid;

    /* Map the data, variance, and weights arrays */
    map = (odata->pntr)[0];
    variance = (odata->pntr)[1];

  }

  /* Create WEIGHTS extension in the output file and map pointer to
     weights array */
  weightsloc = smf_get_xloc ( odata, "SCU2RED", "SCUBA2_WT_ARR", "WRITE", 
                              0, 0, status );
  smf_open_ndfname ( weightsloc, "WRITE", NULL, "WEIGHTS", "NEW", "_DOUBLE",
                     2, lbnd_out, ubnd_out, &wdata, status );
  if ( *status == SAI__OK ) 
    weights = (wdata->pntr)[0];

  /* Create the map using the chosen METHOD */
  if( strncmp( method, "REBIN", 5 ) == 0 ) {
    /* Simple Regrid of the data */
    msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Make map using REBIN method", 
	     status);

    for(i=1; i<=size; i++ ) {
      /* Read data from the ith input file in the group */      
      smf_open_and_flatfield( igrp, NULL, i, &data, status );
      /* ****** 
	 These calls are not needed - we should probably check that we
	 have sky-subtracted and extinction-corrected data 

	 Actually I think a parameter called AUTOSKY should be used to
	 decide whether or not to do the `optimal' sky removal IFF sky
	 removal and ext correction have not been done already

	 ****** */
      /* Remove sky - assume MEAN is good enough for now */
      /*      smf_subtract_plane(data, "MEAN", status);*/
      /* Use raw WVM data to make the extinction correction */
      /*      smf_correct_extinction(data, "WVMR", 0, 0, status);*/
      
      /* Check that the data dimensions are 3 (for time ordered data) */
      if( *status == SAI__OK ) {
	if( data->ndims != 3 ) {
	  msgSeti("I",i);
	  msgSeti("THEDIMS", data->ndims);
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, 
		 "File ^I data has ^THEDIMS dimensions, should be 3.", 
		 status);
	}
      }
      
      /* Check that the input data type is double precision */
      if( *status == SAI__OK ) {
	if( data->dtype != SMF__DOUBLE) {
	  msgSeti("I",i);
	  msgSetc("DTYPE", smf_dtype_string( data, status ));
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, 
		 "File ^I has ^DTYPE data type, should be DOUBLE.",
		 status);
	}
      }
  
      if ( usebad ) {

         /* Retrieve the NDF identifier for this input file */   
	 ndgNdfas ( igrp, i, "READ", &indf, status );

         /* Rebin the data onto the output grid with bad bolometer mask */ 
         smf_bbrebinmap(data, indf, i, size, outfset, lbnd_out, ubnd_out, 
	                map, variance, weights, status );

      } else {
     
         /* Rebin the data onto the output grid without bad bolometer mask */
         smf_rebinmap(data, i, size, outfset, lbnd_out, ubnd_out, 
		      map, variance, weights, status );

      }
   
  
      /* Close the data file */
      if( data != NULL ) {
	smf_close_file( &data, status);
	data = NULL;
      }
      
      /* Break out of loop over data files if bad status */
      if (*status != SAI__OK) {
	errRep(FUNC_NAME, "Rebinning step failed", status);
	break;
      }
    }
  } else if( strncmp( method, "ITERATE", 5 ) == 0 ) {

    /* Iterative map-maker */
    msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Make map using ITERATE method", 
	     status);

    /* Read a group of configuration settings into keymap */
    parState( "CONFIG", &parstate, status );
    if( parstate == PAR__ACTIVE ) {
      kpg1Gtgrp( "CONFIG", &confgrp, &ksize, status );
      kpg1Kymap( confgrp, &keymap, status );
      if( confgrp ) grpDelet( &confgrp, status );      
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "CONFIG unspecified", status);      
    }
    
    /* Loop over all input data files to put in the pointing extension */
    if( *status == SAI__OK ) {
      for(i=1; i<=size; i++ ) {	

        smf_open_file( igrp, i, "UPDATE", 1, &data, status );
        if( *status != SAI__OK) {
          errRep(FUNC_NAME, "Bad status opening smfData", status);      
        }
          
        smf_calc_mapcoord( data, outfset, lbnd_out, ubnd_out, status );
        if( *status != SAI__OK) {
          errRep(FUNC_NAME, "Bad status calculating MAPCOORD", status);      
        }

        smf_close_file( &data, status );
        if( *status != SAI__OK) {
          errRep(FUNC_NAME, "Bad status closing smfData", status);      
        }          

	/* Exit loop if error status */
	if( *status != SAI__OK ) i=size;        
      }
    }

    /* Call the low-level iterative map-maker */
    smf_iteratemap( igrp, keymap, map, variance, weights,
		    (ubnd_out[0]-lbnd_out[0]+1)*(ubnd_out[1]-lbnd_out[1]+1),
		    status );

  }

  /* Write FITS header */
  ndfPtwcs( outfset, ondf, status );
  
  if( outfset != NULL ) {
    astAnnul( outfset );
    outfset = NULL;
  }
  
  smf_close_file ( &wdata, status );
  smf_close_file ( &odata, status );

  if( igrp != NULL ) grpDelet( &igrp, status);
  if( ogrp != NULL ) grpDelet( &ogrp, status);

  ndfEnd( status );
  
  if( *status == SAI__OK ) {
    msgOutif(MSG__VERB," ","MAKEMAP succeeded, map written.", status);
  } else {
    msgOutif(MSG__VERB," ","MAKEMAP failed.", status);
  }
}
