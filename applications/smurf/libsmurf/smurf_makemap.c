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
*     CONFIG = GROUP (Read) 
*        Specifies values for the configuration parameters used by the
*        iterative map maker (METHOD=ITERATE). If the string "def" (case-insensitive)
*        or a null (!) value is supplied, a set of default configuration 
*        parameter values will be used.
*
*        The supplied value should be either a comma-separated list of strings 
*        or the name of a text file preceded by an up-arrow character
*        "^", containing one or more comma-separated list of strings. Each
*        string is either a "keyword=value" setting, or the name of a text 
*        file preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner (any blank lines or lines beginning 
*        with "#" are ignored). Within a text file, newlines can be used
*        as delimiters as well as commas. Settings are applied in the order 
*        in which they occur within the list, with later settings over-riding 
*        any earlier settings given for the same keyword.
*
*        Each individual setting should be of the form:
*
*           <keyword>=<value>
*        
*        The parameters available for are listed in the "Configuration
*        Parameters" sections below. Default values will be used for
*        any unspecified parameters. Unrecognised options are ignored
*        (that is, no error is reported). [current value]
*     IN = NDF (Read)
*          Input file(s)
*     METHOD = LITERAL (Read)
*          Specify which map maker should be used to construct the map. The
*          parameter can take the following values:
*
*          - "REBIN" -- Use a single pass rebinning algorithm. This technique
*          assumes that the data have previously had atmosphere and instrument
*          signatures removed. It makes use of the standard AST library rebinning
*          algorithms (see also KAPPA WCSMOSAIC). It's an excellent choice for
*          obtaining an image quickly, especially of a bright source.
*
*          - "ITERATE" -- Use the iterative map maker. This map maker is much slower
*          than the REBIN algorithm because it continually makes a map, constructs models
*          for different data components (common-mode, spikes etc).
*
*     OUT = NDF (Write)
*          Output file
*     PIXSIZE = REAL (Read)
*          Pixel size in output image, in arcsec. []
*     SYSTEM = LITERAL (Read)
*          The celestial coordinate system for the output cube. One of
*          ICRS, GAPPT, FK5, FK4, FK4-NO-E, AZEL, GALACTIC, ECLIPTIC. It
*          can also be given the value "TRACKING", in which case the
*          system used will be which ever system was used as the tracking
*          system during in the observation.
*
*          The choice of system also determines if the telescope is 
*          considered to be tracking a moving object such as a planet or 
*          asteroid. If system is GAPPT or AZEL, then each time slice in
*          the input data will be shifted in order to put the base
*          telescope position (given by TCS_AZ_BC1/2 in the JCMTSTATE
*          extension of the input NDF) at the same pixel position that it
*          had for the first time slice. For any other system, no such 
*          shifts are applied, even if the base telescope position is
*          changing through the observation. [TRACKING]

*  Iterative MapMaker Configuration Parameters:
*     The following configuration parameters are available for the iterative
*     map maker:
*          - "NUMITER"
*          - "MODELORDER"

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
*     2007-01-25 (AGG):
*        Update API in calls to smf_mapbounds and smf_rebinmap
*     2007-02-06 (AGG):
*        Add uselonlat flag rather that specify hard-wired value in
*        smf_mapbounds
*     2007-03-05 (EC):
*        Changed smf_correct_extinction interface
*     2007-03-20 (TIMJ):
*        Write an output FITS header
*     2007-06-22 (TIMJ):
*        Rework to handle PRV* as well as OBS*
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2005-2007 University of British Columbia.
*     Copyright (C) 2007 Science and Technology Facilities Council.
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

#include "libsc2sim/sc2sim.h"

#define FUNC_NAME "smurf_makemap"
#define TASK_NAME "MAKEMAP"
#define LEN__METHOD 20

void smurf_makemap( int *status ) {

  /* Local Variables */
  Grp *confgrp = NULL;       /* Group containing configuration file */
  smfData *data=NULL;        /* Pointer to SCUBA2 data struct */
  AstFitsChan *fchan = NULL; /* FitsChan holding output NDF FITS extension */
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
  AstKeyMap * obsidmap = NULL; /* Map of OBSIDs from input data */
  smfData *odata=NULL;       /* Pointer to output SCUBA2 data struct */
  Grp *ogrp = NULL;          /* Group containing output file */
  int ondf = NDF__NOID;      /* output NDF identifier */
  int outsize;               /* Number of files in output group */
  AstFrameSet *outfset=NULL; /* Frameset containing sky->output mapping */
  int parstate;              /* State of ADAM parameters */
  float pixsize=3;           /* Size of an output map pixel in arcsec */
  AstKeyMap * prvkeymap = NULL; /* Keymap of input files for PRVxxx headers */
  int size;                  /* Number of files in input group */
  int smfflags=0;            /* Flags for smfData */
  char system[10];           /* Celestial coordinate system for output image */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  int usebad;                /* Flag for whether to use bad bolos mask */
  int uselonlat = 0;         /* Flag for whether to use given lon_0 and
				lat_0 for output frameset */
  void *variance=NULL;       /* Pointer to the variance map */
  smfData *wdata=NULL;       /* Pointer to SCUBA2 data struct for weights */
  void *weights=NULL;        /* Pointer to the weights map */
  HDSLoc *weightsloc=NULL;   /* HDS locator of weights array */

  int moving = 0;            /* Is the telescope base position changing? */

  /* Test createwcs ------------------------------------------------ */

  /*
  double telpos[3]; 
  double instap[2];
  JCMTState state;

  int j;
  FILE *junk;
  int nbol=1280;
  double xbolo[1280];
  double ybolo[1280];
  double xbc[1280];
  double ybc[1280];

  smf_calc_telpos( NULL, "JCMT", telpos, status );
  instap[0] = 0;
  instap[1] = 0;

  junk = fopen( "junk.txt", "w" );

  for( j=0; j<4; j++ ) {

    state.tcs_az_ac1 = 90.*DD2R;
    state.tcs_az_ac2 =  10.*DD2R;
    state.smu_az_jig_x = 0;
    state.smu_az_jig_y = 0;
    state.smu_az_chop_x = 0;
    state.smu_az_chop_y = 0;
    state.rts_end = 53795;

    sc2ast_createwcs( j, &state, instap, telpos, &outfset, status );
    
    astSetC( outfset, "SYSTEM", "J2000" );
    
    smf_get_gridcoords( xbolo, ybolo, 40, 32, status );
    

    astTran2( outfset, nbol, ybolo, xbolo, 1, xbc, ybc ); 
   
    astAnnul(outfset);
 
    for( i=0; i<nbol; i++ ) {
      fprintf(junk,"%lf %lf %lf %lf\n", xbolo[i], ybolo[i], xbc[i], ybc[i]);
    }
  }
  fclose(junk);

  return;
  */
 
  /* --------------------------------------------------------------- */


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


  /* Test create_lutwcs -------------------------------------------- */
  /*
  double telpos[3]; 
  double instap[2];
  JCMTState state;
  
  int j;
  FILE *junk;
  int nbol=144;
  double xbolo[144];
  double ybolo[144];
  double xbc[144];
  double ybc[144];
  
  smfHead * hdr;
  
  smf_open_file( igrp, 1, "READ", 1, &data, status );

  hdr = data->hdr;
  
  smf_tslice_ast( data, 0, 1, status);

  junk = fopen( "junk.txt", "w" );
  */

  /*
  smf_calc_telpos( NULL, "JCMT", telpos, status );
  instap[0] = 0;
  instap[1] = 0;
  
  state.tcs_az_ac1 = 90.*DD2R;
  state.tcs_az_ac2 =  45.*DD2R;
  state.smu_az_jig_x = 0;
  state.smu_az_jig_y = 0;
  state.smu_az_chop_x = 0;
  state.smu_az_chop_y = 0;
  state.rts_end = 53795;

  printf( "LUT: %lf %lf %i\n", (hdr->fplanex)[0], (hdr->fplaney)[0], 
	  hdr->ndet );

  smf_create_lutwcs( 0, hdr->fplanex, hdr->fplaney, hdr->ndet, 
		     &state, instap, telpos, &outfset, status );
  
  
  */
  /*
  astSetC( hdr->wcs, "SYSTEM", "AZEL" );

  for( j=0; j<144; j++ ) {
    xbolo[j] = j+1;
    ybolo[j] = 1;
  }

  astTran2( hdr->wcs, nbol, xbolo, ybolo, 1, xbc, ybc ); 

  for( j=0; j<144; j++ ) {
    xbolo[j] = (hdr->fplanex)[j];
    ybolo[j] = (hdr->fplaney)[j];
  }

  printf("nbol=%i\n", nbol);

  for( j=0; j<nbol; j++ ) {
    fprintf(junk,"%lf %lf %lf %lf\n", 
	    (hdr->fplanex)[j], (hdr->fplaney)[j], 
	    xbc[j], ybc[j]);
  }
  fclose(junk);
  return;

  */
  /*astAnnul(outfset);*/
  
  /* --------------------------------------------------------------- */

  /* Calculate the map bounds */
  msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Determine map bounds", status);
  smf_mapbounds( igrp, size, system, 0, 0, uselonlat, pixsize, lbnd_out, ubnd_out, 
		 &outfset, &moving, status );
  if (*status != SAI__OK) {
    errRep(FUNC_NAME, "Unable to determine map bounds", status);
  }

  if ( moving ) {
    msgOutif(MSG__VERB, " ", "Tracking a moving object", status);
  } else {
    msgOutif(MSG__VERB, " ", "Tracking a stationary object", status);
  }

  /* Create an output smfData */
  ndgCreat ( "OUT", NULL, &ogrp, &outsize, &flag, status );
  smfflags = 0;
  smfflags |= SMF__MAP_VAR;

  smf_open_newfile ( ogrp, 1, SMF__DOUBLE, 2, lbnd_out, ubnd_out, smfflags, 
		     &odata, status );

  if ( *status == SAI__OK ) {

    file = odata->file;
    ondf = file->ndfid;

    /* Map the data, variance, and weights arrays */
    map = (odata->pntr)[0];
    variance = (odata->pntr)[1];

  }

  /* Create provenance keymap */
  prvkeymap = astKeyMap( "" );

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
      if (*status != SAI__OK) break;

      /* ****** 
	 These calls are not needed - we should probably check that we
	 have sky-subtracted and extinction-corrected data 

	 Actually I think a parameter called AUTOSKY should be used to
	 decide whether or not to do the `optimal' sky removal IFF sky
	 removal and ext correction have not been done already

	 ****** */

      /* Remove sky - assume MEAN is good enough for now */
      smf_subtract_plane(data, NULL, "MEAN", status);

      /* Use raw WVM data to make the extinction correction */
      smf_correct_extinction(data, "WVMR", 0, 0, NULL, status);
      
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

      /* Store the filename in the keymap for later - the GRP would be fine
	 as is but we use a keymap in order to reuse smf_fits_add_prov */
      if (*status == SAI__OK)
	astMapPut0I( prvkeymap, data->file->name, 1, NULL );

      /* Handle output FITS header creation */
      if (*status == SAI__OK)
	smf_fits_outhdr( data->hdr->fitshdr, &fchan, &obsidmap, status );

      if ( usebad ) {
	/* Retrieve the NDF identifier for this input file */   
	ndgNdfas ( igrp, i, "READ", &indf, status );
	/* Rebin the data onto the output grid with bad bolometer mask */ 
	smf_bbrebinmap(data, indf, i, size, outfset, lbnd_out, ubnd_out, 
		       map, variance, weights, status );
      } else {

	msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Beginning the REBIN step", status);
     	/* Rebin the data onto the output grid without bad bolometer mask */
	smf_rebinmap(data, i, size, outfset, moving, lbnd_out, ubnd_out, 
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

	/* Store the filename in the keymap for later - the GRP would be fine
	   as is but we use a keymap in order to reuse smf_fits_add_prov */
	if (*status == SAI__OK)
	  astMapPut0I( prvkeymap, data->file->name, 1, NULL );

	/* Handle output FITS header creation (since the file is open and
	   he header is available) */
	smf_fits_outhdr( data->hdr->fitshdr, &fchan, &obsidmap, status );

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

  /* Write WCS */
  ndfPtwcs( outfset, ondf, status );

/* Retrieve the unique OBSID keys from the KeyMap and populate the OBSnnnnn
   and PROVCNT headers from this information. */
  smf_fits_add_prov( fchan, "OBS", obsidmap, status ); 
  smf_fits_add_prov( fchan, "PRV", prvkeymap, status ); 
  
  astAnnul( prvkeymap );
  astAnnul( obsidmap );

/* If the FitsChan is not empty, store it in the FITS extension of the
   output NDF (any existing FITS extension is deleted). */
  if( astGetI( fchan, "NCard" ) > 0 ) kpgPtfts( ondf, fchan, status );

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
