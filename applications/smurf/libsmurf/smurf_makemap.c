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
*        iterative map maker (METHOD=ITERATE). If the string "def"
*        (case-insensitive) or a null (!) value is supplied, a set of
*        default configuration parameter values will be used.
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
*     PARAMS( 2 ) = _DOUBLE (Read)
*          An optional array which consists of additional parameters
*          required by the Sinc, SincSinc, SincCos, SincGauss, Somb,
*          SombCos, and Gauss spreading methods (see parameter SPREAD).
*	   
*          PARAMS( 1 ) is required by all the above schemes. It is used to 
*          specify how many pixels on either side of the output position
*          (that is, the output position corresponding to the centre of the 
*          input pixel) are to receive contributions from the input pixel.
*          Typically, a value of 2 is appropriate and the minimum allowed 
*          value is 1 (i.e. one pixel on each side). A value of zero or 
*          fewer indicates that a suitable number of pixels should be 
*          calculated automatically. [0]
*	   
*          PARAMS( 2 ) is required only by the SombCos, Gauss, SincSinc, 
*          SincCos, and SincGauss schemes.  For the SombCos, SincSinc, and
*          SincCos schemes, it specifies the number of pixels at which the
*          envelope of the function goes to zero.  The minimum value is
*          1.0, and the run-time default value is 2.0.  For the Gauss and
*          SincGauss scheme, it specifies the full-width at half-maximum
*          (FWHM) of the Gaussian envelope.  The minimum value is 0.1, and
*          the run-time default is 1.0.  On astronomical images and 
*          spectra, good results are often obtained by approximately 
*          matching the FWHM of the envelope function, given by PARAMS(2),
*          to the point-spread function of the input data.  []
*     PIXSIZE = REAL (Read)
*          Pixel size in output image, in arcsec. []
*     SPREAD = LITERAL (Read)
*          The method to use when spreading each input pixel value out
*          between a group of neighbouring output pixels. If SPARSE is set 
*          TRUE, then SPREAD is not accessed and a value of "Nearest" is
*          always assumed. SPREAD can take the following values:
*	   
*          - "Linear" -- The input pixel value is divided bi-linearly between 
*          the four nearest output pixels.  Produces smoother output NDFs than 
*          the nearest-neighbour scheme.
*	   
*          - "Nearest" -- The input pixel value is assigned completely to the
*          single nearest output pixel. This scheme is much faster than any
*          of the others. 
*	   
*          - "Sinc" -- Uses the sinc(pi*x) kernel, where x is the pixel
*          offset from the interpolation point (resampling) or transformed
*          input pixel centre (rebinning), and sinc(z)=sin(z)/z.  Use of 
*          this scheme is not recommended.
*	   
*          - "SincSinc" -- Uses the sinc(pi*x)sinc(k*pi*x) kernel. A
*          valuable general-purpose scheme, intermediate in its visual
*          effect on NDFs between the bi-linear and nearest-neighbour
*          schemes. 
*	   
*          - "SincCos" -- Uses the sinc(pi*x)cos(k*pi*x) kernel.  Gives
*          similar results to the "Sincsinc" scheme.
*	   
*          - "SincGauss" -- Uses the sinc(pi*x)exp(-k*x*x) kernel.  Good 
*          results can be obtained by matching the FWHM of the
*          envelope function to the point-spread function of the
*          input data (see parameter PARAMS).
*	   
*          - "Somb" -- Uses the somb(pi*x) kernel, where x is the pixel
*          offset from the transformed input pixel centre, and 
*          somb(z)=2*J1(z)/z (J1 is the first-order Bessel function of the 
*          first kind.  This scheme is similar to the "Sinc" scheme.
*	   
*          - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*          scheme is similar to the "SincCos" scheme.
*	   
*          - "Gauss" -- Uses the exp(-k*x*x) kernel. The FWHM of the Gaussian 
*          is given by parameter PARAMS(2), and the point at which to truncate 
*          the Gaussian to zero is given by parameter PARAMS(1).
*	   
*          For further details of these schemes, see the descriptions of 
*          routine AST_REBINx in SUN/211. ["Nearest"]
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
*     2007-07-05 (TIMJ):
*        Fix provenance file name handling.
*     2007-07-12 (EC):
*        Add moving to smf_bbrebinmap interface
*        Add moving to smf_calc_mapcoord interface
*     2007-10-29 (EC):
*        Modified interface to smf_open_file.
*     2007-11-15 (EC):
*        Modified smf_iteratemap interface.
*     2007-11-28 (EC):
*        Fixed flag in smf_open_file
*     2008-01-22 (EC):
*        Added hitsmap to smf_iteratemap interface
*     2008-02-12 (AGG):
*        - Update to reflect new API for smf_rebinmap
*        - Note smf_bbrebinmap is now deprecated
*        - Remove sky subtraction and extinction calls
*     2008-02-13 (AGG):
*        Add SPREAD and PARAMS parameters to allow choice of
*        pixel-spreading scheme, update call to smf_rebinmap
*     2008-02-15 (AGG):
*        Expand number of dimensions for weights array if using REBIN
*     2008-02-18 (AGG):
*        - Check for all ADAM parameters before call to smf_mapbounds
*        - Change weightsloc to smurfloc
*        - Add EXP_TIME component to output file
*     2008-02-19 (AGG):
*        - Add status check before attempting to access hitsmap pointer
*        - Set exp_time values to BAD if no data exists for that pixel
*     2008-02-20 (AGG):
*        Calculate median exposure time and write FITS entry
*     2008-03-11 (AGG):
*        Update call to smf_rebinmap
*     2008-04-01 (AGG):
*        Write WCS to EXP_TIME component in output file
*     2008-04-02 (AGG):
*        Write 2-D WEIGHTS component + WCS in output file, protect
*        against attempting to access NULL smfFile pointer
*     2008-04-22 (AGG):
*        Use faster histogram-based method for calculating median
*        exposure time
*     2008-04-23 (DSB):
*        Modify call to kpg1Ghstd to pass max and min values by reference
*        rather than by value.
*     2008-04-24 (EC):
*        Added MAXMEM parameter, memory checking for output map
*     2008-05-01 (TIMJ):
*        Use BAD in EXP_TIME when no integration time.
*        Tidy up some status logic.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2005-2008 University of British Columbia.
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
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
  double *exp_time = NULL;    /* Exposure time array written to output file */
  AstFitsChan *fchan = NULL; /* FitsChan holding output NDF FITS extension */
  smfFile *file=NULL;        /* Pointer to SCUBA2 data file struct */
  int flag;                  /* Flag */
  int *histogram = NULL;     /* Histogram for calculating exposure time statistics */
  unsigned int *hitsmap;     /* Hitsmap array calculated in ITERATE method */
  dim_t i;                   /* Loop counter */
  Grp *igrp = NULL;          /* Group of input files */
  int iterate=0;             /* Flag to denote whether to use the ITERATE method */
  AstKeyMap *keymap=NULL;    /* Pointer to keymap of config settings */
  int ksize=0;               /* Size of group containing CONFIG file */
  int lbnd_out[2];           /* Lower pixel bounds for output map */
  double *map=NULL;          /* Pointer to the rebinned map data */
  size_t mapmem;             /* Memory needed for output map */
  size_t mapsize;            /* Number of pixels in output image */
  size_t maxmem;             /* Max memory usage in bytes */
  int maxmem_mb;             /* Max memory usage in Mb */
  double meantexp;           /* Mean exposure time */
  double maxtexp = 0.0;      /* Maximum exposure time */
  double modetexp;           /* Modal exposure time */ 
  double medtexp = 0.0;      /* Median exposure time */
  char method[LEN__METHOD];  /* String for map-making method */
  int moving = 0;            /* Is the telescope base position changing? */
  int nparam = 0;            /* Number of extra parameters for pixel spreading scheme*/
  int numbin;                /* Number of exposure time bins in histogram */
  AstKeyMap * obsidmap = NULL; /* Map of OBSIDs from input data */
  smfData *odata=NULL;       /* Pointer to output SCUBA2 data struct */
  Grp *ogrp = NULL;          /* Group containing output file */
  int ondf = NDF__NOID;      /* output NDF identifier */
  int outsize;               /* Number of files in output group */
  AstFrameSet *outfset=NULL; /* Frameset containing sky->output mapping */
  char pabuf[ 10 ];          /* Text buffer for parameter value */
  double params[ 4 ];        /* astRebinSeq parameters */
  int parstate;              /* State of ADAM parameters */
  double pixsize=3;          /* Size of an output map pixel in arcsec */
  AstKeyMap * prvkeymap = NULL; /* Keymap of input files for PRVxxx headers */
  int rebin=1;               /* Flag to denote whether to use the REBIN method */
  int size;                  /* Number of files in input group */
  int smfflags=0;            /* Flags for smfData */
  HDSLoc *smurfloc=NULL;     /* HDS locator of SMURF extension */
  int spread;                /* Code for pixel spreading scheme */
  double steptime;           /* Integration time per sample, from FITS header */
  double sumtexp;            /* Total exposure time across all pixels */
  char system[10];           /* Celestial coordinate system for output image */
  smfData *tdata=NULL;       /* Exposure time data */
  int tndf = 0;              /* NDF identifier for EXP_TIME */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  int uselonlat = 0;         /* Flag for whether to use given lon_0 and
				lat_0 for output frameset */
  void *variance=NULL;       /* Pointer to the variance map */
  smfData *wdata=NULL;       /* Pointer to SCUBA2 data struct for weights */
  double *weights=NULL;      /* Pointer to the weights map */
  double *weights3d = NULL;  /* Pointer to 3-D weights array */
  int wndf = 0;              /* NDF identifier for WEIGHTS */

  /* Main routine */
  ndfBegin();

  /* Get group of input files */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get the celestial coordinate system for the output cube. */
  parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC,"
	    "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

  /* Get the user defined pixel size */
  parGet0d( "PIXSIZE", &pixsize, status );
  if ( pixsize <= 0 || pixsize > 60. ) {
    msgSetd("PIXSIZE", pixsize);
    *status = SAI__ERROR;
    errRep(" ", 
	   "Invalid pixel size, ^PIXSIZE (must be positive but < 60 arcsec)", 
	   status);
  }

  /* Get the maximum amount of memory that we can use */
  parGet0i( "MAXMEM", &maxmem_mb, status );
  if ( maxmem_mb <= 0 ) {
    msgSeti("MAXMEM", maxmem_mb);
    *status = SAI__ERROR;
    errRep(" ", "Invalid MAXMEM, ^MAXMEM Mb (must be > 0 )", status);
  } else {
    maxmem = (size_t) maxmem_mb * SMF__MB;
  }

  /* Get METHOD - set rebin/iterate flags */
  parChoic( "METHOD", "REBIN", 
	    "REBIN, ITERATE.", 1,
	    method, LEN__METHOD, status);
  if( strncmp( method, "REBIN", 5 ) == 0 ) {
    rebin = 1;
    iterate = 0;
  } else if ( strncmp( method, "ITERATE", 5 ) == 0 ) {
    rebin = 0;
    iterate = 1;
  }

  /* Get remaining parameters so errors are caught early */
  if ( rebin ) {
    /* Obtain desired pixel-spreading scheme */
    parChoic( "SPREAD", "NEAREST", "NEAREST,LINEAR,SINC,"
	      "SINCSINC,SINCCOS,SINCGAUSS,SOMB,SOMBCOS,GAUSS", 
	      1, pabuf, 10, status );

    smf_get_spread( pabuf, &spread, &nparam, status );

    /* Get an additional parameter vector if required. */
    if ( nparam > 0 ) parExacd( "PARAMS", nparam, params, status );
  } else if ( iterate ) {
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
  }

  /* Calculate the map bounds */
  msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Determine map bounds", status);
  smf_mapbounds( igrp, size, system, 0.0, 0.0, uselonlat, pixsize, 
		 lbnd_out, ubnd_out, &outfset, &moving, status );

  if ( moving ) {
    msgOutif(MSG__VERB, " ", "Tracking a moving object", status);
  } else {
    msgOutif(MSG__VERB, " ", "Tracking a stationary object", status);
  }

  /* Check memory requirements for output map */
  smf_checkmem_map( lbnd_out, ubnd_out, rebin, maxmem, &mapmem, status );

  /* Create an output smfData */
  ndgCreat ( "OUT", NULL, &ogrp, &outsize, &flag, status );
  smfflags = 0;
  smfflags |= SMF__MAP_VAR;

  smf_open_newfile ( ogrp, 1, SMF__DOUBLE, 2, lbnd_out, ubnd_out, smfflags, 
		     &odata, status );

  if ( *status == SAI__OK ) {
    file = odata->file;
    ondf = file->ndfid;
    /* Map the data and variance arrays */
    map = (odata->pntr)[0];
    variance = (odata->pntr)[1];
  }

  /* Create provenance keymap */
  prvkeymap = astKeyMap( "" );

  /* Compute number of pixels in output map */
  mapsize = (ubnd_out[0] - lbnd_out[0] + 1) * (ubnd_out[1] - lbnd_out[1] + 1);

  /* Create SMURF extension in the output file and map pointers to
     WEIGHTS and EXP_TIME arrays */
  smurfloc = smf_get_xloc ( odata, "SMURF", "SMURF", "WRITE", 0, 0, status );

  /* Create WEIGHTS component in output file */
  smf_open_ndfname ( smurfloc, "WRITE", NULL, "WEIGHTS", "NEW", "_DOUBLE",
                     2, lbnd_out, ubnd_out, &wdata, status );
  if ( wdata ) {
    weights = (wdata->pntr)[0];
    wndf = wdata->file->ndfid;
  }

  /* Create EXP_TIME component in output file */
  smf_open_ndfname ( smurfloc, "WRITE", NULL, "EXP_TIME", "NEW", "_DOUBLE",
		     2, lbnd_out, ubnd_out, &tdata, status );
  if ( tdata ) {
    exp_time = (tdata->pntr)[0];
    tndf = tdata->file->ndfid;
  }

  /* Create the output map using the chosen METHOD */
  if ( rebin ) {
    /* Now allocate memory for 3-d work array used by smf_rebinmap -
       plane 2 of this 3-D array is stored in the weights component
       later. Initialize to zero. */
    weights3d = smf_malloc( 2*mapsize, sizeof(double), 1, status);

    /* Simple Regrid of the data */
    msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Make map using REBIN method", 
	     status);

    for(i=1; (i<=size) && (*status == SAI__OK); i++ ) {
      /* Read data from the ith input file in the group */      
      smf_open_and_flatfield( igrp, NULL, i, &data, status ); 

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

      /* Store steptime for calculating EXP_TIME */
      if ( i==1 ) {
	smf_fits_getD(data->hdr, "STEPTIME", &steptime, status);
      }

      /* Store the filename in the keymap for later - the GRP would be fine
	 as is but we use a keymap in order to reuse smf_fits_add_prov */
      if (*status == SAI__OK)
        smf_accumulate_prov( prvkeymap, data->file, igrp, i, status );

      /* Handle output FITS header creation */
      if (*status == SAI__OK)
	smf_fits_outhdr( data->hdr->fitshdr, &fchan, &obsidmap, status );

      msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Beginning the REBIN step", status);
      /* Rebin the data onto the output grid */
      smf_rebinmap(data, i, size, outfset, spread, params, moving, 1,
		   lbnd_out, ubnd_out, map, variance, weights3d, status );
  
      /* Close the data file */
      smf_close_file( &data, status);
      
      /* Break out of loop over data files if bad status */
      if (*status != SAI__OK) {
	errRep(FUNC_NAME, "Rebinning step failed", status);
	break;
      }

    }
    /* Calculate exposure time per output pixel from weights array -
       note even if weights is a 3-D array we only use the first
       mapsize number of values which represent the `hits' per
       pixel */
    for (i=0; (i<mapsize) && (*status == SAI__OK); i++) {
      if ( map[i] == VAL__BADD) {
	exp_time[i] = VAL__BADD;
	weights[i] = VAL__BADD;
      } else {
	exp_time[i] = steptime * weights3d[i];
	weights[i] = weights3d[i+mapsize];
	if ( exp_time[i] > maxtexp ) {
	  maxtexp = exp_time[i];
	}
      }
    }
    weights3d = smf_free( weights3d, status );
  } else if ( iterate ) {

    /* Iterative map-maker */
    msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Make map using ITERATE method", 
	     status);

    /* Allocate space for hitsmap */
    hitsmap = smf_malloc( mapsize, sizeof (int), 1, status);

    /* Loop over all input data files to setup provenance handling */
    if( *status == SAI__OK ) {
      for(i=1; i<=size; i++ ) {	

	if (*status == SAI__OK) {
	  smf_open_file( igrp, i, "UPDATE", SMF__NOCREATE_DATA, &data,status );
	  if( *status != SAI__OK) {
	    errRep(FUNC_NAME, "Bad status opening smfData", status);      
	  }
	}
        
	/* Store steptime for calculating EXP_TIME */
	if ( i==1 ) {
	  smf_fits_getD(data->hdr, "STEPTIME", &steptime, status);
	}

	/* Store the filename in the keymap for later - the GRP would be fine
	   as is but we use a keymap in order to reuse smf_fits_add_prov */
	if (*status == SAI__OK)
          smf_accumulate_prov( prvkeymap, data->file, igrp, i, status );

	/* Handle output FITS header creation (since the file is open and
	   he header is available) */
	smf_fits_outhdr( data->hdr->fitshdr, &fchan, &obsidmap, status );

	if (*status == SAI__OK) {
	  smf_close_file( &data, status );
	  if( *status != SAI__OK) {
	    errRep(FUNC_NAME, "Bad status closing smfData", status);      
	  }
	}     

	/* Exit loop if error status */
	if( *status != SAI__OK ) break;
      }
    }

    /* Call the low-level iterative map-maker */
    smf_iteratemap( igrp, keymap, outfset, moving, lbnd_out, ubnd_out,
		    maxmem-mapmem, map, hitsmap, variance, weights, status );

    /* Calculate exposure time per output pixel from hitsmap */
    for (i=0; (i<mapsize) && (*status == SAI__OK); i++) {
      if ( map[i] == VAL__BADD) {
	exp_time[i] = VAL__BADD;
      } else {
	exp_time[i] = steptime * hitsmap[i];
	if ( exp_time[i] > maxtexp ) {
	  maxtexp = exp_time[i];
	}
      }
    }
    hitsmap = smf_free( hitsmap, status );
  } else {
    /* no idea what mode */
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( " ", "Map maker mode not understood. Should not be possible",
	      status );
    }
  }

  /* Write WCS */
  ndfPtwcs( outfset, ondf, status );
  ndfPtwcs( outfset, tndf, status );
  ndfPtwcs( outfset, wndf, status );

  /* Calculate median exposure time - use faster histogram-based
     method which should be accurate enough for our purposes */
  numbin = (int)(maxtexp / steptime) - 1;
  histogram = smf_malloc( (size_t)numbin, sizeof(int), 1, status );
  if ( histogram ) {
    kpg1Ghstd( 1, (int)mapsize, exp_time, numbin, 0, &maxtexp, &steptime, histogram, 
	       status );
    kpg1Hsstp( numbin, histogram, maxtexp, steptime, 
	       &sumtexp, &meantexp, &medtexp, &modetexp, status);
    astSetFitsF(fchan, "MEDTEXP", medtexp, "[s] Median MAKEMAP exposure time", 0);
    histogram = smf_free( histogram, status );
  }


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
