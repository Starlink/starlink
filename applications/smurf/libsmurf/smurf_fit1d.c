/*
*+
*  NAME:
*
*     FIT1D

*  Purpose:
*     Fit 1-D profiles to a data cube

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_fit1d( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This command fits 1-D profiles, such as gaussians, along an axis of
*     a multi-dimensional data cube, that is expected to have spectral
*     baselines or continua removed.  The task creates a (hyper-)cube of
*     the fitted profiles; this NDF also stores the fitted-profile
*     parameters in an extension.  Note that this is a preliminary
*     release of FIT1D.

*     The routine can fit complex spectra with multiple components in a
*     data cube (actually: fit along any axis of a hyper-cube). It is
*     multi-threaded and capable of fitting a large number of (i.e. order
*     a million) spectra in a few minutes, depending of course on the number
*     of cores available. It borrows heavily from the "xgaufit" routine of
*     the GIPSY package.
*
*     The type of profiles that can be fitted and have been tested are
*     "gaussian", "gausshermite1" (gaussian with asymmetric wings), and
*     "gausshermite2" (peaky gaussians possibly with asymmetric wings).
*     See the important "Fitting Functions" topic for more details.
*
*     The parameters for each fitted component reside in the SMURF_FIT1D
*     extension as cube NDFs called COMP_1,..., COMP_N.  For a gaussian the
*     planes in these cubes correspond to: amplitude, position, and fwhm.
*     Further details are under topic "Fitted Parameters".
*
*     A default config file is in $SMURF_DIR/smurf_fit1d.def.
*
*     Please note that this routine is still under active development.
*
*  Usage
*     fit1d in out rms config [userval] [diagndf] [parndf]

*  ADAM Parameters:
*     CONFIG = GROUP (Read)
*          Specifies values for the configuration parameters used by the
*          FIT1D. If the string "def" (case-insensitive) or a null (!)
*          value is supplied, a set of default configuration parameter
*          values will be used from $SMURF_DIR/smurf_fit1d.def.  See the
*          "Configuration Parameters" topic for detailed information.
*     DIAGNDF = GROUP (Read)
*          (NOT YET USED/TESTED!)
*          Name of NDF to use as parameter file COMP_0 with diagnostics
*          and baseline parameters. This component can usually be omitted
*          unless access to the baseline is needed. This parameter can also
*          be used instead of PARNDF except that the given NDFs are
*          interpreted to start with COMP_0, i.e. COMP_0, COMP_1 .. COMP_N.
*          A null value requests that no diagnostic NDF be created. [!]
*     IN = NDF (Read)
*          Baselined input file(s).
*     OUT = NDF (Write)
*          Output file(s) with the fitted profiles.
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null value is supplied no file is created. [!]
*     PARNDF = GROUP (Read)
*          (NOT YET USED/TESTED!)
*          Name of NDF(s) to use for parameter files COMP_1 .. COMP_N with
*          parameters for the function(s). These values will be used as
*          initial values for further processing: for the fit or to generate
*          the model with. Providing fewer than NCOMP files will result in
*          a warning and remaining components will be left unfilled.
*          Supplying more than NCOMP files will generate an error. If the
*          location to a standard SMURF_FIT1D extension is given as
*          "filename.MORE.SMURF_FIT1D" all the components in the extension
*          will be copied.  A null value means no parameter NDFs are
*          required.  [!]
*     RMS = _DOUBLE (Read)
*          RMS in input NDF(s) in data units.
*     USERVAL = GROUP (Read)
*          initial estimates for the fit. Entries are of the form
*          'letter''number' = ( val1, val2 ) or ... = val1. Val1 is the
*          value for the given parameter; Val2 = is the fitmask:
*          0 (fix, do not fit), 1 (use as initial estimate, but fit).
*          If Val2 is omitted it defaults to 0 (fixed).
*          Parameters are indicated by a letter and a number (1..) indicating
*          the component, with '0' meaning to apply to all components.
*
*          -  Gauss* -- a = amplitude, x = position, f = fwhm,
*                       s = skewness (hermite h3), k = kurtosis (hermite h4)
*          -  Voigt  -- a = amplitude, x = position, f = fwhm, l = lorentzian
*          -  Polynomial -- c0, c1, c2, ... = c0 + c1*x + c2*x^2 + ...
*          -  Zerolevel -- z0, z1, z2 ( = z0 + z1*x + z2*x^2 ) (Optional)
*
*          A null value means that no user-supplied estimates are required.
*          [!]

*  Configuration Parameters:
*     A default configuration file can be found at 
*                     $SMURF_DIR/smurf_fit1d.def.
*     AXIS = INTEGER
*          Axis to fit along (starting at 1). A value of 0 translates
*          as fit along highest dimension i.e. Vlsr in a Ra, Dec, Vlsr cube.
*          [0]
*     ESTIMATE_ONLY = LOGICAL
*          Set to 1: The output cube will have the results from the
*          initial estimates routine instead of the the fit.  Good
*          initial estimates are critical for the fit and checking
*          and/or fixing initial estimates may help solve problems. [0]
*     FUNCTION = STRING
*          Function to fit.  Currently implemented are "gaussian", 
*          "gausshermite1", "gausshermite2", "voigt", abnd "polynomial".
*          See topic "Fitting Functions" for details. ["gaussian"]
*     MAXLORZ = REAL
*          Maximum value for the FHWM of the Lorentzian component in
*          terms of ==PIXELS==(!). A negative value implies no constraint.
*          Applies Voigt fits only. [-1]
*     MINAMP = REAL
*          Minimum value for the Amplitude to accept as a genuine fit
*          in terms of the rms. Based on the amplitude alone, at 3-sigma
*          5% of the profiles selected for fitting can be expected to be
*          noise spikes. This value drops to 2% for 5-sigma. All
*          assuming gaussian statistics of course.  Applies to Gauss*
*          and Voigt fits only. [3]
*     MINFWHM = REAL
*          Minimum value for the FHWM (~2.35*Dispersion) to accept as a
*          genuine fit in terms of ==PIXELS==(!).
*          Applies to Gauss* and Voigt fits only. [1.88]
*     MODEL_ONLY = LOGICAL
*          Set to 1: Bypass both the initial estimates and fitting
*          routine and generate profiles directly from the supplied
*          input parameter cube(s) and/or user supplied fixed values.
*          Not supplying all parameters will generate an error. [0]
*     NCOMP = INTEGER
*          Maximum number of 'component' functions to fit to each
*          profile, e.g. a multi-component spectrum of maximum three
*          gaussians. [Note: The complete fit of the gaussians and an
*          optional zerolevel is done concurrently, not iteratively
*          starting e.g. with the strongest component]. The routine will
*          try to find and fit ncomp functions along each profile, but
*          may settle for less. [3]
*     RANGE(2) = REAL
*          Coordinate range along axis to find and fit profiles. The
*          format is (x1, x2) including the ().  For example,
*          Vlsr -20 35 is "(-20,35)".
*          Default is to use the full extent of the axis: [<undef>]

*  Fitting Functions:
*     The function menu provides the choice of four functions for which
*     you can fit the parameters to the data in your profiles.
*
*     1) A standard GAUSSIAN. Parameters are amplitude, centre, and
*        dispersion.
*
*     NOTE that if one of h3 and h4 is not zero, the mean of the
*     distribution is not the position of the maximum.
*     (Reference; Marel, P. van der, Franx, M., A new method for the
*     identification of non-gaussian line profiles in elliptical galaxies.
*     A.J., 407 525-539, 1993 April 20).
*
*     2) GAUSS-HERMITE1 polynomial (h3). Parameters are a,b,c for
*     amplitude, position and fwhm (which are *NOT* the same as maximum
*     amplitude, centre, and dispersion!) and h3.
*                 maximum ~= [determine value and position of max from
*                             fitted profiles using e.g. collapse]
*                  centre ~= b + h3*sqrt(3)
*              dispersion ~= abs( c*(1-3h3^2) ) ~= c
*                skewness ~= 4*sqrt(3)*h3
*
*     3) GAUSS-HERMITE2 polynomial (h3, h4). Same as above, but an extra
*        parameter h4 is included.
*                 maximum ~= [determine value and position of max from
*                             fitted profiles using e.g. collapse]
*                  centre ~= b + h3*sqrt(3)
*              dispersion ~= abs( c*(1+h4*sqrt(6)) )
*                skewness ~= 4*sqrt(3)*h3
*                kurtosis ~= 8*sqrt(6)*h4
*
*     4) VOIGT function. Parameters are a,b,c,d: Area, centre, Doppler
*        and Lorentzian HWHM.
*                 maximum ~= [determine value of max from fitted
*                             profiles using e.g. collapse]
*                  centre ~= b
*            doppler fwhm ~= abs( 2*c )
*         lorentzian fwhm ~= abs( 2*d )
*
*  Fitted Parameters:
*     The fitted parameters are stored in the file header as
*     FILE.MORE.SMURF_FIT1D.COMP_0 to COMP_N, with N depending on how many
*     components are being fitted. These are regular data cubes that can be
*     inspected with e.g. Gaia or extracted using NDFCOPY. The 'planes' in
*     the cubes are:
*
*     COMP_0 diagnostics info, planes:
*          1 = number of components found
*          2 = fit error: (see below)
*
*     COMP_1..N fitted profiles, planes:
*          1 = amplitude (gaussian); 'a' (gausshermite)
*          2 = position  (gaussian); 'b' (gausshermite)
*          3 = fwhm      (gaussian); 'c' (gausshermite)
*          4 = 'h3'      (gausshermite1,2)
*          5 = 'h4'      (gausshermite2)
*          last: function id
*              1 = gaussian; 2 = gausshermite1 (h3);
*              3 = gausshermite2 (h3,h4), 4 = voigt
*
*     FIT ERRORS:
*         >0   Number of iterations needed to achieve convergence
*                according to TOL.
*         -1   Too many free parameters, maximum is 32.
*         -2   No free parameters.
*         -3   Not enough degrees of freedom.
*         -4   Maximum number of iterations too small to obtain a
*                solution which satisfies TOL.
*         -5   Diagonal of matrix contains elements which are zero.
*         -6   Determinant of the coefficient matrix is zero.
*         -7   Square root of negative number.
*         <-10 All fitted components rejected due to minamp, minfwhm,
*                maxlorz, or range constraints.

*  Authors:
*     Remo Tilanus (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-09-29 (RPT):
*        Initial test version from smurf_remsky
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 3 of
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
*     MA 02111-1307, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

/* Starlink includes */
#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "star/ndg.h"
#include "star/util.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"
#include "smurf_par.h"
#include "libsmf/smf_err.h"

/* FIT1D includes */
#include "libsmf/smf_fit1d.h"

/* Simple default string for errRep */
#define FUNC_NAME "smurf_fit1d"
#define TASK_NAME "FIT1D"

/* Fit extention */
#define FIT1D__EXTNAME  "SMURF_FIT1D"
#define FIT1D__EXTTYPE  "SMURF_FIT1D"

static void get_fit1par( int *axis, double *range,
		      smf_math_function *fid, int *ncomp, double *rms,
		      double *critamp, double critdisp[],
		      int *estimate_only, int *model_only, int *status );

static void get_userval ( int in, smf_math_function fid, int ncomp, AstKeyMap **usrvalmap,
			  smfArray *pardata, int *status );

static void map_axis_to_wcs ( smfData *data, int axis, AstMapping **wcsmap,
			       int *status );

static void convert_range_to_pixels ( AstMapping **wcsmap, double *range,
				      int lbnd, int ubnd, int *prange,
				      int *status );

static void convert_coord_to_pixel ( AstMapping **wcsmap, double *coord,
				     int nval, double *pixel, int *status );

static void convert_pixel_to_coord ( AstMapping **wcsmap, double *pixel,
				     int nval, double *coord, int *status );

static void setup_parameter_ndfs ( smfData *data, int axis, int ncomp,
				   int *parndfs, smfArray *pardata,
				   int *status );

static void copy_parameter_ndfs ( smfArray *pardata, int *status );

static void convert_fitted_values( smfData *data,
                            int axis, int ncomp, AstMapping **wcsmap,
                            smfArray *pardata, int *status );

static void ndf2array ( smfData *sdata, int axis, int ipar, int write,
			double *values, double *errors, int *status );

/*
**Main routine
*/

void smurf_fit1d( int * status )
{

  /* Local Variables */
  smfData *data = NULL;          /* Input data struct */
  size_t   i = 0;                /* Loop counter */

  size_t   in = 0;               /* File counter */
  int      indf;                 /* NDF identifier for input file */
  int      outndf;               /* Output NDF identifier */
  Grp     *igrp = NULL;          /* Input group */
  Grp     *ogrp = NULL;          /* Output group */
  size_t   outsize;              /* Nr of NDF names in the output group */
  size_t   size;                 /* Number of files in input group */
  int      icomp;                /* Component index (0 for base-line fit) */
  int      idim;                 /* Index of pixel axis */

  int      axis = 0;             /* Axis to fit along */
  int      iaxis = 0;            /* 0-based axis nr to fit along */
  smf_math_function fid;         /* Function id */
  int      lbnd[ NDF__MXDIM ];   /* Lower NDF pixel bounds */
  int      ubnd[ NDF__MXDIM ];   /* Upper NDF pixel bounds */
  double   range[2] = {0.0,0.0}; /* Range of coordinates to fit over */
  int      prange[2] = {0,0};    /* Range in pixel nrs 1..n */
  int      ncomp = 1;            /* Number of components to fit */
  double   rms = 1;              /* RMS in data */
  double   critamp = 1;          /* Minimal Amplitude */
  double   critdisp[2] = {0.8,0.0};   /* Mimimal Dispersions */
  int      estimate_only = 0;    /* Do estimates only, no fit */
  int      model_only = 0;       /* Create model only from supplied pars */
  fitStruct fcntrl;              /* Pointer to fit control struct */

  /* Parameter NDFs */
  AstKeyMap *usrvalmap=NULL;     /* Pointer to keymap for user values  */
  int       *parndfs;            /* Array of parameter NDFs */
  smfArray  *pardata;            /* Array of smfData pointers for par ndfs */

  /* WCS and axes */
  AstMapping *wcsmap = NULL;     /* Pointer to the Mapping from pixel
				    value to wcs value */
  /*  AstFrame *wcsframe = NULL;  Pointer to Frame describing the WCS axis */



  /* Initialize ndf */
  ndfBegin();

  msgOutiff( MSG__VERB, " ", "%s Started: Get input and output NDFs",
	     status, TASK_NAME );

  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Get output file(s) */
  kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
             &ogrp, &outsize, status );

  if (*status != SAI__OK) goto CLEANUP;

  /* Extract read user supplied values using a keymap */
  get_fit1par( &axis, range, &fid, &ncomp,
		   &rms, &critamp, critdisp, &estimate_only, &model_only,
		   status );

  if (*status != SAI__OK) goto CLEANUP;


  /*
  ** Process the group files
  */
  for ( in=1; in<=size && *status == SAI__OK; in++) {

    /* Copy the data across */
    msgOutf(" ", "Copying input file %d to output for fitting...",
	      status, (int) in);
    ndgNdfas( igrp, in, "READ", &indf, status );
    ndgNdfpr( indf, "WCS,DATA,VARIANCE,QUALITY", ogrp, in, &outndf, status );

    /* Update provenance in the output before we close the input */
    smf_updateprov( outndf, NULL, indf, "SMURF:FIT1D", NULL, status );
    if ( *status != SAI__OK ) {
      msgOut(" ", "Failed to update provenance, continuing...", status );
      *status = SAI__OK;
    }

    ndfAnnul( &indf, status);
    ndfAnnul( &outndf, status);


    /*
    ** Re-open the output file for UPDATE using standard routine
    */
    smf_open_file( ogrp, in, "UPDATE",
		   SMF__NOCREATE_DA | SMF__NOTTSERIES | SMF__NOCREATE_QUALITY,
		   &data, status );

    if ( *status == SAI__OK ) {
      if ( axis < 0 || axis > (int) data->ndims ) {
        *status = SAI__ERROR;
        msgSeti("A", (int) axis);
        msgSeti("D", (int) data->ndims);
        errRep ( FUNC_NAME, "Axis ^A not in valid range of 1 to ^D.", status );
        goto CLEANUP;
      }
      if (axis == 0) axis = (int) data->ndims;
    } else {
      msgSeti("F", in);
      errRep( FUNC_NAME, "Error opening output file ^F for UPDATE.", status);
      goto CLEANUP;
    }

    /* Get data dimensions */
    iaxis = axis - 1;
    for( idim = 0; idim < (int) data->ndims; idim++ ) {
       lbnd[idim] = data->lbnd[idim];
       ubnd[idim] = lbnd[idim] + data->dims[idim] - 1;
    }

    /* Setup an array to hold the identifiers for the parameter value NDFs
       (one for each component, plus one more for the base-line fit). */
    parndfs = astMalloc( (ncomp + 1)*sizeof( *parndfs ) );
    if ( !parndfs ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
        "Failed to set up array to hold the ids for the parameter value NDFs",
             status);
      goto CLEANUP;
    }

    pardata = smf_create_smfArray( status );
    if ( *status != SAI__OK ) {
      errRep( FUNC_NAME,
	"Failed to set up array for parameter smfData pointers",
	     status);
      goto CLEANUP;
    }

    /* Associate fit axis with WCS coordinate (if possible) */
    map_axis_to_wcs ( data, axis, &wcsmap, status );
    if ( wcsmap == NULL ) {
      msgOutf(" ", "Could not associated pixel axis %d with WCS coordinate axis:\nthe fit will be done using pixel coordinates",
	      status, axis);
    }

    /* Convert user-specified range to pixels */
    convert_range_to_pixels( &wcsmap, range, (int) lbnd[iaxis],
			     (int) ubnd[iaxis], prange, status );
    if ( *status != SAI__OK) {
      goto CLEANUP;
    }

    /* Each component is described by NPAR-1 parameters plus a id plane.
       NPAR is defined in the include file and a fixed rather than
       dynamic number is used for easy combination of different type
       of fits. Then initialize the COMP extensions. */
    setup_parameter_ndfs ( data, axis, ncomp, parndfs, pardata, status );
    if ( *status != SAI__OK) {
      goto CLEANUP;
    }

    /* Copy in parameters from external ndfs */
    copy_parameter_ndfs ( pardata, status );
    if ( *status != SAI__OK) {
      goto CLEANUP;
    }

    /* Fill the parameter files with user any supplied values */
    get_userval ( in, fid, ncomp, &usrvalmap, pardata, status );

    /* Fill the fit control structure */
    msgOutiff(MSG__DEBUG, " ", "Populate %s control struct", status,
	      FUNC_NAME);

    fcntrl.fid      = fid;
    for (i = 0; i < MAXPAR; i++) {
      fcntrl.fitmask[i] = 1;
    }
    fcntrl.clip[0]  = VAL__BADD;
    fcntrl.clip[1]  = VAL__BADD;
    fcntrl.rms      = rms;
    fcntrl.critamp  = critamp*rms;
    fcntrl.critdisp[0] = critdisp[0];
    fcntrl.critdisp[1] = critdisp[1];
    fcntrl.estimate_only = estimate_only;
    fcntrl.model_only = model_only;

    /*
    ** Fit the profiles
    */
    msgOutiff(MSG__VERB, " ", "Fitting profiles", status);
    smf_fit_profile( data, (int) axis, prange, (int) ncomp, pardata, &fcntrl,
		     status );


    /* The fit returns results (centre, dispersion) in pixels. Convert
       those back to coordinates */
    msgOutiff(MSG__VERB, " ", "Converting fitted values", status);
    convert_fitted_values( data, (int) axis, (int) ncomp,
			   &wcsmap, pardata, status );

    /* Set character labels */
    msgOutiff(MSG__DEBUG, " ", "Set labels", status);
    if (data) {
      smf_set_clabels( "Fitted profiles",NULL, NULL, data->hdr, status);
    }
    smf_write_clabels( data, status );

    msgOutiff(MSG__DEBUG, " ", "Close parameter NDFs", status);
    /* Close the NDFs used to store the parameter values. */
    if( parndfs ) {
       for( icomp = 0; icomp <= ncomp; icomp++ ) {
          ndfAnnul( parndfs + icomp, status );
       }
    }

    /* Free resources */
    msgOutiff(MSG__DEBUG, " ", "Free data file resources", status);
    smf_close_file( &data, status );
    parndfs = astFree( parndfs );
    pardata = astFree( pardata );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up: release the resources used by the grp routines  */
 CLEANUP:

  msgOutiff(MSG__DEBUG, " ", "Delete groups etc", status);
  if( igrp != NULL ) grpDelet( &igrp, status);
  if( ogrp != NULL ) grpDelet( &ogrp, status);
  if( usrvalmap ) usrvalmap = astAnnul( usrvalmap );

  msgOutiff(MSG__VERB, " ", "%s Finished", status, TASK_NAME);
  ndfEnd( status );
}


/* *** END OF MAIN *** */


static void get_fit1par( int *axis, double *range,
                         smf_math_function *fid, int *ncomp, double *rms,
                         double *critamp, double critdisp[],
                         int *estimate_only, int *model_only, int *status )
/*
** Routine to get parameters from the config file, if parameter is not
** NULL. Patterned after smf_get_cleanpar.c
*/
{
  AstKeyMap *keymap=NULL;        /* Pointer to keymap of config settings */

  if (*status != SAI__OK) return;

  /* Read configuration settings into keymap using defaults and typo
     checking. */
  msgOutif( MSG__VERB, " ", "Reading parameters from config file.", status );
  keymap = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_fit1d.def",
		       NULL, status );

  msgOutif( MSG__VERB, " ", "Extracting values from keymap", status );
  /* Obtain parameters from keymap when non-NULL pointers given */

  /* Get axis to fit along */
  if( axis ) {
    *axis = 0;
    astMapGet0I( keymap, "AXIS", axis );
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... AXIS=%d", status,
		 *axis );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter AXIS from config file",
	     status);
      return;
    }
  }

  /* Get range of pixels to fit over */
  if( range ) {
    int  nval = 0;
    if ( astMapGet1D( keymap, "RANGE", 2, &nval, range ) ) {
      if ( nval < 2 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "RANGE requires two values", status);
	return;
      }
      if ( *status == SAI__OK ) {
	msgOutiff( MSG__VERB, "", "... RANGE=[%f,%f]", status,
		   (float) range[0], (float) range[1] );
      }
    } else {
      range[0] = VAL__BADD;
      range[1] = VAL__BADD;
    }
  }
  if ( (range[0] == 0 && range[1] == 0) ||
       (range[0] == range[1]) ) {
    range[0] = VAL__BADD;
    range[1] = VAL__BADD;
  }

  {
    const char * strpntr = NULL;
    astMapGet0C( keymap, "FUNCTION", &strpntr );
    if (*status == SAI__OK && strpntr ) {
      *fid = smf_mathfunc_fromstring( strpntr, status );
      msgOutiff( MSG__VERB, "", "... FUNCTION=%s (id: %d)", status,
                 smf_mathfunc_str( *fid, status ), *fid );
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Failed to get parameter FUNCTION from config file",
	     status);
      return;
    }
  }

  /* Get ncomp to fit along */
  if( ncomp ) {
    *ncomp = 1;
    astMapGet0I( keymap, "NCOMP", ncomp );
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... NCOMP=%d", status, *ncomp );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter NCOMP from config file",
	     status);
      return;
    }
  }

  /* Rms in input map */
  if( rms ) {
    *rms = 0;
    astMapGet0D( keymap, "RMS", rms );
    if ( *status == SAI__OK ) {
      if ( *rms < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "RMS must be >= 0.", status );
      } else {
	msgOutiff( MSG__VERB, "", "... RMS=%f", status, *rms );
      }
    } else {
      errRep(FUNC_NAME, "Failed to get parameter RMS from config file",
	     status);
      return;
    }
  }

  /* Minimal acceptable amplitude for a component */
  if( critamp ) {
    *critamp = 0;
    astMapGet0D( keymap, "MINAMP", critamp );
    if ( *status == SAI__OK ) {
      if ( *critamp < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "MINAMP must be >= 0.", status );
	return;
      } else {
	msgOutiff( MSG__VERB, "", "... MINAMP=%f", status, *critamp );
      }
    } else {
      errRep(FUNC_NAME, "Failed to get parameter MINAMP from config file",
	     status);
      return;
    }
  }

  /* Minimal acceptable dispersion for a component */
  if( critdisp ) {
    critdisp[0] = 0;
    astMapGet0D( keymap, "MINFWHM", critdisp );
    if ( *status == SAI__OK ) {
      if( critdisp[0] < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "MINFWHM must be >= 0.", status );
	return;
      } else {
	msgOutiff( MSG__VERB, "", "... MINFWHM=%f", status,
		   critdisp[0] );
      }
    } else {
      errRep(FUNC_NAME, "Failed to get parameter MINFWHM from config file",
	     status);
      return;
    }
  }
  /* Convert to dispersion */
  critdisp[0] = FWHM2DISP( critdisp[0] );

  /* Minimal acceptable lorenztian FWHM for a component */
  if( critdisp ) {
    critdisp[1] = 0;
    astMapGet0D( keymap, "MAXLORZ", critdisp+1 );
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... MAXLORZ=%f", status,
		 critdisp[1] );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter MAXLORZ from config file",
	     status);
      return;
    }
  }
  /* Convert to dispersion */
  critdisp[1] = FWHM2DISP( critdisp[1] );

  /* Do initial estimates only */
  if( estimate_only ) {
    *estimate_only = NO;
    astMapGet0I( keymap, "ESTIMATE_ONLY", estimate_only );
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... ESTIMATE_ONLY=%d", status,
		 *estimate_only );
    } else {
      errRep(FUNC_NAME,
         "Failed to get parameter ESTIMATE_ONLY from config file",
	     status);
      return;
    }
  }

  /* Do initial estimates only */
  if( model_only ) {
    *model_only = NO;
    astMapGet0I( keymap, "MODEL_ONLY", model_only );
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... MODEL_ONLY=%d", status,
		 *model_only );
    } else {
      errRep(FUNC_NAME,
         "Failed to get parameter MODEL_ONLY from config file",
	     status);
      return;
    }
  }

  if( keymap ) keymap = astAnnul( keymap );

}


static void get_userval ( int in, smf_math_function fid, int ncomp, AstKeyMap **usrvalmap,
			  smfArray *pardata, int *status )
/*
** User supplied values. The routine opens a keymap file with parameters
** values either to use as initial estimates or to keep fixed in the fit.
** Parameters are indicated by a letter and a number (1..) indicating
** the component, with '0' meaning to apply to all components.
** Gauss*   a = amplitude, x = position, f = fwhm, s = skewness (hermite h3),
**          k = kurtosis (hermite h4),
** Voigt    a = amplitude, x = position, f = fwhm, l = lorentzian (voigt).
** Polynominal: c0, c1, c2, ... = c0 + c1*x + c2*x^2 + ...
** Optional zerolevel: z0, z1, z2 ( = z0 + z1*x + z2*x^2 )
**
** A full entry will be of the form 'a1 = (val1, val2)'. Val1 is the value
** for the given parameter; Val2 = 0 (fix, do not fit), 1 (use as initial
** estimate, but fit). Alternatively, the entry can be 'a1 = val1' for
** which val2 will default to 0 (fixed).
**
*/
{

  /* Allowed ids for non-polynomials */
  const char pars[] = { 'A', 'X', 'F', 'S', 'K', 'L' };
  int npar=sizeof(pars)/sizeof(pars[0]);

  /* Sequence nr of par in ndf pars */
  int ploc[] = {  1,   2,   3,   4,   5,   4 };

  int     icomp, ipar;                 /* Counters */
  char    cpar;                        /* Char code for parameter */
  double  dval[2];                     /* For returned value */

  Grp    *grp = NULL;                  /* Group to hold config values */
  size_t  size;                        /* Size of group */
  char   *value;                       /* Pointer to GRP element buffer */
  char   buffer[ GRP__SZNAM ];         /* Buffer for GRP element */

  if (*status != SAI__OK) return;

  if ( in == 1 ) {
    msgOutif( MSG__VERB, " ", "Reading user supplied parameters file.",
	      status );
    /* Attempt to read the specified defaults file into a GRP group. */
    grp = grpNew( "GRP", status );

    /* Read a group of configuration setting from USERVAL */
    kpg1Gtgrp( "USERVAL", &grp, &size, status );

    /* If no group was supplied, just annul any PAR__NULL error. */
    if( *status == PAR__NULL) {
      *status = SAI__OK;
      errAnnul( status );
      goto CLEANUP3;

      /* If a group was supplied, see if it consists of the single value "def".
	 If so, we will leave the KeyMap unchanged. */
    } else if (size > 0 ) {
      value = buffer;
      if( size == 1 ) {
	grpGet( grp, 1, 1, &value, sizeof(buffer), status );
      } else {
	strcpy( value, " " );
      }

      /* Otherwise, store the configuration settings in the KeyMap. */
      if( ! astChrMatch( value, "DEF" ) ) {
	kpg1Kymap( grp, usrvalmap, status );
      }
    } else {
      goto CLEANUP3;
    }
  }

  int nkey = astMapSize(  *usrvalmap );
  int ikey;
  for( ikey = 0; ikey < nkey; ikey++ ) {

    const char *key = astMapKey( *usrvalmap, ikey );
    sscanf( key,"%c%d", &cpar, &icomp );

    int  nval = 0;
    if ( astMapGet1D( *usrvalmap, key, 2, &nval, dval ) ) {
      if ( nval < 2 ) {
	dval[1] = 0;
      }
      if ( *status == SAI__OK ) {
	msgOutiff( MSG__VERB, "", "... %c%d=%f  %d", status,
		   cpar, icomp, (float) dval[0], (int) dval[1] );
      }
      if ( fid != SMF__MATH_POLYNOMIAL ) {
	/*          indf =
	** iplane = ploc[ipar];
	** fill_parndfs_plane();
	** fitmask[] = (int) dval[2]/ABS(dval[2])*((int) ABS(dval[2]+0.5));
	*/
      }
    } else {
      *status = SAI__OK;
    }

  }

 CLEANUP3:
  if ( grp != NULL ) grpDelet( &grp, status );

}


static void  map_axis_to_wcs ( smfData *data, int axis, AstMapping **wcsmap,
			       int *status )
/*
** Associate fit AXIS with WCS coordinate
*/
{
  int nout;                      /* Number of WCS axes driven by the
				    selected pixel axis */
  int *outs;                     /* Array of output axes driven by the
				    selected pixel axis */

  if (*status != SAI__OK) return;

  /* If the output NDF has a WCS FrameSet (copied from the input NDF),
     then attempt to get a Mapping from the selected pixel axis to the
     corresponding WCS axis, and also get a Frame describing the WCS
     axis. */
  if ( data && data->hdr && data->hdr->wcs ) {

    /* Allocate an array to receive the indicies of the WCS axes that
       are driven by the selected pixel axis. Make it the largest size
       that could possibly be needed (i.e. in case the pixel axis drives
       all the WCS axes). */
    nout = astGetI( data->hdr->wcs, "Naxes" );
    outs = astMalloc( nout*sizeof( *outs ) );

    /* Attempt to split up the total Mapping from pixel to WCS, to get a
       Mapping between the selected pixel axis and one or more WCS axes. */
    astMapSplit( data->hdr->wcs, 1, &axis, outs, wcsmap );

    /* Check the selected pixel axis could be split off from the other
       pixel axes. */
    if( *wcsmap ) {
      /* Check the selected pixel axis drives only one WCS axis. If
	 so, extract that one axis from the total WCS Frame. If not,
	 annul the Mapping pointer to indicate that we can only use pixel
	 coordinates. */
      if(  astGetI( *wcsmap, "Nout" ) != 1 ) {
	*wcsmap = astAnnul( *wcsmap );
	/*
      } else {
	*wcsframe = astPickAxes(  data->hdr->wcs, 1, outs, NULL );
	*/
      }
    }

    /* Free the array of driven WCS axes */
    outs = astFree( outs );
  }
}


static void convert_range_to_pixels ( AstMapping **wcsmap, double *range,
			   int lbnd, int ubnd, int *prange, int *status )
/*
** Convert given range to pixels (running 1..n) along the fit axis and return
** as prange. lbnd and ubnd are grid bounds along the fit axis.
**
*/
{
  double lrange[2];
  int ndims;

  /* Number of pixels along axis */
  ndims = ubnd-lbnd+1;

  /* Default: full axis */
  if ( range[0] == VAL__BADD ) {

    lrange[0] = 1;
    lrange[1] = ndims;

  } else if (!wcsmap) {

      msgOut("",
	     "*WARNING* Interpreting RANGE as pixel values!!!", status);
      lrange[0] = range[0]-lbnd+1;
      lrange[1] = range[1]-lbnd+1;

  } else {
    convert_coord_to_pixel ( wcsmap, range, 2, lrange, status );
  }

  /* Now covert to data array index numbers: +0.5 (round) */
  prange[0] = (int) (lrange[0]+0.5);
  prange[1] = (int) (lrange[1]+0.5);

  if ( range[0] == VAL__BADD ) {
     msgOutiff(MSG__VERB, " ",
          "Range undefined => Full pixel range: %d to %d\n",
          status, prange[0], prange[1]);
  } else {
     msgOutiff(MSG__VERB, " ", "Range %f to %f => Pixel range: %d to %d\n",
	    status, range[0], range[1], prange[0], prange[1]);

  if ( prange[0] >= (int) ndims || prange[1] <= 1 ) {
    msgOutf( "", "Range results in a pixel range %d to %d that is beyond the input ndf axis %d to %d",
	    status, prange[0]+lbnd-1, prange[1]+lbnd-1, lbnd, ubnd );
    *status = SAI__ERROR;
    errRep(TASK_NAME, "Invalid RANGE specified.", status);
  }
}


static void convert_pixel_to_coord ( AstMapping **wcsmap, double *pixel,
				     int nval, double *coord, int *status )
/*
** Convert given pixels (running 1..n) too coordinate values along the
** fit axis.
*/
{
  int i;

  /* Change pixels to array indices: if no mapping exists interpret
     range as grid values */
  if ( wcsmap ) {
      astTran1( *wcsmap, 2, pixel, 1, coord );
  } else {
    msgOut("",
	   "*WARNING* No valid axis mapping: returned values are pixels",
	   status);
    for ( i = 0; i < nval; i++ ) {
      coord[i] = (double) pixel[i];
    }

  }
}


static void convert_coord_to_pixel ( AstMapping **wcsmap, double *coord,
				     int nval, double *pixel, int *status )
/*
** Convert given coordinate values to pixels (running 1..n) along the
** fit axis.
*/
{
  int i;

  /* Only if valid mapping exists */
  if ( wcsmap ) {
      astTran1( *wcsmap, 2, coord, 0, pixel );
  } else {
    msgOut("",
	   "*WARNING* No valid axis mapping: values not converted",
	   status);
    for ( i = 0; i < nval; i++ ) {
      pixel[i] = (double) coord[i];
    }

  }
}


static void setup_parameter_ndfs ( smfData *data, int axis, int ncomp,
				   int *parndfs, smfArray *pardata,
				   int *status )
/*
** Create NDF extensions for the parameter values. These have the same data
** dimensions as the original file, except that along the fit axis the
** extent will be 1..NPAR. Create an extension for each fitted component
** COMP_1...COMP_n, plus one for diagnostics and shared baselines: COMP_0.
*/
{
  /* Parameter NDFs */
  int icomp;                     /* Component index (0 for base-line fit) */
  int idim;                      /* Index of pixel axis */
  int lbndp[ NDF__MXDIM ];       /* Lower NDF pixel bounds for par. NDFs */
  int ubndp[ NDF__MXDIM ];       /* Upper NDF pixel bounds for par. NDFs */

  HDSLoc  *smurfloc=NULL;        /* HDS locator of SMURF extension */
  char ndfnam[ 10 ];             /* Name for parameter NDF */
  int place;                     /* Place holder for parameter NDF */
  void *pntrs[2];       /* Generic pointers for Data and Variance arrays */
  int el;                        /* No. of elements mapped in each array */


  if (*status != SAI__OK) return;


  /* Configure bounds for parameter arrays: along "axis" it will be
     1..NPAR */
  for( idim = 0; idim < (int) data->ndims; idim++ ) {
    lbndp[ idim ] = data->lbnd[ idim ];
    ubndp[idim] = data->lbnd[idim] + data->dims[idim] - 1;
  }
  lbndp[ (axis-1) ] = 1;
  ubndp[ (axis-1) ] = NPAR;


  /* Create SMURF extension in the output file and map pointer to
     fitted data arrays */
  msgOutiff(MSG__DEBUG, " ", "Creating %s extension for %d components",
	    status, FIT1D__EXTNAME, ncomp+1);
  smurfloc = smf_get_xloc ( data, FIT1D__EXTNAME, FIT1D__EXTTYPE,
			    "WRITE", 0, 0, status );

  /* Loop round to create an NDF within the FIT1D extension for each
     component. Each NDF will be used to store the "NPAR" parameter
     values for the component, at each spatial position. Note, we
     create NCOMP+1 NDFs since the first (COMP_0) is used to store
     the fit diagnostics and base-line fit common to all components. */
  for ( icomp = 0; icomp < ncomp+1; icomp++ ) {

    /* Create a place holder for a new NDF within the FIT1D extension,
       with a name of the form "COMP_<icomp>". */
    sprintf( ndfnam, "COMP_%d", icomp );
    msgOutiff(MSG__DEBUG, " ", "...create parameter extension %s",
	      status, ndfnam);
    ndfPlace( smurfloc, ndfnam, &place, status );

    /* Create the new NDF by copying the output NDF to the place holder
       created above, propagating only the WCS component. So the new NDF
       initially will have the same shape as the output NDF, but this
       will be changed below. */
    /* msgOutiff(MSG__DEBUG, " ", "...copy ndf", status); */
    parndfs[icomp] = NDF__NOID;
    ndfScopy( data->file->ndfid, "WCS,NOHISTORY,NOLABEL,NOTITLE,"
	      "NOEXTENSION(*)", &place, parndfs+icomp, status );

    /* Change the shape of the new NDF so that the pixel axis with
       index "axis" has bounds of 1 to NPAR. */
    /* msgOutiff(MSG__DEBUG, " ", "...change shape", status); */
    ndfSbnd( data->ndims, lbndp, ubndp, parndfs[icomp], status );

    /* Change the parameter type the _REAL */
    ndfStype( "_DOUBLE", parndfs[icomp], "DATA", status );
    ndfStype( "_DOUBLE", parndfs[icomp], "VARIANCE", status );

    if (*status != SAI__OK) break;

    /* Map data and variance array of parameter ndfs: 0'th element will
       be used for diagnostics and baseline */
    /* msgOutif(MSG__DEBUG, " ", "...map data & variance", status); */
    ndfMap( parndfs[icomp], "Data,Variance", "_DOUBLE", "WRITE/BAD",
	    pntrs, &el, status );

    smfData *pdata = smf_create_smfData( SMF__NOCREATE_DA   |
                                         SMF__NOCREATE_HEAD |
					 SMF__NOCREATE_FILE |
					 SMF__NOCREATE_FTS, status );

    pdata->ndims = data->ndims;
    for( idim = 0; idim < (int) pdata->ndims; idim++ ) {
      pdata->dims[ idim ] = ubndp[ idim ] - lbndp[ idim ] + 1;
      pdata->lbnd[ idim ] = lbndp[ idim ];
    }
    pdata->dtype = SMF__DOUBLE;
    (pdata->pntr)[0] = pntrs[0];
    (pdata->pntr)[1] = pntrs[1];

    smf_addto_smfArray( pardata, pdata, status );

  }

}


static void copy_parameter_ndfs ( smfArray *pardata, int *status )
/*
** Copy in external ndf parameter files.
*/
{
  /* Parameter NDFs */
  smfData  *pdata = NULL;           /* smfData for component */
  int       icomp;                  /* Component index (0 for base-line fit) */
  int       idim;                   /* Index of pixel axis */
  smfData  *cdata = NULL;           /* Parameter file data struct */

  size_t    csize = 0;              /* Diag ndfs group size */
  size_t    psize = 0;              /* Param ndfs group size */
  Grp      *cgrp = NULL;            /* Components ndfs group */
  Grp      *pgrp = NULL;            /* Parameter ndfs group */
  char      filename[GRP__SZNAM+1] = ""; /* Filename */
  char     *pname = NULL;           /* Temporary pointer */
  int       added;                  /* GRP parameters */
  int       flag;                   /* GRP parameters */

  void     *parpntr, *comppntr;     /* Void pointers */

  if (*status != SAI__OK) return;

  /* Ask for component 0 separately, but if more than one name
     is returned assume it is the full set of ndfs desired and
     try to skip PARNDF. */

  /* Get file names from DIAGNDF */
  kpg1Rgndf( "DIAGNDF", 0, 0, "", &cgrp, &csize, status );

  if ( *status == PAR__NULL ) {
    /* Add empty string in location 1 of the group for COMP_O */
    *status = SAI__OK;
    cgrp = grpNew( "GRP", status );
    grpPut1( cgrp, filename, 0, status );
    csize = 1;
  } else if ( *status != SAI__OK ) {
    errRep(TASK_NAME, "Error reading DIAGNDF parameter", status);
    *status = SAI__OK;
  }

  /* Get file names from PARNDF if DIAGNDF not a list */
  if ( csize < 2 ) {

    kpg1Rgndf( "PARNDF",  0, 0, "", &pgrp, &psize, status );

    if ( *status == PAR__NULL ) {
      psize = 0;
      *status = SAI__OK;
    } else if ( *status != SAI__OK ) {
      errRep(TASK_NAME, "Error reading PARNDF parameter", status);
      *status = SAI__OK;
    } else {
      pname = filename;
      for ( int i = 1; i <= (int) psize; i++ ) {
	grpGet( pgrp, i, 1, &pname, GRP__SZNAM, status );
	grpPut1( cgrp, filename, 0, status );
      }
      csize += psize;
    }

  }

  if ( csize == 0 ) goto CLEANUP2;


  size_t ncomp = pardata->ndat;
  if ( ncomp < csize ) {
    *status = SAI__ERROR;
    msgOutf( " ",
    "ERROR: Nr parameter files (%d) exceeds number of components being fitted (%d)",
	     status, (int) csize, (int) ncomp );
    goto CLEANUP2;
  } else if ( csize > 1 && csize < ncomp ) {
    msgOutf(" ", "WARNING: Nr par. files (%d) less than components being fitted (%d),\n remaining components will be estimated.\n",
	    status, (int) csize-1, (int) ncomp-1 );
  }

  for ( int in = 1, icomp = 0; in <= (int) csize && icomp < (int) ncomp;
	in++, icomp++ ) {

    pname = filename;
    grpGet( cgrp, in, 1, &pname, GRP__SZNAM, status );

    if ( strcmp(filename, "") != 0 ) {

      /* Number of points in input data parameter files */
      pdata = pardata->sdata[icomp];
      int dpts = 1;
      for ( idim = 0; idim < (int) pdata->ndims; idim++ ) {
	dpts *= pdata->dims[idim];
      }

      /* Open and copy the NDF data. */
      smf_open_file( cgrp, in, "READ",
		 SMF__NOCREATE_DA | SMF__NOTTSERIES | SMF__NOCREATE_QUALITY,
		 &cdata, status );

      if ( *status != SAI__OK ) {
	msgSeti("F", in);
	errRep( FUNC_NAME, "Error opening output file ^F for UPDATE.", status);
	goto CLEANUP2;
      }

      /* Number of points in user-specified parameter file */
      int cpts = 1;
      for ( int idim = 0; idim < (int) cdata->ndims; idim++ ) {
        cpts *= cdata->dims[idim];
      }

      if ( cpts != dpts || pdata->ndims != cdata->ndims ) {
	msgOutf(" ", "NDF: '%s' %d axes %d points; %s: %d axes, %d points\n",
		status, filename, (int) pdata->ndims, dpts,
		filename, (int) cdata->ndims, cpts );
	*status = SAI__ERROR;
	errRep(" ",
		"ERROR: IN ndf and %s differ in size or number of axes\n",
		status );
	smf_close_file( &cdata, status );
      }
      if ( *status != SAI__OK ) goto CLEANUP2;

      /* Copy data and variance */
      for ( int j = 0; j < 2; j++ ) {
        parpntr  = (pardata->sdata[icomp]->pntr)[j];
        comppntr = (cdata->pntr)[j];
	smf_dtype_arraycopy( parpntr, pardata->sdata[icomp]->dtype,
                             comppntr, cdata->dtype, cpts, status );
	if ( *status != SAI__OK ) goto CLEANUP2;
      }

    }
  }

 CLEANUP2:
  if( cgrp != NULL ) grpDelet( &cgrp, status);
  if( pgrp != NULL ) grpDelet( &pgrp, status);

}


static void convert_fitted_values( smfData *data,
			  int axis, int ncomp, AstMapping **wcsmap,
			  smfArray *pardata, int *status )
/*
** This routine converts the pixel-based fitted parameters to coordinate
** based ones: e.g. pixels (1..n) along the third axis to velocities
*/
{
  int      idim;                 /* Index of dimension */
  int      icomp;                /* Index of component */
  int      ipar;                 /* Index of fitted parameter */
  int      iprof = 0;            /* Profile counter */

  int      iaxis = 0;            /* 0-based axis nr to fit along */
  int      lbnd[ NDF__MXDIM ];   /* Lower NDF pixel bounds */
  int      ubnd[ NDF__MXDIM ];   /* Upper NDF pixel bounds */
  int      nprofiles = 1;        /* Number of profiles fitted */
  size_t   dstride = 1;          /* Data stride: separation of
                                    pixels along AXIS in data cube */
  double  *pixval, *pixerr;      /* Array for pixel values and errors*/
  double  *wcsval, *wcserr;      /* Array for coordinate values and errors */

  double   dval1[2], dval2[2];   /* Doubles... */
  double   centre, pixscale;     /* Pixelscale */


  if (*status != SAI__OK) return;

  /* No mapping exists between pixel and coordinate */
  if ( !wcsmap ) {
    msgOut(" ",
	   "(***WARNING*** Results are in pixels!!!", status );
    return;
  }

  /* Get dimensions, number of profiles, and dstride */
  iaxis = axis - 1;
  for( idim = 0; idim < (int) data->ndims; idim++ ) {
    lbnd[idim] = data->lbnd[idim];
    ubnd[idim] = lbnd[idim] + data->dims[idim] - 1;
    nprofiles *= data->dims[idim];
    if ( idim < iaxis ) {
      dstride *= data->dims[idim];
    }
  }
  nprofiles /= data->dims[iaxis];

  /* Find pixel scale */
  centre = (int) (data->dims[iaxis]/2.0+0.51); /* Centrish pixel */
  dval1[0] = centre - 1;
  dval1[1] = centre + 1;
  astTran1( *wcsmap, 2, dval1, 1, dval2 );
  if (*status != SAI__OK) return;

  pixscale = fabs(0.5*(dval2[1]-dval2[0]));

  /* Allocate working buffers */
  pixval = astMalloc( (nprofiles)*sizeof( *pixval ) );
  pixerr = astMalloc( (nprofiles)*sizeof( *pixerr ) );
  wcsval = astMalloc( (nprofiles)*sizeof( *wcsval ) );
  wcserr = astMalloc( (nprofiles)*sizeof( *wcserr ) );

  /* Cycle true the extension cubes with the fitted values.
  ** dstride gives the number of elements that seperate adjacent
  ** points along AXIS. Logically this splits the hypercube into
  ** nprofiles/dstride subcubes each with dstride profiles. Thus we can
  ** cycle through the profiles by cycling over the subcubes and each
  ** profile in the subcube.
  */


  /* skip COMP_0 diagnostics extension */
  for ( icomp = 1; icomp < ncomp+1; icomp++ ) {

    /* Loop over planes in each component: 1=Centre, 2=Dispersions, etc.
       skip 0=Amp and final plane with FID */
    for ( ipar = 1; ipar < NPAR-1; ipar++ ) {

      /* Go through the process twice: first to get values
	 then to write the converted values back */

      /* Read fitted values */
      ndf2array( pardata->sdata[icomp], axis, ipar, 0, pixval, pixerr,
		 status );

      if (ipar == 1) {
	/* Fitted centre values */

	astTran1( *wcsmap, (int) nprofiles, pixval, 1, wcsval );
	if (*status != SAI__OK) return;

	/* Errors scale with pixscale */
	for ( iprof = 0; iprof < nprofiles; iprof++ ) {
	  if ( pixerr[iprof] != VAL__BADD) {
	    wcserr[iprof] = pixerr[iprof]*pixscale;
	  } else {
	    wcserr[iprof] = VAL__BADD;
	  }
	}

      } else {
	/* Assume that all remaining scale with pixscale for now */

	for ( iprof = 0; iprof < nprofiles; iprof++ ) {
	  if ( pixval[iprof] != VAL__BADD) {
	    wcsval[iprof] = DISP2FWHM(pixval[iprof]*pixscale);
	  } else {
	    wcsval[iprof] = VAL__BADD;
	  }

	  if ( pixerr[iprof] != VAL__BADD) {
	    wcserr[iprof] = DISP2FWHM(pixerr[iprof]*pixscale);
	  } else {
	    wcserr[iprof] = VAL__BADD;
	  }
	}

      }

      /* Write converted values back */
      ndf2array( pardata->sdata[icomp], axis, ipar, 1, wcsval, wcserr,
		 status );

    } /* end loop over parameters */

  } /* end loop over components */

  pixval = astFree( pixval );
  pixerr = astFree( pixerr );
  wcsval = astFree( wcsval );
  wcserr = astFree( wcserr );

}


static void ndf2array ( smfData *sdata, int axis,  int ipar, int write,
			double *values,  double *errors, int *status )
/*
** This helper routine gathers a ndf plane from the ndf data objects
** into linear arrays or writes such arrays back into the ndf. Axis
** is the dimension along which 'planes' (subcubes) should be extracted
** and ipar the pixel coordinate (1..ndim). If write = 0 the values and
** variances assiciated with the plane are returned in the values and
** errors arrays. If write = 1, the are read from the arrays to the planes.
*/
{
  size_t   k, l;                 /* Loop counters */
  int      idim;                 /* Index of dimension */
  int      iprof = 0;            /* Profile counter */
  size_t   index;                /* Array index */

  int      iaxis = 0;            /* 0-based axis nr to fit along */
  int      lbnd[ NDF__MXDIM ];   /* Lower NDF pixel bounds */
  int      ubnd[ NDF__MXDIM ];   /* Upper NDF pixel bounds */
  int      ndata = 1;            /* Number of elements in cube */
  int      nplanes = 1;          /* Number of planes/subcubes in cube */
  size_t   dstride = 1;          /* Data stride: separation of
                                    pixels along AXIS in data cube */
  size_t   pbase;                /* Data offset location for profile */

  double   *pdata, *pvari;       /* Pointer to data and variance */

  if (*status != SAI__OK) return;

  pdata = (sdata->pntr)[0];
  pvari = (sdata->pntr)[1];

  /* Get dimensions, number of planes, and dstride */
  iaxis = axis - 1;
  for( idim = 0; idim < (int) sdata->ndims; idim++ ) {
    lbnd[idim] = sdata->lbnd[idim];
    ubnd[idim] = lbnd[idim] + sdata->dims[idim] - 1;
    ndata *= sdata->dims[idim];
    if ( idim < iaxis ) {
      dstride *= sdata->dims[idim];
    }
  }
  nplanes = ndata / sdata->dims[iaxis];

  /* Cycle trough the extension cubes with the fitted values.
  ** dstride gives the number of elements that seperate adjacent
  ** points along AXIS. Logically this splits the hypercube into
  ** nplanes/dstride subcubes each with dstride planes. Thus we can
  ** cycle through the planes by cycling over the subcubes and each
  ** plane in the subcube.
  */

  /* Number of subcubes to loop over */
  size_t nsubcubes = (int) (nplanes/dstride+0.5);

  /* Loop over subcubes */
  iprof = 0;
  for ( l = 0; l < nsubcubes; l++ ) {

    /* Extract the subcube into a linear array for type conversion */

    /* Loop over planes in subcube: since the points are
       dstride apart, there are also dstride planes in the subcube */
    for ( k = 0; k < dstride; k++, iprof++ ) {

      /* Offset into current data array */
      pbase = l*dstride*sdata->dims[iaxis] + k;

      /* Select fitted parameter */
      index = pbase + ipar*dstride;

      /* Assume reals */
      if ( write == 1 ) {                           /* Write */
	pdata[index]  = (double) values[iprof];
	pvari[index]  = (double) errors[iprof];
      } else {                                      /* Read */
	values[iprof] = (double) pdata[index];
	errors[iprof] = (double) pvari[index];
      }

    }
  }

}
