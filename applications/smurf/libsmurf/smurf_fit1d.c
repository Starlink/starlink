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
*     A default config file is in $SMURF_DIR/smurf_fit1d.def. A sample
*     file for user specified values is $SMURF_DIR/smurf_fit1d_uval.def.
*
*  Usage
*     fit1d in out rms config [userval] [pardir] [parndf] [parcomp]

*  ADAM Parameters:
*     CONFIG = GROUP (Read)
*          Specifies values for the configuration parameters used by the
*          FIT1D. If the string "def" (case-insensitive) or a null (!)
*          value is supplied, a set of default configuration parameter
*          values will be used from $SMURF_DIR/smurf_fit1d.def.  See the
*          "Configuration Parameters" topic for detailed information.
*     IN = NDF (Read)
*          Baselined input file(s).
*     OUT = NDF (Write)
*          Output file(s) with the fitted profiles.
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null value is supplied no file is created. [!]
*     PARCOMP = GROUP (Read)
*          Component parameter file(s) to use for initial estimates, fixed
*          values, or to generate a model with (see "model_only"). Instead
*          of a comma separated string of filenames a "^list" file can be
*          submitted with each filename on a separate line. Files will map
*          to components 1..N in the order as specified. See "Fitted
*          Parameters" for more information on parameter files.
*
*          The full specification of a parameter file name consists of three
*          parts: an optional directory path ("pardir"), an optional container
*          ndf ("parndf") and plus the base name for the file ("parcomp"):
*                   "pardir"/"parndf".MORE.SMURF_FIT1D."parcomp"
*          For convenience "pardir" and "parndf" can be specified through
*          their respective parameters, but full name(s) can also be
*          specified through "parcomp". In case "pardir" is specified, FIT1D
*          will append a "/". Similarly, if "parndf" is given the string
*          ".MORE.SMURF_FIT1D." will be appended. Note that leaving "parndf"
*          blank will result in a conventional "pardir"/"filename" path.
*
*          In case "parndf" is specified, but "parcomp" is not, all components
*          in the container file will be read. Note that COMP_0 will be skipped
*          since it hold diagnostics information about the fit.
*
*          To escape special characters (",", ".", "@", etc) in the string
*          you may need to use a set of single+double quotes. A null value
*          means no component parameter files will be used. [!]
*     PARDIR = LITERAL (Read)
*          Directory with component parameter files or the parameter NDF.
*          For details see help on PARCOMP. To escape special characters
*          (",", ".", "@", etc) in the string you may need to use a set of
*          single+double quotes. A null value results in use of the current
*          directory. [!]
*     PARNDF = LITERAL (Read)
*          NDF resulting from a previous execution of FIT1D and containing
*          component parameter files as part of its meta-data to use in the
*          current fit. The components are stored as
*          "parndf".MORE.SMURF_FIT1D.COMP_#.
*          For further details see help on PARCOMP. To escape special
*          characters (",", ".", "@", etc) in the string you may need to use
*          a set of single+double quotes. [!]
*     RMS = _DOUBLE (Read)
*          RMS in input NDF(s) in data units.
*     USERVAL = GROUP (Read)
*          Input keymap file with user-supplied fixed values or initial
*          estimates for the fit and a flag whether parameters are to
*          be kept fixed in the fit. The sample/default keymap file is
*          $SMURF_DIR/smurf_fit1d_uval.def. Entries are of the form:
*          "comp"#."fid" = value, "comp"#.par = value, or "fix"#.par = [0,1]
*          with 'par' being a parameter relevant for the function being
*          fitted. '#' (1..n) indicates the component profile being described.
*          Fix indicates a parameter to be kept fixed or fitted.
*
*          If specified "comp"#.fid will override the default function
*          selected in the config file.
*          Parameter names are described in the help item "Fitting Functions"
*                              comp.fid      comp.par
*              Gauss:              1        a, b, c
*              Gausshermite1:      2        a, b, c, h3
*              Gausshermite2:      3        a, b, c, h4
*              Voigt:              4        a, b, c, l
*
*          The "fix"#.par parameter can have a value of:
*               1 = fix parameter at given value, do not fit -or-
*               0 = use as initial estimate, but fit.
*          As for comp, the '#' (1..n) indicates the component.
*
*          A null value for USERVAL means that no user-supplied estimates
*          or fixed values are to be used.  [!]

*  Configuration Parameters:
*     A default configuration file can be found at $SMURF_DIR/smurf_fit1d.def.
*     ABSH3MIN, ABSH3MAX = REAL
*          Min and Max allowable value for the H3 (~skew) parameter in a
*          Gausshermite# fit. If RETRY=1 has been set, a pure gaussian fit
*          (H3=0) will be attempted in case the initial fit of H3 is out
*          of bounds. Set to <undef> if no limit desired. [0.01, 0.5]
*     ABSH4MIN, ABSH4MAX = REAL
*          Min and Max allowable value for the H4 (~peakiness) parameter in
*          a Gausshermite4 fit. If RETRY=1 has been set, a gausshermite1 fit
*          (H4=0) will be attempted in case the initial fit of H4 is out of
*          bounds. Set to <undef> if no limit desired. [0.01, 0.35]
*     AXIS = INTEGER
*          Axis to fit along (starting at 1). A value of 0 translates
*          as fit along highest dimension i.e. Vlsr in a Ra, Dec, Vlsr cube.
*          [0]
*     CLIP(2) = REAL
*          Values in the input profiles outside the specified clip-range
*          [min,max] will be not be used in the fit.
*     ESTIMATE_ONLY = LOGICAL
*          Set to 1: The output cube will have the results from the
*          initial estimates routine instead of the the fit.  Good
*          initial estimates are critical for the fit and checking
*          and/or fixing initial estimates may help solve problems. [0]
*     FUNCTION = STRING
*          Function to fit.  Currently implemented are "gaussian",
*          "gausshermite1", "gausshermite2", "voigt".
*          See topic "Fitting Functions" for details. If your aim is to
*          capture a much emission as possible e.g. in order to create a
*          2-D image from a 3-D cube, gausshermite2 profiles are
*          recommmended. ["gausshermite2"]
*     MAXLORZ = REAL
*          Maximum value for the FHWM of the Lorentzian component ("L") in
*          a Voigt fit in terms of ==PIXELS==(!). If RETRY=1 has been set,
*          a pure gaussian fit (L=0) will be attempted in case the initial
*          fit of H3 is out of bounds. [<undef>]
*     MINAMP = REAL
*          Minimum value for the Amplitude-like parameter to accept as a
*          genuine fit in terms of the RMS(!). Based on this alone at 3-sigma
*          ~5% of the profiles selected for fitting can be expected to be
*          noise spikes. This value drops to ~2% for 5-sigma. All
*          assuming gaussian statistics of course.  [3]
*     MINWIDTH = REAL
*          Minimum value for the FHWM (~2.35*Dispersion) to accept as a
*          genuine fit in terms of ==PIXELS==(!). [1.88]
*     MODEL_ONLY = LOGICAL
*          Set to 1: Bypass both the initial estimates and fitting
*          routine and generate profiles directly from the supplied
*          input parameter cube(s) and/or user supplied fixed values.
*          Not supplying all parameters will generate an error. [0]
*     NCOMP = INTEGER
*          Maximum number of 'component' functions to fit to each
*          profile, e.g. a multi-component spectrum of maximum three
*          gaussians. [Note: The complete fit of the gaussians is done
*          concurrently, not iteratively starting e.g. with the strongest
*          component]. The routine will try to find and fit ncomp
*          functions along each profile, but may settle for less. [3]
*     POS_ONLY = LOGICAL
*          FIT1D expected profiles to have been baselined i.e. fitted
*          profiles typically should not have a negative values. However,
*          Gausshermite profiles naturally give rise to undesired negative
*          features e.g. in fitting skewed profiles. This parameter simply
*          causes the routine to set values in the output profiles to zero
*          whereever they are negative. This generally gives better matching
*          profiles, but of course means the fits are not pur gausshermites
*          anymore. Make sure to set this parameter to NO of your profiles
*          have genuine negative features e.g. as in p-cygni profiles. [YES]
*     RANGE(2) = REAL
*          Coordinate range along axis to find and fit profiles. The
*          format is (x1, x2) including the ().  For example,
*          Vlsr -20 35 is "(-20,35)".
*          Default is to use the full extent of the axis: [<undef>]
*     RETRY = LOGICAL
*          Whenever the lorentzian ("L") or hermite parameters ("H3", "H4")
*          are out of the routine re-tries the fit with the out-of-bounds
*          parameter(s) fixed at 0. This means that in effect the fit cascades
*          to a simpler function:
*             gausshermite2 -> gausshermite1 -> gaussian; voigt -> gaussian.
*          The result is that there are valid fits for more profiles, but the
*          function actually fitted may vary with position.
*          Setting the retry value to 0 prevents this from happening and may
*          cause the fit to fail or be (very) poor. [YES]
*     SORT = STRING
*          Sort the resulting fits:
*          "amp":      sort by decreasing fitted value of the amp-like parameter
*          "width":    sort by decreasing fitted fwhm of the width-like
*                      parameter
*          "position": sort by increasing position along the profile axis
*          "distance": sort by increasing fitted distance from the centre
*                      pixel in the profile.
*          Sorting can be helpful, but be cautioned that it can also
*          complicate things: if there are two components one at -10 km/s
*          and one at 10 km/s sorting by amplitude or width can result
*          in the parameter file for component 1 to be a mix of the -10
*          and 10 km/s features depending on which one was relatively
*          stronger or wider. Similarly, sorting by position can result in
*          low-amplitude fits of noise spikes to be mixed with stronger
*          components. For more precise control try to run the routine
*          iteratively with e.g. a different restricted velocity range to
*          try pick out the different components. Default is to sort by
*          amplitude. ["amp"]
*     SORT_ESTIMATE = LOGICAL
*          Sort initial estimates also with the sorting selected in 'sort'.
*          Estimates can be very inaccurate plus are not checked against
*          any boundary limits until after the fit. Thus this option may
*          not be very helpful.

*  Fitting Functions:
*     The function menu provides the choice of four functions for which
*     you can fit the parameters to the data in your profiles.
*
*     1) A standard GAUSSIAN. Parameters are
*            a = maximum, b = centre, and c = FWHM.
*
*     NOTE that if one of h3 and h4 in a gauss-hermite function is non-zero,
*     the mean of the distribution is not the position of the maximum
*     (Reference; Marel, P. van der, Franx, M., A new method for the
*     identification of non-gaussian line profiles in elliptical galaxies.
*     A.J., 407 525-539, 1993 April 20):
*
*     2) GAUSS-HERMITE1 polynomial (h3). Parameters are
*            a (amplitude), b (position),c (width), and h3
*     as mentioned these are *NOT* the same as maximum, centre, and fwhm
*     of the distribution as for a gaussian:
*                 maximum ~= [determine value and position of max from
*                             fitted profiles using e.g. collapse]
*                  centre ~= b + h3*sqrt(3)
*                    FWHM ~= abs( c*(1-3h3^2) ) ~= c
*                skewness ~= 4*sqrt(3)*h3
*
*     3) GAUSS-HERMITE2 polynomial (h3, h4). Same as previous, but an
*        extra parameter h4 is included:
*                 maximum ~= [determine value and position of max from
*                             fitted profiles using e.g. collapse]
*                  centre ~= b + h3*sqrt(3)
*                    FWHM ~= abs( c*(1+h4*sqrt(6)) )
*                skewness ~= 4*sqrt(3)*h3
*                kurtosis ~= 8*sqrt(6)*h4
*
*     4) VOIGT function. Parameters are
*            a (area), b (centre), c (doppler FWHM), l (lorenztian FWHM),
*            and v (area factor)
*        with relations:
*                 maximum ~= [determine value of max from fitted
*                             profiles using e.g. collapse]
*                  centre ~= b
*            doppler fwhm ~= c
*         lorentzian fwhm ~= l
*                     amp  = v (OUTPUT ONLY!) amplitude calculated from
*                            a (area) using the standard amp2area function
*                            for a voigt (based on the Faddeeva complex
*                            error function):
*                               amp = area / amp2area

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
*                (gaussian)      (gausshermite)      (voigt)
*          1 =    amplitude          'a'              area
*          2 =    position           'b'            position
*          3 =      fwhm             'c'           doppler fwhm  'd'
*          4 =       -               'h3'        lorentzian fwhm 'l'
*          5 =       -               'h4'          amp2area      'v'
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
*         <-10 All fitted components rejected due to minamp, minwidth,
*                maxlorz, or range constraints.

*  Examples:
*    fit1d in=orion out=orion_gauh2 rms=0.22
*       Fits using the settings as defined in the default Configuration
*       file "$SMURF_DIR/smurf_fit1d.def": fitting a single gausshermite2
*       to each profile along the highest dimension of the file "orion.sdf".
*       The input file is expected to be baselined and the zerolevel
*       of the profile to be 0. The fitted profiles are stored in the
*       file "orion_gauh2.sdf" with a component parameter file with the
*       gauss-hermite parameters a, b, c, h3, h4 as
*       "orion_gauh2.more.smurf_fit1d.comp_1"
*       (plus ...comp_0 for the diagnostics component).
*    fit1d in=orion out=orion_gauss rms=0.22 \
*          config='"^$SMURF_DIR/smurf_fit1d.def,function=gauss"'
*        Same as above, but fit a single gaussian instead. Alternatively,
*        use config='"^myfits1d.def"' having the following lines:
*               ^$SMURF_DIR/smurf_fit1d.def
*               function = gaussian
*    fit1d in=orion out=orion_gauh2 rms=0.22 parndf=orion_gauss
*        As in the first example fit a gausshermite2, but use the output
*        from the gaussian fit of the second example for initial estimates.
*        This can help or do harm: while the "internal" initial estimate
*        from Fit1d may be in-accurate, in the first example a fit with a
*        gausshermite2 will be attempted regardless. By contrast, the
*        gaussian fit to a non-gaussian profile in example 2 may have been
*        so poor that is was rejected: in that case current gausshermite2 fit
*        will skip the profile because there won't be any initial estimates.
*    fit1d in=orion out=orion_gauh2 rms=0.22 userval='"^myvalues.def"' \
*          config='"^$SMURF_DIR/smurf_fit1d.def,function=gauss,ncomp=3"'
*        Fit three gaussian components to each profile using initial
*        estimates and fixed values as defined in the file "myvalues.def"
*        (template:" $SMURF_DIR/smurf_fit1d_uval'def"), e.g.:
*             comp1.b  = -5.2
*             fix1.b   = 1
*             comp1.c  = 6
*             comp2.b  = 20.4
*             fix2.b   = 1
*             comp2.c  = 4
*             comp3.b  = 35.3
*        That is: provide a user-defined fixed value for the position
*        (parameter "b") of components 1 and 2, and an initial estimate
*        for component 3. Also provide user-defined initial estimates for
*        the FWHM (parameter "c") of components 1 and 2. Leave it to fit1d
*        to find initial estimates for all other parameters.

*  Authors:
*     Remo Tilanus (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-09-29 (RPT):
*        Initial test version from smurf_remsky
*     2013-08-21 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008-2012 Science and Technology Facilities Council.
*     Copyright (C) 2013 University of British Columbia.
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
#include "star/one.h"
#include "one_err.h"

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

/* Use a normal smurf defaults file for the userval keymap which
** enables the normal error checking of supplied keys.
** If set to 0, no defaults file will be used when opening a
** user supplied file. */
#define USERVALDEFAULTFILE 1


static void get_fit1par( void *pfcntrl, int *status );

static void get_userval ( smfData *data, AstMapping **wcsmap,
			  const char *userfile, void *pfcntrl, int *status );

static void map_axis_to_wcs ( smfData *data, int axis, AstMapping **wcsmap,
			       int *status );

static void convert_range_to_pixels ( AstMapping **wcsmap, double *range,
				      int lbnd, int ubnd, int *prange,
				      int *status );

static void convert_coord_to_pixel ( AstMapping **wcsmap, const double *coord,
				     int nval, double *pixel, int *status );

static void convert_pixel_to_coord ( AstMapping **wcsmap, const double *pixel,
				     int nval, double *coord, int *status );

static void setup_parameter_ndfs ( smfData *data, int axis, int ncomp,
				   int *parndfs, smfArray *pardata,
				   int *status );

static void copy_parameter_ndfs ( smfArray *pardata, int *status );

static void convert_parunits( int mode, smfData *data, int axis,
			      int ncomp, AstMapping **wcsmap,
			      smfArray *pardata, int *status );

static void ndf2array ( smfData *sdata, int axis, int ipar, int write,
			double *values, double *errors, int *status );

/* Allowed parameter ids for non-polynomials and seq nr (plane) in
   parameter ndfs. See 'Fitted functions in smurf_fit1d.c */
const char* parkeys[] = { "A", "B", "C", "H3", "H4", "L", "V", "FID" };
int parloc[]          = {  1,   2,   3,   4,    5,    4,   5,   7 };

/*
**Main routine
*/

void smurf_fit1d( int * status )
{

  /* Local Variables */
  smfData *data = NULL;          /* Input data struct */

  size_t   in = 0;               /* File counter */
  int      indf;                 /* NDF identifier for input file */
  int      outndf;               /* Output NDF identifier */
  Grp     *igrp = NULL;          /* Input group */
  Grp     *ogrp = NULL;          /* Output group */
  size_t   outsize;              /* Nr of NDF names in the output group */
  size_t   size;                 /* Number of files in input group */
  int      idim;                 /* Index of pixel axis */

  int      lbnd[ NDF__MXDIM ];   /* Lower NDF pixel bounds */
  int      ubnd[ NDF__MXDIM ];   /* Upper NDF pixel bounds */
  fitStruct fcntrl;              /* Pointer to fit control struct */

  /* File with user defined values for the function's parameters */
  char f_userval[GRP__SZNAM];    /* name of the file with user defs */

  /* Parameter NDFs */
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

  /* Get the RMS */
  double   rms = 1;              /* RMS in data */
  parGet0d( "RMS", &rms, status );
  fcntrl.rms = rms;

  /* Get any user defined  */
  parGet0c ( "USERVAL", f_userval, GRP__SZNAM, status );
  if ( *status == PAR__NULL ) {
    errAnnul ( status );
    one_strlcpy ( f_userval, "", sizeof(f_userval), status );
  }

  /* Extract and read user supplied values using a keymap */
  get_fit1par( &fcntrl, status );

  /* Copy to local variables */
  int axis = fcntrl.axis;            /* Axis to fit along           */
  int ncomp = fcntrl.ncomp;          /* Number of components to fit */
  double *range = fcntrl.range;      /* Coordinate range            */

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
    smf_open_file( NULL, ogrp, in, "UPDATE",
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
    fcntrl.axis = axis;                  /* Update value in struct */

    /* Get data dimensions */
    int iaxis = axis - 1;
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
    int prange[2] = {VAL__BADI,VAL__BADI}; /* Range in pixel nrs 1..n */
    convert_range_to_pixels( &wcsmap, range, (int) lbnd[iaxis],
			     (int) ubnd[iaxis], prange, status );
    if ( *status != SAI__OK) {
      goto CLEANUP;
    }
    fcntrl.lolimit[1] = prange[0];      /* Prange is limit on centre value */
    fcntrl.hilimit[1] = prange[1];

    /* Each component is described by NPAR-1 parameters plus an id plane.
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

    /* Fitted parameters (centre, dispersion) are in coordinates. Convert
       values from paramter ndfs to pixels for the fit */
    convert_parunits( (int) 0, data, (int) axis, (int) ncomp,
		      &wcsmap, pardata, status );
    if ( *status != SAI__OK) {
      goto CLEANUP;
    }

    /* Fill the parameter files with user any supplied values */
    get_userval ( data, &wcsmap, f_userval, &fcntrl, status );
    if ( *status != SAI__OK) {
      goto CLEANUP;
    }

    /* Fill the rest of the fit control structure */
    msgOutiff(MSG__DEBUG, " ", "Populate %s control struct", status,
	      FUNC_NAME);

    /*
    ** Fit the profiles
    */
    msgOutiff(MSG__VERB, " ", "Fitting profiles", status);
    smf_fit_profile( data, pardata, &fcntrl, status );


    /* The fit returns results (centre, dispersion) in pixels. Convert
       those back to coordinates */
    msgOutiff(MSG__VERB, " ", "Converting fitted values", status);
    convert_parunits( (int) 1, data, (int) axis, (int) ncomp,
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
       for( int icomp = 0; icomp <= ncomp; icomp++ ) {
          ndfAnnul( parndfs + icomp, status );
       }
    }

    /* Free resources */
    msgOutiff(MSG__DEBUG, " ", "Free data file resources", status);
    smf_close_file( NULL, &data, status );
    parndfs = astFree( parndfs );
    pardata = astFree( pardata );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK && ogrp ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up: release the resources used by the grp routines  */
 CLEANUP:

  msgOutiff(MSG__DEBUG, " ", "Delete groups etc", status);
  if( igrp != NULL ) grpDelet( &igrp, status);
  if( ogrp != NULL ) grpDelet( &ogrp, status);

  msgOutiff(MSG__VERB, " ", "%s Finished", status, TASK_NAME);
  ndfEnd( status );
}


/* *** END OF MAIN *** */


static void get_fit1par( void *pfcntrl, int *status )
/*
** Routine to get parameters from the config file, if parameter is not
** NULL. Patterned after smf_get_cleanpar.c
*/
{
  fitStruct *fcntrl = (fitStruct *) pfcntrl; /* Pointer fit control struct */

  /* Initialize limits */
  for ( int i = 0; i < NPAR; i++ ) {
    fcntrl->lolimit[i] = VAL__BADD;
    fcntrl->hilimit[i] = VAL__BADD;
  }

  AstKeyMap *keymap=NULL;        /* Pointer to keymap of config settings */

  if (*status != SAI__OK) return;

  /* Read configuration settings into keymap using defaults and typo
     checking. */
  msgOutif( MSG__VERB, " ", "Reading parameters from config file.", status );
  keymap = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_fit1d.def",
		       NULL, 1, status );

  msgOutif( MSG__VERB, " ", "Extracting values from keymap", status );
  /* Obtain parameters from keymap when non-NULL pointers given */

  /* Get axis to fit along */
  int axis = 0;
  if ( astMapGet0I( keymap, "AXIS", &axis ) )  {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... AXIS=%d", status, axis );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter AXIS from config file",
	     status);
      goto CLEANUP1;
    }
  }
  fcntrl->axis = axis;

  /* Get range of pixels to fit over */
  double range[2] = {VAL__BADD,VAL__BADD};
  int  nval = 0;
  if ( astMapGet1D( keymap, "RANGE", 2, &nval, range ) ) {
    if ( *status == SAI__OK  && nval >  1) {
      msgOutiff( MSG__VERB, "", "... RANGE=[%f,%f]", status,
		 (float) range[0], (float) range[1] );
    }
    if ( nval < 2 || range[0] == range[1] ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "RANGE requires two differing values", status);
      goto CLEANUP1;
    }
  }
  fcntrl->range[0] = range[0];
  fcntrl->range[1] = range[1];

  /* Get clip of pixels to fit over */
  double clip[2] = {VAL__BADD,VAL__BADD};
  nval = 0;
  if ( astMapGet1D( keymap, "CLIP", 2, &nval, clip ) ) {
    if ( *status == SAI__OK  && nval >  1) {
      msgOutiff( MSG__VERB, "", "... CLIP=[%f,%f]", status,
		 (float) clip[0], (float) clip[1] );
    }
    if ( nval < 2 || clip[0] == clip[1] ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "CLIP requires two differing values", status);
      goto CLEANUP1;
    }
  }
  fcntrl->clip[0] = clip[0];
  fcntrl->clip[1] = clip[1];

  smf_math_function fid = 1;
  {
    const char * strpntr = NULL;
    if ( astMapGet0C( keymap, "FUNCTION", &strpntr ) ) {
      if (*status == SAI__OK && strpntr ) {
	fid = smf_mathfunc_fromstring( strpntr, status );
	msgOutiff( MSG__VERB, "", "... FUNCTION=%s (id: %d)", status,
		   smf_mathfunc_str( fid, status ), fid );
      } else {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Failed to get parameter FUNCTION from config file",
	       status);
	goto CLEANUP1;
      }
    }
  }
  fcntrl->fid = fid;

  one_strlcpy ( fcntrl->usort,"amp", sizeof(fcntrl->usort), status );
  {
    const char * strpntr = NULL;
    if ( astMapGet0C( keymap, "SORT", &strpntr ) ) {
      if (*status == SAI__OK && strpntr ) {
	msgOutiff( MSG__VERB, "", "... SORT=%s", status, strpntr);
	one_strlcpy ( fcntrl->usort, strpntr, sizeof(fcntrl->usort), status );
        if  ( strncasecmp( fcntrl->usort, "AMP", 3 ) != 0 &&
              strncasecmp( fcntrl->usort, "POS", 3 ) != 0 &&
              strncasecmp( fcntrl->usort, "DIS", 3 ) != 0 &&
              strncasecmp( fcntrl->usort, "WID", 3 ) != 0 ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unrecognized sort option:\nMust use 3-plus characters of 'amplitude', 'position', 'distance', or 'width'.",
	         status);
	  goto CLEANUP1;
	}
      } else {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Failed to get parameter SORT from config file",
	       status);
	goto CLEANUP1;
      }
    }
  }

  /* Get ncomp to fit along */
  int sort_estim = 0;
  if ( astMapGet0I( keymap, "SORT_ESTIMATE", &sort_estim ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... SORT_ESTIMATE=%d", status, sort_estim );
    } else {
      errRep(FUNC_NAME,
         "Failed to get parameter SORT_ESTIMATE from config file",
	     status);
      goto CLEANUP1;
    }
  }
  fcntrl->sort_estim = sort_estim;

  /* Get ncomp to fit along */
  int ncomp = 1;
  if ( astMapGet0I( keymap, "NCOMP", &ncomp ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... NCOMP=%d", status, ncomp );
      if ( ncomp > MAXCOMPS ) {
        *status = SAI__ERROR;
        msgSeti("C", (int) MAXCOMPS);
	errRep(FUNC_NAME, "Requested components exceed maximum of ^C",
	     status);
	goto CLEANUP1;
      }
    } else {
      errRep(FUNC_NAME, "Failed to get parameter NCOMP from config file",
	     status);
      goto CLEANUP1;
    }
  }
  fcntrl->ncomp = ncomp;

  /* Minimal acceptable amplitude for a component */
  double minamp = 0;
  if ( astMapGet0D( keymap, "MINAMP", &minamp ) ) {
    if ( *status == SAI__OK ) {
      if ( minamp < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "MINAMP must be >= 0.", status );
	goto CLEANUP1;
      } else {
	msgOutiff( MSG__VERB, "", "... MINAMP=%f", status, minamp );
      }
    } else {
      errRep(FUNC_NAME, "Failed to get parameter MINAMP from config file",
	     status);
      goto CLEANUP1;
    }
  }
  fcntrl->lolimit[0] = minamp * fcntrl->rms;

  /* Minimal acceptable dispersion for a component */
  double minwidth = 0.8;
  if ( astMapGet0D( keymap, "MINWIDTH", &minwidth ) ) {
    if ( *status == SAI__OK ) {
      if( minwidth < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "MINWIDTH must be >= 0.", status );
	goto CLEANUP1;
      } else {
	msgOutiff( MSG__VERB, "", "... MINWIDTH=%f", status,
		   minwidth );
      }
    } else {
      errRep(FUNC_NAME, "Failed to get parameter MINWIDTH from config file",
	     status);
      goto CLEANUP1;
    }
  }
  /* Convert to dispersion */
  if ( minwidth != VAL__BADD ) {
    minwidth = FWHM2DISP( minwidth );
  }
  fcntrl->lolimit[2] = minwidth;

  /* Maximal acceptable lorenztian FWHM for a component */
  double maxlorz = VAL__BADD;
  if ( astMapGet0D( keymap, "MAXLORZ", &maxlorz ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... MAXLORZ=%f", status,
		 maxlorz );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter MAXLORZ from config file",
	     status);
      goto CLEANUP1;
    }
  }
  /* Convert to dispersion */
  if ( maxlorz != VAL__BADD ) {
    maxlorz = FWHM2DISP( maxlorz );
  }
  fcntrl->hilimit[3] = maxlorz;

  /* Min, Max acceptable values for hermite parameters */
  double absh3min = VAL__BADD;
  double absh3max = VAL__BADD;
  double absh4min = VAL__BADD;
  double absh4max = VAL__BADD;
  if ( astMapGet0D( keymap, "ABSH3MIN", &absh3min ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... ABSH3MIN=%f", status,
		 absh3min );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter ABSH3MIN from config file",
	     status);
      goto CLEANUP1;
    }
  }
  if ( astMapGet0D( keymap, "ABSH3MAX", &absh3max ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... ABSH3MAX=%f", status,
		 absh3max );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter ABSH3MAX from config file",
	     status);
      goto CLEANUP1;
    }
  }
  if ( astMapGet0D( keymap, "ABSH4MIN", &absh4min ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... ABSH4MIN=%f", status,
		 absh4min );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter ABSH4MIN from config file",
	     status);
      goto CLEANUP1;
    }
  }
  if ( astMapGet0D( keymap, "ABSH4MAX", &absh4max ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... ABSH4MAX=%f", status,
		 absh4max );
    } else {
      errRep(FUNC_NAME, "Failed to get parameter ABSH4MAX from config file",
	     status);
      goto CLEANUP1;
    }
  }
  fcntrl->lolimit[3] = absh3min;
  fcntrl->hilimit[3] = absh3max;
  fcntrl->lolimit[4] = absh4min;
  fcntrl->hilimit[4] = absh4max;

  /* Retry bad fits with out-of-bounds parameter fixed? */
  int do_retry = YES;
  if ( astMapGet0I( keymap, "RETRY", &do_retry ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... RETRY=%d", status,
		 do_retry );
    } else {
      errRep(FUNC_NAME,
         "Failed to get parameter RETRY from config file", status);
      goto CLEANUP1;
    }
  }
  fcntrl->do_retry = do_retry;

  /* Restrict output profile to be positive only (negative value
     are replaced by zeroes */
  int pos_only = YES;
  if ( astMapGet0I( keymap, "POS_ONLY", &pos_only ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... POS_ONLY=%d", status,
		 pos_only );
    } else {
      errRep(FUNC_NAME,
         "Failed to get parameter POS_ONLY from config file",
	     status);
      goto CLEANUP1;
    }
  }
  fcntrl->pos_only = pos_only;

  /* Do initial estimates only */
  int estimate_only = NO;
  if ( astMapGet0I( keymap, "ESTIMATE_ONLY", &estimate_only ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... ESTIMATE_ONLY=%d", status,
		 estimate_only );
    } else {
      errRep(FUNC_NAME,
         "Failed to get parameter ESTIMATE_ONLY from config file",
	     status);
      goto CLEANUP1;
    }
  }
  fcntrl->estimate_only = estimate_only;

  /* Do input model only */
  int model_only = NO;
  if ( astMapGet0I( keymap, "MODEL_ONLY", &model_only ) ) {
    if ( *status == SAI__OK ) {
      msgOutiff( MSG__VERB, "", "... MODEL_ONLY=%d", status,
		 model_only );
    } else {
      errRep(FUNC_NAME,
         "Failed to get parameter MODEL_ONLY from config file",
	     status);
      goto CLEANUP1;
    }
  }
  fcntrl->model_only = model_only;

CLEANUP1:

  if( keymap ) keymap = astAnnul( keymap );

}

static void get_userval ( smfData *data, AstMapping **wcsmap,
			  const char *userfile, void *pfcntrl, int *status )

/*
** User supplied values. The routine opens a keymap file with parameters
** values either to use as initial estimates or to keep fixed in the fit.
** Parameters are indicated by a letter (see help above and smf_fit1d.h.
**
** Entries can have the form: comp#.par = value or fix#.par = [0.1]
**
*/
{
  fitStruct *fcntrl = (fitStruct *) pfcntrl; /* Pointer fit control struct */

  Grp    *grp = NULL;                  /* Group to hold config values    */
  double  fixval[MAXPAR];              /* Fixed/estimated values         */
  int     fixmask[MAXPAR];             /* Par fixed (1) or free in fit   */
  double  pixscale;                    /* Pixel-scale                    */

  AstKeyMap *uvalkmap=NULL;   /* Pointer to keymap of userval settings   */
  AstKeyMap *parkmap =NULL;            /*  Hash for par names used       */

  if (*status != SAI__OK) return;

  /* Initialize arrays */
  for ( int i = 0; i < MAXPAR; i++ ) {
    fixval[i] = VAL__BADD;
    fixmask[i] = 0;
    fcntrl->fixval[i] = VAL__BADD;
    fcntrl->fixmask[i] = 0;
  }

  /* Find pixel scale */
  {
    int    iaxis = fcntrl->axis - 1;      /* 0-based axis nr to fit along */
    double dval1[2], dval2[2];
    double centre = (int) (data->dims[iaxis]/2.0+0.51); /* Centrish pixel */
    dval1[0] = centre - 1;
    dval1[1] = centre + 1;
    astTran1( *wcsmap, 2, dval1, 1, dval2 );
    if (*status != SAI__OK) return;

    pixscale = fabs(0.5*(dval2[1]-dval2[0]));
  }

  if ( strlen(userfile) <= 0 ) {
    goto CLEANUP3;
  }

  /* Set up keymap with parameter name as key and corresponding parameter
  ** ndf "plane" as value, effective creating an associative or hash array
  */
  parkmap = astKeyMap( "KeyError=1,KeyCase=0" );
  int npar=sizeof(parkeys)/sizeof(parkeys[0]);
  for ( int ikey = 0; ikey < npar; ikey++ ) {
    int iplane;
    astMapPut0I( parkmap, parkeys[ikey], parloc[ikey], NULL );
    astMapGet0I( parkmap, parkeys[ikey], &iplane );
  }

  msgOutif( MSG__VERB, " ", "Reading user supplied parameters file.", status );

  /*
  ** Read in parameter file
  */
#if (USERVALDEFAULTFILE)

  uvalkmap = kpg1Config( "USERVAL", "$SMURF_DIR/smurf_fit1d_uval.def",
                         NULL, 1, status );

#else

  /* Set up the group */
  grp = grpNew( "GRP", status );
  grpGrpex( userfile, NULL, grp, &size, &added, &flag, status );

  /* Create a KeyMap from this group. */
  kpg1Kymap( grp, &uvalkmap, status );

  /* Delete the group. */
  grpDelet( &grp, status );

#endif

  /* Obtain parameters from keymap when non-NULL pointers given */
  int nkey = astMapSize(  uvalkmap );

  for( int ikey = 0; ikey < nkey; ikey++ ) {

    /* Read the base key from the map */
    const char *bkey = astMapKey( uvalkmap, ikey );

    /* These two are keymaps themselves */
    if ( strncmp( bkey, "COMP", 4 ) == 0 ||
         strncmp( bkey, "FIX", 3  ) == 0 ) {

      AstObject *obj = NULL;
      astMapGet0A( uvalkmap, bkey, &obj );
      AstKeyMap *compkmap = (AstKeyMap *) obj;

      /* Get the values */
      int nkey2 = astMapSize( compkmap );

      double  dval;                        /* For returned key value */
      int iplane;                          /* From par hash above    */

      for( int ikey2 = 0; ikey2 < nkey2; ikey2++ ) {

        /* Get parameter key  and corresponding ndf plane*/
	const char *pkey = astMapKey( compkmap, ikey2 );

        dval = VAL__BADD;
	astMapGet0D( compkmap, pkey, &dval );
	astMapGet0I( parkmap,  pkey, &iplane ); ;

	if ( *status != SAI__OK ) {
          goto CLEANUP3;
        }

	if ( strncmp( bkey, "COMP", 4 ) == 0 &&  dval != VAL__BADD ) {

          int icomp;
          sscanf(bkey+4, "%d", &icomp);
	  fixval[(icomp-1)*NPAR+iplane-1] = dval;

	} else if ( strncmp( bkey, "FIX", 3 ) == 0 &&  dval != VAL__BADD ) {

          int icomp;
          sscanf(bkey+3, "%d", &icomp);
          /* fixmask 1: fixed; 0: free hence fit */
          fixmask[(icomp-1)*NPAR+iplane-1] = 0;
          if ( dval != 0.0 ) {
	    fixmask[(icomp-1)*NPAR+iplane-1] = 1;
	  }

	}

      }

      if( compkmap ) compkmap = astAnnul( compkmap );

    }

  } /* End loop over keyfile entries */


  /* Convert coordinate-based values to pixel-based values */

  for ( int icomp = 0; icomp < MAXCOMPS; icomp++ ) {

    int offset = icomp*NPAR;

    for ( int i = 0; i < NPAR; i++ ) {

      double cval = fixval[offset+i];
      double pval = cval;

      if ( cval != VAL__BADD ) {
	if ( i == 1 ) {
	  convert_coord_to_pixel ( wcsmap, &cval, 1, &pval, status );
	} else if ( i == 2 ||
		    ( fixval[offset+NPAR-1] == SMF__MATH_VOIGT && i == 3 ) ||
		    ( fixval[offset+NPAR-1] == VAL__BADD &&
		      fcntrl->fid == SMF__MATH_VOIGT && i == 3 ) ) {
	  pval = FWHM2DISP(cval/pixscale);
	}
	fixval[offset+i] = pval;
      }

      char fixed[7] = "free";
      if ( fixmask[offset+i] == 1 )
	one_strlcpy ( fixed, "fixed", sizeof(fixed), status );

      if ( cval != VAL__BADD )
	msgOutiff( MSG__DEBUG, "",
		 "... COMP%d.par%d input %.4f: stored as %f (%s)\n",
		   status, icomp+1, i+1, (float) cval, (float) pval, fixed );
      else if ( fixmask[offset+i] == 1 )
	msgOutiff( MSG__DEBUG, "",
		 "... COMP%d.par%d: (%s)\n", status, icomp+1, i+1, fixed );

    }
  }


  /* Unfortunately need to check here whether any fixed centre position
     given is in range since the lsqfit routine otherwise throws an error
     -5. Remove those and warn the user */

  int removed = 0;
  for ( int icomp = 0; icomp < MAXCOMPS; icomp++ ) {

    int offset = icomp*NPAR;

    if ( fixval[offset+1] != VAL__BADD &&
	 ( fixval[offset+1] < fcntrl->lolimit[1] ||
	   fixval[offset+1] > fcntrl->hilimit[1] ) ) {
      /* Remove this component and shift all others forward */
      for ( int j = offset+NPAR; j < MAXPAR-3; j++ ){
	fixval[j-NPAR] = fixval[j];
	fixmask[j-NPAR] = fixmask[j];
      }
      for ( int j = MAXPAR-3-NPAR; j < MAXPAR-3; j++ ){
	fixval[j-NPAR] = VAL__BADD;
	fixmask[j-NPAR] = 0;
      }
      removed++;
      msgOutf( "",
       "***SKIPPING*** fixed component %d: centre outside fit-range in %s\n",
	       status, (int) icomp+removed, userfile );

      /* reset to current position again */
      icomp--;
    }
  }

  /* Copy local arrays to Struct */
  /*
  memcpy (fcntrl->fixval, fixval, MAXPAR*sizeof(double));
  memcpy (fcntrl->fixmask, fixmask, MAXPAR*sizeof(int));
  */
  for ( int i = 0; i < MAXPAR; i++ ) {
    fcntrl->fixval[i] = fixval[i];
    fcntrl->fixmask[i] = fixmask[i];
  }

CLEANUP3:
  if ( grp != NULL ) grpDelet( &grp, status );
  if ( parkmap ) parkmap = astAnnul( parkmap );
  if ( uvalkmap ) uvalkmap = astAnnul( uvalkmap );

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

    /* Allocate an array to receive the indices of the WCS axes that
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

  if ( prange[0] > prange[1] ) {
    int temp = prange[0];
    prange[0] = prange[1];
    prange[1] = temp;
  }

  if ( range[0] == VAL__BADD ) {
     msgOutiff(MSG__VERB, " ",
          "Range undefined => Full pixel range: %d to %d\n",
          status, prange[0], prange[1]);
  } else {
     msgOutiff(MSG__VERB, " ", "Range %f to %f => Pixel range: %d to %d\n",
	    status, range[0], range[1], prange[0], prange[1]);
  }

  if ( prange[0] >= (int) ndims || prange[1] <= 1 ) {
    msgOutf( "", "Range results in a pixel range %d to %d that is beyond the input ndf axis %d to %d",
	    status, prange[0]+lbnd-1, prange[1]+lbnd-1, lbnd, ubnd );
    *status = SAI__ERROR;
    errRep(TASK_NAME, "Invalid RANGE specified.", status);
  }
}


static void convert_pixel_to_coord ( AstMapping **wcsmap, const double *pixel,
				     int nval, double *coord, int *status )
/*
** Convert given pixels (running 1..n) to coordinate values along the
** fit axis.
*/
{
  int i;

  /* Change pixels to array indices: if no mapping exists interpret
     range as grid values */
  if ( wcsmap ) {
      astTran1( *wcsmap, nval, pixel, 1, coord );
  } else {
    msgOut("",
	   "*WARNING* No valid axis mapping: returned values are pixels",
	   status);
    for ( i = 0; i < nval; i++ ) {
      coord[i] = (double) pixel[i];
    }

  }
}


static void convert_coord_to_pixel ( AstMapping **wcsmap, const double *coord,
                                     int nval, double *pixel, int *status )
/*
** Convert given coordinate values to pixels (running 1..n) along the
** fit axis.
*/
{
  int i;

  /* Only if valid mapping exists */
  if ( wcsmap ) {
    astTran1( *wcsmap, nval, coord, 0, pixel );
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
  Grp      *pgrp = NULL;            /* Parameter ndfs group       */
  Grp      *cgrp = NULL;            /* Components ndfs group      */
  char     *pname = NULL;           /* Temporary pointer          */
  smfData  *pdata = NULL;           /* smfData for component      */
  smfData  *cdata = NULL;           /* Parameter file data struct */
  size_t    psize = 0;              /* Param ndfs group size      */
  size_t    csize = 0;              /* Diag ndfs group size       */
  int       idim;                   /* Index of pixel axis        */

  char      pardir[GRP__SZNAM+1]   = ""; /* PARDIR parameter      */
  char      parndf[GRP__SZNAM+1]   = ""; /* PARNDF parameter      */
  char      rootname[GRP__SZNAM+1] = ""; /* Dir and ndf name      */
  char      compname[GRP__SZNAM+1] = ""; /* Component name        */
  char      filename[GRP__SZNAM+1] = ""; /* Full Filename         */

  void     *parpntr, *comppntr;     /* Void pointers              */

  if (*status != SAI__OK) return;

  /* Directory Path  */
  parGet0c ( "PARDIR", pardir, GRP__SZNAM, status );
  if ( *status == PAR__NULL ) {
    errAnnul ( status );
    one_strlcpy ( pardir, "", sizeof(pardir), status );
  }

  /* Parameter NDF  */
  parGet0c ( "PARNDF", parndf, GRP__SZNAM, status );
  if ( *status == PAR__NULL ) {
    errAnnul ( status );
    one_strlcpy ( parndf, "", sizeof(parndf), status );
  }

  /* Start building up full file name */
  one_strlcpy ( rootname, "", sizeof(rootname), status );
  if ( strcmp(pardir, "") != 0 ) {
    one_strlcpy ( rootname, pardir, sizeof(rootname), status );
    one_strlcat ( rootname, "/", sizeof(rootname), status );
  }

  if ( strcmp(parndf, "") != 0 ) {
    one_strlcat ( rootname, parndf, sizeof(rootname), status );
    one_strlcat ( rootname, ".MORE.SMURF_FIT1D.", sizeof(rootname),
		  status );
  }

  if ( *status == ONE__TRUNC ) {
    *status = SAI__OK;
    msgOutf(" ", "Error: name truncated to '%s'\n", status, rootname );
    *status = ONE__TRUNC;
    errRep(TASK_NAME, "Pardir plus parndf too long ", status);
    goto CLEANUP2;
  }

  /* Got the root set up, now read the names for each component */

  /* Open new group for full component parameter file names. */
  pgrp = grpNew( "GRP", status );

  /* Ask for component parameter files */
  kpg1Rgndf( "PARCOMP",  0, 0, "", &cgrp, &csize, status );
  if ( *status == PAR__NULL ) {
    csize = 0;
    errAnnul(status);
  }

  if ( *status != SAI__OK )
    errRep(TASK_NAME, "Error reading PARNDF parameter", status);
  else {

    int rsize = csize;
    if ( rsize == 0 &&  strcmp(parndf, "") != 0 ) {
      /* Try read any and all parameter files from NDF */
      rsize = MAXCOMPS;
    }

    pname = compname;
    for ( int i = 1; i <= (int) rsize; i++ ) {
      if ( csize > 0 )
	grpGet( cgrp, i, 1, &pname, GRP__SZNAM, status );
      else
        sprintf( compname, "COMP_%d", i );

      one_strlcpy ( filename, rootname, sizeof(filename), status );
      one_strlcat ( filename, compname, sizeof(filename), status );
      if ( *status == ONE__TRUNC ) {
	*status = SAI__OK;
	msgOutf(" ", "Error: name truncated to '%s'\n", status, filename );
	*status = ONE__TRUNC;
	errRep(TASK_NAME, "Pardir + parndf + parcomp too long ", status);
	goto CLEANUP2;
      }

      grpPut1( pgrp, filename, 0, status );

    }
    psize += rsize;
  }

  /* No parameter files to read */
  if ( psize == 0 ) goto CLEANUP2;

  size_t ncomp = pardata->ndat-1;                            /* Skip COMP_0 */

  if ( ncomp < psize &&  ( csize != 0 ||  strcmp(parndf, "") == 0 ) ) {
    msgOutf( " ",
    "WARNING: Nr par. files (%d) exceeds number of components being fitted (%d).\n         Excess parameter files will be skipped.",
	     status, (int) psize, (int) ncomp );
    psize = ncomp;
  } else if ( psize < ncomp ) {
    msgOutf(" ", "WARNING: Nr par. files (%d) less than components being fitted (%d),\n         remaining components will be estimated.",
	    status, (int) psize, (int) ncomp );
  }

  for ( int in = 1, icomp = 1; in <= (int) psize && icomp <= (int) ncomp;
	in++, icomp++ ) {

    pname = filename;
    grpGet( pgrp, in, 1, &pname, GRP__SZNAM, status );

    if ( strcmp(filename, "") != 0 ) {

      /* Number of points in input data parameter files */
      pdata = pardata->sdata[icomp];
      int dpts = 1;
      for ( idim = 0; idim < (int) pdata->ndims; idim++ ) {
	dpts *= pdata->dims[idim];
      }

      /* Open and copy the component NDF data. */
      smf_open_file( NULL, pgrp, in, "READ",
		 SMF__NOCREATE_DA | SMF__NOTTSERIES | SMF__NOCREATE_QUALITY,
		 &cdata, status );

      /* Ran out of components in container file */
      if ( *status != SAI__OK && strcmp(parndf, "") != 0 && csize == 0 ) {
	*status = SAI__OK;           /* Done */
	msgOutf( " ",
		 "Found (%d) component parameters in container file.",
		 status, icomp-1 );
        if ( (icomp-1) < (int) ncomp ) {
	  msgOutf(" ", "WARNING: remaining %d components will be estimated.",
		  status, (int) (ncomp-icomp+1) );
	}
	goto CLEANUP2;
      }

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
	smf_close_file( NULL, &cdata, status );
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


static void convert_parunits( int mode, smfData *data, int axis, int ncomp,
			      AstMapping **wcsmap, smfArray *pardata,
			      int *status )
/*
** Input parameters and parameters ndfs are in physical units whereas
** the program uses pixels. This routine converts the pixel-based
** parameters to coordinate based ones or the reverse:
** e.g. pixels (1..n) along the third axis to velocities.
**   mode =  1: convert pixel-based to coordinate-based values
**        =  0: convert coordinate-based to pixel-based values
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
  double  *inpval, *inperr;      /* Array for pixel values and errors*/
  double  *fidval, *fiderr;      /* Array for function id values and errors*/
  double  *outval, *outerr;      /* Array for coordinate values and errors */

  double   dval1[2], dval2[2];   /* Doubles... */
  double   centre, pixscale;     /* Pixelscale */


  if (*status != SAI__OK) return;

  /* No mapping exists between pixel and coordinate */
  if ( !wcsmap ) {
    msgOut(" ", "(***WARNING*** Results are in pixels!!!", status );
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
  inpval = astMalloc( (nprofiles)*sizeof( *inpval ) );
  inperr = astMalloc( (nprofiles)*sizeof( *inperr ) );
  fidval = astMalloc( (nprofiles)*sizeof( *fidval ) );
  fiderr = astMalloc( (nprofiles)*sizeof( *fiderr ) );
  outval = astMalloc( (nprofiles)*sizeof( *outval ) );
  outerr = astMalloc( (nprofiles)*sizeof( *outerr ) );

  /* Cycle true the extension cubes with the fitted values.
  ** dstride gives the number of elements that seperate adjacent
  ** points along AXIS. Logically this splits the hypercube into
  ** nprofiles/dstride subcubes each with dstride profiles. Thus we can
  ** cycle through the profiles by cycling over the subcubes and each
  ** profile in the subcube.
  */

  /* skip COMP_0 diagnostics extension */

  for ( icomp = 1; icomp <= ncomp; icomp++ ) {


    /* Read function id plane values */
    ndf2array( pardata->sdata[icomp], axis, (int) NPAR-1, 0, fidval, fiderr,
	       status );

    /* Loop over planes in each component: 1=Centre, 2=Dispersions, etc.
       skip 0=Amp and final plane with FID */

    for ( ipar = 1; ipar < NPAR-1; ipar++ ) {

      /* Call ndf2array twice: once to read the values
	 then to write the converted values back */

      /* Read fitted values */
      ndf2array( pardata->sdata[icomp], axis, ipar, 0, inpval, inperr,
		 status );

      /* All fitted "centre-like" values need to be converted to coords */
      if (ipar == 1) {

	astTran1( *wcsmap, (int) nprofiles, inpval, mode, outval );
	if (*status != SAI__OK) return;

	/* Errors scale with pixscale */
	for ( iprof = 0; iprof < nprofiles; iprof++ ) {
	  if ( inperr[iprof] != VAL__BADD) {
            if ( mode == 1)
	      outerr[iprof] = inperr[iprof]*pixscale;
            else
	      outerr[iprof] = inperr[iprof]/pixscale;
	  } else {
	    outerr[iprof] = VAL__BADD;
	  }
	}

      } else {

	for ( iprof = 0; iprof < nprofiles; iprof++ ) {

	  /* "width-like" parameters scale with pixscale */
	  if ( ipar == 2 ||
	       ( fidval[iprof] == SMF__MATH_VOIGT && ipar == 3 ) ) {
	    outval[iprof] = VAL__BADD;
	    outerr[iprof] = VAL__BADD;
	    if ( inpval[iprof] != VAL__BADD) {
	      if ( mode == 1)
		outval[iprof] = DISP2FWHM(inpval[iprof]*pixscale);
	      else
		outval[iprof] = FWHM2DISP(inpval[iprof]/pixscale);
	    }
	    if ( inperr[iprof] != VAL__BADD) {
	      if ( mode == 1)
		outerr[iprof] = DISP2FWHM(inperr[iprof]*pixscale);
	      else
		outerr[iprof] = FWHM2DISP(inperr[iprof]/pixscale);
	    }

	  /* Leave the rest alone */
	  } else {
	    outval[iprof] = inpval[iprof];
	    outerr[iprof] = inperr[iprof];
	  }

	}

      }

      /* Write converted values back */
      ndf2array( pardata->sdata[icomp], axis, ipar, 1, outval, outerr,
		 status );

    } /* end loop over parameters */

  } /* end loop over components */

  inpval = astFree( inpval );  inperr = astFree( inperr );
  fidval = astFree( fidval );  fiderr = astFree( fiderr );
  outval = astFree( outval );  outerr = astFree( outerr );

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
