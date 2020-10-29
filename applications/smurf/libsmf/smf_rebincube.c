
/*
*+
*  Name:
*     smf_rebincube

*  Purpose:
*     Paste a supplied 3D array into an existing output cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebincube( ThrWorkForce *wf, smfData *data, int first, int last,
*                    int *ptime, int badmask, int is2d,
*                    AstSkyFrame *abskyfrm, AstMapping *oskymap,
*                    AstFrame *ospecfrm, AstMapping *ospecmap,
*                    Grp **detgrp, int moving, int usewgt, int spread,
*                    const double params[], int lbnd_out[ 3 ],
*                    int ubnd_out[ 3 ], int genvar, float *data_array,
*                    float *var_array, double *wgt_array, float *texp_array,
*                    float *teff_array, double *fcon, int64_t *nused,
*                    int *nreject, int *naccept, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads that will do the re-binning.
*     data = smfData * (Given)
*        Pointer to the input smfData structure.
*     first = int (Given)
*        Is this the first call to this routine for the current output
*        cube?
*     last = int (Given)
*        Is this the last call to this routine for the current output
*        cube?
*     ptime = int * (Given)
*        Pointer to an array of integers, each one being the index of a
*        time slice that is to be pasted into the output cube. If this is
*        NULL, then all time slices are used. The values in the array
*        should be monotonic increasing and should be terminated by a value
*        of VAL__MAXI.
*     badmask = int (Given)
*        Indicates how the bad pixel mask for each output spectrum is
*        determined. A value of zero causes the bad pixel mask for each
*        output spectrum to be identical to the bad pixel mask of the
*        first input spectrum that contributes to the output spectrum.
*        Any subsequent input spectra that contribute to the same output
*        spectrum but have a different bad pixel mask are ignored. A
*        "badmask" value of 1 causes the bad pixel mask for each output
*        spectrum to be the union of the bad pixel masks of all input
*        spectra that contribute to the output spectrum. That is, an
*        output pixel will be bad if any of the input pixels that
*        contribute to it are bad.
*     is2d = int (Given)
*        Should a 2D weights array be used? If so, the weight and
*        variance within a single output spectrum is assumed to be the
*        same for all spectral channels, and so 2D instead of 3D arrays
*        can be used, saving lots of memory. A 2D array has a single
*        element for each spectrum in the output NDF. A 3D array has an
*        element for every element in the output NDF.
*     abskyfrm = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe
*        the spatial axes of the output cube. This should represent
*        absolute sky coordinates rather than offsets even if "moving" is
*        non-zero.
*     oskymap = AstFrameSet * (Given)
*        A Mapping from 2D sky coordinates in the output cube to 2D
*        spatial grid coordinates in the output cube.
*     ospecfrm = AstFrame * (Given)
*        Pointer to the SpecFrame within the current Frame of the output WCS
*        Frameset.
*     ospecmap = AstMapping * (Given)
*        Pointer to the Mapping from the SpecFrame to the third GRID axis
*        within the current Frame of the output WCS Frameset.
*     detgrp = Grp ** (Given)
*        On entry, a Group containing the names of the detectors to be
*        used. All detectors will be used if this group is empty (or NULL).
*        On exit, the supplied group (if any) is deleted, and a new group
*        is created and return holding the names of the detectors that
*        contributed good data to the output cube.
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If
*        so, each time slice is shifted so that the position specified by
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        output cube.
*     usewgt = int (Given)
*        A flag indicating if the input data should be weighted according
*        to the input variances determined from the input Tsys values.
*     spread = int (Given)
*        Specifies the scheme to be used for dividing each input data value
*        up amongst the corresponding output pixels. See docs for astRebinSeq
*        (SUN/211) for the allowed values.
*     params = const double[] (Given)
*        An optional pointer to an array of double which should contain any
*        additional parameter values required by the pixel spreading scheme.
*        See docs for astRebinSeq (SUN/211) for further information. If no
*        additional parameters are required, this array is not used and a
*        NULL pointer may be given.
*     lbnd_out = dim_t [ 3 ] (Given)
*        The lower pixel index bounds of the output cube.
*     ubnd_out = dim_t [ 3 ] (Given)
*        The upper pixel index bounds of the output cube.
*     genvar = int (Given)
*        Indicates how the output variances should be calculated:
*           0 = do not calculate any output variances
*           1 = use spread of input data values
*           2 = use system noise temperatures
*     data_array = float * (Given and Returned)
*        The 3D data array for the output cube. This is updated on exit to
*        include the data from the supplied input NDF.
*     var_array = float * (Given and Returned)
*        An array in which to store the variances for the output cube if
*        "genvar" is not zero (the supplied pointer is ignored if "genvar" is
*        zero). The supplied array is update on exit to include the data from
*        the supplied input NDF. If "is2d" is non-zero, then this array
*        should be big enough to hold a single spatial plane from the output
*        cube (all planes will then have the same variance and so only one
*        plane need be calculated). If "is2d" is zero, the "var_array"
*        array should be the same shape and size as the output data array.
*     wgt_array = double * (Given and Returned)
*        An array in which to store the relative weighting for each pixel in
*        the output cube. The supplied array is update on exit to include the
*        data from the supplied input NDF. If "genvar" is 2, this array should
*        be the same size as "var_array". If "genvar" is 1, this array should
*        be twice the size of the "var_array" array.
*     texp_array = float * (Given and Returned)
*        A work array, which holds the total exposure time for each output
*        spectrum. It is updated on exit to include the supplied input NDF.
*        It should be big enough to hold a single spatial plane from the
*        output cube.
*     teff_array = float * (Given and Returned)
*        A work array, which holds the effective integration time for each
*        output spectrum, scaled by a factor of 4. It is updated on exit to
*        include the supplied input NDF. It should be big enough to hold a
*        single spatial plane from the output cube.
*     fcon = double * (Given and Returned)
*        If "first" is supplied non-zero, then *fcon is returned holding
*        the ratio of the squared backend degradation factor to the spectral
*        channel width (this is the factor needed for calculating the
*        variances from the Tsys value). This returned value should be
*        left unchanged on subsequent invocations of this function. If
*        "first" is zero on entry, the reurned value is VAL__BADD if the
*        factor for the current file has a different value.
*     nused = int64_t * (Given and Returned)
*        Use to accumulate the total number of input data samples that
*        have been pasted into the output cube.
*     nreject = int * (Given and Returned)
*        The number of input spectra that have been ignored becuase they
*        either do not cover the full spectral range of the output or
*        because they have a different bad pixel mask to the output.
*        Only used if "badmask==0" (it is left unchanged otherwise).
*     naccept = int * (Given and Returned)
*        The number of input spectra that have not been ignored. Only used
*        if "badmask==0" (it is left unchanged otherwise).
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The data array of the supplied input NDF is added into the existing
*     contents of the output data array, and the variance and weights
*     arrays are updated correspondingly.
*
*     Note, few checks are performed on the validity of the input data
*     files in this function, since they have already been checked within
*     smf_cubebounds.

*  Authors:
*     David S Berry (JAC, UClan)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-APR-2006 (DSB):
*        Initial version.
*     2-MAY-2007 (DSB):
*        Add parameter naccept.
*     25-MAY-2007 (TIMJ):
*        Allow fcon to drift a little
*     11-OCT-2007 (DSB):
*        Added parameter "ptime".
*     12-FEB-2008 (DSB):
*        Replaced arguments "index" and "size" with "first" and "last".
*        This is because different output tiles and/or polarisation bins
*        will receive contributions from different input files, not
*        necessarily starting at the first input file or ending at the
*        last.
*     26-SEP-2008 (DSB):
*        Increased tolerance for variations in Tsys conversion factor
*        from 0.001% to 1.0%.
*     14-MAY-2014 (DSB):
*        Report the number of samples pasted into the output cube.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>
#include <stdint.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/ndg.h"
#include "star/atl.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebincube"

void  smf_rebincube( ThrWorkForce *wf, smfData *data, int first, int last,
                     int *ptime, int badmask, int is2d,
                     AstSkyFrame *abskyfrm, AstMapping *oskymap,
                     AstFrame *ospecfrm, AstMapping *ospecmap,
                     Grp **detgrp, int moving, int usewgt, int lbnd_out[ 3 ],
                     int ubnd_out[ 3 ], int spread,
                     const double params[], int genvar, float *data_array,
                     float *var_array, double *wgt_array, float *texp_array,
                     float *teff_array, double *fcon, int64_t *nused,
                     int *nreject, int *naccept, int *status ){

/* Local Variables */
   AstCmpMap *ssmap = NULL;    /* Input GRID->output GRID Mapping for spectral axis */
   AstFrame *specframe = NULL; /* SpecFrame in input WCS */
   AstFrameSet *fs = NULL;     /* WCS FramesSet from input */
   AstMapping *specmap = NULL; /* GRID->Spectral Mapping for current input file */
   dim_t dim[ 3 ];             /* Output array dimensions */
   dim_t iv;                   /* Vector index into output 3D array */
   dim_t nchan;                /* Number of input spectral channels */
   dim_t ndet;                 /* No of detectors in the input */
   dim_t nel;                  /* No. of pixels in output */
   dim_t nout;                 /* Total number of elements in output cube */
   dim_t nslice;               /* No of time slices in the input */
   dim_t nxy;                  /* No of elements in an output spatial plane */
   double fcon2;               /* Variance factor for file */
   double gin[2];              /* Input grid coords */
   double gout[2];             /* Output grid coords */
   double tfac;                /* Factor describing spectral overlap */
   int good_tsys;              /* Flag indicating some good Tsys values found */
   int64_t nused_orig;         /* Supplied value of *nused */
   smfHead *hdr = NULL;        /* Pointer to data header for this time slice */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Begin an AST context.*/
   astBegin;

/* Store a pointer to the input NDFs smfHead structure. */
   hdr = data->hdr;

/* Store the dimensions of the output array. */
   dim[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1;
   dim[ 1 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1;
   dim[ 2 ] = ubnd_out[ 2 ] - lbnd_out[ 2 ] + 1;

/* Note the size of a spatial plane in the output cube. */
   nxy = dim[ 0 ]*dim[ 1 ];

/* Store the total number of elements in the output cube. */
   nout = nxy*dim[ 2 ];

/* Note the number of channels, detectors and time slices in the input data */
   nchan = (data->dims)[ 0 ];
   ndet = (data->dims)[ 1 ];
   nslice = (data->dims)[ 2 ];

/* Store the total number of input elements. */
   nel = nchan*ndet*nslice;

/* Get the conversion factor for converting input Tsys values into input
   variance values. */
   fcon2 = smf_calc_fcon( data, nchan, ( usewgt || genvar == 2 ), &specmap,
                          &specframe, status );

/* Return the factor needed for calculating Tsys from the variance. */
   if( *fcon == -1.0 ) {
      *fcon = fcon2;

   } else if( fcon2 == VAL__BADD ) {
      *fcon = VAL__BADD;

   } else if( fcon2 != *fcon && *fcon != VAL__BADD) {

/* fcon can be different by fraction of a percent and still be accurate enough
   for our purposes */
      double percent = 100.0 * fabs(*fcon - fcon2 ) / *fcon;
      if ( percent > 1.0 ) {
        msgSetd( "ORI", *fcon );
        msgSetd( "NEW", fcon2 );
        smf_smfFile_msg( data->file, "FILE", 1, "<unknown file>" );
        msgSetd( "PC", percent );
        msgOutif( MSG__NORM," ", "WARNING: Tsys conversion factor has "
                  "changed from ^ORI to ^NEW (^PC %) in file ^FILE",
                  status );
        *fcon = VAL__BADD;
      }
   }

/* Get a Mapping that converts values in the input spectral system to the
   corresponding values in the output spectral system. */
   fs = astConvert( specframe, ospecfrm, "" );

/* Concatenate these Mappings with the supplied spectral Mapping to get
   a Mapping from the input spectral grid axis (pixel axis 1) to the
   output spectral grid axis (pixel axis 3). */
   ssmap = astCmpMap( astCmpMap( specmap, astGetMapping( fs, AST__BASE,
                                                         AST__CURRENT ),
                                 1, " " ),
                      ospecmap, 1, " " );

/* In principle, the spectral coverage of the current input file and the
   output file may not be equal. The effect of this input file on the
   returned exposure time arrays should depend on how much spectral overlap
   there is between the two. So find what fraction of the output spectral
   range is contributed to by the spectral range of the input. */
   gin[ 0 ] = 0.5;
   gin[ 1 ] = nchan + 0.5;
   astTran1( ssmap, 2, gin, 1, gout );
   if( gout[ 0 ] < 0.5 ) gout[ 0 ] = 0.5;
   if( gout[ 1 ] > (double) dim[ 2 ] + 0.5 ) gout[ 1 ] = (double) dim[ 2 ] + 0.5;
   tfac = ( fabs( gout[ 1 ] - gout[ 0 ] ) + 1.0  )/( (double) dim[ 2 ] + 1 );

/* Indicate we have not yet found any good Tsys values in the input NDF. */
   good_tsys = 0;

/* Record the original number of input samples that have been pasted into
   the output cube. */
   nused_orig = *nused;

/* If we are using nearest neighbour rebinning, we can use specialist
   code that is faster than AST. */
   if( spread == AST__NEAREST ) {
      smf_rebincube_nn( wf, data, first, last, ptime, nchan, ndet, nslice,
                        nxy, nout, dim, badmask, is2d, (AstMapping *) ssmap,
                        abskyfrm, oskymap, detgrp, moving, usewgt, genvar,
                        tfac, fcon2, data_array, var_array, wgt_array,
                        texp_array, teff_array, nused, nreject, naccept,
                        &good_tsys, status );

/* For all other spreading schemes, we use AST. */
   } else {
      smf_rebincube_ast( wf, data, first, last, ptime, nchan, ndet, nslice,
                         nel, nxy, nout, dim, (AstMapping *) ssmap, abskyfrm,
                         oskymap, detgrp, moving, usewgt, spread, params,
                         genvar, tfac, fcon2, data_array, var_array, wgt_array,
                         texp_array, teff_array, &good_tsys, nused, status );
   }

/* If this is the final pass through this function, convert zero texp_array
   and texp_array values to bad values. */
   if( last ) {
      for( iv = 0; iv < nxy; iv++ ) {
         if( texp_array[ iv ] <= 0.0 ) {
            texp_array[ iv ] = VAL__BADR;
            teff_array[ iv ] = VAL__BADR;
         }
      }
   }

/* Issue a warning if Tsys values were being used to create output
   variances or weight input data values but no good Tsys values were
   found in the input NDF. */
   if( ( usewgt || genvar == 2 ) && !good_tsys ) {
      msgBlank( status );
      smf_smfFile_msg( data->file, "FILE", 1, "<unknown file>" );
      msgOutif( MSG__NORM, " ", "WARNING: ^FILE contains no Tsys values "
                "and will be ignored.", status );
   }

/* Report the number of samples from the input file that were pasted into
   the output cube. */
   smf_smfFile_msg( data->file, "FILE", 1, "<unknown file>" );
   if( nused_orig < *nused ){
      msgSetk( "N", *nused - nused_orig );
      msgOutif( MSG__VERB, " ", " ^N good samples from ^FILE were included in "
                "the output cube.", status );
   } else {
      msgOutif( MSG__NORM, " ", "WARNING: No good samples from ^FILE were "
                "included in the output cube.", status );
   }

/* End the AST context. This will annul all the AST objects created
   within the context. */
   astEnd;
}
