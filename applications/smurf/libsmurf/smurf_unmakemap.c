/*
*+
*  Name:
*     UNMAKEMAP

*  Purpose:
*     Produce simulated time series data from a SCUBA-2 map.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_unmakemap( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine creates one or more simulated SCUBA-2 time series cubes,
*     from a supplied 2D image of the sky. Thus, it performs a sort of
*     inverse to the MAKEMAP application.
*
*     The output time series bolometer samples are created by interpolating the
*     supplied input sky image at the position of the reference time series
*     sample centre. Various interpolation methods can be used (see parameter
*     INTERP). Gaussian noise may also be added (see parameter SIGMA).
*
*     The output time series cubes inherit all meta-data from the
*     corresponding input reference time series. The only thing modified
*     is the values in the NDF "Data" array.

*  ADAM Parameters:
*     IN = NDF (Read)
*          The input 2D image of the sky.
*     INTERP = LITERAL (Read)
*          The method to use when resampling the input sky image pixel values.
*          For details of these schemes, see the descriptions of routines
*          AST_RESAMPLEx in SUN/210. INTERP can take the following values:
*
*          - "Linear" -- The output sample values are calculated by bi-linear
*          interpolation among the four nearest pixels values in the input
*          sky cube.  Produces smoother output NDFs than the nearest-neighbour
*          scheme, but is marginally slower.
*
*          - "Nearest" -- The output sample values are assigned the value of
*          the single nearest input pixel. A very fast method.
*
*          - "Sinc" -- Uses the sinc(pi*x) kernel, where x is the pixel
*          offset from the interpolation point and sinc(z)=sin(z)/z.  Use
*          of this scheme is not recommended.
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
*          offset from the interpolation point and somb(z)=2*J1(z)/z (J1 is
*          the first-order Bessel function of the first kind).  This scheme
*          is similar to the "Sinc" scheme.
*
*          - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*          scheme is similar to the "SincCos" scheme.
*
*          [current value]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          A group of output NDFs into which the simulated time series data
*          will be written. These will hold _DOUBLE data vlues.
*     PARAMS( 2 ) = _DOUBLE (Read)
*          An optional array which consists of additional parameters
*          required by the Sinc, SincSinc, SincCos, SincGauss, Somb and
*          SombCos interpolation schemes (see parameter INTERP).
*
*          PARAMS( 1 ) is required by all the above schemes. It is used to
*          specify how many pixels are to contribute to the interpolated
*          result on either side of the interpolation point in each dimension.
*          Typically, a value of 2 is appropriate and the minimum allowed
*          value is 1 (i.e. one pixel on each side). A value of zero or fewer
*          indicates that a suitable number of pixels should be calculated
*          automatically. [0]
*
*          PARAMS( 2 ) is required only by the SombCos, SincSinc,
*          SincCos, and SincGauss schemes.  For the SombCos, SincSinc, and
*          SincCos schemes, it specifies the number of pixels at which the
*          envelope of the function goes to zero.  The minimum value is
*          1.0, and the run-time default value is 2.0.  For the SincGauss
*          scheme, it specifies the full-width at half-maximum (FWHM) of
*          the Gaussian envelope.  The minimum value is 0.1, and the
*          run-time default is 1.0.  Good results are often obtained by
*          approximately matching the FWHM of the envelope function, given
*          by PARAMS(2), to the point-spread function of the input data. []
*     REF = NDF (Read)
*          A group of existing time series data cubes. These act as templates
*          for the new time series cubes created by this application, and
*          specified via parameter OUT. They should contain _DOUBLE (i.e.
*          flat-fielded) data values.
*     SIGMA = _DOUBLE (Read)
*          The standard deviation of the Gaussian noise to add to the
*          output data. [0.0]
*     USEAXIS = LITERAL (Read)
*          A set of 2 axes to be selected from the Current Frame in the sky
*          map. Each axis can be specified either by giving its index within
*          the Current Frame in the range 1 to the number of axes in the Frame,
*          or by giving its symbol. This parameter is only accessed if the
*          Current Frame in the supplied NDF has more than 2 axes. The dynamic
*          default selects the axes with the same indices as the significant
*          NDF axes.

*  Related Applications:
*     SMURF: MAKEMAP

*  Authors:
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     8-JUN-2011 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2011 Science and Technology Facilities Council.
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
#include "star/atl.h"
#include "star/kaplibs.h"


/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#define FUNC_NAME "smurf_unmakemap"
#define TASK_NAME "UNMAKEMAP"
#define LEN__METHOD 20

void smurf_unmakemap( int *status ) {

/* Local Variables */
   AstFrameSet *wcsin = NULL; /* WCS Frameset for input cube */
   AstMapping *skymap;        /* GRID->SkyFrame Mapping from input WCS */
   AstSkyFrame *abskyfrm;     /* Input SkyFrame (always absolute) */
   AstSkyFrame *skyfrm = NULL;/* SkyFrame from the input WCS Frameset */
   Grp *igrp1 = NULL;         /* Group of input sky cube files */
   Grp *igrp2 = NULL;         /* Group of input template files */
   Grp *ogrp = NULL;          /* Group containing output file */
   char pabuf[ 10 ];          /* Text buffer for parameter value */
   double *in_data = NULL;    /* Pointer to the input cube data array */
   double *out_data = NULL;   /* Pointer to the output cube data array */
   double *pd;                /* Pointer to next element */
   double params[ 4 ];        /* astResample parameters */
   double sigma;              /* Standard deviation of noise to add to output */
   int blank;                 /* Was a blank line just output? */
   int flag;                  /* Was the group expression flagged? */
   int iel;                   /* INdex of next element */
   int ifile;                 /* Input file index */
   int indf;                  /* Input sky map NDF identifier */
   int interp = 0;            /* Pixel interpolation method */
   int moving;                /* Is the telescope base position changing? */
   int nel;                   /* Number of elements in 3D array */
   int ngood;                 /* No. of good values in putput cube */
   int nparam = 0;            /* No. of parameters required for interpolation scheme */
   int sdim[ 2 ];             /* Array of significant pixel axes */
   int slbnd[ 2 ];            /* Array of lower bounds of input map */
   int subnd[ 2 ];            /* Array of upper bounds of input map */
   size_t nskymap;            /* Number of supplied sky cubes */
   size_t outsize;            /* Number of files in output group */
   size_t size;               /* Number of files in input group */
   smfData *data = NULL;      /* Pointer to reference data struct */
   smfData *odata = NULL;     /* Pointer to output data struct */
   ThrWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* We have not yet displayed a blank line on stdout. */
   blank = 0;

/* Begin an AST context */
   astBegin;

/* Begin an NDF context. */
   ndfBegin();

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( smf_get_nthread( status ), status );

/* Get an identifier for the input NDF. We use NDG (via kpg1Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "IN", 1, 1, "", &igrp1, &nskymap, status );
   ndgNdfas( igrp1, 1, "READ", &indf, status );

/* Map the data array in the input sky map. */
   ndfMap( indf, "DATA", "_DOUBLE", "READ", (void **) &in_data, &nel,
           status );

/* Get the WCS FrameSet from the sky map, together with its pixel index
   bounds. */
   kpg1Asget( indf, 2, 0, 1, 1, sdim, slbnd, subnd, &wcsin, status );

/* Check the current Frame is a SKY frame. */
   skyfrm = astGetFrame( wcsin, AST__CURRENT );
   if( !astIsASkyFrame( skyfrm ) && *status == SAI__OK ) {
      ndfMsg( "N", indf );
      *status = SAI__ERROR;
      errRep( " ", " Current Frame in ^N is not a SKY Frame.", status );
   }

/* Get a copy of the current frame that represents absolute coords rather
   than offsets. We assume the target is moving if the map represents
   offsets. */
   moving = ( *status == SAI__OK &&
              !strcmp( astGetC( skyfrm, "SkyRefIs" ), "Origin" ) ) ? 1 : 0;
   abskyfrm = astCopy( skyfrm );
   astClear( abskyfrm, "SkyRefIs" );

/* Get the Mapping from the Sky Frame to grid axis in the iput map. */
   skymap = astGetMapping( wcsin, AST__CURRENT, AST__BASE );

/* Get the pixel interpolation scheme to use. */
   parChoic( "INTERP", "NEAREST", "NEAREST,LINEAR,SINC,"
             "SINCSINC,SINCCOS,SINCGAUSS,SOMB,SOMBCOS",
             1, pabuf, 10, status );

   if( !strcmp( pabuf, "NEAREST" ) ) {
      interp = AST__NEAREST;
      nparam = 0;

   } else if( !strcmp( pabuf, "LINEAR" ) ) {
      interp = AST__LINEAR;
      nparam = 0;

   } else if( !strcmp( pabuf, "SINC" ) ) {
      interp = AST__SINC;
      nparam = 1;

   } else if( !strcmp( pabuf, "SINCSINC" ) ) {
      interp = AST__SINCSINC;
      nparam = 2;

   } else if( !strcmp( pabuf, "SINCCOS" ) ) {
      interp = AST__SINCCOS;
      nparam = 2;

   } else if( !strcmp( pabuf, "SINCGAUSS" ) ) {
      interp = AST__SINCGAUSS;
      nparam = 2;

   } else if( !strcmp( pabuf, "SOMB" ) ) {
      interp = AST__SOMB;
      nparam = 1;

   } else if( !strcmp( pabuf, "SOMBCOS" ) ) {
      interp = AST__SOMBCOS;
      nparam = 2;

   } else if( *status == SAI__OK ) {
      nparam = 0;
      *status = SAI__ERROR;
      msgSetc( "V", pabuf );
      errRep( "", "Support not available for INTERP = ^V (programming "
              "error)", status );
   }

/* Get an additional parameter vector if required. */
   if( nparam > 0 ) parExacd( "PARAMS", nparam, params, status );

/* Get a group of reference time series files to use as templates for
   the output time series files.*/
   ndgAssoc( "REF", 1, &igrp2, &size, &flag, status );

/* Get output file(s) */
   kpg1Wgndf( "OUT", igrp2, size, size, "More output files required...",
              &ogrp, &outsize, status );

/* Get he noise level to add to the output data. */
   parGet0d( "SIGMA", &sigma, status );

/* Loop round all the template time series files. */
   for( ifile = 1; ifile <= (int) size && *status == SAI__OK; ifile++ ) {

/* Start a new NDF context. */
      ndfBegin();

/* Obtain information about the current template NDF, but do not map the
   arrays. */
      smf_open_file( igrp2, ifile, "READ", SMF__NOCREATE_DATA, &data, status );

/* Issue a suitable message and abort if anything went wrong. */
      if( *status != SAI__OK ) {
         errRep( FUNC_NAME, "Could not open input template file.", status );
         break;

      } else {
         if( data->file == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfFile associated with smfData.",
                    status );
            break;

         } else if( data->hdr == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfHead associated with smfData.",
                    status );
            break;
         }
      }

/* Report the name of the input template. */
      smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
      msgSeti( "THISFILE", ifile );
      msgSeti( "NUMFILES", size );
      msgOutif( MSG__NORM, " ", "Simulating ^THISFILE/^NUMFILES ^FILE",
                status );

/* Check the reference time series contains double precision values. */
     smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status );

/* Create a time-ordered deep copy of the input data structure to recieve
   the output data. */
      odata = smf_deepcopy_smfData( data, 0, SMF__NOCREATE_DATA |
                                             SMF__NOCREATE_VARIANCE |
                                             SMF__NOCREATE_QUALITY,
                                    1, 1, status );

/* Allocate memory for the output data array, and initialise it to hold
   bad at every element. */
      nel = odata ? odata->dims[ 0 ]*odata->dims[ 1 ]*odata->dims[ 2 ] : 0;
      out_data = astMalloc( nel*sizeof( *out_data ) );
      if( *status == SAI__OK ) {
         pd = odata->pntr[ 0 ] = out_data;
         for( iel = 0; iel < nel; iel++ ) *(pd++) = VAL__BADD;
      }

/* Resample the cube data into the output time series. */
      smf_resampmap( wf, odata, abskyfrm, skymap, moving, slbnd, subnd,
                     interp, params, sigma, in_data, out_data, &ngood,
                     status );

/* Issue a wrning if there is no good data in the output cube. */
      if( ngood == 0 ) msgOutif( MSG__NORM, " ", "   Output contains no "
                                 "good data values.", status );

/* Write the output data to disk. */
      smf_write_smfData( odata, NULL, NULL, ogrp, ifile, NDF__NOID,
                         MSG__VERB, status );

/* Close the input time series file. */
      smf_close_file( &data, status );

/* End the NDF context. */
      ndfEnd( status );
   }

/* Close any input data file that is still open due to an early exit from
   the above loop. */
   if( data != NULL ) {
      smf_close_file( &data, status );
      data = NULL;
   }

/* Free remaining resources. */
   if( igrp1 != NULL) grpDelet( &igrp1, status);
   if( igrp2 != NULL) grpDelet( &igrp2, status);
   if( ogrp != NULL) grpDelet( &ogrp, status);

/* End the NDF context. */
   ndfEnd( status );

/* End the tile's AST context. */
   astEnd;

/* Issue a status indication.*/
   if( *status == SAI__OK ) {
      msgOutif(MSG__VERB," ",TASK_NAME " succeeded, time series written.", status);
   } else {
      msgOutif(MSG__VERB," ",TASK_NAME " failed.", status);
   }
}
