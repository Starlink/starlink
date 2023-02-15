/* Defining the FPTRAP macro will cause floating point exceptions to
   occur whenever a NaN, inf or overflow is generated. This can make it
   easier to debug the cause of these values. */
#if defined(FPTRAP)
#define _GNU_SOURCE
#include <fenv.h>
#endif

/*
*+
*  Name:
*     UNMAKECUBE

*  Purpose:
*     Produce simulated time series data from a regrided ACSIS data cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_unmakecube( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine creates one or more time series cubes, spanned by
*     (frequency, detector number, time) axes, from one or more input sky
*     cubes spanned by (celestial longitude, celestial latitude, spectrum)
*     axes. Thus, it performs a sort of inverse to the MAKECUBE application.
*
*     The output time series detector samples are created by interpolating the
*     supplied input sky cubes at the position of the reference time series
*     sample centre. Various interpolation methods can be used (see parameter
*     INTERP).
*
*     The output time series cubes inherit all meta-data from the
*     corresponding input reference time series. The only thing modified
*     is the values in the NDF "Data" array.

*  ADAM Parameters:
*     DETECTORS = LITERAL (Read)
*          A group of detector names. Only data for the named detectors
*          will be included in the output time series cubes. If a null (!)
*          value is supplied, data for all detectors will be created. [!]
*     IN = NDF (Read)
*          A group of input (ra,dec,spectrum) sky cubes (for instance, a
*          set of tiles produced by MAKECUBE). If these sky cubes have
*          any spatial overlap, then the output time series data will be
*          derived from the last supplied sky cube that covers the overlap
*          region. That is, sky cubes near the end of the supplied group
*          take precedence over those near the start.
*     INTERP = LITERAL (Read)
*          The method to use when resampling the input sky cube pixel values.
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
*          will be written.
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
*          specified via parameter OUT.
*     USEDETPOS = _LOGICAL (Read)
*          If a true value is supplied, then the detector positions are
*          read from the detector position arrays in each template NDF.
*          Otherwise, the detector positions are calculated on the basis
*          of the FPLANEX/Y arrays. Both methods should (in the absence
*          of bugs) result in identical cubes. [TRUE]

*  Related Applications:
*     SMURF: MAKECUBE

*  Authors:
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     23-JAN-2008 (DSB):
*        Original version.
*     5-MAR-2008 (DSB):
*        Allow multiple sky cubes to be supplied.
*     29-JUN-2009 (DSB):
*        Use new NDG provenance API.
*     2-SEP-2009 (DSB):
*        Always initialise the output to hold bad values.

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

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

#define FUNC_NAME "smurf_unmakecube"
#define TASK_NAME "UNMAKECUBE"
#define LEN__METHOD 20

/* Data types used locally within this file. */

typedef struct SkyCube {
   AstFrame *ispecfrm;      /* SpecFrame from the input WCS Frameset */
   AstMapping *iskymap;     /* GRID->SkyFrame Mapping from input WCS */
   AstMapping *ispecmap;    /* GRID->SpecFrame Mapping from input WCS */
   AstSkyFrame *abskyfrm;   /* Input SkyFrame (always absolute) */
   int indf;                /* Input cube NDF identifier */
   int moving;              /* Is the telescope base position changing? */
   dim_t slbnd[3];          /* Array of lower bounds of input cube */
   dim_t subnd[3];          /* Array of lower bounds of input cube */
} SkyCube;


void smurf_unmakecube( int *status ) {

/* Local Variables */
   AstFrame *tfrm = NULL;     /* Current Frame from input WCS */
   AstFrameSet *wcsin = NULL; /* WCS Frameset for input cube */
   AstMapping *tmap = NULL;   /* Base->current Mapping from input WCS */
   AstSkyFrame *iskyfrm = NULL;/* SkyFrame from the input WCS Frameset */
   Grp *detgrp = NULL;        /* Group of detector names */
   Grp *igrp1 = NULL;         /* Group of input sky cube files */
   Grp *igrp2 = NULL;         /* Group of input template files */
   Grp *ogrp = NULL;          /* Group containing output file */
   NdgProvenance *oprov = NULL;/* Provenance for the output NDF */
   SkyCube *sky_cubes = NULL; /* Pointer to array of sky cube descriptions */
   SkyCube *skycube = NULL;   /* Pointer to next sky cube description */
   char pabuf[ 10 ];          /* Text buffer for parameter value */
   double params[ 4 ];        /* astResample parameters */
   int axes[ 2 ];             /* Indices of selected axes */
   int flag;                  /* Was the group expression flagged? */
   int interp = 0;            /* Pixel interpolation method */
   int nparam = 0;            /* No. of parameters required for interpolation scheme */
   int ondf;                  /* Output time series NDF identifier */
   int outax[ 2 ];            /* Indices of corresponding output axes */
   int overlap;               /* Does time series overlap sky cube? */
   int sdim[3];               /* Array of significant pixel axes */
   int usedetpos;             /* Should the detpos array be used? */
   size_t ifile;              /* Input file index */
   size_t iskycube;           /* Index of current sky cube */
   size_t ndet;               /* Number of detectors supplied for "DETECTORS" */
   size_t nel;                /* Number of elements in 3D array */
   size_t nskycube;           /* Number of supplied sky cubes */
   size_t outsize;            /* Number of files in output group */
   size_t size;               /* Number of files in input group */
   smfData *data = NULL;      /* Pointer to data struct */
   void *in_data = NULL;      /* Pointer to the input cube data array */
   void *out_data = NULL;     /* Pointer to the output cube data array */

#if defined(FPTRAP)
   feenableexcept(FE_DIVBYZERO|FE_INVALID|FE_OVERFLOW);
#endif

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context */
   astBegin;

/* Begin an NDF context. */
   ndfBegin();

/* Get a group holding the input sky cubes. */
   kpg1Rgndf( "IN", 0, 1, "", &igrp1, &nskycube, status );

/* Create an array of structures to hold information about each input sky
   cube. */
   sky_cubes = astMalloc( sizeof( SkyCube )*(dim_t) nskycube );

/* Store a description of each sky cube. */
   if( sky_cubes ) {
      for( iskycube = 0; iskycube < nskycube; iskycube++ ) {
         skycube = sky_cubes + iskycube;

/* Get an NDF identifier for the next sky cube. */
         ndgNdfas( igrp1, iskycube + 1, "READ", &(skycube->indf), status );

/* Get the WCS FrameSet from the sky cube, together with its pixel index
   bounds. */
         kpg1Asget8( skycube->indf, 3, 0, 1, 1, sdim, skycube->slbnd,
                     skycube->subnd, &wcsin, status );

/* Get the base->current Mapping from the input WCS FrameSet, and split it
   into two Mappings; one (iskymap) that maps the first 2 GRID axes into
   celestial sky coordinates, and one (ispecmap) that maps the third GRID
   axis into a spectral coordinate. Also extract the SpecFrame and
   SkyFrame from the current Frame. */
         tmap = astGetMapping( wcsin, AST__BASE, AST__CURRENT );
         tfrm = astGetFrame( wcsin, AST__CURRENT );

         axes[ 0 ] = 1;
         axes[ 1 ] = 2;
         astMapSplit( tmap, 2, axes, outax, &(skycube->iskymap) );
         iskyfrm = astPickAxes( tfrm, 2, outax, NULL );

         axes[ 0 ] = 3;
         astMapSplit( tmap, 1, axes, outax, &(skycube->ispecmap) );
         skycube->ispecfrm = astPickAxes( tfrm, 1, outax, NULL );

/* Create a copy of "iskyfrm" representing absolute coords rather than
   offsets. We assume the target is moving if the cube represents offsets. */
         skycube->abskyfrm = astCopy( iskyfrm );
         astClear( skycube->abskyfrm, "SkyRefIs" );
         skycube->moving = ( *status == SAI__OK &&
                             !strcmp( astGetC( iskyfrm, "SkyRefIs" ),
                                      "Origin" ) ) ? 1 : 0;

/* Invert the Mappings (for the convenience of smf_resamplecube), so
   that they go from current Frame to grid axis. */
         astInvert( skycube->ispecmap );
         astInvert( skycube->iskymap );

/* For efficiency, annul manually the unneeded AST objects created in
   this loop. */
         wcsin = astAnnul( wcsin );
         tmap = astAnnul( tmap );
         tfrm = astAnnul( tfrm );
         iskyfrm = astAnnul( iskyfrm );
      }
   }

/* See if the detector positions are to be read from the RECEPPOS array
   in the template NDFs. Otherwise, they are calculated on the basis of
   the FPLANEX/Y arrays. */
   parGet0l( "USEDETPOS", &usedetpos, status );

/* Get the detectors to use. If a null value is supplied, annull the
   error. Otherwise, make the group case insensitive. */
   detgrp = NULL;
   if( *status == SAI__OK ) {
      kpg1Gtgrp( "DETECTORS", &detgrp, &ndet, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
	 if (detgrp) {
	   grpDelet( &detgrp, status );
	 }
      } else {
         grpSetcs( detgrp, 0, status );
      }
   }

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

/* Create a group holding the names of the corresponding output NDFs. */
   ndgCreat ( "OUT", igrp2, &ogrp, &outsize, &flag, status );
   if( outsize != size && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetk( "O", outsize );
      msgSetk( "I", size );
      errRep( "", "Numbers of input reference cubes (^I) and output "
              "cubes (^O) differ.", status );
   }

/* Loop round all the template time series files. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Start a new NDF context. */
      ndfBegin();

/* Obtain information about the current template NDF, but do not map the
   arrays. */
      smf_open_file( NULL, igrp2, ifile, "READ", SMF__NOCREATE_DATA, &data, status );

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
      msgSetk( "THISFILE", ifile );
      msgSetk( "NUMFILES", size );
      msgOutif( MSG__NORM, " ", "Simulating ^THISFILE/^NUMFILES ^FILE",
                status );

/* Create the output NDF by propagation from the input template NDF.
   Everything is copied except for the array components and any PROVENANCE
   extension. */
      ndgNdfpr( data->file->ndfid, "TITLE,LABEL,UNITS,AXIS,WCS,HISTORY,"
                "NOEXTENSION(PROVENANCE)", ogrp, ifile, &ondf, status );

/* Ensure the output NDF has a history component. */
      ndfHcre( ondf, status );

/* Get a pointer to the mapped output data array. Set all values bad. */
      ndfMap( ondf, "DATA", "_REAL", "WRITE/BAD", &out_data, &nel, status );

/* If the detector positions are to calculated on the basis of FPLANEX/Y
   rather than detpos, then free the detpos array in the templates smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
      if( !usedetpos && data->hdr->detpos ) {
         astFree( (double *) data->hdr->detpos );
         data->hdr->detpos = NULL;
      }

/* Get a pointer to a structure holding provenance information for the
   output time series. */
      oprov = ndgReadProv( ondf, "SMURF:UNMAKECUBE", status );

/* Record details of the template in the provenance structure for the
   output time series. */
      ndgPutProv( oprov, data->file->ndfid, NULL, 0, status );

/* Loop round all input sky cubes. */
      for( iskycube = 0; iskycube < nskycube; iskycube++ ) {
         skycube = sky_cubes + iskycube;

/* Record details of the input cube in the provenance extension of the
   output time series. */
         ndgPutProv( oprov, skycube->indf, NULL, 0, status );

/* See if the current time series overlaps the current sky cube. */
         smf_resampcube( data, skycube->abskyfrm,
                         skycube->iskymap, skycube->ispecfrm,
                         skycube->ispecmap, detgrp, skycube->moving,
                         skycube->slbnd, skycube->subnd, interp,
                         params, NULL, NULL, &overlap, status );

/* If not, pass on to the next sky cube. */
         if( overlap ) {

/* Report the name of the sky cube. */
            ndfMsg( "NDF", skycube->indf );
            msgOutif( MSG__NORM, " ", "   Re-sampling ^NDF", status );

/* Map the data array in the current sky cube. */
            ndfMap( skycube->indf, "DATA", "_REAL", "READ", &in_data, &nel,
                    status );

/* Resample the cube data into the output time series. */
            smf_resampcube( data, skycube->abskyfrm,
                            skycube->iskymap, skycube->ispecfrm,
                            skycube->ispecmap, detgrp, skycube->moving,
                            skycube->slbnd, skycube->subnd, interp,
                            params, in_data, out_data, &overlap, status );

/* Unmap the data array. */
            ndfUnmap( skycube->indf, "DATA", status );
         }
      }

/* Write the provenance structure to the output NDF, and then free it. */
      ndgWriteProv( oprov, ondf, 1, status );
      oprov =ndgFreeProv( oprov, status );

/* Close the input time series file. */
      if( data != NULL ) {
         smf_close_file( NULL, &data, status );
         data = NULL;
      }

/* End the NDF context. */
      ndfEnd( status );
   }

/* Close any input data file that is still open due to an early exit from
   the above loop. */
   if( data != NULL ) {
      smf_close_file( NULL, &data, status );
      data = NULL;
   }

/* Free remaining resources. */
   if( detgrp != NULL) grpDelet( &detgrp, status);
   if( igrp1 != NULL) grpDelet( &igrp1, status);
   if( igrp2 != NULL) grpDelet( &igrp2, status);
   if( ogrp != NULL) grpDelet( &ogrp, status);
   sky_cubes = astFree( sky_cubes );

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
