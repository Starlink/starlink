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
*     ALIGNSYS = _LOGICAL (Read)
*          If TRUE, then the spatial positions of the template time series
*          data are aligned with the supplied sky map in the current
*          co-ordinate system of the map,  Otherwise, they are aligned in
*          the ICRS co-ordinate system. For instance, if the current
*          co-ordinate system in the sky map is AZEL, then setting ALIGNSYS
*          to TRUE will result in the template data being aligned in AZEL
*          directly, disregarding the fact that a given AZEL will correspond
*          to different positions on the sky at different times. [FALSE]
*     ANGROT = _DOUBLE (Read)
*          The angle from the focal plane X axis to the fixed analyser, in
*          degrees. Measured positive in the same sense as rotation from focal
*          plane X to focal plane Y. [90.0]
*     COM = NDF (Read)
*          A group of existing time series NDFs that supply the
*          common-mode signal to be added to the output time series data. The
*          number of NDFs supplied should match the number of NDFs supplied
*          for parameter REF. Each supplied NDF should be one-dimensional,
*          with length equal to the length of the time axis of the
*          corresponding REF cube. No common-mode is added to the data if
*          null (!) is supplied. [!]
*     HARMONIC = _INTEGER (Read)
*          The Q and U values are derived from the fourth harmonic of the
*          half-wave plate rotation. However, to allow investigation of
*          other instrumental effects, it is possible instead to derive
*          equivalent quantities from any specified harmonic. These quantities
*          are calculated in exactly the same way as Q and U, but use the
*          harmonic specified by this parameter. They are stored in the
*          output NDFs given by OUT, in place of the normal fourth
*          harmonic signal. [4]
*     IN = NDF (Read)
*          The input 2D image of the sky. If NDFs are supplied for the
*          QIN and UIN parameters, then IN should hold I values.
*     INSTQ = NDF (Read)
*          An optional 2D input NDF holding the instrumental normalised Q
*          value for each bolometer, with respect to the second pixel axis
*          (i.e. the pixel Y axis). The NDF should have dimensions of
*          (32,40). The total intensity falling on each bolometer is
*          multiplied by the corresponding value in this file, to get the
*          instrumental Q value that is added onto the value read from the
*          QIN parameter. Bad values are treated as zero values. Note,
*          if INSTQ is specified, an error is reported if the supplied
*          template files (see parameter REF) include data for more than
*          one SCUBA-2 sub-array. [!]
*     INSTU = NDF (Read)
*          An optional 2D input NDF holding the instrumental normalised U
*          value for each bolometer, with respect to the second pixel axis
*          (i.e. the pixel Y axis). The NDF should have dimensions of
*          (32,40). The total intensity falling on each bolometer is
*          multiplied by the corresponding value in this file, to get the
*          instrumental U value that is added onto the value read from the
*          UIN parameter. Bad values are treated as zero values. Note,
*          INSTU is specified, an error is reported if the supplied
*          template files (see parameter REF) include data for more than
*          one SCUBA-2 sub-array. [!]
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
*     PAOFF = _DOUBLE (Read)
*          The angle from the fixed analyser to the have-wave plate for a
*          POL_ANG value of zero, in degrees. Measured positive in the same
*          sense as rotation from focal plane X to focal plane Y. [0.0]
*     PASIGN = _LOGICAL (Read)
*          Indicates the sense of rotation of the spinning half-wave plate. If
*          TRUE, it is assumed that a positive POL_ANG value corresponds
*          to rotation from focal plane X to focal plane Y axis. If FALSE, it
*          is assumed that a positive POL_ANG value corresponds to rotation
*          from focal plane Y to focal plane X axis. [TRUE]
*     QIN = NDF (Read)
*          The input 2D image of the sky Q values, with respect to the
*          second pixel axis (i.e. the pixel Y axis). If QIN and UIN are
*          both supplied, then the time series specified by the REF parameter
*          should contain flat-fielded POL2 data. [!]
*     REF = NDF (Read)
*          A group of existing time series data cubes. These act as templates
*          for the new time series cubes created by this application, and
*          specified via parameter OUT. They should contain _DOUBLE (i.e.
*          flat-fielded) data values.
*     SIGMA = _DOUBLE (Read)
*          The standard deviation of the Gaussian noise to add to the
*          output data. [0.0]
*     UIN = NDF (Read)
*          The input 2D image of the sky U values, with respect to the
*          second pixel axis (i.e. the pixel Y axis). If QIN and UIN are
*          both supplied, then the time series specified by the REF parameter
*          should contain flat-fielded POL2 data. [!]
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
*     8-JAN-2013 (DSB):
*        Added parameters PASIGN, PAOFF and ANGROT.
*     20-SEP-2013 (DSB):
*        Added ADAM parameter HARMONIC.
*     5-JAN-2015 (DSB):
*        Added ADAM parameter ALIGNSYS.
*     15-APR-2015 (DSB):
*        Added ADAM parameter COM.

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

#define FUNC_NAME "smurf_unmakemap"
#define TASK_NAME "UNMAKEMAP"
#define LEN__METHOD 20

void smurf_unmakemap( int *status ) {

/* Local Variables */
   AstFrameSet *wcsin = NULL; /* WCS Frameset for input cube */
   AstMapping *skymap;        /* GRID->SkyFrame Mapping from input WCS */
   AstSkyFrame *abskyfrm;     /* Input SkyFrame (always absolute) */
   AstSkyFrame *skyfrm = NULL;/* SkyFrame from the input WCS Frameset */
   Grp *igrp1 = NULL;         /* Group of input sky files */
   Grp *igrp2 = NULL;         /* Group of input template files */
   Grp *igrpc = NULL;         /* Group of input COM files */
   Grp *igrpq = NULL;         /* Group of input Q  sky files */
   Grp *igrpu = NULL;         /* Group of input U sky files */
   Grp *ogrp = NULL;          /* Group containing output file */
   ThrWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */
   char pabuf[ 10 ];          /* Text buffer for parameter value */
   dim_t iel;                 /* Index of next element */
   dim_t ndata;               /* Number of elements in array */
   dim_t ntslice;             /* Number of time slices in array */
   double *ang_data = NULL;   /* Pointer to the FP orientation angles */
   double *in_data = NULL;    /* Pointer to the input I sky map */
   double *inc_data = NULL;   /* Pointer to the input COM data */
   double *inq_data = NULL;   /* Pointer to the input Q sky map */
   double *inu_data = NULL;   /* Pointer to the input U sky map */
   double *outq_data = NULL;  /* Pointer to the Q time series data */
   double *outu_data = NULL;  /* Pointer to the U time series data */
   double *pd;                /* Pointer to next element */
   double *pq = NULL;         /* Pointer to next Q time series value */
   double *pu = NULL;         /* Pointer to next U time series value */
   double *qinst_data = NULL; /* Pointer to the instrumental Q data */
   double *uinst_data = NULL; /* Pointer to the instrumental U data */
   double angrot;             /* Angle from focal plane X axis to fixed analyser */
   double paoff;              /* WPLATE value corresponding to POL_ANG=0.0 */
   double params[ 4 ];        /* astResample parameters */
   double sigma;              /* Standard deviation of noise to add to output */
   int alignsys;              /* Align data in the map's system? */
   int dims[ NDF__MXDIM ];    /* NDF dimensions */
   int flag;                  /* Was the group expression flagged? */
   int harmonic;              /* The requested harmonic */
   int ifile;                 /* Input file index */
   int indf;                  /* Input sky map NDF identifier */
   int indfc;                 /* Input COM NDF identifier */
   int indfin;                /* Input template cube NDF identifier */
   int indfiq;                /* Input instrumental Q NDF */
   int indfiu;                /* Input instrumental U NDF */
   int indfout;               /* Output cube NDF identifier */
   int indfq;                 /* Input Q map NDF identifier */
   int indfu;                 /* Input U map NDF identifier */
   int interp = 0;            /* Pixel interpolation method */
   int moving;                /* Is the telescope base position changing? */
   int ndim;                  /* Number of pixel axes in NDF */
   int nel;                   /* Number of elements in array */
   int nelc;                  /* Number of elements in COM array */
   int nelqu;                 /* Number of elements in Q or U array */
   int ngood;                 /* No. of good values in putput cube */
   int nparam = 0;            /* No. of parameters required for interpolation scheme */
   int pasign;                /* Indicates sense of POL_ANG value */
   int sdim[ 2 ];             /* Array of significant pixel axes */
   int singlesub;             /* Only one subarray allowed? */
   int slbnd[ 2 ];            /* Array of lower bounds of input map */
   int subnd[ 2 ];            /* Array of upper bounds of input map */
   sc2ast_subarray_t subnum;  /* Identifier for subarray */
   sc2ast_subarray_t subnum0 = SC2AST__NULLSUB; /* Identifier for subarray */
   size_t ncom;               /* Number of com files */
   size_t nskymap;            /* Number of supplied sky cubes */
   size_t outsize;            /* Number of files in output group */
   size_t size;               /* Number of files in input group */
   smfData *odata = NULL;     /* Pointer to output data struct */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context */
   astBegin;

/* Begin an NDF context. */
   ndfBegin();

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

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

/* If the ALIGNSYS parameter is TRUE then we align the raw data with the
   map in the current system of the map, rather than the default ICRS. */
   parGet0l( "ALIGNSYS", &alignsys, status );
   if( alignsys ) astSetC( abskyfrm, "AlignSystem", astGetC( abskyfrm,
                                                             "System" ) );

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

/* Get any Q and U input maps. */
   if( *status == SAI__OK ) {

      kpg1Rgndf( "QIN", 1, 1, "", &igrpq, &nskymap, status );
      ndgNdfas( igrpq, 1, "READ", &indfq, status );
      ndfMap( indfq, "DATA", "_DOUBLE", "READ", (void **) &inq_data, &nelqu,
              status );
      if( nelqu != nel && *status == SAI__OK ) {
         ndfMsg( "Q", indfq );
         *status = SAI__ERROR;
         errRep( "", "Q image '^Q' is not the same size as the I image.",
                 status );
      }

      kpg1Rgndf( "UIN", 1, 1, "", &igrpu, &nskymap, status );
      ndgNdfas( igrpu, 1, "READ", &indfu, status );
      ndfMap( indfu, "DATA", "_DOUBLE", "READ", (void **) &inu_data, &nelqu,
              status );
      if( nelqu != nel && *status == SAI__OK ) {
         ndfMsg( "U", indfu );
         *status = SAI__ERROR;
         errRep( "", "U image '^U' is not the same size as the I image.",
                 status );
      }

      if( *status == PAR__NULL ) {
         ndfAnnul( &indfq, status );
         ndfAnnul( &indfu, status );
         inq_data = NULL;
         inu_data = NULL;
         errAnnul( status );
      } else {
         parGet0d( "ANGROT", &angrot, status );
         parGet0d( "PAOFF", &paoff, status );
         parGet0l( "PASIGN", &pasign, status );
      }
   }

/* Get any common-mode files. */
   if( *status == SAI__OK ) {
      kpg1Rgndf( "COM", size, size, "", &igrpc, &ncom, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         ncom = 0;
      }
   }

/* Get any instrumental polarisation files. */
   singlesub = 0;
   if( *status == SAI__OK ) {
      ndfAssoc( "INSTQ", "Read", &indfiq, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         qinst_data = NULL;
      } else {
         ndfDim( indfiq, 2, dims, &ndim, status );
         if( dims[ 0 ] != 32 || dims[ 1 ] != 40 ) {
            *status = SAI__ERROR;
            ndfMsg( "N", indfiq );
            errRep( " ", "Instrumental polarisation file ^N has bad "
                    "dimensions - should be 32x40.", status );
         } else {
            ndfMap( indfiq, "DATA", "_DOUBLE", "READ", (void **) &qinst_data,
                    &nel, status );
            singlesub = 1;
         }
      }

      ndfAssoc( "INSTU", "Read", &indfiu, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         uinst_data = NULL;
      } else {
         ndfDim( indfiu, 2, dims, &ndim, status );
         if( dims[ 0 ] != 32 || dims[ 1 ] != 40 ) {
            *status = SAI__ERROR;
            ndfMsg( "N", indfiu );
            errRep( " ", "Instrumental polarisation file ^N has bad "
                    "dimensions - should be 32x40.", status );
         } else {
            ndfMap( indfiu, "DATA", "_DOUBLE", "READ", (void **) &uinst_data,
                    &nel, status );
            singlesub = 1;
         }
      }
   }

/* Loop round all the template time series files. */
   for( ifile = 1; ifile <= (int) size && *status == SAI__OK; ifile++ ) {

/* Start a new NDF context. */
      ndfBegin();

/* Create the output NDF by propagating everything from the input, except
   for quality and variance. */
      ndgNdfas( igrp2, ifile, "READ", &indfin, status );

      ndfMsg( "FILE", indfin );
      msgSeti( "THISFILE", ifile );
      msgSeti( "NUMFILES", size );
      msgOutif( MSG__NORM, " ", "Simulating ^THISFILE/^NUMFILES ^FILE",
                status );

      ndgNdfpr( indfin, "DATA,HISTORY,LABEL,TITLE,WCS,UNITS,EXTENSION(*)",
                ogrp, ifile, &indfout, status );
      ndfAnnul( &indfin, status );
      ndfAnnul( &indfout, status );

/* We now re-open the output NDF and then modify its data values. */
      smf_open_file( wf, ogrp, ifile, "UPDATE", 0, &odata, status );

/* Issue a suitable message and abort if anything went wrong. */
      if( *status != SAI__OK ) {
         errRep( FUNC_NAME, "Could not open input template file.", status );
         break;

      } else {
         if( odata->file == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfFile associated with smfData.",
                    status );
            break;

         } else if( odata->hdr == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfHead associated with smfData.",
                    status );
            break;
         }
      }

/* If instrumental Q/U arrays have been supplied, then all data must
   refer to a single subarray (since the same instrument Q/U values are used
   for all data). */
      if( singlesub ) {
         smf_find_subarray( odata->hdr, NULL, 0, &subnum, status );
         if( ifile == 1 ) {
            subnum0 = subnum;
         } else if( subnum != subnum0 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "Supplied REF data refer to more than one "
                    "sub-array", status );
            errRep( FUNC_NAME, "All data must be for one subarray since "
                    "instrumental polarisation has been specified.", status );
            break;
         }
      }

/* Check the reference time series contains double precision values. */
     smf_dtype_check_fatal( odata, NULL, SMF__DOUBLE, status );

/* Get the total number of data elements, and the number of time slices. */
     smf_get_dims( odata, NULL, NULL, NULL, &ntslice, &ndata, NULL,
                   NULL, status );

/* Open any COM file. */
      if( ncom ) {
         ndgNdfas( igrpc, ifile, "READ", &indfc, status );
         ndfMap( indfc, "DATA", "_DOUBLE", "READ", (void **) &inc_data,
                 &nelc, status );

/* Check its dimensions. */
         if( *status == SAI__OK ) {
            if( nelc != (int) ntslice ) {
               *status = SAI__ERROR;
               ndfMsg( "C", indfc );
               ndfMsg( "R", indfin );
               msgSeti( "N", nelc );
               msgSeti( "M", ntslice );
               errRep( " ", "Supplied COM file (^C) has ^N samples, but "
                       "the reference NDF (^R) has ^M time-slices.", status );
            }
         }

      } else {
         indfc = NDF__NOID;
         inc_data = NULL;
      }

/* Fill the output with bad values. */
      if( *status == SAI__OK ) {
         pd = odata->pntr[ 0 ];
         for( iel = 0; iel < ndata; iel++ ) *(pd++) = VAL__BADD;
      }

/* Resample the sky map data into the output time series. */
      smf_resampmap( wf, odata, abskyfrm, skymap, moving, slbnd, subnd,
                     interp, params, sigma, in_data, odata->pntr[ 0 ],
                     NULL, &ngood, status );

/* Add on any COM data. */
      smf_addcom( wf, odata, inc_data, status );

/* Issue a wrning if there is no good data in the output cube. */
      if( ngood == 0 ) msgOutif( MSG__NORM, " ", "   Output contains no "
                                 "good data values.", status );

/* If Q and U maps have been given, allocate room to hold resampled Q and
   U values, and fill them with bad values. */
      if( inq_data && inu_data ) {
         pq = outq_data = astMalloc( ndata*sizeof( *outq_data ) );
         pu = outu_data = astMalloc( ndata*sizeof( *outu_data ) );
         if( *status == SAI__OK ) {
            for( iel = 0; iel < ndata; iel++ ) {
               *(pu++) = VAL__BADD;
               *(pq++) = VAL__BADD;
            }
         }

/* Determine the harmonic to use. */
         parGet0i( "HARMONIC", &harmonic, status );

/* Allocate room for an array to hold the anti-clockwise angle from the
   focal plane Y axis to the Y pixel axis in the reference map, at each
   time slice. */
         ang_data = astMalloc( ntslice*sizeof( *ang_data ) );

/* Resample them both into 3D time series. */
         smf_resampmap( wf, odata, abskyfrm, skymap, moving, slbnd, subnd,
                        interp, params, sigma, inq_data, outq_data,
                        ang_data, &ngood, status );
         smf_resampmap( wf, odata, abskyfrm, skymap, moving, slbnd, subnd,
                        interp, params, sigma, inu_data, outu_data,
                        NULL, &ngood, status );

/* Add on any extra Q and U caused by instrumental polarisation. */
         smf_addinst( wf, odata, qinst_data, outq_data, status );
         smf_addinst( wf, odata, uinst_data, outu_data, status );

/* Combine these time series with the main output time series so that the
   main output is analysed intensity. */
         smf_uncalc_iqu( wf, odata, odata->pntr[ 0 ], outq_data, outu_data,
                         ang_data, pasign, AST__DD2R*paoff, AST__DD2R*angrot,
                         harmonic, status );

/* Release work space. */
         outq_data = astFree( outq_data );
         outu_data = astFree( outu_data );
         ang_data = astFree( ang_data );
      }

/* Close the output time series file. */
      smf_close_file( wf, &odata, status );

/* End the NDF context. */
      ndfEnd( status );
   }

/* Close any input data file that is still open due to an early exit from
   the above loop. */
   if( odata != NULL ) {
      smf_close_file( wf, &odata, status );
      odata = NULL;
   }

/* Free remaining resources. */
   if( igrp1 != NULL) grpDelet( &igrp1, status);
   if( igrp2 != NULL) grpDelet( &igrp2, status);
   if( igrpq != NULL) grpDelet( &igrpq, status);
   if( igrpu != NULL) grpDelet( &igrpu, status);
   if( igrpc != NULL) grpDelet( &igrpc, status);
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


