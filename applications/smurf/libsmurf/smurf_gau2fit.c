/*
*+
*  Name:
*     GAU2FIT

*  Purpose:

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_gau2fit( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine finds the parameters of a two-component Gaussian beam
*     by doing a least squares fit to a supplied 2D NDF containing an
*     image of a compact source. The source need not be a point source
*     - it is assumed to be a sharp-edged circular disc of specified radius.
*     On each iteration of the fitting process, this simplified source
*     model is convolved with the candidate two-component beam and the
*     residuals with the supplied image are then found. The parameters of
*     the beam are modified on each iteration to minimise the sum of the
*     squared residuals. The beam consists of two concentric Gaussians added
*     together. The first Gaussian has a fixed amplitude of 1.0. The following
*     parameters are varied during the minimisation process:
*
*     - The FWHM of the first Gaussian.
*     - The FWHM and amplitude of the second Gaussian.
*     - The centre position of the beam within the supplied image.
*     - The peak value in the source.
*     - The background level (but only if parameter FITBACK is TRUE).
*
*     Note, if the two components have the same FWHM, they become
*     degenerate (i.e. indistinguishable), which causes problems for the
*     minimisation process. To avoid this, the cost function that is
*     minimised (i.e. the sum of the squared residuals between the smoothed
*     source model and the supplied map) is multiplied by a factor that
*     increases the cost as the FWHM for the two beams becomes more
*     similar. Consequently the second component will always have a larger
*     FWHM than the first beam.

*  ADAM Parameters:
*     AMP1 = _DOUBLE (Write)
*          An output parameter holding the amplitude of the first fitted
*          Gaussian (always 1.0).
*     AMP2 = _DOUBLE (Write)
*          An output parameter holding the amplitude of the second fitted
*          Gaussian.
*     BACK = _DOUBLE (Write)
*          An output parameter holding the fitted background level. This
*          will be set to zero if FITBACK is FALSE.
*     CENTRE = LITERAL (Write)
*          An output parameter holding the fitted position of the beam
*          centre, in the celestial co-ordinate frame of the NDF (which
*          may or may not be the current co-ordinate system). The string
*          consists of a space separated list of two formatted axis values.
*     FITBACK = _LOGICAL (Read)
*          If FALSE, the background level is fixed at zero throughout the
*          minimisation. If TRUE, the background level is included as
*          one of the free parameters in the fit. [TRUE]
*     FWHM1 = _DOUBLE (Write)
*          An output parameter holding the FWHM of the first fitted Gaussian
*          in arc-sec.
*     FWHM2 = _DOUBLE (Write)
*          An output parameter holding the FWHM of the second fitted Gaussian
*          in arc-sec.
*     IN = NDF (Read)
*          Input image containing the source to be fitted. Note, if
*          parameter FITBACK is FALSE, the source should be on a zero
*          background. The WCS information must contain a pair of celestial
*          axes, but they need not be the current coordinate system.
*     INITCENTRE = LITERAL (Read)
*          An initial guess at the position of the beam centre, in the
*          celestial co-ordinate frame of the NDF (which may or may not
*          be the current co-ordinate system). A space or comma
*          separated list of formatted axis values should be supplied.
*     LOGFILE = LITERAL (Read)
*          Name of a log file in which to store a table holding the beam
*          parameters and cost function at each iteration of the minimisation.
*          This table is in TOPCAT "ascii" format. [!]
*     OUT = NDF (Write)
*          Output NDF containing the fitted model. This is the source
*          model convolved with the final beam.
*     RADIUS = _DOUBLE (Read)
*          The radius of the sharp-edge circular source, in arc-seconds.
*          Must be no smaller than 1.0 arcsec.
*     RMS = _DOUBLE (Write)
*          An output parameter holding the RMS residual between the fitted
*          model and the supplied image.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     12-NOV-2020 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.
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

/* Use the 8 byte API for the NDF library. */
#define NDF_I8 1

#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"

void smurf_gau2fit( int *status ) {

/* Local Variables: */
   AstFrame *cfrm;
   AstFrameSet *iwcs;
   Grp *igrp = NULL;
   ThrWorkForce *wf = NULL;
   char cenbuf[200];
   char logfile[200];
   const char *skyrefis;
   double *ipd;
   double *ipo;
   double a1;
   double a2;
   double a;
   double at[ 2 ];
   double b;
   double back;
   double bc[ 2 ];
   double cc[ 2 ];
   double fwhm1;
   double fwhm2;
   double pixsize;
   double pxscl[ 2 ];
   double radius;
   double rms;
   hdsdim dims[ 2 ];
   hdsdim lbnd[ 2 ];
   hdsdim ubnd[ 2 ];
   int fitback;
   int indf;
   int indfo;
   int isky;
   int pnull;
   int sdim[ 2 ];
   size_t nel;
   size_t size;

/* Start AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

/* Get the input map. */
   kpg1Rgndf( "IN", 1, 1, "", &igrp, &size, status );
   ndgNdfas( igrp, 1, "READ", &indf, status );
   grpDelet( &igrp, status);

/* Get the WCS FrameSet and identify the significiant axes. */
   kpg1Asget8( indf, 2, 1, 1, 1, sdim, lbnd, ubnd, &iwcs, status );
   dims[ 0 ] = ubnd[ 0 ] - lbnd[ 0 ] + 1;
   dims[ 1 ] = ubnd[ 1 ] - lbnd[ 1 ] + 1;

/* Ensure that the current frame is a SkyFrame. */
   kpg1Asffr( iwcs, "SKY", &isky, status );
   if( isky == AST__NOFRAME && *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( " ", "No celestial axes found in WCS info for ^N", status );
   }
   astSetI( iwcs, "Current", isky );

/* Get the nominal pixel size in arcsec. */
   at[ 0 ] = 0.5*dims[ 0 ];
   at[ 1 ] = 0.5*dims[ 1 ];
   kpg1Pxscl( iwcs, at, pxscl, status );
   pixsize = sqrt( pxscl[ 0 ]*pxscl[ 1 ] )*AST__DR2D*3600.0;

/* Map the data array. */
   ndfMap( indf, "Data", "_DOUBLE", "READ", (void **) &ipd, &nel, status );

/* Create and map the output NDF by propagation from the input NDF. */
   ndfProp( indf, "WCS", "OUT", &indfo, status );
   ndfMap( indfo, "Data", "_DOUBLE", "WRITE", (void **) &ipo, &nel, status );

/* If the current Frame is a SkyFrame and the SkyRef attribute is set,
   get the reference position. */
   pnull = 0;
   cfrm = astGetFrame( iwcs, AST__CURRENT );
   if( astIsASkyFrame( cfrm ) ){
      skyrefis = astGetC( cfrm, "SkyRefIs" );
      if( skyrefis && !strcmp( skyrefis, "Origin" ) ){
         cc[ 0 ] = 0.0;
         cc[ 1 ] = 0.0;
         pnull = 1;
      } else if( astTest( cfrm, "SkyRef" ) ){
         cc[ 0 ] = astGetD( cfrm, "SkyRef(1)" );
         cc[ 1 ] = astGetD( cfrm, "SkyRef(2)" );
         pnull = 1;
      }
   }

/* Get the source centre in GRID coords, using the SkyRef position
   obtained above as the dynamic default. */
   kpg1Gtpos( "INITCENTRE", iwcs, pnull, cc, bc, status );

/* Get the source radius in arc-sec. Must be no smaller than 1 arc-sec. */
   parGdr0d( "RADIUS", 1.0, 1.0, 1.0E6, 0, &radius, status );

/* See if the background level is to be fixed at zero. */
   parGet0l( "FITBACK", &fitback, status );

/* Get the log file, if any. */
   if( *status == SAI__OK ) {
      parGet0c( "LOGFILE", logfile, sizeof(logfile), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         logfile[ 0 ] = 0;
      }
   }

/* Fit a 2-component Gaussian to the source in the supplied NDF. */
   smf_twobeam( wf, ipd, dims[ 0 ], dims[ 1 ], pixsize, bc + 0, bc + 1,
                radius, logfile, fitback, &a1, &a2, &fwhm1, &fwhm2, &back,
                ipo, &rms, status );

/* Convert centre grid coords to current frame. */
   astTran2( iwcs, 1, bc, bc + 1, 1, &a, &b );

/* Display the results. */
   msgBlank( status );
   msgOut( " ", "Beam centre:", status );
   msgOutf( " ", "   %s=%s (%s)  %s=%s (%s)", status,
            astGetC( iwcs, "Symbol(1)" ), astFormat( iwcs, 1, a ),
            astGetC( iwcs, "Unit(1)" ),
            astGetC( iwcs, "Symbol(2)" ), astFormat( iwcs, 2, b ),
            astGetC( iwcs, "Unit(2)" ) );
   msgBlank( status );
   msgOut( " ", "First component:", status );
   msgOutf( " ", "   Amplitude: %g", status, a1 );
   msgOutf( " ", "   FWHM: %g arc-sec", status, fwhm1 );
   msgBlank( status );
   msgOut( " ", "Second component:", status );
   msgOutf( " ", "   Amplitude: %g", status, a2 );
   msgOutf( " ", "   FWHM: %g arc-sec", status, fwhm2 );
   msgBlank( status );
   msgOut( " ", "Background:", status );
   msgOutf( " ", "   %g", status, back );
   msgBlank( status );
   msgOut( " ", "RMS residual:", status );
   msgOutf( " ", "   %g", status, rms );
   msgBlank( status );

/* Write the fitted values to the output parameters. */
   parPut0d( "AMP1", a1, status );
   parPut0d( "AMP2", a2, status );
   parPut0d( "BACK", back, status );
   parPut0d( "FWHM1", fwhm1, status );
   parPut0d( "FWHM2", fwhm2, status );
   parPut0d( "RMS", rms, status );
   sprintf( cenbuf, "%s %s", astFormat( iwcs, 1, a ),
            astFormat( iwcs, 2, b ) );
   parPut0c( "CENTRE", cenbuf, status );

/* End the NDF ans AST contexts. */
   ndfEnd( status );
   astEnd;

}
