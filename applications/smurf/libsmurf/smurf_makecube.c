/*
*+
*  Name:
*     MAKECUBE

*  Purpose:
*     Regrid ACSIS spectra into a data cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_makecube( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine converts one or more raw data cubes, spanned by
*     (frequency, detector number, time) axes, into a single output cube
*     spanned by (celestial longitude, celestial latitude, frequency) axes.
*     Nearest neighbour rebinning is used (that is, each input data sample 
*     is placed into the nearest output pixel). 
*
*     The parameters of the projection from sky to pixel grid coordinates
*     can be specified using parameters CROTA, PIXSIZE, REFLAT, REFLON.
*     Alternatively, parameter AUTOGRID can be set true, in which case 
*     projection parameters are determined automatically in a manner that
*     favours projections that place samples centrally within pixels.

*  ADAM Parameters:
*     AUTOGRID = _LOGICAL (Read)
*          Determines how the dynamic default values should be determined 
*          for the projection parameters CROTA, PIXSIZE, REFLAT and REFLON. 
*          If TRUE, then default projection parameters are determined by 
*          adjusting the grid until as many data samples as possible fall 
*          close to the centre of pixels in the output cube. If FALSE, 
*          REFLON/REFLAT are set to the first pointing BASE position, CROTA 
*          is set to zero, and PIXSIZE is are set to 6 arc-seconds. In 
*          addition, if AUTOGRID is TRUE the precise placement of the tangent 
*          point is adjusted by up to 1 pixel along each spatial pixel axis 
*          in order to optimise the grid. [FALSE]
*     CATFRAME = LITERAL (Read)
*          A string determining the co-ordinate Frame in which positions are 
*          to be stored in the output catalogue associated with parameter
*          OUTCAT. The string supplied for CATFRAME can be one of the 
*          following:
*	   
*          - A Domain name such as SKY, AXIS, PIXEL, etc. 
*	   
*          - An integer value giving the index of the required Frame.
*	   
*          - An IRAS90 Sky Co-ordinate System (SCS) values such as 
*          EQUAT(J2000) (see SUN/163).
*	   
*          If a null (!) value is supplied, the positions will be stored 
*          in the current Frame of the output NDF. [!]
*     CATEPOCH = DOUBLE PRECISION (Read)
*          The epoch at which the sky positions stored in the output
*          catalogue were determined. It will only be accessed if an epoch
*          value is needed to qualify the co-ordinate Frame specified by 
*          COLFRAME. If required, it should be given as a decimal years 
*          value, with or without decimal places ("1996.8" for example). 
*          Such values are interpreted as a Besselian epoch if less than 
*          1984.0 and as a Julian epoch otherwise. 
*     CROTA = REAL (Read)
*          The angle, in degrees, from north through east to the second
*          pixel axis in the output cube. The dynamic default value is
*          determined by the AUTOGRID parameter. []
*     DETECTORS = LITERAL (Read)
*          A group of detector names. Only data form the named detectors
*          will be included in the output cube. If a null (!) value is 
*          supplied, data from all detectors will be used. [!]
*     IN = NDF (Read)
*          Input file(s)
*     OUT = NDF (Write)
*          Output file
*     OUTCAT = FILENAME (Write)
*          An output catalogue in which to store all the detector positions 
*          used to make the output cube. By default, these are in the same 
*          sky coordinate system as the current Frame in the output NDF
*          (but see parameter CATFRAME). The label associated with each row 
*          in the catalogue is the detector name. If a null value (!) is 
*          supplied, no output catalogue is produced. See also parameter 
*          CATFRAME. [!]
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
*     PIXSIZE( 2 ) = REAL (Read)
*          Pixel dimensions in the output image, in arcsec. If only one value 
*          is supplied, the same value will be used for both axes. The 
*          dynamic default value is determined by the AUTOGRID parameter. []
*     REFLAT = LITERAL (Read)
*          The formatted celestial latitude value at the tangent point of 
*          the spatial projection in the output cube. This should be provided 
*          in the system specified by parameter SYSTEM. The dynamic default 
*          value is determined by the AUTOGRID parameter. []
*     REFLON = LITERAL (Read)
*          The formatted celestial longitude value at the tangent point of 
*          the spatial projection in the output cube. This should be provided 
*          in the system specified by parameter SYSTEM. The dynamic default 
*          value is determined by the AUTOGRID parameter. []
*     SAVEWEIGHTS = _LOGICAL (Read)
*          If TRUE, then the weights associated with the array of output
*          pixels is stored in an extension named ACSISRED, within the output 
*          NDF. If FALSE the weights are discarded once they have been
*          used. These weights record thre relative weight of the input
*          data associated with each output pixel. [FALSE]
*     SPREAD = LITERAL (Read)
*          The method to use when spreading each input pixel value out
*          between a group of neighbouring output pixels. SPREAD can take 
*          the following values:
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
*          ICRS, GAPPT, FK5, FK4, FK4-NO-E, AZEL, GALACTIC, ECLIPTIC or 
*          TRACKING.
*     USEDETPOS = _LOGICAL (Read)
*          If a true value is supplied, then the detector positions are
*          read from the detector position arrays in each input NDF.
*          Otherwise, the detector positions are calculated on the basis
*          of the FPLANEX/Y arrays. Both methods should (in the absence 
*          of bugs) result in identical cubes. [TRUE]

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2006 (TIMJ):
*        Clone from smurf_makemap
*     18-SEP-2006 (DSB):
*        MAKECUBE code added.
*     6-NOV-2006 (DSB):
*        Added parameter DETECTORS.
*     10-NOV-2006 (DSB):
*        Added HISTORY component to output NDF.
*     14-NOV-2006 (DSB):
*        Added AUTOGRID parameter.
*     20-NOV-2006 (DSB):
*        - Make the DETECTORS parameter case insensitive.
*        - Document label column in OUTCAT.
*     21-NOV-2006 (DSB):
*        AUTOGRID now supplies the dynamic defaults for the projection
*        parametersm which are now acquired after AUTOGRID.
*     23-NOV-2006 (DSB):
*        - SYSTEM can now accept any AST celestial System name.
*        - Fix incorrect indices for "pixsize" array when checking pixel
*        sizes.
*     24-NOV-2006 (DSB):
*        Added PARAMS and SPREAD parameters.
*     28-NOV-2006 (DSB):
*        - Propagate Label and Unit components from first input NDF to the
*        output NDF.
*        - Added SAVEWEIGHTS parameter.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
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

#define FUNC_NAME "smurf_makecube"
#define TASK_NAME "MAKECUBE"
#define LEN__METHOD 20

/* Returns nearest integer to "x" */
#define NINT(x) ( ( (x) > 0 ) ? (int)( (x) + 0.5 ) : (int)( (x) - 0.5 ) )

void smurf_makecube( int *status ) {

/* Local Variables */
   AstSkyFrame *oskyfrm = NULL;/* SkyFrame from the output WCS Frameset */
   AstFrame *ospecfrm = NULL;  /* SpecFrame from the output WCS Frameset */
   AstFrame *tfrm = NULL;      /* Current Frame from output WCS */
   AstFrameSet *swcsout = NULL;/* Spatial WCS FrameSet for output cube */
   AstFrameSet *wcsout = NULL; /* WCS Frameset for output cube */
   AstMapping *oskymap = NULL; /* GRID->SkyFrame Mapping from output WCS */
   AstMapping *ospecmap = NULL;/* GRID->SpecFrame Mapping from output WCS */
   AstMapping *tmap = NULL;   /* Base->current Mapping from output WCS */
   Grp *detgrp = NULL;        /* Group of detector names */
   Grp *igrp = NULL;          /* Group of input files */
   Grp *ogrp = NULL;          /* Group containing output file */
   HDSLoc *weightsloc = NULL; /* HDS locator of weights array */
   char *pname = NULL;        /* Name of currently opened data file */
   char spbuf[ 10 ];          /* Text buffer for SPREAD parameter value */
   char system[ 10 ];         /* Celestial coord system for output cube */
   char reflat[ 41 ];         /* Reference latitude string */
   char reflon[ 41 ];         /* Reference longitude string */
   double par[ 7 ];           /* Projection parameter */
   double params[ 4 ];        /* astRebinSeq parameters */
   double pixsize[ 2 ];       /* Pixel sizes in arc-seconds */
   int axes[ 2 ];             /* Indices of selected axes */
   int autogrid;              /* Determine projection parameters automatically? */
   int flag;                  /* Is group expression to be continued? */
   int ifile;                 /* Input file index */
   int lbnd_out[ 3 ];         /* Lower pixel bounds for output map */
   int lbnd_wgt[ 3 ];         /* Lower pixel bounds for wight array */
   int moving;                /* Is the telescope base position changing? */
   int ndet;                  /* Number of detectors supplied for "DETECTORS" */
   int nparam;                /* No. of parameters required for spreading scheme */
   int nval;                  /* Number of supplied positions */
   int ondf;                  /* output NDF identifier */
   int outax[ 2 ];            /* Indices of corresponding output axes */
   int outsize;               /* Number of files in output group */
   int savewgt;               /* Should weights be saved in the output NDF? */
   int size;                  /* Number of files in input group */
   int smfflags;              /* Flags for smfData */
   int spread;                /* Pixel spreading method */
   int ubnd_out[ 3 ];         /* Upper pixel bounds for output map */
   int ubnd_wgt[ 3 ];         /* Upper pixel bounds for wight array */
   int usedetpos;             /* Should the detpos array be used? */
   smfData *data = NULL;      /* Pointer to data struct */
   smfData *odata = NULL;     /* Pointer to output SCUBA2 data struct */
   smfData *wdata = NULL;     /* Pointer to SCUBA2 data struct */
   smfFile *file = NULL;      /* Pointer to data file struct */
   void *data_array = NULL;   /* Pointer to the rebinned map data */
   void *var_array = NULL;    /* Pointer to the variance map */
   void *wgt_array = NULL;    /* Pointer to the weights map */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an NDF context (we do not begin an AST context since this is
   done within the calling monolith routine). */
   ndfBegin();

/* Get a group of input files */ 
   ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

/* Get the celestial coordinate system for the output cube. */
   parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC"
             "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

/* Get the pixel spreading scheme to use. */
   parChoic( "SPREAD", "NEAREST", "NEAREST,LINEAR,SINC,SINCSINC,SINCCOS"
             "SINCGAUSS,SOMB,SOMBCOS,GAUSS", 1, spbuf, 10, status );

   if( !strcmp( spbuf, "NEAREST" ) ) {
      spread = AST__NEAREST;
      nparam = 0;

   } else if( !strcmp( spbuf, "LINEAR" ) ) {
      spread = AST__LINEAR;
      nparam = 0;

   } else if( !strcmp( spbuf, "SINC" ) ) {      
      spread = AST__SINC;
      nparam = 1;

   } else if( !strcmp( spbuf, "SINCSINC" ) ) {      
      spread = AST__SINCSINC;
      nparam = 2;

   } else if( !strcmp( spbuf, "SINCCOS" ) ) {      
      spread = AST__SINCCOS;
      nparam = 2;

   } else if( !strcmp( spbuf, "SINCGAUSS" ) ) {      
      spread = AST__SINCGAUSS;
      nparam = 2;

   } else if( !strcmp( spbuf, "SOMB" ) ) {      
      spread = AST__SOMB;
      nparam = 1;

   } else if( !strcmp( spbuf, "SOMBCOS" ) ) {      
      spread = AST__SOMBCOS;
      nparam = 2;

   } else if( !strcmp( spbuf, "GAUSS" ) ) {      
      spread = AST__GAUSS;
      nparam = 2;

   } else if( *status == SAI__OK ) {
      nparam = 0;
      *status = SAI__ERROR;
      msgSetc( "V", spbuf );
      errRep( "", "Support not available for SPREAD = ^V (programming "
              "error)", status );
   }

/* Get an additional parameter vector if required. */
   if( nparam > 0 ) parExacd( "PARAMS", nparam, params, status );

/* See of the detector positions are to be read from the RECEPPOS array. 
   Otherwise, they are calculated on the basis of the FPLANEX/Y arrays. */
   parGet0l( "USEDETPOS", &usedetpos, status );

/* Get the detectors to use. If a null value is supplied, annul the
   error. Otherwise, make the group case insensitive. */
   detgrp = NULL;
   if( *status == SAI__OK ) {
      kpg1Gtgrp( "DETECTORS", &detgrp, &ndet, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else {
         grpSetcs( detgrp, 0, status );
      }
   }
  
/* Indicate we have no projection parameters as yet. */
   par[ 0 ] = AST__BAD;
   par[ 1 ] = AST__BAD;
   par[ 2 ] = AST__BAD;
   par[ 3 ] = AST__BAD;
   par[ 4 ] = AST__BAD;
   par[ 5 ] = AST__BAD;
   par[ 6 ] = AST__BAD;

/* See if any unspecified projection parameters are to be determined using
   an optimal fitting process. */
   parGet0l( "AUTOGRID", &autogrid, status );

/* Calculate the default grid parameters. */
   smf_cubegrid( igrp,  size, system, usedetpos, autogrid, par, &moving, 
                 &oskyfrm, status );

/* Get the reference position strings. Use a temprary SkyFrame to
   unformat them into radians. */
   if( *status == SAI__OK ) {
      parDef0c( "REFLON", astFormat( oskyfrm, 1, par[ 2 ] ), status );
      parDef0c( "REFLAT", astFormat( oskyfrm, 2, par[ 2 ] ), status );

      parGet0c( "REFLON", reflon, 40, status );
      parGet0c( "REFLAT", reflat, 40, status );

      if( *status = SAI__OK ) {
         if( astUnformat( oskyfrm, 1, reflon, par + 2 ) == 0 && *status == SAI__OK ) {
            msgSetc( "REFLON", reflon );
            errRep( "", "Bad value supplied for REFLON: '^REFLON'", status );
         }
      
         if( astUnformat( oskyfrm, 2, reflat, par + 3 ) == 0 && *status == SAI__OK ) {
            msgSetc( "REFLAT", reflat );
            errRep( "", "Bad value supplied for REFLAT: '^REFLAT'", status );
         }  
      }
   }
   
/* Get the user defined spatial pixel size in arcsec (the calibration for 
   the spectral axis is fixed by the first input data file - see 
   smf_cubebounds.c). First convert the autogrid values form rads to arcsec
   and establish them as the dynamic default for "PIXSIZE". */
   pixsize[ 0 ] = 0.1*NINT( fabs( par[ 4 ] )*AST__DR2D*36000.0 );
   pixsize[ 1 ] = 0.1*NINT( fabs( par[ 5 ] )*AST__DR2D*36000.0 );

   parDef1d( "PIXSIZE", ( pixsize[ 0 ] == pixsize[ 1 ] ) ? 1 : 2, pixsize,
             status );
   parGet1d( "PIXSIZE", 2, pixsize, &nval, status );

/* If OK, duplicate the first value if only one value was supplied. */
   if( *status == SAI__OK ) {
      if( nval < 2 ) pixsize[ 1 ] = pixsize[ 0 ];
   
/* Check the values are OK. */
      if( pixsize[ 0 ] <= 0 || pixsize[ 1 ] <= 0 ) {
         msgSetd( "P1", pixsize[ 0 ] );
         msgSetd( "P2", pixsize[ 1 ] );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "Invalid pixel sizes (^P1,^P2).", status);
      }

/* Convert to rads, and set the correct signs. */
      par[ 4 ] = ( ( par[ 4 ] < 0.0 ) ? -1.0 : 1.0 )*pixsize[ 0 ]*AST__DD2R/3600.0;
      par[ 5 ] = ( ( par[ 5 ] < 0.0 ) ? -1.0 : 1.0 )*pixsize[ 1 ]*AST__DD2R/3600.0;
   }
   
/* Convert the autogrid CROTA value from rads to degs and set as the
   dynamic default for parameter CROTA (the position angle of the output 
   Y axis, in degrees). The get the CROTA value and convert to rads. */
   parDef0d( "CROTA", par[ 6 ]*AST__DR2D, status );
   parGet0d( "CROTA", par + 6, status );
   par[ 6 ] *= AST__DD2R;

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Display the projection parameters being used. */
   msgBlank( status );
   msgOutif( MSG__NORM, " ", "   Projection parameters used:", status );
   msgSetd( "V", par[ 0 ] );
   msgOutif( MSG__NORM, " ", "      CRPIX1 = ^V", status );
   msgSetd( "V", par[ 1 ] );
   msgOutif( MSG__NORM, " ", "      CRPIX2 = ^V", status );
   msgSetd( "V", par[ 2 ]*AST__DR2D );
   msgSetc( "V2", astFormat( oskyfrm, 1, par[ 2 ] ) );
   msgSetc( "S", astGetC( oskyfrm, "Symbol(1)" ) );
   msgOutif( MSG__NORM, " ", "      CRVAL1 = ^V ( ^S = ^V2 )", status );
   msgSetd( "V", par[ 3 ]*AST__DR2D );
   msgSetc( "V2", astFormat( oskyfrm, 2, par[ 3 ] ) );
   msgSetc( "S", astGetC( oskyfrm, "Symbol(2)" ) );
   msgOutif( MSG__NORM, " ", "      CRVAL2 = ^V ( ^S = ^V2 )", status );
   msgSetd( "V", par[ 4 ]*AST__DR2D );
   msgSetd( "V2", 0.1*NINT(par[ 4 ]*AST__DR2D*36000.0) );
   msgOutif( MSG__NORM, " ", "      CDELT1 = ^V ( ^V2 arcsec )", status );
   msgSetd( "V", par[ 5 ]*AST__DR2D );
   msgSetd( "V2", 0.1*NINT(par[ 5 ]*AST__DR2D*36000.0) );
   msgOutif( MSG__NORM, " ", "      CDELT2 = ^V ( ^V2 arcsec )", status );
   msgSetd( "V", par[ 6 ]*AST__DR2D );
   msgOutif( MSG__NORM, " ", "      CROTA2 = ^V", status );

/* Validate the input files, create the WCS FrameSet to store in the
   output cube, and get the pixel index bounds of the output cube. If
   projection parameters are being determined automatically, the relevant
   variables are returned holding the optimal projection parameter values. */
   smf_cubebounds( igrp, size, oskyfrm, autogrid, usedetpos, par, moving, 
                   lbnd_out, ubnd_out, &wcsout, status );

/* Get the base->current Mapping from the output WCS FrameSet, and split it 
   into two Mappings; one (oskymap) that maps the first 2 GRID axes into 
   celestial sky coordinates, and one (ospecmap) that maps the third GRID
   axis into a spectral coordinate. Also extract the SpecFrame and
   SkyFrame from the current Frame. */
   tmap = astGetMapping( wcsout, AST__BASE, AST__CURRENT );
   tfrm = astGetFrame( wcsout, AST__CURRENT );

   axes[ 0 ] = 1;
   axes[ 1 ] = 2;
   astMapSplit( tmap, 2, axes, outax, &oskymap );
   oskyfrm = astPickAxes( tfrm, 2, outax, NULL );

   axes[ 0 ] = 3;
   astMapSplit( tmap, 1, axes, outax, &ospecmap );
   ospecfrm = astPickAxes( tfrm, 1, outax, NULL );

/* Invert the spectral Mapping (for the convenience of smf_rebincube), so that
   it goes go from current Frame to output grid axis. */
   astInvert( ospecmap );

/* Create a FrameSet describing the spatial axes, and invert it. */
   swcsout = astFrameSet( astFrame( 2, "Domain=GRID", "" ), "" );
   astAddFrame( swcsout, AST__BASE, oskymap, oskyfrm );
   astInvert( swcsout );

/* Create the output NDF. */
   ndgCreat ( "OUT", NULL, &ogrp, &outsize, &flag, status );
   smfflags = 0;
   smfflags |= SMF__MAP_VAR;
   smf_open_newfile( ogrp, 1, SMF__FLOAT, 3, lbnd_out, ubnd_out, smfflags, 
                     &odata, status );

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Save some useful pointers. */
   file = odata->file;
   ondf = file->ndfid;

/* Create a history component in the output NDF. */
   ndfHcre( ondf, status );

/* Copy the Label and Unit strings from the first input NDF, and check
   that all input NDFs have the same Label and Unit strings. */
   smf_labelunit( igrp, size, odata, status );

/* Get pointers to the mapped output data and variance. */
   data_array = (odata->pntr)[ 0 ];
   var_array = (odata->pntr)[ 1 ];

/* Create an array to hold the weights. This has to be twice the size
   of the output cube if astRebinSeq is being used within smf_rebincube. 
   First set up the bounds of the array (a larger array is needed if AST
   is being used to do the rebinning), and then see if the array should 
   be held in temporary work space, or in an extension of the output NDF. */
   
   lbnd_wgt[ 0 ] = 1;
   lbnd_wgt[ 1 ] = 1;
   lbnd_wgt[ 2 ] = 1;
   ubnd_wgt[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1;
   ubnd_wgt[ 1 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1;
   ubnd_wgt[ 2 ] = ubnd_out[ 2 ] - lbnd_out[ 2 ] + 1;
   if( spread != AST__NEAREST ) ubnd_wgt[ 0 ] *= 2;

   parGet0l( "SAVEWEIGHTS", &savewgt, status );

/* Create the extension, or allocate the work space, as required. */
   if( savewgt ) {
      weightsloc = smf_get_xloc ( odata, "ACSISRED", "WT_ARR", "WRITE", 
                                  0, 0, status );
      smf_open_ndfname ( weightsloc, "WRITE", NULL, "WEIGHTS", "NEW", "_DOUBLE",
                      3, (int *) lbnd_wgt, (int *) ubnd_wgt, &wdata, status );
      if( wdata ) wgt_array = (wdata->pntr)[ 0 ];

   } else {
      wgt_array = astMalloc( sizeof( double )*
                          (size_t)ubnd_wgt[ 0 ]*ubnd_wgt[ 1 ]*ubnd_wgt[ 2 ] );
   }

/* Loop round all the input files, pasting each one into the output NDF. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( igrp, ifile, "READ", 1, &data, status );

/* Issue a suitable message and abort if anything went wrong. */
      if( *status != SAI__OK ) {
         errRep( FUNC_NAME, "Could not open input data file.", status );
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

/* Report the name of the input file. */
      pname =  data->file->name;
      msgSetc( "FILE", pname );
      msgSeti( "THISFILE", ifile );
      msgSeti( "NUMFILES", size );
      msgOutif( MSG__VERB, " ", 
                "SMURF_MAKECUBE: Processing ^THISFILE/^NUMFILES ^FILE",
                status );

/* Check that the input data type is single precision. */
      if( data->dtype != SMF__FLOAT ) {
         if( *status == SAI__OK ) {
            msgSetc( "FILE", pname );
            msgSetc( "DTYPE", smf_dtype_string( data, status ) );
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "^FILE has ^DTYPE data type, should "
                    "be REAL.",  status );
         }
         break;
      }     

/* If the detector positions are to calculated on the basis of FPLANEX/Y
   rather than detpos, then free the detpos array in the smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
      if( !usedetpos && data->hdr->detpos ) {
         smf_free( (double *) data->hdr->detpos, status );      
         data->hdr->detpos = NULL;
      }

/* Rebin the data into the output grid. */
      smf_rebincube( data, ifile, size, swcsout, ospecfrm, ospecmap, detgrp, 
                     moving, lbnd_out, ubnd_out, spread, params, data_array, 
                     var_array, wgt_array, status );
   
/* Close the input data file. */
      if( data != NULL ) {
	smf_close_file( &data, status );
	data = NULL;
      }
   }

L999:;

/* Close the input data file that remains open due to an early exit from
   the above loop. */
   if( data != NULL ) {
      smf_close_file( &data, status );
      data = NULL;
   }

/* Store the WCS FrameSet in the output NDF. */
   if( wcsout ) ndfPtwcs( wcsout, ondf, status );
  
/* Close the output data files. */
   if( wdata ) smf_close_file( &wdata, status );
   if( odata ) smf_close_file( &odata, status );

/* Free resources. */  
   if( detgrp != NULL) grpDelet( &detgrp, status);
   if( igrp != NULL) grpDelet( &igrp, status);
   if( ogrp != NULL) grpDelet( &ogrp, status);
   if( !savewgt ) wgt_array = astFree( wgt_array );

/* End the NDF context. */
   ndfEnd( status );

/* Issue a status indication.*/  
   if( *status == SAI__OK ) {
      msgOutif(MSG__VERB," ",TASK_NAME " succeeded, cube written.", status);
   } else {
      msgOutif(MSG__VERB," ",TASK_NAME " failed.", status);
   }
}
