#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/hds.h"
#include "star/kaplibs.h"
#include "ast.h"
#include "par.h"
#include "star/pda.h"
#include "cupid.h"
#include <math.h>
#include <string.h>
#include <stdio.h>

/* Local Constants: */
/* ================ */
#define DTOR  0.017453292519943295769237

/* Global Variables: */
/* ================= */
/* A structure holding the global parameters needed by the function
   cupidGCmodel. Is is declared in cupidGaussClumps. */
extern CupidGC cupidGC;

void makeclumps( int *status ) {
/*
*+
*  Name:
*     MAKECLUMPS

*  Purpose:
*     Create simulated data containing clumps and noise.

*  Language:
*     C

*  Type of Module:
*     ADAM A-task

*  Description:
*     This application creates a new 1-, 2- or 3-dimensional NDF containing 
*     a collection of clumps with background noise. It also creates a
*     catalogue containing the clump parameters.
*
*     The clumps profiles are Gaussian, with elliptical isophotes. The
*     values of each parameter defining the clump shape can be either
*     fixed at a constant value or selected from a given probability
*     distribution.

*  Usage:
*     makeclumps out outcat 

*  ADAM Parameters:
*     ANGLE( 2 ) = _REAL (Read)
*        Defines the distribution from which the spatial position angle of 
*        the major axis of the elliptical clump is chosen. Values should
*        be supplied in units of degrees. See parameter  PARDIST for 
*        additional information. Note, angles are always taken from a
*        uniform distribution, irrespective of the setting of PARDIST. 
*        [current value]
*     BEAMFWHM = _REAL (Read)
*        The spatial FHWM (Full Width at Half Max) of the instrumental beam, 
*        in pixels. The generated clumps are smoothed with a Gaussian beam
*        of this FWHM, before noise is added. No spatial smoothing is 
*        performed if BEAMFWHM is zero. [current value]
*     DECONV = _LOGICAL (Read)
*        If TRUE, the clump properties stored in the output catalogue
*        will be modified to take account of the smoothing caused by the
*        instrumental beam width. [TRUE]
*     FWHM1( 2 ) = _REAL (Read)
*        Defines the distribution from which the FWHM (Full Width at Half
*        Max) for pixel axis 1 of each clump is chosen. Values should be
*        supplied in units of pixel. See parameter PARDIST for additional 
*        information. [current value]
*     FWHM2( 2 ) = _REAL (Read)
*        Defines the distribution from which the FWHM (Full Width at Half
*        Max) for pixel axis 2 of each clump is chosen. Values should be
*        supplied in units of pixel. See parameter PARDIST for additional 
*        information. [current value]
*     FWHM3( 2 ) = _REAL (Read)
*        Defines the distribution from which the FWHM (Full Width at Half
*        Max) for pixel axis 3 of each clump is chosen. Values should be
*        supplied in units of pixel. See parameter PARDIST for additional 
*        information. [current value]
*     LBND() = _INTEGER (Read)
*        The lower pixel bounds of the output NDF. The number of values
*        supplied (1, 2 or 3) defines the number of pixel axes in the output 
*        NDF (an error is reported if the number of values supplied for LBND 
*        and UBND differ).
*     NCLUMP = _INTEGER (Read)
*        The number of clumps to create.
*     OUT = NDF (Write)
*        The NDF to receive the simulated data, including instrumental
*        blurring and noise.
*     MODEL = NDF (Write)
*        The NDF to receive the simulated data, excluding noise. A CUPID
*        extension is added to this NDF, containing information about each 
*        clump in the same format as produced by the FINDCLUMPS command. This 
*        includes an NDF holding an of the individual clump.
*     OUTCAT = FILENAME (Write)
*        The output catalogue in which to store the clump parameters.
*        There will be one row per clump, with the following columns:
*
*        - PeakX: The PIXEL X coordinates of the clump peak value.
*        - PeakY: The PIXEL Y coordinates of the clump peak value.
*        - PeakZ: The PIXEL Z coordinates of the clump peak value.
*        - CenX: The PIXEL X coordinates of the clump centroid.
*        - CenY: The PIXEL Y coordinates of the clump centroid.
*        - CenZ: The PIXEL Z coordinates of the clump centroid.
*        - SizeX: The size of the clump along the X axis, in pixels.
*        - SizeY: The size of the clump along the Y axis, in pixels.
*        - SizeZ: The size of the clump along the Z axis, in pixels.
*        - Sum: The total data sum in the clump.
*        - Peak: The peak value in the clump.
*        - Area: The total number of pixels falling within the clump.
*
*        If the data has less than 3 pixel axes, then the columns
*        describing the missing axes will not be present in the catalogue.
*
*        The "size" of the clump on an axis is the RMS deviation of each 
*        pixel centre from the clump centroid, where each pixel is
*        weighted by the correspinding pixel data value. This excludes
*        the instrumental smoothing specified by BEAMFWHM and VELFWHM.
*     PARDIST = LITERAL (Read)
*        The shape of the distribution from which clump parameter values are 
*        chosen. Can be "Normal" or "Uniform". The distribution for each
*        clump parameter is specified by itw own ADAM parameter containing 
*        two values; the mean and the width of the distribution. If PARDIST 
*        is "Normal", the width is the standard deviation. If PARDIST is 
*        "Uniform", the width is half the range between the maximum and 
*        minimum parameter values. In either case, if a width of zero is
*        supplied, the relevant parameter is given a constant value equal
*        to the specified mean. [current value]
*     PEAK( 2 ) = _REAL (Read)
*        Defines the distribution from which the peak value (above the
*        local background) of each clump is chosen. See parameter PARDIST 
*        for additional information. [current value]
*     RMS = _REAL (Read)
*        The RMS (Gaussian) noise to be added to the output data. [current value]
*     TRUNC = _REAL (Read)
*        The level (above the local background) at which clumps should be 
*        truncated to zero, given as a fraction of the noise level specified 
*        by RMS. [current value]
*     UBND() = _INTEGER (Read)
*        The upper pixel bounds of the output NDF. The number of values
*        supplied (1, 2 or 3) defines the number of pixel axes in the output 
*        NDF (an error is reported if the number of values supplied for LBND 
*        and UBND differ).
*     VELFWHM = _REAL (Read)
*        The FWHM of the Gaussian velocity resolution of the instrument, in 
*        pixels. The generated clumps are smoothed on the velocity axis with 
*        a Gaussian beam of this FWHM, before noise is added. No velocity
*        smoothing is performed if VELFWHM is zero. [current value]
*     VGRAD1( 2 ) = _REAL (Read)
*        Defines the distribution from which the projection of the internal 
*        velocity gradient vector onto pixel axis 1 of each clump is chosen. 
*        Values should be supplied in dimensionless units of "velocity
*        pixels per spatial pixel". See parameter PARDIST for additional 
*        information. [current value]
*     VGRAD2( 2 ) = _REAL (Read)
*        Defines the distribution from which the projection of the internal 
*        velocity gradient vector onto pixel axis 2 of each clump is chosen. 
*        Values should be supplied in dimensionless units of "velocity
*        pixels per spatial pixel". See parameter PARDIST for additional 
*        information. [current value]

*  Notes:
*     - If 3D data is created, pixel axes 1 and 2 are the spatial axes,
*     and pixel axis 3 is the velocity axis.
*     - The positions of the clumps are chosen from a uniform
*     distribution on each axis.

*  Synopsis:
*     void makeclumps( int *status );

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     31-OCT-2005 (DSB):
*        Original version.
*     11-DEC-2006 (DSB):
*        Added parameter DECONV.
*     6-NOV-2007 (DSB):
*        "int" and "size_t" have different sizes on 64 bit machines, so
*         do not cast the address of an int to the address of a size_t.
*     15-JAN-2009 (DSB):
*        Remove ILEVEL arguments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/      

/* Local Variables: */
   HDSLoc *obj;                  /* HDS array of NDF structures */
   HDSLoc *xloc;                 /* HDS locator for CUPID extension */
   char text[ 8 ];               /* Value of PARDIST parameter */
   double beamcorr[ 3 ];         /* Beam widths */
   double par[ 11 ];             /* Clump parameters */
   double sum;                   /* Integrated intensity */
   float *d;                     /* Pointer to next output element */
   float *ipd2;                  /* Pointer to data array */
   float *ipd;                   /* Pointer to data array */
   float angle[ 2 ];             /* Values for ANGLE parameter */
   float beamfwhm;               /* Value of BEAMFWHM parameter */
   float fwhm1[ 2 ];             /* Values for FWHM1 parameter */
   float fwhm2[ 2 ];             /* Values for FWHM2 parameter */
   float fwhm3[ 2 ];             /* Values for FWHM3 parameter */
   float maxpeak;                /* Largest normalised peak value used */
   float peak[ 2 ];              /* Values for PEAK parameter */
   float pos1[ 2 ];              /* Distribution values for axis 1 position */
   float pos2[ 2 ];              /* Distribution values for axis 2 position */
   float pos3[ 2 ];              /* Distribution values for axis 3 position */
   float rms;                    /* Standard deviation of Gaussian noise */
   float trunc;                  /* Min usable clump value */
   float velfwhm;                /* Value of VELFWHM parameter */
   float vgrad1[ 2 ];            /* Values for VGRAD1 parameter */
   float vgrad2[ 2 ];            /* Values for VGRAD2 parameter */
   int addnoise;                 /* Add Gaussian noise to output array? */
   int area;                     /* Clump area */
   int deconv;                   /* Store deconvolved clump properties */
   int dims[ 3 ];                /* Dimensions before axis permutation */
   int i;                        /* Loop count */
   int indf2;                    /* Identifier for output NDF without noise */
   int indf;                     /* Identifier for output NDF with noise */
   int lbnd[ 3 ];                /* Lower pixel bounds */
   int nc;                       /* Number of clumps created */
   int nclump;                   /* Number of clumps to create */
   int nclumps;                  /* Number of stored clumps */
   int ndim;                     /* Number of pixel axes */
   int nel;                      /* Number of elements in array */
   int normal;                   /* Clump parameters normally distributed? */
   int nval;                     /* Number of values supplied */
   int ubnd[ 3 ];                /* Upper pixel bounds */
   size_t st;                    /* A size_t that can be passed as an argument */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Start an AST context */
   astBegin;

/* Start an NDF context */
   ndfBegin();

/* INitialise pointers. */
   xloc = NULL;

/* Get the required axis bounds. */
   parGet1i( "LBND", 3, lbnd, &ndim, status );
   parExaci( "UBND", ndim, ubnd, status );

/* Create the output NDFs. */
   ndfCreat( "OUT", "_REAL", ndim, lbnd, ubnd, &indf, status );
   ndfCreat( "MODEL", "_REAL", ndim, lbnd, ubnd, &indf2, status );

/* Map the DATA component of the output NDFs. */
   ndfMap( indf, "DATA", "_REAL", "WRITE", (void *) &ipd, &nel, status );
   ndfMap( indf2, "DATA", "_REAL", "WRITE", (void *) &ipd2, &nel, status );

/* Get the other needed parameters. Which are needed depends on whether
   we are producing 1, 2 or 3 dimensional data. */
   
   parChoic( "PARDIST", "NORMAL", "NORMAL,UNIFORM", 1, text, 8, status );
   normal = strcmp( text, "NORMAL" ) ? 0 : 1;

   parGet0r( "TRUNC", &trunc, status );
   parGet0i( "NCLUMP", &nclump, status );
   parGet0r( "RMS", &rms, status );

   parGet1r( "FWHM1", 2, fwhm1, &nval, status );
   if( nval == 1 ) fwhm1[ 1 ] = 0.0;

   parGet1r( "PEAK", 2, peak, &nval, status );
   if( nval == 1 ) peak[ 1 ] = 0.0;

   if( ndim == 1 ) {
      parGet0r( "VELFWHM", &velfwhm, status );
      cupidGC.velres_sq = velfwhm*velfwhm;

   } else {

      parGet1r( "ANGLE", 2, angle, &nval, status );
      if( nval == 1 ) angle[ 1 ] = 0.0;
      angle[ 0 ] *= DTOR;
      angle[ 1 ] *= DTOR;

      parGet1r( "FWHM2", 2, fwhm2, &nval, status );
      if( nval == 1 ) fwhm2[ 1 ] = 0.0;
      parGet0r( "BEAMFWHM", &beamfwhm, status );
      cupidGC.beam_sq = beamfwhm*beamfwhm;

      if( ndim == 3 ) {

         parGet1r( "FWHM3", 2, fwhm3, &nval, status );
         if( nval == 1 ) fwhm3[ 1 ] = 0.0;

         parGet0r( "VELFWHM", &velfwhm, status );
         cupidGC.velres_sq = velfwhm*velfwhm;

         parGet1r( "VGRAD1", 2, vgrad1, &nval, status );
         if( nval == 1 ) vgrad1[ 1 ] = 0.0;

         parGet1r( "VGRAD2", 2, vgrad2, &nval, status );
         if( nval == 1 ) vgrad2[ 1 ] = 0.0;

      }
   }

/* There is no axis permutation (i.e. axis 3 will be the velocity axis). */
   cupidGC.dax[ 0 ] = 0;
   cupidGC.dax[ 1 ] = 1;
   cupidGC.dax[ 2 ] = 2;

/* Normalise peak value to the RMS noise level. */
   addnoise = ( rms != 0.0 );
   if( !addnoise ) rms = 1.0;
   peak[ 0 ] /= rms;
   peak[ 1 ] /= rms;

/* Clump positions are chosen from a uniform distribution. Set up the
   arrays describing the mean value and range on each axis. */
   dims[ 0 ] = ubnd[ 0 ] - lbnd[ 0 ] + 1;
   dims[ 1 ] = 1;
   dims[ 2 ] = 1;
   pos1[ 0 ] = 0.5*( dims[ 0 ]  + 1 );
   pos1[ 1 ] = 0.5*dims[ 0 ];

   if( ndim > 1 ) {

      dims[ 1 ] = ubnd[ 1 ] - lbnd[ 1 ] + 1;
      pos2[ 0 ] = 0.5*( dims[ 1 ]  + 1 );
      pos2[ 1 ] = 0.5*dims[ 1 ];

      if( ndim > 2 ) {
         dims[ 2 ] = ubnd[ 2 ] - lbnd[ 2 ] + 1;
         pos3[ 0 ] = 0.5*( dims[ 2 ]  + 1 );
         pos3[ 1 ] = 0.5*dims[ 2 ];
      }
   }

/* Set the PDA random number seed to a non-repeatable value */
   kpg1Pseed( status );

/* Loop round creating clumps. */
   obj = NULL;
   maxpeak = 1.0;
   sum = 0.0;
   nc = 0;
   i = 0;
   while( nc < nclump && *status == SAI__OK) {
      if( i++ == 100*nclump ) {
         if( nc == 0 ) {
            *status = SAI__ERROR;
            errRep( "", "Cannot create any clumps - they all touch an "
                    "edge of the output array", status );

         } else if( nc == 1 ) {
            msgOut( "", "Can only create one clump - all others touch an "
                    "edge of the output array.", status );

         } else {
            msgSeti( "N", nc );
            msgOut( "", "Can only create ^N clumps - all others touch an "
                    "edge of the output array.", status );
         }
         break;
      }

/* Determine the parameter values to use for the clump. */
      par[ 0 ] = cupidRanVal( normal, peak, status );
      par[ 1 ] = 0.0;
      par[ 2 ] = (int) cupidRanVal( 0, pos1, status );
      par[ 3 ] = cupidRanVal( normal, fwhm1, status );

      if( ndim > 1 ) {
         par[ 4 ] = (int) cupidRanVal( 0, pos2, status );
         par[ 5 ] = cupidRanVal( normal, fwhm2, status );
         par[ 6 ] = cupidRanVal( 0, angle, status );

         if( ndim > 2 ) {
            par[ 7 ] = cupidRanVal( 0, pos3, status );
            par[ 8 ] = cupidRanVal( normal, fwhm3, status );
            par[ 9 ] = cupidRanVal( normal, vgrad1, status );
            par[ 10 ] = cupidRanVal( normal, vgrad2, status );
         }
      }

/* Add the clump into the output array. This also creates a "Clump" NDF
   containing the clump data values, appended to the end of the array of
   NDF structures in the HDS object located by "obj". */
      cupidGCUpdateArraysF( NULL, NULL, nel, ndim, dims, par, rms, trunc, 0,
                            lbnd, &obj, i, 0, 0.0, 0, &area, &sum, status );

/* Update the largest peak value. */
      if( par[ 0 ] > maxpeak ) maxpeak = par[ 0 ];

/* See how many clumps we now have (no NDF will have been created for
   this clump if it touches an edge of eh output array). */
      if( obj ) {
         datSize( obj, &st, status );
         nc = (int) st;
      }
      
   }

/* Create the output data array by summing the contents of the NDFs
   describing the found clumps. */
   cupidSumClumps( CUPID__FLOAT, NULL, ndim, lbnd, ubnd, nel, obj, 
                   NULL, ipd2, "GAUSSCLUMPS", status );

/* Add Gaussian noise to the data. */
   if( *status == SAI__OK ) {
      memcpy( ipd, ipd2,sizeof( float )*nel );
      if( addnoise ) {
         d = ipd;
         for( i = 0; i < nel; i++, d++ ) {
            *d +=  pdaRnnor( 0.0, rms );
         }
      }
   }

/* Create a CUPID extension in the output model NDF.*/
   ndfXnew( indf2, "CUPID", "CUPID_EXT", 0, NULL, &xloc, status );

/* See if deconvolved values are to be stored in the output catalogue. */
   parGet0l( "DECONV", &deconv, status );

/* Store the clump properties in the output catalogue. */
   beamcorr[ 0 ] = beamfwhm;
   beamcorr[ 1 ] = beamfwhm;
   beamcorr[ 3 ] = velfwhm;
   cupidStoreClumps( "OUTCAT", xloc, obj, ndim, deconv, 1, beamcorr,
                     "Output from CUPID:MAKECLUMPS", 0, NULL, "",
                     NULL, NULL, &nclumps, status );

/* Relase the extension locator.*/
   datAnnul( &xloc, status );

/* End the NDF context */
   ndfEnd( status );

/* End the AST context */
   astEnd;

/* If an error has occurred, issue another error report identifying the 
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "MAKECLUMPS_ERR", "MAKECLUMPS: Failed to create artifical "
              "clump data.", status );
   }
}
