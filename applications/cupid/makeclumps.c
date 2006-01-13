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

void makeclumps() {
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

*  Synopsis:
*     void makeclumps();

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
*     BACK = _REAL (Read)
*        The constant background level. [current value]
*     BEAMFWHM = _REAL (Read)
*        The spatial FHWM (Full Width at Half Max) of the instrumental beam, 
*        in pixels. The generated clumps are smoothed with a Gaussian beam
*        of this FWHM, before noise is added. No spatial smoothing is 
*        performed if BEAMFWHM is zero. [current value]
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
*        The number fo clumps to create.
*     OUT = NDF (Write)
*        The NDF to receive the simulated data, including instrumental
*        blurring and noise.
*     MODEL = NDF (Write)
*        The NDF to receive the simulated data, excluding noise. A CUPID
*        extension is added to this NDF, containing information about each 
*        clump in the same format as produced by the CLUMPS command. This 
*        includes an NDF holding an of the individual clump.
*     OUTCAT = FILENAME (Write)
*        The output catalogue in which to store the clump parameters.
*        There will be one row per clump, and a subset of the following 
*        columns will be included, depending on the number of pixel axes
*        in the output array:
*
*           - Peak intensity of the clump, above the background level.
*           See parameter PEAK.
*           - The pixel coordinate of the clump centre on axis 1.
*           - The intrinsic FWHM of the clump on axis 1, measured in
*           pixels. This excludes any instrumental blurring specified by 
*           BEAMFWHM or VELFWHM. See parameter FWHM1.
*           - The pixel coordinate of the clump centre on axis 2.
*           - The intrinsic FWHM of the clump on axis 2, measured in
*           pixels. This excludes any instrumental blurring specified by 
*           BEAMFWHM. See parameter FWHM2.
*           - The spatial position angle of the major axis of the clump,
*           measured in degrees, positive from the first pixel axis to the 
*           second pixel axis. See parameter ANGLE.
*           - The pixel coordinate of the clump centre on axis 3.
*           - The intrinsic FWHM of the clump on axis 3, measured in
*           pixels. This excludes any instrumental blurring specified by 
*           VELFWHM. See parameter FWHM3.
*           - The projection of the internal velocity gradient vector
*           onto pixel axis 1, measured in dimensionless units of
*           "velocity pixels per spatial pixel". See parameter VGRAD1.
*           - The projection of the internal velocity gradient vector
*           onto pixel axis 2, measured in dimensionless units of
*           "velocity pixels per spatial pixel". See parameter VGRAD2.
*
*        The output catalogue will contain the first three of these columns
*        if the output array is 1-dimensional, the first six if it is
*        2-dimensional, and all of them if it is 3-dimensional.
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

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     31-OCT-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/      

/* Local Variables: */
   HDSLoc *xloc;                 /* HDS locator for CUPID extension */
   char text[ 8 ];               /* Value of PARDIST parameter */
   double back;                  /* Background level */
   double par[ 11 ];             /* Clump parameters */
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
   int *clist;                   /* Pointer to list of NDF identifiers */
   int addnoise;                 /* Add Gaussian noise to output array? */
   int dims[ 3 ];                /* Dimensions before axis permutation */
   int i;                        /* Loop count */
   int indf2;                    /* Identifier for output NDF without noise */
   int indf;                     /* Identifier for output NDF with noise */
   int lbnd[ 3 ];                /* Lower pixel bounds */
   int nclump;                   /* Number of clumps to create */
   int ndim;                     /* Number of pixel axes */
   int nel;                      /* Number of elements in array */
   int normal;                   /* Clump parameters normally distributed? */
   int nval;                     /* Number of values supplied */
   int ubnd[ 3 ];                /* Upper pixel bounds */

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
   parGet0d( "BACK", &back, status );
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
   clist = NULL;
   maxpeak = 1.0;
   for( i = 0; i < nclump && *status == SAI__OK; i++ ) {

/* Determine the parameter values to use for the clump. */
      par[ 0 ] = cupidRanVal( normal, peak );
      par[ 1 ] = back;
      par[ 2 ] = (int) cupidRanVal( 0, pos1 );
      par[ 3 ] = cupidRanVal( normal, fwhm1 );

      if( ndim > 1 ) {
         par[ 4 ] = (int) cupidRanVal( 0, pos2 );
         par[ 5 ] = cupidRanVal( normal, fwhm2 );
         par[ 6 ] = cupidRanVal( 0, angle );

         if( ndim > 2 ) {
            par[ 7 ] = cupidRanVal( 0, pos3 );
            par[ 8 ] = cupidRanVal( normal, fwhm3 );
            par[ 9 ] = cupidRanVal( normal, vgrad1 );
            par[ 10 ] = cupidRanVal( normal, vgrad2 );
         }
      }

/* Extend the array of Clump NDFs to include room for the new one. */
      clist = astGrow( clist, i + 1, sizeof( int ) );

/* Add the clump into the output array. This also creates a "Clump" NDF
   containing the clump data values. An NDF identifier for this new NDF 
   is added into the "clist" array. */
      cupidGCUpdateArraysF( NULL, NULL, nel, ndim, dims, par, rms, trunc, 0,
                            0, lbnd, clist + i, i, 0, 0.0 );

/* Update the largest peak value. */
      if( par[ 0 ] > maxpeak ) maxpeak = par[ 0 ];

   }

/* Create the output data array by summing the contents of the NDFs
   describing the found clumps. */
   cupidSumClumps( CUPID__FLOAT, NULL, 0, ndim, lbnd, ubnd, nel, clist, nclump,
                   NULL, ipd2, "GAUSSCLUMPS", &back );

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

/* Store the clump properties in the output catalogue. */
   cupidStoreClumps( "OUTCAT", xloc, clist, nclump, ndim, 
                     "Output from CUPID:MAKECLUMPS" );

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

