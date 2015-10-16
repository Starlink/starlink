/*
*+
*  Name:
*     smf_get_projpar

*  Purpose:
*     Modify the map projection using user-specified ADAM parameters

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_get_projpar( AstSkyFrame *skyframe, const double skyref[2],
*                      int moving, int autogrid, int nallpos,
*                      const double allpos[], float telres, double map_pa,
*                      double par[7], int * issparse,int *usedefs, int *status )

*  Arguments:
*     skyframe = AstFrameSet * (Given)
*        Pointer to an AST SkyFrame describing the spatial axes of the
*        output WCS FrameSet.
*     skyref = const double[2] (Given)
*        Coordinates of reference position corresponding to reference pixel.
*     moving = int (Given)
*        Indicate whether the coordinate frame is moving (and so should align
*        in the offset coordinate frame)
*     autogrid = int (Given)
*        Should calculate the projection parameters from the supplied positions
*        automatically. Will be set to false automatically if nallpos is zero
*        or allpos is NULL.
*     nallpos = int (Given)
*        Number of positions stored in allpos. Only used if autogrid is true.
*     allpos = const double[] (Given)
*        Coordinates of each position to be used for autogrid determination.
*        If NULL, autogrid is disabled. Coordinates are stored in pairs.
*     telres = float (Given)
*        Telescope resolution in radians. Only used for autogrid to determine
*        the minimum pixel size.
*     map_pa = double (Given)
*        Map position angle in output coordinate system (radians).
*        Used as dynamic default of CROTA parameter.
*     par = double[ 7 ] (Given and Returned)
*        An array holding the parameters describing the spatial
*        projection between celestial (longitude,latitude) in the
*        system specified by "system", and an interim GRID coordinate
*        system for the output cube (interim because the bounds of the
*        output map are not yet known - the interim grid system is
*        thus more like a PIXEL system for the output cube but with an
*        arbitrary pixel origin). These are stored in the order
*        CRPIX1, CRPIX2, CRVAL1, CRVAL2, CDELT1, CDELT2, CROTA2. The
*        supplied values are used to produce the output WCS
*        FrameSet. All the angular parameters are in units of radians,
*        and CRPIX1/2 are in units of pixels.
*     issparse = int * (Returned)
*        If non-NULL, indicates whether a sparse grid was calculated.
*     usedefs = int * (Returned)
*        If usedefs=1, using input rather than user-defined parameters.
*        Can be NULL, in which case the caller does not care.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function updates the par array which contains a description of
*     the map projection. The array is updated using user-defined
*     ADAM parameters. If no previous values have been specified for
*     par, each element should be initialized to AST__BAD.

*  Authors:
*     DSB: David Berry (JAC, UCLan)
*     EC: Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-11-14 (DSB):
*        Original smf_cubegrid implementation.
*     2008-05-12 (EC):
*        Initial version factored out of smf_cubegrid
*     2008-06-04 (TIMJ):
*        Integrate more of smf_cubegrid to allow more code reuse even though
*        SCUBA-2 does not use autogrid facilities.
*     2008-06-04 (DSB):
*        Correct signs of CDELT values when autogrid algorithm fails.
*     2009-10-29 (TIMJ):
*        If par[4] and par[5] are filled in then fallback to those values
*        rather than filling in with 6 arcsec.
*     2011-03-26 (DSB):
*        Allow pixel ref position (par[0] and par[1]) to be supplied via
*        adam parameters REFPIX1 and REFPIX2, and write out the used
*        pixel ref position to output parameter PIXREF.
*     2011-05-10 (DSB):
*        If REFPIX1 or REFPIX2 is null, ensure both par[0] and par[1]
*        retain their original values.
*     2011-12-08 (TIMJ):
*        Increase number of digits in DEFLON/DEFLAT string
*     2012-05-28 (DSB):
*        If the user supplies a new celestial reference position, then store
*        it in the skyframe.
*     2015-10-16 (DSB):
*        Use smf_set_moving to assign attributes for a moving target,
*        rather than just setting SkyRefIs (smf_set_moving also sets
*        AlignOffset).
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008 University of British Columbia.
*     Copyright (C) 2008-2011 Science & Technology Facilities Council.
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

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "star/kaplibs.h"
#include "star/one.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_get_projpar"

void smf_get_projpar( AstSkyFrame *skyframe, const double skyref[2],
                      int moving, int autogrid, int nallpos,
                      const double * allpos, float telres, double map_pa,
                      double par[7], int * issparse,int *usedefs, int *status ) {

/* Local Variables */
   char reflat[ 41 ];    /* Reference latitude string */
   char reflon[ 41 ];    /* Reference longitude string */
   char usesys[ 41 ];    /* Output skyframe system */
   const char *deflat;   /* Default for REFLAT */
   const char *deflon;   /* Default for REFLON */
   const double fbpixsize = 6.0; /* Fallback pixel size if we have no other information */
   double autorot;       /* Autogrid default for CROTA parameter */
   double defsize[ 2 ];  /* Default pixel sizes in arc-seconds */
   double pixsize[ 2 ];  /* Pixel sizes in arc-seconds */
   double refpix[ 2 ];   /* New REFPIX values */
   double rdiam;         /* Diameter of bounding circle, in rads */
   int coin;             /* Are all points effectively co-incident? */
   int i;
   int nval;             /* Number of values supplied */
   int refine_crpix;     /* Should the pixel ref position be updated? */
   int sparse = 0;       /* Local definition of sparseness */
   int udefs = 0;        /* Flag for defaults used or not */

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* If the number of supplied positions is 0 or null pointer,
   disable autogrid */
   if( nallpos == 0 || !allpos ) autogrid = 0;

/* Get the output system */
   one_strlcpy( usesys, astGetC( skyframe, "SYSTEM"), sizeof(usesys),
                status );

/* Ensure the reference position in the returned SkyFrame is set to the
   first telescope base pointing position. */
   astSetD( skyframe, "SkyRef(1)", skyref[ 0 ] );
   astSetD( skyframe, "SkyRef(2)", skyref[ 1 ] );

/* If the target is moving, ensure the returned SkyFrame represents
   offsets from the first telescope base pointing position rather than
   absolute coords. */
   if( moving ) smf_set_moving( (AstFrame *) skyframe, NULL, status );

/* Set a flag indicating if all the points are co-incident. */
   coin = 0;

/* Set the sky axis values at the tangent point. If the target is moving,
   the tangent point is at (0,0) (i.e. it is at the origin of the offset
   coordinate system). If the target is not moving, the tangent point is
   at the position held in "skyref". */
   if( par ) {
      if( moving ){
         par[ 2 ] = 0.0;
         par[ 3 ] = 0.0;
      } else {
         par[ 2 ] = skyref[ 0 ];
         par[ 3 ] = skyref[ 1 ];
      }

/* If required, calculate the optimal projection parameters. If the target
   is moving, these refer to the offset coordinate system centred on the
   first time slice base pointing position, with north defined by the
   requested output coordinate system. The values found here are used as
   dynamic defaults for the environment parameter */
      if( autogrid ) {
         kpg1Opgrd( nallpos, allpos, strcmp( usesys, "AZEL" ), par, &rdiam,
                          status );

/* See if all the points are effectively co-incident (i.e. within an Airy
   disk). If so, we use default grid parameters that result in a grid of
   1x1 spatial pixels. The grid pixel sizes (par[4] and par[5]) are made
   larger than the area covered by the points in order to avoid points
   spanning two pixels. */
         if( rdiam < telres || nallpos < 3 ) {
            if( rdiam < 0.1*AST__DD2R/3600.0 ) rdiam = 0.1*AST__DD2R/3600.0;
            par[ 0 ] = 0.0;
            par[ 1 ] = 0.0;
            par[ 4 ] = -rdiam*4;
            par[ 5 ] = -par[ 4 ];
            par[ 6 ] = 0.0;

            coin = 1;

/* If the sky positions are not co-incident, and the automatic grid
   determination failed, we cannot use a grid, so warn the user. */
         } else if( par[ 0 ] == AST__BAD ) {
            msgOutif( MSG__NORM, " ", "   Automatic grid determination "
                           "failed: the detector samples do not form a "
                           "regular grid.", status );
         }
      }

/* If autogrid values were not found, use the following fixed default
   values. Do not override extenal defaults for pixel size. */
      if( !autogrid || ( autogrid && par[ 0 ] == AST__BAD ) ) {
         par[ 0 ] = 0.0;
         par[ 1 ] = 0.0;
         if (par[4] == AST__BAD || par[5] == AST__BAD ) {
           par[ 4 ] = (fbpixsize/3600.0)*AST__DD2R;
           par[ 5 ] = (fbpixsize/3600.0)*AST__DD2R;
         }
         par[ 6 ] = map_pa;
      }

/* Ensure the default pixel sizes have the correct signs. */
      if( par[ 4 ] != AST__BAD ) {
         if( !strcmp( usesys, "AZEL" ) ) {
            par[ 4 ] = fabs( par[ 4 ] );
         } else {
            par[ 4 ] = -fabs( par[ 4 ] );
         }
         par[ 5 ] = fabs( par[ 5 ] );
      }

/* See if the output cube is to include a spatial projection, or a sparse
   list of spectra. Disabled if the sparse pointer is NULL. */
      if (issparse) {
        parDef0l( "SPARSE", ( par[ 0 ] == AST__BAD ), status );
        parGet0l( "SPARSE",  &sparse, status );

      }

/* If we are producing an output cube with the XY plane being a spatial
   projection, then get the parameters describing the projection, using the
   defaults calculated above. */
      if( !sparse && *status == SAI__OK ) {
         const int ndigits = 8; /* Number of digits for deflat/deflon precision */

/* If the target is moving, display the tracking centre coordinates for
   the first time slice. */
         if( moving ) {
            astClear( skyframe, "SkyRefIs" );
            msgBlank( status );
            msgSetc( "S1", astGetC( skyframe, "Symbol(1)" ) );
            msgSetc( "S2", astGetC( skyframe, "Symbol(2)" ) );
            msgOutif( MSG__NORM, " ", "   Output sky coordinates are "
                           "(^S1,^S2) offsets from the (moving)", status );
            msgSetc( "S1", astGetC( skyframe, "Symbol(1)" ) );
            msgSetc( "S2", astGetC( skyframe, "Symbol(2)" ) );
            msgSetc( "SREF", astGetC( skyframe, "SkyRef" ) );
            msgOutif( MSG__NORM, " ", "   telescope base position, which "
                           "started at (^S1,^S2) = (^SREF).", status );
            astSet( skyframe, "SkyRefIs=Origin" );
         }

/* Set up a flag indicating that the default values calculated by autogrid
   are being used. */
         udefs = 1;

/* Ensure we have usable CRPIX1/2 values */
         if( par[ 0 ] == AST__BAD ) par[ 0 ] = 1.0;
         if( par[ 1 ] == AST__BAD ) par[ 1 ] = 1.0;

/* Get the crpix1/2 (in the interim GRID frame) to use. Note if the user
   specifies any values. These parameters have vpath=default (which is null)
   and ppath=dynamic. */
         refine_crpix = 0;
         parDef0d( "REFPIX1", par[ 0 ], status );
         parDef0d( "REFPIX2", par[ 1 ], status );
         if( *status == SAI__OK ) {
            parGet0d( "REFPIX1", refpix + 0, status );
            parGet0d( "REFPIX2", refpix + 1, status );
            if( *status == PAR__NULL ) {
               errAnnul( status );
               refine_crpix = 1;
            } else {
               par[ 0 ] = refpix[ 0 ];
               par[ 1 ] = refpix[ 1 ];
            }
         }

/* Get the sky coords reference position strings. Use the returned SkyFrame
   to format and unformat them. */
         if( par[ 2 ] != AST__BAD ) {
            int curdigits;
            curdigits = astGetI( skyframe, "digits(1)" );
            astSetI( skyframe, "digits(1)", ndigits );
            deflon = astFormat( skyframe, 1, par[ 2 ] );
            astSetI( skyframe, "digits(1)", curdigits );
            parDef0c( "REFLON", deflon, status );
         } else {
            deflon = NULL;
         }

         if( par[ 3 ] != AST__BAD ) {
            int curdigits;
            curdigits = astGetI( skyframe, "digits(2)" );
            astSetI( skyframe, "digits(2)", ndigits );
            deflat = astFormat( skyframe, 2, par[ 3 ] );
            astSetI( skyframe, "digits(2)", curdigits );
            parDef0c( "REFLAT", deflat, status );
         } else {
            deflat = NULL;
         }

         parGet0c( "REFLON", reflon, 40, status );
         parGet0c( "REFLAT", reflat, 40, status );

         if( *status == SAI__OK ) {

            if( ( deflat && strcmp( deflat, reflat ) ) ||
                  ( deflon && strcmp( deflon, reflon ) ) ) udefs = 0;

            if( astUnformat( skyframe, 1, reflon, par + 2 ) == 0 && *status == SAI__OK ) {
               msgSetc( "REFLON", reflon );
               errRep( "", "Bad value supplied for REFLON: '^REFLON'", status );
            }

            if( astUnformat( skyframe, 2, reflat, par + 3 ) == 0 && *status == SAI__OK ) {
               msgSetc( "REFLAT", reflat );
               errRep( "", "Bad value supplied for REFLAT: '^REFLAT'", status );
            }

/* Ensure the reference position in the returned SkyFrame is set to the
   supplied position (which defaults to the first telescope base pointing
   position). */
            if( !moving ){
               astSetD( skyframe, "SkyRef(1)", par[ 2 ] );
               astSetD( skyframe, "SkyRef(2)", par[ 3 ] );
            }
         }

/* Get the user defined spatial pixel size in arcsec (the calibration for
   the spectral axis is fixed by the first input data file - see
   smf_cubebounds.c). First convert the autogrid values form rads to arcsec
   and establish them as the dynamic default for "PIXSIZE". */
         nval = 0;
         if( par[ 4 ] != AST__BAD || par[ 5 ] != AST__BAD ) {
           for ( i = 4; i <= 5; i++ ) {
             if ( par[ i ] != AST__BAD ) {
               defsize[ nval ] = 0.1*NINT( fabs( par[ i ] )*AST__DR2D*36000.0 );
               nval++;
             }
           }
           /* set the dynamic default, handling case where both dimensions
              have same default. */
           if (nval == 1) {
             defsize[1] = defsize[0];
           } else if (nval == 2 && defsize[0] == defsize[1]) {
             nval = 1;
           }
           parDef1d( "PIXSIZE", nval, defsize, status );

         } else {
           /* pick a default in case something odd happens and we have
              no other values*/
           defsize[ 0 ] = fbpixsize;
           defsize[ 1 ] = defsize[ 0 ];
           nval = 2;
         }
         if (*status == SAI__OK) {
           pixsize[0] = AST__BAD;
           pixsize[1] = AST__BAD;
           parGet1d( "PIXSIZE", 2, pixsize, &nval, status );
           if (*status == PAR__NULL) {
             /* Null just defaults to what we had before */
             errAnnul( status );
             pixsize[0] = defsize[0];
             pixsize[1] = defsize[1];
             nval = 2;
           }
         }

/* If OK, duplicate the first value if only one value was supplied. */
         if( *status == SAI__OK ) {
            if( nval < 2 ) pixsize[ 1 ] = pixsize[ 0 ];

            if( defsize[ 0 ] != pixsize[ 0 ] ||
                  defsize[ 1 ] != pixsize[ 1 ] ) udefs = 0;

/* Check the values are OK. */
            if( pixsize[ 0 ] <= 0 || pixsize[ 1 ] <= 0 ) {
               msgSetd( "P1", pixsize[ 0 ] );
               msgSetd( "P2", pixsize[ 1 ] );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "Invalid pixel sizes (^P1,^P2).", status);
            }

/* Convert to rads, and set the correct signs. */
            if( par[ 4 ] == AST__BAD || par[ 4 ] < 0.0 ) {
               par[ 4 ] = -pixsize[ 0 ]*AST__DD2R/3600.0;
            } else {
               par[ 4 ] = pixsize[ 0 ]*AST__DD2R/3600.0;
            }

            if( par[ 5 ] == AST__BAD || par[ 5 ] < 0.0 ) {
               par[ 5 ] = -pixsize[ 1 ]*AST__DD2R/3600.0;
            } else {
               par[ 5 ] = pixsize[ 1 ]*AST__DD2R/3600.0;
            }

         }

/* Convert the autogrid CROTA value from rads to degs and set as the
   dynamic default for parameter CROTA (the position angle of the output
   Y axis, in degrees). The get the CROTA value and convert to rads. */
         if( par[ 6 ] != AST__BAD ) {
            autorot = par[ 6 ]*AST__DR2D;
            parDef0d( "CROTA", autorot, status );

         } else {
            parDef0d( "CROTA", map_pa*AST__DR2D, status );
            autorot = AST__BAD;
         }

         parGet0d( "CROTA", par + 6, status );
         if( par[ 6 ] != autorot ) udefs = 0;
         par[ 6 ] *= AST__DD2R;

/* If any parameter were given explicit values which differ from the
   autogrid default values, then we need to re-calculate the optimal CRPIX1/2
   values. We also do this if all the points are effectively co-incident. */
         if( ( coin || !udefs ) && autogrid && refine_crpix ) {
            par[ 0 ] = AST__BAD;
            par[ 1 ] = AST__BAD;
            kpg1Opgrd( nallpos, allpos, strcmp( usesys, "AZEL" ), par,
                       &rdiam, status );
         }

/* Display the projection parameters being used. */
         smf_display_projpars( skyframe, par, status );

/* Write out the reference grid coords to output parameter PIXREF. */
         parPut1d( "PIXREF", 2, par, status );

/* If no grid was found, indicate that no spatial projection will be used. */
      } else {
         msgBlank( status );
         msgOutif( MSG__NORM, " ", "   The output will be a sparse array "
                        "containing a list of spectra.", status );
      }

/* If we have a pre-defined spatial projection, indicate that the output
   array need not be sparse. */
   } else {
      sparse = 0;
   }

/* Return usedefs if requested */
   if( usedefs ) {
     *usedefs = udefs;
   }

/* Set sparse if requested */
   if( issparse ) *issparse = sparse;

}
