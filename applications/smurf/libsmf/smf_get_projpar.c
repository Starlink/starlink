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
*     smf_get_projpar( AstSkyFrame *skyframe, double par[ 7 ], int *usedefs,
*                      int *status );

*  Arguments:
*     skyframe = AstFrameSet * (Given)
*        Pointer to an AST SkyFrame describing the spatial axes of the
*        output WCS FrameSet.  
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
*     usedefs = int * (Returned)
*        If usedefs=1, using input rather than user-defined parameters
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function updates the par array which contains a description of
*     the map projection. The array is updates using user-defined
*     ADAM parameters. If no previous values have been specified for
*     par, each element should be initialized to AST__BAD.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-05-12 (EC):
*        Initial version factored out of smf_cubegrid
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2008 University of British Columbia.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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
#include "star/ndg.h"
#include "star/slalib.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_get_projpar"

void smf_get_projpar( AstSkyFrame *skyframe, double par[ 7 ], int *usedefs, 
                      int *status ) {

  /* Local Variables */
  AstSkyFrame *abskyframe = NULL; /* Output Absolute SkyFrame */
  const char *deflat;             /* Default for REFLAT */
  const char *deflon;             /* Default for REFLON */
  double defrot;                  /* Default for CROTA parameter */
  double defsize[ 2 ];            /* Default pixel sizes in arc-seconds */
  int nval;                       /* Number of values supplied */
  double pixsize[ 2 ];            /* Pixel sizes in arc-seconds */
  char reflat[ 41 ];              /* Reference latitude string */
  char reflon[ 41 ];              /* Reference longitude string */
  int udefs;                      /* Flag for defaults used or not */

  /* Main routine */
  if (*status != SAI__OK) return;

   /* Set up a flag indicating that the default values calculated above are
      being used. */
   udefs = 1;

  /* Ensure we have usable CRPIX1/2 values */
  if( par[ 0 ] == AST__BAD ) par[ 0 ] = 1.0;
  if( par[ 1 ] == AST__BAD ) par[ 1 ] = 1.0;
     
  /* Get the reference position strings. Use the returned SkyFrame to
     format and unformat them. */
  if( par[ 2 ] != AST__BAD ) {
    deflon = astFormat( skyframe, 1, par[ 2 ] );
    parDef0c( "REFLON", deflon, status );
  } else {
    deflon = NULL;
  }

  if( par[ 3 ] != AST__BAD ) {
    deflat = astFormat( skyframe, 2, par[ 3 ] );
    parDef0c( "REFLAT", deflat, status );
  } else {
    deflat = NULL;
  }
  
  parGet0c( "REFLON", reflon, 40, status );
  parGet0c( "REFLAT", reflat, 40, status );
  
  if( *status == SAI__OK ) {
    
    if( ( deflat && strcmp( deflat, reflat ) ) ||
	( deflon && strcmp( deflon, reflon ) ) ) udefs = 0;
    
    if( astUnformat( skyframe, 1, reflon, par + 2 ) == 0 && 
	*status == SAI__OK ) {
      msgSetc( "REFLON", reflon );
      errRep( "", "Bad value supplied for REFLON: '^REFLON'", status );
    }
    
    if( astUnformat( skyframe, 2, reflat, par + 3 ) == 0 && 
	*status == SAI__OK ) {
      msgSetc( "REFLAT", reflat );
      errRep( "", "Bad value supplied for REFLAT: '^REFLAT'", status );
    }  
  }
  
  /* Get the user defined spatial pixel size in arcsec (the calibration for 
     the spectral axis is fixed by the first input data file - see 
     smf_cubebounds.c). First convert the autogrid values form rads to arcsec
     and establish them as the dynamic default for "PIXSIZE". */
  if( par[ 4 ] != AST__BAD && par[ 5 ] != AST__BAD ) {
    defsize[ 0 ] = 0.1*NINT( fabs( par[ 4 ] )*AST__DR2D*36000.0 );
    defsize[ 1 ] = 0.1*NINT( fabs( par[ 5 ] )*AST__DR2D*36000.0 );
    parDef1d( "PIXSIZE", ( defsize[ 0 ] == defsize[ 1 ] ) ? 1 : 2, 
	      defsize, status );
  }
  parGet1d( "PIXSIZE", 2, pixsize, &nval, status );
  
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
    defrot = par[ 6 ]*AST__DR2D;
    parDef0d( "CROTA", defrot, status );
  } else {
    defrot = 0;
  }
  
  parGet0d( "CROTA", par + 6, status );
  if( par[ 6 ] != defrot ) udefs = 0;
  par[ 6 ] *= AST__DD2R;
  
  /* Return usedefs if requested */
  if( usedefs ) {
    *usedefs = udefs;
  }
}
