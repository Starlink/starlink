/*
*+
*  Name:
*     smf_add_spectral_axis

*  Purpose:
*     Add a spectral axis to a 2D SCUBA-2 map.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_add_spectral_axis( int indf, AstFitsChan *fc, int *status );

*  Arguments:
*     indf = int (Given)
*        Identifier for the map. The NDF should be 2-dimensional on entry
*        and will be 3-dimensional on exit.
*     fc = AstFitsChan * (Given)
*        Pointer to a FitsCHan holding the FITS header associated with
*        the output NDF.
*     status = int* (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function adds a third pixel axis to the supplied 2D SCUBA-2
*     map, spanning a single pixel (pixel 1). It also adds a SpecFrame to
*     the current WCS Frame in the NDF, connecting the SpecFrame to the
*     new GRID axis using a WinMap that results in the spectral pixel
*     being centred at the wavelength specified by FITS header WAVELEN,
*     and having a width given by FITS header BANDWID.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     27-OCT-2009 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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

/* Starlink includes */
#include "ast.h"
#include "ndf.h"
#include "star/atl.h"
#include "sae_par.h"

/* Smurf includes */
#include "smf.h"

void smf_add_spectral_axis( int indf, AstFitsChan *fc, int *status ){

/* Local Variables */
   AstFrame *cfrm;         /* Pointer to the current WCS Frame in the NDF */
   AstFrameSet *wcs;       /* Pointer to the WCS FrameSet for the NDF */
   AstSpecFrame *specfrm;  /* Pointer to the new SpecFrame */
   AstWinMap *specmap;     /* Pointer to Mapping from GRID to wavelength */
   char attrib[ 10 ];      /* Buffer for attribute name */
   double bandwid;         /* Bandwidth, in metres */
   double grid_hi;         /* GRID coord at upper edge of spectral pixel */
   double grid_lo;         /* GRID coord at lower edge of spectral pixel */
   double ref_lat;         /* Celestial latitude at reference point */
   double ref_lon;         /* Celestial longitude at reference point */
   double spec_hi;         /* Wavelength at upper edge of spectral pixel */
   double spec_lo;         /* Wavelength at lower edge of spectral pixel */
   double wavelen;         /* Central wavelength, in metres */
   dim_t lbnd[ NDF__MXDIM ]; /* Original lower pixel bounds of the NDF */
   dim_t ubnd[ NDF__MXDIM ]; /* Original lower pixel bounds of the NDF */
   int ndim;               /* Original number of pixel axis in the the NDF */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST Object context so that we do not need to annul explicitly
   the AST Objects created in this function. */
   astBegin;

/* Get the pixel bounds of the NDF. */
   ndfBound( indf, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Get the required FITS header. Return without further action if either
   is not present in the supplied FitsChan, or if the NDF is not
   2-dimensional. */
   if( astGetFitsF( fc, "WAVELEN", &wavelen ) &&
       astGetFitsF( fc, "BANDWID", &bandwid ) && ndim == 2 ) {

/* Get the current WCS FrameSet from the supplied NDF, and get a pointer
   to its current Frame. */
      ndfGtwcs( indf, &wcs, status );
      cfrm = astGetFrame( wcs, AST__CURRENT );

/* Return without action if this is not a SkyFrame. */
      if( astIsASkyFrame( cfrm ) ) {

/* Construct a topocentric wavelength SpecFrame to describe the new spectral
   WCS axis. */
         specfrm = astSpecFrame( "System=wavelen,StdOfRest=topo,Unit=m" );

/* We set the RefRA and RefDec attributes for the SpecFrame to the FK5
   J2000 equivalent of the SkyRef attribute in the current Frame. */
         sprintf( attrib, "SkyRef(%d)", astGetI( cfrm, "LonAxis" ) );
         ref_lon = astGetD( cfrm, attrib );

         sprintf( attrib, "SkyRef(%d)", astGetI( cfrm, "LatAxis" ) );
         ref_lat = astGetD( cfrm, attrib );

         astSetRefPos( specfrm, cfrm, ref_lon, ref_lat );

/* Inherit other relevant Frame attributes from the SkyFrame. */
#define OVERLAY(attr) \
         if( astTest( cfrm, attr ) ) { \
            astSetC( specfrm, attr, astGetC( cfrm, attr ) ); \
         }

         OVERLAY( "Dut1" );
         OVERLAY( "Dtai" );
         OVERLAY( "Epoch" );
         OVERLAY( "ObsAlt" );
         OVERLAY( "ObsLat" );
         OVERLAY( "ObsLon" );

#undef OVERLAY

/* Create a WinMap that gives wavelength as a function of spectral GRID
   position. Assume the pixel centre maps onto WAVELEN and the pixel
   width is BANDWID. */
         grid_lo= 0.5;
         grid_hi = 1.5;
         spec_lo = wavelen - 0.5*bandwid;
         spec_hi = spec_lo + bandwid;
         specmap = astWinMap( 1, &grid_lo, &grid_hi, &spec_lo, &spec_hi, " " );

/* Modify the WCS FrameSet so that the base and current Frames are
   3-dimensional. The current Frame is expanded by adding in the
   SpecFrame, and the base Frame is expanded by adding in a 3rd GRID
   axis. Other Frames are left unchanged. The SpecFrame and the new GRID
   axis are connected using the WinMap created above. */
         atlAddWcsAxis( wcs, (AstMapping *) specmap, (AstFrame *) specfrm,
                        NULL, NULL, status );

/* Change the NDF bounds to include a 3rd axis with pixel bounds "1:1". */
         lbnd[ 2 ] = 1;
         ubnd[ 2 ] = 1;
         ndfSbnd( 3, lbnd, ubnd, indf, status );

/* Store the modified WCS FrameSet in the NDF. */
         ndfPtwcs( wcs, indf, status );
      }
   }

/* End the AST Object context. This will annull annull the AST Objects
   created in this function. */
   astEnd;
}
