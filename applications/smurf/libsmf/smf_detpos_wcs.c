/*
*+
*  Name:
*     smf_detpos_wcs

*  Purpose:
*     Create an AST FrameSet describing a specified time slice based on
*     the detector position stored in a smfHead.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_detpos_wcs( smfHead *hdr, int index, const double telpos[3], 
*                     AstFrameSet **fset, int *status );

*  Arguments:
*     hdr = smfHead * (Given & Returned)
*        The smfHead structure containing the detector positions.
*     index = int (Given)
*        Index into the time series data (the 3rd dimension). Call with a
*        negative index value to free cached resources.
*     telpos = double[ 3 ] (Given)
*        Geodetic lon / lat / altitude of the telscope (deg/deg/metres)
*     fset = AstFrameSet ** (Given)
*        Address of a location at which to put the returned FrameSet
*        pointer. Ignored if "index" is negative.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function is used to create an AST FrameSet for the
*     specified time slice from the "detpos" values in the supplied
*     smfHead structure. 
*
*     The returned FrameSet has a 2D GRID Frame as the base Frame, and a
*     SkyFrame describing tracking coordinates as the current Frame. The
*     first GRID axis is detector index, and the second is un-used.

*  Authors:
*     David Berry (JAC, UCLan)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2-OCT-2006 (DSB):
*        Initial version.
*     2006-11-01 (DSB):
*        Added steptime.
*     2008-04-09 (TIMJ):
*        Correct for rts_end vs tcs_tai elsewhere (smf_open_file).
*        No longer need steptime argument.
*     2008-06-19 (TIMJ):
*        Make sure the statically malloced buffer is resized if the
*        number of receptors changes.
*     20-JUN-2008 (DSB):
*        Tidy up use of astGrow.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"

/* SMURF includes */
#include "sc2da/sc2ast.h"
#include "smf.h"
#include "smf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_detpos_wcs"

/* Seconds per day */
#define SPD 86400.0                    

void smf_detpos_wcs( smfHead *hdr, int index, const double telpos[3], 
                     AstFrameSet **fset, int *status ) {

   const double *p1;           /* Pointer to next lon or lat value to copy */
   double *p2;                 /* Pointer to next lon value */
   double *p3;                 /* Pointer to next lat value */
   int nrec;                   /* Number of detectors */
   int i;                      /* Index of current detector */
   AstLutMap *lonmap = NULL;   /* LutMap holding longitude values */
   AstLutMap *latmap = NULL;   /* LutMap holding latitude values */
   AstCmpMap *cmap1 = NULL;    /* Parallel CmpMap holding both LutMaps */
   AstMapping *map = NULL;     /* GRID->SKY Mapping */
   int outperm[ 2 ];           /* Axis permutation */
   AstSkyFrame *csky = NULL;   /* SkyFrame to put in returned FrameSet */

   static double *latlut = NULL;
   static double *lonlut = NULL;
   static AstPermMap *pmap = NULL;
   static AstFrame *grid = NULL;
   static AstSkyFrame *sky = NULL;

/* If a negative index was supplied just free the allocated resources and
   return. */
   if( index < 0 ) {
      if( latlut ) latlut = astFree( latlut );
      if( lonlut ) lonlut = astFree( lonlut );
      if( pmap ) pmap = astAnnul( pmap );
      if( grid ) grid = astAnnul( grid );
      if( sky ) sky = astAnnul( sky );
      return;
   }

/* Check inherited status */
   if( *status != SAI__OK) return;

/* Get the number of detectors. */
   nrec = hdr->ndet;

/* Get a pointer to the start of the detpos values for the requested
   time slice. */
   p1 = hdr->detpos + 2*nrec*index;

/* Check there is more than 1 detector. */
   if( nrec > 1 ) {

/* It is possible that we have not allocated enough memory since
   this memory is allocated for the first file but subsequent files
   may have more receptors. So we use astGrow. */

/* If required, allocate memory to hold the individual look up tables for
   lon and lat vaues. */
      lonlut = astGrow( lonlut, nrec, sizeof( double ) );
      latlut = astGrow( latlut, nrec, sizeof( double ) );

/* Check the memory was allocated succesfully. */
      if( lonlut && latlut ) {
  
/* Copy the lon and lat values for the requested time slice from the
   smfHead structure to the local lut arrays. */
         p2 = lonlut;
         p3 = latlut;
         for( i = 0; i < nrec; i++ ) {
            *(p2++) = *(p1++);
            *(p3++) = *(p1++);
         }

/* Create the Mapping from GRID to SKY positions. This is a PermMap to
   duplicate the detector index, followed by 2 LutMaps in parallel to
   generate the lon and lat values. Set the LutInterpattribute in these
   LutMaps so that they use nearest neighbour interpolation. */
         lonmap = astLutMap( nrec, lonlut, 1.0, 1.0, "LutInterp=1" );
         latmap = astLutMap( nrec, latlut, 1.0, 1.0, "LutInterp=1" );
         cmap1 = astCmpMap( lonmap, latmap, 0, "" );

         latmap = astAnnul( latmap );
         lonmap = astAnnul( lonmap );

         if( !pmap ) {
            outperm[ 0 ] = 1;
            outperm[ 1 ] = 1;
            pmap = astPermMap( 2, NULL, 2, outperm, NULL, "" );
            astExempt( pmap );
         }
         map = (AstMapping *) astCmpMap( pmap, cmap1, 1, "" );

         pmap = astAnnul( pmap );
         cmap1 = astAnnul( cmap1 );
      }

/* If thre is only one detector, use a PermMap to describe this one
   position. rather than a LutMap (LutMaps cannot describe a single
   position). */
   } else {
      outperm[ 0 ] = -1;
      outperm[ 1 ] = -2;
      map = (AstMapping *) astPermMap( 2, NULL, 2, outperm, p1, "" );
   }

/* Create two Frames to put in the FrameSet. */
   if( !grid ) {
      grid = astFrame( 2, "Domain=GRID" );
      astExempt( grid );
   }

   if( !sky ) {
      sky = astSkyFrame( "System=AzEl" );
      astSetD( sky, "ObsLon", -telpos[ 0 ] );
      astSetD( sky, "ObsLat", telpos[ 1 ] );
      astExempt( sky );

/* If the detpos positions are referred to the TRACKING frame, change
   the SkyFrame from AZEL to the AST equivalent of the TRACKING Frame. */
      if( !hdr->dpazel ) {
         astSetC( sky, "System", sc2ast_convert_system( hdr->state->tcs_tr_sys,
                                                     status ) ); 
      }
   }

/* Take a copy of the skyframe, and then modify its Epoch attribute. We take a
   copy since otherwise all FrameSets returned by this function would share 
   the same current Frame, and so the attribute change would affect them all.
   Always use TCS_TAI. smf_open_file corrects the JCMTState structure
   if TCS_TAI is missing. Remember to convert from TAI to TDB (as required by 
   the Epoch attribute). */
   csky = astCopy( sky );
   astSet( csky, "Epoch=MJD %.*g", DBL_DIG, hdr->state->tcs_tai + 
                                            32.184/SPD ); 

/* Create the FrameSet */
   *fset = astFrameSet( grid, "" );
   astAddFrame( *fset, AST__BASE, map, csky );

/* Free resources */
   map =astAnnul( map );      
   csky =astAnnul( csky );      

/* Exempt the FrameSet pointer from the AST context system rather because 
   we do not know when, or in which context, it will be used. It will be 
   annulled either in smf_tslice_ast or in smf_close_file. */
   astExempt( *fset );

}
