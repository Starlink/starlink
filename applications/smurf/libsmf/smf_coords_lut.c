/*
*+
*  Name:
*     smf_coords_lut

*  Purpose:
*     Create a look-up table holding the output map position of every
*     bolometer sample.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_coords_lut( smfData *data, int tstep, dim_t itime_lo,
*                          dim_t itime_hi, AstSkyFrame *abskyfrm,
*                          AstMapping *oskymap, int moving, int olbnd[ 2 ],
*                          int oubnd[ 2 ], fts2Port fts_port, int *lut,
*                          double *angle, double *lon, double *lat,
*                          dim_t *onmap, int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the input smfData structure.
*     tstep = int (Given)
*        The increment in time slices between full Mapping calculations.
*        The Mapping for intermediate time slices will be approximated.
*     itime_lo = dim_t (Given)
*        The index of the first time slice to be included in the returned
*        LUT.
*     itime_hi = dim_t (Given)
*        The index of the last time slice to be included in the returned
*        LUT.
*     abskyfrm = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe
*        the spatial axes of the output map. This should represent
*        absolute sky coordinates rather than offsets even if "moving" is
*        non-zero.
*     oskymap = AstFrameSet * (Given)
*        A Mapping from 2D sky coordinates in the output map to 2D
*        spatial pixel coordinates in the output map.
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If
*        so, each time slice is shifted so that the position specified by
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        output map.
*     olbnd[ 2 ] = int (Given)
*        The lower pixel index bounds of the output map.
*     oubnd[ 2 ] = int (Given)
*        The upper pixel index bounds of the output map.
*     fts_port = fts2Port (Given)
*        FTS-2 port.
*     lut = int * (Returned)
*        Point to array in which to store the 1-d vector index within the
*        output map, of every bolometer sample in the supplied time slice
*        range. The first value is written to element 0. The last value
*        is written to element "(itime_hi - itime_lo + 1)*nbolo - 1".
*     angle = double * (Returned)
*        The scan angle for the boresight in GRID coordinates at every time
*        slice. Similar to lut, the first value (at itime_lo) is written to
*        element 0. Can be NULL.
*     lon = double * (Given & Returned)
*        A pointer to a (nbolo,ntslice) array in which to return the
*        longitude value (degs) at all samples between and including itime_lo
*        and itime_hi. May be NULL.
*     lat = double * (Returned)
*        A pointer to a (nbolo,ntslice) array in which to return the
*        latitude value (degs) at all samples between and including itime_lo
*        and itime_hi. May be NULL.
*     onmap = dim_t * (Returned)
*        A pointer to a size_t in which to return the count of bolometer
*        samples that fell within the bounds of the map. Ignored if NULL.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     A full calculation of the input bolometer -> output grid Mapping is
*     done at regularly spaced time slices, and used to generate the output
*     map GRID (x,y) coords for every bolometer at each of these selected
*     time slices.
*
*     For other time slices, the output map GRID coords are estimated by
*     adding a constant offset onto all the bolo GRID coords from the
*     previous selected time slice. This constant offset is equal to the
*     shift in boresight position, as measured in the output GRID
*     coordinate system, from the previous selected time slice to the
*     current time slice.
*
*     These output GRID coords are converted into 1-dimensional vector index
*     within the output map, and stored in the returned LUT.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     3-MAR-2010 (DSB):
*        Initial version.
*     10-MAR-2010 (DSB):
*        Check slice FrameSet is not null before exporting it.
*     9-NOV-2010 (EC):
*        Add angle to interface
*     2011-09-15 (TIMJ):
*        Make it work with mising TCS information.
*     12-DEC-2011 (DSB):
*        Add lon and lat to interface.
*     2-AUG-2018 (DSB):
*        Add onmap to interface.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 Science & Technology Facilities Council.
*     Copyright (C) 2010 University of British Columbia.
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
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

void smf_coords_lut( smfData *data, int tstep, dim_t itime_lo,
                     dim_t itime_hi, AstSkyFrame *abskyfrm,
                     AstMapping *oskymap, int moving, int olbnd[ 2 ],
                     int oubnd[ 2 ], fts2Port fts_port, int *lut,
                     double *angle, double *lon, double *lat, dim_t *onmap,
                     int *status ) {

/* Local Variables */
   AstCmpMap *bsmap = NULL;     /* Tracking -> output grid Mapping */
   AstFrame *trfrm = NULL;      /* Tracking Frame */
   AstFrameSet *fs = NULL;      /* Tracking -> output sky FrameSet */
   AstMapping *fullmap = NULL;  /* Full Mapping from bolo GRID to output map GRID */
   AstMapping *offmap = NULL;   /* Mapping from absolute to offset sky coords */
   AstMapping *tr2skyabs = NULL;/* Tracking -> output sky Mapping */
   AstSkyFrame *offsky = NULL;  /* Offset sky frame */
   JCMTState *state;     /* Pointer to telescope info for time slice */
   dim_t ibolo;          /* Vector index of bolometer */
   dim_t idimx;          /* Bolometers per row */
   dim_t idimy;          /* Bolometers per column */
   dim_t ilut;           /* Index of LUT element */
   dim_t itime0;         /* Time slice index at next full calculation */
   dim_t itime;          /* Time slice index */
   dim_t nbolo;          /* Total number of bolometers */
   double *outmapcoord;  /* Array holding output map GRID coords */
   double *px;           /* Pointer to next output map X GRID coord */
   double *py;           /* Pointer to next output map Y GRID coord */
   double *pgx;          /* Pointer to next X output grid coords */
   double *pgy;          /* Pointer to next Y output grid coords */
   double *wgx = NULL;   /* Work space to hold X output grid coords */
   double *wgy = NULL;   /* Work space to hold Y output grid coords */
   double bsx0;          /* Boresight output map GRID X at previous full calc */
   double bsx;           /* Boresight output map GRID X at current time slice */
   double bsxlast;       /* Boresight output map GRID X at previous time slice*/
   double bsy0;          /* Boresight output map GRID Y at previous full calc */
   double bsy;           /* Boresight output map GRID Y at current time slice */
   double bsylast;       /* Boresight output map GRID Y at previous time slice*/
   double dx;            /* Offset in GRID X from previous full calc */
   double dxlast;        /* Offset in GRID X from previous time slice */
   double dy;            /* Offset in GRID Y from previous full calc */
   double dylast;        /* Offset in GRID Y from previous time slice */
   double shift[ 2 ];    /* Shift from PIXEL to GRID in output map */
   double x;             /* Output GRID X at current bolo in current row */
   double xin[ 2 ];      /* Input X values */
   double xout[ 2 ];     /* Output X values */
   double y;             /* Output GRID Y at current bolo in current row */
   double yin[ 2 ];      /* Input Y values */
   double yout[ 2 ];     /* Output Y values */
   int lbnd_in[ 2 ];     /* Lower bounds of input array */
   int np;               /* Number of positions to transform */
   int odimx;            /* Output map X dimension in pixels */
   int odimy;            /* Output map Y dimension in pixels */
   int ox;               /* Output X GRID index (-1) containing current bolo */
   int oy;               /* Output Y GRID index (-1) containing current bolo */
   int ubnd_in[ 2 ];     /* Upper bounds of input array */


/* Initialise */
   if( onmap ) *onmap = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Get the dimensions of the output map. */
   odimx = oubnd[ 0 ] - olbnd[ 0 ] + 1;
   odimy = oubnd[ 1 ] - olbnd[ 1 ] + 1;

/* Get the dimensions of the bolometer array. */
   idimx = (data->dims)[ 0 ];
   idimy = (data->dims)[ 1 ];

/* Store integer bounds within the input bolometer GRID system. */
   lbnd_in[ 0 ] = 1;
   lbnd_in[ 1 ] = 1;
   ubnd_in[ 0 ] = idimx;
   ubnd_in[ 1 ] = idimy;

/* Determine the number of bolometers. */
   nbolo = idimx*idimy;

/* Initialise the index of the next LUT element to write. */
   ilut = 0;

/* Ensure tstep is at least one. */
   if( tstep < 1 ) tstep = 1;

/* Get the time slice index at which to do the next full calculation. */
   itime0 = itime_lo;

/* We only need the following AST objects if we will be approximating
   some caclulations. */
   if( tstep > 1 ) {

/* We need to find the first good TCS index in order to get the
   tracking system (Which for SCUBA-2 won't be changing in the sequence) */
      itime0 = VAL__BADI;
      for (itime = itime_lo; itime <= itime_hi; itime++) {
        JCMTState * slice = &((data->hdr->allState)[itime]);
        if (!(slice->jos_drcontrol >= 0 && slice->jos_drcontrol & DRCNTRL__POSITION)) {
          itime0 = itime;
          break;
        }
      }

      if ((int)itime0 != VAL__BADI) {

/* We need a Frame describing absolute tracking system coords. Take a
   copy of the supplied skyframe (to inherit obslat, obslon, epoch,
   etc), and then set its system to the tracking system. */
        trfrm = astCopy( abskyfrm );
        astSetC( trfrm, "System",
                 sc2ast_convert_system( (data->hdr->allState)[itime0].tcs_tr_sys,
                                        status ) );

/* Get the Mapping from the tracking system to the output (absolute, since
   abskyfrm is absolute) sky system. */
        fs = astConvert( trfrm, abskyfrm, " " );
        if( !fs && *status == SAI__OK ) {
          *status = SAI__ERROR;
          errRep( " ", "smf_coords_lut: Failed to convert from "
                  "tracking system to output WCS system.", status );
        }
        tr2skyabs = astSimplify( astGetMapping( fs, AST__BASE, AST__CURRENT ) );

/* For moving targets, we also need a Frame describing offset sky
   coordinates in the output map, in which the reference point is the
   current telescope base position. This will involve changing the
   SkyRef attribute of the Frame for every time slice, so take a
   copy of the supplied SkyFrame to avoid changing it. */
        if( moving ) {
          offsky = astCopy( abskyfrm );
          astSet( offsky, "SkyRefIs=Origin" );
        }

/* Create the Mapping from offsets within the output map sky coordinate
   system output to map GRID coords to output map GRID coords. This uses a
   ShiftMap to convert from output PIXEL coords (produced by "oskymap") to
   output GRID coords. Note, if the target is moving, "oskymap" maps from
   sky *offsets* to output map PIXEL coords. */
        shift[ 0 ] = 1.5 - olbnd[ 0 ];
        shift[ 1 ] = 1.5 - olbnd[ 1 ];
        bsmap = astCmpMap( oskymap, astShiftMap( 2, shift, " " ), 1, " " );
      } else {
        /* We did not find any good TCS data so force us into tstep==1
         case and end up with VAL__BADI for every element. */
        tstep = 1;
        itime0 = itime_lo;
        msgOutiff(MSG__VERB, "", "All time slices from %zu -- %zu had bad TCS data.",
                 status, (size_t)itime_lo, (size_t)itime_hi );
      }
   }

/* Allocate memory to hold the (x,y) output map grid coords at each bolo
   for a single time slice. */
   outmapcoord = astMalloc( sizeof( *outmapcoord )*2*nbolo );

/* Initialise boresight position for the benefit of the tstep == 1 case. */
   bsx = bsy = 0.0;
   bsx0 = bsy0 = AST__BAD;
   bsxlast = bsylast = AST__BAD;

/* If lon and lat arrays are to be returned, allocate memory to hold the grid
   coords of every bolometer for a single sample. */
   if( lon && lat ) {
      wgx = astMalloc( nbolo*sizeof( *wgx ) );
      wgy = astMalloc( nbolo*sizeof( *wgy ) );
   }

/* Loop round each time slice. */
   state = data->hdr->allState + itime_lo;
   for( itime = itime_lo; itime <= itime_hi && *status == SAI__OK;
        itime++,state++ ) {

/* No need to get the boresight position if we are doing full
   calculations at every time slice. If this time slice has bad
   TCS data then the problem will be caught in smf_rebin_totmap. */
      if( tstep > 1 ) {

/* Transform the current boresight and base (if moving) positions from
   tracking coords to absolute output map sky coords. */
         xin[ 0 ] = state->tcs_tr_ac1;
         yin[ 0 ] = state->tcs_tr_ac2;
         if( moving ) {
            xin[ 1 ] = state->tcs_tr_bc1;
            yin[ 1 ] = state->tcs_tr_bc2;
            np = 2;
         } else {
            np = 1;
         }
         astTran2( tr2skyabs, np, xin, yin, 1, xout, yout );

/* If the target is moving, find the offsets within the output map sky
   coordinate system, from base to boresight at the current time slice.
   These offsets become the new "boresight" position (in xin/yin). Guard
   against assigning bad values to the ref pos, which can cause AST to
   report errors. */
         if( moving && xout[ 1 ] != AST__BAD && yout[ 1 ] != AST__BAD ) {

/* Set the current telescope base position as the reference point in
   "offsky". Then get the Mapping from absolute to offset sky coords. */
            astSetD( offsky, "SkyRef(1)", xout[ 1 ] );
            astSetD( offsky, "SkyRef(2)", yout[ 1 ] );
            offmap = astSkyOffsetMap( offsky );

/* Use this Mapping to convert the current boresight position from
   absolute sky coords to offsets from the current base position. */
            astTran2( offmap, 1, xout, yout, 1, xin, yin );

/* Annul the Mapping to avoid keep the number of AST objects to a minimum. */
            offmap = astAnnul( offmap );

/* If the target is stationary, we can just use the absolute boresight
   position as it is. */
         } else {
            xin[ 0 ] = xout[ 0 ];
            yin[ 0 ] = yout[ 0 ];
         }

/* Transform the above boresight position from output map sky coords to output
   map GRID coords. */
         astTran2( bsmap, 1, xin, yin, 1, &bsx, &bsy );
      }

/* If we have reached the next full calculation... */
      if( itime == itime0 ) {

/* Calculate the full bolometer to map-pixel transformation for the current
   time slice */
         fullmap = smf_rebin_totmap( data, itime, abskyfrm, oskymap, moving,
                                     fts_port, status );

/* If succesful, use it to transform every bolometer position from bolo
   GRID coords to output map GRID coords. */
         if( fullmap ) {
            itime0 += tstep;
            astTranGrid( fullmap, 2, lbnd_in, ubnd_in, 0.1, 1000000, 1,
                         2, nbolo, outmapcoord );
            fullmap = astAnnul( fullmap );

/* Record the boresight grid coords at this time slice. */
            bsx0 = bsx;
            bsy0 = bsy;

/* If we cannot determine a full Mapping for this time slice, move the
   node to the next time slice (otherwise we would loose all the data to
   the next node), and set the boresight position bad to indicate we
   have no mapping for this time slice. */
         } else {
            itime0++;
            bsx0 = AST__BAD;
            bsy0 = AST__BAD;
         }
      }

/* Get the offset from the boresight position at the previous full
   calculation and the current boresight position, in output map GRID
   coords. */
      dx = ( bsx != AST__BAD && bsx0 != AST__BAD ) ? bsx - bsx0 : AST__BAD;
      dy = ( bsy != AST__BAD && bsy0 != AST__BAD ) ? bsy - bsy0 : AST__BAD;

/* Work out the scan direction based on the GRID offsets between this and
   the previous time slice. Angles are calculated using atan2, with values
   ranging from -pi to +pi. */
      if( angle ) {
        double theta = AST__BAD;

        dxlast = ( bsx != AST__BAD && bsxlast != AST__BAD ) ?
          bsx - bsxlast : AST__BAD;

        dylast = ( bsy != AST__BAD && bsylast != AST__BAD ) ?
          bsy - bsylast : AST__BAD;

        if( dxlast != AST__BAD && dylast != AST__BAD &&
            !( !dxlast && !dylast ) ) {
          theta = atan2( dylast, dxlast );
        }

        angle[itime-itime_lo] = theta;

        bsxlast = bsx;
        bsylast = bsy;
      }

/* Initialise pointers to the place to store the final grid coords for
   each bolometer. */
      pgx = wgx;
      pgy = wgy;

/*   Loop round all bolometers. */
      px = outmapcoord;
      py = outmapcoord + nbolo;
      for( ibolo = 0; ibolo < nbolo; ibolo++ ){

/* If good, get the x and y output map GRID coords for this bolometer. */
         if( dx != AST__BAD && dy != AST__BAD ) {
            x = *(px++) + dx;
            y = *(py++) + dy;

/* If required, store them so that we can convert them into lon/lat
   values later. */
            if( pgx && pgy ) {
               *(pgx++) = x;
               *(pgy++) = y;
            }

/* Find the grid indices (minus one) of the output map pixel containing the
   mapped bolo grid coords. One is subtracted in order to simplify the
   subsequent calculation of the vector index. */
            ox = (int) ( x - 0.5 );
            oy = (int) ( y - 0.5 );

/* Check it is within the output map */
            if( ox >= 0 && ox < odimx && oy >= 0 && oy < odimy ) {

/* Increment the count of bolometer samples that fall within the map */
               if( onmap ) (*onmap)++;

/* Find the 1-dimensional vector index into the output array for this
   pixel and store in the next element of the returned LUT. */
               lut[ ilut++ ] = ox + oy*odimx;

/* Store a bad value for points that are off the edge of the output map. */
            } else {
               lut[ ilut++ ] = VAL__BADI;
            }

/* If good, store a bad index in the LUT and move on to the next pixel. */
         } else {
            lut[ ilut++ ] = VAL__BADI;
            px++;
            py++;
            if( pgx && pgy ) {
               *(pgx++) = AST__BAD;
               *(pgy++) = AST__BAD;
            }
         }
      }

/* If required transform the grid coords into (lon,lat) coords and store in
   the relevant elements of the supplied "lon" and "lat" arrays. */
      if( wgx && wgy ) {
         astTran2( oskymap, nbolo, wgx, wgy, 0, lon + itime*nbolo,
                   lat + itime*nbolo );

/* Convert from rads to degs. */
         pgx = lon + itime*nbolo;
         pgy = lat + itime*nbolo;
         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
            if( *pgx != AST__BAD && *pgy != AST__BAD ) {
               *(pgx++) *= AST__DR2D;
               *(pgy++) *= AST__DR2D;
            } else {
               pgx++;
               pgy++;
            }
         }
      }
   }

/* To obtain a reasonable value for the first entry of angle, we simply
   duplicate the second value. */
   if( angle && (itime_hi > itime_lo) ) {
     angle[0] = angle[1];
   }

/* Free remaining work space. */
   outmapcoord = astFree( outmapcoord );
   wgx = astFree( wgx );
   wgy = astFree( wgy );

/* Export the WCS pointer in the data header since it will be annulled at
   a higher level. Note the FrameSet pointer may be null if the last
   full calculation was for a slice with bad telescope data. */
   if( data->hdr->wcs ) astExport( data->hdr->wcs );

/* End the AST context. */
   astEnd;
}


