/*
*+
*  Name:
*     smf_mapregion_approx

*  Purpose:
*     Return an AST Region representing the approximate coverage of an
*     observation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     AstRegion *smf_mapregion_approx( Grp *igrp, int index, int *status )

*  Arguments:
*     igrp = Grp * (Given)
*        Group of time-stream NDF data files.
*     index = int (Given)
*        Index of the file to use for determining the map region,
*        usually 1 to use the first file in the Grp.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to an AST Region (a Circle) defining the appximate extent
*     of the observation on the sky, defined within the tracking system.

*  Description:
*     The returned Circle is determined from the FITS headers MAP_HGHT,
*     MAP_WIDTH, MAP_X, MAP_Y, BASEC1, BASEC2, TRACKSYS, read from the
*     header of the specified input file.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-JUN-2014 (DSB):
*        Original version.
*     24-JUL-2014 (DSB):
*        If the FITS headers that give the map size are not available,
*        use reasonable defaults rather than reporting an error.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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

/* SMURF includes */
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

AstRegion *smf_mapregion_approx( Grp *igrp, int index, int *status ){

/* Local Variables: */
   AstFrame *azelfrm;
   AstFrame *trfrm;
   AstRegion *result = NULL;
   char tracksys[ 80 ];
   const char *system;
   double basec1;
   double basec2;
   double centre[ 2 ];
   double maphght;
   double mapwdth;
   double mapx;
   double mapy;
   double radius;
   smfData *data;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Open the requested file. */
   smf_open_file( NULL, igrp, index, "READ", SMF__NOCREATE_DATA, &data, status );

/* Check that the data is 3-dimensional. */
   if( data->ndims != 3 && *status == SAI__OK ) {
      smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
      msgSeti( "ND", data->ndims );
      *status = SAI__ERROR;
      errRepf( "", "^FILE data has ^ND dimensions, should be 3.", status );
   }

/* Set defaults for the required FITS headers. */
   if( data->hdr->instrument == INST__SCUBA2 ) {
      mapwdth = 3600.0;
      maphght = 3600.0;
   } else {
      mapwdth = 600.0;
      maphght = 600.0;
   }
   mapx = 0.0;
   mapy = 0.0;
   basec1 = VAL__BADD;
   basec2 = VAL__BADD;
   tracksys[ 0 ] = 0;

/* Get the values from the required FITS header. */
   smf_getfitsd( data->hdr, "MAP_WDTH", &mapwdth, status );
   smf_getfitsd( data->hdr, "MAP_HGHT", &maphght, status );
   smf_getfitsd( data->hdr, "MAP_X", &mapx, status );
   smf_getfitsd( data->hdr, "MAP_Y", &mapy, status );
   smf_getfitsd( data->hdr, "BASEC1", &basec1, status );
   smf_getfitsd( data->hdr, "BASEC2", &basec2, status );
   smf_getfitss( data->hdr, "TRACKSYS", tracksys, sizeof(tracksys), status );

/* Report an error if the base position or tracking system is undefined. */
   if( basec1 == VAL__BADD || basec2 == VAL__BADD || tracksys[ 0 ] == 0 ) {
      if( *status == SAI__OK ) {
         smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
         *status = SAI__ERROR;
         errRepf( "", "smf_mapbounds_approx: Cannot determine the region "
                  "covered by ^FILE.", status );
      }
   }

/* Add the offsets (arc-secs) onto the base position (degs), and convert
   to radians. */
   centre[ 0 ] = AST__DD2R*( basec1 + mapx/3600.0 );
   centre[ 1 ] = AST__DD2R*( basec2 + mapy/3600.0 );

/* Find the diagonal length of the map in degs, halve it to get the radius
   and add on 5 arc-mins (the diagonal of one sub-array) to account for the
   fact that the map width and height refer to the region swept out by the
   boresight. Note, map dimensions are in arc-seconds. */
   radius = AST__DD2R*( 0.5*sqrt( maphght*maphght + mapwdth*mapwdth )/3600.0 + 5.0/60.0 );

/* Get the WCS FrameSet describing the first time slice. This is just to
   get a SkyFrame with the extra info (observatory position, date/time,
   etc) needed to describe the tracking system. */
   smf_tslice_ast( data, 1, 1, NO_FTS, status );
   azelfrm = astGetFrame( data->hdr->wcs, AST__CURRENT );

/* Get the AST equivalent to the tracking system, and change the skyframe
   to represent this system. Use a copy to avoid changing the original. */
   system = sc2ast_convert_system( tracksys, status );
   trfrm = astCopy( azelfrm );
   astSetC( trfrm, "System", system );

/* Create the Circle. */
   result = (AstRegion *) astCircle( trfrm, 1, centre, &radius, NULL, " " );

/* Free the skyframe. */
   trfrm = astAnnul( trfrm );

/* Close the file */
   smf_close_file( NULL, &data, status );

/* Return the Circle pointer. */
   return result;
}

