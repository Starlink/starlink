/*
*+
*  Name:
*     smf_create_lutwcs.c

*  Purpose:
*     Create frameset representing JCMT instrument coordinate transformations
*     using lookup tables for the focal plane offsets of each detector.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_create_lutwcs( int clearcache, const double *fplane_x, 
*                        const double *fplane_y, const int n_pix, 
*        		 const JCMTState *state, const double instap[2], 
*                   	 const double telpos[3], double steptime,
*                        AstFrameSet **fset, int *status )

*  Arguments:
*     clearcache = int (Given)
*        If set to 1 and fixed mappings have previously been cached, clear them
*     fplane_x = double* (Given)
*        Lookup table (LUT) specifying x-focal plane offset for each of
*        n_pix pixels in radians. Ignored if cache from previous call exists.
*     fplane_y = double* (Given)
*        Lookup table (LUT) specifying y-focal plane offset for each of
*        n_pix pixels in radians. Ignored if cache from previous call exists.
*     n_pix = int (Given)
*        Number of pixels in fplane_* LUTs. Ignored if cache from previous 
*        call exists.
*     state = JCMTState* (Given)
*        Current JCMT state (time, pointing etc.)
*     instap = double[2] (Given)
*        Additional focal plane offsets that may be applied.
*     telpos = double[3] (Given)
*        LON / Lat / altitude of the telscope (deg/deg/metres)
*     steptime = double (Given)
*        The value of the STEPTIME FITS header holding the exposure time,
*        in seconds.
*     fset = AstFrameSet** (Returned)
*        Constructed frameset.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Build an AST frameset containing mappings from JCMT instrument
*     pixels to to celestial coordinates. The fplane_x/y LUTs are
*     used to specify arbitrary instruments on the JCMT.
*
*     This function allocates static resources (AST object pointers)
*     which should be freed when no longer needed by calling this
*     function with "clearcache" set to 1. When this is done, the
*     cached resources are freed.  If the static part of the
*     transformation has been cached from a previous call, and
*     "clearcache" is not set, the fplane_*, and n_pix parameters get
*     ignored. If "clearcache" is set to 1 and NULL pointers are given
*     for the fplane_x/y, the routine returns, setting the fset to NULL.

*  Authors:
*     Edward Chapin (UBC)
*     B.D.Kelly (bdk@roe.ac.uk)
*     Tim Jenness (timj@jach.hawaii.edu)
*     D.S. Berry (dsb@ast.man.ac.uk)
*     {enter_new_authors_here}

*  History:
*     2006-07-11 (EC):
*        Initial version duplicated from sc2ast_createwcs
*     2006-08-02 (EC):
*        - Renamed to smf_create_lutwcs, generic routine for JCMT instruments
*        - Change API to take JCMTState
*        - Provide focal plane pixel offsets with LUTs
*        - Add SMU chop offsets + instap
*     2006-09-07 (EC):
*        - Added telpos argument
*     2006-09-08 (EC):
*        - Fixed Longitude sign error
*     2006-09-11 (EC):
*        - map_cache was not getting pre-pended properly to transformation
*        - Only apply intrument aperture offset if non-null
*     2006-09-19 (DSB):
*        - Test "instap" rather than "instapmap" before creating instapmap.
*     2006-09-20 (EC):
*        - In jigglemap case mapping was not being appended to mapcache
*        - Check for VAL__BADD SMU offsets before using in jigglemap
*        - Convert SMU offsets to radians from arcsec before using
*     2006-09-21 (DSB):
*        - Modified to use a PermMap instead of a pair of LutMaps if
*          there is only 1 detector.
*        - The returned FrameSet pointer is now exempted completely from 
*          AST context handling rather than being exported to the parent 
*          context. This is because the pointer may need to be referenced 
*          in higher level contexts.
*        - Correct SMU offsets arcsec->rad conversion.
*     2006-11-01 (DSB):
*        Added steptime.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


#include <stdio.h>
#include <math.h>
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "prm_par.h"

/* Data Acquisition Includes */
#include "sc2da/sc2ast.h"


/* SMURF includes */
#include "smurf_par.h"
#include "smf.h"
#include "jcmt/state.h"

#define FUNC_NAME "smf_create_lutwcs"

#define SPD 86400.0                    /* Seconds per day */

void smf_create_lutwcs( int clearcache, const double *fplane_x, 
			const double *fplane_y, const int n_pix, 
			const JCMTState *state, const double instap[2], 
                        const double telpos[3], double steptime, 
                        AstFrameSet **fset, int *status ) {

  /* Local Variables */
  AstMapping *azelmap;            /* tangent plane to spherical azel mapping */
  AstShiftMap *instapmap;         /* Mapping for focal plane shift */
  int haveLUT;                    /* Set if LUTs given */
  AstShiftMap *jigglemap;         /* account for offsets in tangent plane */
  AstMapping *mapping;            /* total pixel -> azel mapping */
  double shifts[ 2 ];             /* size of shifts for jigglemap */ 

  double temp_jig_x=0;            /* SMU x-offset */
  double temp_jig_y=0;            /* SMU y-offset */
  double temp_chop_x=0;           /* SMU chop x-offset */
  double temp_chop_y=0;           /* SMU chopy x-offset */

  /* Required only for LUTs */
  AstLutMap *azlutmap;
  AstLutMap *ellutmap;
  AstMapping *azellutmap;
  AstMatrixMap *rmap;
  AstPermMap *permmap;
  double constants[ 2 ];
  double rmat[ 4 ];
  int inperm[2];
  int outperm[2];

  /* A cache containing a FrameSet and a Mapping. The 
     FrameSet will contain a single Frame representing BOLO # in the 
     array. The result of applying the Mapping to this Frame will be 
     Cartesian (i.e. in the tangent plane) AzEl coords in rads. The 
     AST pointers in this cache are exempted from AST context handling, and
     so need to be released explicitly using astAnnul. This is done by 
     calling this function with the sub-frame number set to -1. 
  */


  static AstMapping *map_cache = NULL;
  static AstFrameSet *frameset_cache = NULL; 

  /* Cache the SkyFrame used to represent final spherical (Az,El) coords */
  static AstSkyFrame *skyframe = NULL;

  /* Cache used to hold Mappings needed in the tangent plane to celestial
     longitude,latitude Mapping. */
  static AstMapping *azel_cache[ 2 ] = { NULL, NULL };


  /* Main routine */

  /* Check that the caller supplied information for the LUTs */
  if( (fplane_x != NULL) && (fplane_y != NULL) && (n_pix >= 1) ) {
    haveLUT = 1;
  } else {
    haveLUT = 0;
  }

  /* Check the clearcache flag. If it is 1, free the cached AST object. 
     Otherwise, report an error if the value is illegal. We do
     this before checking the inherited status so that the memory is freed
     even if an error has occurred. */

  if( clearcache ) {
    if( map_cache ) map_cache = astAnnul( map_cache );
    if( frameset_cache ) frameset_cache = astAnnul( frameset_cache );
    if( azel_cache[ 0 ] ) azel_cache[ 0 ] = astAnnul( azel_cache[ 0 ] );
    if( azel_cache[ 1 ] ) azel_cache[ 1 ] = astAnnul( azel_cache[ 1 ] );
    if( skyframe ) skyframe = astAnnul( skyframe );

    /* If the LUT information is NULL, just return here because the purpose
       of the call was only to clear the cache */

    if( !haveLUT ) return;
  } 

  /* Now initialise the returned pointer and check the inherited status */
  *fset = AST__NULL;
  if ( *status != SAI__OK ) return;

  /* Start an AST context. This means we do not need to worry about
     annulling AST objects. Note, there should be no "return" statements
     before the matching call to astEnd. */
  astBegin;
  
  /* The Mapping from pixel number to AzEl coords can be thought of as
     divided into two parts; the early part which goes from pixel # to
     boresight focal plane offsets, and the later part which goes from
     focal plane boresight offsets to spherical AzEl coords. The
     nature of the early part is fixed for the instrument and does not
     depend on the JCMTState. Therefore we can create the early part
     once and cache them for later use. The later part depends on the
     JCMTState parameters and so cannot be cached. Create the early
     part of the required Mapping if it has not already been
     created. The cached Mapping transforms positions within the Frame
     encapsulated within the cached FrameSet into Tanplane focal
     plane offsets in radians. */

  if( !map_cache ) {

    /* Check that the LUTs were specified! */
    if( haveLUT ) {

      /* Create an AST frame describing GRID coordinates within the instrument
         and put it into the cached FrameSet for this subarray. The centre of
         the first pixel has coords (1.0,1.0) in the GRID Frame. */
      frameset_cache = astFrameSet( astFrame ( 2, "Domain=GRID" ), 
                                    "" );   
    
      /* We add a dummy 2D Frame to the FrameSet so that there is a Frame to
         remove on the first call to astRemoveFrame below. */
      astAddFrame( frameset_cache, AST__BASE, 
                   astUnitMap( 2, "" ), astFrame( 2, "" ) );

      /* Start LUT-specific code */    

      /* The first coordinate is the pixel number, and the second is a
         dummy dimension. Use a permMap to duplicate the first
         dimension and throw away the second. */
      inperm[0] = 0;  /* Inverse transformation not defined */
      inperm[1] = 0;
      outperm[0] = 1;
      outperm[1] = 1;
      permmap = astPermMap( 2, inperm, 2, outperm, NULL, "" );
      map_cache = (AstMapping *) permmap;
    
      /* LUTs give the focal plane Tanplane offsets based on pixel number.
         Connect two LUTs in parallel with a cmpMap to add after the permmap.
         If the supplied tables contain only 1 entry, then use a PermMap
         that assigns constant values to its outputs, instead of two LutMaps, 
         since a LutMap must have at least 2 table entries. */
      if( n_pix > 1 ) {
         azlutmap = astLutMap( n_pix, fplane_x, 1, 1, "" );
         ellutmap = astLutMap( n_pix, fplane_y, 1, 1, "" );
         azellutmap = (AstMapping *) astCmpMap( azlutmap, ellutmap, 0, "" );
      } else {
         outperm[ 0 ] = -1;
         outperm[ 1 ] = -2;
         constants[ 0 ] = fplane_x[ 0 ];
         constants[ 1 ] = fplane_y[ 0 ];
         azellutmap = (AstMapping *) astPermMap( 2, NULL, 2, outperm, 
                                                 constants, "" );
      }
      map_cache = (AstMapping *) astCmpMap( map_cache, azellutmap, 1, "" );

      /* End LUT-specific code */

      /* Apply focal plane ("instrument aperture") offsets */
      if( instap ) {
	instapmap = astShiftMap( 2, instap, "" );
	map_cache = (AstMapping *) astCmpMap( map_cache, instapmap, 1, "" );
      }

      /* Simplify the Cached Mapping. */
      map_cache = astSimplify( map_cache );
      
      /* Exempt the cached AST objects from AST context handling. This means
         that the pointers will not be annulled as a result of calling astEnd. 
         Therefore the objects need to be annulled explicitly when no longer
         needed. this is done by calling this function with "subnum" set to 
         -1.*/
      astExempt( map_cache );
      astExempt( frameset_cache );

    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Can't make cached mapping because no LUT specified.",
	     status);
    }
  }


  if( *status == SAI__OK ) {

    /* Create a Mapping that rotates focal plane coords so that the Y
       axis is parallel to the projection of the elevation axis. */

    rmat[ 0 ] =  cos( state->tcs_az_ang );
    rmat[ 1 ] =  sin( state->tcs_az_ang );
    rmat[ 2 ] = -rmat[ 1 ];
    rmat[ 3 ] = rmat[ 0 ];
    rmap = astMatrixMap( 2, 2, 0, rmat, "" );


    /* Create a Mapping from tanplane AzEl coords (in rads) to spherical
       AzEl coords (in rads). */

    azelmap = sc2ast_maketanmap( state->tcs_az_ac1, state->tcs_az_ac2,
				 azel_cache, 0, status );
  

    /* Get the SMU positional values. Any "bad" value gets set to 0 before
     using it to calculate the jigglemap. If the values are good, convert
    them to radians from arcsec */

    if( state->smu_az_jig_x == VAL__BADD ) temp_jig_x = 0;
    else temp_jig_x = state->smu_az_jig_x/DR2AS;

    if( state->smu_az_jig_y == VAL__BADD ) temp_jig_y = 0;
    else temp_jig_y = state->smu_az_jig_y/DR2AS;

    if( state->smu_az_chop_x == VAL__BADD ) temp_chop_x = 0;
    else temp_chop_x = state->smu_az_chop_x/DR2AS;

    if( state->smu_az_chop_y == VAL__BADD ) temp_chop_y = 0;
    else temp_chop_y = state->smu_az_chop_y/DR2AS;
    

    /* Calculate final mapping with SMU position correction only if needed */
    if( (!temp_jig_x) && (!temp_jig_y) && (!temp_chop_x) && (!temp_chop_y) ) {
    
      /* Combine these with the cached Mapping (from GRID coords for subarray 
         to Tanplane Nasmyth coords in rads), to get total Mapping from GRID 
         coords to spherical AzEl in rads. */

      mapping = (AstMapping *) astCmpMap( map_cache, 
                                          astCmpMap( rmap, azelmap, 1, "" ),
                                          1, "" );    

    } else {
      /* Create a ShiftMap which moves the origin of projection plane (X,Y)
         coords to take account of the small offsets of SMU jiggle pattern. 
         Add this shifted map to the static cached mapping x*/

      shifts[ 0 ] = temp_jig_x + temp_chop_x;
      shifts[ 1 ] = temp_jig_y + temp_chop_y;
      jigglemap = astShiftMap( 2, shifts, "" );
    
      mapping = (AstMapping *) astCmpMap( map_cache, 
                                          astCmpMap( rmap,
                                                     astCmpMap( jigglemap, azelmap, 
                                                                1, "" ), 
                                                     1, "" ),
                                          1, "" );
    }
  
    /* If not already created, create a SkyFrame describing (Az,El). Hard-wire 
       the geodetic longitude and latitude of JCMT into this Frame. Note, the 
       Epoch value should be TDB, but we supply TT (=TAI+32.184 sec) instead 
       since the difference is only 1-2 milliseconds. We cache the created 
       SkyFrame to avoid the overhead of constantly re-creating it. The Epoch 
       is set every time though since this will vary from call to call. */
    if( !skyframe ) {
      skyframe = astSkyFrame ( "system=AzEl" );

      /* Ast assumes longitude increases eastward, so change sign to
	 be consistent with smf_calc_telpos here */
      astSetD( skyframe, "ObsLon", -telpos[0] );
      astSetD( skyframe, "ObsLat", telpos[1] );

      astExempt( skyframe );
    }

    /* Set the date and time at the middle of the observation. Use TCS_TAI 
       values if available, otherwise use RTS_END. Note RTS_END values
       refer to the end of the observation, so subtact half the exposure 
       time from the end time. Remember to convert from TAI to TDB (as
       required by the Epoch attribute). */
    if( state->tcs_tai != VAL__BADD ) {
       astSet( skyframe, "Epoch=MJD %.*g", DBL_DIG, state->tcs_tai + 
                                               32.184/SPD ); 
    } else {
       astSet( skyframe, "Epoch=MJD %.*g", DBL_DIG, state->rts_end +
                                               ( 32.184 - 0.5*steptime)/SPD ); 
    }

    /* Now modify the cached FrameSet to use the new Mapping and SkyFrame.
       First remove the existing current Frame and then add in the new one.
       Note, we add a copy of the SkyFrame rather than the cached SkyFrame 
       itself since the SkyFrame contained in the FrameSet will be modified 
       by later functions.  */
    astRemoveFrame( frameset_cache, AST__CURRENT );
    astAddFrame( frameset_cache, AST__BASE, mapping, 
                 astCopy( skyframe ) );

    /* Return the final FrameSet. */
    *fset = astClone( frameset_cache );
  }

  /* Exempt the returned FrameSet pointer, and then end the AST context. This
     will annul all AST objects created since the matching call to astBegin,
     except for those which have been exported using astExport or exempted
     using astExempt. The use of AST contexts requires that the function
     does not exit prematurely before reaching the astEnd call. Therefore 
     there should usually no "return" statements within the body of the AST
     context. Note, we exempt this pointer from the AST context system rather 
     than exporting it to the parent context because we do not know
     when, or in which context, it will be used. It will be annulled
     either in smf_tslice_ast or in smf_close_file. */
  astExempt( *fset );
  astEnd;

}
