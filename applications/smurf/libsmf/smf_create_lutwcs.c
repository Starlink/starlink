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
*     result = smf_create_lutwcs( int clearcache, const double *fplane_x,
*                                 const double *fplane_y, const int n_pix,
*                                 const JCMTState *state, double dut1,
*                                 double dtai, const double instap[2],
*                                 const double telpos[3], AstFrameSet **fset,
*                                 CreateLutwcsCache *cache, int *status )

*  Arguments:
*     clearcache = int (Given)
*        If set to 1 and fixed mappings have previously been cached, clear them
*     fplane_x = const double* (Given)
*        Lookup table (LUT) specifying x-focal plane offset for each of
*        n_pix pixels in radians. Ignored if cache from previous call exists.
*     fplane_y = const double* (Given)
*        Lookup table (LUT) specifying y-focal plane offset for each of
*        n_pix pixels in radians. Ignored if cache from previous call exists.
*     n_pix = int (Given)
*        Number of pixels in fplane_* LUTs. Ignored if cache from previous
*        call exists.
*     state = JCMTState* (Given)
*        Current JCMT state (time, pointing etc.) If NULL is supplied,
*        the returned FrameSet describes focal plane coords, in radians.
*     dut1 = double (Given)
*        DUT1 correction in seconds.
*     instap = const double[2] (Given)
*        Additional focal plane offsets that may be applied (arc-seconds).
*     telpos = const double[3] (Given)
*        LON / Lat / altitude of the telscope (deg/deg/metres)
*     fset = AstFrameSet** (Returned)
*        Constructed frameset.
*     cache = smfCreateLutwcsCache * (Given)
*        Pointer to a structure holding cached information created by a
*        previous call to this function, or a NULL pointer if this is the
*        first invocation of this function.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to  structure holding cached information used by this
*     function. If a non-Null value is supplied for "cache", then the
*     returned function value will be the supplied "cache" pointer.
*     Otherwise, it will be a pointer to a newly allocated cache structure.

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
*     2007-02-20 (DSB):
*        Clear the cache if the INSTAP values change.
*     2008-04-09 (TIMJ):
*        No longer need steptime (tcs_tai vs rts_end handled elsewhere)
*     2008-12-3 (DSB):
*        Avoid use of static cache.
*     2008-12-16 (DSB):
*        For extra speed, clone the cached SkyFrame rather than copying it.
*     2009-01-13 (TIMJ):
*        Add dut1 argument.
*     2009-04-3 (DSB):
*        Negate usage of instap values.
*     2009-124-14 (DSB):
*        Allow "state" to be NULL in order to request a focal plane
*        FrameSet.
*     2013-6-27 (DSB):
*        Return FrameSet that give sbad output values if any of the required state
*        information is bad.
*     2015-07-07 (DSB):
*        Indicate that sky separations below 0.05 arc-seconds (SC2AST__SKYTOL)
*        are insignificant.
*     2017-01-10 (GSB):
*        Add "dtai" argument.
*     2017-04-06 (GSB):
*        Set dtai in skyframe if present.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006 University of British Columbia.
*     Copyright (C) 2015-2017 East Asian Observatory.
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

smfCreateLutwcsCache *smf_create_lutwcs( int clearcache, const double *fplane_x,
                                         const double *fplane_y, const int n_pix,
                                         const JCMTState *state, double dut1,
                                         double dtai, const double instap[2],
                                         const double telpos[3], AstFrameSet **fset,
                                         smfCreateLutwcsCache *cache, int *status ) {

  /* Local Variables */
  AstMapping *azelmap;            /* tangent plane to spherical azel mapping */
  AstShiftMap *instapmap;         /* Mapping for focal plane shift */
  int haveLUT;                    /* Set if LUTs given */
  AstShiftMap *jigglemap;         /* account for offsets in tangent plane */
  AstMapping *mapping;            /* total pixel -> azel mapping */
  double shifts[ 2 ];             /* size of shifts for jigglemap */
  smfCreateLutwcsCache *result;   /* Returned cache pointer */

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
  AstFrame *fp_pos_frame;


  /* Main routine */

  /* Check that the caller supplied information for the LUTs */
  if( (fplane_x != NULL) && (fplane_y != NULL) && (n_pix >= 1) ) {
    haveLUT = 1;
  } else {
    haveLUT = 0;
  }

  /* Check the clearcache flag. If it is 1, free the cached AST objects
     and then free the cache itself. Otherwise, report an error if the value
     is illegal. We do this before checking the inherited status so that the
     memory is freed even if an error has occurred. */

  if( clearcache && cache ) {
    if( cache->map ) cache->map = astAnnul( cache->map );
    if( cache->frameset ) cache->frameset = astAnnul( cache->frameset );
    if( cache->azel[ 0 ] ) cache->azel[ 0 ] = astAnnul( cache->azel[ 0 ] );
    if( cache->azel[ 1 ] ) cache->azel[ 1 ] = astAnnul( cache->azel[ 1 ] );
    if( cache->skyframe ) cache->skyframe = astAnnul( cache->skyframe );
    cache = astFree( cache );

    /* If the LUT information is NULL, just return here because the purpose
       of the call was only to clear the cache */

    if( !haveLUT ) return NULL;
  }

  /* Now initialise the returned pointers and check the inherited status */
  result = cache;
  if( fset ) *fset = AST__NULL;
  if ( *status != SAI__OK || !fset ) return result;

  /* Start an AST context. This means we do not need to worry about
     annulling AST objects. Note, there should be no "return" statements
     before the matching call to astEnd. */
  astBegin;

/* If no cache was supplied, allocate memory for one now and initialise it.
   The cache contains a FrameSet and a Mapping. The FrameSet will contain a
   single Frame representing BOLO # in the array. The result of applying
   the Mapping to this Frame will be Cartesian (i.e. in the tangent plane)
   AzEl coords in rads. The AST pointers in this cache are exempted from
   AST context handling, and so need to be released explicitly using
   astAnnul. This is done by calling this function with "clearcache"
   set non-zero. */
   if( !cache ) {
      cache = astMalloc( sizeof( *cache ) );
      if( cache ) {
         cache->map = NULL;
         cache->frameset = NULL;
         cache->skyframe = NULL;
         cache->azel[ 0 ] = NULL;
         cache->azel[ 1 ] = NULL;
         cache->instap[ 0 ] = AST__BAD;
         cache->instap[ 1 ] = AST__BAD;
         result = cache;

      } else {
         *status = SAI__ERROR;
         errRep( FUNC_NAME, FUNC_NAME": Can't allocate memory for cache.",
	         status);
         return NULL;
      }
   }

  /* See if either of the instap values has changed. If so, clear the
     cache contents since the old instap values were hard-wired into it. */
  if( ( instap && ( instap[ 0 ] != cache->instap[ 0 ] ||
                     instap[ 1 ] != cache->instap[ 1 ] ) ) ||
      ( !instap && ( 0.0 != cache->instap[ 0 ] ||
                     0.0 != cache->instap[ 1 ] ) ) ){

    if( cache->map ) cache->map = astAnnul( cache->map );
    if( cache->frameset ) cache->frameset = astAnnul( cache->frameset );
    if( cache->azel[ 0 ] ) cache->azel[ 0 ] = astAnnul( cache->azel[ 0 ] );
    if( cache->azel[ 1 ] ) cache->azel[ 1 ] = astAnnul( cache->azel[ 1 ] );
    if( cache->skyframe ) cache->skyframe = astAnnul( cache->skyframe );
  }

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

  if( !cache->map ) {

    /* Check that the LUTs were specified! */
    if( haveLUT ) {

      /* Create an AST frame describing GRID coordinates within the instrument
         and put it into the cached FrameSet for this subarray. The centre of
         the first pixel has coords (1.0,1.0) in the GRID Frame. */
      cache->frameset = astFrameSet( astFrame ( 2, "Domain=GRID" ),
                                    " " );

      /* We add a 2D Frame descirbing focal plane positions to the FrameSet.
         If not telescope state is supplied this frame will be retained
         as the current Frame on exit. If a telescope state is supplied, it
         will be replaced below by a suitable SkyFrame */
      fp_pos_frame = astFrame( 2, "Unit(1)=rad,Unit(2)=rad,Domain=FPLANE"
  			          ",label(1)=FplaneX,label(2)=FplaneY" );
      astSetActiveUnit( fp_pos_frame, 1 );
      astAddFrame( cache->frameset, AST__BASE, astUnitMap( 2, " " ),
                   fp_pos_frame );

      /* Start LUT-specific code */

      /* The first coordinate is the pixel number, and the second is a
         dummy dimension. Use a permMap to duplicate the first
         dimension and throw away the second. */
      inperm[0] = 0;  /* Inverse transformation not defined */
      inperm[1] = 0;
      outperm[0] = 1;
      outperm[1] = 1;
      permmap = astPermMap( 2, inperm, 2, outperm, NULL, " " );
      cache->map = (AstMapping *) permmap;

      /* LUTs give the focal plane Tanplane offsets based on pixel number.
         Connect two LUTs in parallel with a cmpMap to add after the permmap.
         If the supplied tables contain only 1 entry, then use a PermMap
         that assigns constant values to its outputs, instead of two LutMaps,
         since a LutMap must have at least 2 table entries. */
      if( n_pix > 1 ) {
         azlutmap = astLutMap( n_pix, fplane_x, 1, 1, " " );
         ellutmap = astLutMap( n_pix, fplane_y, 1, 1, " " );
         azellutmap = (AstMapping *) astCmpMap( azlutmap, ellutmap, 0, " " );
      } else {
         outperm[ 0 ] = -1;
         outperm[ 1 ] = -2;
         constants[ 0 ] = fplane_x[ 0 ];
         constants[ 1 ] = fplane_y[ 0 ];
         azellutmap = (AstMapping *) astPermMap( 2, NULL, 2, outperm,
                                                 constants, " " );
      }
      cache->map = (AstMapping *) astCmpMap( cache->map, azellutmap, 1, " " );

      /* End LUT-specific code */

      /* Apply focal plane ("instrument aperture") offsets */
      if( instap ) {
	instapmap = astShiftMap( 2, instap, " " );
	cache->map = (AstMapping *) astCmpMap( cache->map, instapmap, 1, " " );

        cache->instap[ 0 ] = instap[ 0 ];
        cache->instap[ 1 ] = instap[ 1 ];
      } else {
        cache->instap[ 0 ] = 0.0;
        cache->instap[ 1 ] = 0.0;
      }

      /* Simplify the Cached Mapping. */
      cache->map = astSimplify( cache->map );

      /* Exempt the cached AST objects from AST context handling. This means
         that the pointers will not be annulled as a result of calling astEnd.
         Therefore the objects need to be annulled explicitly when no longer
         needed. this is done by calling this function with "subnum" set to
         SC2AST__NULLSUB.*/
      astExempt( cache->map );
      astExempt( cache->frameset );

    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Can't make cached mapping because no LUT specified.",
	     status);
    }
  }


  if( *status == SAI__OK ) {

    /* If no state pointer has been supplied, remap the focal plane Frame
       using the cached Mapping, and return a clone of the cached
       FrameSet describing the relationship between GRID and focal plane
       coords */
    if( !state ) {
      astRemapFrame( cache->frameset, AST__CURRENT, cache->map );
      *fset = astClone( cache->frameset );

    /* Otherwise, create a Mapping that rotates focal plane coords so that
       the Y axis is parallel to the projection of the elevation axis. */
    } else {

      /* If any of the required state information is bad, use a Mapping
         that generates bad values. */
      if( state->tcs_az_ac1 == VAL__BADD ||
          state->tcs_az_ac2 == VAL__BADD ||
          state->tcs_az_ang == VAL__BADD ||
          state->tcs_tai == VAL__BADD ) {
         inperm[ 0 ] = 0;
         inperm[ 1 ] = 0;
         outperm[ 0 ] = 0;
         outperm[ 1 ] = 0;
         mapping = (AstMapping *) astPermMap( 2, inperm, 2, outperm,
                                             NULL, " " );

      /* Otherwise reate the mapping from focal plane to sky */
      } else {
         rmat[ 0 ] =  cos( state->tcs_az_ang );
         rmat[ 1 ] =  sin( state->tcs_az_ang );
         rmat[ 2 ] = -rmat[ 1 ];
         rmat[ 3 ] = rmat[ 0 ];
         rmap = astMatrixMap( 2, 2, 0, rmat, " " );

         /* Create a Mapping from tanplane AzEl coords (in rads) to spherical
            AzEl coords (in rads). */
         azelmap = sc2ast_maketanmap( state->tcs_az_ac1, state->tcs_az_ac2,
     	                              cache->azel, status );

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

           mapping = (AstMapping *) astCmpMap( cache->map,
                                               astCmpMap( rmap, azelmap, 1, " " ),
                                               1, " " );

         } else {
           /* Create a ShiftMap which moves the origin of projection plane (X,Y)
              coords to take account of the small offsets of SMU jiggle pattern.
              Add this shifted map to the static cached mapping x*/

           shifts[ 0 ] = temp_jig_x + temp_chop_x;
           shifts[ 1 ] = temp_jig_y + temp_chop_y;
           jigglemap = astShiftMap( 2, shifts, " " );

           mapping = (AstMapping *) astCmpMap( cache->map,
                                               astCmpMap( rmap,
                                                          astCmpMap( jigglemap, azelmap,
                                                                     1, " " ),
                                                          1, " " ),
                                               1, " " );
         }
      }

      /* If not already created, create a SkyFrame describing (Az,El). Hard-wire
         the geodetic longitude and latitude of JCMT into this Frame. Note, the
         Epoch value should be TDB, but we supply TT (=TAI+32.184 sec) instead
         since the difference is only 1-2 milliseconds. We cache the created
         SkyFrame to avoid the overhead of constantly re-creating it. The Epoch
         is set every time though since this will vary from call to call. */
      if( !cache->skyframe ) {
        cache->skyframe = astSkyFrame ( "system=AzEl,SkyTol=%g", SC2AST__SKYTOL );

        /* Ast assumes longitude increases eastward, so change sign to
  	 be consistent with smf_calc_telpos here */
        astSetD( cache->skyframe, "ObsLon", -telpos[0] );
        astSetD( cache->skyframe, "ObsLat", telpos[1] );

        astExempt( cache->skyframe );
      }

      /* Set the date and time at the middle of the observation. Use TCS_TAI
         values corresponding to the centre of the integration and corresponding
         to the astrometry calculation. Remember to convert from TAI to TDB (as
         required by the Epoch attribute). */
      astSet( cache->skyframe, "Epoch=MJD %.*g, dut1=%.*g",
              DBL_DIG, state->tcs_tai + 32.184/SPD,
              DBL_DIG, dut1 );
      if (dtai != VAL__BADD) {
        astSetD(cache->skyframe, "Dtai", dtai);
      }

      /* Now modify the cached FrameSet to use the new Mapping and SkyFrame.
         First remove the existing current Frame and then add in the new one.
         Note, we add a copy of the SkyFrame rather than the cached SkyFrame
         itself since the SkyFrame contained in the FrameSet will be modified
         by later functions.  */
      astRemoveFrame( cache->frameset, AST__CURRENT );
      astAddFrame( cache->frameset, AST__BASE, mapping,
                   astCopy( cache->skyframe ) );

      /* Return the final FrameSet. */
      *fset = astClone( cache->frameset );
    }
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

  return result;
}
