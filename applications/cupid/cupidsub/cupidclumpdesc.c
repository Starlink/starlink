#include "sae_par.h"
#include "cupid.h"
#include "ndf.h"
#include "ast.h"
#include "star/hds.h"
#include "star/kaplibs.h"
#include "mers.h"
#include "prm_par.h"
#include <math.h>
#include <string.h>

/* The maximum number of columns in the catalogue */
#define MXPAR 30

/* The maximum length of a column name. */
#define MXNAMLEN 30

/* The square root of two. */
#define ROOT_TWO 1.4142136

/* Prototype for a Channel sink function that writes to dynemic memory. */
static void cupidMemSink( const char * );

double *cupidClumpDesc( int indf, int deconv, AstMapping *wcsmap,
                        AstFrame *wcsfrm, const char *dataunits,
                        double beamcorr[ 3 ], int backoff, int shape,
                        int velax, double *cpars, const char ***names,
                        const char ***units, int *ncpar, int *ok,
                        char **stcs, AstRegion **region, int *status ){
/*
*+
*  Name:
*     cupidClumpDesc

*  Purpose:
*     Get the parameters describing a single clump.

*  Language:
*     Starlink C

*  Synopsis:
*     double *cupidClumpDesc( int indf, int deconv, AstMapping *wcsmap,
*                             AstFrame *wcsfrm, const char *dataunits,
*                             double beamcorr[ 3 ], int backoff, int shape,
*                             int velax, double *cpars, const char ***names,
*                             const char ***units, int *ncpar, int *ok,
*                             char **stcs, AstRegion **region, int *status )

*  Description:
*     This function calculates the parameters describing a single clump,
*     on the basis of the clump data values in the supplied NDF. If the
*     NDF has "n" pixel axes, the parameters are returned in the following
*     order:
*
*        0  - (n-1) : The pixel or WCS coords of the clump peak value
*        n  - (2n-1): The pixel or WCS coords of the clump centroid
*        2n - (3n-1): The clump size (in pixels or WCS units) on each axis.
*                     This is the standard deviation of the pixel axis value
*                     about the centroid position, weighted by the pixel
*                     values, then corrected to remove the effect of the
*                     instrumental smoothing specified in "beamcorr" (but
*                     only if "deconv" is non-zero).
*        3n         : The total data value in the clump
*        3n + 1     : The peak value in the clump. If "deconv" is non-zero,
*                     this will be larger than the peak data value by a factor
*                     determined by the "beamcorr" values, to take account
*                     of the lowering of the peak value caused by the
*                     instrumental smoothing.
*        3n + 2     : The total volume of the clump (in pixels or WCS units).
*
*     If the supplied NDF has a CUPID extension, then the names of the
*     components within the CUPID extension are used as additional column
*     names, following the above names. All components in the CUPID extension
*     should be scalar primitive numerical values.
*
*     The first invocation of this function may allocate static
*     resources, which should be released when no longer needed by
*     invoking this function one further time with "indf" set to NDF__NOID.

*  Parameters:
*     indf
*        Identifier for an NDF holding the data values associated with
*        the clump. Any pixels which are not part of the clump should be
*        set bad. If this is NDF__NOID, the function will just clean up
*        static resources and then return immediately, without any
*        further action.
*     deconv
*        If non-zero then the clump property values stored in the
*        catalogue and NDF are modified to remove the smoothing effect
*        introduced by the beam width. If zero, the undeconvolved values
*        are stored in the output catalogue and NDF. Note, the filter to
*        remove clumps smaller than the beam width is still applied, even
*        if "deconv" is zero.
*     wcsmap
*        If the output catalogue is to contain values in pixels
*        coordinates, then a NULL pointer should be supplied for "wcsmap".
*        Otherwise, a pointer to a Mapping from the input PIXEL Frame to
*        the WCS Frame should be supplied.
*     wcsfrm
*        A pointer to the current Frame in the WCS FrameSet of the input
*        NDF. Ignored if "wcsmap" is NULL.
*     dataunits
*        The units string describing the data units in the input NDF.
*     beamcorr
*        An array holding the FWHM (in pixels) describing the instrumental
*        smoothing along each pixel axis. If "deconv" is non-zero, the clump
*        widths and peak values stored in the output catalogue are modified
*        to correct for this smoothing.
*     backoff
*        If non-zero, then the background level is subtracted from all
*        clump data values before calculating the clump sizes and centroid.
*        The background level is the minimum data value in the clump. If
*        zero, then the clump sizes and centroid are based on the full data
*        values (this is what the IDL version of ClumpFind does).
*     shape
*        Indicates the shape to use when creating an STC-S description of
*        the spatial shape of the clump. Ignored if "stcs" is NULL.
*        Otherwise:
*           0 - No STC-S to be created ("stcs" is ignored)
*           1 - Use an ellipse to describe the spatial extent of the clump,
*               created using the old algorithm, based on analysis of
*               four marginal profiles at 45 degree intervals.
*           2 - Use a polygon to describe the spatial extent of the clump
*           3 - Use an ellipse to describe the spatial extent of the clump,
*               created by finding many marginal profiles at 1 degree
*               intervals and finding the longest.
*     velax
*        The zero-based index of the velocity pixel axis. Should be -1 if
*        there is no velocity axis.
*     cpars
*        Pointer to an array in which to store the clump parameters. If
*        this is NULL, a new dynamic array is allocated and a pointer to
*        it is returned as the function value. This array should have at
*        least "*ncpar" elements.
*     names
*        Pointer to a location at which to return a pointer to an array
*        of constant character strings. The number of pointers returned in
*        this array will be returned in "*ncpar". Each string is the name
*        associated with the corresponding parameter value returned in "cpars".
*     units
*        Pointer to a location at which to return a pointer to an array
*        of constant character strings. The number of pointers returned in
*        this array will be returned in "*ncpar". Each string is the unit
*        associated with the corresponding parameter value returned in
*        "cpars", and may be blank. Note, if "usewcs" is non-zero, sky
*        position axes have units of "deg" and sky size axes have units
*        of "arcsec".
*     ncpar
*        Pointer to an int in which to return the number of parameters
*        describing the clump.
*     ok
*        Pointer to an int in which to return a flag indicating if the
*        clump can be used or not. This will be set to zero if the clump
*        size is zero after correction for the effect of spatial beam
*        smoothing. It will be set to -1 if the clump size is zero after
*        correction for the effect of spectral resilution. It will be +1
*        if the clump size is larger than the beam on all axes.
*     stcs
*        A pointer to a location at which to return a pointer to a
*        dynamically allocated string containing an STC-S description of
*        the spatial extent of the clump. The memory containing the
*        returned string should be freed using astFree when no longer
*        needed. No STC-S description is created if a NULL value is
*        supplied for "stcs" or if "shape" is zero.
*     region
*        A pointer to a location at which to return a pointer to an AST
*        Region describing the outline of the clump, using the shape
*        specified by parameter "shape".
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to the array holding the returned clump parameters. This
*     will be the same as "cpars" if cpars is not NULL, or will be a
*     pointer to a newly allocated dynamic array otherwise (the array
*     should be freed using astFree when no longer needed).

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-DEC-2005 (DSB):
*        Original version.
*     11-DEC-2006 (DSB):
*        Added parameter "deconv".
*     16-DEC-2006 (DSB):
*        Added parameters "wcsmap", "wcsfrm", "dataunits" and "units".
*     26-JAN-2007 (DSB):
*        Assign units of "rad" to SkyAxis columns, so that they can be
*        converted to "deg" easily later on (using active units).
*     8-MAY-2007 (DSB):
*        Fix bug in conversion of non-sky axis values from pixel to wcs.
*     14-JUN-2007 (DSB):
*        Make "ncomp" static. Fixes spurious values appearing in the
*        extra columsn created using the ExtraCols config parameter.
*     27-NOV-2007 (DSB):
*        If the min and max value in the clump are equal, use uniform
*        weighting.
*     18-MAR-2008 (DSB):
*        Added argument "backoff" for Jenny Hatchell.
*     27-APR-2009 (DSB):
*        Added arguments "stcs" and "velax".
*     25-MAY-2009 (DSB):
*        Added argument "shape".
*     3-OCT-2012 (DSB):
*        Provide facility for cleaning up static resources.
*     11-NOV-2016 (DSB):
*        For consistency, use KPG to determine pixel scales.
*     18-APR-2017 (DSB):
*        Change centroid position from pixel indices to pixel coords.
*        This makes it consistent with the peak position.
*     8-DEC-2017 (DSB):
*        Added "shape=3" option, to avoid the long thin ellipses that
*        could be created using the old algorithm. Keep the old algorithm
*        as an option in case there is some problem with the new algorithm.
     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstFrame *axfrm;         /* 1D Frame containing current Axis only */
   AstFrame *pixfrm;        /* A Frame describing Pixel coords */
   AstFrameSet *fs;         /* Pixel -> WCS FrameSet */
   AstKeyMap *warnings;     /* KeyMap holding warnings generated by astWrite */
   AstRegion *reg;          /* Region describing spatial extent of clump */
   HDSLoc *cloc=NULL;       /* Locator to component of CUPID extension */
   HDSLoc *loc=NULL;        /* Locator to object holding extra column values */
   HDSLoc *xloc=NULL;       /* Locator to CUPID extension */
   char *vu;                /* Pointers to volume unit concatenation point */
   const char *warning;     /* Pointer to a warning string */
   double *ipd;             /* Pointer to start of data array */
   double *pd;              /* Pointer to next element of data array */
   double *ret;             /* Returned list of parameters */
   double at[3];            /* Pixel position at which to get pixel scales */
   double csw;              /* Clump size in WCS units */
   double d;                /* Height above background (the pixel weight) */
   double dmax;             /* Max value in data array */
   double dmin;             /* Min value in data array */
   double outlier[ 3 ];     /* Offset position */
   double peakfactor;       /* Factor by which to increase the peak value */
   double pixpos[ 3 ][ 5 ]; /* Pixel coord positions */
   double pixscl[ 3 ];      /* Pixel scales */
   double pixvol;           /* Volume of 1 cubic pixel in WCS units */
   double s;                /* Sum of data values */
   double sd;               /* Sum of weights */
   double su2;              /* Sum of weighted squared U pixel indices */
   double su;               /* Sum of weighted U axis pixel indices */
   double sv2;              /* Sum of weighted squared V pixel indices */
   double sv;               /* Sum of weighted V axis pixel indices */
   double sx2;              /* Sum of weighted squared X pixel indices */
   double sx;               /* Sum of weighted X pixel indices */
   double sy2;              /* Sum of weighted squared X pixel indices */
   double sy;               /* Sum of weighted Y pixel indices */
   double sz2;              /* Sum of weighted squared X pixel indices */
   double sz;               /* Sum of weighted Z pixel indices */
   double tt;               /* Temp value */
   double u;                /* U coord value */
   double v0;               /* Variance before corr'n for instrumental blurring */
   double v;                /* Variance after corr'n for instrumental blurring */
   double wcspos[ 3 ][ 5 ]; /* WCS coord positions */
   float sig[4];            /* Clump widths on X, U, Y and V axes */
   int i;                   /* Pixel index on 1st pixel axis */
   int icol;                /* Index into returned column arrays */
   int icomp;               /* Index into CUPID extension */
   int iwarn;               /* Warning index */
   int j;                   /* Pixel index on 2nd pixel axis */
   int k;                   /* Pixel index on 3rd pixel axis */
   int lbnd[ 3 ];           /* Lower NDF pixel bounds */
   int n;                   /* Number of good pixels indices */
   int ndim;                /* Number of pixel axes */
   int nel;                 /* Number of elements in mapped array */
   int nwarn;               /* Number of warnings to display */
   int outax[ 3 ];          /* Indices of spatial WCS axes */
   int px;                  /* X pixel index at peak value */
   int py;                  /* Y pixel index at peak value */
   int pz;                  /* Z pixel index at peak value */
   int there;               /* Has the NDF got a CUPID extension? */
   int ubnd[ 3 ];           /* Upper NDF pixel bounds */



   static AstFrame *pixel_frm = NULL;  /* 2D spatial PIXEL Frame */
   static AstFrame *space_frm = NULL;  /* 2D spatial WCS Frame */
   static AstMapping *space_map = NULL;/* 2D spatial WCS Mapping */
   static AstStcsChan *stcs_chan = NULL;     /* Creates STC-S descriptions */
   static char name_buf[ MXPAR ][ MXNAMLEN ];/* Buffers for parameter names */
   static char unit_buf[ 3 ][ 20 ];    /* Buffers for units strings */
   static char volunit_buf[ 60 ];      /* Buffer for volume units string */
   static const char *pnames[ MXPAR ]; /* Parameter names to return */
   static const char *punits[ MXPAR ]; /* Parameter units to return */
   static int ncomp;           /* No. of components in the CUPID extension */
   static int skyaxis[ 3 ];    /* Flags indicating which axes are skyaxes */
   static int space_axes[ 2 ]; /* Zero based indices of spatial pixel axes */
   static int warn = 1;        /* Display astWrite warnings? */

/* If requested clean up static resources and return (do this before
   checking inherited status so that resources are freed even if an error
   has already occurred). */
   if( indf == NDF__NOID ) {
      if( pixel_frm ) pixel_frm = astAnnul( pixel_frm );
      if( space_frm ) space_frm = astAnnul( space_frm );
      if( space_map ) space_map = astAnnul( space_map );
      if( stcs_chan ) stcs_chan = astAnnul( stcs_chan );
      return NULL;
   }

/* Initialise. */
   ret = cpars;
   *ok = 0;
   if( region ) *region = NULL;
   if( stcs ) *stcs = NULL;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* More initialisation. */
   reg = NULL;

/* STC-S does not currently support pixel coords, so ensure we do not
   attempt to create an STC-S Region unless we are reporting things in
   WCS coords. */
   if( !wcsmap ) v0 = 0;

/* Get the bounds of the NDF. */
   ndfBound(  indf, 3, lbnd, ubnd, &ndim, status );

/* If the NDF has a CUPID extension, get a locator to it and see if it
   contains an EXTRA structure. If so get a locator to it. */
   ndfXstat( indf, "CUPID", &there, status );
   if( there ) {
      ndfXloc( indf, "CUPID", "READ", &xloc, status );
      datThere( xloc, "EXTRA", &there, status );
      if( there ) datFind( xloc, "EXTRA", &loc, status );
   }

/* If no pointer was supplied, do some initialisation. */
   if( !ret ) {

/* If the NDF has extra columns in its CUPID extension, get a count of the
   components within it. Limit the number so that we do not have too many
   catalogue columns. */
      if( loc ) {
         datNcomp( loc, &ncomp, status );
         if( ncomp + ndim*3 + 3 > MXPAR ) ncomp = MXPAR - ndim*3 - 3;
      } else {
         ncomp = 0;
      }

/* Determine the number of numerical parameters needed to describe the clump.
   This includes the diagnostic values in the CUPID extension. */
      *ncpar = ndim*3 + 3 + ncomp;

/* Allocate memory for this number of numerical parameters. */
      ret = astMalloc( sizeof( double )*( *ncpar ) );

/* Now create the parameter names. */
      pnames[ 0 ] = "Peak1";
      if( ndim > 1 ) pnames[ 1 ] = "Peak2";
      if( ndim > 2 ) pnames[ 2 ] = "Peak3";

      pnames[ ndim ] = "Cen1";
      if( ndim > 1 ) pnames[ 1 + ndim ] = "Cen2";
      if( ndim > 2 ) pnames[ 2 + ndim ] = "Cen3";

      pnames[ 2*ndim ] = "Size1";
      if( ndim > 1 ) pnames[ 1 + 2*ndim ] = "Size2";
      if( ndim > 2 ) pnames[ 2 + 2*ndim ] = "Size3";

      pnames[ 3*ndim ] = "Sum";
      pnames[ 1 + 3*ndim ] = "Peak";
      pnames[ 2 + 3*ndim ] = "Volume";

/* Store the extra column names taken from the CUPID extension. */
      icol = 2 + 3*ndim + 1;
      for( icomp = 1; icomp <= ncomp && *status == SAI__OK; icomp++, icol++ ) {
         datIndex( loc, icomp, &cloc, status );
         datName( cloc, name_buf[ icol ], status );
         pnames[ icol ] = name_buf[ icol ];

         for( j = 0; j < icol && *status == SAI__OK; j++ ) {

            if( astChrMatch( pnames[ icol ], pnames[ j ] ) ) {
               *status = SAI__ERROR;
               msgSetc( "NM", pnames[ icol ] );
               errRep( " ", "Column name ^NM already in use (programming "
                       "error).", status );
               break;
            }
         }

         datAnnul( &cloc, status );
      }

/* Return a pointer to the array. */
      *names = pnames;

/* Now store the parameter units. First, initialise a pointer to the place
   at which to store the next units string within the total units string
   describing the clump volume. */
      vu = volunit_buf;

/* Annul any existing spatial Mapping and Frame */
      if( space_frm ) space_frm = astAnnul( space_frm );
      if( space_map ) space_map = astAnnul( space_map );

/* Store the parameter units. First deal with cases where catalogue
   columns use WCS units. */
      if( wcsmap ) {

/* Loop round all pixel axes (it is assumed that there are the same
   number of WCS axes). */
         for( i = 1; ( i <= ndim ) && astOK; i++ ) {

/* We want the Domain associated with the current axis, but since the
   current Frame may be a CmpFrame with its own (different) domain name,
   we need to first extract the current axis from the WCS Frame. */
            axfrm = astPickAxes( wcsfrm, 1, &i, NULL );

/* See if the axis is a sky axis */
            skyaxis[ i - 1 ] = !strcmp( "SKY", astGetC( axfrm, "Domain" ) );

/* If so, the unit associated with the axis is set to "deg" rather
   that the normal "hh:mm:ss" style unit string returned for a SkyAxis.
   Also, set appropriate units for the size columns. */
            if( skyaxis[ i - 1 ] ) {
               strcpy( unit_buf[ i - 1 ], "deg" );
               punits[ i - 1 + 2*ndim ] = "arcsec";

/* Otherwise, copy the axis unit into a local buffer since AST will re-use its
   internal buffer in which astGetC returns the attribute value. */
            } else {
               strcpy( unit_buf[ i - 1 ], astGetC( axfrm, "Unit" ) );
               punits[ i - 1 + 2*ndim ] = unit_buf[ i - 1 ];
            }

/*  Annul the Frame pointer */
            axfrm = astAnnul( axfrm );

/* The two clump positions are specified in the axis units chosen above. */
            punits[ i - 1 ] = unit_buf[ i - 1 ];
            punits[ i - 1 + ndim ] = unit_buf[ i - 1 ];

/* Append the "size" unit to the end of the volume unit string, followed
   by a dot if this is not the last axis. */
            strcpy( vu, punits[ i - 1 + 2*ndim ] );
            vu += strlen( punits[ i - 1 + 2*ndim ] );
            if( i != ndim ) {
               strcpy( vu, "." );
               vu++;
            }
         }

/* Stuff neeed to create an STC-S description of the spatial clump
   extent. */
         if( stcs && shape > 0 ) {

/* Create an StcsChan. */
            if( ! stcs_chan ) stcs_chan = astStcsChan( NULL, cupidMemSink,
                                                       " " );

/* Create 2D PIXEL frame within which the clump shape will be defined. */
            if( ! pixel_frm ) pixel_frm = astFrame( 2, "Domain=PIXEL" );

/* Store the  zero based indices of the spatial pixel axes. */
            if( velax == 0 ) {
               space_axes[ 0 ] = 2;
               space_axes[ 1 ] = 3;

            } else if( velax == 1 ) {
               space_axes[ 0 ] = 1;
               space_axes[ 1 ] = 3;

            } else {
               space_axes[ 0 ] = 1;
               space_axes[ 1 ] = 2;
            }

/* Attempt to split off the pixel->WCS mapping for the spatial axes. */
            astMapSplit( wcsmap, 2, space_axes, outax, &space_map );

/* Check any split Mapping can be used. Annul it if not. */
            if( space_map ){
               if( astGetI( space_map, "Nout" ) != 2 ) {
                  space_map = astAnnul( space_map );

/* If it can, get a pointer to the 2D spatial WCS Frame. */
               } else {
                  space_frm = astPickAxes( wcsfrm, 2, outax, NULL );
               }
            }

/* The values in "space_axes" are curently one-based (as required by
   astMapSplit). Change them to zero based for further use. */
            space_axes[ 0 ]--;
            space_axes[ 1 ]--;
         }

/* Now deal with cases where the output catalogue contains column values in
   units of pixels. */
      } else {
         for( i = 0; i < ndim; i++ ) {
            punits[ i ] = "pixel";
            punits[ i + ndim ] = "pixel";
            punits[ i + 2*ndim ] = "pixel";

            strcpy( vu, "pixel" );
            vu += 5;
            if( i != ndim ) {
               strcpy( vu, "." );
               vu++;
            }
         }
      }

/* Store the data units. */
      punits[ 3*ndim ] = dataunits;
      punits[ 1 + 3*ndim ] = dataunits;

/* Store the volume units. */
      punits[ 2 + 3*ndim ] = volunit_buf;

/* Store extra blank units strings for the components of the CUPID extension. */
      for( icol = 2 + 3*ndim + 1; icol < *ncpar; icol++ ) punits[ icol ] = "";

/* Return a pointer to the array of units strings. */
      *units = punits;

/* Indicate we should display any warnings generated by astWrite */
      warn = 1;
   }

/* Map the NDF data array */
   ndfMap(  indf, "Data", "_DOUBLE", "READ", (void *) &ipd, &nel, status );
   if( ipd ) {

/* Find the minimum and maximum pixel value, and the position of the
   peak. We can treat the NDF as if it were 3-dimensional even if it is
   actually 1 or 2 dimension, since ndfBound fills in unused trailing
   elements of "lbnd" and "ubnd" with the value 1. */
      dmin = VAL__BADD;
      dmax = VAL__BADD;

      px = 0;
      py = 0;
      pz = 0;

      pd = ipd;
      for( k = lbnd[ 2 ]; k <= ubnd[ 2 ]; k++ ) {
         for( j = lbnd[ 1 ]; j <= ubnd[ 1 ]; j++ ) {
            for( i = lbnd[ 0 ]; i <= ubnd[ 0 ]; i++, pd++ ) {
               if( *pd != VAL__BADD ) {
                  if( dmin == VAL__BADD ) {
                     dmin = *pd;
                     dmax = *pd;
                     px = i;
                     py = j;
                     pz = k;

                  } else if( *pd < dmin ) {
                     dmin = *pd;

                  } else if( *pd > dmax ) {
                     dmax = *pd;
                     px = i;
                     py = j;
                     pz = k;

                  }
               }
            }
         }
      }

/* Find the other required statistics of the data values. */
      su = 0;
      sv = 0;
      sx = 0;
      sy = 0;
      sz = 0;
      su2 = 0;
      sv2 = 0;
      sx2 = 0;
      sy2 = 0;
      sz2 = 0;
      s = 0;
      sd = 0;
      n = 0;
      pd = ipd;
      for( k = lbnd[ 2 ]; k <= ubnd[ 2 ]; k++ ) {
         for( j = lbnd[ 1 ]; j <= ubnd[ 1 ]; j++ ) {
            for( i = lbnd[ 0 ]; i <= ubnd[ 0 ]; i++, pd++ ) {
               if( *pd != VAL__BADD ) {

/* Use the height of the pixel above the miniumum value as the weight for
   this axis value. If all pixels have the same value use a uniform
   weight of 1.0. */
                  if( backoff ) {
                     if( dmax > dmin ) {
                        d = *pd - dmin;
                     } else {
                        d = 1.0;
                     }
                  } else {
                     d = *pd;
                  }

/* update the weighted sums. */
                  tt = d*i;
                  sx += tt;
                  sx2 += tt*i;

                  tt = d*j;
                  sy += tt;
                  sy2 += tt*j;

                  tt = d*k;
                  sz += tt;
                  sz2 += tt*k;

                  sd += d;
                  s += *pd;
                  n++;

/* U and V are axes at 45 degrees to the first and second spatial axes.
   If we are creating an STC-S ellipse, we record the statistics along
   these extra axes in order to work out an elliptical approximation to
   the spatial extent of the clump. */
                  if( stcs && shape == 1 ) {
                     if( velax == 0 ) {
                        u = ( k + j )/ROOT_TWO;
                        v = ( k - j )/ROOT_TWO;

                     } else if( velax == 1 ) {
                        u = ( k + i )/ROOT_TWO;
                        v = ( k - i )/ROOT_TWO;

                     } else {
                        u = ( j + i )/ROOT_TWO;
                        v = ( j - i )/ROOT_TWO;
                     }

                     tt = d*u;
                     su += tt;
                     su2 += tt*u;

                     tt = d*v;
                     sv += tt;
                     sv2 += tt*v;
                  }
               }
            }
         }
      }

/* Calculate and store the clump parameters, using pixel units initially. */
      if( s != 0 ) {
         ret[ 0 ] = px;
         ret[ ndim ] = sx/sd;

         v0 = sx2/sd - ret[ ndim ]*ret[ ndim ];
         if( v0 <= 0.0 ) v0 = 0.25;

         v = v0 - beamcorr[ 0 ]*beamcorr[ 0 ]/5.5451774;
         *ok = ( v > 0 ) ? 1 : ( ( ndim > 1 ) ? 0 : -1 );
         if( !deconv ) {
            v = v0;
            peakfactor = 1;
         } else {
            peakfactor = v0/v;
         }
         ret[ 2*ndim ] = ( *ok > 0 ) ? sqrt( v ) : 0.0;

         ret[ 0 ] -= 0.5;
         ret[ ndim ] -= 0.5;

         if( ndim > 1 ) {
            ret[ 1 ] = py;
            ret[ 1 + ndim ] = sy/sd;

            v0 = sy2/sd - ret[ 1 + ndim ]*ret[ 1 + ndim ];
            if( v0 <= 0.0 ) v0 = 0.25;

            v = v0 - beamcorr[ 1 ]*beamcorr[ 1 ]/5.5451774;
            if( v > 0 ) {
               if( !deconv ) {
                  v = v0;
               } else {
                  peakfactor *= v0/v;
               }
               ret[ 1 + 2*ndim ] = sqrt( v );
            } else {
               ret[ 1 + 2*ndim ] = 0.0;
               *ok = 0;
            }

            ret[ 1 ] -= 0.5;
            ret[ 1 + ndim ] -= 0.5;

            if( ndim > 2 ) {
               ret[ 2 ] = pz;
               ret[ 2 + ndim ] = sz/sd;

               v0 = sz2/sd - ret[ 2 + ndim ]*ret[ 2 + ndim ];
               if( v0 <= 0.0 ) v0 = 0.25;

               v = v0 - beamcorr[ 2 ]*beamcorr[ 2 ]/5.5451774;

               if( v > 0 ) {
                  if( !deconv ) {
                     v = v0;
                  } else {
                     peakfactor *= v0/v;
                  }
                  ret[ 2 + 2*ndim ] = sqrt( v );

               } else {
                  ret[ 2 + 2*ndim ] = 0.0;
                  *ok = -1;
               }

               ret[ 2 ] -= 0.5;
               ret[ 2 + ndim ] -= 0.5;

            }
         }

      } else {
         peakfactor = 1.0;
         ret[ 0 ] = VAL__BADD;
         ret[ ndim ] = VAL__BADD;
         ret[ 2*ndim ] = VAL__BADD;

         if( ndim > 1 ) {
            ret[ 1 ] = VAL__BADD;
            ret[ 1 + ndim ] = VAL__BADD;
            ret[ 1 + 2*ndim ] = VAL__BADD;

            if( ndim > 2 ) {
               ret[ 2 ] = VAL__BADD;
               ret[ 2 + ndim ] = VAL__BADD;
               ret[ 2 + 2*ndim ] = VAL__BADD;
            }
         }
      }

      ret[ 3*ndim ] = s;
      ret[ 3*ndim + 1 ] = dmax*( (peakfactor > 0.0) ? sqrt( peakfactor ) : 1.0 );
      ret[ 3*ndim + 2 ] = n;

/* If the clump looks usable and an STC-S description of it is required,
   call the routine appropriate to the requested shape to create an AST
   Region. */
      if( stcs && *ok > 0 ) {

/* The new ellipse fitting algorithm avoids creating very long thin ellipses for
   non-elliptical clumps. */
         if( shape == 3 ) {
            reg = cupidEllipseDescNew( pixel_frm, ipd, velax, ret + ndim, space_axes,
                                       ndim, lbnd, ubnd, wcsmap, space_frm,
                                       space_map, status );

/* The old elipse fitting algorithm, is better at genuinly elliptical sources. */
         } else if( shape == 1 ) {
            sig[ 0 ] = ret[ 2*ndim + space_axes[ 0 ] ];
            sig[ 2 ] = ret[ 2*ndim + space_axes[ 1 ] ];
            reg = cupidEllipseDesc( pixel_frm, space_axes, beamcorr,
                                    su/sd, su2/sd, sv2/sd, sv/sd,
                                    ret[ ndim + space_axes[ 0 ] ],
                                    ret[ ndim + space_axes[ 1 ] ],
                                    sig, deconv, ok, wcsmap, space_frm,
                                    space_map, status );

         } else if( shape == 2 ) {
            reg = cupidPolygonDesc( ipd, velax, ret, space_axes, ndim,
                                    lbnd, ubnd, wcsmap, space_frm,
                                    space_map, status );
         }
      }

/* If required, convert the parameter values from pixel units to WCS
   units. */
      if( wcsmap ) {

/* Collect a set of pixel positions to be transformed into WCS coords.
   The first is the peak position, the second is the centroid position.
   The next "ndim" positions are offset away from the peak position along
   each of the "ndim" pixel axes, the displacement along each pixel axis
   being the "size" on that axis. */
         for( i = 0; i < ndim; i++ ) {
            pixpos[ i ][ 0 ] = ret[ i ];
            pixpos[ i ][ 1 ] = ret[ ndim + i ];
            pixpos[ i ][ 2 ] = ret[ i ];
            pixpos[ i ][ 3 ] = ret[ i ];
            pixpos[ i ][ 4 ] = ret[ i ];
         }

         for( i = 0; i < ndim; i++ ) {
            pixpos[ i ][ i + 2 ] += ret[ 2*ndim + i ];
         }

/* Transform these positions into WCS coords. */
         astTranN( wcsmap, ndim + 2, ndim, 5, (double *) pixpos, 1, ndim, 5,
                   (double *) wcspos );

/* Store the peak and centroid WCS positions in the returned array. */
         for( i = 0; i < ndim; i++ ) {
            ret[ i ] = wcspos[ i ][ 0 ];
            ret[ ndim + i ] = wcspos[ i ][ 1 ];
         }

/* Normalise them. */
         astNorm( wcsfrm, ret );
         astNorm( wcsfrm, ret + ndim );

/* For each pixel axis, find the WCS distance between the transformed
   peak position (currently held at the start of "ret"), and the transformed
   outlier position, and store in the returned array (converting sky axes
   from radians to arc-seconds). */
         for( i = 0; i < ndim; i++ ) {
            for( j = 0; j < ndim; j++ ) outlier[ j ] = wcspos[ j ][ 2 + i ];
            csw = astDistance( wcsfrm, ret, outlier );
            if( skyaxis[ i ] ) csw *= AST__DR2D*3600.0;
            ret[ 2*ndim + i ] = csw;
         }

/* Calculate the volume of 1 cubic pixel in WCS units. kpg1Pxscl requires
   a FrameSet so construct one from the supplied Frame and Mapping, using a
   new Frame for PIXEL coords as the Base Frame. */
         for( i = 0; i < ndim; i++ ) at[i] = pixpos[ i ][ 0 ];
         pixfrm = astFrame( ndim, "Domain=Pixel" );
         fs = astFrameSet( pixfrm, " " );
         astAddFrame( fs, AST__BASE, wcsmap, wcsfrm );

/* Get the pixel scales. */
         kpg1Pxscl( fs, at, pixscl, status );

         fs = astAnnul( fs );
         pixfrm = astAnnul( pixfrm );

/* Multiply the pixel scales to get the pixel volume. Convert sky axes
   scales from radians to arc-seconds. */
         pixvol = 1.0;
         for( i = 0; i < ndim; i++ ) {
            if( skyaxis[ i ] ) pixscl[ i ] *= AST__DR2D*3600.0;
            pixvol *= pixscl[ i ];
         }

/* Scale the clump volume from cubic pixel into WCS units. */
         ret[ 3*ndim + 2 ] *= pixvol;

/* Convert sky positions from rads to degs. */
         for( i = 0; i < ndim; i++ ) {
            if( skyaxis[ i ] ) {
               ret[ i ] *= AST__DR2D;
               ret[ ndim + i ] *= AST__DR2D;
            }
         }
      }
   }

/* If an STC-S description is required, and an AST Region describing
   the spatial extent of the clump is available, generate the STC-S
   description, storing it in dynamically allocated memory. */
   if( reg ) {
      astPutChannelData( stcs_chan, stcs );
      if( astWrite( stcs_chan, reg ) == 0 ) *stcs = astFree( *stcs );

/* Display any warnings generated by the above call to astWrite. */
      if( warn ) {
         warnings = astWarnings( stcs_chan );
         if( warnings ) {
            nwarn = astMapSize( warnings );
            for( iwarn = 0; iwarn < nwarn; iwarn++ ) {
               (void) astMapGet0C( warnings, astMapKey( warnings, iwarn ),
                                   &warning );
               if( iwarn == 0 ) {
                  msgBlank( status );
                  msgOut( " ", "The following warnings were generated whilst "
                          "creating STC-S descriptions of clump outlines:",
                          status );
                  msgBlank( status );
               }
               msgSetc( "W", warning );
               msgOut( " ", " - ^W", status );
            }
            warnings = astAnnul( warnings );

/* Suppress future warnings since they will usually be the same for each
   clump. */
            warn = 0;
         }
      }

/* Report an error if the STCS description could not be created. */
      if( *status == SAI__OK && ! *stcs ) {
         *status = SAI__ERROR;
         errRep( " ", "Failed to create an STC-S description of a clump.",
                  status );
      }

   }

/* If there is a CUPID extension, store the values of its components at
   the end of the returned array, and then annul the locator, and then
   delete the object holding the extra column values. */
   if( loc ) {
      icol = 2 + 3*ndim + 1;
      for( icomp = 1; icomp <= ncomp && *status == SAI__OK; icomp++, icol++ ) {
         datIndex( loc, icomp, &cloc, status );
         datGet0D( cloc, ret + icol , status );
         datAnnul( &cloc, status );
      }
      datAnnul( &loc, status );
      datErase( xloc, "EXTRA", status );
   }

/* Annul the CUPID extension locator */
   if( xloc ) datAnnul( &xloc, status );

/* Unmap the NDF data array */
   ndfUnmap(  indf, "Data", status );

/* Free AST Objects. */
   if( reg ) {
      if( region ) *region = astClone( reg );
      reg = astAnnul( reg );
   }

/* Return the array of clump parameters. */
   return ret;
}



static void cupidMemSink( const char *text ) {
   int nc;
   char **buffer;

   buffer = astChannelData;
   nc = ( *buffer ) ? strlen( *buffer ) : 0;
   *buffer = astAppendString( *buffer, &nc, text );
}

