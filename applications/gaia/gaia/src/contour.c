/*+
 *  Name:
 *     gaiaContour

 *  Purpose:
 *     Draws contours of a 2-d array using an Ast Plot

 *  Language:
 *     ANSI-C

 *  Invocation:
 *     status = gaiaContour( const float *image, int nx, int ny,
 *                           const double *cont, int ncont,
 *                           const AstPlot *plot, const char *props[],
 *                           int nprops, int xll, int yll,
 *                           int xsize, int ysize, int fast )

 *  Description:
 *     This routine plots a contour map of a two-dimensional sub-array
 *     for a set of contour levels.  For each contour level, it searches
 *     the array to find a cell of four pixels containing a contour at
 *     that level, and then traces the contour until it closes or ends
 *     outside the image.
 *
 *     The properties of each contour (i.e. colour, line thickness
 *     etc.) are defined as a list of AST attributes. This list should
 *     either consist of one set, which will be applied to all
 *     contours, or a set for each contour
 *
 *     The "fast" argument indicates that contours should be drawn
 *     using straight-lines, rather than geodesics.

 *  Arguments:
 *     plot = *AstPlot (Given)
 *        An AST pointer to the Plot through which the graphics will be
 *        produced. The Current Frame should describe the GRID coordinates
 *        of the array to be contoured.
 *     nx = int (Given)
 *        The first dimension of the two-dimensional array.
 *     ny = int (Given)
 *        The second dimension of the two-dimensional array.
 *     image = *float  (Given)
 *        Two-dimensional array to be contoured.
 *     xll = int (Given)
 *        The x co-ordinate of the lower left pixel of the selected
 *        sub-array.
 *     yll = int (Given)
 *        The y co-ordinate of the lower left pixel of the selected
 *        sub-array.
 *     xsize = int (Given)
 *        The x size of the sub-array to be contoured.
 *     ysize = int (Given)
 *        The y size of the sub-array to be contoured.
 *     ncont = int (Given)
 *        The number of contour levels.
 *     cont = *float (Given)
 *        The contour levels (ncont values).
 *     fast = int (Given)
 *        If true, then contours are drawn using straight lines as the
 *        basic drawing element. Otherwise, the basic drawing element are
 *        geodesic curves in the Current coordinate system of the supplied
 *        Plot. This is much slower to draw, but may be useful when a
 *        severly non-linear or discontinuous mapping exists between grid
 *        coordinates in the array, and graphics coordinates.

 *  Return:
 *     Status on exit, 0 for OK, 1 for invalid plot domain.

 *  Algorithm:
 *     Check for error on entry - return if not o.k.
 *     The routine makes a separate pass through the image for each
 *     contour to be plotted.  The image is divided into "cells" (groups
 *     of four adjacent pixels) and each is examined in turn.  Cells
 *     contining "bad" pixels are ignored, but for all others the
 *     minimum and maximum cell data values are found.  If the contour
 *     level currently being plotted lies between these two values, then
 *     a contour crosses the cell in question, otherwise the cell is
 *     skipped over on this pass.

 *     Having identified a cell containing a contour, the contour
 *     following algorithm is triggered.  Each cell side (a "side" is
 *     one of the lines joining pixel centres) is examined to determine
 *     if the contour passes through it and, if so, at what position.
 *     If the contour only passes through two cell sides, then the cell
 *     is "simple" and is only crossed by a single contour line.  In
 *     this case, the contour entry and exit points are put into a list
 *     of positions (to be plotted), the cell is flagged as "done" and
 *     the algorithm moves on to the cell adjacent to the contour exit
 *     position, where the process is repeated - thereby "following" the
 *     contour.

 *     Contour following continues until the next cell is off the
 *     edge of the image, has already been "done" on this pass, contains
 *     a "bad" pixel or is "confused" (i.e. is crossed by more than one
 *     contour line).  In "confused" cells, all four cell sides are
 *     crossed by contours, and the algorithm pairs the points to form
 *     two line segents to plot which do not cross and which produce the
 *     shortest total length of contour line within the cell.  Contour-
 *     following also terminates if the buffer containing the list of
 *     points to plot becomes full.

 *     When contour following terminates, all pending output is plotted
 *     with the appropriate pen (there are two separate lines to plot if
 *     the final cell was confused).  The scan through the data (looking
 *     for cells which are crossed by the current contour) then resumes
 *     at the cell following the one which initiated the last episode of
 *     contour-following.  Cells which are already flagged as "done" do
 *     not subsequently trigger further contour-following on this pass.

 *  Notes:
 *     -  Magic-value bad pixels are correctly processed.

 *  Implementation Deficiencies:
 *     The contours are not smooth and the scanning algorithm can be made
 *     many times faster by not examining all pixels at all heights.

 *  Authors:
 *     RFWS: Rodney Warren-Smith (STARLINK, Durham)
 *     MJC: Malcolm J. Currie (STARLINK)
 *     DSB: David S. Berry (STARLINK)
 *     PWD: Peter W. Draper (STARLINK, Durham)
 *     {enter_new_authors_here}

 *  History:
 *     06-APR-1999 (PWD):
 *        Original version. This is a C version of the Fortran routine
 *        KPS1_CNTF, written by RFWS, MJC and DSB.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_new_bugs_here}

 *- */

/*  Include files: */
/*  ============== */
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "img.h"
#include "ast.h"

/*  Prototypes: */
/*  =========== */
void gaiaContPlot( AstPlot *plot, int fast, int npts, double x[], double y []);

/*  Local Constants: */
/*  ================ */
#define MAXPTS 100000   /* Maximum number of positions in each axis
                           that define the locus of a contour */
#define NCELL 4         /* Number of pixels in a cell */

#define DATA_TYPE float /*  Data type of image */

/*  Useful functions: */
/*  ================= */

/*  Get pixel value from 2D array, "span" is second dimension */
#define arrayVal( arrayPtr, span, ix, iy ) arrayPtr[(iy)*(span) + (ix)]

/*  Maximum of two values */
#define max( a, b ) ( (a) > (b) ? (a) : (b) )

/*  Minimum of two values */
#define min( a, b ) ( (a) < (b) ? (a) : (b) )

/*  Is there a BAD pixel within the current cell? */
#define badpix( image, span, i, j ) \
 ( \
    ( arrayVal( image, span, i    , j     ) == VAL__BADF ) || \
    ( arrayVal( image, span, i + 1, j     ) == VAL__BADF ) || \
    ( arrayVal( image, span, i + 1, j + 1 ) == VAL__BADF ) || \
    ( arrayVal( image, span, i    , j + 1 ) == VAL__BADF )    \
 )

/*  Distance between two points */
#define rdist( x, y, i, j ) \
 ( \
    sqrt( ( x[i] - x[j] ) * ( x[i] - x[j] ) + \
          ( y[i] - y[j] ) * ( y[i] - y[j] ) ) \
 )
#define dist( x, y, i, j ) ((int) rdist( x, y, i, j ))

/*  Are we outside the allowed part of image? */
#define offimg( xsize, ysize, i, j ) \
 ( \
   ( i < 0 ) || ( j < 0 ) || ( i >= xsize - 1 ) || ( j >= ysize - 1 ) \
 )


/* Routine: */
/* ======== */
int gaiaContour( const DATA_TYPE *image, int nx, int ny,
                 const double *cont, int ncont,
                 AstPlot *plot, const char *props[], int nprops,
                 int xll, int yll, int xsize, int ysize,
                 int fast )
{

  /*  Local Variables: */
  char *domain;       /* Domain of Current Frame in supplied Plot */
  double b[NCELL+1];  /* Storage of the pixel values */
  double bmax;        /* Maximum pixel value in the cell */
  double bmin;        /* Minimum pixel value in the cell */
  double cval;        /* Current contour value */
  double fract;       /* Fractional position of contour from linear
                         iterpolation */
  double x[MAXPTS];   /* X positions of the contour */
  double xtemp;       /* Dummy for swapping x position of contour */
  double y[MAXPTS];   /* Y positions of the contour */
  double ytemp;       /* Dummy for swapping y position of contour */
  int above;          /* Next cell pixel's value is greater than
                         contour level? */
  int anote;          /* Reference cell pixel's value is greater than
                         contour level? */
  int confus;         /* Cell is confused? */
  int dshthr;         /* Is there a threshold? */
  int i;              /* Loop counter through columns */
  int icont;          /* Counter to index contour levels */
  int icurr;          /* Index of original Current Frame */
  int ii;             /* X element numbers of current pixel in sub-array */
  int ipen;           /* Current pen number */
  int iplott;         /* AST pointer to Plot with current pen set */
  int ix;             /* X element numbers of current pixel in
                         full-size array */
  int iy;             /* Y element numbers of current pixel in
                         full-size array */
  int j;              /* Loop counter through lines */
  int jj;             /* Y element numbers of current pixel in sub-array */
  int l;              /* General variable */
  int lin;            /* Current entrance side of new cell */
  int linend;         /* At end of a line? */
  int lside;          /* Current exit side of cell */
  int nexit;          /* Number of cell exits for current cell */
  int npts;           /* Number of points in contour's locus */
  unsigned char *done;/* Workspace for recording pixels that are "done" */

  AstMapping *oldmap; /* Mapping between current and base frames */
  AstMapping *newmap; /* Simplified mapping between current and base frames */
  AstFrame *frame;    /* Copy of the current frame */
  AstPlot *lplot;     /* Copy of input plot */

  /*  Local initialisations */
  double cx[NCELL] = { 0.0, 1.0, 1.0, 0.0  }; /* X co-ordinates of the
                                                 cell corners */
  double cy[NCELL] = { 0.0, 0.0, 1.0, 1.0  }; /* Y co-ordinates of the
                                                 cell corners */
  double dx[NCELL] = { 1.0, 0.0, -1.0, 0.0 }; /* Differential x
                                                 co-ordinates of cell
                                                 corners */
  double dy[NCELL] = { 0.0, 1.0, 0.0, -1.0 }; /* Differential y
                                                 co-ordinates of cell
                                                 corners */
  int imove[NCELL] =  {  0, 1, 0, -1 };  /* X directions to move from
                                            the cell side where a
                                            contour leaves */
  int jmove[NCELL] =  { -1, 0, 1,  0 };  /* Y directions to move from
                                            the cell side where a
                                            contour leaves */
  int newsid[NCELL] = {  2, 3, 0,  1 };  /* Side of entry in the new
                                            cell from the side of exit
                                            from the old cell */

  /*  Check that the current frame in the supplied plot is a "GRID" */
  if ( strcmp( astGetC( plot, "Domain" ), "GRID" ) != 0 ) {
    return 1;
  }

  astShow( plot );


  /*  Get some workspace for locating pixels that have already been
      "done" */
  done = (unsigned char *) malloc( xsize * ysize );

  /*  Simplify the Plot. This adds a new Current Frame into the Plot,
      so note the index of the original Current Frame so that it can
      be re-instated later. This can help to speed up the drawing,
      and also avoids the possibility of the Mapping going via a Frame
      in which the positions are undefined.  */
  icurr = astGetI( plot, "Current" );
  oldmap = (AstMapping *) astGetMapping( plot, AST__BASE, AST__CURRENT );
  newmap = (AstMapping *) astSimplify( oldmap );
  frame = (AstFrame *) astGetFrame( plot, AST__CURRENT );
  astAddFrame( plot, AST__BASE, newmap, frame );
  if ( !astOK ) {
    free( (void *) done );
    astSetI( plot, "Current",  icurr );
    return 2;
  }

  /*  Scan through each contour level. */
  for ( icont = 0; icont < ncont; icont++ ) {
    cval = cont[icont];

    /*  If different properties are being used, produce a modified
        Plot which draws curves with the pen style supplied for this
        contour. */
    if ( nprops > 1 ) {

      /*  Take a deep copy of the supplied Plot. This Plot will be
          modify using the supplied attribute settings. A copy is used
          so that the original plotting attributes can be re-instated
          later. */
      lplot = astCopy( plot );

      /*  Set the AST Attribute settings from properties lists */
      astSet( lplot, props[icont] );
    } else {

      /*  If the same properties are being used for all contours, just
          clone the supplied Plot pointer. */
      lplot = astClone( plot );

      /*  And set the values (if not already done). */
      if ( icont == 0 ) {
        astSet( lplot, props[0] );
      }
    }

    /*  Initialise the store of cells done. */
    memset( done, '\0', xsize * ysize );

    /*  Initialise counter for number of x-y co-ordinates to plot. */
    npts = -1;

    /*  Scan the image, looking for a cell containing the current
        contour level. */
    for ( j = 0; j < ysize-1; j++ ) {
      for ( i = 0; i < xsize-1; i++ ) {

        /*  If he cell has already been contoured, omit it. */
        if ( ! arrayVal( done, xsize, i, j ) ) {

          /*  Note this cell has been looked at. */
          arrayVal( done, xsize, i, j ) = 1;

          /*  Find the position of the current pixel in the full
              two-dimensional array. */
          ix = i + xll - 1;
          iy = j + yll - 1;

          /*  Don't use this cell if there is a bad pixel adjacent. */
          if ( ! badpix( image, nx, ix, iy ) ) {

            /*  Extract data values and test if they contain the
                contour. */
            b[0] = arrayVal( image, nx, ix, iy );
            b[1] = arrayVal( image, nx, ix + 1, iy );
            b[2] = arrayVal( image, nx, ix + 1, iy + 1 );
            b[3] = arrayVal( image, nx, ix, iy + 1 );
            bmax = max( max( b[0], b[1] ), max( b[2], b[3] ) );
            bmin = min( min( b[0], b[1] ), min( b[2], b[3] ) );

            if ( cval < bmax && cval > bmin ) {
              b[4] = b[0];

              /*  Initialise the pointers to the cells on this
                  contour. */
              ii = i;
              jj = j;

              /*  Initialise the cell side where the contour enters
                  the cell. */
              lin = -1;
              linend = 0;
              while ( ! linend ) {
                nexit = 0;

                /*  Scan the cell sides, searching for intersections
                    with the contour. */
                anote = ( b[0] >= cval );
                for ( l = 0; l < 4; l++ ) {
                  above = ( b[l+1] >= cval );

                  /*  Don't count contour exits from the same side as
                      it entered. */
                  if ( ( above != anote ) && ( l != lin ) ) {
                    lside = l;
                    nexit++;
		    npts++;

                    /*  Calculate the co-ordinates of the contour exit
                        point from the cell by linear interpolation,
                        and store them in X and Y.*/
                    fract = ( cval - b[l] ) / ( b[l+1] - b[l] );
                    x[npts] = 1.0 + ix + cx[l] + dx[l] * fract;
                    y[npts] = 1.0 + iy + cy[l] + dy[l] * fract;

                  }
                  anote = above;
                }

                /*  The cell is confused if the number of contour
                    exits does not match the number of entries. */
                if ( lin == -1 ) {
                  confus = ( nexit != 2 );
                } else {
                  confus = ( nexit != 1 );
                }

                /*  Find the co-ordinates of the next cell which the
                    contour enters. */
                ii = ii + imove[lside];
                jj = jj + jmove[lside];
                ix = ix + imove[lside];
                iy = iy + jmove[lside];

                /*  Find the side of the new cell through which it
                    enters. */
                lin = newsid[lside];

                /*  It is the end of current contour line if the:
                    o  contour goes off the edge of the image,
                    o  hits an invalid pixel,
                    o  enters a cell already contoured,
                    o  leaves a confused cell, or
                    o  exceeds the storage space for the X and Y
                    arrays.
                */
                if ( offimg( xsize, ysize, ii, jj ) ) {
                  linend = 1;
                } else {
                  linend = 
                          badpix( image, nx, ix, iy ) ||
                          confus ||
                          arrayVal( done, xsize, ii, jj ) ||
                          ( npts >= MAXPTS - 1 );
                }

                /*  If we are continuing on this contour, extract the
                    data for next cell and mark the cell done. */
                if ( ! linend ) {
                  b[0] = arrayVal( image, nx, ix, iy );
                  b[1] = arrayVal( image, nx, ix + 1, iy );
                  b[2] = arrayVal( image, nx, ix + 1, iy + 1 );
                  b[3] = arrayVal( image, nx, ix, iy + 1 );
                  b[4] = b[0];
                  arrayVal( done, xsize, ii, jj ) = 1;
                }
              }  /*  End while (!linend) return to analyse the new cell.*/

              /*  If the last cell on a contour was confused, all four
                  cell sides will be crossed by a contour. The
                  crossing points must be correctly paired. There are
                  three possible pairing combinations which leave the
                  first point in its original position. */
              if ( confus ) {

                /*  Check if the current pairing causes contour lines
                    to cross.  If so, swap the appropriate pair of
                    points so they no longer cross. */
                if ( ( max( x[npts], x[npts-1] ) >
                       max( x[npts-2], x[npts-3] ) &&
                       min( x[npts], x[npts-1] ) <
                       min( x[npts-2], x[npts-3] )
                       ) ||
                     ( max( x[npts], x[npts-1] ) <
                       max( x[npts-2], x[npts-3] ) &&
                       min( x[npts], x[npts-1] ) >
                       min( x[npts-2], x[npts-3] )
                       )
                     ) {
                  xtemp = x[npts-1];
                  ytemp = y[npts-1];
                  x[npts-1] = x[npts-2];
                  y[npts-1] = y[npts-2];
                  x[npts-2] = xtemp;
                  y[npts-2] = ytemp;
                }

                /*  Make a further swap if necessary, to find the
                    pairing (out of the two which remain) which
                    produces the shorter total length of contour
                    line. */
                if ( dist( x, y, npts, npts-1 ) +
                     dist( x, y, npts-2, npts-3 )
                     >
                     dist( x, y, npts-1, npts-2 ) +
                     dist( x, y, npts-3, npts ) ) {

                  /*  Swap the pairing if necessary. */
                  xtemp = x[npts];
                  ytemp = y[npts];
                  x[npts] = x[npts-2];
                  y[npts] = y[npts-2];
                  x[npts-2] = xtemp;
                  y[npts-2] = ytemp;
                }
                npts = npts - 2;
              } /*  End of confusion check. */

              /*  Plot the stored contour. */
              npts++;
              gaiaContPlot( lplot, fast, npts, x, y );

              /*  Plot the segment of the other contour found in the
                  confused cell. */
              if ( confus ) {
		gaiaContPlot( lplot, fast, 2, &x[npts], &y[npts] );
              }

              /* Reset the number of points to plot. */
              npts = -1;

            } /*  End of contour-lies-between-pixels check. */

          } /*  End of bad-pixel check.*/

        } /*  End of already contoured-pixel check. */
        
      } /*  End of the loop through the columns.*/

    } /*  End of the loop through the lines. */

    /*  Annul the temporary copy of the supplied Plot which was used
        to do the drawing. */
    lplot = (AstPlot *) astAnnul( lplot );

  } /*  End of the contour-level loop.*/

  /*  Remove the Current Frame we added, re-instate the original Current
      Frame. */
  free( (void *) done );
  astRemoveFrame( plot, AST__CURRENT );
  astSetI( plot, "Current", icurr );
  return 0;
}
#include "grf.h"
void gaiaContPlot( AstPlot *plot, int fast, int npts, double *x, double *y) {

  /*  Local Variables: */
  double xydata[2][MAXPTS];
  int i;

  /*  Copy each point into a correctly formatted buffer. */
  for ( i = 0; i < npts; i++ ) {
    xydata[0][i] = x[i];
    xydata[1][i] = y[i];
  }

  /*  Draw the Poly-curve. */
  astPolyCurve( plot, npts, 2, MAXPTS, (const double(*)[]) xydata );
  astGFlush;
}
