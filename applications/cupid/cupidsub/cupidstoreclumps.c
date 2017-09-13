#include "sae_par.h"
#include "prm_par.h"
#include "star/hds.h"
#include "star/atl.h"
#include "star/ndg.h"
#include "star/cvg.h"
#include "kpg_err.h"
#include "star/kaplibs.h"
#include "par.h"
#include "fitsio.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "cupid.h"
#include "cupid.h"
#include <string.h>
#include <stdio.h>

/* Local Constants: */
#define MAXCAT   4096 /* Max length of catalogue name */
#define LOGTAB   16   /* Width of one log file column, in characters */

void cupidStoreClumps( const char *param1, const char *param2, int indf,
                       HDSLoc *xloc, HDSLoc *obj, int ndim, int deconv,
                       int backoff, int stccol, int velax, double beamcorr[ 3 ],
                       const char *ttl, int usewcs, AstFrameSet *iwcs,
                       const char *dataunits, Grp *hist,
                       FILE *logfile, int *nclumps, int *status ){
/*
*+
*  Name:
*     cupidStoreClumps

*  Purpose:
*     Store properties of all clumps found by the CLUMPS command.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidStoreClumps( const char *param1, const char *param2, int indf,
*                            HDSLoc *xloc, HDSLoc *obj, int ndim, int deconv,
*                            int backoff, int stccol, int velax,
*                            double beamcorr[ 3 ], const char *ttl, int usewcs,
*                            AstFrameSet *iwcs, const char *dataunits,
*                            Grp *hist, FILE *logfile, int *nclumps, int *status )

*  Description:
*     This function optionally saves the clump properties in an output
*     catalogue, and then copies the NDF describing the found clumps into
*     the supplied CUPID extension.

*  Parameters:
*     param1
*        The ADAM parameter to associate with the KAPPA-style output
*        catalogue. This can be in any format supported by the CAT_ library.
*        It can be used with KAPPA commands such as LISTSHOW. May be NULL.
*     param2
*        The ADAM parameter to associate with the JSA-style output catalogue.
*        This will always be a FITS binary table, including headers
*        inherited form the input NDF and CADC-style provenance headers.
*        May be NULL.
*     indf
*        The input NDF supplied to findclumps by the user. Only used if a
*        non-NULL value is supplied for param2.
*     xloc
*        HDS locator for the CUPID extension of the NDF in which to store
*        the clump properties. May be NULL.
*     obj
*        A locator for an HDS array the clump NDF structures.
*     ndim
*        The number of pixel axes in the data.
*     deconv
*        If non-zero then the clump proprties values stored in the
*        catalogue and NDF are modified to remove the smoothing effect
*        introduced by the beam width. If zero, the undeconvolved values
*        are stored in the output catalogue and NDF. Note, the filter to
*        remove clumps smaller than the beam width is still applied, even
*        if "deconv" is zero.
*     backoff
*        If non-zero, then the background level is subtracted from all
*        clump data values before calculating the clump sizes and centroid.
*        The background level is the minimum data value in the clump. If
*        zero, then the clump sizes and centroid are based on the full data
*        values (this is what the IDL version of ClumpFind does).
*     stccol
*        If non-zero, then the output catalogue will contain a column holding
*        a textual description of the spatial extent of each clump, in  the
*        form of an STC-S description. The non-zero value indicates the shape
*        to use:
*           0 - No STC-S to be created
*           1 - Use an ellipse to describe the spatial extent of the clump
*           2 - Use a polygon to describe the spatial extent of the clump
*     velax
*        The index of the velocity pixel axis. Only used if "ndim" is 3.
*     backoff
*        If non-zero, then the background level is subtracted from all
*     beamcorr
*        An array holding the FWHM (in pixels) describing the instrumental
*        smoothing along each pixel axis. If "deconv" is non-zero, the clump
*        widths and peak values stored in the output catalogue are modified
*        to correct for this smoothing.
*     ttl
*        The title for the output catalogue (if any).
*     usewcs
*        Should the columns in the output catalogue that represent clump
*        position, size and volume hold values in the coordinate system
*        specified by the current Frame of the WCS FrameSet supplied via
*        "iwcs"? If not, they hold values in pixel coordinates. Ignored if
*        "iwcs" is NULL (in which case the catalogue will hold values in
*        pixel coords).
*     iwcs
*        The WCS FrameSet from the input data, or NULL.
*     dataunits
*        The Units component from the output NDF.
*     hist
*        A group containing lines of text to be stored in the output
*        catalogue has history information.
*     logfile
*        Pointer to a file identifier for the output log file, or NULL.
*     nclumps
*        Pointer to an int in which to return the number of clumps stored
*        in the output NDF.
*     status
*        Pointer to the inherited status value.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008-2013 Science & Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-NOV-2005 (DSB):
*        Original version.
*     11-DEC-2006 (DSB):
*        Added parameter "deconv".
*     16-DEC-2006 (DSB):
*        Added parameters "usewcs" and "dataunits".
*     25-JAN-2007 (DSB):
*        Added parameters "hist" and "logfile".
*     23-MAR-2007 (DSB):
*        Added parameter "nclumps".
*     28-MAR-2007 (TIMJ):
*        Up MAXCAT to 4096 characters from 50.
*     14-MAY-2007 (TIMJ):
*        Prevent log file columns overlapping.
*     18-MAR-2008 (DSB):
*        Added argument "backoff" for Jenny Hatchell.
*     14-JAN-2009 (TIMJ):
*        Use MERS for message filtering.
*     27-APR-2009 (DSB):
*        Added parameters "velax" and "stccol".
*     3-OCT-2012 (DSB):
*        Clean up static resources allocated in cupidClumpDesc.
*     18-NOV-2013 (DSB):
*        Added parameter param2 and indf to allow a JSA-style catalogue
*        to be created..
*     22-NOV-2013 (DSB):
*        Report the number of clumps that fail the beam width test
*        separately for spatial and spectral axes.
*     17-JAN-2014 (DSB):
*        Do not report an error if there ar no usable clumps (requested
*        by Andy and Malcolm).
*     18-FEB-2014 (DSB):
*        Erase any CADC provenance headers inherited form the input NDF.
*     4-APR-2014 (DSB):
*        Correct the entry names used when retrieving STC descriptions
*        from the STC KeyMap, when writing JSA catalogues. The names are
*        of the form "Shape_%d", where the integer value "%d" is one-based,
*        not zero-based.
*     25-MAY-2017 (DSB):
*        Switch off group history and provenance recording during this
*        function. This is because it can inflate the time taken to run
*        findclumps enormously if there are many thousands of clumps.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstFitsChan *fc;         /* Headers to store in FITS binary table */
   AstFitsTable *table;     /* Staging post for FITS binary table data */
   AstFrame *frm1;          /* Frame describing clump parameters */
   AstFrame *frm2;          /* Frame describing clump centres */
   AstFrame *wcsfrm;        /* Current Frame describing WCS coords */
   AstKeyMap *stc_km;       /* KeyMap holding STC-S clump descriptions */
   AstMapping *map;         /* Mapping from "frm1" to "frm2" */
   AstMapping *wcsmap;      /* Mapping from PIXEL to current Frame */
   AstRegion *region;       /* Region describing clump outline */
   HDSLoc *aloc;            /* Locator for array of Clump structures */
   HDSLoc *cloc;            /* Locator for array cell */
   HDSLoc *dloc;            /* Locator for cell value */
   HDSLoc *ncloc;           /* Locator for array cell */
   NdgProvenance *prov;     /* Provenance info from input NDF */
   char *line = NULL;       /* Pointer to buffer for log file output */
   char *p1;                /* Pointer to next character */
   char *stc_data;          /* Array of fixed-length STC strings */
   char *stcptr = NULL;     /* Pointer to buffer holding STC-S clump description */
   char attr[ 15 ];         /* AST attribute name */
   char buf2[ 2*LOGTAB ];   /* Buffer for a log file unit string */
   char buf[ 2*LOGTAB ];    /* Buffer for a log file column value */
   char cat1[ MAXCAT + 1 ]; /* KAPPA-style catalogue name */
   char cat2[ MAXCAT + 1 ]; /* JSA-style catalogue name */
   char key[ 20 ];          /* KeyMap key */
   char unit[ 10 ];         /* String for NDF Unit component */
   const char **names;      /* Component names */
   const char **units;      /* Component units */
   const char *cname;       /* Pointer to column name string */
   const char *dom;         /* Pointer to domain string */
   double *cpars;           /* Array of parameters for a single clump */
   double *t;               /* Pointer to next table value */
   double *tab;             /* Pointer to catalogue table */
   double *tj;              /* Pointer to next table entry to write*/
   fitsfile *fptr;          /* Pointer to FITS file structure */
   int bad;                 /* Does clump touch an area of bad pixels? */
   hdsdim i;                /* Index of next locator */
   hdsdim iclump;           /* Usable clump index */
   int icol;                /* Zero based column index */
   int ifrm;                /* Frame index */
   int indf1;               /* Identifier for supplied NDF */
   int indf2;               /* Identifier for copied NDF */
   int irow;                /* One-based row index */
   int istc;                /* Number of STC-S descriptions created */
   int nbad;                /* No. of clumps touching an area of bad pixels */
   int nc;                  /* Number of characters currently in "line" */
   int ncpar;               /* Number of clump parameters */
   int nfrm;                /* Total number of Frames */
   int nok;                 /* No. of usable clumps */
   int nsmall1;             /* No. of clumps smaller than the spatial beam size */
   int nsmall2;             /* No. of clumps smaller than the spectral resolution */
   int ok;                  /* Is the clump usable? */
   int old_ghstate;         /* Non-zero if group history recording is switched on */
   int old_pvstate;         /* Non-zero if provenance recording is switched on */
   int pixfrm;              /* Index of PIXEL Frame */
   int place;               /* Place holder for copied NDF */
   int there;               /* Does component exist?*/
   size_t max_stclen;       /* Max length of any STC string */
   size_t nndf;             /* Total number of NDFs */
   size_t stclen;           /* Length of STC string */

/* Initialise */
   *nclumps = 0;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Temporaily switch off group history and provenance recording since there
   can be thousands of clump NDFs. */
   ndgHltgh( 0, &old_ghstate, status );
   ndgHltpv( 0, &old_pvstate, status );

/* Get the total number of NDFs supplied. */
   datSize( obj, &nndf, status );

/* If we are writing the information to an NDF extension, create an array
   of "nndf" Clump structures in the extension, and get a locator to it. */
   if( xloc ) {
      hdsdim ndfdims[1];
      aloc = NULL;
      datThere( xloc, "CLUMPS", &there, status );
      if( there ) datErase( xloc, "CLUMPS", status );
      ndfdims[0] = nndf;
      datNew( xloc, "CLUMPS", "CLUMP", 1, ndfdims, status );
      datFind( xloc, "CLUMPS", &aloc, status );
   } else {
      aloc = NULL;
   }

/* Indicate that no memory has yet been allocated to store the parameters
   for a single clump. */
   cpars = NULL;

/* Indicate we have not yet found any clumps smaller than the beam size. */
   nsmall1 = 0;
   nsmall2 = 0;

/* Indicate we have not yet found any clumps that touch any areas of
   bad pixels. */
   nbad = 0;

/* Number of CLUMP structures created so far. */
   iclump = 0;

/* Number of STC-S descriptions stored. */
   istc = 0;

/* Loop round all Frames in the FrameSet, noting the index of the PIXEL
   Frame, and removing any GRID or AXIS Frames. */
   pixfrm = AST__NOFRAME;
   if( iwcs ) {
      nfrm = astGetI( iwcs, "NFrame" );
      for( ifrm = nfrm; ifrm > 0; ifrm-- ) {
         dom = astGetC( astGetFrame( iwcs, ifrm ), "Domain" );
         if( dom ){
            if( !strcmp( dom, "PIXEL" ) ) {
               pixfrm = ifrm;

            } else if( !strcmp( dom, "AXIS" ) ||
                       !strcmp( dom, "GRID" ) ) {
               astRemoveFrame( iwcs, ifrm );
               if( pixfrm != AST__NOFRAME ) pixfrm--;
            }
         }
      }
   }

/* If the catalogue is to hold WCS values rather than PIXEL values, get the
   Mapping from the PIXEL Frame to the WCS Frame. */
   if( usewcs && pixfrm != AST__NOFRAME ) {
      wcsmap = astGetMapping( iwcs, pixfrm, AST__CURRENT );
      wcsfrm = astGetFrame( iwcs, AST__CURRENT );
   } else {
      wcsmap = NULL;
      wcsfrm = NULL;
   }

/* Indicate that no memory has yet been allocated to store the full table
   of parameters for all clumps. */
   tab = NULL;

/* If we are creating an STC-S column, create a KeyMap to hold the
   strings to put in the column. The kpg1Wrcat function requires the
   KeyMap to contain an entry called COLNAMES that holds all the column
   names. */
   if( stccol ) {
      stc_km =  astKeyMap( " " );
      astMapPut0C( stc_km, "COLNAMES", "Shape", NULL );
   } else {
      stc_km = NULL;
   }

/* Loop round the non-null identifiers, keeping track of the one-based row
   number corresponding to each one. */
   irow = 0;
   nok = 0;
   max_stclen = 0;
   for( i = 1; i <= nndf && *status == SAI__OK; i++ ) {
      ncloc = NULL;
      datCell( obj, 1, &i, &ncloc, status );

      errBegin( status );
      ndfFind( ncloc, " ", &indf1, status );
      errEnd( status );

      datAnnul( &ncloc,status );
      if( indf1 != NDF__NOID ) {
         irow++;

/* The Unit component of the NDF will be set to "BAD" if the clump
   touches any areas of bad pixels in the input data array. Count how
   many of these clumps there are. */
         unit[ 0 ] = 0;
         ndfCget( indf1, "Unit", unit, 9, status );
         if( !strcmp( unit, "BAD" ) ){
            bad = 1;
            nbad++;
         } else {
            bad = 0;
         }

/* Calculate the clump parameters from the clump data values stored in the
   NDF. This allocates memory if needed, and also returns some global
   information which is the same for every clump (the parameter names and
   units, the indices of the parameters holding the clump central position,
   and the number of parameters). */
         cpars = cupidClumpDesc( indf1, deconv, wcsmap, wcsfrm, dataunits,
                                 beamcorr, backoff, stccol, velax, cpars,
                                 &names, &units, &ncpar, &ok, &stcptr,
                                 &region, status );

/* If we have not yet done so, allocate memory to hold a table of clump
   parameters. In this table, all the values for column 1 come first,
   followed by all the values for column 2, etc (this is the format required
   by KPG1_WRLST). */
         if( !tab ) {
            tab = astMalloc( sizeof(double)*nndf*ncpar );

/* If a log file is being created, write the column names & units to the
   log file. Each column has a field width of LOGTAB characters. */
            if( logfile ) {
               nc = 0;

               sprintf( buf, "%-*s", LOGTAB, "Index" );
               line = astAppendString( line, &nc, buf );

               for( icol = 0; icol < ncpar; icol++ ) {
                  sprintf( buf, "%-*s", LOGTAB, names[ icol ] );
                  line = astAppendString( line, &nc, buf );
               }
               fprintf( logfile, "%s\n", line );


               nc = 0;

               sprintf( buf, "%-*s", LOGTAB, "" );
               line = astAppendString( line, &nc, buf );

               for( icol = 0; icol < ncpar; icol++ ) {
                  sprintf( buf2, "[%s]", units[ icol ] );
                  sprintf( buf, "%-*s", LOGTAB, buf2 );
                  line = astAppendString( line, &nc, buf );
               }
               fprintf( logfile, "%s\n", line );

            }
         }

/* Put the new clump into the table. */
         if( tab ) {

/* Count the number of clumps which are smaller than the beam size. Also
   set the Unit component of the NDF to "BAD" to indicate that the clump
   should not be used. */
            if( bad ){
               ok = 0;

            } else if( ok <= 0 ) {
               ndfCput( "BAD", indf1, "Unit", status );
               if( ok == 0 ) {  /* Small on the spatial axes, but not small on the spectral axis */
                  nsmall1++;
               } else if( ok < 0 ) { /* Small on the spectral axis, and maybe also the spatial axes */
                  nsmall2++;
               }
            }

/* If the row is usable, increment the number of good rows, and write the
   values to the log file if required. */
            if( ok > 0 ) {
               nok++;
               if( logfile ) {
                  nc = 0;

                  sprintf( buf, "%-*d", LOGTAB-1, nok );
                  line = astAppendString( line, &nc, buf );

                  for( icol = 0; icol < ncpar; icol++ ) {
                     sprintf( buf, " %-*.*g", LOGTAB-1, LOGTAB-5, cpars[ icol ] );
                     line = astAppendString( line, &nc, buf );
                  }
                  fprintf( logfile, "%s\n", line );
               }
            }

/* Put the clump parameters into the table. Store bad values if the clump
   is too small. */
            t = tab + irow - 1;
            for( icol = 0; icol < ncpar; icol++ ) {
               *t = ( ok > 0 ) ? cpars[ icol ] : VAL__BADD;
               t += nndf;
            }

/* If required, put the clump parameters into the current CLUMP structure. */
            if( aloc && ok > 0  ) {

/* Get an HDS locator for the next cell in the array of CLUMP structures. */
               iclump++;
               cloc = NULL;
               datCell( aloc, 1, &iclump, &cloc, status );

/* Store each clump parameter in a component of this CLUMP structure. */
               dloc = NULL;
               for( icol = 0; icol < ncpar; icol++ ) {
                  datNew( cloc, names[ icol ], "_DOUBLE", 0, NULL, status );
                  datFind( cloc, names[ icol ], &dloc, status );
                  datPutD( dloc, 0, NULL, cpars + icol, status );
                  datAnnul( &dloc, status );
               }

/* Store the supplied NDF in a component called "MODEL" of the CLUMP
   structure. */
               ndfPlace( cloc, "MODEL", &place, status );
               ndfCopy( indf1, &place, &indf2, status );
               ndfAnnul( &indf2, status );

/* Store an AST Region in a component called "OUTLINE" of the CLUMP
   structure. */
               if( region ) {
                  kpg1Wwrt( (AstObject *) region, "OUTLINE", cloc, status );
                  region = astAnnul( region );
               }

/* Free the locator to the CLUMP structure. */
               datAnnul( &cloc, status );
            }
         }

/* If required, store the STC-S clump description in the KeyMap, and then
   free the string returned by cupidClumpDesc. */
         if( stc_km && stcptr && ok > 0 ) {
            sprintf( key, "Shape_%d", ++istc );
            astMapPut0C( stc_km, key, stcptr, NULL );

            stclen = strlen( stcptr );
            if( stclen > max_stclen ) max_stclen = stclen;
         }
         stcptr = astFree( stcptr );

/* Free the NDF identifier. */
         ndfAnnul( &indf1, status );
      }
   }

/* Clean up static resources in cupidClumpDesc. */
   cupidClumpDesc( NDF__NOID, deconv, wcsmap, wcsfrm, dataunits, beamcorr,
                   backoff, stccol, velax, cpars, &names, &units, &ncpar,
                   &ok, &stcptr, &region, status );

/* Tell the user how many usable clumps there are and how many were rejected
   due to being smaller than the beam size. */

   if( nsmall1 == 1 ) {
      msgOutiff( MSG__NORM, "", "1 further clump rejected because it "
                 "is smaller than the spatial beam width.", status );
   } else if( nsmall1 > 1 ) {
      msgOutiff( MSG__NORM, "", "%d further clumps rejected because "
                 "they are smaller than the spatial beam width.",
                 status, nsmall1 );
   }

   if( nsmall2 == 1 ) {
      msgSetc( "W", ( ndim > 1 ) ? "(this clump may also "
               "be smaller than the spatial beam width)" : "" );
      msgOutiff( MSG__NORM, "", "1 further clump rejected because it "
                 "is smaller than the spectral resolution ^W.", status );

   } else if( nsmall2 > 1 ) {
      msgSeti( "N", nsmall2 );
      msgSetc( "W", ( ndim > 1 ) ? "(some of these may also "
               "be smaller than the spatial beam width)" : "" );
      msgOutif( MSG__NORM, "", "^N further clumps rejected because "
                 "they are smaller than the spectral resolution ^W.",
                 status );
   }

   if( nbad == 1 ) {
     msgOutif( MSG__NORM, "", "1 further clump rejected because it includes "
             "too many bad pixels.", status );
   } else if( nbad > 1 ) {
     msgSeti( "N", nbad );
     msgOutif( MSG__NORM, "", "^N further clumps rejected because they include "
             "too many bad pixels.", status );
   }

   if( iclump == 0 ) {
     msgOutif( MSG__NORM, "", "No usable clumps found.", status );
   } else if( iclump == 1 ){
     msgOutif( MSG__NORM, "", "One usable clump found.", status );
   } else {
     msgSeti( "N", iclump );
     msgOutif( MSG__NORM, "", "^N usable clumps found.", status );
   }
   msgBlankif( MSG__NORM, status );

/* Resize the array of clump structures, and return the size of the array. */
   if( aloc && iclump < nndf && iclump ) datAlter( aloc, 1, &iclump, status );
   *nclumps = iclump;

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* See if a KAPPA_style output catalogue is to be created. If not, annull the
   null parameter error. */
   if( param1 ) {
      parGet0c( param1, cat1, MAXCAT, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         cat1[ 0 ] = 0;
      }
   } else {
      cat1[ 0 ] = 0;
   }

/* See if a JSA_style output catalogue is to be created. If not, annull the
   null parameter error. */
   if( param2 ) {
      parGet0c( param2, cat2, MAXCAT, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         cat2[ 0 ] = 0;
      }
   } else {
      cat2[ 0 ] = 0;
   }

/* If either catalogue is to be created.... */
   if( ( cat1[ 0 ] || cat2[ 0 ] ) && *status == SAI__OK ) {

/* Remove any rows in the table which describe clumps smaller than the
   beam size (these will have been set to bad values above). The good
   rows are shuffled down to fill the gaps left by the bad rows. */
      iclump = 0;
      for( irow = 0; irow < nndf; irow++ ) {
         if( tab[ irow ] != VAL__BADD ) {
            if( irow != iclump ) {
               t = tab + irow;
               tj = tab + iclump;
               for( icol = 0; icol < ncpar; icol++ ) {
                  *tj = *t;
                  tj += nndf;
                  t += nndf;
               }
            }
            iclump++;
         }
      }

/* If required, create the KAPPA-style catalogue. */
      if( cat1[ 0 ] && *status == SAI__OK && iclump ){

/* Create a Frame with "ncpar" axes describing the table columns. Set the
   axis Symbols and Units to the column names and units. Any axis which
   initially has a unit of "deg" is a sky axis. Since the AST SkyFrame
   class requires rads rather than degs, we initially set such axes to
   "rad".  */
         frm1 = astFrame( ncpar, "Domain=PARAMETERS,Title=Clump parameters" );
         for( icol = 0; icol < ncpar; icol++ ) {
            sprintf( attr, "Symbol(%d)", icol + 1 );
            astSetC( frm1, attr, names[ icol ] );
            sprintf( attr, "Unit(%d)", icol + 1 );
            if( !strcmp( units[ icol ], "deg" ) ) {
               astSetC( frm1, attr, "rad" );
            } else {
               astSetC( frm1, attr, units[ icol ] );
            }
         }

/* Ensure the ActiveUnit flag is set for this frame so that we can swap
   between rads and degs automatically if required. */
         astSetActiveUnit( frm1, 1 );

/* Create a Mapping (a PermMap) from the Frame representing the "ncpar" clump
   parameters, to the "ndim" Frame representing clump centre pixel positions.
   The inverse transformation supplies bad values for the other parameters. */
         map = (AstMapping *) astPermMap( ncpar, NULL, ndim, NULL, NULL, " " );

/* If no WCS FrameSet was supplied.... */
         if( !iwcs ) {

/* Create a Frame with "ndim" axes describing the pixel coords at the
   clump centre. */
            frm2 = astFrame( ndim, "Domain=PIXEL,Title=Pixel coordinates" );
            astSetC( frm2, "Symbol(1)", "P1" );
            if( ndim > 1 ) {
               astSetC( frm2, "Symbol(2)", "P2" );
               if( ndim > 2 ) astSetC( frm2, "Symbol(3)", "P3" );
            }

/* Create a FrameSet to store in the output catalogue. It has two Frames,
   the base Frame has "ncpar" axes - each axis describes one of the table
   columns. The current Frame has 2 axes and describes the clump (x,y)
   position. The ID value of FIXED_BASE is a special value recognised by
   kpg1Wrlst. */
            iwcs = astFrameSet( frm1, "ID=FIXED_BASE" );
            astAddFrame( iwcs, AST__BASE, map, frm2 );
            astSetI( iwcs, "CURRENT", 1 );

/* If a WCS FrameSet was supplied, add in "frm1" as the base Frame,
   connecting it to the original PIXEL Frame or Current Frame (as
   selected by "usewcs") using "map". */
         } else {

/* Add the new Frame describing the catalogue columns into the FrameSet,
   leaving it the current Frame. If the catalogue position and width
   columns holds values in pixel coordinates, connect the new Frame to the
   PIXEL Frame using the "map" mapping. If the catalogue position and width
   columns holds values in WCS coordinates, connect the new Frame to the
   current Frame using the "map" mapping. */
            astInvert( map );
            astAddFrame( iwcs, ( usewcs ? AST__CURRENT : pixfrm ), map, frm1 );

/* Now change the units associated with any sky axes in the base Frame
   from "rad" to "deg" (the column values are stored in degs). This will
   automatically re-map the Frame so that the column deg values get
   converted to rad values as required by AST. */
            for( icol = 0; icol < ncpar && *status == SAI__OK; icol++ ) {
               if( !strcmp( units[ icol ], "deg" ) ){
                  sprintf( attr, "Unit(%d)", icol + 1 );
                  astSetC( iwcs, attr, "deg" );
               }
            }

/* Set the same Frame to be the base Frame as well as the current Frame. */
            astSetI( iwcs, "Base", astGetI( iwcs, "Current" ) );

/* Set the ID attribute of the FrameSet to "FIXED_BASE" in order to force
   kpg1_wrlst to write out the positions in the original base Frame. */
            astSet( iwcs, "ID=FIXED_BASE" );
         }

/* Create the output catalogue */
         kpg1Wrcat( param1, nndf, iclump, ncpar, tab, AST__BASE, iwcs,
                    ttl, 1, NULL, stc_km, NULL, hist, 1, status );
       }

/* If required, create the JSA-style catalogue. */
      if( cat2[ 0 ] && *status == SAI__OK && iclump ){

/* Create an AST FitsTable structure to act as a staging post for the
   FITS binary table. */
         table = astFitsTable( NULL, " " );

/* Add each column of floating point values to the FitsTable. */
         for( icol = 0; icol < ncpar; icol++ ) {
            astAddColumn( table, names[ icol ], AST__DOUBLETYPE, 0, NULL,
                          units[ icol ] );
            astPutColumnData( table, names[ icol ], 0, iclump*sizeof( double ),
                              tab + nndf*icol );
         }

/* If required, add a string column holding STC shapes to the FitsTable. */
         if( stc_km && astMapGet0C( stc_km, "COLNAMES", &cname ) ) {
            astAddColumn( table, cname, AST__STRINGTYPE, 0, NULL, "" );

            if( istc != iclump ) {
               if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( "", "cupidstoreclumps: inconsistent numbers of "
                           "clumps (%d) and STC outlines (%d) (programming "
                           "error).", status, iclump, istc );
               }

            } else {
               stc_data = astCalloc( max_stclen, iclump );
               if( *status == SAI__OK ) {
                  p1 = stc_data;
                  for( istc = 1; istc <= nndf; istc++ ) {
                     sprintf( key, "Shape_%d", istc );
                     if( astMapGet0C( stc_km, key, (const char **) &stcptr ) ) {
                        strncpy( p1, stcptr, max_stclen );
                        p1 += max_stclen;
                     }
                  }

                  astPutColumnData( table, cname, max_stclen,
                                    max_stclen*iclump, stc_data );
                  stc_data = astFree( stc_data );
               }
            }
         }

/* Create a new empty FITS file, and get a FITSIO unit number for it.
   The current HDU is the primary HDU on exit. */
         cvgCreat( param2, 1, 1, &fptr, status );


/* Get a FitsChan holding the contents of the FITS extension from the
   input NDF. Annul the error and create an empty FitsChan if the NDF
   has no FITS extension. */
         if( *status == SAI__OK ) {
            kpgGtfts( indf, &fc, status );
            if( *status == KPG__NOFTS ) {
               errAnnul( status );
               fc = astFitsChan( NULL, NULL, " " );
            }

/* Ensure the FitsChan contains a PRODUCT keyword set to "clump". */
            atlPtfts( fc, "PRODUCT", "clump", "This file contains "
                      "a clump catalogue", status );

/* Erase all CADC style provenance headers. */
            astClear( fc, "Card" );
            while( astFindFits( fc, "PRVCNT", NULL, 0 ) ) astDelFits( fc );
            astClear( fc, "Card" );
            while( astFindFits( fc, "PRV%d", NULL, 0 ) ) astDelFits( fc );
            astClear( fc, "Card" );
            while( astFindFits( fc, "OBSCNT", NULL, 0 ) ) astDelFits( fc );
            astClear( fc, "Card" );
            while( astFindFits( fc, "OBS%d", NULL, 0 ) ) astDelFits( fc );
            astClear( fc, "Card" );
            while( astFindFits( fc, "FILEID", NULL, 0 ) ) astDelFits( fc );

/* Clean all standard cards from the FitsChan. */
            cvgClean( fc, status );

/* Put the contents of the FitsChan into the current (i.e. primary) HDU. */
            cvgFc2hd( fc, 0, fptr, status );
         }

/* Write CADC-style provenance records to the current (i.e primary) HDU. */
         prov = ndgReadProv( NDF__NOID, "CUPID:FINDCLUMPS", status );
         ndgPutProv( prov, indf, NULL, 0, status );
         cvgPcadc( prov, fptr, status );
         prov = ndgFreeProv( prov, status );

/* Copy the contents of the FitsTable to the FITS file. */
         cvgFt2bt( table, fptr, "CUPID:FINDCLUMPS", 0, 0, status );

/* Add CHECKSUM and DATASUM headers. */
         if( *status == SAI__OK ) {
            int fstat = 0;
            ffpcks( fptr, &fstat );
         }

/* Close the FITS file. */
         cvgClose( &fptr, status );

      }
   }

L999:

/* If required, annul the locator for the array of CLUMP structures. */
   if( aloc ) datAnnul( &aloc, status );

/* Free resources. */
   if( stc_km ) stc_km = astAnnul( stc_km );
   if( line ) line = astFree( line );
   tab = astFree( tab );
   cpars = astFree( cpars );

/* Switch group history and provenance recording back to their original
   states. */
   ndgHltgh( old_ghstate, NULL, status );
   ndgHltpv( old_pvstate, NULL, status );

/* End the AST context. */
   astEnd;

}
