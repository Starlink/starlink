#include "irq_err.h"
#include "mers.h"
#include "ndf.h"
#include "ndf_err.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "sae_par.h"
#include "star/grp.h"
#include "star/irq.h"
#include "star/kaplibs.h"
#include "star/ndg.h"
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

/* Prototypes for local private functions. */
static int Report( AstKeyMap *km, int itest, const char *format,
                   int *status, ... );
static void CompareFrameAttr( AstKeyMap *km, int itest, int ifrm,
                              AstFrame *frm1, AstFrame *frm2,
                              const char *attr, int *ok, int *status );
static void GetValueAndQual( const char *param, const char *quals,
                             char defqual, double *value, char *qual,
                             int *status );
static int FindNBad( int indf, const char *comp, int *status );
static AstKeyMap *GetQualNames( int indf, int *status );
static AstKeyMap *GetRootAncestors( int indf, int *status );
static int CountQualityDiff( int indf1, int indf2, int *nel, int *status );
static double FindMaxPixelDiff( int indf1, int indf2, const char *comp,
                                char qual, int *status );





F77_SUBROUTINE(ndfcompare)( INTEGER(status) ){
/*
*+
*  Name:
*     NDFCOMPARE

*  Purpose:
*     Compares a pair of NDFs for equivalence.

*  Language:
*     C (designed to be called from Fortran)

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDFCOMPARE( STATUS )

*  Arguments:
*     status = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application compares two supplied NDFs, and sets the
*     Parameter SIMILAR to "FALSE" if they are significantly different
*     in any way, and to "TRUE" if they are not significantly
*     different.
*
*     If they are not similar, a textual description of the differences
*     is written to standard output, and to any file specified by
*     Parameter REPORT.
*
*     The two NDFS are compared in the following ways. Each test has
*     an integer identifier, and the list of tests to be used can be
*     controlled by Parameters DOTESTS and SKIPTESTS. Tests that are
*     not included by default are indicated by the test number being in
*     square brackets. Some tests have parameters that control the exact
*     nature of the test. These are listed in parentheses at the end of
*     the description test listed below.
*
*        1   - The number of pixel axes are compared.
*        2   - The pixel bounds are compared.
*        3   - The list of co-ordinate systems in the WCS FrameSet are
*              compared.
*        4   - The presence or absence of NDF components are compared (COMP).
*        5   - The sky positions of a grid of pixels are compared (ACCPOS).
*        6   - The data units strings are compared (WHITE).
*        7   - The label strings are compared (CASE,WHITE).
*        8   - The title strings are compared (CASE,WHITE).
*        9   - The data types are compared.
*        10  - The lists of NDF extensions are compared.
*        11  - The number of bad DATA values are compared (NBAD).
*        12  - The number of bad VARIANCE values are compared (NBAD).
*        13  - The pixel DATA values are compared (ACCDAT).
*        14  - The pixel VARIANCE values (if any) are compared (ACCVAR).
*        15  - The pixel QUALITY values (if any) are compared (NBAD).
*        16  - The QUALITY names (if any) are compared.
*       [17] - The lists of root ancestor NDFs that were used to create
*              each NDF are compared.

*  Usage:
*     ndfcompare in1 in2 [report]

*  ADAM Parameters:
*     ACCDAT = LITERAL (Read)
*        The maximum difference allowed between two pixel data values
*        for them to be considered equivalent. The supplied string
*        should contain a numerical value followed by a single character
*        (case insensitive) from the list below indicating how the
*        numerical value is to be used.
*
*        - "V" --- The numerical value is a signal-to-noise value. The
*        absolute difference in pixel data value is divided by the
*        square root of the smaller of the two variances associated with
*        the pixels (one from each input NDF). If the resulting ratio is
*        smaller than the ACCDAT value, then the two pixel data values
*        are considered to be equivalent. An error is reported if either
*        NDF does not have a VARIANCE component.
*
*        - "R" --- The numerical value is a relative error. The absolute
*        difference between the two pixel data values is divided by the
*        absolute mean of the two data values. If the resulting ratio is
*        smaller than the ACCDAT value, then the two pixel data values
*        are considered to be equivalent. To avoid problems with pixels
*        where the mean is close to zero, a lower limit equal to the RMS
*        of the data values is placed on the mean value used in the
*        above ratio.
*
*        - "A" --- The numerical value is an absolute error. If the
*        absolute difference in pixel data value is smaller than the
*        ACCDAT value, then the two pixel data values are considered to
*        be equivalent.
*
*        If no character is included in the ACCDAT string, "R" is
*        assumed.  ["1E-6 R"]
*     ACCPOS = _DOUBLE (Read)
*        The maximum difference allowed between two axis values for
*        them to be considered equivalent, in units of pixels on the
*        corresponding pixel axes. [0.2]
*     ACCVAR = LITERAL (Read)
*        The maximum difference allowed between two pixel variance
*        values for them to be considered equivalent. The supplied
*        string should contain a numerical value followed by a single
*        character (case insensitive) from the list below indicating how
*        the numerical value is to be used.
*
*        - "R" --- The numerical value is a relative error. The absolute
*        difference in variance value is divided by the absolute mean of
*        the two variance values. If the resulting ratio is smaller than
*        the ACCVAR value, then the two pixel variances are considered to
*        be equivalent.
*
*        - "A" --- The numerical value is an absolute error. If the
*        absolute difference in variance values is smaller than the
*        ACCVAR value, then the two pixel variances are considered to be
*        equivalent.
*
*        If no character is included in the ACCVAR string, "R" is assumed.
*        ["1E-6 R"]
*     CASE = _LOGICAL (Read)
*        If TRUE, then string comparisons are case sensitive. Otherwise
*        they are case insensitive. [TRUE]
*     COMP = _LITERAL (Read)
*        A comma separated list of the NDF components to include in the
*        test. If a null (!) value is supplied, all NDF components are
*        included. [!]
*     DOTESTS() = _INTEGER (Read)
*        An initial list of indices for the tests to be performed, or
*        null (!) if all tests are to be included in the initial list.
*        This initial list is modified by excluding any tests specified
*        by Parameter SKIPTESTS. [!]
*     IN1 = NDF (Read)
*        The first NDF.
*     IN2 = NDF (Read)
*        The second NDF.
*     NBAD = LITERAL (Read)
*        The maximum difference allowed between the number of bad values
*        in each NDF. The same value is used for both DATA and VARIANCE
*        arrays. It is also used as the maximum number of pixel that can
*        have different QUALITY values. The supplied string should contain
*        a numerical value followed by a single character (case insensitive)
*        from the list below indicating how the numerical value is to
*        be used.
*
*        - "R" --- The numerical value is a relative error. The absolute
*        difference in the number of bad values is divided by the mean
*        number of bad values in both NDFs (for the QUALITY array, the
*        total number of pixels in the NDF is used as the denominator in
*        this ratio). If the resulting ratio is smaller than the NBAD value,
*        then the two NDFs are considered to be equivalent for the purposes
*        of this test.
*
*        - "A" --- The numerical value is an absolute error. If the absolute
*        difference in the number of bad values is smaller than the NBAD
*        value, then the two NDFs are considered to be equivalent for the
*        purposes of this test.
*
*        If no character is included in the NBAD string, "R" is assumed.
*        ["0.001 R"]
*     REPORT = LITERAL (Read)
*        The name of a text file to create in which details of the
*        differences found between the two NDFs will be store. [!]
*     SKIPTESTS() = _INTEGER (Read)
*        A list of indices for tests that are to removed from the initial
*        list of tests specified by Parameter DOTESTS. If a null (!) value
*        is supplied, the initial list is left unchanged. [15]
*     SIMILAR = _LOGICAL (Write)
*        Set to FALSE on exit if any of the used tests indicate that the
*        two NDFs differ.
*     WHITE = _LOGICAL (Read)
*        If TRUE, then trailing or leading white space is ignored when
*        comparing strings. [FALSE]

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-JUL-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}
*-
*/
   GENPTR_INTEGER(status)

/* Local Constants: */
#define MXTEST 15         /* The number of available tests */
#define NCOMP 9           /* The number of available NDF components */
#define MXCLEN 9          /* The Max length of an NDF component name */

/* Local Variables: */
   AstCmpMap *map;
   AstFrame *frm1;
   AstFrame *frm2;
   AstFrameSet *iwcs1;
   AstFrameSet *iwcs2;
   AstKeyMap *km1;
   AstKeyMap *km2;
   AstKeyMap *reports;
   AstWinMap *wmap;
   FILE *fd = NULL;
   Grp *grp = NULL;
   HDSLoc *xloc = NULL;
   char *comps[NCOMP];
   char *dump1;
   char *dump2;
   char attr[30];
   char cbuf1[200];
   char cbuf2[200];
   char cbuf[NCOMP*MXCLEN];
   char path1[ GRP__SZFNM + 1];
   char path2[ GRP__SZFNM + 1];
   char report[ GRP__SZFNM ];
   const char *ctemp;
   const char *key;
   const char *unit;
   double *coords1;
   double *coords2;
   double acc;
   double accpos;
   double at[ NDF__MXDIM ];
   double err;
   double ina[ NDF__MXDIM ];
   double inb[ NDF__MXDIM ];
   double maxerr;
   double outa[ NDF__MXDIM ];
   double outb[ NDF__MXDIM ];
   double posacc1[ NDF__MXDIM ];
   double posacc2[ NDF__MXDIM ];
   int axmax;
   int casesen;
   int dotest[ MXTEST ];
   int glbnd[ NDF__MXDIM ];
   int gubnd[ NDF__MXDIM ];
   int iaxis;
   int icomp;
   int iel;
   int iext;
   int ifrm;
   int ikey;
   int indf1;
   int indf2;
   int itest;
   int lbnd1[ NDF__MXDIM ];
   int lbnd2[ NDF__MXDIM ];
   int mnval[ MXTEST ];
   int mxval[ MXTEST ];
   int nbad1;
   int nbad2;
   int ncomp;
   int ndiff;
   int ndim1;
   int ndim2;
   int ndim;
   int nel;
   int next;
   int nfrm;
   int nkey;
   int npix;
   int ntest;
   int nwcs;
   int ok;
   int olbnd[ NDF__MXDIM ];
   int oplen;
   int oubnd[ NDF__MXDIM ];
   int ovlap;
   int similar;
   int state1;
   int state2;
   int tests[ MXTEST ];
   int ubnd1[ NDF__MXDIM ];
   int ubnd2[ NDF__MXDIM ];
   int white;
   int xsize1;
   size_t size;
   size_t xsize;
   unsigned char bb1;
   unsigned char bb2;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Start NDF and AST contexts. */
   ndfBegin();
   astBegin;

/* Store pointers to the array of strings used to store NDF component
   names. */
   for( icomp = 0; icomp < NCOMP; icomp++ ) {
      comps[icomp] = cbuf + MXCLEN*icomp;
   }

/* Get identifiers for the two input NDF. We use NDG (via kpg1Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "IN1", 1, 1, "", &grp, &size, status );
   ndgNdfas( grp, 1, "READ", &indf1, status );
   grpDelet( &grp, status );

   kpg1Rgndf( "IN2", 1, 1, "", &grp, &size, status );
   ndgNdfas( grp, 1, "READ", &indf2, status );
   grpDelet( &grp, status );

/* Get the basic WCS info about each NDF. */
   ndfBound( indf1, NDF__MXDIM, lbnd1, ubnd1, &ndim1, status );
   ndfGtwcs( indf1, &iwcs1, status );

   ndfBound( indf2, NDF__MXDIM, lbnd2, ubnd2, &ndim2, status );
   ndfGtwcs( indf2, &iwcs2, status );

/* Get the bounds of the overlap region, and set a flag indicating if
   there is any overlap in pixel space. */
   ovlap = 1;
   ndim = ( ndim1 < ndim2 ) ? ndim1 : ndim2;
   for( iaxis = 0; iaxis < ndim; iaxis++ ) {
      olbnd[ iaxis ] = ( lbnd2[ iaxis ] > lbnd1[ iaxis ] ) ? lbnd2[ iaxis ] : lbnd1[ iaxis ];
      oubnd[ iaxis ] = ( ubnd2[ iaxis ] < ubnd1[ iaxis ] ) ? ubnd2[ iaxis ] : ubnd1[ iaxis ];
      if( oubnd[ iaxis ] < olbnd[ iaxis ] ) ovlap = 0;
   }

/* See if string comparisons should ignore leading and trailing white
   space. */
   parGet0l( "WHITE", &white, status );

/* See if string comparisons should ignore case. */
   parGet0l( "CASE", &casesen, status );

/* Get the initial list of tests to perform. Test identifiers are
   one-based integers. The "tests" array is a list of test identifiers.
   The "dotest" array has a boolean flag for each test indicating if the
   test should be performed. If null (!) is supplied, annul the error and
   perform all tests. */
   if( *status == SAI__OK ) {

      for( itest = 0; itest < MXTEST; itest++ ) {
         mxval[ itest ] = MXTEST;
         mnval[ itest ] = 1;
      }

      parGrmvi( "DOTESTS", MXTEST, mnval, mxval, tests, &ntest, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         for( itest = 0; itest < MXTEST; itest++ ) dotest[ itest ] = 1;
      } else {
         memset( dotest, 0, sizeof( dotest ) );
         for( itest = 0; itest < ntest; itest++ ) {
            dotest[ tests[itest] - 1 ] = 1;
         }
      }
   }

/* Remove from the list any tests that are specified by parameter SKIPTESTS. */
   if( *status == SAI__OK ) {
      parGrmvi( "SKIPTESTS", MXTEST, mnval, mxval, tests, &ntest, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else {
         for( itest = 0; itest < ntest; itest++ ) {
            dotest[ tests[itest] - 1 ] = 0;
         }
      }
   }

/* Create an AST KeyMap to hold the text of each failed test. */
   reports = astKeyMap("SortBy=AgeDown");

/* Assume the NDFs are equivalent. */
   similar = 1;

/* Loop round, doing each required test. */
   for( itest = 0; itest < MXTEST && *status == SAI__OK; itest++ ) {
      if( dotest[ itest ] ) {

/* Compare the number of pixel axes. */
/* --------------------------------- */
         if( itest == 0 ) {
            if( ndim1 != ndim2 ) {
               similar = Report( reports, itest, "IN1 has %d pixel axes but IN2 "
                                 "has %d", status, ndim1, ndim2 );
            }

/* Compare the pixel bounds. */
/* ------------------------- */
         } else if( itest == 1 ) {
            for( iaxis = 0; iaxis < ndim1; iaxis++ ) {
               if( lbnd1[ iaxis ] != lbnd2[ iaxis ] ) {
                  similar = Report( reports, itest, "The lower bound for pixel "
                                    "axis %d is %d in IN1 but %d in IN2.",
                                    status, iaxis + 1, lbnd1[ iaxis ],
                                    lbnd2[ iaxis ] );
               }
               if( ubnd1[ iaxis ] != ubnd2[ iaxis ] ) {
                  similar = Report( reports, itest, "The upper bound for pixel "
                                    "axis %d is %d in IN1 but %d in IN2.",
                                    status, iaxis + 1, ubnd1[ iaxis ],
                                    ubnd2[ iaxis ] );
               }
            }

/* Compare the list of coordinate systems in the WCS FrameSets */
/* ----------------------------------------------------------- */
         } else if( itest == 2 ) {
            ok = 1;

/* First check they have the same number of Frames. */
            nfrm = astGetI( iwcs1, "NFrame" );
            if( nfrm != astGetI( iwcs2, "NFrame" ) ) {
               ok = Report( reports, itest, "IN1 has %d WCS Frames but IN2 has "
                            "%d", status, nfrm, astGetI( iwcs2, "NFrame" ) );
            }

/* Then check the principle attributes match in each Frame. */
            if( ok ) {
               for( ifrm = 1; ifrm <= nfrm; ifrm++ ) {
                  frm1 = astGetFrame( iwcs1, ifrm );
                  frm2 = astGetFrame( iwcs2, ifrm );

                  CompareFrameAttr( reports, itest, ifrm, frm1, frm2, "Naxes", &ok, status );
                  CompareFrameAttr( reports, itest, ifrm, frm1, frm2, "Domain", &ok, status );
                  CompareFrameAttr( reports, itest, ifrm, frm1, frm2, "System", &ok, status );
                  CompareFrameAttr( reports, itest, ifrm, frm1, frm2, "Title", &ok, status );

                  for( iaxis = 0; iaxis < astGetI( frm1, "Naxes" ); iaxis++ ) {
                     sprintf( attr, "Label(%d)", iaxis + 1 );
                     CompareFrameAttr( reports, itest, ifrm, frm1, frm2, attr, &ok, status );
                     sprintf( attr, "Symbol(%d)", iaxis + 1 );
                     CompareFrameAttr( reports, itest, ifrm, frm1, frm2, attr, &ok, status );
                     sprintf( attr, "Unit(%d)", iaxis + 1 );
                     CompareFrameAttr( reports, itest, ifrm, frm1, frm2, attr, &ok, status );
                  }

/* Rather than checking every attribute of every possible class of Frame
   (which would be very difficult to maintain), we just compare the textual
   dumps of each Frame. */
                  if( ok ) {
                     dump1 = astToString( frm1 );
                     dump2 = astToString( frm2 );
                     if( dump1 && dump2 ){

/* Find the first characters that differ in dump1 and dump2. */
                        const char *p1 = dump1;
                        const char *p2 = dump2;
                        while( *p1 && *p2 && ( *p1 == *p2 ) ){
                           p1++;
                           p2++;
                        }

/* Any difference found? */
                        if( *p1 || *p2 ) {

/* Back up to the previous newline character, and find the length of the
   line. */
                           if( *p1 ) {
                              while( p1[-1] != '\n' && p1 > dump1 ) p1--;
                           }
                           if( *p2 ) {
                              while( p2[-1] != '\n' && p2 > dump2 ) p2--;
                           }

/* Make the report, including the first mismatching string. */
                           ok = Report( reports, itest, "WCS Frame %d (%s) has "
                                        "different attributes in IN1 "
                                        "(starting at '%.*s') and IN2 "
                                        "(starting at '%.*s').", status, ifrm,
                                        astGetC( frm1, "Domain" ),
                                        strcspn( p1, "\n" ), p1,
                                        strcspn( p2, "\n" ), p2 );
                        }
                     }
                     dump1 = astFree( dump1 );
                     dump2 = astFree( dump2 );
                  }

/* Release local resources. */
                  frm1 = astAnnul( frm1 );
                  frm2 = astAnnul( frm2 );
               }
            }

            if( !ok ) similar = 0;

/* Check the presence or absence of NDF components specified by parameter
   COMP. */
/* -------------------------------------------------------------------- */
         } else if( itest == 3 ) {
            parChoiv( "COMP", NCOMP, "TITLE,LABEL,UNITS,DATA,VARIANCE,QUALITY,"
                      "AXIS,WCS,HISTORY", comps, MXCLEN, &ncomp, status );
            if( *status == PAR__NULL ) {
               errAnnul( status );
               comps[ 0 ] = "TITLE";
               comps[ 1 ] = "LABEL";
               comps[ 2 ] = "UNITS";
               comps[ 3 ] = "DATA";
               comps[ 4 ] = "VARIANCE";
               comps[ 5 ] = "QUALITY";
               comps[ 6 ] = "AXIS";
               comps[ 7 ] = "WCS";
               comps[ 8 ] = "HISTORY";
               ncomp = 9;
            }

            for( icomp = 0; icomp < ncomp; icomp++ ) {
               ndfState( indf1, comps[icomp], &state1, status );
               ndfState( indf2, comps[icomp], &state2, status );
               if( state1 && !state2 ) {
                  similar = Report( reports, itest, "The %s component is "
                                    "defined in IN1 but not in IN2",
                                    status, comps[icomp] );
               } else if( !state1 && state2 ) {
                  similar = Report( reports, itest, "The %s component is "
                                    "defined in IN2 but not in IN1",
                                    status, comps[icomp] );
               }
            }

/* Compare the WCS positions of a grid of pixels. It would take too long
   to do all pixels, so we remap the PIXEL Frame so that it voers a
   smaller grid that spans no more than 20 elements on each axis. */
/* ------------------------------------------------------------------- */
         } else if( itest == 4 ) {

/* Can only do this if the number of pixel axes are the same, and the
   number of WCS axes are the same, and there is some overlap. */
            nwcs = astGetI( iwcs1, "Naxes" );
            if( ndim1 == ndim2 && nwcs == astGetI( iwcs2, "Naxes" ) && ovlap ) {

/* Get the required accuracy as a number of pixels. */
               parGet0d( "ACCPOS", &accpos, status );

/* Get the axis scales for the first NDF, and get the corresponding
   tolerance on axis value. */
               for( iaxis = 0; iaxis < ndim1; iaxis++ ) {
                  at[ iaxis ] = 0.5*( ubnd1[ iaxis ] - lbnd1[ iaxis ] + 1 );
               }
               kpg1Pxscl( iwcs1, at, posacc1, status );
               for( iaxis = 0; iaxis < nwcs; iaxis++ ) {
                  posacc1[ iaxis ] *= accpos;
               }

/* Get the axis scales for the second NDF. */
               for( iaxis = 0; iaxis < ndim2; iaxis++ ) {
                  at[ iaxis ] = 0.5*( ubnd2[ iaxis ] - lbnd2[ iaxis ] + 1 );
               }
               kpg1Pxscl( iwcs2, at, posacc2, status );
               for( iaxis = 0; iaxis < nwcs; iaxis++ ) {
                  posacc2[ iaxis ] *= accpos;
               }

/* Find the smaller tolerance on each WCS axis. */
               for( iaxis = 0; iaxis < nwcs; iaxis++ ) {
                  if( posacc2[ iaxis ] < posacc1[ iaxis ] ) posacc1[ iaxis ] = posacc2[ iaxis ];
               }

/* Get a WinMap that maps the overlap region in pixel coords onto a box
   that spans no more than 20 pixels on any axis. */
               nel = 1;
               for( iaxis = 0; iaxis < ndim; iaxis++ ) {
                  outa[ iaxis ] = olbnd[ iaxis ];
                  outb[ iaxis ] = oubnd[ iaxis ];
                  npix = oubnd[ iaxis ] - olbnd[ iaxis ] + 1;
                  if( npix > 20 ) npix = 20;
                  ina[ iaxis ] = 1.0;
                  inb[ iaxis ] = npix;
                  glbnd[ iaxis ] = 1;
                  gubnd[ iaxis ] = npix;
                  nel *= npix;
               }
               wmap = astWinMap( ndim, ina, inb, outa, outb, " " );

/* Get the PIXEL -> WCS Mapping for the first NDF, and prepend it with the
   above WinMap. The overlap region then has the bounds glbnd, uhbnd in
   the input space of the Mapping. The NDF library ensures that the
   PIXEL Frame is always Frame 2. */
               map = astCmpMap( wmap, astGetMapping( iwcs1, 2, AST__CURRENT ),
                                1, " " );

/* Get the WCS coords at each pixel in this reduced grid. */
               coords1 = astMalloc( nwcs*nel*sizeof(*coords1) );
               astTranGrid( map, ndim, glbnd, gubnd, 0.0, 50, 1, nwcs,
                            nel, coords1 );

/* Do the same for the second NDF. */
               map = astCmpMap( wmap, astGetMapping( iwcs2, 2, AST__CURRENT ),
                                1, " " );
               coords2 = astMalloc( nwcs*nel*sizeof(*coords2) );
               astTranGrid( map, ndim, glbnd, gubnd, 0.0, 50, 1, nwcs,
                            nel, coords2 );

/* Loop round each tested point. */
               maxerr = 0.0;
               for( iel = 0; iel < nel; iel++ ) {

/* Loop round each axis value fat the current tested point. */
                  for( iaxis = 0; iaxis < nwcs; iaxis++ ) {

/* Get the size of the error (i.e the difference in WCS axis value) as a
   fraction of the required accuracy. Record the maximum error. */
                     err = fabs( coords1[iaxis*nel+iel] - coords2[iaxis*nel+iel] )
                                 / posacc1[ iaxis ];
                     if( err > maxerr ) {
                        maxerr = err;
                        axmax = iaxis;
                     }
                  }
               }

/* Free resources. */
               coords1 = astFree( coords1 );
               coords2 = astFree( coords2 );

/* Make a report if the maximum error was too large. */
               if( maxerr > 1.0 ) {
                  maxerr *= posacc1[ axmax ];

                  sprintf( attr, "InternalUnit(%d)", axmax+1 );
                  unit = astGetC( iwcs1, attr );
                  if( unit && !strcmp( unit, "rad" ) ){
                     maxerr *= AST__DR2D;
                     unit = "deg";
                     if( maxerr < 1.0 ) {
                        maxerr *= 3600.0;
                        unit = "arc-sec";
                     }
                  }

                  sprintf( attr, "Label(%d)", axmax+1 );
                  similar = Report( reports, itest, "Differences in position of "
                                    "up to %g %s were found between "
                                    "corresponding pixels within IN1 and "
                                    "IN2 on the %s axis", status,
                                     maxerr, unit, astGetC( iwcs1, attr ) );
               }
            }

/* Compare the data units string. */
/* ------------------------------ */
         } else if( itest == 5 ) {
            cbuf1[0] = 0;
            ndfCget( indf1, "UNITS", cbuf1, sizeof(cbuf1), status );

            cbuf2[0] = 0;
            ndfCget( indf2, "UNITS", cbuf2, sizeof(cbuf2), status );

            if( white ){
               astRemoveLeadingBlanks( cbuf1 );
               astRemoveLeadingBlanks( cbuf2 );
               astChrTrunc( cbuf1 );
               astChrTrunc( cbuf2 );
            }

            if( strcmp( cbuf1, cbuf2 ) ) {
               similar = Report( reports, itest, "IN1 has data units '%s' but "
                                 "IN2 has data units '%s'.", status, cbuf1,
                                 cbuf2 );
            }

/* Compare the label string. */
/* ------------------------- */
         } else if( itest == 6 ) {
            cbuf1[0] = 0;
            ndfCget( indf1, "LABEL", cbuf1, sizeof(cbuf1), status );

            cbuf2[0] = 0;
            ndfCget( indf2, "LABEL", cbuf2, sizeof(cbuf2), status );

            if( white ){
               astRemoveLeadingBlanks( cbuf1 );
               astRemoveLeadingBlanks( cbuf2 );
               astChrTrunc( cbuf1 );
               astChrTrunc( cbuf2 );
            }

            if( ( casesen && strcmp( cbuf1, cbuf2 ) ) ||
                ( !casesen && !astChrMatch( cbuf1, cbuf2 ) ) ) {
               similar = Report( reports, itest, "IN1 has label '%s' but "
                                 "IN2 has label '%s'.", status, cbuf1,
                                 cbuf2 );
            }

/* Compare the Title string. */
/* ------------------------- */
         } else if( itest == 7 ) {
            cbuf1[0] = 0;
            ndfCget( indf1, "TITLE", cbuf1, sizeof(cbuf1), status );

            cbuf2[0] = 0;
            ndfCget( indf2, "TITLE", cbuf2, sizeof(cbuf2), status );

            if( white ){
               astRemoveLeadingBlanks( cbuf1 );
               astRemoveLeadingBlanks( cbuf2 );
               astChrTrunc( cbuf1 );
               astChrTrunc( cbuf2 );
            }

            if( ( casesen && strcmp( cbuf1, cbuf2 ) ) ||
                ( !casesen && !astChrMatch( cbuf1, cbuf2 ) ) ) {
               similar = Report( reports, itest, "IN1 has title '%s' but "
                                 "IN2 has title '%s'.", status, cbuf1,
                                 cbuf2 );
            }

/* Compare the data types. */
/* ----------------------- */
         } else if( itest == 8 ) {
            cbuf1[0] = 0;
            ndfType( indf1, "DATA", cbuf1, sizeof(cbuf1), status );
            cbuf2[0] = 0;
            ndfType( indf2, "DATA", cbuf2, sizeof(cbuf2), status );

            if( strcmp( cbuf1, cbuf2 ) ) {
               similar = Report( reports, itest, "IN1 has data type '%s' but "
                                 "IN2 has data type '%s'.", status, cbuf1,
                                 cbuf2 );
            }

/* Compare the list of NDF extensions. */
/* ----------------------------------- */
         } else if( itest == 9 ) {
            km1 = astKeyMap( " " );
            ndfXnumb( indf1, &next, status );
            for( iext = 0; iext < next; iext++ ) {
               ndfXname( indf1, iext + 1, cbuf1, sizeof(cbuf1), status );
               ndfXloc( indf1, cbuf1, "Read", &xloc, status );
               datSize( xloc, &xsize, status );
               datAnnul( &xloc, status );
               astMapPut0I( km1, cbuf1, xsize, NULL );
            }

            ndfXnumb( indf2, &next, status );
            for( iext = 0; iext < next; iext++ ) {
               ndfXname( indf2, iext + 1, cbuf2, sizeof(cbuf2), status );
               if( astMapGet0I( km1, cbuf2, &xsize1 ) ) {
                  ndfXloc( indf2, cbuf2, "Read", &xloc, status );
                  datSize( xloc, &xsize, status );
                  datAnnul( &xloc, status );
                  astMapRemove( km1, cbuf2 );

                  if( xsize != (size_t) xsize1 ) {
                     similar = Report( reports, itest, "Extension '%s' has "
                                       "size %zu in IN1 but size %zu in IN2.",
                                       status, cbuf2, xsize1, xsize );
                  }

               } else {
                  similar = Report( reports, itest, "IN2 has a '%s' extension "
                                    "but IN1 does not.", status, cbuf2 );
               }
            }

            next = astMapSize( km1 );
            for( iext = 0; iext < next; iext++ ) {
               similar = Report( reports, itest, "IN1 has a '%s' extension "
                                 "but IN2 does not.", status,
                                 astMapKey( km1, iext ) );
            }

/* Compare the number of bad data values. */
/* -------------------------------------- */
         } else if( itest == 10 ) {

/* Get the NBAD value and the qualifying character that says how to
   interpret the value. */
            GetValueAndQual( "NBAD", "RA", 'R', &acc, cbuf2, status );

/* Count the number of bad data values in IN1, and IN2. */
            nbad1 = FindNBad( indf1, "DATA", status );
            nbad2 = FindNBad( indf2, "DATA", status );

/* Make a report if these numbers differ by too much. */
            if( cbuf2[0] == 'R' ) {
               if( fabs( nbad1 - nbad2 ) > acc*( nbad1 + nbad2 )/2 ) {
                  similar = Report( reports, itest, "The number of bad data "
                                    "values in IN1 and IN2 differ by %g %%.",
                                    status, 200*fabs( 1.0*(nbad1 - nbad2) )
                                                   /(nbad1 + nbad2) );
               }
            } else if( fabs( nbad1 - nbad2 ) > acc ) {
               similar = Report( reports, itest, "The number of bad data "
                                 "values in IN1 and IN2 differ by %d.",
                                 status, abs( nbad1 - nbad2 ) );
            }

/* Compare the number of bad variance values. */
/* ------------------------------------------ */
         } else if( itest == 11 ) {

/* Check that both NDFs have VARIANCE components. */
            ndfState( indf1, "VARIANCE", &state1, status );
            ndfState( indf2, "VARIANCE", &state2, status );
            if( state1 && state2 ) {

/* Get the NBAD value and the qualifying character that says how to
   interpret the value. */
               GetValueAndQual( "NBAD", "RA", 'R', &acc, cbuf2, status );

/* Count the number of bad variance values in IN1, and IN2. */
               nbad1 = FindNBad( indf1, "VARIANCE", status );
               nbad2 = FindNBad( indf2, "VARIANCE", status );

/* Make a report if these numbers differ by too much. */
               if( cbuf2[0] == 'R' ) {
                  if( fabs( nbad1 - nbad2 ) > acc*( nbad1 + nbad2 )/2 ) {
                     similar = Report( reports, itest, "The number of bad variance "
                                       "values in IN1 and IN2 differ by %g %%.",
                                       status, 200*fabs( nbad1 - nbad2 )/
                                                       ( nbad1 + nbad2 ) );
                  }
               } else if( fabs( nbad1 - nbad2 ) > acc ) {
                  similar = Report( reports, itest, "The number of bad variance "
                                    "values in IN1 and IN2 differ by %d.",
                                    status, abs( nbad1 - nbad2 ) );
               }
            }

/* Compare the data values. */
/* ------------------------ */
         } else if( itest == 12 ) {

/* Get the ACCDAT value and the qualifying character that says how to
   interpret the value. */
            GetValueAndQual( "ACCDAT", "RVA", 'R', &acc, cbuf2, status );

/* Find the largest difference in pixel data value, in the same system as
   the ACCDAT value. */
            maxerr = FindMaxPixelDiff( indf1, indf2, "DATA", cbuf2[0],
                                        status );

/* Make a report if there is no overlap (but only if the test on pixel
   bounds has not already reported this). */
            if( maxerr == VAL__MAXD ) {
               if( !dotest[ 1 ] ) {
                  similar = Report( reports, itest, "The pixel data values in"
                                   " IN1 and IN2 cannot be compared since "
                                   "the NDFs do not overlap.", status );
               }

/* Make a report if the difference is too large. */
            } else if( maxerr > acc ) {
               if( cbuf2[0] == 'R' ){
                  unit = "%";
                  maxerr *= 100.0;
               } else if( cbuf2[0] == 'V' ){
                  unit = "sigma";
               } else {
                  cbuf2[0] = 0;
                  ndfCget( indf1, "UNITS", cbuf2, sizeof(cbuf2), status );
                  unit = cbuf2;
               }

               similar = Report( reports, itest, "The pixel data values in IN1 "
                                 "and IN2 differ by up to %g %s.", status,
                                 maxerr, unit );
            }

/* Compare the variance values. */
/* ---------------------------- */
         } else if( itest == 13 ) {

/* Check that both NDFs have VARIANCE components. */
            ndfState( indf1, "VARIANCE", &state1, status );
            ndfState( indf2, "VARIANCE", &state2, status );
            if( state1 && state2 ) {

/* Get the ACCVAR value and the qualifying character that says how to
   interpret the value. */
               GetValueAndQual( "ACCVAR", "RA", 'R', &acc, cbuf2, status );

/* Find the largest difference in pixel data value, in the same system as
   the ACCDAT value. */
               maxerr = FindMaxPixelDiff( indf1, indf2, "VARIANCE",
                                          cbuf2[0], status );

/* Make a report if there is no overlap (but only if the test on pixel
   bounds or data values has not already reported this). */
               if( maxerr == VAL__MAXD ) {
                  if( !dotest[ 1 ] && !dotest[ 12 ] ) {
                     similar = Report( reports, itest, "The pixel variances in"
                                      " IN1 and IN2 cannot be compared since "
                                      "the NDFs do not overlap.", status );
                  }

/* Make a report if the difference is too large. */
               } else if( maxerr > acc ) {
                  if( cbuf2[0] == 'R' ){
                     unit = "%";
                     maxerr *= 100.0;
                  } else {
                     cbuf2[0] = '0';
                     ndfCget( indf1, "UNITS", cbuf2, sizeof(cbuf2), status );
                     if( *cbuf2 ) {
                        sprintf( cbuf1, "(%s)**2", cbuf2 );
                        unit = cbuf1;
                     } else {
                        unit = cbuf2;
                     }
                  }

                  similar = Report( reports, itest, "The pixel variance values "
                                    "in IN1 and IN2 differ by up to %g %s.",
                                    status, maxerr, unit );
               }
            }

/* Compare the QUALITY values. */
/* --------------------------- */
         } else if( itest == 14 ) {

/* Check that both NDFs have QUALITY components. */
            ndfState( indf1, "QUALITY", &state1, status );
            ndfState( indf2, "QUALITY", &state2, status );
            if( state1 && state2 ) {

/* Get the NBAD value and the qualifying character that says how to
   interpret the value. */
               GetValueAndQual( "NBAD", "RA", 'R', &acc, cbuf2, status );

/* Find the number of pixel that have different QUALITY values. */
               ndiff = CountQualityDiff( indf1, indf2, &nel, status );

/* Make a report if these numbers differ by too much. */
               if( ndiff == VAL__MAXI ){
                  if( !dotest[ 1 ] && !dotest[ 12 ] && !dotest[ 13 ] ) {
                     similar = Report( reports, itest, "The Quality values in "
                                       "IN1 and IN2 cannot be compared since the"
                                       "NDFs do not overlap.", status );
                  }

               } else if( ( cbuf2[0] == 'R' && ndiff > acc*nel ) ||
                          ( cbuf2[0] == 'A' && ndiff > acc ) ){
                  similar = Report( reports, itest, "%d pixels have differing "
                                   "quality values in IN1 and IN2.", status,
                                   ndiff );
               }

/* Also compare the bad bits values. */
               ndfBb( indf1, &bb1, status );
               ndfBb( indf2, &bb2, status );
               if( bb1 != bb2 ) {
                  similar = Report( reports, itest, "The Bad Bits value is %d "
                                    "in IN1 but %d in IN2.", status, bb1,
                                    bb2 );
               }
            }

/* Compare quality names. */
/* ---------------------- */
         } else if( itest == 15 ) {

/* Get KeyMaps holding the quality names in the two NDFs. */
            km1 = GetQualNames( indf1, status );
            km2 = GetQualNames( indf2, status );

/* Loop round each quality name in IN1. */
            nkey = astMapSize( km1 );
            for( ikey = 0; ikey < nkey; ikey++ ) {
               key = astMapKey( km1, ikey );

/* If this quality name also exists in IN2, remove it from the km2
   KeyMap. */
               if( astMapHasKey( km2, key ) ) {
                  astMapRemove( km2, key );

/* If this quality name does not exists in IN2, report it. */
               } else {
                  similar = Report( reports, itest, "The Quality name '%s' is "
                                    "defined in IN1 but not in IN2.", status,
                                    key );
               }
            }

/* Make reports for any Quality names that are in IN2 but not in IN1. */
            nkey = astMapSize( km2 );
            for( ikey = 0; ikey < nkey; ikey++ ) {
               key = astMapKey( km2, ikey );
               similar = Report( reports, itest, "The Quality name '%s' is "
                                 "defined in IN2 but not in IN1.", status,
                                 key );
            }

/* Compare the list of root ancestors used in the creation of the two NDFs. */
/* ------------------------------------------------------------------------ */
         } else if( itest == 16 ) {

/* Get KeyMaps holding the paths to the root ancestors used in the
   creation of the two NDFs. */
            km1 = GetRootAncestors( indf1, status );
            km2 = GetRootAncestors( indf2, status );

/* Loop round each root ancestor in IN1. */
            nkey = astMapSize( km1 );
            for( ikey = 0; ikey < nkey; ikey++ ) {
               key = astMapKey( km1, ikey );

/* If this root ancestor also exists in IN2, remove it from the km2
   KeyMap. */
               if( astMapHasKey( km2, key ) ) {
                  astMapRemove( km2, key );

/* If this root ancestor does not exists in IN2, report it. */
               } else {
                  similar = Report( reports, itest, "The NDF '%s' was "
                                    "used in the creation of IN1 but not "
                                    "of IN2.", status, key );
               }
            }

/* Make reports for any root ancestors that are in IN2 but not in IN1. */
            nkey = astMapSize( km2 );
            for( ikey = 0; ikey < nkey; ikey++ ) {
               key = astMapKey( km2, ikey );
               similar = Report( reports, itest, "The NDF '%s' was used "
                                 "in the creation of IN2 but not of IN1.",
                                 status, key );
            }

/* Report an error if the test has not yet been implemented. */
         } else {
            *status = SAI__ERROR;
            errRepf( "", "Test '%d' is not yet implemented - programming "
                     "error.", status, itest + 1 );
         }
      }
   }

/* Write out the similarity output parameter value. */
   parPut0l( "SIMILAR", similar, status );

/* Create a report file if required. */
   if( !similar && *status == SAI__OK ) {
      parGet0c( "REPORT", report, sizeof(report), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );

      } else if( *status == SAI__OK ) {
         fd = fopen( report, "w" );
         if( !fd ) {
            *status = SAI__ERROR;
            errRepf( "", "Failed to create report file (%s): %s.", status,
                     report, strerror(errno) );
         } else {

            ndfMsg( "IN1", indf1 );
            msgLoad( "", "^IN1", path1, sizeof(path1), &oplen, status );
            fprintf( fd, "\nIN1 is %s\n", path1 );

            ndfMsg( "IN2", indf2 );
            msgLoad( "", "^IN2", path2, sizeof(path2), &oplen, status );
            fprintf( fd, "IN2 is %s\n\n", path2 );

            nkey = astMapSize( reports );
            for( ikey = 0; ikey < nkey; ikey++ ) {
               key = astMapKey( reports, ikey );
               astMapGet0C( reports, key, &ctemp );
               fprintf( fd, "%s\n", ctemp );
            }

            fclose( fd );
         }
      }

   } else {
      msgOut( "", "   No differences were found between the two NDFs.",
              status );
   }

/* End the NDF and AST contexts. */
   astEnd;
   ndfEnd( status );

/* If an error has occurred, issue another error report identifying the
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "NDFCOMPARE_ERR", "NDFCOMPARE: Failed to compare two NDFs.",
              status );
   }
}





/* ------------------------------------------------------------------- */

/* Private functions to print a report line to standard output, and also
   store it in a KeyMap. */

static int Report( AstKeyMap *km, int itest, const char *format,
                   int *status, ... ) {

/* Local Variables: */
   char key[ 15 ];
   char text[ 1000 ];
   va_list args;
   static int prevtest = -1;

/* Check inherited status. */
   if( *status != SAI__OK ) return 0;

/* If a different test has been started, put a header item into the
   KeyMap, and print it to standard output. */
   if( itest != prevtest ) {
      sprintf( key, "%d", astMapSize(km) + 1 );
      sprintf( text, "\nTest %d:", itest + 1 );
      astMapPut0C( km, key, text, NULL );
      msgOutf( "", "%s", status, text );
      prevtest = itest;
   }

/* Format the text. */
   va_start( args, status );
   vsnprintf( text, sizeof(text), format, args );
   va_start( args, status );

/* Print it to standard output. */
   msgOutf( "", "   %s", status, text );

/* Store in the KeyMap. */
   sprintf( key, "%d", astMapSize(km) + 1 );
   astMapPut0C( km, key, text, NULL );

/* Always return zero. */
   return 0;
}





/* ------------------------------------------------------------------- */

/* Compare two character attributes of a Frame. */

static void CompareFrameAttr( AstKeyMap *reports, int itest, int ifrm,
                              AstFrame *frm1, AstFrame *frm2,
                              const char *attr, int *ok, int *status ){

/* Local Variables: */
   const char *cval1;
   const char *cval2;

/* Check the inherited status. Return immediately if a previous check
   failed. */
   if( *status != SAI__OK || !ok ) return;

/* Get the two attribute values. */
   cval1 = astGetC( frm1, attr );
   cval2 = astGetC( frm2, attr );

/* If they are both defined, but different, make a report. */
   if( cval1 && cval2 ){
      if( strcmp( cval1, cval2 ) ){
         *ok = Report( reports, itest, "WCS Frame %d has %s='%s' in IN1 but "
                      "%s='%s' in IN2", status, ifrm, attr, cval1, attr, cval2 );
      }

/* If only the first is defined, make a report. */
   } else if ( cval1 ) {
      *ok = Report( reports, itest, "WCS Frame %d has %s='%s' in IN1 but is "
                   "undefined in IN2", status, ifrm, attr, cval1 );

/* If only the second is defined, make a report. */
   } else if ( cval2 ) {
      *ok = Report( reports, itest, "WCS Frame %d has %s='%s' in IN2 but is "
                   "undefined in IN1", status, ifrm, attr, cval2 );
   }

}





/* ------------------------------------------------------------------- */

/* Get a string from the environment, and extract a numerical value
   and qualifying character (V, R or A) from it. */

static void GetValueAndQual( const char *param, const char *quals,
                             char defqual, double *value, char *qual,
                             int *status ){

/* Local Variables: */
   char cbuf[200];
   int nc;
   int ntry;

/* Initialise returned values. */
   *value = 0.0;
   *qual = ' ';

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Loop until good values are obtained. */
   ntry = 0;
   while( *status == SAI__OK && ++ntry < 10 ) {

/* Get a string from the environment. */
      *value = 0.0;
      *qual = ' ';
      parGet0c( param, cbuf, sizeof(cbuf), status );

/* Attempt to extract a numerical value from the start of the string. */
      if( astSscanf( cbuf, " %lg %n", value, &nc ) == 1 ) {

/* If nothing else followed the numerical value, use the default qualifier. */
         if( nc >= strlen( cbuf ) ) {
            *qual = defqual;
            break;
         } else {

/* Remove leading and trailing white space from the remaining string, and
   convert to upper case. */
            astRemoveLeadingBlanks( cbuf + nc );
            astChrTrunc( cbuf + nc );
            astChrCase( NULL, cbuf + nc, 1, 0 );

/* Check the remaining string consists of a single character. */
            if( strlen(  cbuf + nc ) == 1 ) {

/* Check the character is contained in the list of allowed characters. If
   so, store it in the returned variable. */
               if( strchr( quals, cbuf[nc] ) ) *qual = cbuf[nc];
            }

/* If the qualifier was illegal, report an error, flush it, cancel the
   parameter abd try again. */
            if( *qual == ' ' && *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( "", "Bad string '%s' obtained for parameter %s - "
                        "the qualifier must be aingle character chosen "
                        "from '%s'.", status, cbuf, param, quals );
               errFlush( status );
               parCancl( param, status );

/* Leave the loop when we have a good qualifier or an error occurs. */
            } else {
               break;
            }
         }

/* If the string did not start with a numerical value, report an error,
   flush it, cancel the parameter abd try again. */
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( "", "Bad string '%s' obtained for parameter %s - "
                  "the string must start with a legal numerical value.",
                  status, cbuf, param );
         errFlush( status );
         parCancl( param, status );
      }
   }

   if( ntry == 10 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "Failed to get a value for paramater %s after 10 "
               "attempts.", status, param );
   }

}





/* ------------------------------------------------------------------- */


/* Define a macro that expands to the code needed to count the bad values
   using a specified data type. */
#define COUNT_BAD(Type,BadVal) { \
   const Type *ptr = ip; \
   for( i = 0; i < el; i++ ) { \
      if( *(ptr++) == BadVal ) result++; \
   } \
}

/* Find the number of bad values in an array component of an NDF. */

static int FindNBad( int indf, const char *comp, int *status ){

/* Local Variables: */
   char dtype[ NDF__SZFTP + 1];
   char type[ NDF__SZTYP + 1];
   int el;
   int i;
   int result;
   void *ip;

/* Initialise */
   result = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return 0;

/* Map the requested component using an appropriate data type. */
   ndfMtype( "_INTEGER,_REAL,_DOUBLE", indf, indf, comp, type,
             sizeof(type), dtype, sizeof(dtype), status );
   ndfMap( indf, comp, type, "READ", &ip, &el, status );

/* Count the bad values. */
   if( !strcmp( type, "_INTEGER" ) ) {
      COUNT_BAD(int,VAL__BADI)
   } else if( !strcmp( type, "_REAL" ) ) {
      COUNT_BAD(float,VAL__BADR)
   } else if( !strcmp( type, "_DOUBLE" ) ) {
      COUNT_BAD(double,VAL__BADD)
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "Unsupported data type '%s' - programming error in "
               "FindNBad.", status, type );
   }

/* Unmap the array. */
   ndfUnmap( indf, comp, status );

/* Return the result. */
   return result;
}

#undef COUNT_BAD




/* ------------------------------------------------------------------- */

/* Return a KeyMap containing the QUality Names found in an NDF. */

static AstKeyMap *GetQualNames( int indf, int *status ){

/* Local Variables; */
   AstKeyMap *result;
   IRQLocs *qlocs;
   IRQcntxt contxt = 0;
   char commnt[IRQ__SZCOM+1];
   char qname[IRQ__SZQNM+1];
   char xname[DAT__SZNAM+1];
   int bit;
   int done;
   int fixed;
   int value;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

/* Create the returned KeyMap. */
   result = astKeyMap( " " );

/* Find the NDF extension containing the quality names (if any), and get
   locators for the information in the extension. */
   irqFind( indf, &qlocs, xname, status );

/* Annul the error if no quality name info was found. */
   if( *status == IRQ__NOQNI ) {
      errAnnul( status );

/* Otherwise, loop round each quality name. */
   } else {
      done = 0;
      while( !done && *status == SAI__OK ){

/* Get the quality name (and other info). */
         irqNxtqn( qlocs, &contxt, qname, &fixed, &value, &bit,
                   commnt, sizeof(commnt), &done, status );

/* Put the quality name in the KeyMap. */
         astMapPut0I( result, qname, 1, NULL );
      }

/* Free quality */
      irqRlse( &qlocs, status );
   }

/* Return the KeyMap. */
   return result;
}





/* ------------------------------------------------------------------- */

/* Return a KeyMap containing the root ancestors of an NDF. */

static AstKeyMap *GetRootAncestors( int indf, int *status ){

/* Local Variables; */
   AstKeyMap *result;
   NdgProvenance *prov;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

/* Read provenance info from the NDF. */
   prov = ndgReadProv( indf, "KAPPA:NDFCOMPARE", status );

/* Get a KeyMap holding the root ancestors. */
   result = ndgRootProv( prov, status );

/* Free the prevenance structure. */
   prov = ndgFreeProv( prov, status );

/* Return the KeyMap. */
   return result;
}





/* ------------------------------------------------------------------- */

/* Return the number of pixels that have different Quality values in the
   two NDFs. "*nel" is returned holding the total number of pixels in the
   second NDF. */

static int CountQualityDiff( int indf1, int indf2, int *nel, int *status ){

/* Local Variables: */
   int result;
   const unsigned char *ip1;
   const unsigned char *ip2;
   int i;
   int indf1s;
   int indf2s;

/* Initialise */
   result = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Begin an NDF context so we do not need to worry about unmapping and
   annulling NDFs created in this function. */
   ndfBegin();

/* Ensure we are dealing with NDFs that have equal bounds. */
   ndfClone( indf1, &indf1s, status );
   ndfClone( indf2, &indf2s, status );
   ndfMbnd( "TRIM", &indf1s, &indf2s, status );

/* If the two NDFs have no overlap, annull the error and return VAL__MAXI
   to indicate this. */
   if( *status == NDF__NOTRM ) {
      errAnnul( status );
      result = VAL__MAXI;

/* Map the Quality component. */
   } else {
      ndfMap( indf1s, "QUALITY", "_UBYTE", "READ", (void **) &ip1, nel, status );
      ndfMap( indf2s, "QUALITY", "_UBYTE", "READ", (void **) &ip2, nel, status );

/* Loop round counting the pixels with different quality values. */
      for( i = 0; i < *nel; i++ ) {
         if( *(ip1++) != *(ip2++) ) result++;
      }
   }

/* End the NDF context. */
   ndfEnd( status );

/* Return the count. */
   return result;
}





/* ------------------------------------------------------------------- */

/* Define a macro that expands to the code needed to find the maximum
   difference using the "A" qualifier (absolute error), for a specified
   data type. */
#define MAXERROR_A(Type,BadVal) { \
   const Type *p1 = ip1; \
   const Type *p2 = ip2; \
   for( i = 0; i < el; i++ ) { \
      if( *p1 != BadVal && *p2 != BadVal ) { \
         diff = fabs( *p1 - *p2 ); \
         if( diff > result ) result = diff; \
      } \
   } \
}

/* Define a macro that expands to the code needed to find the maximum
   difference using the "R" qualifier (relative error), for a specified
   data type. */
#define MAXERROR_R(Type,BadVal) { \
   const Type *p2 = ip2; \
   const Type *p1 = ip1; \
   double mean; \
   double rms1; \
   double rms2; \
   double rms; \
   int ngood; \
\
/* First need to get the RMS value in both arrays. */ \
   rms1 = 0.0;  \
   ngood = 0;  \
   for( i = 0; i < el; i++,p1++ ) { \
      if( *p1 != BadVal ) { \
         rms1 += (*p1)*(*p1); \
         ngood++; \
      } \
   } \
   rms1 = ( ngood > 0 ) ? sqrt( rms1/ngood ): VAL__MAXD; \
\
   rms2 = 0.0; \
   ngood = 0; \
   for( i = 0; i < el; i++,p2++ ) { \
      if( *p2 != BadVal ) { \
         rms2 += (*p2)*(*p2); \
         ngood++; \
      } \
   } \
   rms2 = ( ngood > 0 ) ? sqrt( rms2/ngood ): VAL__MAXD; \
\
/* Choose the lower RMS. */ \
   rms = ( rms1 < rms2 ) ? rms1 : rms2; \
\
/* Now loop round looking at the relative difference between \
   corresponding pairs of pixels. */ \
   p1 = ip1; \
   p2 = ip2; \
   for( i = 0; i < el; i++,p1++,p2++ ) { \
      if( *p1 != BadVal && *p2 != BadVal ) { \
\
/* If the absolute mean value is less than the RMS we use the RMS instead \
   when forming the relative error. */ \
         mean = 0.5*fabs( *p1 + *p2 ); \
         if( mean < rms ) mean = rms; \
\
/* Form the relative error and update the maximum relative error found so \
   far. */ \
         if( mean > 0.0 ) { \
            diff = fabs( *p1 - *p2 )/mean; \
            if( diff > result ) result = diff; \
         } \
      } \
   } \
}

/* Define a macro that expands to the code needed to find the maximum
   difference using the "V" qualifier (standard deviations), for a specified
   data type. */
#define MAXERROR_V(Type,BadVal) { \
   const Type *p2 = ip2; \
   const Type *p1 = ip1; \
   const Type *pv2 = ipv2; \
   const Type *pv1 = ipv1; \
   double sigma; \
\
/* Loop round looking at the standard deviation between \
   corresponding pairs of pixels. */ \
   for( i = 0; i < el; i++,p1++,p2++,pv1++,pv2++ ) { \
      if( *p1 != BadVal && *p2 != BadVal && \
          *pv1 != BadVal && *pv2 != BadVal && \
          *pv1 > 0.0 && *pv2 > 0.0 ) { \
\
/* Find the mimimum variance, and convert to standard devioation. */ \
         sigma = sqrt( ( (*pv1) < (*pv2) ) ? *pv1 : *pv2 ); \
\
/* Form the number of standard deviations that separate the two data \
   values, and update the maximum relative error found so far. */ \
         if( sigma > 0.0 ) { \
            diff = fabs( *p1 - *p2 )/sigma; \
            if( diff > result ) result = diff; \
         } \
      } \
   } \
}


/* Find the largest difference in pixel value. */

static double FindMaxPixelDiff( int indf1, int indf2, const char *comp,
                                char qual, int *status ){

/* Local Variables: */
   char dtype[ NDF__SZFTP + 1];
   char type[ NDF__SZTYP + 1];
   double diff;
   double result;
   int el;
   int i;
   int indf1s;
   int indf2s;
   void *ip1;
   void *ip2;
   void *ipv1;
   void *ipv2;

/* Initialise */
   result = 0.0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Begin an NDF context so we do not need to worry about unmapping and
   annulling NDFs created in this function. */
   ndfBegin();

/* Ensure we are dealing with NDFs that have equal bounds. */
   ndfClone( indf1, &indf1s, status );
   ndfClone( indf2, &indf2s, status );
   ndfMbnd( "TRIM", &indf1s, &indf2s, status );

/* If the two NDFs have no overlap, annull the error and return VAL__MAXD
   to indicate this. */
   if( *status == NDF__NOTRM ) {
      errAnnul( status );
      result = VAL__MAXD;

/* Map the requested component using an appropriate data type. */
   } else {
      ndfMtype( "_INTEGER,_REAL,_DOUBLE", indf1s, indf2s, comp, type,
                sizeof(type), dtype, sizeof(dtype), status );
      ndfMap( indf1s, comp, type, "READ", &ip1, &el, status );
      ndfMap( indf2s, comp, type, "READ", &ip2, &el, status );

/* First handle cases where the accuracy is specified as a
   signal-to-noise value. */
      if( qual == 'V' ) {

/* We need to map the Variance arrays as well. */
         ndfMap( indf1s, "VARIANCE", type, "READ", &ipv1, &el, status );
         ndfMap( indf2s, "VARIANCE", type, "READ", &ipv2, &el, status );

         if( !strcmp( type, "_INTEGER" ) ) {
            MAXERROR_V(int,VAL__BADI)
         } else if( !strcmp( type, "_REAL" ) ) {
            MAXERROR_V(float,VAL__BADR)
         } else if( !strcmp( type, "_DOUBLE" ) ) {
            MAXERROR_V(double,VAL__BADD)
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "Unsupported data type '%s' - programming error in "
                     "FindMaxPixelDiff.", status, type );
         }

/* Now handle cases where the accuracy is specified as a relative error. */
      } else if( qual == 'R' ) {
         if( !strcmp( type, "_INTEGER" ) ) {
            MAXERROR_R(int,VAL__BADI)
         } else if( !strcmp( type, "_REAL" ) ) {
            MAXERROR_R(float,VAL__BADR)
         } else if( !strcmp( type, "_DOUBLE" ) ) {
            MAXERROR_R(double,VAL__BADD)
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "Unsupported data type '%s' - programming error in "
                     "FindMaxPixelDiff.", status, type );
         }

/* First handle cases where the accuracy is specified as an absolute
   error. */
      } else if( qual == 'A' ) {
         if( !strcmp( type, "_INTEGER" ) ) {
            MAXERROR_A(int,VAL__BADI)
         } else if( !strcmp( type, "_REAL" ) ) {
            MAXERROR_A(float,VAL__BADR)
         } else if( !strcmp( type, "_DOUBLE" ) ) {
            MAXERROR_A(double,VAL__BADD)
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "Unsupported data type '%s' - programming error in "
                     "FindMaxPixelDiff.", status, type );
         }

/* Report an error for any other type of qualifier. */
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( "", "Bad qualifier (%c) supplied - programming error in "
                  "FindMaxPixelDiff.", status, qual );
      }
   }

/* End the NDF context. */
   ndfEnd( status );

/* Return the maximum difference. */
   return result;
}

#undef MAXERROR_A
#undef MAXERROR_R
#undef MAXERROR_V





