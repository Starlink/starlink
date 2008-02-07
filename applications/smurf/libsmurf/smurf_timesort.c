/*
*+
*  Name:
*     TIMESORT

*  Purpose:
*     Re-order the time slices in a raw data cube into increasing time.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_timesort( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine accepts as input one or more raw data cubes, spanned
*     by (frequency, detector number, time) axes. For each one, it sorts 
*     the time slices into monotonically increasing time value and writes 
*     the resulting cube to a new output NDF. The ACSIS and JCMTSTATE 
*     extensions and the WCS component are modified along with the main
*     Data array, so that the resulting cube remains internally consistent.
*
*     The main reason for using this routine is to ensure that data has a
*     defined transformation from WCS coordinates to pixel coordinates.

*  ADAM Parameters:
*     IN = NDF (Read)
*          A group of input NDFs, each holding raw time series data.
*     OUT = NDF (Write)
*          A group of output NDFs.

*  Authors:
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     7-NOV-2007 (DSB):
*        Original version.
*     7-FEB-2008 (DSB):
*        Store provenance info in the output NDFs.

*  Copyright:
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"


/* SMURF includes */
#include "libsmf/smf.h"

/* Number of ACSIS extension components that need to be re-ordered. */
#define NACSIS 3

void smurf_timesort( int *status ) {

/* Local Variables */
   AstFrame *cfrm = NULL;     /* Current Frame from output WCS */
   AstFrameSet *wcs = NULL;   /* WCS Frameset from output NDF */
   AstLutMap *lut = NULL;     /* LutMap holding ordered time values */
   AstMapping *map = NULL;    /* Base->current Mapping from output WCS */
   AstMapping *omap = NULL;   /* Other axes Mapping */
   AstMapping *tmap = NULL;   /* Time axis Mapping */
   Grp *igrp1 = NULL;         /* Group of input files */
   Grp *igrp2 = NULL;         /* Group of output files */
   HDSLoc *loc1 = NULL;       /* HDS locator for input extension */
   HDSLoc *loc1c = NULL;      /* HDS locator for input extension component */
   HDSLoc *loc2 = NULL;       /* HDS locator for output extension */
   HDSLoc *loc2c = NULL;      /* HDS locator for output extension component */
   char name[ DAT__SZNAM + 1 ]; /* HDS component name */
   char type[ DAT__SZTYP + 1 ]; /* HDS data type */
   double *grid = NULL;       /* Array of GRID values for the time axis */
   double *tai = NULL;        /* Array of TAI values for the time axis */
   double *tai_ptr = NULL;    /* Pointer to mapped TCS_TAI array */
   int *index = NULL;         /* Pointer to time index array */
   int axes[ 2 ];             /* Input axes to be selected */
   int dims[ 3 ];             /* Array dimensions */
   int el;                    /* Number of elements mapped */
   int i;                     /* Loop count */
   int ifile;                 /* Index of current input file */
   int indf1;                 /* NDF identifier for input NDF */
   int indf2;                 /* NDF identifier for output NDF */
   size_t len;                   /* The length of an HDS character type */
   int ncomp;                 /* Number of components in extension*/
   int ndim;                  /* Number of array dimensions */
   int nout[ NDF__MXDIM ];    /* Outputs fed by the selected inputs */ 
   size_t ntai;               /* The length of the TCS_TAI array */
   int outsize;               /* Number of output NDFs */
   int size;                  /* Number of input NDFs */
   int sorted;                /* Was the TCS_TAI array already sorted? */
   int there;                 /* Does object exist? */
   void *ipin;                /* Pointer to input array */
   void *ipout;               /* Pointer to output array */

/* NDF array component names */
   static char *comp[2] = {"DATA", "VARIANCE"}; 

/* ACSIS arrays to be re-ordered */
   static char *acsis[NACSIS] = {"RECEPPOS", "TSYS", "TRX" };

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an NDF context (we do not begin an AST context since this is
   done within the calling monolith routine). */
   ndfBegin();

/* Get a group of input files */ 
   kpg1Rgndf( "IN", 0, 1, "  Give more NDFs...", &igrp1, &size, status );

/* Get a group of exactly "size" names for the output NDFs.  Base 
   modification elements on the group containing the input NDFs. */
   kpg1Wgndf( "OUT", igrp1, size, size, "  Give more NDFs...",
              &igrp2, &outsize, status );

/* Loop round each input NDF. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Tell the user which NDF is being processed. */
      msgSeti( "I", ifile );
      msgSeti( "N", size );
      msgOutif( MSG__VERB, "", "Processing NDF  ^I of ^N...", status );

/* Begin AST and NDF contexts for this pair of input and output NDFs. */
      astBegin;
      ndfBegin();

/* Get an NDF identifier for the input NDF. */
      ndgNdfas( igrp1, ifile, "READ", &indf1, status );

/* Get a locator to the JCMTSTATE extension. This will report an error if
   the extension does not exist. */
      ndfXloc( indf1, "JCMTSTATE", "READ", &loc1, status );

/* Map the TCS_TAI array in the JCMTSTATE extension. */
      datFind( loc1, "TCS_TAI", &loc1c, status );
      datMapV( loc1c, "_DOUBLE", "READ", (void **) &tai_ptr, &ntai, status );

/* Obtain a sorted index for the TCS_TAI values in the JCMTSTATE
   extension. */
      index = smf_sortd( ntai, tai_ptr, &sorted, status );

/* Free the TCS_TAI array now so that we can re-access the array later
   when we come to re-order each array in the JCMTSTATE extension. */
      datUnmap( loc1c, status );
      datAnnul( &loc1c, status );

/* Create the output NDF from the input NDF. If the input NDF TCS_TAI
   values are already sorted, propagate everything and pass on to the the
   next input NDF. Otherwise propagate everything except the data arrays
   and then go on to copy the re-ordered arrays into the output. */
      if( sorted ) {
         ndgNdfpr( indf1, "Data,Variance,Quality,Units,Axis,WCS,"
                   "NoExtension(Provenance)", igrp2, ifile, &indf2, status );
      } else {
         ndgNdfpr( indf1, "Units,Axis,WCS,NoExtension(Provenance)", igrp2, 
                   ifile, &indf2, status );

/* Get the pixel dimensions of the input NDF. Report an error if not
   three dimensional. */
         ndfDim( indf1, 3, dims, &ndim, status ); 
         if( ndim != 3 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "NDF", indf1 );
            errRep( "", "Input NDF ^NDF is not 3 dimensional.", status );
         }        

/* Re-order the Data and Variance arrays (if they exist). */
         for( i = 0; i < 2 && *status == SAI__OK; i++ ) {
            ndfState( indf1, comp[ i ], &there, status ); 
            if( there ) {
               ndfMap( indf1, comp[ i ], "_REAL", "READ", &ipin, &el, status );
               ndfMap( indf2, comp[ i ], "_REAL", "WRITE", &ipout, &el, status );
               smf_reorderr( (float *) ipin, ndim, dims, 2, index,
                             (float *) ipout, status );
            }
         }

/* Re-order every component of the JCMTSTATE extension. It is assumed
   that each component is an array in which the last axis corresponds to
   the time axis. First get a locator for the output JCMTSTATE extension. */
         ndfXloc( indf2, "JCMTSTATE", "UPDATE", &loc2, status );

/* Find out how many components there are in the JCMTSTATE extension, and
   then loop round them all, using a one-based component index. */
         datNcomp( loc1, &ncomp, status );
         for( i = 1; i <= ncomp && *status == SAI__OK; i++ ) {

/* Get locators for the current JCMTSTATE component in both input and
   output NDF. */
            datIndex( loc1, i, &loc1c, status );
            datIndex( loc2, i, &loc2c, status );

/* Determine the shape and type of the array component. */
            datShape( loc1c, 3, dims, &ndim, status );
            datType( loc1c, type, status );

/* Pass on if the component is scalar, or if the last dimension is not of
   the same length as the TCS_TAI array. */
            if( ndim > 0 && dims[ ndim - 1 ] == ntai ) {

/* Map the input and output arrays. */
               datMap( loc1c, type, "READ", ndim, dims, &ipin, status );
               datMap( loc2c, type, "WRITE", ndim, dims, &ipout, status );

/* Re-order the array. */
               if( !strcmp( type, "_REAL" ) ) {
                  smf_reorderr( (float *) ipin, ndim, dims, ndim - 1, index,
                                (float *) ipout, status );

               } else if( !strcmp( type, "_DOUBLE" ) ) {
                  smf_reorderd( (double *) ipin, ndim, dims, ndim - 1, index,
                                (double *) ipout, status );

               } else if( !strcmp( type, "_INTEGER" ) ) {
                  smf_reorderi( (int *) ipin, ndim, dims, ndim - 1, index,
                                (int *) ipout, status );

               } else if( !strncmp( type, "_CHAR", 5 ) ) {
                  datLen( loc1c, &len, status );
                  smf_reorderc( (char *) ipin, len, ndim, dims, ndim - 1, 
                                index, (char *) ipout, status );

               } else if( *status == SAI__OK ) {
                  datName( loc1c, name, status );
                  *status = SAI__ERROR;
                  msgSetc( "COMP", name );
                  msgSetc( "TYPE", type );
                  errRep( "", "TIMESORT: Cannot re-order JCMTSTATE.^COMP."
                              "^TYPE data type not yet supported.", status );
               }

/* Unmap the mapped arrays. */
               datUnmap( loc1c, status );
               datUnmap( loc2c, status );
            }

/* Annul the component locators in the input and output NDFs. */
            datAnnul( &loc1c, status );
            datAnnul( &loc2c, status );
         }

/* Annul the JCMTSTATE locator for the output NDF. */
         datAnnul( &loc2, status );
      }

/* Annul the JCMTSTATE locator for the input NDF. */
      datAnnul( &loc1, status );

/* Pass on if the input NDF does not have an ACSIS extension. */
      ndfXstat( indf1, "ACSIS", &there, status );     
      if( there ) {

/* We now re-order selected components of the ACSIS extension. First get a
   locator for the extension in both input and output NDFs. */
         ndfXloc( indf1, "ACSIS", "READ", &loc1, status );
         ndfXloc( indf2, "ACSIS", "UPDATE", &loc2, status );

/* Loop round each component to be re-ordered. */
         for( i = 0; i < NACSIS && *status == SAI__OK; i++ ) {

/* Get locators for the current ACSIS component in both input and
   output NDF. */
            datFind( loc1, acsis[ i ], &loc1c, status );
            datFind( loc2, acsis[ i ], &loc2c, status );

/* Determine the shape and type of the array component. */
            datShape( loc1c, 3, dims, &ndim, status );
            datType( loc1c, type, status );

/* Map the input and output arrays. */
            datMap( loc1c, type, "READ", ndim, dims, &ipin, status );
            datMap( loc2c, type, "WRITE", ndim, dims, &ipout, status );

/* Re-order the array. */
            if( !strcmp( type, "_REAL" ) ) {
               smf_reorderr( (float *) ipin, ndim, dims, ndim - 1, index,
                             (float *) ipout, status );

            } else if( !strcmp( type, "_DOUBLE" ) ) {
               smf_reorderd( (double *) ipin, ndim, dims, ndim - 1, index,
                             (double *) ipout, status );

            } else if( !strcmp( type, "_INTEGER" ) ) {
               smf_reorderi( (int *) ipin, ndim, dims, ndim - 1, index,
                             (int *) ipout, status );

            } else if( !strncmp( type, "_CHAR", 5 ) ) {
               datLen( loc1c, &len, status );
               smf_reorderc( (char *) ipin, len, ndim, dims, ndim - 1, 
                             index, (char *) ipout, status );

            } else if( *status == SAI__OK ) {
               datName( loc1, name, status );
               *status = SAI__ERROR;
               msgSetc( "COMP", name );
               msgSetc( "TYPE", type );
               errRep( "", "TIMESORT: Cannot re-order ACSIS.^COMP."
                           "^TYPE data type not yet supported.", status );
            }

/* Unmap the mapped arrays. */
            datUnmap( loc1c, status );
            datUnmap( loc2c, status );

/* Annul the component locators in the input and output NDFs. */
            datAnnul( &loc1c, status );
            datAnnul( &loc2c, status );
         }

/* Annul the ACSIS locator for the output NDF. */
         datAnnul( &loc2, status );
      }

/* Now modify the WCS in the output NDF. First get the existing WCS
   FrameSet from the output NDF. */
      ndfGtwcs( indf2, &wcs, status );

/* Get pointers to the current Frame, and the GRID->WCS Mapping. */
      cfrm = astGetFrame( wcs, AST__CURRENT );
      map = astGetMapping( wcs, AST__BASE, AST__CURRENT );

/* Split off the Mapping for the third (time) axis. */
      axes[ 0 ] = 3;
      astMapSplit( map, 1, axes, nout, &tmap );
      if( tmap && astGetI( tmap, "Nout" ) == 1 ) {

/* Get a table of time values for every grid index, in order of increasing 
   time value. */
         grid = astMalloc( sizeof( double )*ntai );
         tai = astMalloc( sizeof( double )*ntai );
         for( i = 0; i < ntai; i++ ) grid[ i ] = index[ i ] + 1.0;
         astTran1( tmap, ntai, grid, 1, tai );

/* Create a LutMap holding these sorted time values. */
         lut = astLutMap( ntai, tai, 1.0, 1.0, "" );

/* Split off a Mapping for the other two axes. */
         axes[ 0 ] = 1;
         axes[ 1 ] = 2;
         astMapSplit( map, 2, axes, nout, &omap );
         if( omap && astGetI( omap, "Nout" ) == 2 ) {

/* Put this Mapping in parallel with the time axis Mapping created above. */
            map = (AstMapping *) astCmpMap( omap, lut, 0, "" );
            
/* Remove the current Frame from the FrameSet, then add it back in again
   using the above Mapping to connect it to the GRID (base) Frame. */
            astRemoveFrame( wcs, AST__CURRENT );
            astAddFrame( wcs, AST__BASE, map, cfrm );

/* Store the modifed WCS FrameSet in the output NDF. */
            ndfPtwcs( wcs, indf2, status );

         }

/* Free resources. */
         grid = astFree( grid );
         tai = astFree( tai );

      }

/* Free remaining resources. */
      index = astFree( index );
      datAnnul( &loc1, status );

/* Record indf1 as a direct parent of indf2. */
      ndgPtprv( indf2, indf1, NULL, 0, "SMURF:TIMESORT", status );

/* End the AST and NDF contexts for this pair of input and output NDFs. */
      ndfEnd( status );
      astEnd;

/* If an error has occurred processing this input NDF, flush it and
   proceed with thenext NDF. */
      if( *status != SAI__OK ) {
         msgSeti( "I", ifile );
         errRep( "", "TIMESORT: failed to process input NDF number ^I.", 
                 status );
         errFlush( status );
      }
   }

/* Free resources. */
   grpDelet( &igrp1, status );
   grpDelet( &igrp2, status );

/* End the NDF context. */
   ndfEnd( status );

/* Issue a status indication.*/  
   if( *status == SAI__OK ) {
      msgOutif( MSG__VERB, "", "TIMESORT succeeded.", status);
   } else {
      msgOutif( MSG__VERB, "", "TIMESORT failed.", status);
   }
}
