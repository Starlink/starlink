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
*
*     There are two main modes, selected by parameter MERGE. If MERGE 
*     is FALSE, then the time slices in each input NDF are sorted
*     independently of the other NDFs, and each output NDF contains 
*     data only from the corresponding input NDF. If MERGE is TRUE, then 
*     the time slices in all input NDFs are sorted into a single list.
*     This list is then divided up into chunks (in a manner selected by
*     parameter SIZELIMIT), and the time slices are written out
*     sequentially to a number of output NDFs. If any time slice is
*     present in more than one input NDF, then the data values are for
*     the two or more input time slices are merged into a single time 
*     slice.
*
*     MERGE = TRUE should be used to sort the time slices contained in a
*     set of sub-scans from a single sub-system of an observation.

*  ADAM Parameters:
*     IN = NDF (Read)
*          A group of input NDFs, each holding raw time series data.
*     LIMITTYPE = LITERAL (Read)
*          Only accessed if parameter MERGE is set TRUE and a positive
*          value is supplied for SIZELIMIT. Specifies the units of the 
*          SIZELIMIT value. It must be one of:
*
*          - "SPECTRA": SIZELIMIT is the maximum number of spectra in each 
*          output NDF.
*          - "SLICES": SIZELIMIT is the maximum number of time slices in 
*          each output NDF.
*          - "FILESIZE": SIZELIMIT is the maximum number of megabytes of 
*          data in each output NDF.
*
*          Note, when using the FILESIZE option the specified file size
*          only includes the size of the Data and Variance components in
*          the NDF. Consequently, the actual file size may be a little
*          larger than the requested size because of the extra
*          information held in NDF extensions. ["FILESIZE"]
*     MERGE = _LOGICAL (Read)
*          If FALSE, then each input NDF is sorted independently of the
*          other input NDFs, and the sorted data is written to a separate 
*          output NDF. If TRUE, then the time slices for all the input NDFs 
*          are read into a single list, which is then sorted. The sorted
*          data can be written out to a single large output file or can be
*          split up into several smaller output files, as specified by
*          the SIZELIMIT parameter. In this mode, all input NDFs must
*          come from the same observation (as indicated by the OBSIDSS
*          value in the FITS extension). An error is reported if this is
*          not the case. [FALSE]
*     NOUT = _INTEGER (Write)
*          An output parameter in which is stored the number of output NDFs 
*          created.
*     OUT = NDF (Write)
*          A group of output NDFs. If parameter MERGE is FALSE, then a
*          separate output NDF is created for each input NDF and so the
*          size of the supplied group should equal the number of input NDF.
*          If parameter MERGE is TRUE, then the number of output NDFs is
*          determined by the SIZELIMIT parameter. In this case, the
*          supplied group should contain only a single value, and the
*          name of each output NDF will be formed by appending "_1",
*          "_2", etc, to the end of the supplied base name.
*     SIZELIMIT = _INTEGER (Read)
*          Only accessed if parameter MERGE is set TRUE. It is a number that 
*          specifies the maximum size of each output NDF when merging data
*          from several input NDFs (see parameter MERGE). The minimum
*          number of output NDFs needed to hold all the input data will be
*          used. The final output NDF may be smaller than the specified
*          maximum size. The value given is either the file size in
*          megabytes, the number of time slices, or the number of spectra,
*          as specified by parameter LIMITTYPE. If a null (!) value is
*          supplied, then the number of output NDFs will be the same as
*          the number of input NDFs, and all output NDFs will have the
*          same size. If a negative or zero value is supplied, then a single 
*          output NDF will be created holding all the input data. [!]

*  Authors:
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     7-NOV-2007 (DSB):
*        Original version.
*     7-FEB-2008 (DSB):
*        Store provenance info in the output NDFs.
*     14-MAR-2008 (DSB):
*        Added sub-scan merging facility.

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

#include <limits.h>
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"
#include "par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Number of ACSIS extension components that need to be re-ordered. */
#define NACSIS 3

void smurf_timesort( int *status ) {

/* Local Variables */
   AstFitsChan *fc;
   AstFrame *cfrm = NULL;     
   AstFrameSet *wcs = NULL;   
   AstKeyMap *km1;
   AstKeyMap *km2;
   AstLutMap *lut = NULL;     
   AstMapping *map = NULL;    
   AstMapping *omap = NULL;   
   AstMapping *tmap = NULL;   
   Grp *igrp1 = NULL;         
   Grp *igrp2 = NULL;         
   HDSLoc *loc1 = NULL;       
   HDSLoc *loc1c = NULL;      
   HDSLoc *loc2 = NULL;       
   HDSLoc *loc2c = NULL;      
   char *obsidss = NULL;
   char *obsidss0 = NULL;
   char *pname = NULL;
   char *qts_in;
   char *qts_out;
   char basename[ GRP__SZNAM + 1 ];
   char fullname[ GRP__SZNAM + 10 ];
   char ltbuf[ 11 ];                
   char name[ DAT__SZNAM + 1 ];
   char type[ DAT__SZTYP + 1 ];
   const char *comps = NULL;
   const char *dom;
   const char *timeorg;
   const char *timescl;
   const char *timesys;
   const char *timeunt;
   double *grid = NULL;       
   double *tai = NULL;        
   double *tai_ptr = NULL;    
   double *taiout = NULL;
   float *dts_in;
   float *dts_out;
   float *vts_in;
   float *vts_out;
   int *file_index = NULL;
   int *first = NULL;         
   int *index = NULL;         
   int *itimeout = NULL;
   int *ndfid = NULL;
   int *rts = NULL;
   int axes[ 2 ];             
   int dims[ 3 ];             
   int el;                    
   int hasqual;
   int hasvar;
   int i;                     
   int iel;
   int ifile;                 
   int indf1;                 
   int indf1s;
   int indf2;                 
   int indf2s;
   int init;
   int iout;
   int j;
   int k;
   int lbnd[ 3 ];
   int merge;                 
   int nchan;
   int ncomp;                 
   int ndet;
   int ndim;                  
   int nnout[ NDF__MXDIM ];     
   int nout;
   int nrem;
   int nts_in;            
   int nts_out;
   int outsize;               
   int place;
   int rts_num0;
   int rts_num;
   int rts_num_last;
   int size;                  
   int sizelimit;
   int slbnd[ 3 ];
   int sorted;                 
   int subnd[ 3 ];
   int there;                 
   int ubnd[ 3 ];
   size_t len;                
   size_t ntai;               
   void *ipin;                
   void *ipout;               
   void *ptr[2];              

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

/* See if we are sorting each input file individually, or sorting the
   merged input data. */
   parGet0l( "MERGE", &merge, status );

/* First handle cases where we are sorting individual input files. */
/* =============================================================== */
   if( !merge ) {

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
         astMapSplit( map, 1, axes, nnout, &tmap );
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
            astMapSplit( map, 2, axes, nnout, &omap );
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
   proceed with the next NDF. */
         if( *status != SAI__OK ) {
            msgSeti( "I", ifile );
            errRep( "", "TIMESORT: failed to process input NDF number ^I.", 
                    status );
            errFlush( status );
         }
      }

/* Free resources. */
      grpDelet( &igrp2, status );



/* Now handle cases where the input files are being merged. */
/* ======================================================== */
   } else {

/* Lists are created below that concatenate all the time slices from
   every input NDF (these lists are stored in AST KeyMaps, keyed by the
   name of the corresponding JCMTSTATE or ACSIS extension item). Allocate 
   an array to store the index of the first time slice within these lists 
   for each input file. */
      first = astMalloc( sizeof( int )*size );    

/* Allocate an array to store the NDF identifier for each input file. */
      ndfid = astMalloc( sizeof( int )*size );    

/* Create a KeyMap to hold JCMTSTATE data values. */
      km1 = astKeyMap( "" );

/* Create a KeyMap to hold ACSIS data values. */
      km2 = astKeyMap( "" );

/* Initialise a pointer to an array used to hold the index of the
   input NDF from which each time slice in the concatenated list of time
   slices was read. */
      file_index = NULL;

/* Initialise the total number of time slices currently in the lists of
   concatenated time slices. */
      nts_in = 0;

/* Assume for the moment that all input NDFs have Variance and QUality
   arrays. */
      hasvar = 1;
      hasqual = 1;

/* Loop round each input NDF. */
      for( ifile = 0; ifile < size && *status == SAI__OK; ifile++ ) {
   
/* Tell the user which NDF is being processed. */
         msgSeti( "I", ifile + 1 );
         msgSeti( "N", size );
         msgOutif( MSG__VERB, "", "Reading header information from NDF ^I "
                   "of ^N...", status );
   
/* Begin an AST context for this input NDF. */
         astBegin;
   
/* Get an NDF identifier for the input NDF. */
         ndgNdfas( igrp1, ifile + 1, "READ", &indf1, status );
         ndfid[ ifile ] = indf1;
   
/* Get a FitsChan holding the contents of the FITS extension. */
         kpgGtfts( indf1, &fc, status );

/* Get the value of the OBSIDSS keyword. Report an error if it was not
   found. */
         if( !astGetFitsS( fc, "OBSIDSS", &obsidss ) && 
             *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "NDF", indf1 );
            errRep( "", "The NDF '^NDF' has no OBSIDSS value in its "
                    "FITS extension.", status );
         }

/* Get the shape of the data array. */
         ndfDim( indf1, 3, dims, &ndim, status ); 
         if( ndim != 3 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "NDF", indf1 );
            errRep( "", "Input NDF ^NDF is not 3 dimensional.", status );
         }        

/* If this is the first input file, take a permanent copy of the OBSIDSS
   string, and store the number of channels and the number of detectors
   in the data. */
         if( ifile == 0 ) {
            obsidss0 = astStore( NULL, obsidss, strlen( obsidss ) + 1 );
            nchan = dims[ 0 ];
            ndet = dims[ 1 ];

/* If this is not the first input file, check that the OBSIDSS string is
   the same as that from the first input file. Report an error if not. 
   Also check the number of channels and detectors are the same as in the
   first input NDF. */
         } else if( strcmp( obsidss, obsidss0 ) && *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc( "O1", obsidss0 );
            msgSetc( "O2", obsidss );
            ndfMsg( "NDF", indf1 );
            errRep( "", "The OBSIDSS value in '^NDF' (^O2) differs from "
                    "that in the first NDF (^O1).", status );

         } else if( dims[ 0 ] != nchan && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "NDF", indf1 );
            errRep( "", "The number of spectral channels in '^NDF' differs "
                    "from the first NDF.", status );

         } else if( dims[ 1 ] != ndet && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "NDF", indf1 );
            errRep( "", "The number of receptors in '^NDF' differs "
                    "from the first NDF.", status );
         }

/* If all input NDFs read so far have a Variance array, see if this one
   does too. */
         if( hasvar ) ndfState( indf1, "Variance", &hasvar, status );

/* If all input NDFs read so far have a Quality array, see if this one
   does too. */
         if( hasqual ) ndfState( indf1, "Quality", &hasqual, status );

/* Read all the data in the JCMTSTATE extension of the current input NDF,
   and append it to entries in the appropriate KeyMap. Each entry in the
   KeyMap has a name equal to the name of a component within the JCMTSTATE
   extension. The first NDF defines the components that are expected in a
   JCMTSTATE extension. If any subsequent NDF does not have any of the
   components read form the first NDF, then an error is reported. */
         smf_ext2km( indf1, "JCMTSTATE", km1, ifile ? 2 : 1, status );

/* Similarly, record all the data in the ACSIS extension. */
         smf_ext2km( indf1, "ACSIS", km2, ifile ? 2 : 1, status );

/* Store the index within the "rts" array of the first RTS value for this 
   file. */
         first[ ifile ] = nts_in;

/* Increment the total number of time slices recorded.· */
         nts_in += dims[ 2 ];

/* Extend the "file_index" array and store the file index in every new 
   element. */
         file_index = astGrow( file_index, nts_in, sizeof( int ) );
         if( *status == SAI__OK ) {
            for( i = nts_in - dims[ 2 ]; i < nts_in; i++ ) {
               file_index[ i ] = ifile;
            }
         }

/* Get the WCS FrameSet from the input NDF. */
         ndfGtwcs( indf1, &wcs, status );

/* Get pointers to the current Frame, and the GRID->WCS Mapping. */
         cfrm = astGetFrame( wcs, AST__CURRENT );
         map = astGetMapping( wcs, AST__BASE, AST__CURRENT );

/* Report an error if the 3rd WCS axis is not a time axis. */
         dom = astGetC( cfrm, "Domain(3)" );
         if( dom && strcmp( dom, "TIME" ) ) {
            if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               ndfMsg( "N", indf1 );
               errRep( "", "WCS axis 3 is not a time axis in \"^N\".",
                       status );
            }
         }

/* If this is the first input NDF, record the main details of the time
   axis. */
         if( ifile == 0 ) {
            timesys = astGetC( cfrm, "System(3)" );
            timescl = astGetC( cfrm, "TimeScale(3)" );
            timeorg = astGetC( cfrm, "TimeOrigin(3)" );
            timeunt = astGetC( cfrm, "Unit(3)" );
   
/* For later input NDFs, report an error if any of the details are
   different. */
         } else if( *status == SAI__OK ) {
            if( strcmp( timesys, astGetC( cfrm, "System(3)" ) ) ) {
               ndfMsg( "N", indf1 );
               msgSetc( "T", astGetC( cfrm, "System(3)" ) );
               *status = SAI__ERROR;
               errRep( "", "Unexpected time system (^T) in \"^N\".",
                       status );

            } else if( strcmp( timescl, astGetC( cfrm, "TimeScale(3)" ) ) ) {
               ndfMsg( "N", indf1 );
               msgSetc( "T", astGetC( cfrm, "TimeScale(3)" ) );
               *status = SAI__ERROR;
               errRep( "", "Unexpected time scale (^T) in \"^N\".",
                       status );

            } else if( strcmp( timeorg, astGetC( cfrm, "TimeOrigin(3)" ) ) ) {
               ndfMsg( "N", indf1 );
               msgSetc( "T", astGetC( cfrm, "TimeOrigin(3)" ) );
               *status = SAI__ERROR;
               errRep( "", "Unexpected time origin (^T) in \"^N\".",
                       status );

            } else if( strcmp( timeunt, astGetC( cfrm, "Unit(3)" ) ) ) {
               ndfMsg( "N", indf1 );
               msgSetc( "T", astGetC( cfrm, "Unit(3)" ) );
               *status = SAI__ERROR;
               errRep( "", "Unexpected time unit (^T) in \"^N\".",
                       status );
            }
         }

/* Split off the Mapping for the third (time) axis. */
         axes[ 0 ] = 3;
         astMapSplit( map, 1, axes, nnout, &tmap );
         if( tmap && astGetI( tmap, "Nout" ) == 1 ) {
   
/* Get an array holding the time axis grid indices in the current input NDF. */
            grid = astGrow( grid, dims[ 2 ], sizeof( double ) );
            for( i = 0; i < dims[ 2 ]; i++ ) grid[ i ] = i + 1;

/* Expand the table of time values so that there is room for the time
   values from the current input NDF. */
            tai = astGrow( tai, nts_in, sizeof( double ) );

/* Transform the grid indices to get the time axis values, appending them
   to the end of the "tai" array. */
            astTran1( tmap, dims[ 2 ], grid, 1, tai + nts_in - dims[ 2 ] );

/* Report an error if the Mapping could not be split. */
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", indf1 );
            errRep( "", "Unable to extract the time mapping from "
                    "the WCS FrameSet in \"^N\".", status );
         }            

/* Free resources. */
         astEnd;
      }

/* Get an array holding the list of concatenated RTS_NUM values read from
   the input NDFs. */
      rts = astMalloc( sizeof(int)*nts_in );
      if( !astMapGet1I( km1, "RTS_NUM", nts_in, &nts_in, rts ) ) {
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "", "The first input NDF (and maybe others) did not "
                    "contain a JCMTSTATE.RTS_NUM array.", status );
         }
      }

/* Get an array holding an index of the "rts" and "file_index" arrays that 
   accesses the arrays in order of increasing rts_num value. */
      index = smf_sorti( nts_in, rts, &sorted, status );

/* Note the number of unique RTS_NUM values. */
      if( index ) {
         rts_num_last = -INT_MAX;
         nts_out = 0;
         for( j = 0; j < nts_in; j++ ) {
            i = index[ j ];
            rts_num = rts[ i ];
            if( rts_num > rts_num_last ) nts_out++;
            rts_num_last = rts_num;
         }
      }

/* Get the required maximum output file size, and convert to a number of
   time slices. */
      nout = 0;
      if( *status == SAI__OK ) {

         parGet0i( "SIZELIMIT", &sizelimit, status );      
         if( *status == SAI__OK ) {
            if( sizelimit <= 0 ) {
               nout = 1;
               sizelimit = nts_out;

            } else {
               parChoic( "LIMITTYPE", "FILESIZE", "FILESIZE,SPECTRA,SLICES", 0, 
                         ltbuf, 10, status );
      
               if( !strcmp( ltbuf, "SPECTRA" ) ) {
                  sizelimit = sizelimit / ndet;
         
               } else if( !strcmp( ltbuf, "FILESIZE" ) ) {
                  sizelimit = sizelimit / 
                              ( ndet*nchan*( VAL__NBR*( hasvar ? 2 : 1 ) + 
                                             ( hasqual ? VAL__NBUB : 0) ) );
               }
            }
   
/*  Find the number of output NDFs needed. */
            nout = nts_out/sizelimit;
            if( nout*sizelimit < nts_out ) nout++;

/* If a null value was supplied, the number of output NDFs is the same as
   the number of input NDFs, and output time slices are divided evenly
   between them. */
         } else if( *status == PAR__NULL ) {
            errAnnul( status );
            nout = size;
            sizelimit = nts_out/nout;
            if( nout*sizelimit < nts_out ) sizelimit++;
         }
      }

/* Store a list of NDF component names to be mapped. */
      if( hasvar ) {
         comps = "Data,Variance";
      } else {
         comps = "Data";
      }

/* Get the WCS FrameSet from the first input NDF. */
      ndfGtwcs( ndfid[ 0 ], &wcs, status );

/* Get pointers to the current Frame, and the GRID->WCS Mapping. */
      cfrm = astGetFrame( wcs, AST__CURRENT );
      map = astGetMapping( wcs, AST__BASE, AST__CURRENT );

/* Split off the Mapping for the first and second axes. */
      axes[ 0 ] = 1;
      axes[ 1 ] = 2;
      astMapSplit( map, 2, axes, nnout, &omap );
      if( !omap || astGetI( omap, "Nout" ) != 2 ) {
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", indf1 );
            errRep( "", "Unable to extract the non-time mappings from "
                    "the WCS FrameSet in \"^N\".", status );
         }            
      }

/* Get a group of exactly one name upon which to base the names of the output 
   NDFs. The supplied string is the base name for the output NDFs, to which 
   "_1", "_2", etc, will be appended. Get the base name, and then delete the 
   group. */
      kpg1Wgndf( "OUT", igrp1, 1, 1, "", &igrp2, &outsize, status );
      pname = basename;
      grpGet( igrp2, 1, 1, &pname, GRP__SZNAM, status );
      grpDelet( &igrp2, status);
      basename[ astChrLen( basename ) ] = 0;

/* Initialise the bounds of each output NDF. */
      lbnd[ 0 ] = 1;
      lbnd[ 1 ] = 1;
      lbnd[ 2 ] = 1;
      ubnd[ 0 ] = nchan;
      ubnd[ 1 ] = ndet;
      ubnd[ 2 ] = sizelimit;

/* Initialise the bounds of each section used to represent one time slice
   in the output NDF. */
      slbnd[ 0 ] = 1;
      slbnd[ 1 ] = 1;
      subnd[ 0 ] = nchan;
      subnd[ 1 ] = ndet;

/* The number of time slices remaining to be written out. */
      nrem = nts_out;

/* Initialise the index into the "index" array. "j" steps through the
   input time slices, from 0 to ( nts_in - 1 ).  The "index[j]" value
   also steps through the input time slices, but in order of increasing
   RTS_NUM value. */
      j = 0;

/* Loop round each output NDF. */
      itimeout = NULL;
      taiout = NULL;
      for( iout = 0; iout < nout && *status == SAI__OK; iout++ ) {

/* Create the output NDF by propagation from the first input NDF. */
         sprintf( fullname, "%s_%d", basename, iout + 1 );
         msgSetc( "N", fullname );
         msgOutif( MSG__VERB, "", "Opening output NDF \"^N\".", status );
         ndfPlace( NULL, fullname, &place, status );
         ndfScopy( ndfid[ 0 ], "Units,Axis,NoExtension(PROVENANCE)", &place,
                   &indf2, status );

/* Modify its shape. The last output NDF will probably not need to be the
   full size, so ensure no output NDF is bigger than it needs to be. */
         if( nrem < sizelimit ) ubnd[ 2 ] = nrem;
         ndfSbnd( 3, lbnd, ubnd, indf2, status ); 

/* Ensure we have arrays large enough to hold the input time slice
   index and tai for each output time slice. */
         itimeout = astGrow( itimeout, ubnd[ 2 ], sizeof( int ) );
         taiout = astGrow( taiout, ubnd[ 2 ], sizeof( double ) );

/* Get the RTS_NUM value from the next input time slice. This value is
   also the RTS_NUM value for the next output time slice. */
         i = index[ j ];
         rts_num = rts[ i ];

/* Loop round each output time slice. */
         for( k = 0; k < ubnd[ 2 ]  && *status == SAI__OK; k++ ) {

/* Get a section of the output NDF covering this time slice. */
            slbnd[ 2 ] = k + 1;
            subnd[ 2 ] = k + 1;
            ndfSect( indf2, 3, slbnd, subnd, &indf2s, status ); 

/* Map the required output NDF section array components. */
            ndfMap( indf2s, comps, "_REAL", "WRITE", ptr, &el, 
                    status );
            dts_out = ptr[ 0 ];
            vts_out = hasvar ? ptr[ 1 ] : NULL;
   
            if( hasqual ) {
               ndfMap( indf2s, "Quality", "_UBYTE", "WRITE", ptr, 
                       &el, status );
               qts_out = ptr[ 0 ];
            } else {
               qts_out = NULL;
            }

/* Store the input time slice index for this output time slice. */
            itimeout[ k ] = i;

/* Store the input time slice tai for this output time slice. */
            taiout[ k ] = tai[ i ]; 

/* Loop round all input time slices that refer to the same RTS_NUM value. */
            init = 1;
            rts_num0 = rts_num;
            while( rts_num == rts_num0 ) {

/* Get the index of the input NDF from which the current RTS_NUM value was 
   read. */
               ifile = file_index[ i ];

/* Get a section of the input NDF covering this time slice. */
               slbnd[ 2 ] = i  - first[ ifile ] + 1;
               subnd[ 2 ] = slbnd[ 2 ];
               ndfSect( ndfid[ ifile ], 3, slbnd, subnd, &indf1s, status ); 

/* Map the required input NDF section array components. */
               ndfMap( indf1s, comps, "_REAL", "READ",  ptr, &el, status );
               dts_in = ptr[ 0 ];
               vts_in = hasvar ? ptr[ 1 ] : NULL;
   
               if( hasqual ) {
                  ndfMap( indf1s, "Quality", "_UBYTE", "READ", ptr, 
                          &el, status );
                  qts_in = ptr[ 0 ];
               } else {
                  qts_in = NULL;
               }

/*  Loop round all elements in the time slice. Copy any good array values 
    from input to output. */
               for( iel = 0; iel < el; iel++ ) {
                  if( dts_in[ iel ] != VAL__BADR ) {
                     dts_out[ iel ] = dts_in[ iel ];
                     if( hasvar ) vts_out[ iel ] = vts_in[ iel ];
                     if( hasqual) qts_out[ iel ] = qts_in[ iel ];

                  } else if( init ) {
                     dts_out[ iel ] = VAL__BADR;
                     if( hasvar ) vts_out[ iel ] = VAL__BADR;
                     if( hasqual) qts_out[ iel ] = 0;
                  }
               }

/* Annul the identifier for the current time slice in the input NDF. */
               ndfAnnul( &indf1s, status );

/* Move on to the next input time slice. */
               if( ++j < nts_in ) {
                  i = index[ j ];
                  rts_num = rts[ i ];
                  init = 0;
               } else {
                  break;
               }
            }

/* Annul the identifier for the current time slice in the output NDF. */
            ndfAnnul( &indf2s, status );
         }

/* Copy input JCMTSTATE values to the current output NDF. */
         smf_km2ext( indf2, "JCMTSTATE", km1, itimeout, status );

/* Copy input ACSIS values to the current output NDF. */
         smf_km2ext( indf2, "ACSIS", km2, itimeout, status );

/* Create a LutMap to holdthe TAI value for each time slice in the
   current output NDF. */
         lut = astLutMap( ubnd[ 2 ], taiout, 1.0, 1.0, "" );

/* Put this Mapping in parallel with the Mapping for the other axes. */
         map = (AstMapping *) astCmpMap( omap, lut, 0, "" );
               
/* Remove the current Frame from the WCS FrameSet, then add it back in again
   using the above Mapping to connect it to the GRID (base) Frame. */
         astRemoveFrame( wcs, AST__CURRENT );
         astAddFrame( wcs, AST__BASE, map, cfrm );
   
/* Store the modifed WCS FrameSet in the output NDF. */
         ndfPtwcs( wcs, indf2, status );

/* Reduce the number of time slices remaining to be written out. */
         nrem -= ubnd[ 2 ];

/* Record each input NDF as a direct parent of indf2. */
         for( ifile = 0; ifile < size; ifile++ ) {
            ndgPtprv( indf2, ndfid[ ifile ], NULL, 0, "SMURF:TIMESORT", 
                      status );
         }
   
/* Delete the output NDF if an error occurred. Otherwise, annul the
   output NDF identifier. */
         ndfMsg( "N", indf2 );
         msgOutif( MSG__VERB, "", "Closing output NDF \"^N\".", status );
         if( *status != SAI__OK ) {
            ndfDelet( &indf2, status );
         } else {
            ndfAnnul( &indf2, status );
         }
      }

/* Write out the number of output NDFs. */
      parPut0i( "NOUT", nout, status );

/* Free resources. */
      ndfid = astFree( ndfid );
      taiout = astFree( taiout );
      itimeout = astFree( itimeout );
      first = astFree( first );
      index = astFree( index );
      file_index = astFree( file_index );
      rts = astFree( rts );
      obsidss0 = astFree( obsidss0 );
      grid = astFree( grid );
      tai = astFree( tai );
   }

/* Free resources. */
   grpDelet( &igrp1, status );

/* End the NDF context. */
   ndfEnd( status );

/* Issue a status indication.*/  
   if( *status == SAI__OK ) {
      msgOutif( MSG__VERB, "", "TIMESORT succeeded.", status);
   } else {
      msgOutif( MSG__VERB, "", "TIMESORT failed.", status);
   }
}
