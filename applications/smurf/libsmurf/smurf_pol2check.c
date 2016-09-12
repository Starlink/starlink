/*
*+
*  Name:
*     POL2CHECK

*  Purpose:
*     Check if specified NDFs probably hold POL-2 data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_pol2check( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This application checks each supplied file to see if it looks like
*     it probably holds POL-2 data in a recognised form. If it does, it
*     is categorised as either:
*
*     - raw analysed intensity time-series data
*     - Q, U or I time-series data created by CALCQU
*     - Q, U or I maps created by MAKEMAP.
*
*     If requested, output text files are created each holding a list
*     of the paths for the NDFs in each category.
*
*     The checks are based on NDF meta-data and FITS headers. It is
*     possible that an NDF could pass these checks and yet fail to open
*     in other smurf task if any of the additional meta-data required by
*     those tasks has been corrupted or is otherwise inappropriate.

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDFs(s).
*     JUNKFILE = LITERAL (Read)
*        The name of a text file to create containing the paths to the
*        input NDFs that do not hold POL-2 data in any recognised form.
*        Only accessed if one or more such NDFs are found within the
*        group of NDFs specified by parameter IN. Supplying null (!)
*        results in no file being created. [!]
*     JUNKFOUND = _LOGICAL (Write)
*        Returned TRUE if one or more of the input NDFs is not a recognised
*        POL-2 file.
*     MAPFILE = LITERAL (Read)
*        The name of a text file to create containing the paths to the
*        input NDFs that hold 2-dimensional maps of Q, U or I from
*        POL-2 data. Only accessed if one or more such NDFs are found within
*        the group of NDFs specified by parameter IN. Supplying null (!)
*        results in no file being created. [!]
*     MAPFOUND = _LOGICAL (Write)
*        Returned TRUE if one or more of the input NDFs holds 2-dimensonal
*        maps of Q, U  or I from POL-2 data.
*     MAPINFO = LITERAL (Read)
*        The name of a text file to create containing a line of
*        information for each input file listed in the MAPFILE file (in
*        the same order). Each line contains two space-sparated items:
*        the first is a single letter Q, U or I indicating the Stokes
*        parameter, and the second is an identifier of the form
*        "<UT>_<OBS>_<SUBSCAN>", where <UT> is the 8 digit UT date, <OBS>
*        is the 5 digit observation number and <SUBSCAN> is the four digit
*        number for the first subscan in the chunk (usually "0003" except
*        for observations made up of more than one discontiguous chunks).
*        No file is created if null (!) is supplied. [!]
*     RAWFILE = LITERAL (Read)
*        The name of a text file to create containing the paths to the
*        input NDFs that hold raw analysed intensity POL-2 time-series
*        data. Only accessed if one or more such NDFs are found within
*        the group of NDFs specified by parameter IN. Supplying null (!)
*        results in no file being created. [!]
*     RAWFOUND = _LOGICAL (Write)
*        Returned TRUE if one or more of the input NDFs holds raw analysed
*        intensity POL-2 time-series data.
*     RAWINFO = LITERAL (Read)
*        The name of a text file to create containing a line of
*        information for each input file listed in the RAWFILE file (in
*        the same order). Each line contains a key for the raw data file
*        of the form ""<UT>_<OBS>", where <UT> is the 8 digit UT date, and
*        <OBS> is the 5 digit observation number. No file is created if
*        null (!) is supplied. [!]
*     STOKESFILE = LITERAL (Read)
*        The name of a text file to create containing the paths to the
*        input NDFs that hold Q, U or I POL-2 time-series data. Only
*        accessed if one or more such NDFs are found within the group of
*        NDFs specified by parameter IN. Supplying null (!) results in no
*        file being created. [!]
*     STOKESFOUND = _LOGICAL (Write)
*        Returned TRUE if one or more of the input NDFs holds Q, U or I
*        POL-2 time-series data.
*     STOKESINFO = LITERAL (Read)
*        The name of a text file to create containing a line of
*        information for each input file listed in the STOKESFILE file (in
*        the same order). Each line contains two space-sparated items:
*        the first is a single letter Q, U or I indicating the Stokes
*        parameter, and the second is an identifier of the form
*        "<UT>_<OBS>_<SUBSCAN>", where <UT> is the 8 digit UT date, <OBS>
*        is the 5 digit observation number and <SUBSCAN> is the four digit
*        number for the first subscan in the chunk (usually "0003" except
*        for observations made up of more than one discontiguous chunks).
*        No file is created if null (!) is supplied. [!]
*     STOKES = LITERAL (Read)
*        The name of a text file to create containing the identifiers

*  Notes:
*     - This application was written originally for use within the pol2scan.py
*     script, as a means of speeding up operations that are very slow when
*     imlemented via multiple calls to KAPPA commands such as "fitsval", etc.

*  Authors:
*     David S Berry (EAO, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-SEP-2016 (DSB):
*        Original version.
*     12-SEP-2016 (DSB):
*        Add parameter STOKESINFO, MAPINFO and RAWINFO.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory
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
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "msg_par.h"
#include "ndf.h"
#include "par.h"
#include "par_err.h"
#include "ast.h"
#include "sae_par.h"
#include "star/grp.h"
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "jcmt/state.h"
#include "smurf_par.h"
#include "smurflib.h"


/* Main entry */
void smurf_pol2check( int *status ) {

/* Local Variables: */
   AstFitsChan *fc;           /* The contents of the NDF's FITS extension */
   AstKeyMap *km;             /* KeyMap holding lists of matching NDFs */
   FILE *fd;                  /* File descriptor for output text file */
   Grp *igrp = NULL;          /* Group of input files */
   char *cval;                /* Header value */
   char *pname;               /* Pointer to input filename */
   char buf[GRP__SZNAM+1];    /* Path to matching NDF */
   char filepath[GRP__SZNAM+1];/* NDF path, derived from GRP */
   char label[GRP__SZNAM+1];  /* NDF label string */
   int dims[NDF__MXDIM];      /* No. of pixels along each axis of NDF */
   int indf;                  /* NDF identifier */
   int ndims;                 /* Number of dimensions in NDF */
   int obs;                   /* Observation number */
   int ok;                    /* NDF holds POL-2 data ? */
   int subscan;               /* Subscan number  at start of chunk */
   int there;                 /* Does it exist? */
   int utdate;                /* UT date */
   int veclen;                /* No. of matching NDFs */
   size_t i;                  /* Index into group */
   size_t isize;              /* Number of input NDFs */

/* Check inhereited status */
   if( *status != SAI__OK ) return;

/* Start new AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* Get a group of input NDFs. */
   kpg1Rgndf( "IN", 0, 1, "  Give more NDFs...", &igrp, &isize, status );

/* Create KeyMap to hold the classified lists of NDF paths. */
   km = astKeyMap( " " );

/* Loop round all NDFs. */
   for( i = 1; i <= isize && *status == SAI__OK; i++ ) {
      ok = 0;

/* Get the NDF path from the group. */
      pname = filepath;
      grpGet( igrp, i, 1, &pname, sizeof(filepath), status );

/* Open the NDF and get an identifier for it. */
      ndgNdfas( igrp, i, "READ", &indf, status );

/* Get a FitsChan holding the contents of the FITS extension. */
      ndfXstat( indf, "FITS", &there, status );
      if( there ) {
         kpgGtfts( indf, &fc, status );

/* Check the INBEAM header exists and is "pol" (case insensitive). */
         if( astTestFits( fc, "INBEAM", NULL ) &&
             astGetFitsS( fc, "INBEAM", &cval ) &&
             !strncmp( cval, "pol", 3 ) ) {

/* Get the pixel dimensions of the NDF. */
            ndfDim( indf, NDF__MXDIM, dims, &ndims, status );

/* Time-series data is 3-dimensional, the first axes are 32 and 40, and
   the third axis is more than one. */
            if( ndims == 3 && dims[0] == 32 && dims[1] == 40 && dims[2] > 1 ) {

/* It must have a JCMTSTATE extension. */
               ndfXstat( indf, JCMT__EXTNAME, &there, status );
               if( there ) {

/* For raw analysed intensity data check that the NDF Label component is
   "Signal". */
                  ndfCget( indf, "Label", label, sizeof(label), status );
                  if( !strcmp( label, "Signal" ) ) {
                     astMapPutElemC( km, "RAW_TS", -1, filepath );
                     msgOutf( "", "   %s - raw analysed intensity time-series",
                              status, filepath );
                     ok = 1;

/* Also form and store the line of extra information. */
                     astGetFitsI( fc, "UTDATE", &utdate );
                     astGetFitsI( fc, "OBSNUM", &obs );
                     sprintf( buf, "%8.8d_%5.5d", utdate, obs );
                     astMapPutElemC( km, "RAW_INFO", -1, buf );

/* For Stokes parameter data check that the NDF Label component is
   "Q", "U" or "I". */
                  } else if( !strcmp( label, "Q" ) ||
                             !strcmp( label, "U" ) ||
                             !strcmp( label, "I" ) ) {
                     astMapPutElemC( km, "STOKES_TS", -1, filepath );
                     msgOutf( "", "   %s - Stokes parameter time-series",
                              status, filepath );
                     ok = 1;

/* Also form and store the line of extra information. */
                     astGetFitsI( fc, "UTDATE", &utdate );
                     astGetFitsI( fc, "OBSNUM", &obs );
                     astGetFitsI( fc, "NSUBSCAN", &subscan );
                     sprintf( buf, "%s %8.8d_%5.5d_%4.4d", label, utdate, obs,
                              subscan );
                     astMapPutElemC( km, "STOKES_INFO", -1, buf );
                  }
               }

/* If the data is 2 dimensional, or 3 dimensional with a degenerate 3rd
   axis, it's a map. Check it has a Label of Q, U, or I. */
            } else if( ndims == 2 || ( ndims == 3 && dims[2] == 1 ) ) {
               ndfCget( indf, "Label", label, sizeof(label), status );
               if( !strcmp( label, "Q" ) || !strcmp( label, "U" ) ||
                   !strcmp( label, "I" ) ) {
                  astMapPutElemC( km, "MAP", -1, filepath );
                  msgOutf( "", "   %s - Stokes map", status, filepath );
                  ok = 1;

/* Also form and store the line of extra information. */
                  astGetFitsI( fc, "UTDATE", &utdate );
                  astGetFitsI( fc, "OBSNUM", &obs );
                  astGetFitsI( fc, "NSUBSCAN", &subscan );
                  sprintf( buf, "%s %8.8d_%5.5d_%4.4d", label, utdate, obs,
                           subscan );
                  astMapPutElemC( km, "MAP_INFO", -1, buf );
               }
            }
         }

/* Annul the FitsChan. */
         fc = astAnnul( fc );
      }

/* If the NDF was not recognised as POL-2 data, store it in the junk bin. */
      if( !ok ) {
         astMapPutElemC( km, "JUNK", -1, filepath );
         msgOutf( "","   %s - not a POL-2 file", status, filepath );
      }

/* Close the NDF. */
      ndfAnnul( &indf, status );
   }

   msgBlank( status );
   msgOutf( "", "Out of %zu input NDFs:", status, isize );

/* Create text files holding the lists. */
   veclen = astMapLength( km, "RAW_TS" );
   parPut0l( "RAWFOUND", ( veclen > 0 ), status );
   if( veclen > 0 ) {
      msgOutf( "", "   %d hold raw analysed POL-2 time-series data.",
               status, veclen );
      parGet0c( "RAWFILE", filepath, sizeof(filepath), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else if ( *status == SAI__OK ) {
         fd = fopen( filepath, "w" );
         for( i = 0; (int) i < veclen; i++ ) {
            astMapGetElemC( km, "RAW_TS", sizeof(buf), i, buf );
            fprintf( fd, "%s\n", buf );
         }
         fclose( fd );
      }
   }

   veclen = astMapLength( km, "RAW_INFO" );
   if( veclen > 0 ) {
      parGet0c( "RAWINFO", filepath, sizeof(filepath), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else if ( *status == SAI__OK ) {
         fd = fopen( filepath, "w" );
         for( i = 0; (int) i < veclen; i++ ) {
            astMapGetElemC( km, "RAW_INFO", sizeof(buf), i, buf );
            fprintf( fd, "%s\n", buf );
         }
         fclose( fd );
      }
   }

   veclen = astMapLength( km, "STOKES_TS" );
   parPut0l( "STOKESFOUND", ( veclen > 0 ), status );
   if( veclen > 0 ) {
      msgOutf( "", "   %d hold Stokes parameter POL-2 time-series data.",
               status, veclen );
      parGet0c( "STOKESFILE", filepath, sizeof(filepath), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else if ( *status == SAI__OK ) {
         fd = fopen( filepath, "w" );
         for( i = 0; (int) i < veclen; i++ ) {
            astMapGetElemC( km, "STOKES_TS", sizeof(buf), i, buf );
            fprintf( fd, "%s\n", buf );
         }
         fclose( fd );
      }
   }

   veclen = astMapLength( km, "STOKES_INFO" );
   if( veclen > 0 ) {
      parGet0c( "STOKESINFO", filepath, sizeof(filepath), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else if ( *status == SAI__OK ) {
         fd = fopen( filepath, "w" );
         for( i = 0; (int) i < veclen; i++ ) {
            astMapGetElemC( km, "STOKES_INFO", sizeof(buf), i, buf );
            fprintf( fd, "%s\n", buf );
         }
         fclose( fd );
      }
   }

   veclen = astMapLength( km, "MAP" );
   parPut0l( "MAPFOUND", ( veclen > 0 ), status );
   if( veclen > 0 ) {
      msgOutf( "", "   %d hold POL-2 maps.", status, veclen );
      parGet0c( "MAPFILE", filepath, sizeof(filepath), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else if ( *status == SAI__OK ) {
         fd = fopen( filepath, "w" );
         for( i = 0; (int) i < veclen; i++ ) {
            astMapGetElemC( km, "MAP", sizeof(buf), i, buf );
            fprintf( fd, "%s\n", buf );
         }
         fclose( fd );
      }
   }

   veclen = astMapLength( km, "MAP_INFO" );
   if( veclen > 0 ) {
      parGet0c( "MAPINFO", filepath, sizeof(filepath), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else if ( *status == SAI__OK ) {
         fd = fopen( filepath, "w" );
         for( i = 0; (int) i < veclen; i++ ) {
            astMapGetElemC( km, "MAP_INFO", sizeof(buf), i, buf );
            fprintf( fd, "%s\n", buf );
         }
         fclose( fd );
      }
   }


   veclen = astMapLength( km, "JUNK" );
   parPut0l( "JUNKFOUND", ( veclen > 0 ), status );
   if( veclen > 0 ) {
      msgOutf( "", "   %d do not hold recognised POL-2 data.", status,
               veclen );
      parGet0c( "JUNKFILE", filepath, sizeof(filepath), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else if ( *status == SAI__OK ) {
         fd = fopen( filepath, "w" );
         for( i = 0; (int) i < veclen; i++ ) {
            astMapGetElemC( km, "JUNK", sizeof(buf), i, buf );
            fprintf( fd, "%s\n", buf );
         }
         fclose( fd );
      }
   }

/* Free resources. */
   if( igrp ) grpDelet( &igrp, status);

/* End the NDF and AST contexts. */
   ndfEnd( status );
   astEnd;

/* Issue a status indication.*/
   if( *status == SAI__OK ) {
     msgOutif( MSG__VERB, " ", "POL2CHECK succeeded.", status);
   } else {
     msgOutif( MSG__VERB, " ", "POL2CHECK failed.", status);
   }
}




