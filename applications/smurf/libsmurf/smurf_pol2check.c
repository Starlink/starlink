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
*
*     An error is reported if POL2 data from more than one waveband (450
*     or 850) is present in the list of supplied data files.
*
*     By default, an error is also reported if POL2 data for more than
*     one object is present in the list of supplied data files (this check
*     can be disabled by setting parameter MULTIOBJECT to TRUE). The object
*     is given by FITS header "OBJECT".

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
*     MISSING = LITERAL (Read)
*        The name of a text file to create identifying any missing raw
*        data sub-scans. No file is created if no sub-scans are missing
*        or if no raw data is supplied. The largest expected sub-scan
*        number for all sub-arrays is the largest sub-scan number for
*        which any raw data was found for any sub-array. The text file
*        will contain a line for each sub-array that has any missing
*        sub-scans. Each line will start with the sub-array name and be
*        followed by a space spearated list of sub-scan identifiers.
*        For instance, "S8A: _0012 _0034".
*     MULTIOBJECT = _LOGICAL (Read)
*        Indicates if it is acceptable for the list of input files to
*        include data for multiple objects. If FALSE, an error is reported
*        if data for more than one object is specified by parameter IN.
*        Otherwise, no error is reported if multiple objects are found.
*        [FALSE]
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
*     12-OCT-2016 (DSB):
*        Add parameters MISSING.
*     16-NOV-2016 (DSB):
*        Correct reporting of missing files. Previous scheme only worked
*        if the input file list contained data for only one observation.
*     31-MAR-2017 (DSB):
*        Add support for 450 um data.
*     9-APR-2018 (DSB):
*        Add parameter MULTIOBJECT.
*     8-JUN-2019 (DSB):
*        Coadded maps do not have UTDATE or OBSNUM headers. So use
*        "<OBJECT>_<DATE-OBS>" as the key for coadded maps.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2016-2017 East Asian Observatory
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

/* A value that is larger than the number of raw data sub-scans in any POL-2
   observation. */
#define MAX_SCAN 200

/* A structure that describes the raw data subscans that have been found
   for a single observation. We allocate one such structure for each
   observation. */
typedef struct SubScanInfo {
   int maxscan;            /* Largest NSUBSCAN value found for any subarray */
   char sa_scans[ MAX_SCAN ];/* Flags showing the S4/8A subscans found */
   char sb_scans[ MAX_SCAN ];/* Flags showing the S4/8B subscans found */
   char sc_scans[ MAX_SCAN ];/* Flags showing the S4/8C subscans found */
   char sd_scans[ MAX_SCAN ];/* Flags showing the S4/8D subscans found */
   int sa_count;             /* Any S4/8A raw data files supplied? */
   int sb_count;             /* Any S4/8B raw data files supplied? */
   int sc_count;             /* Any S4/8C raw data files supplied? */
   int sd_count;             /* Any S4/8D raw data files supplied? */
} SubScanInfo;

/* Main entry */
void smurf_pol2check( int *status ) {

/* Local Variables: */
   AstFitsChan *fc;           /* The contents of the NDF's FITS extension */
   AstKeyMap *km;             /* KeyMap holding lists of matching NDFs */
   AstKeyMap *sskm;           /* KeyMap holding SubScan information */
   FILE *fd;                  /* File descriptor for output text file */
   Grp *igrp = NULL;          /* Group of input files */
   SubScanInfo *ssptr;        /* Pointer to subscan info for current obs */
   char *cval;                /* Header value */
   char *pname;               /* Pointer to input filename */
   char buf[1000];            /* Path to matching NDF */
   char dateobs[80];          /* Pointer to DATE-OBS  */
   char filepath[GRP__SZNAM+1];/* NDF path, derived from GRP */
   char label[GRP__SZNAM+1];  /* NDF label string */
   char object0[80];          /* Pointer to Object for first POL2 file */
   char object[80];           /* Pointer to Object for current POL2 file */
   const char *key;           /* Pointer to KeyMap key string */
   const char *wave0;         /* Pointer to waveband for first POL2 file */
   const char *wave;          /* Pointer to waveband for current POL2 file */
   int dims[NDF__MXDIM];      /* No. of pixels along each axis of NDF */
   int ikey;                  /* Index of next key */
   int indf;                  /* NDF identifier */
   int multiobject;           /* OK for more than one object to be given? */
   int ndims;                 /* Number of dimensions in NDF */
   int nkey;                  /* Number of keys in KeyMap */
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

/* Create KeyMap to hold information about which raw data subscans have
   been found in each observation. */
   sskm = astKeyMap( " " );

/* See if it is OK for data for more than one object to be supplied. */
   parGet0l( "MULTIOBJECT", &multiobject, status );

/* Loop round all NDFs. */
   wave0 = NULL;
   *object0 = 0;
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
            wave = NULL;
            *object = 0;

/* Get the pixel dimensions of the NDF. */
            ndfDim( indf, NDF__MXDIM, dims, &ndims, status );

/* Time-series data is 3-dimensional, the first axes are 32 and 40, and
   the third axis is more than one. */
            if( ndims == 3 && dims[0] == 32 && dims[1] == 40 && dims[2] > 1 ) {

/* It must have a JCMTSTATE extension. */
               ndfXstat( indf, JCMT__EXTNAME, &there, status );
               if( there ) {

/* For raw analysed intensity data check that the NDF Label component is
   "Signal". The initial contents of "label;" are used as the default, so
   ensure that the initial value of "label" is a properly terminated string. */
                  *label = 0;
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

/* We want to check later that no sub-scans are missing from the
   observations, so set flags for each sub-array indicating which NSUBSCAN
   header values have been found. If this is the first time we've been
   here for the current observation, allocate and initialise a structure
   holding flag arrays to indicate that no subscans have been found. */
                     if( !astMapGet0P( sskm, buf, (void **) &ssptr ) ) {
                        ssptr = astCalloc( 1, sizeof(*ssptr) );
                        astMapPut0P( sskm, buf, ssptr, NULL );
                     }

                     astGetFitsI( fc, "NSUBSCAN", &subscan );
                     if( subscan > ssptr->maxscan ) ssptr->maxscan = subscan;
                     astGetFitsS( fc, "SUBARRAY", &cval );

                     if( astChrMatch( cval, "s8a" ) || astChrMatch( cval, "s4a" ) ) {
                        ssptr->sa_count++;
                        ssptr->sa_scans[ subscan - 1 ] = 1;
                     } else if( astChrMatch( cval, "s8b" ) || astChrMatch( cval, "s4b" ) ) {
                        ssptr->sb_count++;
                        ssptr->sb_scans[ subscan - 1 ] = 1;
                     } else if( astChrMatch( cval, "s8c" ) || astChrMatch( cval, "s4c" ) ) {
                        ssptr->sc_count++;
                        ssptr->sc_scans[ subscan - 1 ] = 1;
                     } else if( astChrMatch( cval, "s8d" ) || astChrMatch( cval, "s4d" ) ) {
                        ssptr->sd_count++;
                        ssptr->sd_scans[ subscan - 1 ] = 1;
                     } else if( *status == SAI__OK ) {
                        *status = SAI__ERROR;
                        errRepf("","Unsupported SUBARRAY header value "
                                "'%s' found in %s.", status, cval, filepath );
                     }


/* Get the waveband. */
                     wave = (cval[1] == '8') ? "S8" : "S4";

/* Get the object. */
                     astGetFitsS( fc, "OBJECT", &cval );
                     if( *status == SAI__OK ) strcpy( object, cval );

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

/* Get the waveband. */
                     astGetFitsS( fc, "SUBARRAY", &cval );
                     wave = (cval[1] == '8') ? "S8" : "S4";

/* Get the object. */
                     astGetFitsS( fc, "OBJECT", &cval );
                     if( *status == SAI__OK ) strcpy( object, cval );
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

/* Also form and store the line of extra information. Note, if the map is
   a coadd of several observations, it will not have UTDATE or OBSNUM
   headers. In such cases use a key of the form "<OBJECT>_<DATE-OBS>". */
                  if( astGetFitsI( fc, "UTDATE", &utdate ) &&
                      astGetFitsI( fc, "OBSNUM", &obs ) &&
                      astGetFitsI( fc, "NSUBSCAN", &subscan ) ){
                     sprintf( buf, "%s %8.8d_%5.5d_%4.4d", label, utdate, obs,
                              subscan );
                  } else {
                     astGetFitsS( fc, "OBJECT", &cval );
                     if( *status == SAI__OK ) strcpy( object, cval );
                     astGetFitsS( fc, "DATE-OBS", &cval );
                     if( *status == SAI__OK ) strcpy( dateobs, cval );
                     sprintf( buf, "%s %s_%s", label, object, dateobs );
                  }
                  astMapPutElemC( km, "MAP_INFO", -1, buf );


/* Get the waveband. */
                  astGetFitsS( fc, "FILTER", &cval );
                  if( !strncmp( cval, "850", 3 ) ) {
                     wave = "S8";
                  } else if( !strncmp( cval, "450", 3 ) ) {
                     wave = "S4";
                  } else if( *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     errRepf("","Unsupported FILTER header value "
                                "'%s' found in %s.", status, cval, filepath );
                  }

/* Get the object. */
                  astGetFitsS( fc, "OBJECT", &cval );
                  if( *status == SAI__OK ) strcpy( object, cval );

               }
            }

/* Check the waveband of this POL2 file is the same as the waveband for
   all previous POL2 files. */
            if( wave ) {
               if( wave0 ) {
                  if( strcmp( wave, wave0 ) && *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     errRepf(" ","Both 450 um and 850 um POL2 data "
                             "supplied - only one waveband should "
                             "be supplied.", status );
                  }
               } else {
                  wave0 = wave;
               }
            }

/* Check the data is for the same object as all previous files. */
            if( *object ) {
               if( *object0 ) {
                  if( !multiobject && strcmp( object, object0 ) && *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     errRepf(" ","Data supplied for multiple objects ('%s' "
                             "and '%s'). If you really want to process these "
                             "objects together, re-run with parameter setting "
                             "MULTIOBJECT=YES.", status, object, object0 );
                  }
               } else {
                  strcpy( object0, object );
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
         for( i = 0; (int) i < veclen && fd; i++ ) {
            astMapGetElemC( km, "RAW_TS", sizeof(buf), i, buf );
            fprintf( fd, "%s\n", buf );
         }
         if( fd ) fclose( fd );
      }

/* Report any missing sub-scans. */
      fd = NULL;
      nkey = astMapSize( sskm );
      for( ikey = 0; ikey < nkey; ikey++ ) {
         key = astMapKey( sskm, ikey );
         astMapGet0P( sskm, key, (void **) &ssptr );

         if( ssptr->sa_count ) ssptr->sa_count = ssptr->maxscan - ssptr->sa_count;
         if( ssptr->sb_count ) ssptr->sb_count = ssptr->maxscan - ssptr->sb_count;
         if( ssptr->sc_count ) ssptr->sc_count = ssptr->maxscan - ssptr->sc_count;
         if( ssptr->sd_count ) ssptr->sd_count = ssptr->maxscan - ssptr->sd_count;

         if( ssptr->sa_count || ssptr->sb_count || ssptr->sc_count || ssptr->sd_count ) {

            if( !fd ) {
               parGet0c( "MISSING", filepath, sizeof(filepath), status );
               if( *status == PAR__NULL ) {
                  errAnnul( status );
               } else if( *status == SAI__OK ){
                  fd = fopen( filepath, "w" );
               }
               if( !fd ) fd = stdout;

               msgBlank( status );
               msgOutf( "", "WARNING: The raw data files for some sub-scans "
                        "seem to be missing.", status );
            }

            if( fd == stdout ) fprintf( fd, "   " );
            fprintf( fd, "\nObservation %s:\n", key );

            if( ssptr->sa_count ) {
               if( fd == stdout ) fprintf( fd, "   " );
               fprintf( fd, "   %sA: ", wave0 );
               for( subscan = 0; subscan < ssptr->maxscan; subscan++ ) {
                  if( !ssptr->sa_scans[ subscan ] ) {
                     fprintf( fd, "_%04d ", subscan + 1 );
                  }
               }
               fprintf( fd, "\n" );
            }

            if( ssptr->sb_count ) {
               if( fd == stdout ) fprintf( fd, "   " );
               fprintf( fd, "   %sB: ", wave0 );
               for( subscan = 0; subscan < ssptr->maxscan; subscan++ ) {
                  if( !ssptr->sb_scans[ subscan ] ) {
                     fprintf( fd, "_%04d ", subscan + 1 );
                  }
               }
               fprintf( fd, "\n" );
            }

            if( ssptr->sc_count ) {
               if( fd == stdout ) fprintf( fd, "   " );
               fprintf( fd, "   %sC: ", wave0 );
               for( subscan = 0; subscan < ssptr->maxscan; subscan++ ) {
                  if( !ssptr->sc_scans[ subscan ] ) {
                     fprintf( fd, "_%04d ", subscan + 1 );
                  }
               }
               fprintf( fd, "\n" );
            }

            if( ssptr->sd_count ) {
               if( fd == stdout ) fprintf( fd, "   " );
               fprintf( fd, "   %sD: ", wave0 );
               for( subscan = 0; subscan < ssptr->maxscan; subscan++ ) {
                  if( !ssptr->sd_scans[ subscan ] ) {
                     fprintf( fd, "_%04d ", subscan + 1 );
                  }
               }
               fprintf( fd, "\n" );
            }
         }

         ssptr = astFree( ssptr );
      }

      if( fd && fd != stdout ) {
         fclose( fd );
         msgOutf( "", "See file %s for details.", status, filepath );
         msgBlank( status );
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




