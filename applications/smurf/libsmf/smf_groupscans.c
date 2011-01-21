/*
*+
*  Name:
*     smf_groupscans

*  Purpose:
*     Group time series NDFs according to OBSID, SUBSYSNR and NSUBSCAN.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     AstKeyMap *smf_groupscans( const Grp *igrp,  int size, int *maxsyspop,
*                                int *conform, Grp **ogrp, int *status );

*  Arguments:
*     igrp = const Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     maxsyspop = int * (Returned)
*        Returned holding the largest number of NDFs that refer to the
*        same observation and sub-system.
*     conform = int * (Returned)
*        Returned non-zero if all the input NDFs have names that conform
*        to the naming convention for ACSIS raw time series files.
*     ogrp = Grp ** (Returned)
*        Location at which to return a pointer to a new GRP group. This
*        group holds the names of the first input NDF related to each
*        set of related sub-scans (i.e. sub-scans for the same observation
*        and sub-system). They are ordered in the same order that the
*        sub-scans would be accessed if the nested KeyMaps returned by this
*        function are traversed using the order of keys defined by astMapKey.

*  Returned Value:
*     A pointer to the AST KeyMap.

*  Description:
*     This function classifies each NDF in the supplied group using the
*     values for the OBSID, SUBSYSNR and NSUBSCAN keywords in the FITS
*     extension. It returns a pointer to an AST KeyMap containing entries
*     for each OBSID value found in the specified NDFs. Each of these
*     entries has a key equal to the OBSID value, and a value which is a
*     pointer to another AST KeyMap containing information about the NDFs
*     that have the OBSID value. Each of these subsiduary KeyMaps has an
*     entry for each SUBSYSNR value found for the OBSID value. The key is
*     the SUBSYSNR value, and the entry is a pointer to another KeyMap
*     containing information about the NDFs that have the SUBSYSNR and
*     OBSID values. Each of these KeyMaps has an entry for each NSUBSCAN
*     value, the key being the NSUBSCAN value and the value being the
*     integer (one-based) index of the NDF within "igrp".

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     17-MAR-2008 (DSB):
*        Initial version.
*     27-MAR-2008 (DSB):
*        Use smf_getobsidss to get the OBSIDSS value, and then split it
*        up into OBSID and SUBSYSNR.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "ndf.h"
#include "grp.h"
#include "smf.h"
#include "mers.h"
#include "star/ndg.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/kaplibs.h"
#include "star/one.h"

AstKeyMap *smf_groupscans( const Grp *igrp,  int size, int *maxsyspop,
                           int *conform, Grp **ogrp, int *status ){

/* Local Variables */
   AstFitsChan *fc = NULL;
   AstKeyMap *obsmap = NULL;
   AstKeyMap *result = NULL;
   AstKeyMap *sysmap = NULL;
   char *cvalue = NULL;
   char *match = NULL;
   char *nsubscan= NULL;
   char *pname = NULL;
   char filename[GRP__SZNAM + 1];
   const char *key = NULL;
   char obsid[SZFITSCARD];
   char obsidss[SZFITSCARD];
   char subsysnr[SZFITSCARD];
   int ifile;
   int indf1;
   int iobs;
   int isys;
   int nobs;
   int nscan;
   int nsys;

/* Initialise returned values. */
   *maxsyspop = 0;
   *conform = 1;
   *ogrp = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Create the returned KeyMap. */
   result = astKeyMap( " " );

/* Loop round all the input NDFs. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Get the path for this input NDF. */
      pname = filename;
      grpGet( igrp, ifile, 1, &pname, GRP__SZNAM, status );

/* See if it conforms to the expected form of an ACSIS raw time series
   file name (allowing optional arbitrary suffix and prefix). */
      match = astChrSub( pname, "a\\d{8}_\\d{5}_\\d{2}_\\d{4}_?", NULL, 0 );
      if( match ) {
         match = astFree( match );
      } else {
         *conform = 0;
      }

/* Get an NDF identifier for the input NDF. */
      ndgNdfas( igrp, ifile, "READ", &indf1, status );

/* Get a FitsChan holding the contents of the FITS extension. */
      kpgGtfts( indf1, &fc, status );

      /* Get the value of the OBSIDSS and OBSID keywords */
      (void)smf_getobsidss( fc, obsid, sizeof(obsid), obsidss,
                            sizeof(obsidss), status );

      /* and we also need SUBSYSNR as a string */
      if (astGetFitsS( fc, "SUBSYSNR", &cvalue )) {
          one_strlcpy( subsysnr, cvalue, sizeof(subsysnr), status );
      } else if (*status == SAI__OK) {
        *status = SAI__ERROR;
        errRep( " ", "Unable to get SUBSYSNR from FITS header", status);
      }

/* Get a pointer to the KeyMap holding information about this OBSID
   value. Create a new KeyMap and store a pointer for it in the returned
   KeyMap if this is the first time this OBSID value has been encountered. */
      if( !astMapGet0A( result, obsid, &obsmap ) ) {
         obsmap = astKeyMap( " " );
         astMapPut0A( result, obsid, obsmap, NULL );
      }

/* Get a pointer to the KeyMap holding information about this SUBSYSNR
   value. Create a new KeyMap and store a pointer for it in the returned
   KeyMap if this is the first time this SUBSYSNR value has been encountered. */
      if( !astMapGet0A( obsmap, subsysnr, &sysmap ) ) {
         sysmap = astKeyMap( " " );
         astMapPut0A( obsmap, subsysnr, sysmap, NULL );
      }

/* Get the value of the NSUBSCAN keyword. Use a null string if it was not
   found. Store the NDF index in the keymap. */
      if( astGetFitsS( fc, "NSUBSCAN", &nsubscan ) ) {
        astMapPut0I( sysmap, nsubscan, ifile, NULL );
      } else {
        astMapPut0I( sysmap, "", ifile, NULL );
      }

/* Free resources */
      sysmap = astAnnul( sysmap );
      obsmap = astAnnul( obsmap );
      fc = astAnnul( fc );
      ndfAnnul( &indf1, status );
   }

/* Create a new GRP group to hold the names of the first input NDF
   associated with each observation/sub-system. */
   *ogrp = grpNew( " ", status );

/* Now traverse the returned set of nested KeyMaps in their natural
   order. We use a separate pass through the KeyMaps (rather than storing
   the files names at the same time that the KeyMaps are created - above)
   so that we can ensure that the order in which the names are stored in
   the group matches that in which the KeyMaps will be accessed by the
   caller. */
   nobs = astMapSize( result );
   for( iobs = 0; iobs < nobs; iobs++ ) {
      key = astMapKey( result, iobs );
      astMapGet0A( result, key, &obsmap );

      nsys = astMapSize( obsmap );
      for( isys = 0; isys < nsys; isys++ ) {
         key = astMapKey( obsmap, isys );
         astMapGet0A( obsmap, key, &sysmap );


         key = astMapKey( sysmap, 0 );
         astMapGet0I( sysmap, key, &ifile );

         ndgCpsup( igrp, ifile, *ogrp, status );

/* Update the largest number of subscans for an observation/sub-system */
         nscan = astMapSize( sysmap );
         if( nscan > *maxsyspop ) *maxsyspop = nscan;

         sysmap = astAnnul( sysmap );
      }

      obsmap = astAnnul( obsmap );
   }

/* Return the result. */
   return result;
}
