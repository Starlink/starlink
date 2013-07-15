/*
*+
*  Name:
*     TILELIST

*  Purpose:
*     List the sky tiles that overlap a given set of raw data files or an
*     AST Region.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_tilelist( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns a list containing the indices of the sky tiles
*     (for a named JCMT instrument) that receive data from a given set of
*     raw data files, or that overlap a supplied AST Region.

*  ADAM Parameters:
*     REGION = LITERAL (Read)
*        The name of a text file containing the AST Region. If null (!)
*        is supplied, the IN parameter is used instead. [!]
*     IN = NDF (Read)
*        Input raw data files that define the required list of tiles.
*        ONly used if REGION is null (!).
*     INSTRUMENT = LITERAL (Read)
*        The JCMT instrument (different instruments have different
*        tiling schemes and pixel sizes). The following instrument
*        names are recognised (unambiguous abbreviations may be
*        supplied): "SCUBA-2(450)", "SCUBA-2(850)", "HARP", "RxA",
*        "RxWD", "RxWB". Only used if a value is supplied for REGION
*        (the instrument is determined from the data supplied for IN
*        otherwise).
*     TILES(*) = _INTEGER (Write)
*        An output parameter to which is written the list of integer tile
*        indices.

*  Related Applications:
*     SMURF: MAKECUBE, MAKEMAP, TILEINFO.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-MAR-2011 (DSB):
*        Original version.
*     12-JUL-2013 (DSB):
*        Added parameter IN.

*  Copyright:
*     Copyright (C) 2011,2013 Science and Technology Facilities Council.
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

/* System includes */
#include <stdlib.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"

#include "libsmf/tiles.h"   /* Move this to smf_typ.h and smf.h when done */


F77_SUBROUTINE(ast_isaregion)( INTEGER(THIS), INTEGER(STATUS) );
static int tilelist_icomp(const void *a, const void *b);


void smurf_tilelist( int *status ) {

/* Local Variables */
   AstObject *obj;
   AstRegion *region;
   Grp *igrp = NULL;
   Grp *sgrp = NULL;
   char text[ 200 ];
   int *tiles = NULL;
   int i;
   int ntile;
   size_t size;
   size_t ssize;
   smf_inst_t instrument;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Start a new AST context. */
   astBegin;

/* Attempto to get an AST Region (assumed to be in some 2D sky coordinate
   system). */
   kpg1Gtobj( "REGION", "Region",
              (void (*)( void )) F77_EXTERNAL_NAME(ast_isaregion),
              &obj, status );
   region = (AstRegion *) obj;

/* If successful, get the instrument name, and convert to an integer
   identifier. */
   if( *status == SAI__OK && region ) {

      parChoic( "INSTRUMENT", "SCUBA-2(450)", "SCUBA-2(450),SCUBA-2(850),"
                "HARP,RxA,RxWD,RxWB", 0, text, sizeof(text), status );

      if( !strcmp( text, "SCUBA-2(450)" ) ) {
         instrument = SMF__INST_SCUBA_2_450;

      } else if( !strcmp( text, "SCUBA-2(850)" ) ) {
         instrument = SMF__INST_SCUBA_2_850;

      } else if( !strcmp( text, "HARP" ) ) {
         instrument = SMF__INST_HARP;

      } else if( !strcmp( text, "RXA" ) ) {
         instrument = SMF__INST_RXA;

      } else if( !strcmp( text, "RxWD" ) ) {
         instrument = SMF__INST_RXWD;

      } else if( !strcmp( text, "RxWB" ) ) {
         instrument = SMF__INST_RXWB;

      } else {
         instrument = SMF__INST_NONE;
      }

/* Get the list of identifiers for tiles that overlap the region. */
      tiles = smf_skytiles_region( region, instrument, &ntile, status );

/* If no Region was supplied, annull the error and get a group of input
   data files. */
   } else if( *status == PAR__NULL ) {
      errAnnul( status );
      kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

/* Get a group containing just the files holding science data. */
      smf_find_science( igrp, &sgrp, 0, NULL, NULL, 1, 1, SMF__NULL, NULL,
                        NULL, NULL, NULL, status );

/* Check we have at least once science file. */
      ssize = grpGrpsz( sgrp, status );
      if( ssize == 0 ) {
         msgOutif( MSG__NORM, " ", "None of the supplied input frames were SCIENCE.",
                   status );

/* Get the list of identifiers for tiles that receive any data. */
      } else {
         tiles = smf_skytiles_data( sgrp, ssize, &ntile, status );
      }

/* Delete the groups. */
      if( igrp ) grpDelet( &igrp, status);
      if( sgrp ) grpDelet( &sgrp, status);
   }

/* Sort the list of overlapping tiles into ascending order. */
   if( *status == SAI__OK ) {
      qsort( tiles, ntile, sizeof( *tiles ), tilelist_icomp );

/* Display the list of overlapping tiles. */
      msgBlank( status );
      for( i = 0; i < ntile; i++ ) {
         msgSeti( "I", tiles[ i ] );
         msgOut( "", "   ^I", status );
      }
      msgBlank( status );

/* Write out the list of overlapping tiles to the output parameter. */
      parPut1i( "TILES", ntile, tiles, status );
   }

/* Free resources. */
   tiles = astFree( tiles );

/* End the AST context. */
   astEnd;

/* Issue a status indication.*/
   msgBlank( status );
   if( *status == SAI__OK ) {
      msgOutif( MSG__VERB, "", "TILELIST succeeded.", status);
   } else {
      msgOutif( MSG__VERB, "", "TILELIST failed.", status);
   }
}






/* Service function for "qsort", called in smurf_tilelist. It sorts
   integers into increasing order. */
static int tilelist_icomp(const void *a, const void *b){
   return *((const int *) a) - *((const int *) b);
}
