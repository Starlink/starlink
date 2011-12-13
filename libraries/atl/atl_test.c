/*
*+
*  Name:
*     ATL_TEST

*  Purpose:
*     Test installation of the stand-alone ATL package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     PROGRAM

*  Invocation:
*     RUN ATL_TEST

*  Description:
*     This program tests the installation of the stand-alone ATL
*     package. Note, it is not an exhaustive test of the ATL_ system
*     itself.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-MAY-2006 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include <string.h>
#include "star/atl.h"
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include <stdio.h>
#define LEN 200

static FILE *input_stream;

const char *Source( void );


int main( void ){
   AstChannel *channel;
   AstFrameSet *fs;
   AstFrame *cfrm;
   int status;
   int axes[2], lbnd[2], ubnd[2];
   double work[2700];

/* Initialise the global status */
   status = SAI__OK;
   astWatch( &status );

/* Open the input file. */
   input_stream = fopen( "chanmap.ast", "r" );

/* Create a Channel and read an Object from it. */
   channel = astChannel( Source, NULL, "" );
   fs = (AstFrameSet *) astRead( channel );

/* Annul the Channel and close the file when done. */
   channel = astAnnul( channel );
   (void) fclose( input_stream );

/* Attemp to trim off the third axis from the 3D current Frame. */
   axes[ 0 ] = 1;
   axes[ 1 ] = 2;
   lbnd[ 0 ] = 1;
   ubnd[ 0 ] = 771;
   lbnd[ 1 ] = 1;
   ubnd[ 1 ] = 1314;
   atlAxtrm( fs, axes, lbnd, ubnd, work, &status );

/* Check the current Frame is a SkyFrame */
   cfrm = astGetFrame( fs, AST__CURRENT );
   if( !astIsASkyFrame( cfrm ) ) {
      if( status == SAI__OK ) {
         status = SAI__ERROR;
         errRep( "", "Error 1; current Frame is not a SkyFrame.", &status );
      }
   }

/* Check its Domain. */
   if( astOK && strcmp( "SKY-ROI1", astGetC( cfrm, "Domain" ) ) ){
      status = SAI__ERROR;
      errRep( "", "Error 2; Incorrect Domain for SkyFrame.", &status );
   }

/* If an error occurred, then report a contextual message. */
   if( status != SAI__OK ) {
      errRep( "", "ATL_TEST: ATL_ installation test failed.", &status );
   } else {
      msgOut( "", "ATL_TEST: ATL_ installation test passed.", &status );
   }

   return status;
}


const char *Source( void ) {
   static char buffer[ LEN + 2 ];
   return fgets( buffer, LEN + 2, input_stream );
}



