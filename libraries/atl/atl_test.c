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
   AstFitsChan *fc;
   AstFrame *cfrm;
   AstFrameSet *fs;
   AstFrameSet *fs2;
   double work[2700];
   int axes[2], lbnd[2], ubnd[2];
   int status;

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


/* Create a FitsChan and put a basic spectral cube header in it. */
   fc = astFitsChan( NULL, NULL, " " );
   astPutFits( fc, "NAXIS   =                    3", 0 );
   astPutFits( fc, "CTYPE1  = 'VELO-LSR'          ", 0 );
   astPutFits( fc, "CRVAL1  =                9000.", 0 );
   astPutFits( fc, "CRPIX1  =                450.5", 0 );
   astPutFits( fc, "CDELT1  =    -232.827120536449", 0 );
   astPutFits( fc, "CTYPE2  = 'GLON--GLS'         ", 0 );
   astPutFits( fc, "CRVAL2  =     208.992297968235", 0 );
   astPutFits( fc, "CRPIX2  =                    0", 0 );
   astPutFits( fc, "CDELT2  =               0.0125", 0 );
   astPutFits( fc, "CTYPE3  = 'GLAT--GLS'         ", 0 );
   astPutFits( fc, "CRVAL3  =    -19.3843855749591", 0 );
   astPutFits( fc, "CRPIX3  =                    0", 0 );
   astPutFits( fc, "CDELT3  =               0.0125", 0 );

/* Read a FrameSet from it. */
   astClear( fc, "Card" );
   fs = astRead( fc );

/* Split this FrameSet to extract the sky axes. */
   fs2 = atlFrameSetSplit( fs, "SKY", NULL, NULL, &status );
   if( astOK && !fs2 ) {
      status = SAI__ERROR;
      errRep( "", "Error 2; atlFrameSetSplit failed.", &status );
   }
   if( astOK && ( astGetI( fs2, "Nin" ) != 2 || astGetI( fs2, "Nout" ) != 2 ) ){
      status = SAI__ERROR;
      errRep( "", "Error 3; atlFrameSetSplit failed.", &status );
   }
   if( astOK && !astIsASkyFrame( astGetFrame( fs2, AST__CURRENT ) ) ){
      status = SAI__ERROR;
      errRep( "", "Error 4; atlFrameSetSplit failed.", &status );
   }

/* Split this FrameSet to extract the spectral axes. */
   fs2 = atlFrameSetSplit( fs, "SPECTRUM", NULL, NULL, &status );
   if( astOK && !fs2 ) {
      status = SAI__ERROR;
      errRep( "", "Error 5; atlFrameSetSplit failed.", &status );
   }
   if( astOK && ( astGetI( fs2, "Nin" ) != 1 || astGetI( fs2, "Nout" ) != 1 ) ){
      status = SAI__ERROR;
      errRep( "", "Error 6; atlFrameSetSplit failed.", &status );
   }
   if( astOK && !astIsASpecFrame( astGetFrame( fs2, AST__CURRENT ) ) ){
      status = SAI__ERROR;
      errRep( "", "Error 7; atlFrameSetSplit failed.", &status );
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



