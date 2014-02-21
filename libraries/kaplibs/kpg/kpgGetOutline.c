#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/hds.h"
#include "ndf.h"
#include "ast.h"

/* Module variables */
static HDSLoc *xloc;
static hdsdim iline;
static char *buff;
static size_t bufflen;
static int nline;

/* Prototypes for private functions. */
static const char *mysource( void );

/* The name of the NDF extension in which the STC-S polygon is stored. */
#define XNAME "OUTLINE"

AstRegion *kpgGetOutline( int indf, int *status ){
/*
*+
*  Name:
*     kpgGetOutline

*  Purpose:
*     Retrieve an STC polygon describing the spatial extent of an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     AstRegion *kpgGetOutline( int indf, int *status )

*  Arguments:
*     indf = int (Given)
*        Identifier for the NDF.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     If The NDF contains an OUTLINE extension, it is expected to be a
*     character array copntaining an STC-S description of a polygon. If
*     this is the case, the polygon is returned as the function value.
*     Otherwise a NULL pointer is returned.

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*    21-FEB-2014 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2014 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


/* Local Variables: */
   AstRegion *result = NULL;
   AstStcsChan *chan;
   char xtype[DAT__SZTYP + 1];
   hdsdim dims[ DAT__MXDIM ];
   int actdim;
   int there;

/* Check the inherited status */
   if (*status != SAI__OK) return result;

/* See if the "OUTLINE" extension exists in the NDF, and if so, get a
   locator to it. */
   ndfXstat( indf, XNAME, &there, status );
   if( there ) {
      ndfXloc( indf, XNAME, "Read", &xloc, status );

/* Check the extension is a character array. */
      datType( xloc, xtype, status );
      if( !strncmp( xtype, "_CHAR", 4 ) ) {

/* Check it is a vector and get its length. */
         datShape( xloc, DAT__MXDIM, dims, &actdim, status );
         if( actdim == 1 ) {
            nline = dims[ 0 ];

/* Get the length of each string, and allocate a buffer to hold one string. */
            datClen( xloc, &bufflen, status );
            bufflen += 20;
            buff = astMalloc( bufflen );

/* Create an StcsChan that can be used to convert the STC-S polygon
   description into an AST Polgon. */
            chan = astStcsChan( mysource, NULL, " " );

/* Do the conversion. */
            iline = 0;
            result = astRead( chan );

/* Free resources. */
            chan = astAnnul( chan );
            buff = astFree( buff );
         }
      }

/* Free resources. */
      datAnnul( &xloc, status );
   }

/* Return the Polygon pointer. */
   return result;
}


/* Function to read an element from the array of text strings stored in
   the NDF extension and return it to the StcsChan class. */
static const char *mysource( void ){
   int status = SAI__OK;
   HDSLoc *cloc = NULL;
   size_t nc;

   if( iline < nline ) {
      iline++;
      datCell( xloc, 1, &iline, &cloc, &status );
      datGet0C( cloc, buff, bufflen, &status );
      datAnnul( &cloc, &status );

/* Ensure the string ends with a space (we over-allocated buff so that we
   could do this safely). */
      nc = strlen( buff );
      buff[ nc ] = ' ';
      buff[ nc + 1 ] = 0;

      if( status == SAI__OK ) printf("!! %s\n", buff);


      return ( status == SAI__OK ) ? buff : NULL;
   } else {
      return NULL;
   }
}
