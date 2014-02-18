/*
*+
*  Name:
*     smf_export_noi

*  Purpose:
*     Export the NOI model to an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_export_noi( smfData *noi, const char *name, int boxsize,
*                          int *status )

*  Arguments:
*     noi = smfData * (Given)
*        The NOI model.
*     name = const char * (Given)
*        The name of the NDF to create.
*     boxsize = int (Given)
*        The number of adjacent times slices that have the same noise value.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function exports the compressed NOI model to a new NDF. It can
*     be imported again via function smf_import_noi.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-SEP-2013 (DSB):
*        Original version.
*     24-JAN-2014 (DSB):
*        Put the time axis first in the output NDF in order to avoid
*        unnecessary re-ordering of the values when it is read in by the
*        next invocation of makemap.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/hds.h"
#include "ast.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_export_noi( smfData *noi, const char *name, int boxsize, int *status ){

/* Local Variables */
   HDSLoc *xloc = NULL;
   dim_t ntslice;
   dim_t nrows;
   dim_t ncols;
   dim_t nbolo;
   double *ip;
   double *dataptr;
   double *dp;
   double *pd;
   int el;
   int ibolo;
   int indf;
   int itime;
   int lbnd[ 3 ];
   int nz;
   int place;
   int ubnd[ 3 ];
   size_t bstride;
   size_t tstride;
   int iz;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Report an error if the number of time slices that share a single NOI
   value is not known. */
   if( boxsize == 0 ) {
      *status = SAI__ERROR;
      errRep( "", "smf_export_noi: noise boxsize is not yet known "
              "(programming error).", status );
      return;
   }

/* Get the dimensions of the NOI model. */
   smf_get_dims( noi, &nrows, &ncols, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

/* Determine the number of boxes to use. This is the length of 3rd axis.
   of the new NDF. */
   if( ntslice == 1 ) {
      nz = 1;
   } else {
      nz = ntslice / boxsize;
   }

   if( nz <= 0 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf("", "smf_export_noi: boxsize (%d) and ntslice (%d) are "
              "inconsistent (programming error).", status, boxsize,
              (int) ntslice );
   }

/* Get a pointer to the NOI data values. */
   dataptr = noi->pntr[ 0 ];

/* Create the NDF. Use the axis ordering needed by smf_model_create. This
   avoid re-ordering the values every time it is read in. */
   ndfPlace( NULL, name, &place, status );
   lbnd[ 0 ] = 1;
   lbnd[ 1 ] = 1;
   lbnd[ 2 ] = 1;
   ubnd[ 0 ] = nz;
   ubnd[ 1 ] = ncols;
   ubnd[ 2 ] = nrows;
   ndfNew( "_DOUBLE", 3, lbnd, ubnd, &place, &indf, status );

/* Map the Data array of the NDF and copy the NOI values into it. */
   ndfMap( indf, "DATA", "_DOUBLE", "WRITE", (void **) &ip, &el, status );
   if( *status == SAI__OK ) {

/* Initialise the time slice at the middle of the current box in the model. */
      itime = ( nz == 1 ) ? 0 : boxsize/2;

/* We step sequentially through teh NDF pixels. Initialise a pointer to
   the first pixel value. */
      pd = ip;

/* Loop round each bolometer. */
      for( ibolo = 0; ibolo < (int) nbolo; ibolo++ ) {

/* Get a pointer to the noise value for the current bolometer at the
   centre of the first box. */
         dp = dataptr + ibolo*bstride + itime*tstride;

/* Now loop round each box of time slices. */
         for( iz = 0; iz < nz; iz++ ) {

/* The NOI model is filled with 1.0 values by smf_model_create, and can also
   be set to zero to indicate missing values. Therefore convert both these
   values into VAL__BADD. */
            *(pd++) = (*dp == 0.0 || *dp == 1.0) ? VAL__BADD : *dp;

/* Move the input pointer on to the next box. */
            dp += boxsize*tstride;
         }
      }

/* Store the box size as an extension item in the NDF. */
      ndfXnew( indf, SMURF__EXTNAME, SMURF__EXTTYPE, 0, NULL, &xloc, status );
      ndfXpt0i( boxsize, indf, SMURF__EXTNAME, "NOI_BOXSIZE", status );
      datAnnul( &xloc, status );
   }

/* Annul the NDF identifier. */
   ndfAnnul( &indf, status );
}

