 /*
*+
*  Name:
*     smf_write_bolotable

*  Purpose:
*     Write a single bolometer time-stream to an ASCII formatted table

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_write_bolotable( const smfData *data, const char *filename,
*                          dim_t ibolo, const dim_t *lbnd, const dim_t *dims,
*                          int *status )

*  Arguments:
*     data = smfData * (Given)
*        Pointer to smfData to dump. Returns without action if NULL pointer.
*     filename = const char * (Given)
*        Name of output text file.
*     ibolo = dim_t (Given)
*        Zero-based index of bolometer to dump.
*     lbnd = const dim_t * (Given):
*        Lower pixel index bounds of output map.
*     dims = const dim_t * (Given):
*        Dimensions of output map, in pixels.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     A text file is created with the given name containing a table of
*     values in TOPCAT "ascii" format. Each row represents a single
*     time-slice in the supplied smfData. The columns are:
*
*     ITIME: The time slice index
*     RTS_NUM: The RTS_NUM value of the time slice
*     IX: The X pixel index of the map pixel into which the bolometer sample falls.
*     IY: The Y pixel index of the map pixel into which the bolometer sample falls.
*     DATA: The Data value of the bolometer sample.
*     VAR: The Variance of the bolometer sample (if available).
*     QUAL: The Quality of the bolometer sample (if available).

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     17-MAR-2021 (DSB):
*        Initial Version

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory.
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

*-
*/

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"
#include "star/one.h"

/* SMURF includes */
#include "libsmf/smf.h"


void smf_write_bolotable( const smfData *data, const char *filename,
                          dim_t ibolo, const dim_t *lbnd, const dim_t *dims,
                          int *status ){

/* Local Variables */
   FILE *fd = NULL;
   JCMTState *state;
   char buf[200];
   const smf_qual_t *qual;
   dim_t bstride;
   dim_t itime;
   dim_t ix;
   dim_t iy;
   dim_t nbolo;
   dim_t nc;
   dim_t ntslice;
   dim_t tstride;
   double *dat;
   double *var;
   int *lut;
   smfHead *hdr;
   smf_qfam_t qfamily = SMF__QFAM_NULL;

/* Check inheritd status */
   if( *status != SAI__OK || !data ) return;

/* Check the smfData is 3-dimensional. */
   if( data->ndims != 3 ) {
      *status = SAI__ERROR;
      errRep(" ", "smf_write_bolotable: supplied smfData is not "
             "3-dimensional.", status );
      return;
   }

/* Get pointers to the required data and lut arrays, and header. */
   dat = (data->pntr)[0];
   if( !dat ) {
      *status = SAI__ERROR;
      errRep(" ", "smf_write_bolotable: supplied smfData has no data "
             "array.", status );
      return;
   }

   lut = data->lut;
   if( !lut ) {
      *status = SAI__ERROR;
      errRep(" ", "smf_write_bolotable: supplied smfData has no LUT "
             "array.", status );
      return;
   }

   hdr = data->hdr;
   if( !hdr ) {
      *status = SAI__ERROR;
      errRep(" ", "smf_write_bolotable: supplied smfData has header.",
             status );
      return;
   }

/* Open the text file. */
   fd = fopen( filename, "w" );
   if( !fd && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "smf_write_bolotable: failed to open output "
               "file '%s'.", status, filename );
      return;
   }

/* Get pointers to the optional variance and quality arrays. */
   var = (data->pntr)[1];
   qual = smf_select_cqualpntr( data, &qfamily, status );

/* Get the smfData dimensions and strides. */
   smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

/* Check the supplied bolometer index */
   if( ibolo >= nbolo && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "smf_write_bolotable: supplied bolometer index "
               "(%zu) is out of bounds.", status, ibolo );
   }

/* Write the output file header line. */
   fprintf( fd, "# IBOLO = %d\n", (int) ibolo );
   nc = sprintf( buf, "# ITIME RTS_NUM IX IY DATA" );
   if( var ) nc += sprintf( buf + nc, " VAR" );
   if( qual ) nc += sprintf( buf + nc, " QUAL" );
   fprintf( fd, "%s\n", buf );

/* Initialise pointers to the bolometer's values for the first time
   slice. */
   dat += ibolo*bstride;
   lut += ibolo*bstride;
   if( var ) var += ibolo*bstride;
   if( qual ) qual += ibolo*bstride;

/* Get a pointer to the JCMTSTATE for the first time slice. */
   state = hdr->allState;

/* Loop round all time slices. */
   for( itime = 0; itime < ntslice; itime++,state++ ){

/* Write the time slice index and RTS_NUM value to the line buffer. */
      nc = sprintf( buf, "%ld %u", itime, state->rts_num );

/* Append the pixel indices of the pixel that recieves the bolometer
   sample, if known. */
      if( *lut != VAL__BADI ){
         iy = *lut/dims[0];
         ix = *lut - iy*dims[0];
         ix += lbnd[ 0 ];
         iy += lbnd[ 1 ];
         nc += sprintf( buf + nc, " %d %d", (int) ix, (int) iy );
      } else {
         nc += sprintf( buf + nc, " null null" );
      }

/* Append the data value, if known. */
      if( *dat != VAL__BADD ){
         nc += sprintf( buf + nc, " %g", *dat );
      } else {
         nc += sprintf( buf + nc, " null" );
      }

/* If required and known, append the variance value. */
      if( var ){
         if( *var != VAL__BADD ){
            nc += sprintf( buf + nc, " %g", *var );
         } else {
            nc += sprintf( buf + nc, " null" );
         }
      }

/* If required, append the quality value. */
      if( qual ) nc += sprintf( buf + nc, " %d", (int) *qual );

/* Write out the line buffer to the output file. */
      fprintf( fd, "%s\n", buf );

/* Increment pointers for the next time slice. */
      dat += tstride;
      lut += tstride;
      if( var ) var += tstride;
      if( qual ) qual += tstride;
   }

/* Close the output file. */
   if( fd ) fclose( fd );
   if( *status == SAI__OK ) msgOutf( " ", "Diagnostics: created file '%s'.",
                                     status, filename );
}

