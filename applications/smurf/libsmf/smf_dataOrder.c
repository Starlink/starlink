/*
*+
*  Name:
*     smf_dataOrder

*  Purpose:
*     Set the data/variance/quality array order for a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_dataOrder( smfData *data, int isTordered, *status ); 

*  Arguments:
*     data = smfData* (Given and Returned)
*        Group of input data files
*     isTordered = int (Given)
*        If 0, ensure data is ordered by bolometer. If 1 ensure data is
*        ordered by time slice (default ICD ordering)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function is used to change the ordering of
*     DATA/VARIANCE/QUALITY arrays associated with a smfData. Normally
*     SCUBA-2 data is stored as time-ordered data; each 40x32 element
*     chunk of contiguous memory contains bolometer data from the same
*     time slice. This array ordering is impractical for time-domain
*     operations such as filtering. In these cases it is preferable to
*     have all of the data from a single bolometer stored in a
*     contiguous chunk of memory (bolometer ordered).  Use this
*     function to change between the two ordering schemes. Note that
*     this routine first checks the current array order before doing
*     anything; if the requested array order matches the current order
*     it simply returns. If the smfData was memory mapped then the
*     routine changes the data order in-place (slightly
*     slower). Otherwise a new buffer is allocated with the re-ordered
*     data, and the old buffer is free'd. If flags set to SMF__NOCREATE_FILE
*     and a file is associated with the data, don't write anything 
*     (workaround for cases where it was opened read-only).

*
*  Authors:
*     EC: Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-09-13 (EC):
*        Initial version.
*     2007-10-31 (EC):
*        - changed index multiplications to successive additions in tight loops
*        - re-order pointing LUT if it exists
*     2007-11-28 (EC):
*        Update FITS keyword TORDERED when data order is modified
*     2007-12-14 (EC):
*        - fixed LUT re-ordering pointer bug
*        - extra file existence checking if writing TORDERED FITS entry
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-01-25 (EC):
*        -removed setting of TORDERED keyword from FITS header
*        -always set data dimensions (moved out of loop)
*     2008-02-08 (EC):
*        -Fixed dtype for QUALITY -- SMF__UBYTE instead of SMF__USHORT

*  Notes:
*     Nothing is done about the FITS channels or WCS information stored in
*     the header, so anything that depends on them will get confused by
*     bolo-ordered data produced with this routine.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */

#define FUNC_NAME "smf_dataOrder"

void smf_dataOrder( smfData *data, int isTordered, int *status ) {

  /* Local Variables */
  dim_t bolobase;               /* base index for time-ordered array offsets */
  smf_dtype dtype;              /* Data array type */
  dim_t i;                      /* loop counter */
  int inPlace=0;                /* If set change array in-place */
  dim_t j;                      /* loop counter */  
  dim_t k;                      /* loop counter */
  void *oldbuf=NULL;            /* Pointer to old buffer */
  int *oldlut=NULL;             /* Pointer to pointing LUT */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  void *newbuf=NULL;            /* Pointer to new buffer */
  dim_t newdims[3];             /* Size of each dimension new buffer */ 
  int *newlut=NULL;             /* Pointer to pointing LUT */
  dim_t ntslice;                /* Number of time slices */
  dim_t timebase;               /* base index for time-ordered array offsets */
  size_t sz=0;                  /* Size of element of data array */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check for valid isTordered */
  if( (isTordered != 0) && (isTordered != 1) ) {
    *status = SAI__ERROR;
    msgSeti("ISTORDERED",isTordered);
    errRep(FUNC_NAME, "Invalid isTordered (0/1): ^ISTORDERED", status);
    return;
  }

  /* Check for a valid data */
  if( !data ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "NULL data supplied", status);
    return;
  }

  /* If value of isTordered matches current value in smfData return */
  if( data->isTordered == isTordered ) return;
  
  /* Make sure we're looking at 3-dimensions of bolo data */
  if( data->ndims != 3 ) {
    *status = SMF__WDIM;
    msgSeti("NDIMS",data->ndims);
    errRep(FUNC_NAME, 
           "Don't know how to handle ^NDIMS dimensions, should be 3.", status);
    return;
  }

  /* inPlace=1 if smfData was mapped! */
  if( data->file ) {
    if( data->file->fd || data->file->ndfid ) {
      inPlace = 1;
    }
  } 

  /* Calculate data dimensions */
  if( data->isTordered ) {
    nbolo = (data->dims)[0]*(data->dims)[1];
    ntslice = (data->dims)[2];
    newdims[0] = (data->dims)[2];
    newdims[1] = (data->dims)[0];
    newdims[2] = (data->dims)[1];
  } else {
    ntslice = (data->dims)[0];
    nbolo = (data->dims)[1]*(data->dims)[2];
    newdims[0] = (data->dims)[1];
    newdims[1] = (data->dims)[2];
    newdims[2] = (data->dims)[0];
  }
  ndata = nbolo*ntslice;

  /* Loop over elements of data->ptr and re-form arrays */
  for( i=0; i<3; i++ ) if( data->pntr[i] ) {

      /* Pointer to component we're looking at */
      oldbuf = data->pntr[i];

      if( i==2 ) {     /* If QUALITY unsigned char */
        dtype = SMF__UBYTE;
      } else {         /* Otherwise get type from the smfData */ 
        dtype = data->dtype;      
      }

      /* Size of data type */
      sz = smf_dtype_sz(dtype, status);

      /* Allocate buffer */
      newbuf = smf_malloc( ndata, sz, 0, status );

      if( *status == SAI__OK ) {

        timebase = 0;
        if( isTordered ) { /* Converting bolo ordered -> time ordered */
	
          /* Switch on the data type outside the main loops */	
          switch( dtype ) {
          case SMF__INTEGER:
            for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
              bolobase = 0;
              for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
                ((int *)newbuf)[timebase+k] = ((int *)oldbuf)[bolobase+j];
                bolobase += ntslice;
              }
              timebase += nbolo;
            }
            break;
	  
          case SMF__FLOAT:
            for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
              bolobase = 0;
              for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
                ((float *)newbuf)[timebase+k] = ((float *)oldbuf)[bolobase+j];
                bolobase += ntslice;
              }
              timebase += nbolo;
            }
            break;
	  
          case SMF__DOUBLE:
            for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
              bolobase = 0;
              for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
                ((double *)newbuf)[timebase+k] = ((double *)oldbuf)[bolobase+j];
                bolobase += ntslice;
              }
              timebase += nbolo;
            }
            break;
	  
          case SMF__UBYTE:
            for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
              bolobase = 0;
              for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
                ((unsigned char *)newbuf)[timebase+k] = 
                  ((unsigned char *)oldbuf)[bolobase+j];
                bolobase += ntslice;
              }
              timebase += nbolo;
            }
            break;

          default:
            msgSetc("DTYPE",smf_dtype_string(data, status));
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Don't know how to handle ^DTYPE type.", status);
          }

        } else {          /* Converting time ordered -> bolo ordered */

          /* Switch on the data type outside the main loops */
	
          switch( dtype ) {
          case SMF__INTEGER:
            for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
              bolobase = 0;
              for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
                ((int *)newbuf)[bolobase+j] = ((int *)oldbuf)[timebase+k];
                bolobase += ntslice;
              }
              timebase += nbolo;
            }
            break;

          case SMF__FLOAT:
            for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
              bolobase = 0;
              for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
                ((float *)newbuf)[bolobase+j] = ((float *)oldbuf)[timebase+k];
                bolobase += ntslice;
              }
              timebase += nbolo;
            }
            break;

          case SMF__DOUBLE:
            for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
              bolobase = 0;
              for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
                ((double *)newbuf)[bolobase+j] = ((double *)oldbuf)[timebase+k];
                bolobase += ntslice;
              }
              timebase += nbolo;
            }
            break;

          case SMF__UBYTE:
            for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
              bolobase = 0;
              for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
                ((unsigned char *)newbuf)[bolobase+j] = 
                  ((unsigned char *)oldbuf)[timebase+k];
                bolobase += ntslice;
              }
              timebase += nbolo;
            }
            break;

          default:
            msgSetc("DTYPE",smf_dtype_string(data, status));
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Don't know how to handle ^DTYPE type.", status);
          }
        }

        if( inPlace ) {
          /* Copy newbuf to oldbuf */

          memcpy( oldbuf, newbuf, ndata*sz );

          /* Free newbuf */
          newbuf = smf_free( newbuf, status );

        } else {

          /* Free oldbuf */
          oldbuf = smf_free( oldbuf, status );

          /* Set pntr to newbuf */
          data->pntr[i] = (void *) newbuf;
        }
      }
    }

  /* If NDF associated with data, modify dimensions of the data */
  if( data->file ) {
    if( data->file->ndfid != NDF__NOID ) {
      fprintf(stderr,
              "Do not yet handle changing dimensions of NDF on reorder\n");
    }
  }

  /* Check for the existence of a pointing LUT in the MAPCOORD extension */
  oldlut = data->lut;
  if( oldlut == NULL ) { 
    /* If there is a MAPCOORD xtension it's not open. Try opening here. */
    smf_open_mapcoord( data, "UPDATE", status );
    
    if( *status == SAI__OK ) {
      /* Good status so the LUT is now mapped */
      oldlut = data->lut;
    } else if( *status == SMF__NOLUT ) {
      /* There is no extension to map. Annul the error and continue. */
      errAnnul( status );
    }
  }

  /* If there is a mapped LUT re-order it here */
  if( oldlut ) {

    newlut = smf_malloc( ndata, sizeof(*newlut), 0, status );

    if( *status == SAI__OK ) {
      if( isTordered ) { /* Converting bolo ordered -> time ordered */
        timebase = 0;
        for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
          bolobase = 0;
          for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
            ((int *)newlut)[timebase+k] = ((int *)oldlut)[bolobase+j];
            bolobase += ntslice;
          }
          timebase += nbolo; 
        }
      } else {           /* Converting time ordered -> bolo ordered */
        timebase = 0;
        for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
          bolobase = 0;
          for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
            ((int *)newlut)[bolobase+j] = ((int *)oldlut)[timebase+k];
            bolobase += ntslice;
          }
          timebase += nbolo; 
        }
      }

      if( inPlace ) {
        /* Copy newlut to oldlut */
        memcpy( oldlut, newlut, ndata*sizeof(*newlut) );
	
        /* Free newlut */
        newlut = smf_free( newlut, status );
	
      } else {
	
        /* Free oldlut */
        oldlut = smf_free( oldlut, status );

        /* Set pntr to newbuf */
        data->lut = newlut;
      }
    }
  }

  /* Set the new dimensions in the smfData */
  if( *status == SAI__OK ) {
    memcpy( data->dims, newdims, 3*sizeof(*newdims) );
    data->isTordered = isTordered;
  }

}
