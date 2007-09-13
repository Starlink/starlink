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
*
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
*     data, and the old buffer is free'd.

*
*  Authors:
*     EC: Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-09-13 (EC):
*        Initial version.

*  Notes:

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

/* Other includes */

#define FUNC_NAME "smf_dataOrder"

void smf_dataOrder( smfData *data, int isTordered, int *status ) {

  /* Local Variables */
  dim_t base;                   /* Store base index for array offsets */
  size_t dsize;                 /* Size of data type in bytes */
  smf_dtype dtype;              /* Data array type */
  dim_t i;                      /* loop counter */
  int inPlace=0;                /* If set change array in-place */
  dim_t j;                      /* loop counter */  
  dim_t k;                      /* loop counter */
  void *oldbuf=NULL;            /* Pointer to old buffer */
  void *newbuf=NULL;            /* Pointer to new buffer */
  dim_t newdims[3];             /* Size of each dimension new buffer */ 
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */
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

  /* If value of isTordered matches current value in smfData return */
  if( data->isTordered == isTordered ) return;
  
  /* Make sure we're looking at 3-dimensions of bolo data */
  if( data->ndims != 3 ) {
    *status = SAI__ERROR;
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
      dtype = SMF__USHORT;
    } else {         /* Otherwise get type from the smfData */ 
      dtype = data->dtype;      
    }

    /* Size of data type */
    sz = smf_dtype_sz(dtype, status);

    /* Allocate buffer */
    newbuf = smf_malloc( ndata, sz, 0, status );

    if( *status == SAI__OK ) {

      if( isTordered ) { /* Converting bolo ordered -> time ordered */
	
	/* Switch on the data type outside the main loops */
	
	switch( dtype ) {
	case SMF__INTEGER:
	  for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
	    base = j*nbolo; /* base index in time-ordered array */
	    for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
	      ((int *)newbuf)[base+k] = ((int *)oldbuf)[k*ntslice+j];
	    }
	  }
	  break;
	  
	case SMF__FLOAT:
	  for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
	    base = j*nbolo; /* base index in time-ordered array */
	    for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
	      ((float *)newbuf)[base+k] = ((float *)oldbuf)[k*ntslice+j];
	    }
	  }
	  break;
	  
	case SMF__DOUBLE:
	  for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
	    base = j*nbolo; /* base index in time-ordered array */
	    for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
	      ((double *)newbuf)[base+k] = ((double *)oldbuf)[k*ntslice+j];
	    }
	  }
	  break;
	  
	case SMF__USHORT:
	  for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
	    base = j*nbolo; /* base index in time-ordered array */
	    for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
	      ((short *)newbuf)[base+k] = ((short *)oldbuf)[k*ntslice+j];
	    }
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
	    base = j*nbolo; /* base index in time-ordered array */
	    for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
	      ((int *)newbuf)[k*ntslice+j] = ((int *)oldbuf)[base+k];
	    }
	  }
	  break;

	case SMF__FLOAT:
	  for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
	    base = j*nbolo; /* base index in time-ordered array */
	    for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
	      ((float *)newbuf)[k*ntslice+j] = ((float *)oldbuf)[base+k];
	    }
	  }
	  break;

	case SMF__DOUBLE:
	  for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
	    base = j*nbolo; /* base index in time-ordered array */
	    for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
	      ((double *)newbuf)[k*ntslice+j] = ((double *)oldbuf)[base+k];
	    }
	  }
	  break;

	case SMF__USHORT:
	  for( j=0; j<ntslice; j++ ) { /* Loop over tslice */
	    base = j*nbolo; /* base index in time-ordered array */
	    for( k=0; k<nbolo; k++ ) { /* Loop over bolo */
	      ((short *)newbuf)[k*ntslice+j] = ((short *)oldbuf)[base+k];
	    }
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
	smf_free( newbuf, status );

      } else {

	/* Free oldbuf */
	smf_free( oldbuf, status );

	/* Set pntr to newbuf */
	data->pntr[i] = (void *) newbuf;
      }

      /* Set the new dimensions in the smfData */
      if( *status == SAI__OK ) {
	memcpy( data->dims, newdims, 3*sizeof(*newdims) );
	data->isTordered = isTordered;
      }
    }
  }
}
