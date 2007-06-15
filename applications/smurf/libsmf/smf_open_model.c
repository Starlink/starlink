/*
*+
*  Name:
*     smf_open_model

*  Purpose:
*     File access function for DIMM model component files

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_open_model( Grp *igrp, int index, char *mode, smfData **data,
*                     int *status ) {

*  Arguments:
*     ingrp = const Grp * (Given)
*        NDG group identifier
*     index = int (Given)
*        Index corresponding to required file in group
*     mode = char * (Given)
*        File access mode
*     data = smfData ** (Returned)
*        Pointer to pointer smfData struct to be filled with file info and data
*        Should be freed using smf_close_file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine replaces smf_open_file to read the simple binary format
*  of files used by the Dynamic Iterative Map Maker (DIMM) using non-starlink
*  memory mapping of the files to enable multi-threaded disk i/o (since
*  starlink calls are not thread-safe).

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-06-13 (EC):
*        Initial version.
*     2007-06-14(EC)
*        Move file information to smfFile from smfData, 
*        added file description and file name.

*  Notes:

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

/* System includes */
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_open_model"

void smf_open_model( Grp *igrp, int index, char *mode, smfData **data,
		     int *status ) {
  
  void *buf=NULL;               /* Pointer to total container buffer */
  size_t datalen=0;             /* Size of data buffer in bytes */
  double *dataptr=NULL;         /* Pointer to data portion of buffer */
  dim_t dims[NDF__MXDIM];       /* Dimensions of data array */
  smf_dtype dtype=SMF__NULL;    /* Type of data stored in component */
  int fd=0;                     /* File descriptor */
  size_t headlen=0;             /* Size of header in bytes */ 
  dim_t i;                      /* Loop counter */
  int mflags=0;                 /* bit flags for mmap */
  char name[GRP__SZNAM+1];      /* Name of container file */
  size_t ndata=0;               /* Number of elements in data array */
  int ndims;                    /* Number of dims in data array */
  int oflags=0;                 /* bit flags for open */
  char *pname=NULL;             /* Poiner to fname */

  if ( *status != SAI__OK ) return;

  /* Obtain a character string corresponding to the file name */
  pname = name;
  grpGet( igrp, index, 1, &pname, GRP__SZNAM, status );

  /* Open the template file */

  if( *status == SAI__OK ) {
    if( mode[0] == 'R' ) {
      oflags = O_RDONLY;
      mflags = PROT_READ;
    } else {
      oflags = O_RDWR;
      mflags = PROT_READ | PROT_WRITE;
    }

    if( (fd = open( name, oflags, S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH )) == -1 ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unable to open model container file", status ); 
    } else {
  
      /* Read the header */

      if( read( fd, &dtype, sizeof(dtype) ) == -1 ) {               /* dtype */
	*status = SAI__ERROR;
	errRep( FUNC_NAME, "Unable to read dtype from container header", 
		status ); 
      } else if( read( fd, &ndims, sizeof(ndims) ) == -1 ) {        /* ndims */
	*status = SAI__ERROR;
	errRep( FUNC_NAME, "Unable to read ndims from container header", 
		status ); 
      } else {
	if( ndims < 1 ) {
	  *status = SAI__ERROR;
	  errRep( FUNC_NAME, "Container header has bad dimensions", status ); 
	} else if( read( fd, dims, ndims*sizeof(dims[0]) ) == -1 ) { /* dims */
	  *status = SAI__ERROR;
	  errRep( FUNC_NAME, "Unable to read dims from container header", 
		  status ); 
	}
      }
    }
  }

  /* Calculate buffer sizes */

  if( *status == SAI__OK ) {
    ndata = 1;
    for( i=0; i<ndims; i++ ) {
      ndata *= dims[i];
    }
    
    datalen = ndata * smf_dtype_sz(dtype,status);
    headlen = sizeof(dtype) + sizeof(ndims) + ndims*sizeof(dims[0]);

    /* map the data */
    if( (buf = mmap( 0, datalen+headlen, mflags, MAP_SHARED, fd, 0 ) ) == 
	MAP_FAILED ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unable to map model container file", status ); 
    } else {
      dataptr = buf + headlen;
    }
  }

  /* Allocate memory for empty smfdata and fill relevant parts */
  *data = smf_create_smfData( SMF__NOCREATE_HEAD | SMF__NOCREATE_DA, status );

  if( *status == SAI__OK ) {
    /* Data type from header */
    (*data)->dtype = dtype;

    /* Data pointer points to memory AFTER HEADER */
    (*data)->pntr[0] = (void *) dataptr;

    /* Store pointer to entire mapped array for later un-mapping, as well
       as the length of the mapped buffer and file descriptor */
    (*data)->file->DIMMbuf = buf;
    (*data)->file->DIMMlen = datalen+headlen;
    (*data)->file->DIMMfd = fd;

    /* Copy the DIMM filename into the smfFile */
    strncpy( (*data)->file->name, name, SMF_PATH_MAX );

    /* Dimensions of data array */
    (*data)->ndims = ndims;
    for( i=0; i<ndims; i++ ) {
      (*data)->dims[i] = dims[i];
    }
  }
}
