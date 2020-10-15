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
*     smf_open_model( Grp *igrp, dim_t index, const char *mode, smfData **data,
*                     int *status ) {

*  Arguments:
*     ingrp = const Grp * (Given)
*        NDG group identifier
*     index = dim_t (Given)
*        Index corresponding to required file in group
*     mode = const char * (Given)
*        File access mode. "R" for read-only otherwise assumed to be read/write
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
*     Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2007-06-13 (EC):
*        Initial version.
*     2007-06-14 (EC):
*        Move file information to smfFile from smfData,
*        added file descriptor and file name.
*     2007-06-25 (EC):
*        Header length is now static / padded to multiple of pagesize
*     2007-08-20 (TIMJ):
*        Use const char * mode
*     2007-12-14 (EC):
*        -DIMM files now leave both header + data mapped
*        -Include isTordered flag stored in DIMM files
*     2010-03-16 (TIMJ):
*        Use one_strlcpy instead of strncpy.
*     2010-09-21 (COBA):
*        Add SMF__NOCREATE_FTS

*  Notes:

*  Copyright:
*     Copyright (C) 2007, 2010 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 University of British Columbia.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
#include "star/one.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_open_model"

void smf_open_model( const Grp *igrp, dim_t index, const char *mode,
                     smfData **data, int *status ) {

  void *buf=NULL;               /* Pointer to total container buffer */
  dim_t buflen = 0;             /* datalen + headlen */
  dim_t datalen=0;              /* Size of data buffer in bytes */
  int fd=0;                     /* File descriptor */
  smfDIMMHead head;             /* Header for the file */
  dim_t headlen=0;              /* Size of header in bytes */
  int mflags=0;                 /* bit flags for mmap */
  char name[GRP__SZNAM+1];      /* Name of container file */
  int oflags=0;                 /* bit flags for open */
  char *pname=NULL;             /* Poiner to fname */

  if ( *status != SAI__OK ) return;

  /* Obtain the file name */
  pname = name;
  grpGet( igrp, (int) index, 1, &pname, GRP__SZNAM, status );

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

      if( read( fd, &head, sizeof(head) ) == -1 ) {
	*status = SAI__ERROR;
      	errRep( FUNC_NAME, "Unable to read container header",  status );
      }
    }
  }

  /* Calculate buffer sizes */

  if( *status == SAI__OK ) {

    /* Get the size of the header and data section */
    smf_calc_mmapsize( sizeof(head), &(head.data), &headlen, &datalen,
                       &buflen, status );

    /* map the entire file including the header */
    if( (buf = mmap( 0, buflen, mflags, MAP_SHARED, fd, 0 ) )
	== MAP_FAILED ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unable to map model container file", status );
    }
  }

  /* Allocate memory for empty smfdata and fill relevant parts */
  *data = smf_create_smfData( SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status );

  if( *status == SAI__OK ) {
    /* Data from file header */
    (*data)->dtype = head.data.dtype;
    (*data)->ndims = head.data.ndims;
    (*data)->isTordered = head.data.isTordered;
    memcpy( (*data)->dims, head.data.dims, sizeof( head.data.dims ) );
    (*data)->hdr->steptime = head.hdr.steptime;

    /* Data pointer points to memory AFTER HEADER */
    (*data)->pntr[0] = (unsigned char*)buf + headlen;

    /* Store the file descriptor to enable us to unmap when we close */
    (*data)->file->fd = fd;

    /* Copy the DIMM filename into the smfFile */
    one_strlcpy( (*data)->file->name, name, sizeof((*data)->file->name), status );
  }

}
