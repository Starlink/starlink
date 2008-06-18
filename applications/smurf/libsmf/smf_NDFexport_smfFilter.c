/*
*+
*  Name:
*     smf_NDFexport_smfFilter

*  Purpose:
*     Export smfFilter to NDF file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_NDFexport_smfFilter( const smfFilter *filt, const char *name, 
*                           int *status );

*  Arguments:
*     filt = const smfFilter* (Given)
*        Pointer to smfFilter
*     name = const char* (Given)
*        Name of the NDF container
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates a new NDF container and stores the real- or
*     complex-valued filter.
*
*  Authors:
*     EC: Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-06-12 (EC):
*        Initial version.
*     2008-06-18 (EC):
*        Fixed error in calculation of df (frequency steps)

*  Notes:
*
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "jcmt/state.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_NDFexport_smfFilter"

void smf_NDFexport_smfFilter( const smfFilter *filt, const char *name, 
                           int *status ) {

  /* Local Variables */
  void *data[3]={NULL,NULL,NULL}; /* Pointer to mapped data array */
  size_t i;                   /* Counter */
  int lbnd[2];                /* lower dimension bounds */
  int n;                      /* Number of mapped elements */
  size_t nbytes;              /* Number of bytes to copy */
  int place;                  /* NDF placeholder */
  int ndfid;                  /* Another NDF placeholder */
  int ubnd[2];                /* lower dimension bounds */

  if (*status != SAI__OK ) return;

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "NULL smfFilter supplied.", status );
    return;
  }

  if( !filt->real ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfFilter contains a NULL buffer", status );
    return;
  }

  /* Start an NDF context */
  ndfBegin();
  
  /* Create HDS container */
  ndfPlace( NULL, name, &place, status );

  /* Create NDF inside container */
  lbnd[0] = 1;
  ubnd[0] = filt->dim;

  lbnd[1] = 1;
  ubnd[1] = 2;

  ndfNew( "_DOUBLE", 2, lbnd, ubnd, &place, &ndfid, status );

  /* Map the data array */
  ndfMap( ndfid, "DATA", "_DOUBLE", "WRITE", &data[0], &n, status );

  /* Copy the real and imaginary parts of the data into the NDF */
  if( *status == SAI__OK ) {
    nbytes = filt->dim*sizeof(*filt->real);
    memcpy( data[0], filt->real, nbytes);
    if( filt->imag ) {
      memcpy( ((char *) data[0])+nbytes, filt->imag, nbytes );
    }
  }

  /* Clean up */
  ndfAnnul( &ndfid, status );
  ndfEnd( status );
}
