/*+
 *  Name:
 *     aio_frefco_c.c
 *
 *  Purpose:
 *     Destroy a format convertor object
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Description:
 *
 *
 *  Invokation:
 *
 *     CALL AIO_FREFCO( PTR, STATUS )
 *
 *  Authors:
 *
 *     David J. Allan (ROSAT,BHVAD::DJA)
 *
 *  History:
 *
 *     26-Apr-1994 (DJA):
 *        Original version.
 *- */


#include <stdlib.h>			/* malloc etc */
#include <string.h>			/* String manipulation */

#include "sae_par.h"			/* Starlink constants */
#include "cnf.h"			/* C & Fortran interfacing */
#include "aio.h"			/* Asterix I/O library */


F77_SUBROUTINE(aio_frefco)( POINTER(fco), INTEGER(status) )
  {
  GENPTR_INTEGER(status)

  AIOformatControl	*fcoref;

  if ( *status != SAI__OK )             /* Check status on entry */
    return;

  fcoref = (AIOformatControl *) *fco;	/* Import the block pointer */

  cnf_free( fcoref->fmt );		/* Release format string */

  free( fcoref );			/* Release the block storage space */

  *fco = (F77_POINTER_TYPE) 0;		/* Reset the external pointer */
  }
