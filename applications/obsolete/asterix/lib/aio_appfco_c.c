/*+
 *  Name:
 *     aio_appfco_c.c
 *
 *  Purpose:
 *     Apply a format convert to a specified element of a Fortran array
 *     Special care is taken to handle Fortran character arrays correctly.
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
 *     CALL AIO_APPFCO( FCO, LDIMS, DPTR, INDICES, STR, STATUS )
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


F77_SUBROUTINE(aio_appfco)( POINTER(fco), INTEGER_ARRAY(dims),
                            POINTER(dptr), INTEGER_ARRAY(indices),
                            CHARACTER(str), INTEGER(status) TRAIL(str) )
  {
  GENPTR_POINTER(fco)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_POINTER(dptr)
  GENPTR_INTEGER_ARRAY(indices)
  GENPTR_CHARACTER(str)
  GENPTR_INTEGER(status)

  AIOformatControl	*fcoref;
  int			data_length;	/* Variable to hold string size */

  DECLARE_CHARACTER(fmt,AIO__MXFMTLEN);	/* Local format string */

  if ( *status != SAI__OK )             /* Check status on entry */
    return;

  fcoref = (AIOformatControl *) *fco;	/* Import foreign pointer */

  cnf_copyf( fcoref->fmt,fcoref->fmtlen,/* Import format string */
             fmt, fmt_length );

  if ( fcoref->type == AIO_T_CHAR )	/* Character array? */
    {
    data_length = fcoref->size;		/* Get length of mapped character
					   strings */

    (*fcoref->func)( &dims[0], &dims[1],
                     &dims[2], &dims[4],
                     &dims[4], &dims[5],
                     &dims[6], *dptr,
                     indices,
                     CHARACTER_ARG(fmt),
                     CHARACTER_ARG(str),
                     INTEGER_ARG(status)
                     TRAIL_ARG(data)
                     TRAIL_ARG(fmt) TRAIL_ARG(str) );
    }
  else
    (*fcoref->func)( &dims[0], &dims[1],
                     &dims[2], &dims[4],
                     &dims[4], &dims[5],
                     &dims[6], *dptr,
                     indices,
                     CHARACTER_ARG(fmt),
                     CHARACTER_ARG(str),
                     INTEGER_ARG(status)
                     TRAIL_ARG(fmt) TRAIL_ARG(str) );
  }
