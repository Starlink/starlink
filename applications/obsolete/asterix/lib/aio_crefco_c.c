/*+
 *  Name:
 *     aio_crefco_c.c
 *
 *  Purpose:
 *     Create a format convertor object given object type and desired format
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
 *     CALL AIO_CREFCO( TYPE, FMT, PTR, STATUS )
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


F77_SUBROUTINE(aio_crefco)( CHARACTER(type), CHARACTER(fmt),
                            POINTER(fco), INTEGER(status)
                            TRAIL(type) TRAIL(fmt) )
  {
  GENPTR_CHARACTER(type)
  GENPTR_CHARACTER(fmt)
  GENPTR_INTEGER(status)

  AIOformatControl	*fcoref;

  if ( *status != SAI__OK )             /* Check status on entry */
    return;

  fcoref = (AIOformatControl *) 	/* Allocate memory for convertor */
       malloc(sizeof(AIOformatControl));
  if ( ! fcoref )
    {
    *status = SAI__ERROR;
    ems_rep_c( " ", "Unable to allocate memory for format control block",
               status );
    }

  else if ( ! strncmp( type, "_INTEGER", 8 ) )
    {
    fcoref->type = AIO_T_INTEGER;
    fcoref->func = F77_EXTERNAL_NAME(aio_itoc);
    fcoref->size = sizeof(F77_INTEGER_TYPE);
    }

  else if ( ! strncmp( type, "_REAL", 5 ) )
    {
    fcoref->type = AIO_T_REAL;
    fcoref->func = F77_EXTERNAL_NAME(aio_rtoc);
    fcoref->size = sizeof(F77_REAL_TYPE);
    }

  else if ( ! strncmp( type, "_DOUBLE", 7 ) )
    {
    fcoref->type = AIO_T_DOUBLE;
    fcoref->func = F77_EXTERNAL_NAME(aio_dtoc);
    fcoref->size = sizeof(F77_DOUBLE_TYPE);
    }

  else if ( ! strncmp( type, "_LOGICAL", 8 ) )
    {
    fcoref->type = AIO_T_LOGICAL;
    fcoref->func = F77_EXTERNAL_NAME(aio_ltoc);
    fcoref->size = sizeof(F77_LOGICAL_TYPE);
    }

  else if ( ! strncmp( type, "_CHAR", 5 ) )
    {
    fcoref->type = AIO_T_CHAR;
    fcoref->func = F77_EXTERNAL_NAME(aio_ctoc);
    fcoref->size = atoi( type + 6 );
    }

  else
    {
    *status = SAI__ERROR;
    ems_setc_c( "TYPE", type, type_length );
    ems_rep_c( " ", "Unrecognised format type /^TYPE/",
               status );
    }

  if ( *status == SAI__OK ) 		/* Status still ok? */
    {
    fcoref->fmt = cnf_creim( fmt, 	/* Store format in the block */
                        fmt_length );
    fcoref->fmtlen = fmt_length;

    *fco = (F77_POINTER_TYPE) fcoref;	/* Set return value */
    }
  }
