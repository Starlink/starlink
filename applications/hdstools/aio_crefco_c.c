/*+
 *  Name:
 *     aio_crefco_c.c
 *
 *  Purpose:
 *     Create a format convertor object given object type and desired format
 *
 *  Language:
 *     Starlink ANSI C
 *
 *  Description:
 *
 *
 *  Invocation:
 *     CALL AIO_CREFCO( TYPE, FMT, PTR, STATUS )
 *
 *  Authors:
 *     David J. Allan (ROSAT,BHVAD::DJA)
 *     Alan J. Chipperfield (CLRC)
 *     Tim Jenness (JAC, Hawaii)
 *
 *  History:
 *
 *     26-Apr-1994 (DJA):
 *        Original version.
 *     17-Sep-2001 (AJC):
 *        Update EMS usage.
 *     03-Jan-2008 (TIMJ):
 *        Use CNF to export the fortran pointer.
 *-
 */


#include <stdlib.h>			/* malloc etc */
#include <string.h>			/* String manipulation */

#include "sae_par.h"			/* Starlink constants */
#include "cnf.h"			/* C & Fortran interfacing */
#include "ems.h"                        /* EMS prototypes */
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
       cnfMalloc(sizeof(AIOformatControl));
  if ( ! fcoref )
    {
    *status = SAI__ERROR;
    emsRep( " ", "Unable to allocate memory for format control block",
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
    emsSetnc( "TYPE", type, type_length );
    emsRep( " ", "Unrecognised format type /^TYPE/",
               status );
    }

  if ( *status == SAI__OK ) 		/* Status still ok? */
    {
    fcoref->fmt = cnfCreim( fmt, 	/* Store format in the block */
                        fmt_length );
    fcoref->fmtlen = fmt_length;

    *fco = cnfFptr( fcoref );	/* Set return value */
    }
  }
