#include "cnf.h"
#include "dat_par.h"
#include "sae_par.h"
#include "ccdaux.h"

   void *ccdMall( char *type, int size, int *status ) {
/*
*+
*  Name:
*     ccdMall
*
*  Purpose:
*     C wrapper for fortran CCD1_MALL routine.
*
*  Arguments:
*     type = char *
*        HDS type of memory to allocate, as a null-terminated string.
*     size = int
*        Number of elements of type type to allocate.
*     status = int
*        The global status.
*
*  Return Value:
*     A pointer to a block of memory which will hold size elements of
*     type type.  This pointer has been registered with the CCDPACK
*     memory allocation machinery (and a fortiori the CNF memory 
*     allocation machinery) and so must be deallocated using CCD1_MFREE.
*     The pointer returned is a C pointer, and thus suitable for direct
*     use by C code.  If it is to be used by Fortran code it must
*     be processed with the function cnfFptr.
*-
*/
      DECLARE_CHARACTER( ftype, DAT__SZTYP );
      F77_POINTER_TYPE ptr;

      if ( *status != SAI__OK ) return (void *) NULL;

      cnfExprt( type, ftype, DAT__SZTYP );
      F77_CALL(ccd1_mall)( INTEGER_ARG(&size), CHARACTER_ARG(ftype),
                           POINTER_ARG(&ptr), INTEGER_ARG(status)
                           TRAIL_ARG(ftype) );
      return cnfCptr( ptr );
   }
                           

/* $Id$ */
