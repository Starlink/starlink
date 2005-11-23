#include <string.h>
#include "dat_par.h"
#include "f77.h"
#include "star/hds_fortran.h"
#include "hds.h"

F77_SUBROUTINE(dat_putc)( CHARACTER(loc),
                          INTEGER(ndim),
                          INTEGER_ARRAY(dim),
                          CHARACTER_ARRAY(value),
                          INTEGER(status)
                          TRAIL(loc)
                          TRAIL(value) );

void idlDatPutc( HDSLoc *loc,
              int ndim,
              const int *dim,
              char *const *value,
              int value_length,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_CHARACTER_ARRAY_DYN(fvalue);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   HDS_EXPORT_CLOCATOR( loc, floc, status );

   for (i=ndim,nvalues=1;i;i--) nvalues*=dim[i-1];

   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fdim,ndim);
   F77_EXPORT_INTEGER_ARRAY(dim,fdim,ndim);
   F77_CREATE_CHARACTER_ARRAY(fvalue,value_length-1,nvalues);
   F77_EXPORT_CHARACTER_ARRAY_P(value,fvalue,fvalue_length,nvalues);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_putc)( CHARACTER_ARG(floc),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(fdim),
                       CHARACTER_ARRAY_ARG(fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc)
                       TRAIL_ARG(fvalue) );

   F77_FREE_INTEGER(fdim);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
