#include "f77.h"
#include "merswrap.h"

F77_SUBROUTINE(msg_bell)( INTEGER(status) );

void msgBell( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(msg_bell)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(msg_sync)( INTEGER(status) );

void msgSync( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(msg_sync)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}
