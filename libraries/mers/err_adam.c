#include <string.h>
#include "f77.h"
#include "merswrap.h"

F77_SUBROUTINE(err_clear)( INTEGER(status) );

void errClear( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_CALL(err_clear)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(err_start)(  );

void errStart(  ) {

   F77_CALL(err_start)(  );

   return;
}

F77_SUBROUTINE(err_stop)( INTEGER(status) );

void errStop( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(err_stop)( INTEGER_ARG(&fstatus) );

   return;
}

F77_SUBROUTINE(msg_bell)( INTEGER(status) );

void msgBell( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(msg_bell)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(msg_ifget)( CHARACTER(pname),
                           INTEGER(status)
                           TRAIL(pname) );

void msgIfget( const char *pname,
               int *status ) {

DECLARE_CHARACTER_DYN(fpname);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER( fpname, strlen( pname ) );
   F77_EXPORT_CHARACTER( pname, fpname, fpname_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(msg_ifget)( CHARACTER_ARG(fpname),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fpname) );

   F77_FREE_CHARACTER( fpname );
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
