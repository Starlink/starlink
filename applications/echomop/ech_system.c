#include "f77.h"

F77_SUBROUTINE( ech_system )( CHARACTER(cmd), INTEGER(istat) TRAIL(cmd) )
{
GENPTR_CHARACTER( cmd )
GENPTR_INTEGER( istat )

F77_CHARACTER_TYPE *thecmd;

    thecmd = cmd;
    *istat = system( thecmd );

return;
}
