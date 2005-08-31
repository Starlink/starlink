#if !defined( SLV_INCLUDED )
#define SLV_INCLUDED 1

#include <sys/types.h>

void SlvKill( pid_t, int * );
void SlvKillW( pid_t, int * );
void SlvWaitK( pid_t, int, int * );
#endif
