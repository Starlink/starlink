
#include <sys/types.h>

   typedef struct {
      int semid;
      pid_t pid;
   } tclevent_t;

   int tcleventstart( tclevent_t *tev );
   int tcleventstop( tclevent_t *tev );

