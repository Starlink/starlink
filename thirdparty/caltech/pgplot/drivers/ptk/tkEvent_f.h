#ifndef TKEVENT_VT
#define TKEVENT_VT
typedef struct TkeventVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tkEvent.t"
#undef VFUNC
#undef VVAR
} TkeventVtab;
extern TkeventVtab *TkeventVptr;
extern TkeventVtab *TkeventVGet(void);
#endif /* TKEVENT_VT */
