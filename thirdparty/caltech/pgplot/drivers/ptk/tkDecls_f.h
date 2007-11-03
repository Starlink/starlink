#ifndef TKDECLS_VT
#define TKDECLS_VT
typedef struct TkdeclsVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tkDecls.t"
#undef VFUNC
#undef VVAR
} TkdeclsVtab;
extern TkdeclsVtab *TkdeclsVptr;
extern TkdeclsVtab *TkdeclsVGet(void);
#endif /* TKDECLS_VT */
