#ifndef TKINTDECLS_VT
#define TKINTDECLS_VT
typedef struct TkintdeclsVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tkIntDecls.t"
#undef VFUNC
#undef VVAR
} TkintdeclsVtab;
extern TkintdeclsVtab *TkintdeclsVptr;
extern TkintdeclsVtab *TkintdeclsVGet(void);
#endif /* TKINTDECLS_VT */
