#ifndef TCLDECLS_VT
#define TCLDECLS_VT
typedef struct TcldeclsVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tclDecls.t"
#undef VFUNC
#undef VVAR
} TcldeclsVtab;
extern TcldeclsVtab *TcldeclsVptr;
extern TcldeclsVtab *TcldeclsVGet(void);
#endif /* TCLDECLS_VT */
