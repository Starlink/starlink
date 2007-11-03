#ifndef TK_VT
#define TK_VT
typedef struct TkVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tk.t"
#undef VFUNC
#undef VVAR
} TkVtab;
extern TkVtab *TkVptr;
extern TkVtab *TkVGet(void);
#endif /* TK_VT */
