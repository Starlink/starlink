#ifndef LANG_VT
#define LANG_VT
typedef struct LangVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "Lang.t"
#undef VFUNC
#undef VVAR
} LangVtab;
extern LangVtab *LangVptr;
extern LangVtab *LangVGet(void);
#endif /* LANG_VT */
