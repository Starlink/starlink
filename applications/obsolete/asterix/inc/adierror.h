#if !defined(_ADI_ERROR_H_)
#define _ADI_ERROR_H_ 1

#ifndef NOEMS
#include "sae_par.h"
#endif


#define _ESTREF(_x) \
  extern char *_x;

_ESTREF(Estr__AdjObjRef)
_ESTREF(Estr__AltObjShp)
_ESTREF(Estr__CloFilObj)
_ESTREF(Estr__CloObjId)
_ESTREF(Estr__ComFilObj)
_ESTREF(Estr__CopObj)
_ESTREF(Estr__CopObjCmp)
_ESTREF(Estr__CreFilObj)
_ESTREF(Estr__CreObjDat)
_ESTREF(Estr__CreRef)
_ESTREF(Estr__DefCls)
_ESTREF(Estr__DefClsCac)
_ESTREF(Estr__DefClsDes)
_ESTREF(Estr__DefClsPrt)
_ESTREF(Estr__DefCpa)
_ESTREF(Estr__DefCst)
_ESTREF(Estr__DefFilRep)
_ESTREF(Estr__DefFun)
_ESTREF(Estr__DefGen)
_ESTREF(Estr__DefGenDis)
_ESTREF(Estr__DefMcf)
_ESTREF(Estr__DefMth)
_ESTREF(Estr__DefVar)
_ESTREF(Estr__DelObj)
_ESTREF(Estr__DelObjCmp)
_ESTREF(Estr__DelObjPrp)
_ESTREF(Estr__DelStrCmp)
_ESTREF(Estr__EvlExp)
_ESTREF(Estr__ExeFun)
_ESTREF(Estr__ExeMth)
_ESTREF(Estr__FshObjGrp)
_ESTREF(Estr__GetNumCmp)
_ESTREF(Estr__GetNumPrp)
_ESTREF(Estr__GetObjDat)
_ESTREF(Estr__GetObjNam)
_ESTREF(Estr__GetObjRef)
_ESTREF(Estr__GetObjShp)
_ESTREF(Estr__GetObjTyp)
_ESTREF(Estr__GetRefObj)
_ESTREF(Estr__IndStrCmp)
_ESTREF(Estr__IndObjPrp)
_ESTREF(Estr__LnkFilObj)
_ESTREF(Estr__LnkObjGrp)
_ESTREF(Estr__LodDefPkg)
_ESTREF(Estr__LocClsDef)
_ESTREF(Estr__LocObjCel)
_ESTREF(Estr__LocObjCmp)
_ESTREF(Estr__LocObjPrp)
_ESTREF(Estr__LocObjSli)
_ESTREF(Estr__LocStrCmp)
_ESTREF(Estr__MapObjDat)
_ESTREF(Estr__OpeFilObj)
_ESTREF(Estr__PriObj)
_ESTREF(Estr__PrsExp)
_ESTREF(Estr__PutObjDat)
_ESTREF(Estr__PutRefObj)
_ESTREF(Estr__SetObjDat)
_ESTREF(Estr__TstObjDer)
_ESTREF(Estr__TstObjExi)
_ESTREF(Estr__TstObjSta)
_ESTREF(Estr__UlnFilObj)
_ESTREF(Estr__UnmObjDat)


/*
 * Safe to assume that if user wants error reporting routines then the
 * codes will be wanted too
 */
#include "adi_err.h"


/*
 * Macro to define the current C/Fortran interface routine
 */
#define _ERR_REP(_n,_s) if ( ! _ok(status) ) adix_errout(_n,_s,status);

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Function prototypes
 */
void	adix_erranl( ADIstatus status );
void	adix_setetc( char *tok, char *val, int vlen );
void	adix_seteti( char *tok, ADIinteger val );
void	adix_setecs( ADIstatype code, char *ctx, int clen, va_list ap, ADIstatus status );
char	*adix_errmsg( ADIstatype code, char *buf, int buflen );
void	adix_errout( char *rtn, char *str, ADIstatus status );

#ifdef NOEMS
void	adix_errctx( char *buf, int buflen );
#endif

#ifdef __cplusplus
}
#endif

#endif
