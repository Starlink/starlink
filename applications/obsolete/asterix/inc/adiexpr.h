#if !defined(_ADI_EXPR_H_)
#define _ADI_EXPR_H_

#define         UT_cid_etn    ADI_G_alloc_etn
#define         _etn_head(_x)   (*(_class_data(_x)+0))
#define         _etn_args(_x)   (*(_class_data(_x)+1))
#define		_etn_q(_x)	(_DTDEF(_x)->selfid==UT_cid_etn)

#define 	_GET_HEDARG(_hed,_arg,_x) \
		{ADIobj	*cdata=_class_data((_x));\
		 (_hed)=*cdata;(_arg)=*(cdata+1);}

#define 	_GET_HEDARG_A(_hed,_arg,_x) \
		{(_hed) = _class_data((_x)); (_arg) = (_hed) + 1;}

#define _ARGLOOP_1ST_TO_NTH(x) \
    for ( (x) = vs_base; ((x) < vs_top); (x)++ )
#define _ARGLOOP_1ST_TO_NTH_AND(x,cond) \
    for ( (x) = vs_base; ((x) < vs_top) && (cond); (x)++ )
#define _ARGLOOP_2ND_TO_NTH(x) \
    for ( (x) = vs_base+1; ((x) < vs_top); (x)++ )
#define _ARGLOOP_2ND_TO_NTH_AND(x,cond) \
    for ( (x) = vs_base+1; ((x) < vs_top) && (cond); (x)++ )
#define _ARGLOOP_NTH_TO_1ST(x) \
    for ( (x) = vs_top-1; ((x) >= vs_base); (x)-- )
#define _ARGLOOP_NTH_TO_1ST_AND(x,cond) \
    for ( (x) = vs_top-1; ((x) >= vs_base) && (cond); (x)-- )
#define _NARG ((int)(vs_top-vs_base))

/*
 * Global variablex externally referenced
 */
extern ADIobj UT_cid_etn;
extern ADIobj	*vs_base;
extern ADIobj	*vs_top;
extern ADIstackFrame *fs_top;

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Function prototypes
 */

ADIobj		adix_fexec( char *func, int flen, int narg, ADIobj args[],
                        ADIstatus status );
void 		ADIetnInit( ADIstatus status );
ADIobj		ADIetnNew( ADIobj head, ADIobj args, ADIstatus status );
ADIobj 		ADIexprEvalInt( ADIobj expr, ADIobj symlist, int level, ADIlogical *changed,
			ADIstatus status );
ADIobj 		ADIexprEval( ADIobj expr, ADIobj symlist, ADIlogical ownres, ADIstatus status );
ADIobj 		ADIexprEvalList( ADIobj elist, ADIobj symlist, ADIstatus status );
ADIobj 		ADIexprMapFun( ADIobj head, ADIobj *first, ADIinteger llen,
			int nlist, ADIstatus status );
ADIobj 		ADIexprOwnArg( ADIobj *arg, ADIstatus status );
void 		ADIexprPopFS( ADIstatus status );
void 		ADIexprPushFS( int tslot, ADIobj func, ADIstatus status );

ADIlogical 	ADIexprTestBind( ADIobj sbind, int narg, ADIobj args[],
			ADIstatus status );
ADIclassDef	*ADIexprArgClass( int iarg, ADIobj arg, ADIstatus status );

ADIobj 		ADIsymAddBind( ADIobj name, int upvar, ADIobj bind,
			ADIstatus status );
ADIobj 		ADIsymFind( ADIobj name, int upvar, ADIlogical takefirst, int forms,
			ADIstatus status );
ADIobj 		ADIsbindNew( ADIsbindForm form, ADIobj data,
			ADIstatus status );

/*
 * Exception handling
 */
void 		ADIexecAccept( char *except, int elen, ADIstatus status );
void 		ADIexecAcceptI( ADIobj except, ADIstatus status );
void 		ADIexecReset( ADIstatus status );
void 		ADIexecRaiseInt( ADIobj except, ADIstatype code, char *message,
			int mlen, va_list ap, ADIstatus status );
void 		ADIexecRaise( char *exname, int exlen, ADIstatype code, char *message,
			int mlen, ADIstatus status, ... );
void 		ADIexecRaiseI( ADIobj except, ADIstatype code, char *message,
			int mlen, ADIstatus status, ... );

ADIobj 		ADIexprParseString( char *string, int slen, ADIobj grammar,
			ADIstatus status );

#ifdef __cplusplus
}
#endif

#endif
