/*+ adicface.h - ADI C language prototypes*
 *
 *    Description :
 *
 *    Authors :
 *
 *     DJA: David J. Allan (JET-X,University of Birmingham)
 *
 *    History :
 *
 *     15 Nov 1994 (DJA):
 *        Original version.
 *-
 */
#if !defined(_ADI_CFACE_H_)
#define _ADI_CFACE_H_ 1

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Interface to error system
 */
void adic_erranl( ADIstatus status );
void adic_errmsg( ADIstatype code, char *buf, int buflen );
void adic_setecs( ADIstatype code, char *ctx, ADIstatus status, ... );
void adic_setetc( char *tok, char *val, int vlen );
void adic_seteti( char *tok, int val );
#ifdef NOEMS
void adic_errctx( char *buf, int buflen );
#endif

/*
 * Property handling :
 */
void adic_nprp( ADIobj id, int *nprp, ADIstatus status );
void adic_indprp( ADIobj id, int index, ADIobj *pid, ADIstatus status );

/*
 * Structure handling :
 */
void adic_ncmp( ADIobj id, int *ncmp, ADIstatus status );
void adic_indcmp( ADIobj id, int index, ADIobj *cid, ADIstatus status );

/*
 * Reference count access & manipulation
 */
void adic_clone( ADIobj id, ADIobj *cid, ADIstatus status );
void adic_refcnt( ADIobj id, int *cnt, ADIstatus status );
void adic_refadj( ADIobj id, int incr, ADIstatus status );

/*
 * Object references
 */
void adic_cgetref( ADIobj rid, char *name, ADIobj *id, ADIstatus status );
void adic_cputref( ADIobj rid, char *name, ADIobj id, ADIstatus status );
void adic_getref( ADIobj rid, ADIobj *id, ADIstatus status );
void adic_newref( ADIobj id, ADIobj *rid, ADIstatus status );
void adic_putref( ADIobj rid, ADIobj id, ADIstatus status );

/*
 * List handling
 */
void adic_newlst( ADIobj aid, ADIobj bid, ADIobj *id, ADIstatus status );
void adic_getlst( ADIobj lid, ADIobj *aid, ADIobj *bid, ADIstatus status );
void adic_lstnth( ADIobj lid, ADIinteger n, ADIobj *eid, ADIstatus status );

/*
 * Data system definitions
 */
void adic_defcac( ADIobj clsid, ADIinteger number, ADIstatus status );
void adic_defcls( char *name, char *supers, char *members,
		  ADIobj *id, ADIstatus status );
void adic_defcpa( char *name, ADIcMethodCB exec, ADIobj *id, ADIstatus );
void adic_defdes( ADIobj clsid, ADIcMethodCB rtn, ADIstatus status );
void adic_deffun( char *spec, ADIcMethodCB exec,
		  ADIobj *id, ADIstatus );
void adic_defgdp( ADIobj genid, ADIcGenericDispatchCB exec, ADIstatus status );
void adic_defgen( char *spec, char *options, ADIcGenericDispatchCB exec,
		  ADIobj *id, ADIstatus );
void adic_defmcf( char *name, ADIcMethodCombinationCB exec,
		  ADIobj *id, ADIstatus );
void adic_defmth( char *spec, ADIcMethodCB exec,
		  ADIobj *id, ADIstatus );
void adic_defprt( ADIobj clsid, ADIcMethodCB rtn, ADIstatus status );
void adic_defrep( char *name, ADIobj *id, ADIstatus status );
void adic_defvar( char *name, ADIlogical global, ADIobj value, ADIstatus status ); 
void adic_dervd( ADIobj id, char *name, ADIlogical *der, ADIstatus status );
void adic_loccls( char *cls, ADIobj *clsid, ADIstatus status );

/*
 * Method execution
 */
void adic_calnxt( ADIstatus status );
void adic_exec( char *func, int narg, ADIobj args[], ADIobj *res, ADIstatus status );
void adic_exec2( char *func, ADIobj arg1, ADIobj arg2, ADIobj *res, ADIstatus status );
void adic_execi( ADIobj func, int narg, ADIobj args[], ADIobj *res, ADIstatus status );
ADIobj adic_nulmth( int narg, ADIobj args[], ADIstatus status );

/*
 * System method interfaces
 */
void adic_ccopy( ADIobj in, char *inmem, ADIobj out,
		 char *outmem, ADIstatus status );
void adic_copy( ADIobj id, ADIobj *cid, ADIstatus status );
void adic_print( ADIobj id, ADIstatus status );
 void adic_setlnk( ADIobj id, ADIobj lid, ADIstatus status );
/* void adic_unlnk( ADIobj id, ADIstatus status ); */

/*
 * Data creation
 */
void adic_new0( char *cls, ADIobj *id, ADIstatus status );
void adic_new1( char *cls, int nval, ADIobj *id, ADIstatus status );
void adic_newn( char *cls, int ndim, int dims[], ADIobj *id, ADIstatus status );
#define _defc_adi_new_decl(_t) void _TM_name(adic_new,_t)( int ndim, int dims[], ADIobj *id, ADIstatus status )
void    adic_newc( int ndim, int dims[], ADIobj *id, ADIstatus status );
#define _defc_adi_new0_decl(_t) void _TM_name(adic_new0,_t)( ADIobj *id, ADIstatus status )
void    adic_new0c( ADIobj *id, ADIstatus status );
#define _defc_adi_new1_decl(_t) void _TM_name(adic_new1,_t)( int nval, ADIobj *id, ADIstatus status )
void    adic_new1c( int nval, ADIobj *id, ADIstatus status );
#define _defc_adi_newv_decl(_t) void _TM_name(adic_newv,_t)( int ndim, int dims[], _TM_ctype(_t) value[], ADIobj *id, ADIstatus status )
void    adic_newvc( int ndim, int dims[], char *value[], ADIobj *id, ADIstatus status );
#define _defc_adi_newv0_decl(_t) void _TM_name(adic_newv0,_t)(_TM_ctype(_t) value, ADIobj *id, ADIstatus status )
void    adic_newv0c( char *str, ADIobj *id, ADIstatus status );
void    adic_newv0c_n( char *str, int len, ADIobj *id, ADIstatus status );
#define _defc_adi_newv1_decl(_t) void _TM_name(adic_newv1,_t)( int nval, _TM_ctype(_t) value[], ADIobj *id, ADIstatus status )
void    adic_newv1c( int nval, char *value[], ADIobj *id, ADIstatus status );
void adic_new0c_n( int len, ADIobj *id, ADIstatus status );

/*
 * Data access
 */
void    adic_get( ADIobj id, char *type, int ndim, int dimx[], void *value, int dims[], ADIstatus status );
#define _defc_adi_get_decl(_t) void _TM_name(adic_get,_t)( ADIobj id, int ndim, int dimx[], _TM_ctype(_t) *value, int dims[], ADIstatus status )
void    adic_getc( ADIobj id, int ndim, int dimx[], int len, char *value, int dims[], ADIstatus status );

void    adic_get0( ADIobj id, char *type, void *value, ADIstatus status );
#define _defc_adi_get0_decl(_t) void _TM_name(adic_get0,_t)( ADIobj id, _TM_ctype(_t) *value, ADIstatus status )
void    adic_get0c( ADIobj id, int len, char *value, ADIstatus status );

void    adic_get1( ADIobj id, char *type, int mxval, void *value, int *nval, ADIstatus status );
#define _defc_adi_get1_decl(_t) void _TM_name(adic_get1,_t)( ADIobj id, int mxval, _TM_ctype(_t) *value, int *nval, ADIstatus status )
void    adic_get1c( ADIobj id, int mxval, int len, char *value, int *nactval, ADIstatus status );

void	adic_map( ADIobj id, char *type, char *mode, void **vptr, ADIstatus status );
#define _defc_adi_map_decl(_t) void _TM_name(adic_map,_t)(ADIobj id,char *mode, _TM_ctype(_t) **vptr, ADIstatus status )
void    adic_mapc( ADIobj id, char *mode, char **vptr, ADIstatus status );

void    adic_put( ADIobj id, char *type, int ndim, int dims[], void *value, ADIstatus status );
#define _defc_adi_put_decl(_t) void _TM_name(adic_put,_t)( ADIobj id, int ndim, int dims[], _TM_ctype(_t) *value, ADIstatus status )
void    adic_putc( ADIobj id, int ndim, int dims[], char **value, ADIstatus status );

void    adic_put0( ADIobj id, char *type, void *value, ADIstatus status );
#define _defc_adi_put0_decl(_t) void _TM_name(adic_put0,_t)( ADIobj id, _TM_ctype(_t) value, ADIstatus status )
void    adic_put0c( ADIobj id, char *value, ADIstatus status );

void    adic_put1( ADIobj id, char *type, int nval, void *value, ADIstatus status );
#define _defc_adi_put1_decl(_t) void _TM_name(adic_put1,_t)( ADIobj id, int nval, _TM_ctype(_t) *value, ADIstatus status )
void    adic_put1c( ADIobj id, int nval, char **value, ADIstatus status );

void	adic_unmap( ADIobj id, void *vptr, ADIstatus status );

/*
 * Component data creation
 */
void adic_cnew( ADIobj id, char *name, char *cls, int ndim, int dims[], ADIstatus status );
void adic_cnew0( ADIobj id, char *name, char *cls, ADIstatus status );
void adic_cnew1( ADIobj id, char *name, char *cls, int nval, ADIstatus status );
#define _defc_adi_cnew_decl(_t) void _TM_name(adic_cnew,_t)( ADIobj id, char *name, int ndim, int dims[], ADIstatus status )
void adic_cnewc( ADIobj id, char *name, int ndim, int dims[], ADIstatus status );
#define _defc_adi_cnew0_decl(_t) void _TM_name(adic_cnew0,_t)( ADIobj id, char *name, ADIstatus status )
void adic_cnew0c( ADIobj id, char *name, ADIstatus status );
#define _defc_adi_cnew1_decl(_t) void _TM_name(adic_cnew1,_t)( ADIobj id, char *name, int nval, ADIstatus status )
void    adic_cnew1c( ADIobj id, char *name, int nval, ADIstatus status );
#define _defc_adi_cnewv_decl(_t) void _TM_name(adic_cnewv,_t)( ADIobj id, char *name, int ndim, int dims[], _TM_ctype(_t) value[], ADIstatus status )
void    adic_cnewvc( ADIobj id, char *name, int ndim, int dims[], char *value[], ADIstatus status );
#define _defc_adi_cnewv0_decl(_t) void _TM_name(adic_cnewv0,_t)( ADIobj id, char *name, _TM_ctype(_t) value, ADIstatus status )
void    adic_cnewv0c( ADIobj id, char *name, char *str, ADIstatus status );
void    adic_cnewv0c_n( ADIobj id, char *name, char *str, int len, ADIstatus status );
#define _defc_adi_cnewv1_decl(_t) void _TM_name(adic_cnewv1,_t)( ADIobj id, char *name, int nval, _TM_ctype(_t) value[], ADIstatus status )
void    adic_cnewv1c( ADIobj id, char *name, int nval, char *value[], ADIstatus status );

/*
 * Component data access
 */
void    adic_cget( ADIobj id, char *name, char *type, int ndim, int dimx[], void *value, int dims[], ADIstatus status );
#define _defc_adi_cget_decl(_t) void _TM_name(adic_cget,_t)( ADIobj id, char *name, int ndim, int dimx[], _TM_ctype(_t) *value, int dims[], ADIstatus status )
void    adic_cgetc( ADIobj id, char *name, int ndim, int dimx[], int len, char *value, int dims[], ADIstatus status );

void    adic_cget0( ADIobj id, char *name, char *type, void *value, ADIstatus status );
#define _defc_adi_cget0_decl(_t) void _TM_name(adic_cget0,_t)( ADIobj id, char *name,_TM_ctype(_t) *value, ADIstatus status )
void    adic_cget0c( ADIobj id, char *name, int len, char *value, ADIstatus status );

void    adic_cget1( ADIobj id, char *name, char *type, int mxval, void *value, int *nval, ADIstatus status );
#define _defc_adi_cget1_decl(_t) void _TM_name(adic_cget1,_t)( ADIobj id, char *name, int mxval, _TM_ctype(_t) *value, int *nval, ADIstatus status )
void    adic_cget1c( ADIobj id, char *name, int mxval, int len, char *value, int *nactval, ADIstatus status );

void	adic_cmap( ADIobj id, char *name, char *type, char *mode, void **vptr, ADIstatus status );
#define _defc_adi_cmap_decl(_t) void _TM_name(adic_cmap,_t)( ADIobj id, char *name, char *mode, _TM_ctype(_t) **vptr, ADIstatus status )
void    adic_cmapc( ADIobj id, char *name, char *mode, char **vptr, ADIstatus status );

void    adic_cput( ADIobj id, char *name, char *type, int ndim, int dims[], void *value, ADIstatus status );
#define _defc_adi_cput_decl(_t) void _TM_name(adic_cput,_t)( ADIobj id, char *name, int ndim, int dims[], _TM_ctype(_t) *value, ADIstatus status )
void    adic_cputc( ADIobj id, char *name, int ndim, int dims[], char **value, ADIstatus status );

void    adic_cput0( ADIobj id, char *name, char *type, void *value, ADIstatus status );
#define _defc_adi_cput0_decl(_t) void _TM_name(adic_cput0,_t)( ADIobj id, char *name,_TM_ctype(_t) value, ADIstatus status )
void    adic_cput0c( ADIobj id, char *name, char *value, ADIstatus status );

void    adic_cput1( ADIobj id, char *name, char *type, int nval, void *value, ADIstatus status );
#define _defc_adi_cput1_decl(_t) void _TM_name(adic_cput1,_t)( ADIobj id, char *name, int nval, _TM_ctype(_t) *value, ADIstatus status )
void    adic_cput1c( ADIobj id, char *name, int mxval, char **value, ADIstatus status );

void 	adic_putid( ADIobj id, ADIobj value, ADIstatus status );
void 	adic_cputid( ADIobj id, char *member, ADIobj value, ADIstatus status );
void 	adic_find( ADIobj id, char *name, ADIobj *cid, ADIstatus status );
void 	adic_findi( ADIobj id, ADIobj name, ADIobj *cid, ADIstatus status );
void	adic_cunmap( ADIobj id, char *name, void *vptr, ADIstatus status );
void 	adic_there( ADIobj id, char *name, ADIlogical *there, ADIstatus status );

/*
 * Object enquiry
 */
void adic_clen( ADIobj id, ADIinteger *len, ADIstatus status );
void adic_cshape( ADIobj id, char *name, int mxndim, int dims[], int *ndim, ADIstatus status );
void adic_csize( ADIobj id, char *name, int *nelm, ADIstatus status );
void adic_cstate( ADIobj id, char *name, ADIlogical *state, ADIstatus status );
void adic_ctype( ADIobj id, char *name, int blen, char *buf, ADIstatus status );
void adic_name( ADIobj id, int blen, char *buf, ADIstatus status );
void adic_shape( ADIobj id, int mxndim, int dims[], int *ndim, ADIstatus status );
void adic_size( ADIobj id, int *nelm, ADIstatus status );
void adic_state( ADIobj id, ADIlogical *state, ADIstatus status );
void adic_type( ADIobj id, int blen, char *buf, ADIstatus status );

/*
 * Changing object attributes
 */
void adic_alter( ADIobj id, int ndim, int dims[], ADIstatus status );
void adic_calter( ADIobj id, char *name, int ndim, int dims[], ADIstatus status );
void adic_ccell( ADIobj id, char *name, int ndim, int index[], ADIobj *cid, ADIstatus status );
void adic_cell( ADIobj id, int ndim, int index[], ADIobj *cid, ADIstatus status );
void adic_cslice( ADIobj id, char *name, int ndim, int diml[], int dimu[], ADIobj *sid, ADIstatus status );
void adic_slice( ADIobj id, int ndim, int diml[], int dimu[], ADIobj *sid, ADIstatus status );

/*
 * Object destruction
 */
void adic_cerase( ADIobj id, char *member, ADIstatus status );
void adic_erase( ADIobj *id, ADIstatus status );

/*
 * Symbol packages
 */
void adic_reqpkg( char *pkg, ADIstatus status );

void adic_accpt( char *except, ADIstatus status );
void adic_raise( char *except, ADIstatype code, char *msg, ADIstatus status );


/* Other */
void adic_probe( ADIstatus status );
void adic_cmnstr( char *string, ADIobj *id, ADIstatus status );
void adic_defrcb( ADIobj rid, char *name,
		  ADICB rtn, ADIstatus status );
void adic_getlink( ADIobj id, ADIobj *lid, ADIstatus status );
void adic_getpath( ADIobj id, int mxlen, char *buf, ADIstatus status );
void adic_locrcb( ADIobj rid, char *name,
		  ADIobj *rtn, ADIstatus status );
void adic_locrep( char *name, ADIobj *id, ADIstatus status );

void adic_flose( ADIobj id, ADIstatus status );
void adic_fcreat( char *fspec, ADIobj id, ADIobj *fid, ADIstatus status );
void adic_fopen( char *fspec, char *cls, char *mode, ADIobj *id, ADIstatus status );

void adic_eprsc( char *string, ADIobj grammar, ADIobj *expr, ADIstatus status );
void adic_eeval( ADIobj expr, ADIobj symlist, ADIlogical ownres, ADIobj *value, ADIstatus status );

#define _defc_all6(_t) \
  _defc_adi_get0_decl(_t); \
  _defc_adi_get1_decl(_t); \
  _defc_adi_map_decl(_t); \
  _defc_adi_new_decl(_t); \
  _defc_adi_new0_decl(_t); \
  _defc_adi_new1_decl(_t); \
  _defc_adi_newv_decl(_t); \
  _defc_adi_newv0_decl(_t); \
  _defc_adi_newv1_decl(_t); \
  _defc_adi_put_decl(_t); \
  _defc_adi_put0_decl(_t); \
  _defc_adi_put1_decl(_t);

#define _defc_call6(_t) \
  _defc_adi_cget0_decl(_t); \
  _defc_adi_cget1_decl(_t); \
  _defc_adi_cmap_decl(_t); \
  _defc_adi_cnew_decl(_t); \
  _defc_adi_cnew0_decl(_t); \
  _defc_adi_cnew1_decl(_t); \
  _defc_adi_cnewv_decl(_t); \
  _defc_adi_cnewv0_decl(_t); \
  _defc_adi_cnewv1_decl(_t); \
  _defc_adi_cput_decl(_t); \
  _defc_adi_cput0_decl(_t); \
  _defc_adi_cput1_decl(_t);

_defc_all6(b)
_defc_all6(ub)
_defc_all6(w)
_defc_all6(uw)
_defc_all6(i)
_defc_all6(r)
_defc_all6(d)
_defc_all6(l)
_defc_all6(p)
#undef _defc_all6

_defc_call6(b)
_defc_call6(ub)
_defc_call6(w)
_defc_call6(uw)
_defc_call6(i)
_defc_call6(r)
_defc_call6(d)
_defc_call6(l)
_defc_call6(p)
#undef _defc_call6

#ifdef __cplusplus
}
#endif

#endif
