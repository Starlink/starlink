/*
*+
*  Name:
*     adicface.c

*  Purpose:
*     The ADI C interface

*  Language:
*     ANSI C

*  Type of Module:
*     C source file

*  Description:
*     This file provides the C interface functions to ADI. The
*     routines supplied are,
*
*      Error system :
*
*	adic_erranl	- Annul a reported error
*	adic_errctx	- Return error context string
* 	adic_errmsg	- Return error message given code
*	adic_setecs	- Set error code and context string
*
*      Property handling :
*
*       adic_nprp	- Return number of properties
*       adic_indprp	- Locate property by number
*
*      Structure handling :
*
*	adic_ncmp	- Number of components in structure
*	adic_indcmp	- Locate structure component by number
*
*      Reference count :
*
*       adic_clone	- Clone an identifier
*	adic_refadj	- Apply increment to object reference count
*	adic_refcnt	- Return object reference count
*
*      Data system definitions :
*
*	adic_defcac	- Define class allocation cluster size
*       adic_defcls	- Define a new class
*       adic_defcpa	- Define a new command parser
*	adic_defdes	- Define a destructor for a class
*	adic_deffun	- Define a function
* 	adic_defgdp	- Define generic dispatch procedure
*	adic_defgen	- Define a generic function
*	adic_defmcf	- Define a method combination form (no Fortran
*			  equivalent)
*	adic_defmth	- Define a method
*	adic_defprt	- Define class printer method
*       adic_defrep	- Define a new file representation
*	adic_defvar	- Define a new variable value
*	adic_dervd	- Is an object derived from specified class?
*	adic_loccls	- Locate a class by name
*
*      Method execution :
*
*       adic_exec	- execute named method
*       adic_exec2	- execute named method with 2 arguments
*	adic_execi	- execute named method (ADI string)
*	adic_calnxt	- invoke next method in method hierarchy
*	adic_nulmth	- Null method with standard argument list
*
*      System method interfaces :
*
*	adic_ccopy	- Copy object component to output object component
*	adic_copy	- Make a copy of an objects data
*	adic_print	- Text representation of object
*
*      Data creation :
*
*       adic_[c]new      - Create n-D object [component] of named type
*       adic_[c]new<t>   - Create n-D object [component] of type <t>
*       adic_[c]new0     - Create scalar object [component] of named type
*       adic_[c]new0<t>  - Create scalar object [component] of type <t>
*       adic_[c]new1     - Create 1-D object [component] of named type
*       adic_[c]new1<t>  - Create 1-D object [component] of type <t>
*	adic_newlst	 - Create new list object
*	adic_newref	 - Create a reference object
*       adic_[c]newv<t>  - Create n-D object [component] with value
*       adic_[c]newv0<t> - Create scalar object [component] with value
*       adic_[c]newv1<t> - Create 1-D object [component] with value
*
*      Data access :
*
*	adic_find	 - Locate object component
*	adic_findi	 - Locate object component with ADI name
*       adic_[c]get	 - Get n-D object [component] values with user type
*       adic_[c]get<t>	 - Get n-D object [component] values
*       adic_[c]get0	 - Get scalar object [component] value with user type
*       adic_[c]get0<t>	 - Get scalar object [component] value
*       adic_[c]get1	 - Get 1-D object [component] values with user type
*       adic_[c]get1<t>	 - Get 1-D object [component] values
*       adic_getlst      - Get an object [component] list components
*	adic_[c]getref	 - Get an object [component] reference
*	adic_[c]map	 - Map object [component] with user type
*	adic_[c]map<t>	 - Map object [component] with a specific type
*       adic_[c]put	 - Put n-D object [component] values with user type
*       adic_[c]put<t>	 - Put n-D object [component] values
*       adic_[c]put0	 - Put scalar object [component] value with user type
*       adic_[c]put0<t>	 - Put scalar object [component] value
*       adic_[c]put1	 - Put 1-D object [component] values with user type
*       adic_[c]put1<t>	 - Put 1-D object [component] values
*	adic_[c]putid	 - Put ADI object into object [component]
*	adic_[c]putref	 - Write an object [component] reference
*	adic_there	 - Does an object component exist?
*	adic_[c]unmap	 - Unmap object [component]
*
*      Enquiry routines :
*
*	adic_clen	 - Enquire string length
* 	adic_[c]shape	 - Enquire object [component] dimensions
*	adic_[c]size	 - Enquire object [component] number of elements
*	adic_[c]state	 - Enquire object [component] data state
* 	adic_name	 - Enquire object name
*	adic_[c]type	 - Enquire object [component] class
*
*      Changing object attributes :
*
*       adic_[c]alter    - Alter object [component] dimensionality
*       adic_[c]cell	 - Index an object [component] array cell
*       adic_[c]slice    - Access object [component] slice
*
*      Object destruction :
*
*	adic_[c]erase	 - Destroy an object [component]
*
*      Symbol packages :
*
*	adic_reqpkg	 - Load a package from the search path
*
*      Data system :
*
*       adic_fclone      - Create a new as a clone of an input
*       adic_fclose      - Close a file system object
*       adic_fcomit      - Comit any buffered file changes to disk
*       adic_fcreat      - Create a new file system object
*       adic_fopen       - Open existing file system object
*       adic_setlnk      - Link two objects

*      Exception handling :
*
*	adic_accpt	 - Accept an exception with given C name
*	adic_raise	 - Raise an exception with C name
*
*      Miscellaneous :
*
*	adic_link	 - Link an identifier to a name group
*	adic_flush	 - Erase all objects in a name group

*  Authors:
*     DJA: David J. Allan (JET-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     16-AUG-1994 (DJA):
*        Original version.
*     {enter_further_changes_here}
*

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/


#include <string.h>
#include <stdarg.h>

#include "asterix.h"
#include "aditypes.h"
#include "adikrnl.h"
#include "adimem.h"
#include "adiarray.h"
#include "adierror.h"
#include "adiexpr.h"
#include "adilist.h"
#include "adistrng.h"
#include "adipkg.h"
#include "adisyms.h"
#include "adifsys.h"

#include "adicface.h"                   /* Prototypes for this module */


/* -------------------------------------------------------------------------
 * ADI error handling
 * -------------------------------------------------------------------------
 * Interface to error handling. No checking of status or testing whether
 * initialised here. The latter point allows errors which occur during
 * initialisation to be inviestigated.
 */
void adic_erranl( ADIstatus status )
  {
  adix_erranl( status );
  }

void adic_errmsg( ADIstatype code, char *buf, int buflen )
  {
  adix_errmsg( code, buf, buflen );
  }

void adic_setecs( ADIstatype code, char *ctx, ADIstatus status, ... )
  {
  va_list	ap;

  va_start(ap,status);

  adix_setecs( code, ctx, _CSM, ap, status );

  va_end(ap);
  }

#ifdef NOEMS
void adic_errctx( char *buf, int buflen )
  {
  adix_errctx( buf, buflen );
  }
#endif


/* -------------------------------------------------------------------------
 * Property handling
 * -------------------------------------------------------------------------
 */
void adic_nprp( ADIobj id, int *nprp, ADIstatus status )
  {
  _chk_stat;

  adix_nprp( id, nprp, status );

  _ERR_REP( "adic_nprp", Estr__GetNumPrp );
  }

void adic_indprp( ADIobj id, int index, ADIobj *pid, ADIstatus status )
  {
  _chk_stat;

  adix_indprp( id, index, pid, status );

  _ERR_REP("adic_indprp",Estr__IndObjPrp);
  }

/* -------------------------------------------------------------------------
 * Structure handling
 * -------------------------------------------------------------------------
 */
void adic_ncmp( ADIobj id, int *ncmp, ADIstatus status )
  {
  _chk_stat;

  adix_ncmp( id, ncmp, status );

  _ERR_REP( "adic_ncmp", Estr__GetNumCmp );
  }

void adic_indcmp( ADIobj id, int index, ADIobj *cid, ADIstatus status )
  {
  _chk_stat;

  adix_indcmp( id, index, cid, status );

  _ERR_REP("adic_indcmp",Estr__IndStrCmp);
  }

/* -------------------------------------------------------------------------
 * Reference counts
 * -------------------------------------------------------------------------
 */

void adic_clone( ADIobj id, ADIobj *cid, ADIstatus status )
  {
  _chk_stat;

  *cid = adix_clone( id, status );

  _ERR_REP("adic_clone",Estr__CloObjId);
  }

void adic_refadj( ADIobj id, int incr, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

  adix_refadj( id, incr, status );      /* Increment reference count */

  _ERR_REP( "adic_refadj", Estr__AdjObjRef );
  }

void adic_refcnt( ADIobj id, int *cnt, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

  *cnt = adix_refcnt( id, status );     /* Retrieve reference count */

  _ERR_REP( "adic_refcnt", Estr__GetObjRef );
  }


/* -------------------------------------------------------------------------
 * Data system definitions
 * -------------------------------------------------------------------------
 */

void adic_defcac( ADIobj clsid, ADIinteger number, ADIstatus status )
  {
  _chk_stat;                 	      	/* Standard checks */

  ADIdefClassCluster( clsid, number, status );

  _ERR_REP( "adic_defcac", Estr__DefClsCac);
  }

void adic_defcls( char *name, char *parents,
		  char *members, ADIobj *tid, ADIstatus status )
  {
  _chk_stat;                 		/* Standard checks */

  ADIdefClass_e( name, _CSM, parents, _CSM, members, _CSM, tid, status );

  _ERR_REP( "adic_defcls", Estr__DefCls);
  }

void adic_defcpa( char *name, ADIcMethodCB rtn,
		  ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_defcpa( name, _CSM,
	       ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
	       id, status );

  _ERR_REP("adic_defcpa",Estr__DefCpa);
  }

void adic_defdes( ADIobj clsid, ADIcMethodCB rtn, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  ADIkrnlDefDestruc( clsid,
		     ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
		     status );

  _ERR_REP("adic_defdes",Estr__DefClsDes);
  }

void adic_deffun( char *spec, ADIcMethodCB rtn,
		  ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_deffun( spec, _CSM,
	       ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
	       id, status );

  _ERR_REP("adic_deffun",Estr__DefFun);
  }

void adic_defgdp( ADIobj genid, ADIcGenericDispatchCB rtn, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_defgdp( genid,			/* Invoke kernel routine */
	       ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
	       status );

  _ERR_REP( "adic_defgdp", Estr__DefGenDis );
  }

void adic_defgen( char *spec, char *options, ADIcGenericDispatchCB rtn,
		  ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

/* Invoke kernel routine */
  adix_defgen( spec, _CSM, options, _CSM,
	       ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
	       id, status );

  _ERR_REP("adic_defgen",Estr__DefGen);
  }

void adic_defmcf( char *name, ADIcMethodCombinationCB rtn,
		  ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_defmcf( name, _CSM,     /* Invoke kernel routine */
	       ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
	       id, status );

  _ERR_REP("adic_defmcf",Estr__DefMcf);
  }

void adic_defmth( char *spec, ADIcMethodCB rtn,
		  ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_defmth( spec, _CSM,
	       ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
	       id, status );

  _ERR_REP("adic_defmth",Estr__DefMth);
  }

void adic_defprt( ADIobj clsid, ADIcMethodCB rtn, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  ADIkrnlDefPrnt( clsid,
		  ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
		  status );

  _ERR_REP("adic_defprt",Estr__DefClsPrt);
  }

void adic_defrep( char *name, ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_defrep( name, _CSM, id, status );/* Invoke kernel routine */

  _ERR_REP( "adic_defrep", Estr__DefFilRep );
  }

void adic_defvar( char *name, ADIlogical global, ADIobj id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_defvar( name, _CSM, global, id, status );

  _ERR_REP( "adic_defvar", Estr__DefVar );
  }

void adic_dervd( ADIobj id, char *name, ADIlogical *der, ADIstatus status )
  {
  _chk_stat;

  *der = ADIkrnlChkDerived( id, name, _CSM, status );

  _ERR_REP( "adic_dervd", Estr__TstObjDer );
  }

void adic_loccls( char *cls, ADIobj *clsid, ADIstatus status )
  {
  _chk_stat;

  *clsid = ADIkrnlFindClsExt( cls, _CSM, status );

  _ERR_REP( "adic_loccls", Estr__LocClsDef );
  }


/* -------------------------------------------------------------------------
 * Method execution
 * -------------------------------------------------------------------------
 */

void adic_calnxt( ADIstatus status )
  {
  if ( _ok(status) )
    *status = ADI__CALNXTMTH;
  }

void adic_exec( char *func, int narg, ADIobj args[], ADIobj *res, ADIstatus status )
  {
  ADIobj	resid;

  _chk_stat;

  resid = adix_exec( func, _CSM, narg, args, status );

  if ( res )
    *res = resid;

  _ERR_REP( "adic_exec", Estr__ExeMth );
  }

void adic_exec2( char *func, ADIobj arg1, ADIobj arg2, ADIobj *res, ADIstatus status )
  {
  ADIobj	resid;

  _chk_stat;

  resid = adix_exec2( func, _CSM, arg1, arg2, status );

  if ( res )
    *res = resid;

  _ERR_REP( "adic_exec2", Estr__ExeMth );
  }

void adic_execi( ADIobj func, int narg, ADIobj args[], ADIobj *res, ADIstatus status )
  {
  ADIobj	resid;

  _chk_stat;

  resid = adix_execi( func, narg, args, status );

  if ( res )
    *res = resid;

  _ERR_REP( "adic_execi", Estr__ExeMth );
  }

ADIobj adic_nulmth( int narg, ADIobj args[], ADIstatus status )
  {
  return ADI__nullid;
  }


/* -------------------------------------------------------------------------
 * System method interfaces
 * -------------------------------------------------------------------------
 */

void adic_ccopy( ADIobj in, char *inmem, ADIobj out,
		 char *outmem, ADIstatus status )
  {
  _chk_stat;

  adix_ccopy( in, inmem, _CSM, out, outmem, _CSM, status );

  _ERR_REP("adic_ccopy", Estr__CopObjCmp);
  }


void adic_copy( ADIobj id, ADIobj *cid, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

  *cid = adix_copy( id, status );

  _ERR_REP( "adic_copy", Estr__CopObj );
  }

void adic_print( ADIobj id, ADIstatus status )
  {
  _chk_stat;

  adix_print( ADI_G_curint->StdOut, id, 0, ADI__false, status );

  _ERR_REP( "adic_print", Estr__PriObj );
  }

/* -------------------------------------------------------------------------
 * Data creation
 * -------------------------------------------------------------------------
 */

void adic_new( char *cls, int ndim, int dims[], ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  adix_newn( ADI__nullid, NULL, 0, cls, _CSM, ndim, dims, id, status );

  _ERR_REP("adic_new", Estr__CreObjDat);
  }

void adic_new0( char *cls, ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  adix_newn( ADI__nullid, NULL, 0, cls, _CSM, 0, NULL, id, status );

  _ERR_REP("adic_new0", Estr__CreObjDat);
  }

void adic_new1( char *cls, int nval, ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  adix_newn( ADI__nullid, NULL, 0, cls, _CSM, 1, &nval, id, status );

  _ERR_REP("adic_new1", Estr__CreObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_new,_t)( int ndim, int dims[], ADIobj *id, ADIstatus status )\
  { \
  _chk_stat; \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, ndim, dims, NULL, \
	&_TM_alloc(_t), 0, id, status ); \
  _ERR_REP(_TM_names(adic_new,_t),Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_new0,_t)( ADIobj *id, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, NULL, \
	&_TM_alloc(_t), 0, id, status ); \
  _ERR_REP(_TM_names(adic_new0,_t), Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_new1,_t)( int nval, ADIobj *id, ADIstatus status )\
  { \
  _chk_stat; \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 1, &nval, NULL, \
	&_TM_alloc(_t), 0, id, status ); \
  _ERR_REP(_TM_names(adic_new1,_t),Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_newv,_t)( int ndim, int dims[], _TM_ctype(_t) value[], ADIobj *id, ADIstatus status )\
  { \
  _chk_stat; \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, ndim, dims, value, \
	&_TM_alloc(_t), sizeof(_TM_ctype(_t)), id, status ); \
  _ERR_REP(_TM_names(adic_newv,_t),Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_newvc( int ndim, int dims[], char *value[], ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  adix_new_n( ADI__true, ADI__nullid, NULL, 0, ndim, dims, value,
	&_TM_alloc(c), _CSM, id, status );

  _ERR_REP("adic_newvc",Estr__CreObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_newv0,_t)( _TM_ctype(_t) value, ADIobj *id, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, &value, \
	&_TM_alloc(_t), sizeof(value), id, status ); \
  _ERR_REP(_TM_names(adic_newv0,_t),Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_newv0c( char *value, ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, &value,
	&_TM_alloc(c), _CSM, id, status );

  _ERR_REP("adic_newv0c",Estr__CreObjDat);
  }

void adic_new0c_n( int len, ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard checks */

/* Use user supplied length */
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, NULL,
	      &_TM_alloc(c), 0, id, status );

  if ( _ok(status) && (len>0) ) {
    ADIstring	*sdat = _str_data(*id);
    sdat->len = len;
    sdat->data = strx_alloc( len, status );
    _han_set(*id) = ADI__true;
    }

  _ERR_REP("adic_new0c_n",Estr__CreObjDat);
  }

void adic_newv0c_n( char *value, int len, ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard checks */

/* Use user supplied length */
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, value,
	      &_TM_alloc(c), len, id, status );

  _ERR_REP("adic_newv0c_n",Estr__CreObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_newv1,_t)( int nval, _TM_ctype(_t) value[], ADIobj *id, ADIstatus status )\
  { \
  _chk_stat; \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 1, &nval, value, \
	&_TM_alloc(_t), sizeof(_TM_ctype(_t)), id, status ); \
  _ERR_REP(_TM_names(adic_newv1,_t),Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_newv1c( int nval, char *value[], ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 1, &nval, value,
	      &_TM_alloc(c), _CSM, id, status );

  _ERR_REP("adic_newv1c",Estr__CreObjDat);
  }

/* -------------------------------------------------------------------------
 * Data access
 * -------------------------------------------------------------------------
 */

void adic_get( ADIobj id, char *type, int ndim, int dimx[],
	       void *value, int dims[], ADIstatus status )
  {
  _chk_stat;

  adix_get_nn( 1, id, NULL, 0, type, _CSM, ndim, dimx, value, dims, status );

  _ERR_REP( "adic_get", Estr__GetObjDat);
  }


#define _genproc(_t) \
void _TM_name(adic_get,_t)( ADIobj id, int ndim, int dimx[], \
		_TM_ctype(_t) *value, int dims[], ADIstatus status ) \
  { \
  _chk_stat; \
  adix_get_n( 1, id, NULL, 0, ndim, dimx, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), value, dims, status ); \
  _ERR_REP(_TM_names(adic_get,_t), Estr__GetObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_getc( ADIobj id, int ndim, int dimx[], int len,
		char *value, int dims[], ADIstatus status )
  {
  _chk_stat;

  adix_get_n( 1, id, NULL, 0, ndim, dimx, &_TM_alloc(c), len,
	      value, dims, status );

  _ERR_REP("adic_getc", Estr__GetObjDat);
  }

void adic_get0( ADIobj id, char *type, void *value, ADIstatus status )
  {
  _chk_stat;

  adix_get_nn( 1, id, NULL, 0, type, _CSM, 0, NULL, value, NULL, status );

  _ERR_REP( "adic_get0", Estr__GetObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_get0,_t)( ADIobj id, _TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_get_n( 1, id, NULL, 0, 0, NULL, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), value, NULL, status ); \
  _ERR_REP(_TM_names(adic_get0,_t), Estr__GetObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void _TM_name(adic_get0,c)( ADIobj id, int len, char *value, ADIstatus status )
  {
  _chk_stat;

  adix_get_n( 1, id, NULL, 0, 0, NULL, &_TM_alloc(c), len,
	      value, NULL, status );

  _ERR_REP("adic_get0c", Estr__GetObjDat);
  }

void adic_get1( ADIobj id, char *type, int mxval,
		void *value, int *nactval, ADIstatus status )
  {
  _chk_stat;

  adix_get_nn( 1, id, NULL, 0, type, _CSM, 1, &mxval, value, nactval, status );

  _ERR_REP( "adic_get1", Estr__GetObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_get1,_t)( ADIobj id, int mxval, \
		_TM_ctype(_t) *value, int *nactval, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_get_n( 1, id, NULL, 0, 1, &mxval, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), value, nactval, status ); \
  _ERR_REP(_TM_names(adic_get1,_t), Estr__GetObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_get1c( ADIobj id, int mxval, int len,
		 char *value, int *nactval, ADIstatus status )
  {
  _chk_stat;

  adix_get_n( 1, id, NULL, 0, 1, &mxval, &_TM_alloc(c), len,
	      value, nactval, status );

  _ERR_REP("adic_get1c", Estr__GetObjDat);
  }

void adic_map( ADIobj id, char *type, char *mode, void **vptr,
	       ADIstatus status )
  {
  _chk_stat;

  adix_map_t( 1, id, NULL, 0, type, _CSM, mode, _CSM, vptr, status );

  _ERR_REP("adic_map", Estr__MapObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_map,_t)( ADIobj id, char *mode, \
		_TM_ctype(_t) **vptr, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_map_n( 1, id, NULL, 0, mode, _CSM, &_TM_alloc(_t), \
	      sizeof(_TM_ctype(_t)), (void **) vptr, status ); \
  _ERR_REP(_TM_names(adic_map,_t), Estr__MapObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc


void adic_put0( ADIobj id, char *type, void *value, ADIstatus status )
  {
  _chk_stat;

  adix_put_nn( 1, id, NULL, 0, type, _CSM, 0, NULL,
	       value, status );

  _ERR_REP( "adic_put0", Estr__PutObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_put0,_t)( ADIobj id, _TM_ctype(_t) value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_put_n( 1, id, NULL, 0, 0, NULL, &_TM_alloc(_t), \
	     sizeof(value), &value, status ); \
  _ERR_REP(_TM_names(adic_put0,_t), Estr__PutObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_put0c( ADIobj id, char *value, ADIstatus status )
  {
  _chk_stat;

  adix_put_n( 1, id, NULL, 0, 0, NULL, &_TM_alloc(c),
	      _CSM, &value, status );

  _ERR_REP( "adic_put0c", Estr__PutObjDat );
  }

void adic_put( ADIobj id, char *type, int ndim, int dims[],
	       void *value, ADIstatus status )
  {
  _chk_stat;
  adix_put_nn( 1, id, NULL, 0, type, _CSM, ndim, dims, value, status );

  _ERR_REP( "adic_put", Estr__PutObjDat);
  }


#define _genproc(_t) \
void _TM_name(adic_put,_t)( ADIobj id, int ndim, int dims[], \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_put_n( 1, id, NULL, 0, ndim, dims, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, status ); \
  _ERR_REP(_TM_names(adic_put,_t), Estr__PutObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_putc( ADIobj id, int ndim, int dims[],
		 char **value, ADIstatus status )
  {
  _chk_stat;

  adix_put_n( 1, id, NULL, 0, ndim, dims, &_TM_alloc(c),
	      _CSM, value, status );

  _ERR_REP( "adic_putc", Estr__PutObjDat );
  }


void adic_put1( ADIobj id, char *type, int nval,
		void *value, ADIstatus status )
  {
  _chk_stat;
  adix_put_nn( 1, id, NULL, 0, type, _CSM, 1, &nval, value, status );

  _ERR_REP( "adic_put1", Estr__PutObjDat );
  }

#define _genproc(_t) \
void _TM_name(adic_put1,_t)( ADIobj id, int nval, \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_put_n( 1, id, NULL, 0, 1, &nval, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, status ); \
  _ERR_REP(_TM_names(adic_put1,_t), Estr__PutObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_put1c( ADIobj id, int nval,
		 char **value, ADIstatus status )
  {
  _chk_stat;

  adix_put_n( 1, id, NULL, 0, 1, &nval, &_TM_alloc(c),
	      _CSM, value, status );

  _ERR_REP( "adic_put1c", Estr__PutObjDat );
  }

void adic_unmap( ADIobj id, void *vptr, ADIstatus status )
  {
  _chk_stat;

  adix_unmap_n( id, NULL, 0, vptr, status );

  _ERR_REP( "adic_unmap", Estr__UnmObjDat );
  }

/* -------------------------------------------------------------------------
 * Component data creation
 * -------------------------------------------------------------------------
 */

void adic_cnew( ADIobj id, char *name, char *cls, int ndim, int dims[], ADIstatus status )
  {
  _chk_stat;

  adix_newn( id, name, _CSM, cls, _CSM, ndim, dims, NULL, status );

  _ERR_REP( "adic_cnew", Estr__CreObjDat );
  }

void adic_cnew0( ADIobj id, char *name, char *cls, ADIstatus status )
  {
  _chk_stat;

  adix_newn( id, name, _CSM, cls, _CSM, 0, NULL, NULL, status );

  _ERR_REP( "adic_cnew0", Estr__CreObjDat );
  }

void adic_cnew1( ADIobj id, char *name, char *cls, int nval, ADIstatus status )
  {
  _chk_stat;

  adix_newn( id, name, _CSM, cls, _CSM, 1, &nval, NULL, status );

  _ERR_REP( "adic_cnew1", Estr__CreObjDat );
  }

#define _genproc(_t) \
void _TM_name(adic_cnew,_t)( ADIobj id, char *name, int ndim, int dims[], ADIstatus status )\
  { \
  _chk_stat; \
  adix_new_n( ADI__true, id, name, _CSM, ndim, dims, NULL, \
		&_TM_alloc(_t), 0, NULL, status ); \
  _ERR_REP(_TM_names(adic_cnew,_t), Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_cnew0,_t)( ADIobj id, char *name, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_new_n( ADI__true, id, name, _CSM, 0, NULL, NULL, &_TM_alloc(_t), \
	      0, NULL, status ); \
  _ERR_REP(_TM_names(adic_cnew0,_t), Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_cnew1,_t)( ADIobj id, char *name, int nval, ADIstatus status )\
  { \
  _chk_stat; \
  adix_new_n( ADI__true, id, name, _CSM, 1, &nval, NULL, &_TM_alloc(_t), \
	      0, NULL, status ); \
  _ERR_REP(_TM_names(adic_cnew1,_t), Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_cnewv,_t)( ADIobj id, char *name, int ndim, int dims[], _TM_ctype(_t) value[], ADIstatus status )\
  { \
  _chk_stat; \
  adix_new_n( ADI__true, id, name, _CSM, ndim, dims, value, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), \
	     NULL, status ); \
  _ERR_REP(_TM_names(adic_cnewv,_t), Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cnewvc( ADIobj id, char *name, int ndim, int dims[], char *value[], ADIstatus status )
  {
  _chk_stat;

  adix_new_n( ADI__true, id, name, _CSM, ndim, dims, value, &_TM_alloc(c),
	      _CSM, NULL, status );

  _ERR_REP( "adic_cnewvc", Estr__CreObjDat );
  }

#define _genproc(_t) \
void _TM_name(adic_cnewv0,_t)( ADIobj id, char *name, _TM_ctype(_t) value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_new_n( ADI__true, id, name, _CSM, 0, NULL, &value, &_TM_alloc(_t), \
	     sizeof(value), \
	     NULL, status ); \
  _ERR_REP(_TM_names(adic_cnewv0,_t), Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cnewv0c( ADIobj id, char *name, char *value, ADIstatus status )
  {
  _chk_stat;

  adix_new_n( ADI__true, id, name, _CSM, 0, NULL, &value,
	&_TM_alloc(c), _CSM, NULL, status );

  _ERR_REP( "adic_cnewv0c", Estr__CreObjDat );
  }

void adic_cnewv0c_n( ADIobj id, char *name, _TM_ctype(c) value, int len,
		     ADIstatus status )
  {
  _chk_stat;                 /* Standard checks */

  adix_new_n( ADI__true, id, name, _CSM, 0, NULL, &value,
	      &_TM_alloc(c), len, NULL, status );

  _ERR_REP( "adic_cnewv0c_n", Estr__CreObjDat );
  }

#define _genproc(_t) \
void _TM_name(adic_cnewv1,_t)( ADIobj id, char *name, int nval, _TM_ctype(_t) value[], ADIstatus status )\
  { \
  _chk_stat; \
  adix_new_n( ADI__true, id, name, _CSM, 1, &nval, value, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), NULL, status ); \
  _ERR_REP(_TM_names(adic_cnewv1,_t), Estr__CreObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cnewv1c( ADIobj id, char *name, int nval, char *value[], ADIstatus status )
  {
  _chk_stat;

  adix_new_n( ADI__true, id, name, _CSM, 1, &nval, value, &_TM_alloc(c),
	      _CSM, NULL, status );

  _ERR_REP( "adic_cnewv1c", Estr__CreObjDat );
  }

/* -------------------------------------------------------------------------
 * Component data access
 * -------------------------------------------------------------------------
 */

void adic_cget( ADIobj id, char *name, char *type, int ndim, int dimx[],
	        void *value, int dims[], ADIstatus status )
  {
  _chk_stat;

  adix_get_nn( 1, id, name, _CSM, type, _CSM, ndim, dimx,
	       value, dims, status );

  _ERR_REP( "adic_cget", Estr__GetObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_cget,_t)( ADIobj id, char *name, int ndim, int dims[], \
	_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_get_n( 1, id, name, _CSM, ndim, dims, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), value, NULL, status ); \
  _ERR_REP(_TM_names(adic_cget,_t),Estr__GetObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cgetc( ADIobj id, char *name, int ndim, int dimx[], int len,
		 char *value, int dims[], ADIstatus status )
  {
  _chk_stat;

  adix_get_n( 1, id, name, _CSM, ndim, dimx, &_TM_alloc(c),
	      len, value, dims, status );

  _ERR_REP("adic_cgetc", Estr__GetObjDat);
  }

void adic_cget0( ADIobj id, char *name, char *type, void *value,
                 ADIstatus status )
  {
  _chk_stat;

  adix_get_nn( 1, id, name, _CSM, type, _CSM, 0, NULL, value, NULL, status );

  _ERR_REP( "adic_cget0", Estr__GetObjDat );
  }

#define _genproc(_t) \
void _TM_name(adic_cget0,_t)( ADIobj id, char *name, _TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_get_n( 1, id, name, _CSM, 0, NULL, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), value, NULL, status ); \
  _ERR_REP(_TM_names(adic_cget0,_t),Estr__GetObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cget0c( ADIobj id, char *name, int len, char *value, ADIstatus status )
  {
  _chk_stat;

  adix_get_n( 1, id, name, _CSM, 0, NULL, &_TM_alloc(c),
	      len, value, NULL, status );

  _ERR_REP("adic_cget0c", Estr__GetObjDat);
  }


void adic_cget1( ADIobj id, char *name, char *type, int mxval,
	         void *value, int *nactval, ADIstatus status )
  {
  _chk_stat;

  adix_get_nn( 1, id, name, _CSM, type, _CSM, 1, &mxval, value,
               nactval, status );

  _ERR_REP(_TM_names(adic_cget1,_t),Estr__GetObjDat);
  }


#define _genproc(_t) \
void _TM_name(adic_cget1,_t)( ADIobj id, char *name, int mxval, \
	_TM_ctype(_t) *value, int *nactval, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_get_n( 1, id, name, _CSM, 1, &mxval, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, nactval, status ); \
  _ERR_REP(_TM_names(adic_cget1,_t),Estr__GetObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cget1c( ADIobj id, char *name, int mxval, int len,
		  char *value, int *nactval, ADIstatus status )
  {
  _chk_stat;

  adix_get_n( 1, id, name, _CSM, 1, &mxval, &_TM_alloc(c),
	      len, value, nactval, status );

  _ERR_REP("adic_cget1c", Estr__GetObjDat);
  }

void adic_cmap( ADIobj id, char *name, char *type, char *mode,
		void **vptr, ADIstatus status )
  {
  _chk_stat;

  adix_map_t( 1, id, name, _CSM, type, _CSM,
	      mode, _CSM, vptr, status );

  _ERR_REP("adic_cmap",Estr__MapObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_cmap,_t)( ADIobj id, char *name, char *mode, \
		_TM_ctype(_t) **vptr, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_map_n( 1, id, name, _CSM, mode, _CSM, \
	      &_TM_alloc(_t), sizeof(_TM_ctype(_t)), \
	      (void **) vptr, status ); \
  _ERR_REP(_TM_names(adic_cmap,_t),Estr__MapObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

void adic_cput( ADIobj id, char *name, char *type, int ndim, int dims[],
		void *value, ADIstatus status )
  {
  _chk_stat;
  adix_put_nn( 1, id, name, _CSM, type, _CSM, ndim, dims, value, status );
  _ERR_REP( "adic_cput", Estr__PutObjDat );
  }

#define _genproc(_t) \
void _TM_name(adic_cput,_t)( ADIobj id, char *name, int ndim, int dims[], \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_put_n( 1, id, name, _CSM, ndim, dims, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), value, status ); \
  _ERR_REP(_TM_names(adic_cput,_t), Estr__PutObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cputc( ADIobj id, char *name, int ndim, int dims[],
		 char **value, ADIstatus status )
  {
  _chk_stat;

  adix_put_n( 1, id, name, _CSM, ndim, dims, &_TM_alloc(c),
	      _CSM, value, status );

  _ERR_REP("adic_cputc", Estr__PutObjDat);
  }

void adic_cput0( ADIobj id, char *name, char *type, void *value,
                 ADIstatus status )
  {
  _chk_stat;

  adix_put_nn( 1, id, name, _CSM, type, _CSM, 0, NULL, value, status );

  _ERR_REP(_TM_names(adic_cput0,_t), Estr__PutObjDat);
  }


#define _genproc(_t) \
void _TM_name(adic_cput0,_t)( ADIobj id, char *name, _TM_ctype(_t) value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_put_n( 1, id, name, _CSM, 0, NULL, &_TM_alloc(_t), \
	     sizeof(value), &value, status ); \
  _ERR_REP(_TM_names(adic_cput0,_t), Estr__PutObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cput0c( ADIobj id, char *name, char *value, ADIstatus status )
  {
  _chk_stat;

  adix_put_n( 1, id, name, _CSM, 0, NULL, &_TM_alloc(c),
	      _CSM, &value, status );

  _ERR_REP("adic_cput0c", Estr__PutObjDat);
  }


void adic_cput1( ADIobj id, char *name, char *type, int nval,
		 void *value, ADIstatus status )
  {
  _chk_stat;

  adix_put_nn( 1, id, name, _CSM, type, _CSM, 1, &nval, value, status );

  _ERR_REP( "adic_cput1", Estr__PutObjDat);
  }

#define _genproc(_t) \
void _TM_name(adic_cput1,_t)( ADIobj id, char *name, int nval, \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_stat; \
  adix_put_n( 1, id, name, _CSM, 1, &nval, &_TM_alloc(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, status ); \
  _ERR_REP(_TM_names(adic_cput1,_t), Estr__PutObjDat);}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cput1c( ADIobj id, char *name, int nval,
		  char **value, ADIstatus status )
  {
  _chk_stat;

  adix_put_n( 1, id, name, _CSM, 1, &nval, &_TM_alloc(c), _CSM,
	      value, status );

  _ERR_REP("adic_cput1c", Estr__PutObjDat);
  }

void adic_putid( ADIobj id, ADIobj vid, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_putid( id, NULL, 0, vid, status );

  _ERR_REP("adic_putid", Estr__PutObjDat);
  }

void adic_cputid( ADIobj id, char *name, ADIobj vid, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_putid( id, name, _CSM, vid, status );

  _ERR_REP("adic_cputid", Estr__PutObjDat);
  }

void adic_cunmap( ADIobj id, char *name, void *vptr, ADIstatus status )
  {
  _chk_stat;

  adix_unmap_n( id, name, _CSM, vptr, status );

  _ERR_REP("adic_cunmap", Estr__UnmObjDat);
  }

void adic_find( ADIobj id, char *name, ADIobj *cid, ADIstatus status )
  {
  _chk_stat;

  *cid = adix_find( id, name, _CSM, status );

  _ERR_REP( "adic_find", Estr__LocObjCmp );
  }

void adic_findi( ADIobj id, ADIobj name, ADIobj *cid, ADIstatus status )
  {
  _chk_stat;

  *cid = adix_findi( id, name, status );

  _ERR_REP( "adic_findi", Estr__LocObjCmp );
  }

void adic_there( ADIobj id, char *name, ADIlogical *there, ADIstatus status )
  {
  _chk_stat;

  *there = adix_there( id, name, _CSM, status );

  _ERR_REP( "adic_there", Estr__TstObjExi);
  }

/* -------------------------------------------------------------------------
 * Object enquiry
 * -------------------------------------------------------------------------
 */
void adic_clen( ADIobj id, ADIinteger *clen, ADIstatus status )
  {
  _chk_stat;             /* Standard entry checks */

  ADIstrngGetLen( id, clen, status );

  _ERR_REP( "adic_clen", Estr__GetObjShp );
  }

void adic_cshape( ADIobj id, char *name, int mxndim, int dims[], int *ndim, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_shape( id, name, _CSM, mxndim, dims, ndim, status );

  _ERR_REP( "adic_cshape", Estr__GetObjShp );
  }

void adic_csize( ADIobj id, char *name, int *nelm, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_size( id, name, _CSM, nelm, status );

  _ERR_REP( "adic_csize", Estr__GetObjShp );
  }

void adic_cstate( ADIobj id, char *name, ADIlogical *state,
		  ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  *state = adix_state( id, name, _CSM, status );

  _ERR_REP( "adic_cstate", Estr__TstObjSta );
  }

void adic_ctype( ADIobj id, char *name, int blen, char *buf,
		 ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

  ADIstrngExport( adix_qcls( id, name, _CSM, status ), ADI__true,
		  buf, blen, status );

  _ERR_REP( "adic_ctype", Estr__GetObjTyp );
  }


void adic_name( ADIobj id, int blen, char *buf, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Construct object name */
  adix_name( id, ADI__true, buf, blen, status );

  _ERR_REP( "adic_name", Estr__GetObjNam );
  }

void adic_shape( ADIobj id, int mxndim, int dims[], int *ndim, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_shape( id, NULL, 0, mxndim, dims, ndim, status );

  _ERR_REP( "adic_shape", Estr__GetObjShp );
  }

void adic_size( ADIobj id, int *nelm, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_size( id, NULL, 0, nelm, status );

  _ERR_REP( "adic_csize", Estr__GetObjShp );
  }

void adic_state( ADIobj id, ADIlogical *state, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  *state = adix_state( id, NULL, 0, status );

  _ERR_REP( "adic_state", Estr__TstObjSta );
  }

void adic_type( ADIobj id, int blen, char *buf, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

  ADIstrngExport( adix_qcls( id, NULL, 0, status ), ADI__true,
		  buf, blen, status );

  _ERR_REP( "adic_type", Estr__GetObjTyp );
  }

/* -------------------------------------------------------------------------
 * Changing object attributes
 * -------------------------------------------------------------------------
 */
void adic_alter( ADIobj id, int ndim, int dims[], ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  ADIaryAlter( id, NULL, 0, ndim, dims, status );

  _ERR_REP( "adic_alter", Estr__AltObjShp );
  }

void adic_calter( ADIobj id, char *name, int ndim, int dims[], ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  ADIaryAlter( id, name, _CSM, ndim, dims, status );

  _ERR_REP( "adic_calter", Estr__AltObjShp );
  }

void adic_ccell( ADIobj id, char *name, int ndim, int index[],
		 ADIobj *cid, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_cell( id, name, _CSM, ndim, index, cid, status );

  _ERR_REP( "adic_ccell", Estr__LocObjCel );
  }

void adic_cell( ADIobj id, int ndim, int index[],
		ADIobj *cid, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_cell( id, NULL, 0, ndim, index, cid, status );

  _ERR_REP( "adic_cell", Estr__LocObjCel );
  }

void adic_cslice( ADIobj id, char *name, int ndim, int diml[], int dimu[],
		  ADIobj *sid, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_slice( id, name, _CSM, ndim, diml, dimu, sid, status );

  _ERR_REP( "adic_cslice", Estr__LocObjSli );
  }

void adic_slice( ADIobj id, int ndim, int diml[], int dimu[], ADIobj *sid,
		 ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_slice( id, NULL, 0, ndim, diml, dimu, sid, status );

  _ERR_REP( "adic_slice", Estr__LocObjSli );
  }

/* -------------------------------------------------------------------------
 * Object destruction
 * -------------------------------------------------------------------------
 */

void adic_erase( ADIobj *id, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

  adix_erase( id, status );

  _ERR_REP( "adic_erase", Estr__DelObj );
  }

void adic_cerase( ADIobj id, char *name, ADIstatus status )
  {
  _chk_stat;             		/* Standard entry checks */

  adix_cerase( id, name, _CSM, status );/* Invoke kernel routine */

  _ERR_REP( "adic_cerase", Estr__DelObjCmp );
  }

/* -------------------------------------------------------------------------
 * Symbol packages
 * -------------------------------------------------------------------------
 */

void adic_reqpkg( char *pkg, ADIstatus status )
  {
  _chk_stat;

  ADIpkgRequire( pkg, _CSM, status );

  _ERR_REP( "adic_reqpkg", Estr__LodDefPkg );
  }

/* -------------------------------------------------------------------------
 * List objects
 * -------------------------------------------------------------------------
 */

void adic_lstnth( ADIobj lid, ADIinteger n, ADIobj *eid, ADIstatus status )
  {
  ADIobj 	*eaddr;

  _chk_stat;

  eaddr = lstx_nth( lid, n, status );

  *eid = eaddr ? *eaddr : ADI__nullid;

  _ERR_REP( "adic_lstnth", Estr__GetObjDat );
  }


void adic_newlst( ADIobj aid, ADIobj bid, ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  *id = lstx_cell( aid, bid, status );

  _ERR_REP( "adic_newlst", Estr__CreObjDat );
  }


void adic_getlst( ADIobj id, ADIobj *aid, ADIobj *bid, ADIstatus status )
  {
  _chk_init;

  _GET_CARCDR(*aid,*bid,id);

  _ERR_REP( "adic_getlst", Estr__GetObjDat );
  }


/* -------------------------------------------------------------------------
 * Object references
 * -------------------------------------------------------------------------
 */
void adic_cgetref( ADIobj id, char *name, ADIobj *rid, ADIstatus status )
  {
  _chk_stat;

  adix_get_n( 1, id, name, _CSM, 0, NULL, &UT_cid_ref, sizeof(ADIobj),
	      rid, NULL, status );

  _ERR_REP( "adic_cgetref", Estr__GetRefObj );
  }

void adic_cputref( ADIobj id, char *name, ADIobj rid, ADIstatus status )
  {
  _chk_stat;

  adix_put_n( 1, id, name, _CSM, 0, NULL, &UT_cid_ref,
	      sizeof(ADIobj), &rid, status );

  _ERR_REP( "adic_cputref", Estr__PutRefObj );
  }

void adic_getref( ADIobj id, ADIobj *rid, ADIstatus status )
  {
  _chk_stat;

  adix_get_n( 1, id, NULL, 0, 0, NULL, &UT_cid_ref, sizeof(ADIobj),
	      rid, NULL, status );

  _ERR_REP( "adic_getref", Estr__GetRefObj );
  }

void adic_newref( ADIobj rid, ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, 0, &rid, &UT_cid_ref,
	      sizeof(ADIobj), id, status );

  _ERR_REP( "adic_newref", Estr__CreRef );
  }

void adic_putref( ADIobj id, ADIobj rid, ADIstatus status )
  {
  _chk_stat;

  adix_put_n( 1, id, NULL, 0, 0, NULL, &UT_cid_ref,
	      sizeof(ADIobj), &rid, status );

  _ERR_REP( "adic_putref", Estr__PutRefObj );
  }


/* -------------------------------------------------------------------------
 * Data system
 * -------------------------------------------------------------------------
 */
void adic_fclone( ADIobj id, char *fspec, char *cls, ADIobj *fid,
		  ADIstatus status )
  {
  _chk_stat;

  adix_fclone( id, fspec, _CSM, cls, _CSM, fid, status );

  _ERR_REP( "adic_fclone", Estr__CreFilObj );
  }

void adic_fclose( ADIobj id, ADIstatus status )
  {
  _chk_stat;

  ADIfsysFileClose( id, status );

  _ERR_REP( "adic_fclose", Estr__CloFilObj );
  }

void adic_fcomit( ADIobj id, ADIstatus status )
  {
  _chk_stat;

  ADIfsysFileComit( id, status );

  _ERR_REP( "adic_fcomit", Estr__ComFilObj );
  }

void adic_fcreat( char *fspec, ADIobj id, ADIobj *fid,
		  ADIstatus status )
  {
  _chk_stat;

  adix_fcreat( fspec, _CSM, id, fid, status );

  _ERR_REP( "adic_fcreat", Estr__CreFilObj );
  }

void adic_fopen( char *fspec, char *cls, char *mode, ADIobj *id,
		 ADIstatus status )
  {
  _chk_stat;

  adix_fopen( fspec, _CSM, cls, _CSM, mode, _CSM, id, status );

  _ERR_REP( "adic_fopen", Estr__OpeFilObj );
  }

void adic_setlnk( ADIobj id, ADIobj lid, ADIstatus status )
  {
  ADIobj	args[2];

  _chk_stat;

  args[0] = id; args[1] = lid;

  adix_base_SetLink( 2, args, status );

  _ERR_REP( "adic_setlnk", Estr__LnkFilObj );
  }

/*
 void adic_unlnk( ADIobj id, ADIstatus status )
  {
  _chk_stat;

  adix_unlnk( id, status );

  _ERR_REP( "adic_unlnk", Estr__UlnFilObj );
  } */


/* -------------------------------------------------------------------------
 * Exception handling
 * -------------------------------------------------------------------------
 */

void adic_accpt( char *except, ADIstatus status )
  {
  ADIexecAccept( except, _CSM, status );
  }

void adic_raise( char *except, ADIstatype code, char *msg, ADIstatus status )
  {
  ADIexecRaise( except, _CSM, code, msg, _CSM, status );
  }


/* -------------------------------------------------------------------------
 * Miscellaneous
 * -------------------------------------------------------------------------
 */

void adic_flush( char *grp, ADIstatus status )
  {
  _chk_stat;

  adix_id_flush( grp, _CSM, status );

  _ERR_REP( "adic_flush", Estr__FshObjGrp );
  }

void adic_link( ADIobj id, char *grp, ADIstatus status )
  {
  _chk_stat;

  adix_id_link( id, grp, _CSM, status );

  _ERR_REP( "adic_link", Estr__LnkObjGrp );
  }


/* ----------------- unincorporated routines --------------- */

void adic_defrcb( ADIobj rid, char *name, ADICB rtn, ADIstatus status )
  {
  _chk_stat;                 /* Standard entry checks */

  adix_defrcb( rid, name, _CSM, /* Invoke kernel routine */
	       ADIkrnlNewEproc( ADI__true, (ADICB) rtn, status ),
	       status );
  }

void adic_getfile( ADIobj id, ADIobj *fid, ADIstatus status )
  {
  _chk_stat;

  adix_getfile( id, fid, status );
  }

void adic_getlink( ADIobj id, ADIobj *lid, ADIstatus status )
  {
  _chk_stat;

  *lid = adix_getlink( id, status );
  }

void adic_getpath( ADIobj id, int mxlen, char *buf, ADIstatus status )
  {
  int   actlen;

  _chk_stat;

  adix_getpath( id, ADI__true, mxlen, buf, &actlen, status );
  }

void adic_locrcb( ADIobj rid, char *name, ADIobj *rtn, ADIstatus status )
  {
  _chk_stat;                 /* Standard entry checks */

  adix_locrcb( rid, name, _CSM, rtn, status );
  }

void adic_locrep( char *name, ADIobj *id, ADIstatus status )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_locrep( name, _CSM, id, status );
  }

void adic_cmnstr( char *string, ADIobj *id, ADIstatus status )
  {
  _chk_stat;

  *id = adix_cmnC( string, status );

  _ERR_REP( "adic_cmnstr", Estr__DefCst );
  }

void adic_probe( ADIstatus status )
  {
  adix_probe( status );
  }



void adic_eprsc( char *string, ADIobj grammar, ADIobj *expr, ADIstatus status )
/*
*+
*  Name:
*     adic_eprsc

*  Purpose:
*     Parse an expression from a null terminated string for a given grammar

*  Language:
*     C

*  Invocation:
*     (void) adic_eprsc( string, grammar, &expr, status )

*  Description:
*     {routine_description}

*  Arguments:
*     string = char * (given)
*        The string containing the expression to be parsed
*     grammar = ADIobj (given)
*        The grammar used to parse the string. Currently not used
*     expr = ADIobj * (returned)
*	 The parsed expression
*     status = ADIstatus (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {routine_references}...

*  Keywords:
*     adi, adi:c, expression, parsing, grammar

*  Copyright:
*     Copyright University of Birmingham 1995.

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      4 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*.
*/
  {
  _chk_stat;

  *expr = ADIexprParseString( string, _CSM, grammar, status );

  _ERR_REP( "adic_eprsc", Estr__PrsExp );
  }

void adic_eeval( ADIobj expr, ADIobj symlist, ADIlogical ownres,
		 ADIobj *value, ADIstatus status )
  {
  _chk_stat;

  *value = ADIexprEval( expr, symlist, ownres, status );

  _ERR_REP( "adic_eeval", Estr__EvlExp );
  }


void adic_fexec( char *func, int narg, ADIobj args[], ADIobj *res, ADIstatus status )
  {
  ADIobj	resid;

  _chk_stat;

  resid = adix_fexec( func, _CSM, narg, args, status );

  if ( res )
    *res = resid;

  _ERR_REP( "adic_fexec", Estr__ExeFun );
  }
