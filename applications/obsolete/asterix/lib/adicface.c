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
*	adic_errctx	- return error context string
* 	adic_errmsg	- return error message given code
*	adic_setec	- set error code
*	adic_setecs	- set error code and context string
* 	adic_setes	- set context string
*	adic_setetc	- set character error token
*	adic_seteti	- set integer error token
*
*      Property handling :
*
*	adic_delprp	- delete named property
*	adic_locprp	- locate named property
*       adic_nprp	- return number of properties
*       adic_indprp	- locate property by number
*
*      Structure handling :
*
*	adic_delcmp	- delete named structure component
*	adic_loccmp	- locate named structure component
*	adic_ncmp	- number of components in structure
*	adic_indcmp	- locate structure component by number
*
*      Reference count :
*
*       adic_clone	- Clone an identifier
*	adic_refadj	- apply increment to object reference count
*	adic_refcnt	- return object reference count
*
*      Context manipulation :
*
*       adic_mark	- create new context
* 	adic_rlse	- release a context
*
*      Data system definitions :
*
*       adic_defcls	- define a new class
* 	adic_defgdp	- define generic dispatch procedure
*	adic_defgen	- define a generic function
*	adic_defmcf	- define a method combination form (no Fortran
*			  equivalent)
*	adic_defmth	- define a method
*       adic_defrep	- define a new file representation
*
*      Method execution :
*
*       adic_exec	- execute named method
*	adic_execi	- execute named method (ADI string)
*	adic_calnxt	- invoke next method in method hierarchy
*
*      System method interfaces :
*
*	adic_copy	- Make a copy of an objects data
*	adic_print	- Text representation of object
*	adic_setlnk	- Set ADI link field for ADIbase derived class pairs
*	adic_unlnk	- Reset ADI link field
*
*      Data creation :
*
*	adic_new	- Create new n-D object of named type
*       adic_new<t>	- Create new n-D primitive
*	adic_new0	- Create new scalar of named type
*       adic_new0<t>	- Create new scalar primitive
*	adic_new1	- Create new 1-D object of named type
*       adic_new1<t>	- Create new 1-D primitive
*       adic_newv<t>	- Create new n-D primitive with value
*       adic_newv0<t>	- Create new scalar primitive with value
*       adic_newv1<t>	- Create new 1-D primitive with value
*
*      Data access :
*
*       adic_get<t>	- Get n-D primitive values
*       adic_get0<t>	- Get scalar primitive value
*       adic_get1<t>	- Get 1-D primitive values
*	adic_map	- Map component with user specified type
*	adic_map<t>	- Map component with a specific type
*       adic_put<t>	- Put n-D primitive values
*       adic_put0<t>	- Put scalar primitive value
*       adic_put1<t>	- Put 1-D primitive values
*	adic_unmap	- Unmap object
*
*      Component data creation :
*
*	adic_cnew	- Create new n-D object of named type
*	adic_cnew0	- Create new scalar of named type
*	adic_cnew1	- Create new 1-D object of named type
*       adic_cnew<t>	- Create new n-D primitive
*       adic_cnew0<t>	- Create new scalar primitive
*       adic_cnew1<t>	- Create new 1-D primitive
*       adic_cnewv<t>	- Create new n-D primitive with value
*       adic_cnewv0<t>	- Create new scalar primitive with value
*       adic_cnewv1<t>	- Create new 1-D primitive with value
*
*      Component data access :
*
*       adic_cget<t>	- Get n-D primitive values
*       adic_cget0<t>	- Get scalar primitive value
*       adic_cget1<t>	- Get 1-D primitive values
*	adic_cmap	- Map component with user specified type
*	adic_cmap<t>	- Map component with a specific type
*       adic_cput<t>	- Put n-D primitive values
*       adic_cput0<t>	- Put scalar primitive value
*       adic_cput1<t>	- Put 1-D primitive values
*       adic_cset<t>	- Set n-D primitive values
*       adic_cset0<t>	- Set scalar primitive value
*       adic_cset1<t>	- Set 1-D primitive values
*	adic_cputid	- Put ADI object into object
*	adic_cunmap	- Unmap object component
*	adic_find	- Locate component data
*	adic_there	- Does a component exist?
*
*      Enquiry routines :
*
*	adic_class	- Enquire object class
* 	adic_cshape	- Enquire object component dimensions
* 	adic_csize	- Enquire object component number of elements
* 	adic_name	- Enquire object name
* 	adic_shape	- Enquire object dimensions
* 	adic_size	- Enquire object number of elements
*	adic_state	- Enquire object data state
*
*      Changing object attributes :
*
*       adic_alter	- Alter dimensionality
*       adic_calter	- Alter dimensionality of component
*       adic_ccell      - Index a cell of an array object component
*       adic_cell       - Index a cell of an array object
*       adic_cslice	- Access object component slice
*       adic_slice	- Access object slice
*
*      Object destruction :
*
*	adic_erase	- Destroy an object
*	adic_cerase	- Destroy member of property of object
*
*      Symbol packages :
*
*	adic_reqpkg	- Load a package from the search path
*
*      Data system :
*
*       adi_fclose      - Close a file system object
*       adi_fcreat      - Create a new file system object
*       adi_fopen       - Open existing file system object
*
*      Miscellaneous :
*
*	adic_link	- Link an identifier to a name group
*	adic_flush	- Erase all objects in a name group

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

#include "asterix.h"
#include "aditypes.h"
#include "adikrnl.h"
#include "adimem.h"
#include "adiarray.h"
#include "adierror.h"
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
void adic_errmsg( ADIstatype code, char *buf, int buflen )
  {
  adix_errmsg( code, buf, buflen );
  }

void adic_setec( ADIstatype code, ADIstatus status )
  {
  adix_setec( code, status );
  }

void adic_setecs( ADIstatype code, char *ctx, ADIstatus status )
  {
  adix_setecs( code, ctx, _CSM, status );
  }

void adic_setes( char *ctx, ADIstatus status )
  {
  adix_setes( ctx, _CSM, status );
  }

void adic_setetc( char *tok, char *val, int vlen )
  {
  adix_setetc( tok, val, vlen );
  }

void adic_seteti( char *tok, int val )
  {
  adix_seteti( tok, val );
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
void adic_delprp( ADIobj id, char *pname, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_delprp");

  adix_delprp( id, pname, _CSM, status );

  _ERR_OUT;
  }

void adic_locprp( ADIobj id, char *pname, ADIobj *pid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_locprp");

  adix_locprp( id, pname, _CSM, pid, status );

  _ERR_OUT;
  }

void adic_nprp( ADIobj id, int *nprp, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_nprp");

  adix_nprp( id, nprp, status );

  _ERR_OUT;
  }

void adic_indprp( ADIobj id, int index, ADIobj *pid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_indprp");

  adix_indprp( id, index, pid, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Structure handling
 * -------------------------------------------------------------------------
 */
void adic_delcmp( ADIobj id, char *cname, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_delcmp");

  adix_delcmp( id, cname, _CSM, status );

  _ERR_OUT;
  }

void adic_loccmp( ADIobj id, char *cname, ADIobj *cid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_loccmp");

  adix_loccmp( id, cname, _CSM, cid, status );

  _ERR_OUT;
  }

void adic_ncmp( ADIobj id, int *ncmp, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_ncmp");

  adix_ncmp( id, ncmp, status );

  _ERR_OUT;
  }

void adic_indcmp( ADIobj id, int index, ADIobj *cid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_indcmp");

  adix_indcmp( id, index, cid, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Reference counts
 * -------------------------------------------------------------------------
 */

void adic_clone( ADIobj id, ADIobj *cid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_clone");		/* Mark routine for error reporting */

  *cid = adix_clone( id, status );

  _ERR_OUT;
  }

void adic_refadj( ADIobj id, int incr, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_refadj");		/* Mark routine for error reporting */
  adix_refadj( id, incr, status );      /* Increment reference count */
  _ERR_OUT;
  }

void adic_refcnt( ADIobj id, int *cnt, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */
  _ERR_IN("adic_refcnt");		/* Mark routine for error reporting */

  *cnt = adix_refcnt( id, status );     /* Retrieve reference count */
  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Context management
 * -------------------------------------------------------------------------
 */

void adic_mark( void )
  {
  ADIstatype    lstatus = SAI__OK;
  ADIstatus     status = &lstatus;

  _chk_init;

  adix_mark();
  }

void adic_rlse( void )
  {
  ADIstatype    lstatus = SAI__OK;
  ADIstatus     status = &lstatus;

  _chk_init_err;

  if ( _ok(status) )
    adix_rlse();
  }

/* -------------------------------------------------------------------------
 * Data system definitions
 * -------------------------------------------------------------------------
 */

void adic_defcls( char *name, char *parents,
		  char *members, ADIobj *tid, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard checks */

  _ERR_IN("adic_defcls");		/* Mark routine for error reporting */

  ADIdefClass_e( name, _CSM, parents, _CSM,
	       members, _CSM, tid, status );

  _ERR_OUT;
  }

void adic_defgdp( ADIobj genid, ADIcGenericDispatchCB rtn, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  _ERR_IN("adic_defgdp");		/* Mark routine for error reporting */

  adix_defgdp( genid,			/* Invoke kernel routine */
	       adix_neweprc( ADI__true, (ADICB) rtn, status ),
	       status );

  _ERR_OUT;
  }

void adic_defgen( char *spec, char *options, ADIcGenericDispatchCB rtn,
		  ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  _ERR_IN("adic_defgen");		/* Mark routine for error reporting */

  adix_defgen( spec, _CSM, options,     /* Invoke kernel routine */
	       _CSM,
	       adix_neweprc( ADI__true, (ADICB) rtn, status ),
	       id, status );

  _ERR_OUT;
  }

void adic_defmcf( char *name, ADIcMethodCombinationCB rtn,
		  ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  _ERR_IN("adic_defmcf");		/* Mark routine for error reporting */

  adix_defmcf( name, _CSM,     /* Invoke kernel routine */
	       adix_neweprc( ADI__true, (ADICB) rtn, status ),
	       id, status );

  _ERR_OUT;
  }

void adic_defmth( char *spec, ADIcMethodCB rtn,
		  ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  _ERR_IN("adic_defmth");		/* Mark routine for error reporting */

  adix_defmth( spec, _CSM,     /* Invoke kernel routine */
	       adix_neweprc( ADI__true, (ADICB) rtn, status ),
	       id, status );

  _ERR_OUT;
  }

void adic_defrep( char *name, ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  _ERR_IN("adic_defrep");		/* Mark routine for error reporting */

  adix_defrep( name, _CSM, id, status );/* Invoke kernel routine */

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Method execution
 * -------------------------------------------------------------------------
 */

void adic_calnxt( ADIstatus status )
  {
  _chk_init_err;

  if ( _ok(status) )
    *status = ADI__CALNXTMTH;
  }

void adic_exec( char *func, int narg, ADIobj args[], ADIobj *res, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_exec");		/* Mark routine for error reporting */

  *res = adix_exec( func, _CSM, narg, args, status );

  _ERR_OUT;
  }

void adic_execi( ADIobj func, int narg, ADIobj args[], ADIobj *res, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_execi");		/* Mark routine for error reporting */

  *res = adix_execi( func, narg, args, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * System method interfaces
 * -------------------------------------------------------------------------
 */

void adic_copy( ADIobj id, ADIobj *cid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_copy");			/* Mark routine for error reporting */

  *cid = adix_copy( id, status );

  _ERR_OUT;
  }

void adic_print( ADIobj id, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  adix_print( ADIcvStdOut, id, 0, ADI__false, status );
  }

void adic_setlnk( ADIobj id, ADIobj lid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_setlnk");

  adix_setlnk( id, lid, status );

  _ERR_OUT;
  }

void adic_unlnk( ADIobj id, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_unlnk");		/* Mark routine for error reporting */

  adix_unlnk( id, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Data creation
 * -------------------------------------------------------------------------
 */

void adic_new( char *cls, int ndim, int dims[], ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_new");

  adix_newn( ADI__nullid, NULL, 0, cls, _CSM,
	     ndim, dims, id, status );

  _ERR_OUT;
  }

void adic_new0( char *cls, ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_new0");

  adix_newn( ADI__nullid, NULL, 0, cls, _CSM,
	     0, NULL, id, status );

  _ERR_OUT;
  }

void adic_new1( char *cls, int nval, ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_new1");

  adix_newn( ADI__nullid, NULL, 0, cls, _CSM,
	     1, &nval, id, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_new,_t)( int ndim, int dims[], ADIobj *id, ADIstatus status )\
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_new,_t)); \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, ndim, dims, NULL, _cdef_data(_TM_alloc(_t)), \
	      0, id, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_new0,_t)( ADIobj *id, ADIstatus status ) \
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_new0,_t)); \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, NULL, _cdef_data(_TM_alloc(_t)), \
	      0, id, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_new1,_t)( int nval, ADIobj *id, ADIstatus status )\
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_new1,_t)); \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 1, &nval, NULL, _cdef_data(_TM_alloc(_t)), \
	      0, id, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_newv,_t)( int ndim, int dims[], _TM_ctype(_t) value[], ADIobj *id, ADIstatus status )\
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_newv,_t)); \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, ndim, dims, value, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), \
	     id, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_newvc( int ndim, int dims[], charptr value[], ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_newvc");		/* Mark routine for error reporting */

  adix_new_n( ADI__true, ADI__nullid, NULL, 0, ndim, dims, value, _cdef_data(_TM_alloc(c)),
	      _CSM, id, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_newv0,_t)( _TM_ctype(_t) value, ADIobj *id, ADIstatus status ) \
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_newv0,_t)); \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, &value, _cdef_data(_TM_alloc(_t)), \
	     sizeof(value), \
	     id, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_newv0c( char *value, ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_newv0c");		/* Mark routine for error reporting */

  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, &value,
	_cdef_data(_TM_alloc(c)), _CSM, id, status );

  _ERR_OUT;
  }

void adic_newv0c_n( _TM_ctype(c) value, int len,
		    ADIobj *id, ADIstatus status )
  {
  _chk_stat; _chk_init;                 /* Standard checks */

  _ERR_IN("adic_newv0c_n");		/* Mark routine for error reporting */

  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 0, NULL, &value,/* Use user supplied length */
	      _cdef_data(_TM_alloc(c)), len, id, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_newv1,_t)( int nval, _TM_ctype(_t) value[], ADIobj *id, ADIstatus status )\
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_newv1,_t)); \
  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 1, &nval, value, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), \
	     id, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_newv1c( int nval, charptr value[], ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_newv1c");		/* Mark routine for error reporting */

  adix_new_n( ADI__true, ADI__nullid, NULL, 0, 1, &nval, value,
	      _cdef_data(_TM_alloc(c)), _CSM, id, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Data access
 * -------------------------------------------------------------------------
 */

#define _genproc(_t) \
void _TM_name(adic_get,_t)( ADIobj id, int ndim, int dims[], \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_get,_t)); \
  adix_get_n( 1, id, NULL, 0, ndim, dims, _TM_code(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_getc( ADIobj id, int ndim, int dims[], int len,
		char *value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_getc");			/* Mark routine for error reporting */

  adix_get_n( 1, id, NULL, 0, ndim, dims,
	      _TM_code(c), len,
	      value, NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_get0,_t)( ADIobj id, _TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_get0,_t)); \
  adix_get_n( 1, id, NULL, 0, 0, NULL, _TM_code(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, NULL, status ); \
  if ( !_ok(status) ) adic_setes( "Error reading object value", status );\
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void _TM_name(adic_get0,c)( ADIobj id, int len, char *value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_get0c");		/* Mark routine for error reporting */

  adix_get_n( 1, id, NULL, 0, 0, NULL, _TM_code(c), len,
	    value, NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_get1,_t)( ADIobj id, int mxval, \
		_TM_ctype(_t) *value, int *nactval, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_get1,_t)); \
  adix_get_n( 1, id, NULL, 0, 1, &mxval, _TM_code(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, nactval, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_get1c( ADIobj id, int mxval, int len,
		 char *value, int *nactval, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_get1c");		/* Mark routine for error reporting */

  adix_get_n( 1, id, NULL, 0, 1, &mxval,
	      _TM_code(c), len,
	      value, nactval, status );

  _ERR_OUT;
  }

void adic_map( ADIobj id, char *type, char *mode, void **vptr,
	       ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_map");

  adix_map_t( 1, id, NULL, 0, type, _CSM,
	      mode, _CSM, vptr, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_map,_t)( ADIobj id, char *mode, \
		_TM_ctype(_t) **vptr, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_map,_t)); \
  adix_map_n( 1, id, NULL, 0, mode, _CSM, \
	      _TM_code(_t), sizeof(_TM_ctype(_t)), \
	      (void **) vptr, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_put0,_t)( ADIobj id, _TM_ctype(_t) value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_put0,_t)); \
  adix_put_n( 1, id, NULL, 0, 0, NULL, _cdef_data(_TM_alloc(_t)), \
	     sizeof(value), \
	     &value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void _TM_name(adic_put0,c)( ADIobj id, char *value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_put0c");		/* Mark routine for error reporting */

  adix_put_n( 1, id, NULL, 0, 0, NULL, _cdef_data(_TM_alloc(c)),
	      _CSM, &value, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_put,_t)( ADIobj id, int ndim, int dims[], \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_put,_t)); \
  adix_put_n( 1, id, NULL, 0, ndim, dims, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), \
	     value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_putc( ADIobj id, int ndim, int dims[],
		 char **value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_putc");		/* Mark routine for error reporting */

  adix_put_n( 1, id, NULL, 0, ndim, dims, _cdef_data(_TM_alloc(c)),
	      _CSM, value, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_put1,_t)( ADIobj id, int nval, \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_put1,_t)); \
  adix_put_n( 1, id, NULL, 0, 1, &nval, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), \
	     value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_put1c( ADIobj id, int nval,
		 char **value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_put1c");		/* Mark routine for error reporting */

  adix_put_n( 1, id, NULL, 0, 1, &nval, _cdef_data(_TM_alloc(c)),
	      _CSM, value, status );

  _ERR_OUT;
  }

void adic_unmap( ADIobj id, void *vptr, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_unmap");

  adix_unmap_n( id, NULL, 0, vptr, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Component data creation
 * -------------------------------------------------------------------------
 */

void adic_cnew( ADIobj id, char *name, char *cls, int ndim, int dims[], ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_cnew");

  adix_newn( id, name, _CSM, cls, _CSM,
	     ndim, dims, NULL, status );

  _ERR_OUT;
  }

void adic_cnew0( ADIobj id, char *name, char *cls, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_cnew0");

  adix_newn( id, name, _CSM, cls, _CSM,
	     0, NULL, NULL, status );

  _ERR_OUT;
  }

void adic_cnew1( ADIobj id, char *name, char *cls, int nval, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_cnew1");

  adix_newn( id, name, _CSM, cls, _CSM,
	     1, &nval, NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cnew,_t)( ADIobj id, char *name, int ndim, int dims[], ADIstatus status )\
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_cnew,_t)); \
  adix_new_n( ADI__true, id, name, _CSM, ndim, dims, NULL, _cdef_data(_TM_alloc(_t)), \
	      0, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_cnew0,_t)( ADIobj id, char *name, ADIstatus status ) \
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_cnew0,_t)); \
  adix_new_n( ADI__true, id, name, _CSM, 0, NULL, NULL, _cdef_data(_TM_alloc(_t)), \
	      0, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_cnew1,_t)( ADIobj id, char *name, int nval, ADIstatus status )\
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_cnew1,_t)); \
  adix_new_n( ADI__true, id, name, _CSM, 1, &nval, NULL, _cdef_data(_TM_alloc(_t)), \
	      0, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_cnewv,_t)( ADIobj id, char *name, int ndim, int dims[], _TM_ctype(_t) value[], ADIstatus status )\
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_cnewv,_t)); \
  adix_new_n( ADI__true, id, name, _CSM, ndim, dims, value, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), \
	     NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cnewvc( ADIobj id, char *name, int ndim, int dims[], charptr value[], ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_cnewvc");		/* Mark routine for error reporting */

  adix_new_n( ADI__true, id, name, _CSM, ndim, dims, value, _cdef_data(_TM_alloc(c)),
	      _CSM, NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cnewv0,_t)( ADIobj id, char *name, _TM_ctype(_t) value, ADIstatus status ) \
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_cnewv0,_t)); \
  adix_new_n( ADI__true, id, name, _CSM, 0, NULL, &value, _cdef_data(_TM_alloc(_t)), \
	     sizeof(value), \
	     NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cnewv0c( ADIobj id, char *name, char *value, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_cnewv0c");		/* Mark routine for error reporting */

  adix_new_n( ADI__true, id, name, _CSM, 0, NULL, &value,
	_cdef_data(_TM_alloc(c)), _CSM, NULL, status );

  _ERR_OUT;
  }

void adic_cnewv0c_n( ADIobj id, char *name, _TM_ctype(c) value, int len,
		     ADIstatus status )
  {
  _chk_stat; _chk_init;                 /* Standard checks */

  _ERR_IN("adic_cnewv0c_n");		/* Mark routine for error reporting */

  adix_new_n( ADI__true, id, name, _CSM, 0, NULL, &value,/* Use user supplied length */
	      _cdef_data(_TM_alloc(c)),
	      len, NULL, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cnewv1,_t)( ADIobj id, char *name, int nval, _TM_ctype(_t) value[], ADIstatus status )\
  { \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_names(adic_cnewv1,_t)); \
  adix_new_n( ADI__true, id, name, _CSM, 1, &nval, value, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), \
	     NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cnewv1c( ADIobj id, char *name, int nval, charptr value[], ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_cnewv1c");		/* Mark routine for error reporting */

  adix_new_n( ADI__true, id, name, _CSM, 1, &nval, value, _cdef_data(_TM_alloc(c)),
	      _CSM, NULL, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Component data access
 * -------------------------------------------------------------------------
 */

#define _genproc(_t) \
void _TM_name(adic_cget,_t)( ADIobj id, char *name, int ndim, int dims[], \
	_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cget,_t)); \
  adix_get_n( 1, id, name, _CSM, ndim, dims, _TM_code(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cgetc( ADIobj id, char *name, int ndim, int dims[], int len,
		 char *value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cgetc");		/* Mark routine for error reporting */

  adix_get_n( 1, id, name, _CSM, ndim, dims, _TM_code(c),
	      len, value, NULL, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cget0,_t)( ADIobj id, char *name, _TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cget0,_t)); \
  adix_get_n( 1, id, name, _CSM, 0, NULL, _TM_code(_t), \
	     sizeof(_TM_ctype(_t)), value, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cget0c( ADIobj id, char *name, int len, char *value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cget0c");		/* Mark routine for error reporting */

  adix_get_n( 1, id, name, _CSM, 0, NULL, _TM_code(c),
	      len, value, NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cget1,_t)( ADIobj id, char *name, int mxval, \
	_TM_ctype(_t) *value, int *nactval, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cget1,_t)); \
  adix_get_n( 1, id, name, _CSM, 1, &mxval, _TM_code(_t), \
	     sizeof(_TM_ctype(_t)), \
	     value, nactval, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cget1c( ADIobj id, char *name, int mxval, int len,
		  char *value, int *nactval, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cget1c");		/* Mark routine for error reporting */

  adix_get_n( 1, id, name, _CSM, 1, &mxval, _TM_code(c),
	      len, value, nactval, status );
  _ERR_OUT;
  }

void adic_cmap( ADIobj id, char *name, char *type, char *mode,
		void **vptr, ADIstatus status )
  {
  _chk_init_err; _chk_stat;
  _ERR_IN("adic_cmap");

  adix_map_t( 1, id, name, _CSM, type, _CSM,
	      mode, _CSM, vptr, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cmap,_t)( ADIobj id, char *name, char *mode, \
		_TM_ctype(_t) **vptr, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cmap,_t)); \
  adix_map_n( 1, id, name, _CSM, mode, _CSM, \
	      _TM_code(_t), sizeof(_TM_ctype(_t)), \
	      (void **) vptr, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

#define _genproc(_t) \
void _TM_name(adic_cput,_t)( ADIobj id, char *name, int ndim, int dims[], \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cput,_t)); \
  adix_put_n( 1, id, name, _CSM, ndim, dims, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cputc( ADIobj id, char *name, int ndim, int dims[],
		 char **value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cputc");		/* Mark routine for error reporting */

  adix_put_n( 1, id, name, _CSM, ndim, dims, _cdef_data(_TM_alloc(c)),
	      _CSM, value, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cput0,_t)( ADIobj id, char *name, _TM_ctype(_t) value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cput0,_t)); \
  adix_put_n( 1, id, name, _CSM, 0, NULL, _cdef_data(_TM_alloc(_t)), \
	     sizeof(value), &value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cput0c( ADIobj id, char *name, char *value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cput0c");		/* Mark routine for error reporting */

  adix_put_n( 1, id, name, _CSM, 0, NULL, _cdef_data(_TM_alloc(c)),
	      _CSM, &value, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cput1,_t)( ADIobj id, char *name, int nval, \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cput1,_t)); \
  adix_put_n( 1, id, name, _CSM, 1, &nval, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), \
	     value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cput1c( ADIobj id, char *name, int nval,
		  char **value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cput1c");		/* Mark routine for error reporting */

  adix_put_n( 1, id, name, _CSM, 1, &nval, _cdef_data(_TM_alloc(c)), _CSM,
	      value, status );

  _ERR_OUT;
  }

void adic_cputid( ADIobj id, char *name, ADIobj vid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_cputid");		/* Mark routine for error reporting */

  adix_cputid( id, name, _CSM, /* Invoke kernel routine */
	       vid, status );

  _ERR_OUT;
  }

void adic_cunmap( ADIobj id, char *name, void *vptr, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cunmap");

  adix_unmap_n( id, name, _CSM, vptr, status );

  _ERR_OUT;
  }

void adic_find( ADIobj id, char *name, ADIobj *cid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_find");			/* Mark routine for error reporting */

  *cid = adix_find( id, name,		/* Invoke kernel routine */
	 _CSM, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cset,_t)( ADIobj id, char *name, int ndim, int dims[], \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cset,_t)); \
  adix_set_n( 1, id, name, _CSM, ndim, dims, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), \
	     value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_csetc( ADIobj id, char *name, int ndim, int dims[],
		 char **value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_csetc");		/* Mark routine for error reporting */

  adix_set_n( 1, id, name, _CSM, ndim, dims,
	      _cdef_data(_TM_alloc(c)), _CSM,
	      value, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cset0,_t)( ADIobj id, char *name, _TM_ctype(_t) value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cset0,_t)); \
  adix_set_n( 1, id, name, _CSM, 0, NULL, _cdef_data(_TM_alloc(_t)), \
	     sizeof(value), &value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cset0c( ADIobj id, char *name, char *value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cset0c");		/* Mark routine for error reporting */

  adix_set_n( 1, id, name, _CSM, 0, NULL, _cdef_data(_TM_alloc(c)), _CSM,
	       &value, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
void _TM_name(adic_cset1,_t)( ADIobj id, char *name, int nval, \
		_TM_ctype(_t) *value, ADIstatus status ) \
  { \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_names(adic_cset1,_t)); \
  adix_set_n( 1, id, name, _CSM, 1, &nval, _cdef_data(_TM_alloc(_t)), \
	     sizeof(_TM_ctype(_t)), value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(ub)	_genproc(w)	_genproc(uw)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
_genproc(p)
#undef _genproc

void adic_cset1c( ADIobj id, char *name, int nval,
		  char **value, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_cset1c");		/* Mark routine for error reporting */

  adix_set_n( 1, id, name, _CSM, 1, &nval, _cdef_data(_TM_alloc(c)), _CSM,
	      value, status );
  _ERR_OUT;
  }

void adic_there( ADIobj id, char *name, ADIlogical *there, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  _ERR_IN("adic_there");		/* Mark routine for error reporting */

  *there = adix_there( id, name,	/* Invoke kernel routine */
	 _CSM, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Object enquiry
 * -------------------------------------------------------------------------
 */
void adic_class( ADIobj id, int blen, char *buf, ADIstatus status )
  {
  char 	*cname;

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_class");		/* Mark routine for error reporting */

  cname = adix_qcls( id, status );	/* Locate class name string */

  strx_expc( strlen(cname), cname, 	/* Export data */
		      blen, buf );

  _ERR_OUT;
  }

void adic_cshape( ADIobj id, char *name, int mxndim, int dims[], int *ndim, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_cshape");		/* Mark routine for error reporting */

  adix_shape( id, name, _CSM, mxndim,	/* Invoke kernel routine */
	      dims, ndim, status );

  _ERR_OUT;
  }

void adic_csize( ADIobj id, char *name, int *nelm, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_csize");		/* Mark routine for error reporting */

/* Invoke kernel routine */
  adix_size( id, name, _CSM, nelm, status );

  _ERR_OUT;
  }

void adic_name( ADIobj id, int blen, char *buf, ADIstatus status )
  {
  char 	*cname;

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_cerase");		/* Mark routine for error reporting */

  cname = adix_name( id, status );	/* Get address of name */

  strx_expc( strlen(cname), cname, 	/* Export data */
		      blen, buf );

  _ERR_OUT;
  }

void adic_shape( ADIobj id, int mxndim, int dims[], int *ndim, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_shape");		/* Mark routine for error reporting */

  adix_shape( id, NULL, 0, mxndim, dims,/* Invoke kernel routine */
	      ndim, status );

  _ERR_OUT;
  }

void adic_size( ADIobj id, int *nelm, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_size");			/* Mark routine for error reporting */

/* Invoke kernel routine */
  adix_size( id, NULL, 0, nelm, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Changing object attributes
 * -------------------------------------------------------------------------
 */
void adic_alter( ADIobj id, int ndim, int dims[], ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_alter");		/* Mark routine for error reporting */

  ADIaryAlter( id, NULL, 0, ndim,	/* Invoke kernel routine */
	       dims, status );

  _ERR_OUT;
  }

void adic_calter( ADIobj id, char *name, int ndim, int dims[], ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_calter");		/* Mark routine for error reporting */

  ADIaryAlter( id, name, _CSM,  	/* Invoke kernel routine */
	       ndim, dims, status );

  _ERR_OUT;
  }

void adic_ccell( ADIobj id, char *name, int ndim, int index[],
                 ADIobj *cid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_ccell");		/* Mark routine for error reporting */

  adix_cell( id, name, _CSM, ndim,	/* Invoke kernel routine */
	     index, cid, status );

  _ERR_OUT;
  }

void adic_cell( ADIobj id, int ndim, int index[],
                ADIobj *cid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_cell");			/* Mark routine for error reporting */

  adix_cell( id, NULL, 0, 		/* Invoke kernel routine */
             ndim, index, cid,
             status );

  _ERR_OUT;
  }

void adic_cslice( ADIobj id, char *name, int ndim, int diml[], int dimu[],
                  ADIobj *sid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_cslice");		/* Mark routine for error reporting */

  adix_slice( id, name, _CSM, ndim,	/* Invoke kernel routine */
	      diml, dimu, sid, status );

  _ERR_OUT;
  }

void adic_slice( ADIobj id, int ndim, int diml[], int dimu[], ADIobj *sid,
                 ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_slice");		/* Mark routine for error reporting */

  adix_slice( id, NULL, 0, ndim,	/* Invoke kernel routine */
              diml, dimu, sid,
              status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Object destruction
 * -------------------------------------------------------------------------
 */

void adic_erase( ADIobj *id, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_erase");		/* Mark routine for error reporting */

  adix_erase( id, 1, status );

  _ERR_OUT;
  }

void adic_cerase( ADIobj id, char *name, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("adic_cerase");		/* Mark routine for error reporting */

  adix_cerase( id, name, _CSM, status );/* Invoke kernel routine */

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Symbol packages
 * -------------------------------------------------------------------------
 */

void adic_reqpkg( char *pkg, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_reqpkg");

  ADIpkgRequire( pkg, _CSM, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Data system
 * -------------------------------------------------------------------------
 */
void adic_fclose( ADIobj id, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_fclose");		/* Mark routine for error reporting */

  ADIfsysFileClose( id, status );

  _ERR_OUT;
  }

void adic_fcreat( char *fspec, ADIobj id, ADIobj *fid,
                  ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_fcreat");		/* Mark routine for error reporting */

  adix_fcreat( fspec, _CSM, id, fid, status );

  _ERR_OUT;
  }

void adic_fopen( char *fspec, char *cls, char *mode, ADIobj *id,
                 ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_fopen");		/* Mark routine for error reporting */

  adix_fopen( fspec, _CSM, cls, _CSM,	/* Invoke kernel routine */
	      mode, _CSM, id, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Miscellaneous
 * -------------------------------------------------------------------------
 */

void adic_flush( char *grp, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_flush");

  adix_id_flush( grp, _CSM, status );

  _ERR_OUT;
  }

void adic_link( ADIobj id, char *grp, ADIstatus status )
  {
  _chk_init; _chk_stat;

  _ERR_IN("adic_link");

  adix_id_link( id, grp, _CSM, status );

  _ERR_OUT;
  }


/* ----------------- unincorporated routines --------------- */

void adic_defrcb( ADIobj rid, char *name, ADICB rtn, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  adix_defrcb( rid, name, _CSM, /* Invoke kernel routine */
	       adix_neweprc( ADI__true,
		 (ADICB) rtn, status ),
	       status );
  }

void adic_getfile( ADIobj id, ADIobj *fid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  adix_getfile( id, fid, status );
  }

void adic_getlink( ADIobj id, ADIobj *lid, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  *lid = adix_getlink( id, status );
  }

void adic_getpath( ADIobj id, int mxlen, char *buf, ADIstatus status )
  {
  int   actlen;

  _chk_init_err; _chk_stat;

  adix_getpath( id, ADI__true, mxlen, buf, &actlen, status );
  }

void adic_locrcb( ADIobj rid, char *name, ADIobj *rtn, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  adix_locrcb( rid, name, _CSM, /* Invoke kernel routine */
	       rtn, status );
  }

void adic_locrep( char *name, ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  adix_locrep( name, _CSM, id, status );/* Invoke kernel routine */
  }

void adic_cmnstr( char *string, ADIobj *id, ADIstatus status )
  {
  _chk_init; _chk_stat;

  *id = adix_cmnC( string, status );
  }


