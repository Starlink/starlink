/*
*+
*  Name:
*     adifface.c

*  Purpose:
*     The ADI Fortran interface

*  Language:
*     C (part ANSI, part not)

*  Type of Module:
*     C source file

*  Description:
*     This file provides the Fortran interface functions to ADI. The
*     routines supplied are (assuming F77TRUNC is not defined),
*
*      Error system :
*
*	adi_errctx	- return error context string
* 	adi_errmsg	- return error message given code
*	adi_setec	- set error code
*	adi_setecs	- set error code and context string
* 	adi_setes	- set context string
*	adi_setetc	- set character error token
*	adi_seteti	- set integer error token
*
*      Property handling :
*
*	adi_delprp	- delete named property
*	adi_locprp	- locate named property
*       adi_nprp	- return number of properties
*       adi_indprp	- locate property by number
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
*	adi_clone	- Clone an identifier
*	adi_refadj	- Apply increment to object reference count
*	adi_refcnt	- Return object reference count
*
*      Context manipulation :
*
*       adi_mark	- create new context
* 	adi_rlse	- release a context
*
*      Data system definitions :
*
*       adi_defcls	- define a new class
*       adi_defgdp	- define generic dispatch procedure
*	adi_defgen	- define a generic function
*	adi_defmth	- define a method
*       adi_defrep	- define a new file representation
*
*      Method execution :
*
*       adi_exec	- execute named method
*	adi_execi	- execute named method (ADI string)
*	adi_calnxt	- invoke next method in method hierarchy
*
*      System method interfaces :
*
*	adi_copy	- Make a copy of an objects data
*	adi_print	- Text representation of object
*	adi_setlnk	- Set ADI link field for ADIbase derived class pairs
*	adi_unlnk	- Reset ADI link field
*
*      Primitive data creation :
*
*       adi_new         - Create new n-D object of named type
*       adi_new<t>      - Create new n-D primitive
*       adi_new0        - Create new scalar of named type
*       adi_new0<t>     - Create new scalar primitive
*       adi_new1        - Create new 1-D object of named type
*       adi_new1<t>     - Create new 1-D primitive
*       adi_newv<t>     - Create new n-D primitive with value
*       adi_newv0<t>    - Create new scalar primitive with value
*       adi_newv1<t>    - Create new 1-D primitive with value
*
*      Data access :
*
*       adi_get<t>      - Get n-D primitive values
*       adi_get0<t>     - Get scalar primitive value
*       adi_get1<t>     - Get 1-D primitive values
*	adi_map		- Map with user specified type
*	adi_map<t>	- Map with a specific type
*       adi_put<t>      - Put n-D primitive values
*       adi_put0<t>     - Put scalar primitive value
*       adi_put1<t>     - Put 1-D primitive values
*	adi_unmap	- Unmap object
*
*      Component data creation :
*
*       adi_cnew        - Create new n-D object of named type
*       adi_cnew0       - Create new scalar of named type
*       adi_cnew1       - Create new 1-D object of named type
*       adi_cnew<t>     - Create new n-D primitive
*       adi_cnew0<t>    - Create new scalar primitive
*       adi_cnew1<t>    - Create new 1-D primitive
*       adi_cnewv<t>    - Create new n-D primitive with value
*       adi_cnewv0<t>   - Create new scalar primitive with value
*       adi_cnewv1<t>   - Create new 1-D primitive with value
*
*      Component data access :
*
*       adi_cget<t>     - Get n-D primitive values
*       adi_cget0<t>    - Get scalar primitive value
*       adi_cget1<t>    - Get 1-D primitive values
*	adi_cmap	- Map component with user specified type
*	adi_cmap<t>	- Map component with a specific type
*       adi_cput<t>     - Put n-D primitive values
*       adi_cput0<t>    - Put scalar primitive value
*       adi_cput1<t>    - Put 1-D primitive values
*       adi_cputid      - Put ADI object into object
*       adi_cset<t>     - Set n-D primitive values
*       adi_cset0<t>    - Set scalar primitive value
*       adi_cset1<t>    - Set 1-D primitive values
*	adi_find	- Locate component data
*	adi_there	- Does a component exist?
*	adi_cunmap	- Unmap object component
*
*      Enquiry routines :
*
*	adi_class	- Enquire object class
* 	adi_cshape	- Enquire object component dimensions
* 	adi_name	- Enquire object name
* 	adi_shape	- Enquire object dimensions
*	adi_state	- Enquire object data state
*
*      Changing object attributes :
*
*       adi_alter       - Alter dimensionality
*       adi_calter      - Alter dimensionality of component
*       adi_ccell	- Index a cell of an array object component
*       adi_cell	- Index a cell of an array object
*       adi_cslice      - Access object component slice
*       adi_slice       - Access object slice
*
*      Object destruction :
*
*	adi_erase	- Destroy an object
*	adi_cerase	- Destroy member of property of object
*
*      Symbol packages :
*
*	adi_reqpkg	- Load a package from the search path
*
*      Data system :
*
*       adi_fclose	- Close a file system object
*       adi_fcreat	- Create a new file system object
*       adi_fopen	- Open existing file system object
*
*      Miscellaneous :
*
*	adi_link	- Link an identifier to a name group
*	adi_flush	- Erase all objects in a name group

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

/* Toggle this entire file on the value of the ADI_F77 macro
 */
#ifdef ADI_F77

#include "adilist.h"
#include "adistrng.h"
#include "aditable.h"
#include "adicface.h"
#include "adierror.h"
#include "adikrnl.h"
#include "adimem.h"
#include "adipkg.h"
#include "adifsys.h"
#include "adifface.h"                   /* Prototypes for this module */


/*
 * Macro to assign Fortran logical based on C logical
 */
#define _ASSFLOG(_x,_y) if (_y) (_x) = F77_TRUE; else (_x) = F77_FALSE;


/* -------------------------------------------------------------------------
 * ADI error handling
 * -------------------------------------------------------------------------
 */
F77_SUBROUTINE(adifn(errmsg))( INTEGER(code), CHARACTER(buf) TRAIL(buf) )
  {
  GENPTR_INTEGER(code)
  GENPTR_CHARACTER(buf)

  adix_errmsg( *code, buf, buf_length );
  }

F77_SUBROUTINE(adifn(setec))( INTEGER(code), INTEGER(status) )
  {
  GENPTR_INTEGER(code)
  GENPTR_INTEGER(status)

  adix_setec( *code, status );
  }

F77_SUBROUTINE(adifn(setecs))( INTEGER(code), CHARACTER(ctx), INTEGER(status) TRAIL(ctx) )
  {
  GENPTR_INTEGER(code)
  GENPTR_CHARACTER(ctx)
  GENPTR_INTEGER(status)

  adix_setecs( *code, ctx, ctx_length, status );
  }

F77_SUBROUTINE(adifn(setes))( CHARACTER(ctx), INTEGER(status) TRAIL(ctx) )
  {
  GENPTR_CHARACTER(ctx)
  GENPTR_INTEGER(status)

  adix_setes( ctx, ctx_length, status );
  }

F77_SUBROUTINE(adifn(setetc))( CHARACTER(tok), CHARACTER(val) TRAIL(tok) TRAIL(val) )
  {
  GENPTR_CHARACTER(tok)
  GENPTR_CHARACTER(val)
  char		tstr[21];

  strncpy( tstr, tok, _MIN(21,tok_length) );

  adix_setetc( tstr, val, val_length );
  }

F77_SUBROUTINE(adifn(seteti))( CHARACTER(tok), INTEGER(val) TRAIL(tok) )
  {
  GENPTR_CHARACTER(tok)
  GENPTR_INTEGER(val)

  char		tstr[21];

  strncpy( tstr, tok, _MIN(21,tok_length) );

  adix_seteti( tstr, *val );
  }

#ifdef NOEMS
F77_SUBROUTINE(adifn(errctx))( CHARACTER(buf) TRAIL(buf) )
  {
  GENPTR_CHARACTER(buf)
  adix_errctx( buf, buf_length );
  }
#endif

/* -------------------------------------------------------------------------
 * Property handling
 * -------------------------------------------------------------------------
 */
F77_SUBROUTINE(adifn(delprp))( INTEGER(id), CHARACTER(pname), INTEGER(status)
			    TRAIL(pname) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(pname)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_DELPRP");

  adix_delprp( (ADIobj) *id, pname, pname_length, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(locprp))( INTEGER(id), CHARACTER(pname), INTEGER(pid),
			    INTEGER(status) TRAIL(pname) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(pname)
  GENPTR_INTEGER(pid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_LOCPRP");

  adix_locprp( (ADIobj) *id, pname, pname_length, (ADIobj *) pid, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(nprp))( INTEGER(id), INTEGER(nprp), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(nprp)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_NPRP");

  adix_nprp( (ADIobj) *id, nprp, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(indprp))( INTEGER(id), INTEGER(index), INTEGER(pid),
			    INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(index)
  GENPTR_INTEGER(pid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_INDPRP");

  adix_indprp( (ADIobj) *id, *index, (ADIobj *) pid, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Reference counts
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(clone))( INTEGER(id), INTEGER(cid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CLONE");			/* Mark routine for error reporting */

  *((ADIobj *) &cid) = adix_clone( (ADIobj) id,
	status );
  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(refadj))( INTEGER(id), INTEGER(incr), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(incr)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_REFADJ");		/* Mark routine for error reporting */

  adix_refadj( (ADIobj) *id,            /* Increment reference count */
	       *incr, status );
  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(refcnt))( INTEGER(id), INTEGER(cnt), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(cnt)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_REFCNT");		/* Mark routine for error reporting */

  *cnt = adix_refcnt( (ADIobj) *id,     /* Retrieve reference count */
		       status );
  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Context management
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(mark))( void )
  {
  ADIstatype    lstatus = SAI__OK;
  ADIstatus     status = &lstatus;

  _chk_init;

  adix_mark();
  }

F77_SUBROUTINE(adifn(rlse))( void )
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

F77_SUBROUTINE(adifn(defcls))( CHARACTER(name), CHARACTER(parents),
			    CHARACTER(members), INTEGER(tid),
			    INTEGER(status) TRAIL(name) TRAIL(parents)
			    TRAIL(members) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(parents)
  GENPTR_CHARACTER(members)
  GENPTR_INTEGER(tid)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;                 /* Standard checks */

  _ERR_IN("ADI_DEFCLS");		/* Mark routine for error reporting */

  ADIdefClass_e( name, name_length, parents, parents_length,
		 members, members_length, (ADIobj *) tid, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(defgdp))( INTEGER(genid), ADICB rtn, INTEGER(status) )
  {
  GENPTR_INTEGER(genid)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;                 /* Standard checks */

  _ERR_IN("ADI_DEFGDP");		/* Mark routine for error reporting */

  adix_defgdp( *genid,
	       adix_neweprc( ADI__false, rtn, status ),
	       status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(defgen))( CHARACTER(name), CHARACTER(options),
			    ADICB rtn, INTEGER(id),
			    INTEGER(status) TRAIL(name) TRAIL(options) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(options)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;                 /* Standard checks */

  _ERR_IN("ADI_DEFGEN");		/* Mark routine for error reporting */

  adix_defgen( name, name_length,
	       options, options_length,
	       adix_neweprc( ADI__false, rtn, status ),
	       (ADIobj *) id, status );
  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(defmth))( CHARACTER(spec), ADIfMethodCB rtn,
			    INTEGER(id), INTEGER(status) TRAIL(spec) )
  {
  _chk_init; _chk_stat;                 /* Standard entry checks */

  _ERR_IN("ADI_DEFMTH");		/* Mark routine for error reporting */

  adix_defmth( spec, spec_length,     	/* Invoke kernel routine */
	       adix_neweprc( ADI__false, (ADICB) rtn, status ),
	       (ADIobj *) id, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(defrep))( CHARACTER(name), INTEGER(id),
			    INTEGER(status) TRAIL(name) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;                 /* Standard checks */

  _ERR_IN("ADI_DEFREP");		/* Mark routine for error reporting */

  adix_defrep( name, name_length,
	       (ADIobj *) id, status );
  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Method execution
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(calnxt))( INTEGER(status) )
  {
  GENPTR_INTEGER(status)

  _chk_init_err;

  if ( _ok(status) )
    *status = ADI__CALNXTMTH;
  }

F77_SUBROUTINE(adifn(exec))( CHARACTER(func), INTEGER(narg),
			  INTEGER_ARRAY(args), INTEGER(res),
			  INTEGER(status) TRAIL(func) )
  {
  GENPTR_CHARACTER(func)
  GENPTR_INTEGER(narg)
  GENPTR_INTEGER_ARRAY(args)
  GENPTR_INTEGER(res)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_EXEC");			/* Mark routine for error reporting */

  *res = (F77_INTEGER_TYPE)
	  adix_exec( func, func_length,
		     *narg,
		     (ADIobj *) args, status );
  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(execi))( INTEGER(func), INTEGER(narg),
			   INTEGER_ARRAY(args), INTEGER(res),
			   INTEGER(status) )
  {
  GENPTR_INTEGER(narg)
  GENPTR_INTEGER_ARRAY(args)
  GENPTR_INTEGER(res)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_EXECI");		/* Mark routine for error reporting */

  *res = (F77_INTEGER_TYPE)
	adix_execi( (ADIobj) *func, *narg,
	  (ADIobj *) args, status );
  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * System method interfaces
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(copy))( INTEGER(id), INTEGER(cid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_COPY");			/* Mark routine for error reporting */

  *((ADIobj *) cid) = adix_copy( (ADIobj) *id, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(print))( INTEGER(id), INTEGER(status) )
  {
  _ERR_IN("ADI_PRINT");		/* Mark routine for error reporting */

  _chk_init_err; _chk_stat;

  adix_print( *id, ADI__false, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(setlnk))( INTEGER(id), INTEGER(lid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(lid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_SETLNK");

  adix_setlnk( (ADIobj) *id, (ADIobj) *lid, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(unlnk))( INTEGER(id), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_UNLNK");			/* Mark routine for error reporting */

  adix_unlnk( (ADIobj) *id, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Data creation
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(new))( CHARACTER(cls), INTEGER(ndim),
			    INTEGER_ARRAY(dims), INTEGER(id),
			    INTEGER(status) TRAIL(cls) )
  {
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_NEW");

  adix_newn( ADI__nullid, NULL, 0, cls, cls_length, *ndim,
             dims, (ADIobj *) id, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(new0))( CHARACTER(cls), INTEGER(id),
			  INTEGER(status) TRAIL(cls) )
  {
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_NEW0");

  adix_newn( ADI__nullid, NULL, 0, cls, cls_length, 0,
	     NULL, (ADIobj *) id, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(new1))( CHARACTER(cls), INTEGER(nval),
		INTEGER(id), INTEGER(status) TRAIL(cls) )
  {
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(nval)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_NEW1");

  adix_newn( ADI__nullid, NULL, 0, cls, cls_length, 1, nval,
	    (ADIobj *) id, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(new,_t))( INTEGER(ndim), INTEGER_ARRAY(dims), INTEGER(id), INTEGER(status) )\
  { \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER_ARRAY(dims) \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(new,_t)); \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, *ndim, dims, NULL, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      0, (ADIobj *) id, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(new0,_t))( INTEGER(id), INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(new0,_t)); \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 0, NULL, NULL, \
	     _cdef_ctrl(_TM_alloc(_t)), \
	     0, (ADIobj *) id, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(new1,_t))( INTEGER(nval), INTEGER(id), INTEGER(status) )\
  { \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(new1,_t)); \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 1, nval, NULL, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      0, (ADIobj *) id, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc


#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(newv,_t))( INTEGER(ndim), INTEGER_ARRAY(dims), _TM_ftype(_t) value[], INTEGER(id), INTEGER(status) )\
  { \
  GENPTR_INTEGER(ndim) \
  GENPTR_INTEGER_ARRAY(dims) \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(newv,_t)); \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, *ndim, dims, value, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      sizeof(_TM_ftype(_t)), \
	      (ADIobj *) id, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(newvc))( INTEGER(ndim), INTEGER_ARRAY(dims),
			       CHARACTER_ARRAY(value),
			       INTEGER(id), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_NEWVC");			/* Mark routine for error reporting */

  adix_new_n( ADI__false, ADI__nullid, NULL, 0, *ndim, dims, value,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      (ADIobj *) id, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(newv0,_t))( _TM_ftype(_t) *value, INTEGER(id), INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(newv0,_t)); \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 0, NULL, value, \
	     _cdef_ctrl(_TM_alloc(_t)), \
	     sizeof(_TM_ftype(_t)), \
	     (ADIobj *) id, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(newv0c))( CHARACTER(value), INTEGER(id),
			    INTEGER(status) TRAIL(value) )
  {
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_NEWV0C");		/* Mark routine for error reporting */

  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 0, NULL,
              (void *) &value,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      (ADIobj *) id, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(newv1,_t))( INTEGER(nval), _TM_ftype(_t) value[], INTEGER(id), INTEGER(status) )\
  { \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(newv1,_t)); \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 1, nval, value, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      sizeof(_TM_ftype(_t)), \
	      (ADIobj *) id, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(newv1c))( INTEGER(nval), CHARACTER_ARRAY(value),
			    INTEGER(id), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(nval)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_NEWV1C");		/* Mark routine for error reporting */

  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 1, nval, value,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      (ADIobj *) id, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Data access
 * -------------------------------------------------------------------------
 */

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(get,_t))( INTEGER(id), INTEGER(ndim), \
		INTEGER_ARRAY(dims), _TM_ftype(_t) *value, INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(ndim) \
  GENPTR_INTEGER(dims) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(get,_t)); \
  adix_get_n( 0, (ADIobj) *id, NULL, 0, *ndim, dims, \
	      _TM_code(_t), \
	      sizeof(_TM_ftype(_t)), \
	      value, NULL, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(getc))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(dims),
		CHARACTER_ARRAY(value), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(nactval)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_GETC");		/* Mark routine for error reporting */

  adix_get_n( 0, (ADIobj) *id, NULL, 0, *ndim, dims,
	      _TM_code(c), value_length,
	      value, NULL, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(get0,_t))( INTEGER(id), _TM_ftype(_t) *value, INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(get0,_t)); \
  adix_get_n( 0, (ADIobj) *id, NULL, 0, 0, NULL, _TM_code(_t), \
	     sizeof(_TM_ftype(_t)), \
	     value, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(get0c))( INTEGER(id), CHARACTER(value), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_GET0C");		/* Mark routine for error reporting */

  adix_get_n( 0, (ADIobj) *id, NULL, 0, 0, NULL, _TM_code(c),
	      value_length,
	      value, NULL, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(get1,_t))( INTEGER(id), INTEGER(mxval), \
		_TM_ftype(_t) *value, INTEGER(nactval), INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(mxval) \
  GENPTR_INTEGER(nactval) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(get1,_t)); \
  adix_get_n( 0, (ADIobj) *id, NULL, 0, 1, mxval, \
	      _TM_code(_t), \
	      sizeof(_TM_ftype(_t)), \
	      value, nactval, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(get1c))( INTEGER(id), INTEGER(mxval),
		CHARACTER_ARRAY(value), INTEGER(nactval), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(mxval)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(nactval)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_GET1C");		/* Mark routine for error reporting */

  adix_get_n( 0, (ADIobj) *id, NULL, 0, 1, mxval,
	      _TM_code(c), value_length,
	      value, nactval, status );
  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(map))( INTEGER(id), CHARACTER(type), CHARACTER(mode),
			    POINTER(vptr), INTEGER(status) TRAIL(type)
			    TRAIL(mode) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(type)
  GENPTR_CHARACTER(mode)
  GENPTR_POINTER(vptr)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_MAP");

  adix_map_t( 0, (ADIobj) *id, NULL, 0, type, type_length, mode,
	      mode_length, (void **) vptr, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(map,_t))( INTEGER(id), CHARACTER(mode), \
		POINTER(vptr), INTEGER(status) TRAIL(mode) ) { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(mode) \
  GENPTR_POINTER(vptr) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(map,_t)); \
  adix_map_n( 0, (ADIobj) *id, NULL, 0, mode, mode_length, \
	      _TM_code(_t), sizeof(_TM_ftype(_t)), \
	      (void **) vptr, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(put,_t))( INTEGER(id), INTEGER(ndim), \
		INTEGER_ARRAY(dims), _TM_ftype(_t) *value, INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(ndim) \
  GENPTR_INTEGER_ARRAY(dims) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(put,_t)); \
  adix_put_n( 0, (ADIobj) *id, NULL, 0, *ndim, dims, \
	      _cdef_ctrl(_TM_alloc(_t)), \
              sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(putc))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(dims),
		CHARACTER_ARRAY(value), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_PUTC");			/* Mark routine for error reporting */

  adix_put_n( 0, (ADIobj) *id, NULL, 0, *ndim, dims,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      value, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(put0,_t))( INTEGER(id), _TM_ftype(_t) *value, INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(put0,_t)); \
  adix_put_n( 0, (ADIobj) *id, \
	      NULL, 0, 0, NULL, _cdef_ctrl(_TM_alloc(_t)),\
	      sizeof(_TM_ftype(_t)),\
	      value, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(put0c))( INTEGER(id), CHARACTER(value), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_PUT0C");		/* Mark routine for error reporting */

  adix_put_n( 0, (ADIobj) *id, NULL, 0,
	      0, NULL, _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      (void *) &value, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(put1,_t))( INTEGER(id), INTEGER(nval), \
		_TM_ftype(_t) *value, INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(put1,_t)); \
  adix_put_n( 0, (ADIobj) *id, NULL, 0, 1, nval, \
	      _cdef_ctrl(_TM_alloc(_t)), \
              sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(put1c))( INTEGER(id), INTEGER(nval),
		CHARACTER_ARRAY(value), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(nval)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_PUT1C");			/* Mark routine for error reporting */

  adix_put_n( 0, (ADIobj) *id, NULL, 0, 1, nval,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      value, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(unmap))( INTEGER(id), POINTER(vptr), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_POINTER(vptr)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_UNMAP");

  adix_unmap_n( (ADIobj) *id, NULL, 0, (void *) vptr, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Component data creation
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(cnew))( INTEGER(pid), CHARACTER(name),
                             CHARACTER(cls), INTEGER(ndim),
			     INTEGER_ARRAY(dims),
			     INTEGER(status) TRAIL(name) TRAIL(cls) )
  {
  GENPTR_INTEGER(pid)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_CNEW");

  adix_newn( (ADIobj) *pid, name, name_length, cls, cls_length, *ndim,
	     dims, NULL, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(cnew0))( INTEGER(pid), CHARACTER(name),
                              CHARACTER(cls),
			      INTEGER(status) TRAIL(name) TRAIL(cls) )
  {
  GENPTR_INTEGER(pid)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_CNEW0");

  adix_newn( (ADIobj) *pid, name, name_length, cls, cls_length, 0,
             NULL, NULL, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(cnew1))( INTEGER(pid), CHARACTER(name),
			      CHARACTER(cls), INTEGER(nval),
			      INTEGER(status) TRAIL(name) TRAIL(cls) )
  {
  GENPTR_INTEGER(pid)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(nval)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_CNEW1");

  adix_newn( (ADIobj) *pid, name, name_length, cls, cls_length, 1, nval,
	     NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnew,_t))( INTEGER(pid), CHARACTER(name), \
	INTEGER(ndim), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) )\
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER_ARRAY(dims) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(cnew,_t)); \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, *ndim, dims, NULL, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      0, NULL, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnew0,_t))( INTEGER(pid), CHARACTER(name), INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(cnew0,_t)); \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 0, NULL, NULL, \
	     _cdef_ctrl(_TM_alloc(_t)), \
	     0, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnew1,_t))( INTEGER(pid), CHARACTER(name), INTEGER(nval), INTEGER(status) TRAIL(name) )\
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(cnew1,_t)); \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 1, nval, NULL, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      0, NULL, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc


#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnewv,_t))( INTEGER(pid), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), _TM_ftype(_t) value[], INTEGER(status) TRAIL(name) )\
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(ndim) \
  GENPTR_INTEGER_ARRAY(dims) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(cnewv,_t)); \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, *ndim, dims, value, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      sizeof(_TM_ftype(_t)), \
	      NULL, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cnewvc))( INTEGER(pid), CHARACTER(name), INTEGER(ndim),
			       INTEGER_ARRAY(dims), CHARACTER_ARRAY(value),
			       INTEGER(status) TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(pid)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_CNEWVC");			/* Mark routine for error reporting */

  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, *ndim, dims, value,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnewv0,_t))( INTEGER(pid), CHARACTER(name), _TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(cnewv0,_t)); \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 0, NULL, value, \
	     _cdef_ctrl(_TM_alloc(_t)), \
	     sizeof(_TM_ftype(_t)), \
	     NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cnewv0c))( INTEGER(pid), CHARACTER(name), CHARACTER(value),
			        INTEGER(status) TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(pid)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_CNEWV0C");		/* Mark routine for error reporting */

  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 0, NULL,
              (void *) &value,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnewv1,_t))( INTEGER(pid), CHARACTER(name), INTEGER(nval), _TM_ftype(_t) value[], INTEGER(status) TRAIL(name) )\
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(status) \
  _chk_init; _chk_stat; \
  _ERR_IN(_TM_fnames(cnewv1,_t)); \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 1, nval, value, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      sizeof(_TM_ftype(_t)), \
	      NULL, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cnewv1c))( INTEGER(pid), CHARACTER(name), INTEGER(nval),
				CHARACTER_ARRAY(value), INTEGER(status)
			        TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(pid)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(nval)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_CNEWV1C");		/* Mark routine for error reporting */

  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 1, nval, value,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      NULL, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Component data access
 * -------------------------------------------------------------------------
 */

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cget,_t))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), \
		_TM_ftype(_t) *value, INTEGER(nactval), INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(ndim) \
  GENPTR_INTEGER_ARRAY(dims) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(cget,_t)); \
  adix_get_n( 0, (ADIobj) *id, name, name_length, *ndim, dims, \
	      _TM_code(_t), \
	      sizeof(_TM_ftype(_t)), \
	      value, NULL, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cgetc))( INTEGER(id), CHARACTER(name), INTEGER(ndim),
		INTEGER_ARRAY(dims), CHARACTER_ARRAY(value), INTEGER(status)
		TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CGETC");			/* Mark routine for error reporting */

  adix_get_n( 0, (ADIobj) *id, name, name_length, *ndim, dims,
	      _TM_code(c), value_length,
	      value, NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cget0,_t))( INTEGER(id), CHARACTER(name), \
	 _TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _ERR_IN(_TM_fnames(cget0,_t)); \
  _chk_init_err; _chk_stat; \
  adix_get_n( 0, (ADIobj) *id, name, name_length, 0, NULL, _TM_code(_t), \
	     sizeof(_TM_ftype(_t)), \
	     value, NULL, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cget0c))( INTEGER(id), CHARACTER(name),
	     CHARACTER(value), INTEGER(status) TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CGET0C");		/* Mark routine for error reporting */

  adix_get_n( 0, (ADIobj) *id, name, name_length, 0, NULL, _TM_code(c),
	      value_length, value, NULL, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cget1,_t))( INTEGER(id), CHARACTER(name), INTEGER(mxval), \
		_TM_ftype(_t) *value, INTEGER(nactval), INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(mxval) \
  GENPTR_INTEGER(nactval) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(cget1,_t)); \
  adix_get_n( 0, (ADIobj) *id, name, name_length, 1, mxval, \
	      _TM_code(_t), \
	      sizeof(_TM_ftype(_t)), \
	      value, nactval, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cget1c))( INTEGER(id), CHARACTER(name), INTEGER(mxval),
		CHARACTER_ARRAY(value), INTEGER(nactval), INTEGER(status)
		TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(mxval)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(nactval)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CGET1C");		/* Mark routine for error reporting */

  adix_get_n( 0, (ADIobj) *id, name, name_length, 1, mxval,
	      _TM_code(c), value_length,
	      value, nactval, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(cmap))( INTEGER(id), CHARACTER(name), CHARACTER(type),
			     CHARACTER(mode), POINTER(vptr), INTEGER(status)
			     TRAIL(name) TRAIL(type) TRAIL(mode) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(type)
  GENPTR_CHARACTER(mode)
  GENPTR_POINTER(vptr)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CMAP");

  adix_map_t( 0, (ADIobj) *id, name, name_length, type, type_length, mode,
	      mode_length, (void **) vptr, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cmap,_t))( INTEGER(id), CHARACTER(name), \
		CHARACTER(mode), POINTER(vptr), INTEGER(status) \
		TRAIL(name) TRAIL(mode) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_CHARACTER(mode) \
  GENPTR_POINTER(vptr) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(cmap,_t)); \
  adix_map_n( 0, (ADIobj) *id, name, name_length, mode, mode_length, \
	      _TM_code(_t), sizeof(_TM_ftype(_t)), \
	      (void **) vptr, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cput,_t))( INTEGER(id), CHARACTER(name), \
		INTEGER(ndim), INTEGER_ARRAY(dims), \
		_TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(ndim) \
  GENPTR_INTEGER_ARRAY(dims) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(cput,_t)); \
  adix_put_n( 0, (ADIobj) *id, name, name_length, *ndim, dims, \
	      _cdef_ctrl(_TM_alloc(_t)), \
              sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cputc))( INTEGER(id), CHARACTER(name), INTEGER(ndim),
		INTEGER_ARRAY(dims), CHARACTER_ARRAY(value), INTEGER(status) TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CPUTC");			/* Mark routine for error reporting */

  adix_put_n( 0, (ADIobj) *id, name,
	      name_length, *ndim, dims,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      value, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cput0,_t))( INTEGER(id), CHARACTER(name), \
	 _TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _ERR_IN(_TM_fnames(cput0,_t)); \
  _chk_init_err; _chk_stat; \
  adix_put_n( 0, (ADIobj) *id, name, name_length, \
	      0, NULL, _cdef_ctrl(_TM_alloc(_t)),\
	     sizeof(_TM_ftype(_t)), \
	     value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cput0c))( INTEGER(id), CHARACTER(name),
	     CHARACTER(value), INTEGER(status) TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CPUT0C");		/* Mark routine for error reporting */

  adix_put_n( 0, (ADIobj) *id, name, name_length,
	      0, NULL, _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      (void *) &value, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cput1,_t))( INTEGER(id), CHARACTER(name), \
		INTEGER(nval), \
		_TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(cput1,_t)); \
  adix_put_n( 0, (ADIobj) *id, name, name_length, 1, nval, \
	      _cdef_ctrl(_TM_alloc(_t)), \
              sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cput1c))( INTEGER(id), CHARACTER(name), INTEGER(nval),
		CHARACTER_ARRAY(value), INTEGER(status) TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(nval)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CPUT1C");		/* Mark routine for error reporting */

  adix_put_n( 0, (ADIobj) *id, name,
	      name_length, 1, nval,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      value, status );

  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cset0,_t))( INTEGER(id), CHARACTER(name), \
	 _TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _ERR_IN(_TM_fnames(cset0,_t)); \
  _chk_init_err; _chk_stat; \
  adix_set_n( 0, (ADIobj) *id, name, name_length, \
	      0, NULL, _cdef_ctrl(_TM_alloc(_t)),\
	     sizeof(_TM_ftype(_t)), \
	     value, status ); \
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cset0c))( INTEGER(id), CHARACTER(name),
	     CHARACTER(value), INTEGER(status) TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CSET0C");		/* Mark routine for error reporting */

  adix_set_n( 0, (ADIobj) *id, name, name_length,
	      0, NULL, _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      (void *) &value, status );
  _ERR_OUT;
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cset1,_t))( INTEGER(id), CHARACTER(name), \
		INTEGER(nval), \
		_TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(status) \
  _chk_init_err; _chk_stat; \
  _ERR_IN(_TM_fnames(cset1,_t)); \
  adix_set_n( 0, (ADIobj) *id, name, name_length, 1, nval, \
	      _cdef_ctrl(_TM_alloc(_t)), \
	      sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_OUT;}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(cset1c))( INTEGER(id), CHARACTER(name), INTEGER(nval),
		CHARACTER_ARRAY(value), INTEGER(status) TRAIL(name) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(nval)
  GENPTR_CHARACTER_ARRAY(value)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CSET1C");		/* Mark routine for error reporting */

  adix_set_n( 0, (ADIobj) *id, name,
	      name_length, 1, nval,
	      _cdef_ctrl(_TM_alloc(c)),
	      value_length,
	      value, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(cputid))( INTEGER(id), CHARACTER(name),
			    INTEGER(vid), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(vid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CPUTID");		/* Mark routine for error reporting */

  adix_cputid( (ADIobj) *id, name,
	       name_length, (ADIobj) *vid, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(cunmap))( INTEGER(id), CHARACTER(name), POINTER(vptr),
			       INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_POINTER(vptr)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CUNMAP");

  adix_unmap_n( (ADIobj) *id, name, name_length, (void *) *vptr, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(find))( INTEGER(id), CHARACTER(name),
			     INTEGER(cid), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_FIND");		/* Mark routine for error reporting */

  *((ADIobj *) &cid) = adix_find( (ADIobj) *id, name,
		      name_length, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(there))( INTEGER(id), CHARACTER(name),
			      LOGICAL(there), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_LOGICAL(there)
  GENPTR_INTEGER(status)

  ADIboolean	cres;			/* Result from kernel */

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_THERE");			/* Mark routine for error reporting */

  cres = adix_there( (ADIobj) *id, name,
		      name_length, status );

  _ASSFLOG(*there,cres);		/* Set return value */

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Object enquiry
 * -------------------------------------------------------------------------
 */
F77_SUBROUTINE(adifn(class))( INTEGER(id), CHARACTER(cls), INTEGER(status) TRAIL(cls) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(status)

  char 	*cname;

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  cname = adix_qcls( (ADIobj) *id, status );	/* Locate class name string */

  strx_expf( strlen(cname), cname, 	/* Export data */
		cls_length, cls );
  }

F77_SUBROUTINE(adifn(cshape))( INTEGER(id), CHARACTER(name), INTEGER(mxndim),
                               INTEGER_ARRAY(dims), INTEGER(ndim),
                               INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(mxndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_CSHAPE");		/* Mark routine for error reporting */

  adix_shape( (ADIobj) *id, name,	/* Invoke kernel routine */
              name_length, *mxndim,
	      dims, ndim, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(name))( INTEGER(id), CHARACTER(name), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

  char 	*cname;

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_NAME");			/* Mark routine for error reporting */

  cname = adix_name( (ADIobj) id,	/* Invoke kernel routine */
		     status );

  strx_expf( strlen(cname), cname, 	/* Export data */
	     name_length, name );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(shape))( INTEGER(id), INTEGER(mxndim), INTEGER_ARRAY(dims), INTEGER(ndim), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(mxndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_SHAPE");			/* Mark routine for error reporting */

  adix_shape( (ADIobj) *id, NULL, 0,	/* Invoke kernel routine */
              *mxndim,
	      dims, ndim, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Changing object attributes
 * -------------------------------------------------------------------------
 */
F77_SUBROUTINE(adifn(alter))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(dims),
                              INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_ALTER");			/* Mark routine for error reporting */

  adix_alter( (ADIobj) *id, NULL, 0,    /* Invoke kernel routine */
              *ndim, dims, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(calter))( INTEGER(id), CHARACTER(name), INTEGER(ndim),
                               INTEGER_ARRAY(dims),
                               INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_CALTER");		/* Mark routine for error reporting */

  adix_alter( (ADIobj) *id, name, 	/* Invoke kernel routine */
              name_length,
              *ndim, dims, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(ccell))( INTEGER(id), CHARACTER(name), INTEGER(ndim),
                              INTEGER_ARRAY(index), INTEGER(cid),
                              INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(index)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_CCELL");			/* Mark routine for error reporting */

  adix_cell( (ADIobj) *id, name, 	/* Invoke kernel routine */
              name_length,
              *ndim, index,
              (ADIobj *) cid, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(cell))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(index),
                             INTEGER(cid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(index)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_CELL");			/* Mark routine for error reporting */

  adix_cell( (ADIobj) *id, NULL, 0,	/* Invoke kernel routine */
              *ndim, index,
              (ADIobj *) cid, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(cslice))( INTEGER(id), CHARACTER(name), INTEGER(ndim),
                               INTEGER_ARRAY(diml), INTEGER_ARRAY(dimu),
                               INTEGER(sid),
                               INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(diml)
  GENPTR_INTEGER_ARRAY(dimu)
  GENPTR_INTEGER(sid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_CSLICE");		/* Mark routine for error reporting */

  adix_slice( (ADIobj) *id, name, 	/* Invoke kernel routine */
              name_length,
              *ndim, diml, dimu,
              (ADIobj *) sid, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(slice))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(diml),
                              INTEGER_ARRAY(dimu), INTEGER(sid),
                              INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(diml)
  GENPTR_INTEGER_ARRAY(dimu)
  GENPTR_INTEGER(sid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_SLICE");			/* Mark routine for error reporting */

  adix_slice( (ADIobj) *id, NULL, 0,    /* Invoke kernel routine */
	      *ndim, diml, dimu,
	      (ADIobj *) sid, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Object destruction
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(erase))( INTEGER(id), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;             /* Standard entry checks */

  _ERR_IN("ADI_ERASE");			/* Mark routine for error reporting */

  adix_erase( (ADIobj *) id, 1, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(cerase))( INTEGER(id), CHARACTER(name),
			    INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  _ERR_IN("ADI_CERASE");		/* Mark routine for error reporting */

  adix_cerase( *id, name,
	       name_length, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Symbol packages
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(reqpkg))( CHARACTER(pkg), INTEGER(status) TRAIL(pkg) )
  {
  GENPTR_CHARACTER(pkg)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_REQPKG");

  ADIpkgRequire( pkg, pkg_length, status );

  _ERR_OUT;
  }


/* -------------------------------------------------------------------------
 * Data system routines
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(fclose))( INTEGER(id), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;			/* Check initialised and ok */

  _ERR_IN("ADI_FCLOSE");		/* Mark routine for error reporting */

/* Invoke kernel routine to close file */
  ADIfsysFileClose( (ADIobj) *id, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(fcreat))( CHARACTER(fspec), INTEGER(id),
                               INTEGER(fid), INTEGER(status)
                               TRAIL(fspec) )
  {
  GENPTR_CHARACTER(fspec)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(fid)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;			/* Check initialised and ok */

  _ERR_IN("ADI_FCREAT");		/* Mark routine for error reporting */

/* Invoke kernel routine to create file */
  adix_fcreat( fspec, fspec_length, (ADIobj) *id, (ADIobj *) fid, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(fopen))( CHARACTER(fspec), CHARACTER(cls),
                              CHARACTER(mode), INTEGER(id), INTEGER(status)
                              TRAIL(fspec) TRAIL(cls) TRAIL(mode) )
  {
  GENPTR_CHARACTER(fspec)
  GENPTR_CHARACTER(cls)
  GENPTR_CHARACTER(mode)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;			/* Check initialised and ok */

  _ERR_IN("ADI_FOPEN");			/* Mark routine for error reporting */

  adix_fopen( fspec, fspec_length,	/* Invoke kernel routine */
	      cls, cls_length,
              mode, mode_length,
              (ADIobj *) id, status );

  _ERR_OUT;
  }

/* -------------------------------------------------------------------------
 * Miscellaneous
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(flush))( CHARACTER(grp), INTEGER(status) TRAIL(grp) )
  {
  GENPTR_CHARACTER(grp)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_FLUSH");

  adix_id_flush( grp, grp_length, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(link))( INTEGER(id), CHARACTER(grp), INTEGER(status)
			     TRAIL(grp) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(grp)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_LINK");

  adix_id_link( (ADIobj) *id, grp, grp_length, status );

  _ERR_OUT;
  }


/* ----------------- unincorporated routines --------------- */


F77_SUBROUTINE(adifn(cmnstr))( CHARACTER(str), INTEGER(id),
			    INTEGER(status) TRAIL(str) )
  {
  GENPTR_CHARACTER(str)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;

  _ERR_IN("ADI_CMNSTR");		/* Mark routine for error reporting */

  *id = adix_cmn( str, str_length, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(defrcb))( INTEGER(rid), CHARACTER(name),
			    ADICB rtn, INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(rid)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;                 /* Standard checks */

  _ERR_IN("ADI_DEFRCB");		/* Mark routine for error reporting */

  adix_defrcb( *rid, name, name_length,
	       adix_neweprc( ADI__false,
                  (ADICB) rtn, status ),
	       status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(getfile))( INTEGER(id), INTEGER(fid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(fid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  adix_getfile( *id, (ADIobj *) fid, status );
  }

F77_SUBROUTINE(adifn(getlink))( INTEGER(id), INTEGER(lid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(lid)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  *lid = adix_getlink( *id, status );
  }

F77_SUBROUTINE(adifn(getpath))( INTEGER(id), CHARACTER(path), INTEGER(lpath),
                             INTEGER(status) TRAIL(path) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(path)
  GENPTR_INTEGER(lpath)
  GENPTR_INTEGER(status)

  _chk_init_err; _chk_stat;

  adix_getpath( (ADIobj) *id, ADI__false, path_length, path, lpath, status );
  }

F77_SUBROUTINE(adifn(locrcb))( INTEGER(rid), CHARACTER(name),
			       INTEGER(rtn), INTEGER(status) TRAIL(name) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(rid)
  GENPTR_INTEGER(rtn)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;                 /* Standard checks */

  _ERR_IN("ADI_LOCRCB");		/* Mark routine for error reporting */

  adix_locrcb( *rid, name, name_length,
	       (ADIobj *) rtn, status );

  _ERR_OUT;
  }

F77_SUBROUTINE(adifn(locrep))( CHARACTER(name),
			    INTEGER(id), INTEGER(status) TRAIL(name) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_init; _chk_stat;                 /* Standard checks */

  adix_locrep( name, name_length,
	       (ADIobj *) id, status );
  }

/* End of ADI_F77 defined test
 */
#endif
