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
*	adi_setecs	- set error code and context string
*
*      Property handling :
*
*	adi_delprp	 - delete named property
*	adi_locprp	 - locate named property
*       adi_nprp	 - return number of properties
*       adi_indprp	 - locate property by number
*
*      Structure handling :
*
*	adi_delcmp	 - delete named structure component
*	adi_loccmp	 - locate named structure component
*	adi_ncmp	 - number of components in structure
*	adi_indcmp	 - locate structure component by number
*
*      Reference count :
*
*	adi_clone	 - Clone an identifier
*	adi_refadj	 - Apply increment to object reference count
*	adi_refcnt	 - Return object reference count
*
*      Data system definitions :
*
*       adi_defcac	 - Define class cluster size
*       adi_defcls	 - Define a new class
*	adi_defdes	 - Define a class destructor
*       adi_defgdp	 - Define generic dispatch procedure
*	adi_defgen	 - Define a generic function
*	adi_defmth	 - Define a method
*	adi_defprt	 - Define a class printer
*       adi_defrep	 - Define a new file representation
*	adi_dervd	 - Is an object derived from a specified class
*
*      Method execution :
*
*       adi_exec	 - Execute named method
*       adi_exec2	 - Execute named method with 2 arguments
*	adi_execi	 - Execute named method (ADI string)
*	adi_calnxt	 - Invoke next method in method hierarchy
*
*      System method interfaces :
*
*	adi_ccopy	 - Copy object component to output object component
*	adi_copy	 - Make a copy of an objects data
*	adi_print	 - Text representation of object
*	adi_setlnk	 - Set ADI link field for ADIbase derived class pairs
*	adi_unlnk	 - Reset ADI link field
*
*      Primitive data creation :
*
*       adi_[c]new       - Create n-D object [component] of named type
*       adi_[c]new<t>    - Create n-D object [component] of type <t>
*       adi_[c]new0      - Create scalar object [component] of named type
*       adi_[c]new0<t>   - Create scalar object [component] of type <t>
*       adi_[c]new1      - Create 1-D object [component] of named type
*       adi_[c]new1<t>   - Create 1-D object [component] of type <t>
*       adi_[c]newv<t>   - Create n-D object [component] with value
*       adi_[c]newv0<t>  - Create scalar object [component] with value
*       adi_[c]newv1<t>  - Create 1-D object [component] with value
*
*      Data access :
*
*	adi_find	 - Locate object component
*       adi_[c]get<t>	 - Get n-D object [component] values
*       adi_[c]get0<t>	 - Get scalar object [component] value
*       adi_[c]get1<t>	 - Get 1-D object [component] values
*	adi_[c]map	 - Map object [component] with user specified type
*	adi_[c]map<t>	 - Map object [component] with a specific type
*       adi_[c]put<t>	 - Put n-D object [component] values
*       adi_[c]put0<t>	 - Put scalar object [component] value
*       adi_[c]put1<t>	 - Put 1-D object [component] values
*	adi_[c]putid	 - Put ADI object into object [component]
*       adi_[c]set<t>	 - Set n-D object [component] values
*       adi_[c]set0<t>	 - Set scalar object [component] value
*       adi_[c]set1<t>	 - Set 1-D object [component] values
*	adi_there	 - Does an object component exist?
*	adi_[c]unmap	 - Unmap object [component]
*
*      Enquiry routines :
*
*	adi_clen	 - Enquire string length
* 	adi_[c]shape	 - Enquire object [component] dimensions
*	adi_[c]size	 - Enquire object [component] number of elements
*	adi_[c]state	 - Enquire object [component] data state
* 	adi_name	 - Enquire object name
*	adi_[c]type	 - Enquire object [component] class
*
*      Changing object attributes :
*
*       adi_[c]alter     - Alter object [component] dimensionality
*       adi_[c]cell	 - Index an object [component] array cell
*       adi_[c]slice     - Access object [component] slice
*
*      Object destruction :
*
*	adi_[c]erase	 - Destroy an object [component]
*
*      Object references :
*
*	adi_getref	 - Read an object reference
*	adi_newref	 - Create a reference object
*	adi_putref	 - Write an object reference
*
*      Symbol packages :
*
*	adi_reqpkg	 - Load a package from the search path
*
*      Data system :
*
*       adi_fclose	 - Close a file system object
*       adi_fcreat	 - Create a new file system object
*       adi_fopen	 - Open existing file system object
*
*      Miscellaneous :
*
*	adi_link	 - Link an identifier to a name group
*	adi_flush	 - Erase all objects in a name group

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

/* Toggle this entire file on the value of the ADI_F77 macro
 */
#ifdef ADI_F77

#include "adilist.h"
#include "adistrng.h"
#include "aditable.h"
#include "adicface.h"
#include "adierror.h"
#include "adikrnl.h"
#include "adiarray.h"
#include "adimem.h"
#include "adipkg.h"
#include "adisyms.h"
#include "adifsys.h"
#include "adifface.h"                   /* Prototypes for this module */


/*
 * Macro to assign Fortran logical based on C logical
 */
#define _ASSFLOG(_x,_y) if (_y) (_x) = F77_TRUE; else (_x) = F77_FALSE;


/*
 * Macro to assign Fortran POINTER type given some real pointer sptr
 */
#define EXPORT_POINTER(sptr,dptr_addr) \
{F77_POINTER_TYPE _pnt = (F77_POINTER_TYPE) sptr; \
*((F77_POINTER_TYPE *) dptr_addr) = _pnt;}



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

F77_SUBROUTINE(adifn(setecs))( INTEGER(code), CHARACTER(ctx), INTEGER(status) TRAIL(ctx) )
  {
  GENPTR_INTEGER(code)
  GENPTR_CHARACTER(ctx)
  GENPTR_INTEGER(status)

  adic_setecs( *code, "%*s", status, ctx_length, ctx );
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

  _chk_stat;

  adix_delprp( (ADIobj) *id, pname, pname_length, status );

  _ERR_REP("ADI_DELPRP",Estr__DelObjPrp);
  }

F77_SUBROUTINE(adifn(locprp))( INTEGER(id), CHARACTER(pname), INTEGER(pid),
			    INTEGER(status) TRAIL(pname) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(pname)
  GENPTR_INTEGER(pid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_locprp( (ADIobj) *id, pname, pname_length, (ADIobj *) pid, status );

  _ERR_REP("ADI_DELPRP",Estr__LocObjPrp);
  }

F77_SUBROUTINE(adifn(nprp))( INTEGER(id), INTEGER(nprp), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(nprp)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_nprp( (ADIobj) *id, nprp, status );

  _ERR_REP( "ADI_NPRP", Estr__GetNumPrp );
  }

F77_SUBROUTINE(adifn(indprp))( INTEGER(id), INTEGER(index), INTEGER(pid),
			    INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(index)
  GENPTR_INTEGER(pid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_indprp( (ADIobj) *id, *index, (ADIobj *) pid, status );

  _ERR_REP("ADI_INDPRP",Estr__IndObjPrp);
  }


/* -------------------------------------------------------------------------
 * Component handling
 * -------------------------------------------------------------------------
 */
F77_SUBROUTINE(adifn(delcmp))( INTEGER(id), CHARACTER(cname), INTEGER(status)
			       TRAIL(cname) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(cname)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_delcmp( (ADIobj) *id, cname, cname_length, status );

  _ERR_REP( "ADI_DELCMP", Estr__DelStrCmp );
  }

F77_SUBROUTINE(adifn(loccmp))( INTEGER(id), CHARACTER(cname), INTEGER(cid),
			       INTEGER(status) TRAIL(cname) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(cname)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_loccmp( (ADIobj) *id, cname, cname_length, (ADIobj *) cid, status );

  _ERR_REP( "ADI_LOCCMP", Estr__LocStrCmp );
  }

F77_SUBROUTINE(adifn(ncmp))( INTEGER(id), INTEGER(ncmp), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(ncmp)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_ncmp( (ADIobj) *id, ncmp, status );

  _ERR_REP( "ADI_NCMP", Estr__GetNumCmp );
  }

F77_SUBROUTINE(adifn(indcmp))( INTEGER(id), INTEGER(index), INTEGER(cid),
			       INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(index)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_indcmp( (ADIobj) *id, *index, (ADIobj *) cid, status );

  _ERR_REP( "ADI_INDCMP", Estr__IndStrCmp );
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

  _chk_stat;

  *((ADIobj *) cid) = adix_clone( (ADIobj) *id, status );

  _ERR_REP( "ADI_CLONE", Estr__CloObjId );
  }

F77_SUBROUTINE(adifn(refadj))( INTEGER(id), INTEGER(incr), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(incr)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

/* Increment reference count */
  adix_refadj( (ADIobj) *id, *incr, status );

  _ERR_REP( "ADI_REFADJ", Estr__AdjObjRef );
  }

F77_SUBROUTINE(adifn(refcnt))( INTEGER(id), INTEGER(cnt), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(cnt)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

/* Retrieve reference count */
  *cnt = adix_refcnt( (ADIobj) *id, status );

  _ERR_REP( "ADI_REFCNT", Estr__GetObjRef );
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

  _chk_stat;                 		/* Standard checks */

  ADIdefClass_e( name, name_length, parents, parents_length,
		 members, members_length, (ADIobj *) tid, status );

  _ERR_REP( "ADI_DEFCLS", Estr__DefCls );
  }


F77_SUBROUTINE(adifn(defcac))( INTEGER(clsid), INTEGER(number),
			       INTEGER(status) )
  {
  GENPTR_INTEGER(clsid)
  GENPTR_INTEGER(number)
  GENPTR_INTEGER(status)

  _chk_stat;                 		/* Standard checks */

  ADIdefClassCluster( (ADIobj) *clsid, *number, status );

  _ERR_REP( "ADI_DEFCAC", Estr__DefClsCac );
  }

F77_SUBROUTINE(adifn(defdes))( INTEGER(clsid), ADIfMethodCB rtn,
			       INTEGER(status) )
  {
  _chk_stat;             	/* Standard entry checks */

  ADIkrnlDefDestruc( (ADIobj) *clsid,
		   ADIkrnlNewEproc( ADI__false, (ADICB) rtn, status ),
		   status );

  _ERR_REP( "ADI_DEFDES", Estr__DefClsDes );
  }

F77_SUBROUTINE(adifn(defgdp))( INTEGER(genid), ADICB rtn, INTEGER(status) )
  {
  GENPTR_INTEGER(genid)
  GENPTR_INTEGER(status)

  _chk_stat;                 		/* Standard checks */

  adix_defgdp( *genid,
	       ADIkrnlNewEproc( ADI__false, rtn, status ),
	       status );

  _ERR_REP( "ADI_DEFGDP", Estr__DefGenDis );
  }

F77_SUBROUTINE(adifn(defgen))( CHARACTER(name), CHARACTER(options),
			    ADICB rtn, INTEGER(id),
			    INTEGER(status) TRAIL(name) TRAIL(options) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(options)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;                 		/* Standard checks */

  adix_defgen( name, name_length, options, options_length,
	       ADIkrnlNewEproc( ADI__false, rtn, status ),
	       (ADIobj *) id, status );

  _ERR_REP( "ADI_DEFGEN", Estr__DefGen );
  }

F77_SUBROUTINE(adifn(defmth))( CHARACTER(spec), ADIfMethodCB rtn,
			    INTEGER(id), INTEGER(status) TRAIL(spec) )
  {
  _chk_stat;                 		/* Standard entry checks */

  adix_defmth( spec, spec_length,     	/* Invoke kernel routine */
	       ADIkrnlNewEproc( ADI__false, (ADICB) rtn, status ),
	       (ADIobj *) id, status );

  _ERR_REP( "ADI_DEFMTH", Estr__DefMth );
  }

F77_SUBROUTINE(adifn(defprt))( INTEGER(clsid), ADIfMethodCB rtn,
			       INTEGER(status) )
  {
  _chk_stat;             		/* Standard entry checks */

  ADIkrnlDefPrnt( (ADIobj) *clsid,
		   ADIkrnlNewEproc( ADI__false, (ADICB) rtn, status ),
		   status );

  _ERR_REP( "ADI_DEFPRT", Estr__DefClsPrt );
  }

F77_SUBROUTINE(adifn(defrep))( CHARACTER(name), INTEGER(id),
			    INTEGER(status) TRAIL(name) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;                 		/* Standard checks */

  adix_defrep( name, name_length, (ADIobj *) id, status );

  _ERR_REP( "ADI_DEFREP", Estr__DefFilRep );
  }

F77_SUBROUTINE(adifn(dervd))( INTEGER(id), CHARACTER(name), LOGICAL(der),
		       INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_LOGICAL(der)
  GENPTR_INTEGER(status)

  ADIlogical	lder;

  _chk_stat;

  lder = ADIkrnlChkDerived( (ADIobj) *id, name, name_length, status );

  _ASSFLOG(*der,lder);		/* Set return value */

  _ERR_REP( "ADI_DERVD", Estr__TstObjDer );
  }


/* -------------------------------------------------------------------------
 * Method execution
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(calnxt))( INTEGER(status) )
  {
  GENPTR_INTEGER(status)

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

  _chk_stat;             		/* Standard entry checks */

  *res = (F77_INTEGER_TYPE)
	  adix_exec( func, func_length,
		     *narg, (ADIobj *) args, status );

  _ERR_REP( "ADI_EXEC", Estr__ExeMth );
  }

F77_SUBROUTINE(adifn(exec2))( CHARACTER(func), INTEGER(arg1),
			  INTEGER(arg2), INTEGER(res),
			  INTEGER(status) TRAIL(func) )
  {
  GENPTR_CHARACTER(func)
  GENPTR_INTEGER(arg1)
  GENPTR_INTEGER(arg2)
  GENPTR_INTEGER(res)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

  *res = (F77_INTEGER_TYPE)
	  adix_exec2( func, func_length, (ADIobj) *arg1,
		      (ADIobj) *arg2, status );

  _ERR_REP( "ADI_EXEC2", Estr__ExeMth );
  }

F77_SUBROUTINE(adifn(execi))( INTEGER(func), INTEGER(narg),
			   INTEGER_ARRAY(args), INTEGER(res),
			   INTEGER(status) )
  {
  GENPTR_INTEGER(func)
  GENPTR_INTEGER(narg)
  GENPTR_INTEGER_ARRAY(args)
  GENPTR_INTEGER(res)
  GENPTR_INTEGER(status)

  _chk_stat;

  *res = (F77_INTEGER_TYPE)
	adix_execi( (ADIobj) *func, *narg,
	  (ADIobj *) args, status );

  _ERR_REP( "ADI_EXECI", Estr__ExeMth );
  }

/* -------------------------------------------------------------------------
 * System method interfaces
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(ccopy))( INTEGER(in), CHARACTER(inmem), INTEGER(out),
			      CHARACTER(outmem), INTEGER(status)
			      TRAIL(inmem) TRAIL(outmem) )
  {
  GENPTR_INTEGER(in)
  GENPTR_CHARACTER(inmem)
  GENPTR_INTEGER(out)
  GENPTR_CHARACTER(outmem)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_ccopy( (ADIobj) *in, inmem, inmem_length, (ADIobj) *out,
	      outmem, outmem_length, status );

  _ERR_REP( "ADI_CCOPY", Estr__CopObjCmp );
  }


F77_SUBROUTINE(adifn(copy))( INTEGER(id), INTEGER(cid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_stat;

  *((ADIobj *) cid) = adix_copy( (ADIobj) *id, status );

  _ERR_REP( "ADI_COPY", Estr__CopObj );
  }

F77_SUBROUTINE(adifn(print))( INTEGER(id), INTEGER(status) )
  {
  _chk_stat;

  adix_print( ADIcvStdOut, *id, 0, ADI__false, status );

  _ERR_REP( "ADI_PRINT", Estr__PriObj );
  }

F77_SUBROUTINE(adifn(setlnk))( INTEGER(id), INTEGER(lid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(lid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_setlnk( (ADIobj) *id, (ADIobj) *lid, status );

  _ERR_REP( "ADI_SETLNK", Estr__LnkFilObj );
  }

F77_SUBROUTINE(adifn(unlnk))( INTEGER(id), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_unlnk( (ADIobj) *id, status );

  _ERR_REP( "ADI_UNLNK", Estr__UlnFilObj );
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

  _chk_stat;

  adix_newn( ADI__nullid, NULL, 0, cls, cls_length, *ndim,
	     dims, (ADIobj *) id, status );

  _ERR_REP( "ADI_NEW", Estr__CreObjDat );
  }

F77_SUBROUTINE(adifn(new0))( CHARACTER(cls), INTEGER(id),
			  INTEGER(status) TRAIL(cls) )
  {
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_newn( ADI__nullid, NULL, 0, cls, cls_length, 0,
	     NULL, (ADIobj *) id, status );

  _ERR_REP( "ADI_NEW0", Estr__CreObjDat );
  }

F77_SUBROUTINE(adifn(new1))( CHARACTER(cls), INTEGER(nval),
		INTEGER(id), INTEGER(status) TRAIL(cls) )
  {
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(nval)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_newn( ADI__nullid, NULL, 0, cls, cls_length, 1, nval,
	    (ADIobj *) id, status );

  _ERR_REP( "ADI_NEW1", Estr__CreObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(new,_t))( INTEGER(ndim), INTEGER_ARRAY(dims), INTEGER(id), INTEGER(status) )\
  { \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER_ARRAY(dims) \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, *ndim, dims, NULL, \
	      &_TM_alloc(_t), 0, (ADIobj *) id, status );\
  _ERR_REP( _TM_fnames(new,_t), Estr__CreObjDat );}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(new0,_t))( INTEGER(id), INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 0, NULL, NULL, \
	     &_TM_alloc(_t), 0, (ADIobj *) id, status ); \
  _ERR_REP( _TM_fnames(new0,_t), Estr__CreObjDat );}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(new1,_t))( INTEGER(nval), INTEGER(id), INTEGER(status) )\
  { \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 1, nval, NULL, \
	      &_TM_alloc(_t), 0, (ADIobj *) id, status );\
  _ERR_REP( _TM_fnames(new1,_t), Estr__CreObjDat );}

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
  _chk_stat; \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, *ndim, dims, value, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      (ADIobj *) id, status );\
  _ERR_REP( _TM_fnames(newv,_t), Estr__CreObjDat );}

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

  _chk_stat;

  adix_new_n( ADI__false, ADI__nullid, NULL, 0, *ndim, dims, value,
	      &_TM_alloc(c), value_length, (ADIobj *) id, status );

  _ERR_REP( "ADI_NEWVC", Estr__CreObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(newv0,_t))( _TM_ftype(_t) *value, INTEGER(id), INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 0, NULL, value, \
	     &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	     (ADIobj *) id, status ); \
  _ERR_REP( _TM_fnames(newv0,_t), Estr__CreObjDat );}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(newv0c))( CHARACTER(value), INTEGER(id),
			    INTEGER(status) TRAIL(value) )
  {
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 0, NULL,
	      (void *) value, &_TM_alloc(c),
	      value_length, (ADIobj *) id, status );

  _ERR_REP( "ADI_NEWV0C", Estr__CreObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(newv1,_t))( INTEGER(nval), _TM_ftype(_t) value[], INTEGER(id), INTEGER(status) )\
  { \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 1, nval, value, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      (ADIobj *) id, status );\
  _ERR_REP( _TM_fnames(newv1,_t), Estr__CreObjDat );}

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

  _chk_stat;

  adix_new_n( ADI__false, ADI__nullid, NULL, 0, 1, nval, value,
	      &_TM_alloc(c), value_length, (ADIobj *) id, status );

  _ERR_REP( "ADI_NEWV1C", Estr__CreObjDat );
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
  _chk_stat; \
  adix_get_n( 0, (ADIobj) *id, NULL, 0, *ndim, dims, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      value, NULL, status );\
  _ERR_REP( _TM_fnames(get,_t), Estr__GetObjDat );}

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

  _chk_stat;

  adix_get_n( 0, (ADIobj) *id, NULL, 0, *ndim, dims, &_TM_alloc(c),
	      value_length, value, NULL, status );

  _ERR_REP( "ADI_GETC", Estr__GetObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(get0,_t))( INTEGER(id), _TM_ftype(_t) *value, INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_get_n( 0, (ADIobj) *id, NULL, 0, 0, NULL, &_TM_alloc(_t), \
	     sizeof(_TM_ftype(_t)), value, NULL, status ); \
  _ERR_REP( _TM_fnames(get0,_t), Estr__GetObjDat );}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(get0c))( INTEGER(id), CHARACTER(value), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_get_n( 0, (ADIobj) *id, NULL, 0, 0, NULL, &_TM_alloc(c),
	      value_length, value, NULL, status );

  _ERR_REP( "ADI_GET0C", Estr__GetObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(get1,_t))( INTEGER(id), INTEGER(mxval), \
		_TM_ftype(_t) *value, INTEGER(nactval), INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(mxval) \
  GENPTR_INTEGER(nactval) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_get_n( 0, (ADIobj) *id, NULL, 0, 1, mxval, &_TM_alloc(_t), \
	      sizeof(_TM_ftype(_t)), value, nactval, status );\
  _ERR_REP( _TM_fnames(get1,_t), Estr__GetObjDat );}

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

  _chk_stat;

  adix_get_n( 0, (ADIobj) *id, NULL, 0, 1, mxval, &_TM_alloc(c), value_length,
	      value, nactval, status );

  _ERR_REP( "ADI_GET1C", Estr__GetObjDat );
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

  void	*mptr;

  _chk_stat;

  adix_map_t( 0, (ADIobj) *id, NULL, 0, type, type_length, mode,
	      mode_length, &mptr, status );

  _EXPORT_POINTER(mptr,vptr);

  _ERR_REP( "ADI_MAP", Estr__MapObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(map,_t))( INTEGER(id), CHARACTER(mode), \
		POINTER(vptr), INTEGER(status) TRAIL(mode) ) { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(mode) \
  GENPTR_POINTER(vptr) \
  GENPTR_INTEGER(status) \
  void	*mptr; \
  _chk_stat; \
  adix_map_n( 0, (ADIobj) *id, NULL, 0, mode, mode_length, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      &mptr, status );\
  _EXPORT_POINTER(mptr,vptr); \
  _ERR_REP( _TM_fnames(map,_t), Estr__MapObjDat );}

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
  _chk_stat; \
  adix_put_n( 0, (ADIobj) *id, NULL, 0, *ndim, dims, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_REP( _TM_fnames(put,_t), Estr__PutObjDat );}

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

  _chk_stat;

  adix_put_n( 0, (ADIobj) *id, NULL, 0, *ndim, dims, &_TM_alloc(c),
	      value_length, value, status );

  _ERR_REP( "ADI_PUTC", Estr__PutObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(put0,_t))( INTEGER(id), _TM_ftype(_t) *value, INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_put_n( 0, (ADIobj) *id, \
	      NULL, 0, 0, NULL, &_TM_alloc(_t),\
	      sizeof(_TM_ftype(_t)), value, status );\
  _ERR_REP( _TM_fnames(put0,_t), Estr__PutObjDat );}

_genproc(b)	_genproc(w)
_genproc(i)	_genproc(r)	_genproc(d)	_genproc(l)
#undef _genproc

F77_SUBROUTINE(adifn(put0c))( INTEGER(id), CHARACTER(value), INTEGER(status) TRAIL(value) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_put_n( 0, (ADIobj) *id, NULL, 0, 0, NULL, &_TM_alloc(c),
	      value_length, (void *) value, status );

  _ERR_REP( "ADI_PUT0C", Estr__PutObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(put1,_t))( INTEGER(id), INTEGER(nval), \
		_TM_ftype(_t) *value, INTEGER(status) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_put_n( 0, (ADIobj) *id, NULL, 0, 1, nval, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_REP( _TM_fnames(put1,_t), Estr__PutObjDat );}

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

  _chk_stat;

  adix_put_n( 0, (ADIobj) *id, NULL, 0, 1, nval, &_TM_alloc(c), value_length,
	      value, status );

  _ERR_REP( "ADI_PUT1C", Estr__PutObjDat );
  }

F77_SUBROUTINE(adifn(unmap))( INTEGER(id), POINTER(vptr), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_POINTER(vptr)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_unmap_n( (ADIobj) *id, NULL, 0, (void *) *vptr, status );

  _ERR_REP( "ADI_UNMAP", Estr__UnmObjDat );
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

  _chk_stat;

  adix_newn( (ADIobj) *pid, name, name_length, cls, cls_length, *ndim,
	     dims, NULL, status );

  _ERR_REP( "ADI_CNEW", Estr__CreObjDat );
  }

F77_SUBROUTINE(adifn(cnew0))( INTEGER(pid), CHARACTER(name),
			      CHARACTER(cls),
			      INTEGER(status) TRAIL(name) TRAIL(cls) )
  {
  GENPTR_INTEGER(pid)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_newn( (ADIobj) *pid, name, name_length, cls, cls_length, 0,
	     NULL, NULL, status );

  _ERR_REP( "ADI_CNEW0", Estr__CreObjDat );
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

  _chk_stat;

  adix_newn( (ADIobj) *pid, name, name_length, cls, cls_length, 1, nval,
	     NULL, status );

  _ERR_REP( "ADI_CNEW1", Estr__CreObjDat );
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
  _chk_stat; \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, *ndim, dims, NULL, \
	      &_TM_alloc(_t), 0, NULL, status );\
  _ERR_REP( _TM_fnames(cnew,_t), Estr__CreObjDat );}

_genproc(b)	_genproc(w)	_genproc(i)	_genproc(r)
_genproc(d)	_genproc(l)	_genproc(c)
#undef _genproc

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnew0,_t))( INTEGER(pid), CHARACTER(name), INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 0, NULL, NULL, \
	     &_TM_alloc(_t), 0, NULL, status ); \
  _ERR_REP( _TM_fnames(cnew0,_t), Estr__CreObjDat );}

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
  _chk_stat; \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 1, nval, NULL, \
	      &_TM_alloc(_t), 0, NULL, status );\
  _ERR_REP( _TM_fnames(cnew1,_t), Estr__CreObjDat );}

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
  _chk_stat; \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, *ndim, dims, value, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      NULL, status );\
  _ERR_REP( _TM_fnames(cnewv,_t), Estr__CreObjDat );}

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

  _chk_stat;

  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, *ndim, dims, value,
	      &_TM_alloc(c), value_length, NULL, status );

  _ERR_REP( "ADI_CNEWVC", Estr__CreObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnewv0,_t))( INTEGER(pid), CHARACTER(name), _TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 0, NULL, value, \
	     &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	     NULL, status ); \
  _ERR_REP( _TM_fnames(cnewv0,_t), Estr__CreObjDat );}

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

  _chk_stat;

  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 0, NULL,
	      (void *) value, &_TM_alloc(c), value_length, NULL, status );

  _ERR_REP( "ADI_CNEWV0C", Estr__CreObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cnewv1,_t))( INTEGER(pid), CHARACTER(name), INTEGER(nval), _TM_ftype(_t) value[], INTEGER(status) TRAIL(name) )\
  { \
  GENPTR_INTEGER(pid) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(nval) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 1, nval, value, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      NULL, status );\
  _ERR_REP( _TM_fnames(cnewv1,_t), Estr__CreObjDat );}

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

  _chk_stat;

  adix_new_n( ADI__false, (ADIobj) *pid, name, name_length, 1, nval, value,
	      &_TM_alloc(c), value_length, NULL, status );

  _ERR_REP( "ADI_CNEWV1C", Estr__CreObjDat );
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
  _chk_stat; \
  adix_get_n( 0, (ADIobj) *id, name, name_length, *ndim, dims, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      value, NULL, status );\
  _ERR_REP( _TM_fnames(cget,_t), Estr__CreObjDat );}

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

  _chk_stat;

  adix_get_n( 0, (ADIobj) *id, name, name_length, *ndim, dims,
	      &_TM_alloc(c), value_length, value, NULL, status );

  _ERR_REP( "ADI_CGETC", Estr__GetObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cget0,_t))( INTEGER(id), CHARACTER(name), \
	 _TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_get_n( 0, (ADIobj) *id, name, name_length, 0, NULL, &_TM_alloc(_t), \
	     sizeof(_TM_ftype(_t)), value, NULL, status ); \
  _ERR_REP( _TM_fnames(cget0,_t), Estr__CreObjDat );}

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

  _chk_stat;

  adix_get_n( 0, (ADIobj) *id, name, name_length, 0, NULL, &_TM_alloc(c),
	      value_length, value, NULL, status );

  _ERR_REP( "ADI_CGET0C", Estr__GetObjDat );
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
  _chk_stat; \
  adix_get_n( 0, (ADIobj) *id, name, name_length, 1, mxval, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      value, nactval, status );\
  _ERR_REP( _TM_fnames(cget1,_t), Estr__CreObjDat );}

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

  _chk_stat;

  adix_get_n( 0, (ADIobj) *id, name, name_length, 1, mxval, &_TM_alloc(c),
	      value_length, value, nactval, status );

  _ERR_REP( "ADI_CGET1C", Estr__CreObjDat );
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

  void	*mptr;

  _chk_stat;

  adix_map_t( 0, (ADIobj) *id, name, name_length, type, type_length, mode,
	      mode_length, &mptr, status );

  _EXPORT_POINTER(mptr,vptr);

  _ERR_REP( "ADI_CMAP", Estr__MapObjDat );
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
  void		*mptr; \
  _chk_stat; \
  adix_map_n( 0, (ADIobj) *id, name, name_length, mode, mode_length, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      &mptr, status );\
  _EXPORT_POINTER(mptr,vptr);\
  _ERR_REP( _TM_fnames(cmap,_t), Estr__MapObjDat );}

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
  _chk_stat; \
  adix_put_n( 0, (ADIobj) *id, name, name_length, *ndim, dims, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_REP( _TM_fnames(cput,_t), Estr__PutObjDat );}

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

  _chk_stat;

  adix_put_n( 0, (ADIobj) *id, name, name_length, *ndim, dims,
	      &_TM_alloc(c), value_length, value, status );

  _ERR_REP( "ADI_CPUTC", Estr__PutObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cput0,_t))( INTEGER(id), CHARACTER(name), \
	 _TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_put_n( 0, (ADIobj) *id, name, name_length, \
	      0, NULL, &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	     value, status ); \
  _ERR_REP( _TM_fnames(cput0,_t), Estr__PutObjDat );}

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

  _chk_stat;

  adix_put_n( 0, (ADIobj) *id, name, name_length, 0, NULL, &_TM_alloc(c),
	      value_length, (void *) value, status );

  _ERR_REP( "ADI_CPUT0C", Estr__PutObjDat );
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
  _chk_stat; \
  adix_put_n( 0, (ADIobj) *id, name, name_length, 1, nval, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_REP( _TM_fnames(cput1,_t), Estr__PutObjDat );}

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

  _chk_stat;

  adix_put_n( 0, (ADIobj) *id, name, name_length, 1, nval,
	      &_TM_alloc(c), value_length, value, status );

  _ERR_REP( "ADI_CPUT1C", Estr__PutObjDat );
  }

#define _genproc(_t) \
F77_SUBROUTINE(_TM_fname(cset0,_t))( INTEGER(id), CHARACTER(name), \
	 _TM_ftype(_t) *value, INTEGER(status) TRAIL(name) ) \
  { \
  GENPTR_INTEGER(id) \
  GENPTR_CHARACTER(name) \
  GENPTR_INTEGER(status) \
  _chk_stat; \
  adix_set_n( 0, (ADIobj) *id, name, name_length, \
	      0, NULL, &_TM_alloc(_t),\
	     sizeof(_TM_ftype(_t)), value, status ); \
  _ERR_REP( _TM_fnames(cset0,_t), Estr__SetObjDat );}

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

  _chk_stat;

  adix_set_n( 0, (ADIobj) *id, name, name_length, 0, NULL, &_TM_alloc(c),
	      value_length, (void *) value, status );

  _ERR_REP( "ADI_CSET0C", Estr__SetObjDat );
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
  _chk_stat; \
  adix_set_n( 0, (ADIobj) *id, name, name_length, 1, nval, \
	      &_TM_alloc(_t), sizeof(_TM_ftype(_t)), \
	      value, status );\
  _ERR_REP( _TM_fnames(cset1,_t), Estr__SetObjDat );}

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

  _chk_stat;

  adix_set_n( 0, (ADIobj) *id, name, name_length, 1, nval,
	      &_TM_alloc(c), value_length, value, status );

  _ERR_REP( "ADI_CSET1C", Estr__SetObjDat );
  }

F77_SUBROUTINE(adifn(putid))( INTEGER(id), INTEGER(vid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(vid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_putid( (ADIobj) *id, NULL, 0, (ADIobj) *vid, status );

  _ERR_REP( "ADI_PUTID", Estr__PutObjDat );
  }

F77_SUBROUTINE(adifn(cputid))( INTEGER(id), CHARACTER(name),
			    INTEGER(vid), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(vid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_putid( (ADIobj) *id, name, name_length, (ADIobj) *vid, status );

  _ERR_REP( "ADI_CPUTID", Estr__PutObjDat );
  }

F77_SUBROUTINE(adifn(cunmap))( INTEGER(id), CHARACTER(name), POINTER(vptr),
			       INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_POINTER(vptr)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_unmap_n( (ADIobj) *id, name, name_length, (void *) *vptr, status );

  _ERR_REP( "ADI_CUNMAP", Estr__UnmObjDat );
  }

F77_SUBROUTINE(adifn(find))( INTEGER(id), CHARACTER(name),
			     INTEGER(cid), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_stat;

  *((ADIobj *) cid) = adix_find( (ADIobj) *id, name, name_length, status );

  _ERR_REP( "ADI_FIND", Estr__LocObjCmp );
  }

F77_SUBROUTINE(adifn(there))( INTEGER(id), CHARACTER(name),
			      LOGICAL(there), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_LOGICAL(there)
  GENPTR_INTEGER(status)

  ADIlogical	cres;			/* Result from kernel */

  _chk_stat;

  cres = adix_there( (ADIobj) *id, name, name_length, status );

  _ASSFLOG(*there,cres);		/* Set return value */

  _ERR_REP( "ADI_THERE", Estr__TstObjExi );
  }

/* -------------------------------------------------------------------------
 * Object enquiry
 * -------------------------------------------------------------------------
 */
F77_SUBROUTINE(adifn(clen))( INTEGER(id), INTEGER(clen), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(clen)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

  ADIstrngGetLen( (ADIobj) *id, clen, status );

  _ERR_REP( "ADI_CLEN", Estr__GetObjShp );
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

  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_shape( (ADIobj) *id, name, name_length, *mxndim,
	      dims, ndim, status );

  _ERR_REP( "ADI_CSHAPE", Estr__GetObjShp );
  }

F77_SUBROUTINE(adifn(csize))( INTEGER(id), CHARACTER(name), INTEGER(nelm),
			      INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(nelm)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_size( (ADIobj) *id, name, name_length, nelm, status );

  _ERR_REP( "ADI_CSIZE", Estr__GetObjShp );
  }

F77_SUBROUTINE(adifn(cstate))( INTEGER(id), CHARACTER(name),
			       LOGICAL(state), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_LOGICAL(state)
  GENPTR_INTEGER(status)

  ADIlogical	cres;

  _chk_stat;             /* Standard entry checks */

  cres = adix_state( (ADIobj) *id, name, name_length, status );

  _ASSFLOG(*state,cres);		/* Set return value */

  _ERR_REP( "ADI_CSTATE", Estr__TstObjSta );
  }


F77_SUBROUTINE(adifn(ctype))( INTEGER(id), CHARACTER(name), CHARACTER(cls),
			      INTEGER(status) TRAIL(name) TRAIL(cls) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(status)

  _chk_stat;             /* Standard entry checks */

  ADIstrngExport( adix_qcls( (ADIobj) *id, name, name_length, status),
		  ADI__false, cls, cls_length, status );

  _ERR_REP( "ADI_CTYPE", Estr__GetObjTyp );
  }

F77_SUBROUTINE(adifn(name))( INTEGER(id), CHARACTER(name), INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_name( (ADIobj) *id, ADI__false, name, name_length, status );

  _ERR_REP( "ADI_NAME", Estr__GetObjNam );
  }

F77_SUBROUTINE(adifn(shape))( INTEGER(id), INTEGER(mxndim), INTEGER_ARRAY(dims), INTEGER(ndim), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(mxndim)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

  adix_shape( (ADIobj) *id, NULL, 0, *mxndim, dims, ndim, status );

  _ERR_REP( "ADI_SHAPE", Estr__GetObjShp );
  }

F77_SUBROUTINE(adifn(size))( INTEGER(id), INTEGER(nelm), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(nelm)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

  adix_size( (ADIobj) *id, NULL, 0, nelm, status );

  _ERR_REP( "ADI_SIZE", Estr__GetObjShp );
  }


F77_SUBROUTINE(adifn(state))( INTEGER(id), LOGICAL(state), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_LOGICAL(state)
  GENPTR_INTEGER(status)

  ADIlogical	cres;

  _chk_stat;             /* Standard entry checks */

  cres = adix_state( (ADIobj) *id, NULL, 0, status );

  _ASSFLOG(*state,cres);		/* Set return value */

  _ERR_REP( "ADI_STATE", Estr__TstObjSta );
  }

F77_SUBROUTINE(adifn(type))( INTEGER(id), CHARACTER(cls), INTEGER(status) TRAIL(cls) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(cls)
  GENPTR_INTEGER(status)

  _chk_stat;             /* Standard entry checks */

  ADIstrngExport( adix_qcls( (ADIobj) *id, NULL, 0, status), ADI__false,
		  cls, cls_length, status );

  _ERR_REP( "ADI_TYPE", Estr__GetObjTyp );
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

  _chk_stat;             		/* Standard entry checks */

  ADIaryAlter( (ADIobj) *id, NULL, 0, *ndim, dims, status );

  _ERR_REP( "ADI_ALTER", Estr__AltObjShp );
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

  _chk_stat;             		/* Standard entry checks */

  ADIaryAlter( (ADIobj) *id, name, name_length, *ndim, dims, status );

  _ERR_REP( "ADI_CALTER", Estr__AltObjShp );
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

  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_cell( (ADIobj) *id, name, name_length, *ndim, index,
	      (ADIobj *) cid, status );

  _ERR_REP( "ADI_CCELL", Estr__LocObjCel );
  }

F77_SUBROUTINE(adifn(cell))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(index),
			     INTEGER(cid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(ndim)
  GENPTR_INTEGER_ARRAY(index)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  _chk_stat;             		/* Standard entry checks */

  adix_cell( (ADIobj) *id, NULL, 0, *ndim, index,
	      (ADIobj *) cid, status );

  _ERR_REP( "ADI_CELL", Estr__LocObjCel );
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

  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_slice( (ADIobj) *id, name, name_length,
	      *ndim, diml, dimu, (ADIobj *) sid, status );

  _ERR_REP( "ADI_CSLICE", Estr__LocObjSli );
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

  _chk_stat;             		/* Standard entry checks */

/* Invoke kernel routine */
  adix_slice( (ADIobj) *id, NULL, 0, *ndim, diml, dimu,
	      (ADIobj *) sid, status );

  _ERR_REP( "ADI_SLICE", Estr__LocObjSli );
  }

/* -------------------------------------------------------------------------
 * Object destruction
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(erase))( INTEGER(id), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;             	/* Standard entry checks */

  adix_erase( (ADIobj *) id, 1, status );

  _ERR_REP( "ADI_ERASE", Estr__DelObj );
  }

F77_SUBROUTINE(adifn(cerase))( INTEGER(id), CHARACTER(name),
			    INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_cerase( *id, name, name_length, status );

  _ERR_REP( "ADI_CERASE", Estr__DelObj );
  }

/* -------------------------------------------------------------------------
 * Symbol packages
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(reqpkg))( CHARACTER(pkg), INTEGER(status) TRAIL(pkg) )
  {
  GENPTR_CHARACTER(pkg)
  GENPTR_INTEGER(status)

  _chk_stat;

  ADIpkgRequire( pkg, pkg_length, status );

  _ERR_REP( "ADI_REQPKG", Estr__LodDefPkg );
  }

/* -------------------------------------------------------------------------
 * Object references
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(getref))( INTEGER(id), INTEGER(rid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(rid)
  GENPTR_INTEGER(status)

  _chk_stat;

  *((ADIobj *) rid) = adix_getref( (ADIobj) *id, status );

  _ERR_REP( "ADI_GETREF", Estr__GetRefObj );
  }

F77_SUBROUTINE(adifn(newref))( INTEGER(rid), INTEGER(id), INTEGER(status) )
  {
  GENPTR_INTEGER(rid)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;

  *((ADIobj *) id) = adix_newref( (ADIobj) *rid, status );

  _ERR_REP( "ADI_NEWREF", Estr__CreRef );
  }

F77_SUBROUTINE(adifn(putref))( INTEGER(id), INTEGER(rid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(rid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_putref( (ADIobj) *id, (ADIobj) *rid, status );

  _ERR_REP( "ADI_PUTREF", Estr__PutRefObj );
  }


/* -------------------------------------------------------------------------
 * Data system routines
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(fclose))( INTEGER(id), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;				/* Check initialised and ok */

/* Invoke kernel routine to close file */
  ADIfsysFileClose( (ADIobj) *id, status );

  _ERR_REP( "ADI_FCLOSE", Estr__CloFilObj );
  }

F77_SUBROUTINE(adifn(fcreat))( CHARACTER(fspec), INTEGER(id),
			       INTEGER(fid), INTEGER(status)
			       TRAIL(fspec) )
  {
  GENPTR_CHARACTER(fspec)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(fid)
  GENPTR_INTEGER(status)

  _chk_stat;				/* Check initialised and ok */

  adix_fcreat( fspec, fspec_length, (ADIobj) *id, (ADIobj *) fid, status );

  _ERR_REP( "ADI_FCREAT", Estr__CreFilObj );
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

  _chk_stat;				/* Check initialised and ok */

/* Invoke kernel routine */
  adix_fopen( fspec, fspec_length, cls, cls_length,
	      mode, mode_length, (ADIobj *) id, status );

  _ERR_REP( "ADI_FOPEN", Estr__OpeFilObj );
  }

/* -------------------------------------------------------------------------
 * Miscellaneous
 * -------------------------------------------------------------------------
 */

F77_SUBROUTINE(adifn(flush))( CHARACTER(grp), INTEGER(status) TRAIL(grp) )
  {
  GENPTR_CHARACTER(grp)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_id_flush( grp, grp_length, status );

  _ERR_REP( "ADI_FLUSH", Estr__FshObjGrp );
  }

F77_SUBROUTINE(adifn(link))( INTEGER(id), CHARACTER(grp), INTEGER(status)
			     TRAIL(grp) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(grp)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_id_link( (ADIobj) *id, grp, grp_length, status );

  _ERR_REP( "ADI_LINK", Estr__LnkObjGrp );
  }


/* ----------------- unincorporated routines --------------- */


F77_SUBROUTINE(adifn(cmnstr))( CHARACTER(str), INTEGER(id),
			    INTEGER(status) TRAIL(str) )
  {
  GENPTR_CHARACTER(str)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;

  *id = adix_cmn( str, str_length, status );

  _ERR_REP( "ADI_CMNSTR", Estr__DefCst );
  }

F77_SUBROUTINE(adifn(defrcb))( INTEGER(rid), CHARACTER(name),
			    ADICB rtn, INTEGER(status) TRAIL(name) )
  {
  GENPTR_INTEGER(rid)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

  _chk_stat;                 /* Standard checks */

  adix_defrcb( *rid, name, name_length,
	       ADIkrnlNewEproc( ADI__false, (ADICB) rtn, status ),
	       status );
  }

F77_SUBROUTINE(adifn(getfile))( INTEGER(id), INTEGER(fid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(fid)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_getfile( *id, (ADIobj *) fid, status );
  }

F77_SUBROUTINE(adifn(getlink))( INTEGER(id), INTEGER(lid), INTEGER(status) )
  {
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(lid)
  GENPTR_INTEGER(status)

  _chk_stat;

  *lid = adix_getlink( *id, status );
  }

F77_SUBROUTINE(adifn(getpath))( INTEGER(id), CHARACTER(path), INTEGER(lpath),
                             INTEGER(status) TRAIL(path) )
  {
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(path)
  GENPTR_INTEGER(lpath)
  GENPTR_INTEGER(status)

  _chk_stat;

  adix_getpath( (ADIobj) *id, ADI__false, path_length, path, lpath, status );
  }

F77_SUBROUTINE(adifn(locrcb))( INTEGER(rid), CHARACTER(name),
			       INTEGER(rtn), INTEGER(status) TRAIL(name) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(rid)
  GENPTR_INTEGER(rtn)
  GENPTR_INTEGER(status)

  _chk_stat;                 /* Standard checks */

  adix_locrcb( *rid, name, name_length, (ADIobj *) rtn, status );
  }

F77_SUBROUTINE(adifn(locrep))( CHARACTER(name),
			    INTEGER(id), INTEGER(status) TRAIL(name) )
  {
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

  _chk_stat;                 		/* Standard checks */

  adix_locrep( name, name_length, (ADIobj *) id, status );
  }

F77_SUBROUTINE(adifn(probe))( INTEGER(status) )
  {
  GENPTR_INTEGER(status)

  adix_probe( status );
  }

/* End of ADI_F77 defined test
 */
#endif
