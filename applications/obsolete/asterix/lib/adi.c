/*
 *    Description :
 *
 *     The ADI data system provides class oriented data storage to client
 *     software.
 *
 *    Method :
 *
 *     ADI communicates with clients using object tokens which are usually
 *     declared as integers in the client language. A token indexes a table
 *     of object handles. These store,
 *
 *       a) The object class id
 *       b) The location of the object's instance data
 *       c) A reference count
 *       d) Dependency information
 *       e) Various flags
 *
 *     Because the token used to reference an object provides two levels of
 *     indirection, ADI can reallocate the object data space with affecting
 *     client software.
 *
 *
 *    Implementation notes :
 *
 *     - ADI Object Structure
 *
 *     - ADI Object Allocation
 *
 *       Object allocation is performed using basic blocks. A BB is a
 *       block of memory with a header, which stores the data required
 *       for a number of objects of the same type. BB construction is
 *       controlled by a BB allocator, one of which exists for each
 *       type.
 *
 *     - Memory Transfers
 *
 *       Controlled using the MTA object. An MTA describes a conceptual
 *       block of data, which need not be contiguous in process memory.
 *       The MTA has 2 sets of dimensional information. The 'ddims' field
 *       describes the process memory space which bounds the subject data.
 *       The 'udims' field describes the data used within this volume,
 *       with 'uorig' describing the N-dimensional origin of this sub-
 *       volume. The MTA data structure contains other information, the
 *       base type and size in bytes of each data element.
 *
 *       A complication arises when referring to character strings. The
 *       natural mode for the C programmer to supply an array of character
 *       strings is as an array of char *. In Fortran however, and for C
 *       when strings are the output form, a 2-d array of characters is
 *       more natural. Thus, the convention is that when an MTA has an
 *       element size of _CSM then the data item refers to char **,
 *       otherwise simply char *. The second complication is that ADI
 *       arrays of strings are stored in neither of these forms, but as
 *       an array of ADIstring's.
 *
 *       ** slices **
 *
 *     - Arrays
 *
 *     - Method combination forms
 *
 *     - Generic Functions
 *
 *    To be done :
 *
 *     Streaming :
 *
 *	 Stream arrays, structures
 *	 Handle static objects
 *
 *     MTA handling :
 *
 *	 Extend MTA system to provide externally usable facility. Ability to
 *	 create MTAs describing user data items, ability to copy between two
 *	 MTAs, slicing to create a new MTA.
 *
 *     User visibility of slots :
 *
 *       user routine to access argument numbers given slot number
 *
 *
 *     does adix_name work on slices/cells,putid'd objects?
 *     encapsulate global variables in a ADIinterpreter object
 *     remove direct references to stderr in err.lib
 *     argument default values
 *     where to trap Print(stream,obj) method creation by
 *     erase should *always* clear in case future _putid
 *     use printf convertors for conv -> C?
 *     free list created in method lookup
 *     grouping of identifiers
 *     string slicing
 *     string mapping
 *     string allocation using byte type?
 *     separate DOS/UNIX/Windows stream initialisation & error system
 *     buffer locdat info to speed up _there+_find pairs
 *     locrcb should report errors?
 *     Formatting styles, eg. Print(stream,FortranStyle|C|LaTex|Input,object)
 *     file extensions a-list, how to handle multiple extensions?
 *     member name overloading
 *     parser grammar should be loadable
 *     dodgy putting kernel objects as data members - can't erase
 *     support C and Fortran array indexing
 *     exit handlers
 *     vector ops, eg. cput0i( id, "a,b,c", 23, status )
 *     Memory leaks
 *     Public hash table interface
 *     Data packing and stream representation of ADI data
 *     Iterators and locking
 *     Special non-recursive list destructor
 *     Lock access when object is mapped
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>

#include "asterix.h"
#include "ems.h"

#include "aditypes.h"
#include "adiconv.h"
#include "adikrnl.h"
#include "adimem.h"
#include "adifuncs.h"
#include "adilist.h"
#include "adistrng.h"
#include "adiparse.h"
#include "adicface.h"
#include "aditable.h"
#include "adiexpr.h"
#include "adifsys.h"
#include "adisyms.h"
#include "adierror.h"
#include "adiarray.h"
#include "adipkg.h"

/*  Forward definitions
 *
 */
ADIobj adix_delgen( int narg, ADIobj args[], ADIstatus status );
ADIobj adix_delhan( int narg, ADIobj args[], ADIstatus status );
ADIobj adix_delmapctrl( int narg, ADIobj args[], ADIstatus status );
ADIobj adix_delmco( int narg, ADIobj args[], ADIstatus status );
ADIobj adix_delobj( int narg, ADIobj args[], ADIstatus status );
ADIobj adix_stdmcf( ADIobj, int, ADIobj [], ADIstatus status );
ADIobj adix_locgen( ADIobj name, int narg, ADIstatus status );

/* Declare kernel data types
 *
 */
_DEF_STATIC_CDEF("_MappingControl",mapctrl,8,ADImapCtrl);
_DEF_STATIC_CDEF("_Method",mthd,48,ADImethod);
_DEF_STATIC_CDEF("_MethodCombinationForm",mco,8,ADImethComb);
_DEF_STATIC_CDEF("_GenericFunction",gnrc,24,ADIgeneric);
_DEF_STATIC_CDEF("_ExternalProcedure",eprc,48,ADIeproc);
_DEF_STATIC_CDEF("_SuperclassRecord",pdef,48,ADIsuperclassDef);
_DEF_STATIC_CDEF("_MemberRecord",mdef,48,ADImemberDef);
_DEF_STATIC_CDEF("_MemoryTransfer",mta,8,ADImta);
_DEF_STATIC_CDEF("_ClassDeclaration",cdef,16,ADIclassDef);
_DEF_STATIC_CDEF("_ObjectHandle",han,512,ADIobjHan);


ADIobj       UT_cid_b;
ADIobj       UT_cid_ub;
ADIobj       UT_cid_w;
ADIobj       UT_cid_uw;
ADIobj       UT_cid_i;
ADIobj       UT_cid_r;
ADIobj       UT_cid_d;
ADIobj       UT_cid_l;
ADIobj       UT_cid_c;
ADIobj       UT_cid_p;
ADIobj       UT_cid_struc;
ADIobj       UT_cid_ref;
ADIobj	     UT_cid_strm;


ADIobj          ADI_G_grplist = ADI__nullid;

ADIlogical      ADI_G_init = ADI__false;
ADIlogical      ADI_G_init_failed = ADI__false;

ADIobj       ADI_G_commonstrings = ADI__nullid;


/* We maintain a linked list of class definition structures. ADI_G_firstcdef
 * holds the address of the first class definition structure. Subsequent
 * links are stored in the class structures.
 */
static ADIclassDef    *ADI_G_firstcdef = NULL;
static ADIclassDef    **ADI_G_cdeflink = &ADI_G_firstcdef;

/* The base interpreter */
ADIinterp	ADI_G_baseinterp = _INIT_INTERP;
ADIinterp	*ADI_G_curint = &ADI_G_baseinterp;


/*
 * Standard frequently used names
 */
ADIobj DnameAfter       = ADI__nullid;
ADIobj DnameAround      = ADI__nullid;
ADIobj DnameBefore      = ADI__nullid;
ADIobj DnamePrimary     = ADI__nullid;
ADIobj DnameNewLink     = ADI__nullid;
ADIobj DnameSetLink     = ADI__nullid;
ADIobj DnameUnLink      = ADI__nullid;

ADIobj DsysGlobalBase   = ADI__nullid;

ADIobj ADIcvFalse       = ADI__nullid;
ADIobj ADIcvTrue        = ADI__nullid;
ADIobj ADIcvNulCons     = ADI__nullid;

static ADIobj   ADI_G_stdmcf = ADI__nullid;

char *adix_idd( ADIobj id )
  {
  ADIblock	*b;
  ADIidIndex	ib = _ID_IBLK(id);

  b = ADI_G_blks[ib];

  return b->data + _ID_SLOT(id)*b->size;
  }

char *adix_iddt( ADIobj id, ADIclassDef **cdef )
  {
  ADIblock	*b;
  ADIidIndex	ib = _ID_IBLK(id);

  b = ADI_G_blks[ib];

  *cdef = b->cdef;

  return b->data + _ID_SLOT(id)*b->size;
  }


/*
 * Adjust name length variable to ignore trailing white space
 */
void adix_ntrunc( char *name, int *len )
  {
  char *nptr = name + (*len) - 1;

  while( (*len) && isspace(*nptr) ) {
    (*len)--; nptr--;
    }
  }

char *adix_accname( ADIacmode mode )
  {
  char          *aname;

  switch( mode ) {
    case ADI__read:
      aname = "READ";
      break;
    case ADI__write:
      aname = "WRITE";
      break;
    case ADI__update:
      aname = "UPDATE";
      break;
    }

  return aname;
  }


/*
 *  Allocate a block of objects.
 *  Unless action is taken by the caller invalid
 *  data can be created if (for example) the data object is primitive and
 *  contains pointers. If however the type is primitive, and a data
 *  initialiser has been supplied, then that will be copied.
 */
ADIobj adix_cls_nallocd( ADIclassDef *cdef, int ndim, int dims[],
			 void *idata, ADIstatus status )
  {
  ADIobj                data;           /* Raw data block id */
  ADIobjHan		*hdata = NULL;
  int                   nelm = 1;       /* Total number of elements needed */
  int         		i,j;
  char        		*dat;
  char        		*rdata;
  char        		*pdata;
  ADIobj                rval = ADI__nullid;

  _chk_stat_ret(ADI__nullid);           /* Check status on entry */

/* Zero size? Must be abstract */
  if ( ! cdef->alloc.size )
    adic_setecs( ADI__ILLOP,
      "Pure abstract class /%s/ cannot be instantiated", status, cdef->name );

  else {

/* Array of items? */
    if ( ndim ) {

/* Count number of elements */
      nelm = ADIaryCountNelm( ndim, dims );

/* Allocate the data segment */
      data = ADImemAllocObj( cdef, nelm, status );

/* Create array descriptor and wrap it in a handle */
      rval = ADIaryNew( ndim, dims, data, ADI__nullid, status );
      }

/* Allocate single element */
    else {
      rval = data = ADImemAllocObj( cdef, 1, status );
      }

/* Get data address unless kernel object with no data */
    if ( ! (cdef->kernel && ! idata && ! cdef->pdata) )
      rdata = adix_idd( data );

/* Wrap in handle if non-kernel */
    if ( ! cdef->kernel ) {
      rval = ADIkrnlNewHan( rval, ADI__false, &hdata, status );
      hdata->data = rdata;
      }
    }

/* Allocation went ok? */
  if ( _ok(status) ) {

/* Primitive and user supplied data */
    if ( cdef->prim && idata ) {
      dat = rdata;
      pdata = (char *) idata;
      for( j=cdef->alloc.size; j; j-- )
	*dat++ = *pdata++;

      if ( ! cdef->kernel )
	hdata->dataset = ADI__true;
      }

/* Primitive and initialiser there? Default initialisation does not *set* the */
/* set bit of handled objects */
    else if ( cdef->prim && cdef->pdata ) {
      char        *dat = rdata;

      for( i=nelm; i; i-- ) {
	char *pdata = cdef->pdata;

	for( j=cdef->alloc.size; j; j-- )
	  *dat++ = *pdata++;
	}
      }

/* Class instance? */
    else if ( ! (cdef->kernel || cdef->prim ) ) {

      ADIobj	*iptr;
      ADIobj    *optr = (ADIobj *) rdata;

/* User data supplied? */
      if ( idata ) {
	for( i=nelm; i; i-- ) {
	  iptr = (ADIobj *) idata;

	  for( j=cdef->nslot; j; j-- )
	    *optr++ = *iptr++;
	  }
	}

/* Member initialisations to be performed? */
      else if ( cdef->meminit ) {
	for( i=nelm; i; i-- ) {
	  ADIobj  curmem = cdef->members;

	  for( j=cdef->nslot; j; j--, optr++ ) {
	    ADImemberDef  *mptr = _mdef_data(curmem);

	    if ( _valid_q(mptr->cdata) )
	      *optr = adix_copy( mptr->cdata, status );
	    else if ( _valid_q(mptr->defcls) )
	      *optr = adix_cls_nallocd( _cdef_data(mptr->defcls), 0, 0, NULL, status );
	    else
	      *optr = ADI__nullid;

	    curmem = mptr->next;
	    }
	  }
	}

      else if ( cdef->nslot )
	for( i=cdef->nslot*nelm; i; i-- )
	  *optr++ = ADI__nullid;

/* Class instance is always set */
      hdata->dataset = ADI__true;
      }
    }

  return rval;                          /* Return new object */
  }


ADIobj adix_cls_nalloc( ADIclassDef *cdef, int ndim, int dims[], ADIstatus status )
  {
  return adix_cls_nallocd( cdef, ndim, dims, NULL, status );
  }


ADIobj adix_cls_alloc( ADIclassDef *cdef, ADIstatus status )
  {
  return adix_cls_nallocd( cdef, 0, 0, NULL, status );
  }


void adix_merase( ADIobj *id, ADIinteger nval, ADIstatus status )
  {
  ADIobj		lid = *id;
  ADIinteger		ival = nval;
  ADIclassDef           *tdef;          /* Class definition data */

  _chk_init_err; _chk_stat;

/* Valid handle? */
  if ( _valid_q(lid) ) {

/* Locate class definition block */
    tdef = _ID_TYPE(lid);

/* Destructor defined? */
    if ( _valid_q(tdef->destruc) ) {
      for( ; ival-- ; ) {
	adix_exemth( ADI__nullid, tdef->destruc, 1, id, status );
	lid = ADImemIdAddOff( lid, 1, status );
	}
      }

    else if ( ! tdef->prim ) {          /* Class instance? */
      ADIobj    *optr;
      int       i;

      for( ; ival-- ; ) {
	optr = _class_data(lid);
	for( i=0; i<tdef->nslot; i++, optr++ )
	  if ( _valid_q(*optr) ) {
	    if ( _han_q(*optr) )
              _han_name(*optr) = ADI__nullid;
	    adix_merase( optr, 1, status );
	    }
	lid = ADImemIdAddOff( lid, 1, status );
	}
      }

    if ( *status == ADI__NOTDEL )       /* Didn't delete data? */
      *status = SAI__OK;
    else
      ADImemFreeObj( id, nval, status ); /* Free objects */
    }
  }

void adix_erase( ADIobj *id, ADIstatus status )
  {
  adix_merase( id, 1, status );
  }


ADIobj adix_delobj( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj  *optr = _obj_data(args[0]);

  if ( _valid_q(*optr) )
    adix_erase( optr, status );

  return ADI__nullid;
  }


ADIobj adix_delmapctrl( int narg, ADIobj args[], ADIstatus status )
  {
  ADImapCtrl  *mptr = _mapctrl_data(args[0]);

  if ( mptr->dynamic )
    ADImemFree( mptr->dptr, mptr->nbyte, status );

  return ADI__nullid;
  }


ADIobj adix_delhan( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobjHan  *hptr = _han_data(args[0]);

/* Decrement reference count */
  hptr->ref--;

/* Outstanding references? */
  if ( hptr->ref )
    *status = ADI__NOTDEL;
  else {
    if ( hptr->slice ) {              /* Is this a slice? */
      if ( _ary_q(hptr->id) )         /* Vector slice? */
	adix_erase( &hptr->id,     /* Delete array block ignoring data */
		      status );

      adix_refadj( hptr->pid, -1, status );

/* If component has data set, parent inherits that */
      if ( hptr->dataset )
	_han_set(hptr->pid) = ADI__true;
      }

/* Not sliced data */
    else {

/* Check that object has been unhooked from parent. If not, its most likely an */
/* illegal delete where the user has got an identifier to an object component  */
/* and is trying to delete it more times than he is allowed. The correct way to */
/* do this is handled by adix_cerase which resets the name field appropriately */
      if ( _valid_q(hptr->name) ) {
	adic_setecs( ADI__INTGTY, "Attempt to delete object component (name %S) using primitive erase", status, hptr->name  );
	}
      else {
	adix_erase( &hptr->id, status );
	}
      }

/* Property list defined? Properties must be deleted by hand as the value objects */
/* have name links. This is quicker than the recursive list destructor anyway */
    if ( _valid_q(hptr->pl) ) {
      ADIobj	*car,*cdr,*dpair,val,oldcur;
      ADIobj	curp = hptr->pl;

      while ( _valid_q(curp) ) {
	_GET_CARCDR_A(dpair,cdr,curp);
	val = _CDR(*dpair);
	if ( _valid_q(val) )
	  if ( _han_q(val) )
	    _han_name(val) = ADI__nullid;

/* Delete dotted pair */
	adix_merase( dpair, 1, status );
	*dpair = ADI__nullid;

	oldcur = curp;
	curp = *cdr;
	*cdr = ADI__nullid;
	adix_merase( &oldcur, 1, status );
	}
      }
    }

  return ADI__nullid;
  }


ADIobj ADIkrnlNewHan( ADIobj id, ADIlogical slice, ADIobjHan **ehdata,
		      ADIstatus status )
  {
  ADIobjHan	*hdata;                  /* Pointer to handle data */
  ADIobj 	newh = ADI__nullid;

/* Allocate new handle */
  newh = adix_cls_nallocd( &KT_DEFN_han, 0, 0, NULL, status );

  if ( _ok(status) ) {                  /* Allocated ok? */
    hdata = _han_data(newh);            /* Locate data block */

    hdata->id = id;                     /* Store object reference */

    hdata->pl = ADI__nullid;            /* No properties */
    hdata->pid = ADI__nullid;           /* No parent defined */
    hdata->name = ADI__nullid;          /* No name defined */
    hdata->lock = ADI__nullid;          /* No object locks */
    hdata->ref = 1;                     /* Initialise the reference count */
    hdata->data = NULL;

    hdata->markdel = ADI__false;        /* Not marked for delete */
    hdata->readonly = ADI__false;       /* Dynamic object by default */
    hdata->dataset = ADI__false;        /* Data not set yet */
    hdata->slice = slice;               /* Object is a slice? */
    hdata->recur = 0;

    *ehdata = hdata;			/* Export handle data address */
    }

  return newh;                          /* Return new handle */
  }


char *adix_dtdat( ADIobj id, ADIstatus status )
  {
  ADIclassDef	*cdef;
  char		*data;
  ADIobjHan	*hptr;
  ADIarray	*aptr;

  data = adix_iddt( id, &cdef );

  if ( cdef == &KT_DEFN_han ) {
    hptr = (ADIobjHan *) data;

    if ( ! hptr->data )
      hptr->data = adix_dtdat(hptr->id,status);

    return hptr->data;
    }
  else if ( cdef == &KT_DEFN_ary ) {
    aptr = (ADIarray *) data;

    return _ID_DATA(aptr->data);
    }
  else
    return data;
  }


ADIclassDef *adix_dtdef( ADIobj id, ADIstatus status )
  {
  ADIarray           	*ad;
  ADIobjHan             *hd;
  ADIclassDef        	*allc;
  ADIclassDef		*cdef;
  char			*data;

  data = adix_iddt( id, &cdef );

  if ( cdef == &KT_DEFN_han ) {
    hd = (ADIobjHan *) data;
    if ( hd->ref ) {
      cdef = allc = _ID_TYPE(hd->id);
      if ( cdef == &KT_DEFN_ary )
        data = adix_idd( hd->id );
      }
    else {
      adic_setecs( ADI__FATAL, "Attempt to access dead object - programmer error", status );
      cdef = NULL;
      }
    }

  if ( cdef == &KT_DEFN_ary ) {
    ad = (ADIarray *) data;
/*    allc = _DTDEF(ad->data); */
    allc = _ID_TYPE(ad->data);
    }
  else
    allc = cdef;

  return allc;
  }


ADIobj ADIkrnlNewClassDef( char *name, int nlen, ADIstatus status )
  {
  ADIclassDef           *tdef;          /* New definition */
  ADIobj                typid;          /* Object identifier */

  if ( !_ok(status) )                   /* Check status on entry */
    return ADI__nullid;

  _GET_NAME(name,nlen);                 /* Import string */

/* New class definition structure */
  typid = adix_cls_nallocd( &KT_DEFN_cdef, 0, 0, NULL, status );

/* Allocate memory for definition */
  tdef = _cdef_data(typid);

/* Duplicate name */
  tdef->name = strx_dupl( name, nlen );

/* Insert name in common table? */
  if ( _valid_q(ADI_G_commonstrings) )
    tdef->aname = adix_cmnC( tdef->name, status );

/* No destructor or printer by default */
  tdef->destruc = ADI__nullid;
  tdef->prnt = ADI__nullid;
  tdef->istrm = ADI__nullid;
  tdef->ostrm = ADI__nullid;

  tdef->link = NULL;                    /* Add to linked list */
  *ADI_G_cdeflink = tdef;
  ADI_G_cdeflink = &tdef->link;

  tdef->nslot = 0;                      /* Starting values */
  tdef->members = ADI__nullid;
  tdef->superclasses = ADI__nullid;
  tdef->sdslist = ADI__nullid;
  tdef->defmem = DEF_MEMBER_FLAG_VALUE;

  tdef->kernel = ADI__false;
  tdef->meminit = ADI__false;
  tdef->adider = ADI__false;

  tdef->bexpr[0] = ADI__nullid;
  tdef->bexpr[1] = ADI__nullid;
  tdef->bexpr[2] = ADI__nullid;

  tdef->cnvs = ADI__nullid;
  tdef->pdata = NULL;

  return typid;
  }


/* Create a new external procedure object
 */
ADIobj ADIkrnlNewEproc( ADIlogical is_c, ADICB func, ADIstatus status )
  {
  ADIobj        newid = ADI__nullid;    /* The new object */
  ADIeproc	proc;

  _chk_init; _chk_stat_ret(ADI__nullid);

  if ( func ) {                         /* Function is defined? */

/* Fill in data slots */
    proc.prc = func;
    proc.c = is_c;

/* New external procedure object */
    newid = adix_cls_nallocd( &KT_DEFN_eprc, 0, 0, &proc, status );
    }

  return newid;                         /* Return the new object */
  }


/*
 * Define a primitive class
 *
 */
void adix_def_pclass( char *name, size_t size, ADIobj *tid,
		      ADIstatus status )
  {
  ADIclassDef          *tdef;           /* New class definition */

  if ( !_ok(status) )                   /* Check status on entry */
    return;

/* Allocate new class storage */
  *tid = ADIkrnlNewClassDef( name, _CSM, status );
  tdef = _cdef_data(*tid);

/* Initialise basic block control */
  ADImemInitBlock( &tdef->alloc, size, ADI__EXHANTAB, *tid, status );

  tdef->prim = ADI__true;
  }


void adix_def_pclass_data( ADIclassDef *tdef, char *data, ADIstatus status )
  {
  _chk_stat;
  tdef->pdata = data;
  }


void ADIdefClassMakeDlist( ADIclassDef *tdef, ADIstatus status )
  {
  ADIobj        curp,curpp,sdselem;
  ADIobj	dslist = ADI__nullid;
  ADIobj        *ipoint = &dslist;
  ADIclassDef	*pcdef;
  ADIsuperclassDef	*pdef;

  _chk_stat;

/* Superclasses present? */
  if ( _valid_q(tdef->superclasses) ) {

/* Put class name at head of list */
    lstx_inscel( tdef->aname, &ipoint, status );

/* Each each direct superclass name in turn */
    curp = tdef->superclasses;
    while ( _valid_q(curp) ) {
      pdef = _pdef_data(curp);
      pcdef = _cdef_data(pdef->clsid);

      lstx_inscel( pdef->name, &ipoint, status );

      if ( _valid_q(pcdef->sdslist) ) {
	curpp = pcdef->sdslist;
	while ( _valid_q(curpp) ) {
	  _GET_CARCDR( sdselem, curpp, curpp );
	  lstx_addtoset( &tdef->sdslist, sdselem, status );
	  }
	}

      curp = _pdef_next(curp);
      }

    lstx_addtoset( &tdef->sdslist, dslist, status );
    }
  }


void ADIdefClassConvertNames( ADIclassDef *tdef, ADIstatus status )
  {
  ADIobj        cmem;
  ADImemberDef	*mdef;

  _chk_stat;

  for( cmem = tdef->members; _valid_q(cmem); ) {
    mdef = _mdef_data(cmem);
    mdef->aname = adix_cmn( mdef->name, mdef->nlen, status );
    cmem = mdef->next;
    }
  }


/* A bunch of functions which help to create the class precedence list */

ADIobj adix_cons_pairs_aux( ADIobj lst, ADIstatus status )
  {
  ADIobj	car,cdr;
  ADIobj	cadr,cddr;
  ADIobj        rval;

  _chk_stat_ret(ADI__nullid);

  _GET_CARCDR(car,cdr,lst);
  _GET_CARCDR(cadr,cddr,cdr);

  if ( _null_q(cddr) ) {
    rval = lstx_cell( lst, ADI__nullid, status );
    }
  else {
    rval = lstx_cell( lstx_new2( car, cadr, status ),
		      adix_cons_pairs_aux( cdr, status ),
		      status );
    }

  return rval;
  }


ADIobj adix_cons_pairs( ADIobj lst, ADIstatus status )
  {
  _chk_stat_ret(ADI__nullid);

  return adix_mapcar1( adix_cons_pairs_aux, lstx_append, lst, status );
  }


ADIlogical adix_filt_classes_mtest( ADIobj x, ADIobj y, ADIstatus status )
  {
  return (x == _CADR(y)) ? ADI__true : ADI__false;
  }


/*
 * Construct a list of those classes which do *not* appear as the second
 * element in any of the precedence pairs
 */
ADIobj adix_filt_classes( ADIobj classes, ADIobj ppairs, ADIstatus status )
  {
  ADIobj        cls,cdr;
  ADIobj        curp = classes;
  ADIobj        rval = ADI__nullid;

  _chk_stat_ret(ADI__nullid);

  while ( _valid_q(curp) && _ok(status) ) {

    _GET_CARCDR(cls,cdr,curp);

    if ( ! adix_member( cls, ppairs, adix_filt_classes_mtest, status ) )
      lstx_push( cls, &rval, status );

/* Next test class */
    curp = cdr;
    }

  return rval;
  }


ADIobj adix_filt_cands( ADIobj candidates, ADIobj plist,
		      ADIobj dsupers, ADIstatus status )
  {
  ADIobj	carcand,cdrcand;

  ADIobj rval = ADI__nullid;

  _chk_stat_ret(ADI__nullid);

  _GET_CARCDR(carcand,cdrcand,candidates);

/* List is of length one? */
  if ( _null_q(cdrcand) )
    rval = carcand;

  else {
    ADIobj cursub = plist;              /* Loop over possible subclasses */
    ADIobj sub;

    while ( _valid_q(cursub) && _null_q(rval)) {

      ADIobj	nxtsub;
      ADIobj 	curcan = candidates;       /* Loop over candidates */

      _GET_CARCDR(sub,nxtsub,cursub);

      while ( _valid_q(curcan) ) {

	ADIobj	can,nxtcan;

	_GET_CARCDR(can,nxtcan,curcan);

	if ( adix_member( can,
			  adix_assoc( sub, dsupers, status ),
			  NULL, status ) ) {
	  rval = can;
	  break;
	  }

	curcan = nxtcan;
	}

      cursub = nxtsub;
      }
    }

  return rval;
  }


ADIlogical adix_filt_pairs_test( ADIobj x, ADIobj args, ADIstatus status )
  {
  return (args == _CAR(x)) ? ADI__true : ADI__false;
  }


ADIobj adix_filt_pairs( ADIobj ppairs, ADIobj winner, ADIstatus status )
  {
  _chk_stat_ret(ADI__nullid);

  return adix_removeif( adix_filt_pairs_test, winner, ppairs, status );
  }


/*
 * Establish the precedence order of a set of classes. The classes to
 * be ordered form the first argument. The second is the list of all
 * direct superclass relationships, ie. for each class a list of that
 * classes direct superclasses.
 */
ADIobj adix_estab_ord( ADIobj classes, ADIobj dsupers, ADIstatus status )
  {
  ADIobj        ppairs;
  ADIobj        preclst = ADI__nullid;  /* Precedence list */
  ADIobj        curcls = classes;       /* Current class list */

  _chk_stat_ret(ADI__nullid);

/* Make precedence pairs */
  ppairs = adix_cons_pairs( dsupers, status );

/* While more classes to process */
  while ( _valid_q(curcls) && _ok(status) ) {
    ADIobj      cands;
    ADIobj      winner;

/* Next lot of candidates */
    cands = adix_filt_classes( curcls, ppairs, status );

    if ( _null_q(cands) ) {

/* If only one class left then it the winner */
      if ( _null_q(_CDR(curcls)) )
        winner = _CAR(curcls);

      else {
        adic_setecs( ADI__INVARG,
   "Unable to order method list %O with respect to direct superclass set %O",
            status, classes, dsupers );
        return ADI__nullid;
        }
      }
    else {
      winner = adix_filt_cands( cands, preclst, dsupers, status );

      ppairs = adix_filt_pairs( ppairs, winner, status );

      }

    curcls = adix_removeif( NULL, winner, curcls, status );

    lstx_push( winner, &preclst, status );
    }

  return lstx_revrsi( preclst, status );/* Return the precedence list */
  }


ADIobj adix_delgen( int narg, ADIobj args[], ADIstatus status )
  {
  ADIgeneric	*gen = _gnrc_data(args[0]);

  adix_erase( &gen->name, status );
  adix_erase( &gen->args, status );
  adix_erase( &gen->cdisp, status );
  adix_erase( &gen->fdisp, status );

  return ADI__nullid;
  }

void adix_defgdp( ADIobj genid, ADIobj disp, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  if ( _eprc_c(disp) )
    _gnrc_cdisp(genid) = disp;
  else
    _gnrc_fdisp(genid) = disp;
  }

ADIobj adix_defgen_i( ADIobj name, int narg, ADIobj args, ADIobj mcomb,
		      ADIstatus status )
  {
  ADIgeneric	*gdata;
  ADIobj        newid;                  /* The new object */

  _chk_stat_ret(ADI__nullid);

  newid = adix_cls_nallocd( &KT_DEFN_gnrc, 0, 0, NULL, status );

  if ( _ok(status) ) {                  /* Set generic record fields */

/* Locate data block */
    gdata = _gnrc_data(newid);

    gdata->name = name;
    gdata->narg = narg;
    gdata->args = args;
    gdata->mcomb = mcomb;
    gdata->cdisp = ADI__nullid;
    gdata->fdisp = ADI__nullid;
    gdata->mlist = ADI__nullid;

    ADIsymAddBind( name, -1,
		   ADIsbindNew( ADI__generic_sb, newid, status ),
		   status );
    }

  return newid;
  }


/*
 * Parse function/method/generic definition. Simple expression parse, then take head
 * and arguments from expression node
 */
void adix_prs_namarg( ADIobj stream, char *thing, ADIobj *head, int *narg,
		      ADIobj *args, ADIstatus status )
  {
  ADIobj	*hadr,*aadr;
  ADIobj	aspec;

/* Check status on entry */
  _chk_stat;

/* Parse expression. Extract name and argument list and discard top-level */
/* expression node */
  aspec = ADIparseExpInt( stream, ADI__nullid, 1, status );
  if ( _ok(status) ) {
    if ( _etn_q(aspec) ) {

/* Locate head and argument insertion points */
      _GET_HEDARG_A(hadr,aadr,aspec);

/* Set return variables */
      *head = *hadr;
      *args = *aadr;
      if ( narg )
	*narg = lstx_len( *args, status );

/* Clear expression node fields so that our return data is not deleted */
      *hadr = ADI__nullid;
      *aadr = ADI__nullid;

/* Destroy expression node */
      adix_erase( &aspec, status );
      }
    else {
      adix_erase( &aspec, status );
      adic_setecs( ADI__SYNTAX, "Error parsing %s definition", status, thing );
      }
    }
  }


void adix_defgen( char *spec, int slen, char *options, int olen,
		  ADIobj rtn, ADIobj *id, ADIstatus status )
  {
  ADIobj        args = ADI__nullid;     /* Generic argument list */
  ADIobj        gname;                  /* New generic name */
  ADIobj        mcomb = ADI_G_stdmcf;   /* Method combination form */
  int           narg;               	/* Number of generic arguments */
  ADIobj        newid;                  /* The new generic function */
  ADIobj        pstream;                /* Parse stream */

  _chk_init; _chk_stat;                 /* Check status on entry */

/* Import strings used in this rtn */
  _GET_NAME(options,olen);

/* Put specification into parse stream */
  pstream = ADIstrmExtendC( ADIstrmNew( "r", status ), spec, slen, status );
  ADInextToken( pstream, status );

/* Parse definition and get head and arguments */
  adix_prs_namarg( pstream, "generic", &gname, &narg, &args, status );

/* Free the parsing stream */
  adic_erase( &pstream, status );

  if ( _ok(status) ) {                  /* Parsing went ok? */

/* Allocate space for new generic */
    newid = adix_defgen_i( gname, narg, args, mcomb, status );

    if ( _valid_q(rtn) )                /* Routine supplied? */
      adix_defgdp( newid, rtn, status );/* Install it */

    if ( id )                           /* User wants the identifier? */
      *id = newid;                      /* Return identifier */
    }
  }


ADIobj ADIkrnlNewMth( ADImethod *mdata, ADIstatus status )
  {
  return adix_cls_nallocd( &KT_DEFN_mthd, 0, 0, (void *) mdata, status );
  }


void adix_defcpa( char *name, int nlen,
		  ADIobj rtn, ADIobj *id, ADIstatus status )
  {
  ADIobj        cname;                   /* New command name */
  ADImethod	mth;
  ADIobj        newid;                  /* The new function */

  _chk_init; _chk_stat;                 /* Check status on entry */

/* Locate name in command string table */
  cname = adix_cmn( name, nlen, status );

/* Allocate the new method and fill in the fields */
  mth.args = ADI__nullid;
  mth.form = ADI__nullid;
  mth.exec = rtn;
  newid = ADIkrnlNewMth( &mth, status );

/* Create function symbol binding and add to symbol table */
  ADIsymAddBind( cname, -1,
		 ADIsbindNew( ADI__command_sb, newid, status ),
		 status );

/* Return identifier if user wants it */
  if ( id )
    *id = cname;
  }


void adix_deffun( char *spec, int slen,
		  ADIobj rtn, ADIobj *id, ADIstatus status )
  {
  ADIobj        fargs;     		/* Argument list */
  ADIobj        fname;                  /* New function name */
  ADImethod	mth;
  ADIobj        newid;                  /* The new function */
  ADIobj        pstream;                /* Parse stream */

  _chk_init; _chk_stat;                 /* Check status on entry */

/* Put specification into parse stream */
  pstream = ADIstrmExtendC( ADIstrmNew( "r", status ), spec, slen, status );
  ADInextToken( pstream, status );

/* Parse definition and get head and arguments */
  adix_prs_namarg( pstream, "function", &fname, NULL, &fargs, status );

/* Release the parse stream */
  adic_erase( &pstream, status );

/* Parsing went ok? */
  if ( _ok(status) ) {

/* Allocate the new method and fill in the fields */
    mth.args = fargs;
    mth.form = ADI__nullid;
    mth.exec = rtn;

    newid = ADIkrnlNewMth( &mth, status );
    if ( _ok(status) ) {

/* Create function symbol binding and add to symbol table in global scope */
      ADIsymAddBind( fname, -1,
		     ADIsbindNew( ADI__fun_sb, newid, status ),
		     status );
      }

    if ( id )                           /* User wants identifier? */
      *id = fname;                      /* Return identifier */
    }
  }


void adix_defmth( char *spec, int slen,
		  ADIobj rtn, ADIobj *id, ADIstatus status )
  {
  ADIobj        args = ADI__nullid;     /* Generic argument list */
  ADItokenType	ctok;			/* First token in parse stream */
  ADIobj        gname;                  /* New generic name */
  ADIobj        mform = DnamePrimary; 	/* Method form */
  ADImethod	mthd;			/* New method data */
  int           narg;               	/* Number of generic arguments */
  ADIobj        newid;                  /* The new generic function */
  ADIobj        pstream;                /* Parse stream */

/* Check status on entry */
  _chk_init; _chk_stat;

/* Put specification into parse stream */
  pstream = ADIstrmExtendC( ADIstrmNew( "r", status ), spec, slen, status );
  ctok = ADInextToken( pstream, status );

/* Check for prefix character denoting non-primary method forms */
  if ( ctok == TOK__PLUS )
    mform = DnameAfter;
  else if ( ctok == TOK__MINUS )
    mform = DnameBefore;
  else if ( ctok == TOK__AT )
    mform = DnameAround;
  if ( mform != DnamePrimary )
    ctok = ADInextToken( pstream, status );

/* Parse definition and get head and arguments */
  adix_prs_namarg( pstream, "method", &gname, &narg, &args, status );

/* Release the parse stream */
  adic_erase( &pstream, status );

  if ( _ok(status) ) {                  /* Parsing went ok? */
    ADIobj      gnid;                   /* Generic function identifier */

/* Look for generic function */
    gnid = adix_locgen( gname, narg, status );

/* Allocate space for new generic with null argument names and */
/* standard method combination if generic is not already defined */
    if ( _null_q(gnid) )
      gnid = adix_defgen_i( gname, narg, ADI__nullid, ADI_G_stdmcf, status );

/* Allocate the new method and fill in the fields */
    mthd.args = args;
    mthd.form = mform;
    mthd.exec = rtn;

    newid = ADIkrnlNewMth( &mthd, status );
    if ( _ok(status) ) {
      ADIobj		*glist = &_gnrc_mlist(gnid);

/* Add method to generic's list */
      *glist = lstx_cell( newid, *glist, status );
      }

    if ( id )                           /* User wants identifier? */
      *id = newid;                      /* Return identifier */
    }
  }


ADIobj ADIdefClassLocMember( ADIobj memlist, char *name, int nlen,
			     ADIstatus status )
  {
  ADIobj        curm = memlist;
  ADIobj        found = ADI__false;

  while ( _valid_q(curm) && !found ) {
    ADImemberDef     *mdata = _mdef_data(curm);

    if ( ! strx_cmp2c( mdata->name, mdata->nlen, name, nlen ) )
      found = ADI__true;
    else
      curm = mdata->next;
    }

  return found && _ok(status) ? curm : ADI__nullid;
  }


ADIobj ADIdefClassCopyMember( ADIobj pmem,
			      ADIobj **ipoint, ADIstatus status )
  {
  ADIobj        newid;                  /* The new member definition */
  ADImemberDef  *pdata = _mdef_data(pmem);

  _chk_stat_ret(ADI__nullid);           /* Check status on entry */

/* Allocate storage for new member */
  newid = adix_cls_nallocd( &KT_DEFN_mdef, 0, 0, NULL, status );

/* Store name and length in characters */
  _mdef_name(newid) = strx_dupl( pdata->name, pdata->nlen );
  _mdef_nlen(newid) = pdata->nlen;
  _mdef_aname(newid) = pdata->aname;
  adix_refadj( pdata->aname, 1, status );
  _mdef_defcls(newid) = pdata->defcls;

/* Inherit any constant data */
  _mdef_cdata(newid) = pdata->cdata;
  if ( _valid_q(pdata->cdata) )
    adix_refadj( pdata->cdata, 1, status );

  _mdef_next(newid) = ADI__nullid;

/* Insert new object into list and update insertion point */
  **ipoint = newid;
  *ipoint = &_mdef_next(newid);

/* Return new object */
  return newid;
  }


ADIobj ADIdefClassNewMember( ADIobj pstr, ADIobj *members,
			     ADIobj **ipoint, ADIstatus status )
  {
  ADIobj        emem = ADI__nullid;     /* Existing member definition */
  ADIobj        memclsid = ADI__nullid; /* Member class definition block */
  ADIobj        memname = ADI__nullid;  /* Name of new member */
  ADIobj        memtype = ADI__nullid;  /* Name of new member type */
  char          *name;
  int           nlen;
  ADIobj        newid;                  /* The new member definition */

  _chk_stat_ret(ADI__nullid);           /* Check status on entry */

/* Class member initialisation can only happen once the types required */
/* to represent the common string table are in place */
  if ( _valid_q(ADI_G_commonstrings ) ) {

    ADIstring   *sptr;

/* Locate the string on the parse stream in the common string table */
    memname = prsx_symname( pstr, status );

/* Get the next token. If it too is TOK__SYM then the first symbol is */
/* interpreted as a class name, and the second name is the member name */
    if ( ADInextToken( pstr, status ) == TOK__SYM ) {
      memtype = memname;

/* Locate the member class definition */
      memclsid = ADIkrnlFindClsI( memtype, status );

/* Validate class name */
    if ( _null_q(memclsid) )
      adic_setecs( ADI__INVARG,
	     "Unknown class name /%S/ in member specification",
	     status, memtype );

/* Parse the member name */
      memname = prsx_symname( pstr, status );

/* Advance to token after member name */
      ADInextToken( pstr, status );
      }

/*   Locate data for name string. Check not already present */
    sptr = _str_data(memname);
    name = sptr->data;
    nlen = sptr->len;
    }

  else
    ADIstrmGetTokenData( pstr, &name, &nlen, status );

/* Look for member name in existing list */
  emem = ADIdefClassLocMember( *members, name, nlen, status );

/* The member name does not already exist */
  if ( _null_q(emem) ) {

/* Allocate storage for new member */
    newid = adix_cls_nallocd( &KT_DEFN_mdef, 0, 0, NULL, status );

/* Store name and length in characters */
    _mdef_name(newid) = strx_dupl( name, nlen );
    _mdef_nlen(newid) = nlen;
    _mdef_aname(newid) = memname;
    _mdef_defcls(newid) = memclsid;
    _mdef_cdata(newid) = ADI__nullid;
    _mdef_next(newid) = ADI__nullid;

/* Insert into list */
    **ipoint = newid;
    *ipoint = &_mdef_next(newid);
    }

/* Member name already exists. We allow the user to override the
/* initialisation type of the new member, but the new type but be the
/* same type or derived from the existing member type */
  else {
    if ( memclsid != _mdef_defcls(emem) &&
	_valid_q(memclsid) && _valid_q(_mdef_defcls(emem)) ) {
      ADIclassDef     *c1 = _cdef_data(memclsid);
      ADIclassDef     *c2 = _cdef_data(_mdef_defcls(emem));

      if ( ! adix_chkder( c1, c2, status ) )
	adic_setecs( ADI__INVARG,
"The initialisation class of member %*s must be derived from %s", status,
		nlen, name, c2->name );
      }

/* Overrides are ok? */
    if ( _ok(status) ) {
      _mdef_defcls(emem) = memclsid;
      newid = emem;
      }
    }

/* Advance to token after member name */
  if ( _null_q(ADI_G_commonstrings) )
    ADInextToken( pstr, status );

  return newid;
  }


void ADIdefClassNewMemberData( ADIobj pstr, ADIobj member,
			       ADIstatus status )
  {
  ADImemberDef  *memb = _mdef_data(member);

  _chk_stat;                    /* Check status on entry */

/* If data already exists, the scrub it */
  if ( _valid_q(memb->cdata) )
    adic_erase( &memb->cdata, status );

/* Skip over the assignment token */
  ADInextToken( pstr, status );

/* Read a constant value from the stream */
  memb->cdata = prsx_cvalue( pstr, status );
  }


void ADIparseClassMembers( ADIobj pstream,
			   ADIobj *members, ADIstatus status )
  {
  ADIlogical    defmem = ADI__false;
  ADIobj        *mnext = members;
  ADIlogical    more = ADI__true;

  _chk_init;                    /* Check status on entry */

/* Locate member list insertion point */
  while ( _valid_q(*mnext) )
    mnext = &_mdef_next(*mnext);

/* While more class members to parse */
  while ( more && _ok(status) ) {

    ADIobj      newm;           /* The new member */

/* Add member to list */
    newm = ADIdefClassNewMember( pstream, members, &mnext, status );

/* This is the default member? */
    if ( ADIcurrentToken(pstream,status) == TOK__MUL ) {
      if ( ! defmem ) {
	defmem = ADI__true;
	_mdef_nlen(newm) = - _mdef_nlen(newm);
	ADInextToken( pstream, status );
	}
      else
	adic_setecs( ADI__INVARG, "Default member already defined", status );
      }

/* Definition supplies initial data? */
    if ( ADIcurrentToken(pstream,status) == TOK__ASSIGN )
      ADIdefClassNewMemberData( pstream, newm, status );

/* Comma delimits member names, otherwise at end of list. Shouldn't need */
/* TOK__END soak as parsing either from constant string or from EOL free */
/* stream?? */
    if ( ADIifMatchToken( pstream, TOK__COMMA, status ) ) {
      while ( ADIcurrentToken( pstream, status ) == TOK__END )
	ADInextToken( pstream, status );
      }
    else
      more = ADI__false;
    }
  }


ADIobj ADIkrnlAllocSuper( ADIobj clsid, ADIstatus status )
  {
  ADIsuperclassDef	sup;

  sup.clsid = clsid;
  sup.name = _cdef_data(clsid)->aname;
  sup.next = ADI__nullid;

/* Allocate storage for new superclass */
  return adix_cls_nallocd( &KT_DEFN_pdef, 0, 0, &sup, status );
  }


void ADIparseClassSupers( ADIobj pstream, ADIobj *supers,
			  ADIobj *members, ADIstatus status )
  {
  ADItokenType  ctok;
  ADIobj        curp;                   /* Loop over superclasses */
  ADIobj        *mnext = members;       /* Member list insertion point */
  ADIlogical    more = ADI__true;       /* More classes in list? */
  ADIobj        sname;                  /* Superclass name string */
  ADIobj        *snext = supers;        /* Superclass list insertion point */
  ADIobj        stid;                   /* Superclass definition object */

  _chk_init;                    /* Check status on entry */

/* Trap empty stream */
  more = (ADIcurrentToken(pstream,status) == TOK__SYM);

/* While more superclass names to parse */
  while ( more && _ok(status) ) {

/* Locate superclass name in common table */
    sname = prsx_symname( pstream, status );

/* Locate superclass by name */
    stid = ADIkrnlFindClsI( sname, status );

/* Error if not found */
    if ( _null_q(stid) )
      adic_setecs( ADI__INVARG,
	  "Unknown class name /%S/ in superclass specification",
	  status, sname );
    else {

/* Allocate storage for new superclass */
      *snext = ADIkrnlAllocSuper( stid, status );

/* Get next token */
      ctok = ADInextToken( pstream, status );

/* Comma delimits superclass names, otherwise end */
      if ( ctok == TOK__COMMA ) {
	ctok = ADInextToken( pstream, status );
	while ( ctok == TOK__END )
	  ctok = ADInextToken( pstream, status );
	if ( ctok != TOK__SYM )
	  adic_setecs( ADI__INVARG,
		       "Syntax error in superclass specification", status );
	}
      else
	more = ADI__false;

/* Update insertion point */
      if ( _ok(status) )
	snext = &_pdef_next(*snext);
      }

/* End of loop over superclass names */
    }

/* Loop over superclasses, copying members to new class */
  curp = *supers;
  while ( _ok(status) && _valid_q(curp) ) {
    ADIobj  pmem;
    ADIclassDef *ptdef = _cdef_data(_pdef_clsid(curp));

/* Loop over all slots of the superclass */
    for( pmem = ptdef->members; _valid_q(pmem); pmem = _mdef_next(pmem) )
      (void) ADIdefClassCopyMember( pmem, &mnext, status );

/* Next superclass in list */
    curp = _pdef_next(curp);
    }
  }


ADIobj ADIdefClass_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical            anyinit = ADI__false;
  ADIobj                cid;            /* The new definition structure */
  ADIobj                curm;           /* Loop over members */
  ADIobj                curo;           /* Loop over options */
  ADIobj		dpair;		/* Options name.value pair */
  ADIinteger		ioval;		/* Integer option value */
  ADIobj                members = args[2];
  ADIobj                name = args[0];
  ADIinteger		nunit = ADI__EXHANTAB;
  ADIobj		onam;		/* Option name */
  ADIobj		options = args[3];
  ADIobj		oval;		/* Option value */
  size_t                size;
  ADIobj		*spoint;	/* Superclass record insertion point */
  ADIobj                supers = args[1];
  ADIclassDef        	*tdef;          /* New class definition */

  _chk_stat_ret(ADI__nullid);

/* Allocate new class definition record */
  cid = ADIkrnlNewClassDef( _str_dat(name), _str_len(name), status );
  tdef = _cdef_data(cid);

/* Mark as a structured data type */
  tdef->prim = ADI__false;

/* Store superclass and member lists */
  tdef->members = members;
  tdef->superclasses = supers;

/* Add the global base class to the end of the superclass list */
  if ( _valid_q(DsysGlobalBase) ) {
    spoint = &tdef->superclasses;
    while ( _valid_q(*spoint) )
      spoint = &_pdef_next(*spoint);
    *spoint = ADIkrnlAllocSuper( DsysGlobalBase, status );
    }

/* Class has superclasses? */
  if ( _valid_q(tdef->superclasses) ) {

/* Derived from ADIbase? */
    if ( _valid_q(DsysADIbase) )
      tdef->adider = adix_chkder( tdef, _cdef_data(DsysADIbase), status );

/* Inherit allocation cluster size from superclass if present */
    nunit = _cdef_data(_pdef_clsid(tdef->superclasses))->alloc.nunit;
    }

/* Count members, and determine whether any members have class initialisation */
  curm = members;
  tdef->nslot = 0;
  while ( _valid_q(curm) ) {
    ADImemberDef        *memb = _mdef_data(curm);

    tdef->nslot++;
    if ( memb->nlen < 0 ) {
      tdef->defmem = tdef->nslot;
      memb->nlen = - memb->nlen;
      }

    if ( _valid_q(memb->defcls) || _valid_q(memb->cdata) )
      anyinit = ADI__true;

    curm = memb->next;
    }

/* Store flag describing method initialisation */
  tdef->meminit = anyinit;

/* Number of bytes required per instance of the new class */
  size = tdef->nslot * sizeof(ADIobj);

/* Initialise basic block control for this class */
  ADImemInitBlock( &tdef->alloc, size, nunit, cid, status );

/* Construct direct super class list if table built. Also done manually */
  if ( _valid_q(ADI_G_commonstrings) )
    ADIdefClassMakeDlist( tdef, status );

/* Process options if present */
  if ( _valid_q(options) ) {

/* Get number of options */
    curo = *_struc_data(options);

/* Loop over options */
     while ( _valid_q(curo) && _ok(status) ) {

/* Locate dotted pair and next cell */
      _GET_CARCDR(dpair,curo,curo);

/* Get name and value from dotted pair */
      _GET_CARCDR(onam,oval,dpair);

/* Compare against allowed options */
      if ( ! strx_cmpc( "ClusterSize", 11, onam ) ) {

/* Define cluster size for class */
	adic_get0i( oval, &ioval, status );
	ADIdefClassCluster( cid, ioval, status );

	}
      else {
	adic_setecs( ADI__SYNTAX, "Illegal option name /%S/", status, onam );
	}
      }
    }

/* Set function return value */
  return cid;
  }


void ADIdefClassCluster( ADIobj clsid, ADIinteger number,
			 ADIstatus status )
  {
  _chk_init_err; _chk_init;

  if ( _cdef_q(clsid) ) {
    if ( number > 0 ) {
      _cdef_data(clsid)->alloc.nunit = number;
      }
    else
      adic_setecs( ADI__INVARG,
		       "Invalid allocation cluster size", status );
    }
  else
    adic_setecs( ADI__INVARG,
		       "Syntax error in superclass specification", status );
  }


void ADIdefClass_e( char *name, int nlen, char *parents, int plen,
		    char *members, int mlen, ADIobj *tid, ADIstatus status )
  {
  ADIobj                args[4] = {ADI__nullid,ADI__nullid,ADI__nullid,ADI__nullid};
					/* Arguments for internal routine */
					/* These are name, supers, members */
  ADIobj                cid;            /* New class identifier */
  ADIobj             pstream = ADI__nullid;        /* Parse stream */

/* Ensure ADI is initialised */
  _chk_init; _chk_stat;

/* Get class name string */
  _GET_NAME(name,nlen);

/* Create ADI string for name */
  adic_newv0c_n( name, nlen, args, status );

/* Import strings used in this rtn */
  _GET_STRING(parents,plen);
  _GET_STRING(members,mlen);

/* Parse parent specification if the string isn't empty. */
  if ( parents && (plen>0) ) {

/* Put parents specification into parse stream */
    pstream = ADIstrmExtendC( ADIstrmNew( "r", status ), parents, plen, status );

    if ( ADInextToken( pstream, status ) != TOK__END )
      ADIparseClassSupers( pstream, args+1, args+2, status );
    }

/* Parse member specification if the string isn't empty */
  if ( members && (mlen>0) ) {
    if ( _null_q(pstream) )
      pstream = ADIstrmNew( "r", status );
    else
      ADIclearStream( pstream, status );

/* Put members specification into parse stream */
    ADIstrmExtendC( pstream, members, mlen, status );

    if ( ADInextToken( pstream, status ) != TOK__END )
      ADIparseClassMembers( pstream, args+2, status );
    }

/* Erase the parse stream if defined */
  if ( _valid_q(pstream) )
    adic_erase( &pstream, status );

/* Make new class definition */
  cid = ADIdefClass_i( 4, args, status );

/* Set return value if required */
  if ( tid )
    *tid = cid;
  }


void adix_delcls( ADIclassDef **cvar, ADIstatus status )
  {
  ADIobj        cur;
  ADIclassDef *tdef = *cvar;

  if ( tdef->link )                     /* Remove most recent first */
    adix_delcls( &tdef->link, status );

  if ( _valid_q(tdef->members) ) {      /* Members list */
    for( cur = tdef->members; _valid_q(cur); cur = _mdef_next(cur) )
      adix_erase( &_mdef_aname(cur), status );
    }

  if ( _valid_q(tdef->superclasses) ) { /* Parents list */
    for( cur = tdef->superclasses; _valid_q(cur); cur = _pdef_next(cur) )
      adix_erase( &_pdef_name(cur), status );
    }

/*  strx_free( tdef->name, status );      The class name */

  *cvar = NULL;
  }


void ADIkrnlDefDestrucInt( ADIclassDef *cdef, ADIobj dmethod,
			   ADIstatus status )
  {
  if ( _ok(status) )
    cdef->destruc = dmethod;
  }

void ADIkrnlDefDestrucKint( ADIclassDef *cdef, ADIcMethodCB cmeth,
			   ADIstatus status )
  {
  ADIkrnlDefDestrucInt( cdef, ADIkrnlNewEproc( ADI__true, (ADICB) cmeth, status ),
			status );
  }

void ADIkrnlDefDestruc( ADIobj clsid, ADIobj dmethod, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Check status on entry */

/* Store destructor pointer */
  ADIkrnlDefDestrucInt( _cdef_data(clsid), dmethod, status );
  }

void ADIkrnlDefPrnt( ADIobj clsid, ADIobj pmethod, ADIstatus status )
  {
  _chk_init_err; _chk_stat;             /* Check status on entry */

/* Store printer pointer */
  _cdef_prnt(clsid) = pmethod;
  }

void ADIkrnlDefOstrmInt( ADIclassDef *cdef, ADIobj osmethod, ADIstatus status )
  {
  if ( _ok(status) )
    cdef->ostrm = osmethod;
  }

void ADIkrnlDefOstrmKint( ADIclassDef *cdef, ADIcOstreamer osmeth, ADIstatus status )
  {
  ADIkrnlDefOstrmInt( cdef, ADIkrnlNewEproc( ADI__true, (ADICB) osmeth, status ),
		      status );
  }

void ADIkrnlDefIstrmInt( ADIclassDef *cdef, ADIobj ismethod, ADIstatus status )
  {
  if ( _ok(status) )
    cdef->istrm = ismethod;
  }

void ADIkrnlDefIstrmKint( ADIclassDef *cdef, ADIcIstreamer ismeth, ADIstatus status )
  {
  ADIkrnlDefIstrmInt( cdef, ADIkrnlNewEproc( ADI__true, (ADICB) ismeth, status ),
		      status );
  }


#define _def_prnt(_suffix,_type,_castetype,_format) \
void ADIprnt##_suffix( int narg, ADIobj args[], ADIstatus status ) { \
  ADIstrmFprintf( args[0], _format, status, (_castetype) *((_type *) _DTDAT(args[1])) ); \
  }

_def_prnt(b,ADIbyte,	int, "%d")
_def_prnt(ub,ADIubyte,	int, "%d")
_def_prnt(w,ADIword,	int, "%d")
_def_prnt(uw,ADIuword,	int, "%d")
_def_prnt(i,ADIinteger,	ADIinteger, "%I")
_def_prnt(r,ADIreal,	ADIreal, "%g")
_def_prnt(d,ADIdouble,	ADIdouble, "%g")
#undef _def_prnt

void ADIprntl( int narg, ADIobj args[], ADIstatus status )
  {
  static char *tstr = "True";
  static char *fstr = "False";

  ADIlogical    val = *((ADIlogical *) _DTDAT(args[1]));

  ADIstrmFprintf( args[0], val ? tstr : fstr, status );
  }

void ADIprntc( int narg, ADIobj args[], ADIstatus status )
  {
  ADIstrmFprintf( args[0], "%S", status, args[1] );
  }

void ADIprntp( int narg, ADIobj args[], ADIstatus status )
  {
  ADIstrmFprintf( args[0], "%p", status, args[1] );
  }

void ADIprntstruc( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj  sid = *_struc_data(args[1]);

  ADIstrmFprintf( args[0], "{", status );

  while ( _valid_q(sid) ) {
    ADIobj	scar, scdr;
    ADIobj	scaar, scdar;

    _GET_CARCDR(scar,scdr,sid);
    _GET_CARCDR(scaar,scdar,scar);

    ADIstrmFprintf( args[0], "%S = %O%s", status, scaar,
		   scdar, _valid_q(scdr) ? ", " : "" );

    sid = scdr;
    }

  ADIstrmFprintf( args[0], "}", status );
  }

void ADIprntref( int narg, ADIobj args[], ADIstatus status )
  {
  ADIstrmFprintf( args[0], "Reference to %O", status, *_obj_data(args[1]) );
  }


void adix_probe( ADIstatus status )
  {
  ADIclassDef	*cdef = ADI_G_firstcdef;

  _chk_init; _chk_stat;

  ADIstrmPrintf( "%-25s %5s %7s %7s(   %%)  %s\n\n",
		 status, "Type name", "N_blk", "N_alloc", "N_used", "N_bytes" );

  while ( cdef ) {
    ADIblock	*bptr;
    ADIinteger	nalloc = 0;
    ADIinteger	nbytes = 0;
    ADIinteger	nused = 0;
    int		nb = 0;
    ADIidIndex	iblk = cdef->alloc.f_block;

    while ( iblk != ADI__nullid ) {
      nb++;
      bptr = ADI_G_blks[iblk];
      nalloc += bptr->nunit;
      nbytes += bptr->nunit * bptr->size + sizeof(ADIblock);
      nused += bptr->nunit - bptr->nfree;

      iblk = bptr->next;
      }

    if ( nb ) {
      ADIstrmPrintf( "%-25s %5d %7I %7I(%3d%%) %8I\n", status,
		   cdef->name, nb, nalloc, nused,
		   (int) (100.0*((float) nused)/((float) nalloc)),
		   nbytes );
      }

    cdef = cdef->link;
    }

  ADIstrmFlush( status );
  }



void adix_exit()
  {
  static                                /* List of stuff to be deleted */
    ADIobj *dlist[] = {
    &ADI_G_replist,                     /* Representation list */
    &DnameAfter, &DnameAround,          /* Method forms */
    &DnameBefore, &DnamePrimary,
    &DnameSetLink, &DnameUnLink,        /* Data model operations */
    &ADI_G_baseinterp.StdIn, &ADI_G_baseinterp.StdOut, &ADI_G_baseinterp.StdErr,
    NULL
    };

  ADIobj        **dobj = dlist;
  ADIstatype    status = SAI__OK;

/* While more to delete */
  while ( *dobj ) {
    if ( _valid_q(**dobj) )             /* Referenced object is defined? */
      adix_erase( *dobj, &status );

    dobj++;                             /* Next element of dlist */
    }

/* Scrub the type definitions list */
  adix_delcls( &ADI_G_firstcdef, &status );

/* Remove the common string table */
/*  adix_erase( &ADI_G_commonstrings, 1, &status ); */

/* Remove the dynamically allocated data */
  ADImemStop( &status );

  ADI_G_init = ADI__false;              /* Mark as uninitialised */
  }

ADIobj adix_config( char *attr, ADIobj newval, ADIstatus status )
  {
  ADIobj	oldval = ADI__nullid;
  ADIobj	*ovalad = NULL;

  _chk_stat_ret(ADI__nullid);

  if ( ! strcmp( attr, "StdOut" ) )
    ovalad = &ADI_G_curint->StdOut;

  else if ( ! strcmp( attr, "StdErr" ) )
    ovalad = &ADI_G_curint->StdErr;

  if ( ovalad ) {
    oldval = *ovalad;
    *ovalad = newval;
    }

  return oldval;
  }

void ADIkrnlAddPtypes( ADIptypeTableEntry ptable[], ADIstatus status )
  {
  ADIobj		alloc;
  ADIptypeTableEntry	*pt = ptable;
  ADIclassDef		*tdef;

/* Loop over records in table */
  for( ; pt->name; pt++ ) {

/* Install the type */
    adix_def_pclass( pt->name, pt->size, &alloc, status );

/* Store allocator address */
    if ( pt->alloc ) *(pt->alloc) = alloc;

/* Locate its data */
    tdef = _cdef_data(alloc);

/* Printer defined? If so, create descriptor and store */
    if ( pt->prnt )
      ADIkrnlDefPrnt( alloc,
		      ADIkrnlNewEproc(ADI__true,(ADICB) pt->prnt,status),
		      status );

/* Input streamer defined? If so, create descriptor and store */
    if ( pt->istrm )
      ADIkrnlDefIstrmKint( tdef, (ADIcIstreamer) pt->istrm, status );

/* Output streamer defined? If so, create descriptor and store */
    if ( pt->ostrm )
      ADIkrnlDefOstrmKint( tdef, (ADIcOstreamer) pt->ostrm, status );
    }
  }


ADIinteger ADIostrmHan( ADIstream *stream, ADIobj id, char *data, int wmode,
			ADIstatus status )
  {
  ADIinteger	len = 0;
  ADIobjHan	*hdata = (ADIobjHan *) data;

  len += adix_ostrm_i( stream, hdata->id, wmode, status );

  return len;
  }

ADIobj ADIistrmHan( ADIstream *stream, ADIstatus status )
  {
  ADIobj	hobj = ADI__nullid;

  hobj = adix_istrm_i( stream, status );

  return hobj;
  }

/*
 * Output streaming functions for simple types
 */
#define _GEN_OSTRM(_type,_T) \
ADIinteger ADIostrm##_T( ADIstream *stream, ADIobj id, char *data, int wmode, \
			 ADIstatus status ) { \
  if ( wmode ) \
    _OS_STRM_write(stream,sizeof(_type),data);\
  return sizeof(_type);\
  }

_GEN_OSTRM(ADIbyte,B)
_GEN_OSTRM(ADIubyte,UB)
_GEN_OSTRM(ADIword,W)
_GEN_OSTRM(ADIuword,UW)
_GEN_OSTRM(ADIinteger,I)
_GEN_OSTRM(ADIreal,R)
_GEN_OSTRM(ADIdouble,D)
_GEN_OSTRM(ADIlogical,L)

ADIinteger ADIostrmC( ADIstream *stream, ADIobj id, char *data, int wmode,
		      ADIstatus status )
  {
  ADIinteger	slen;
  ADIstring	*sptr = (ADIstring *) data;

/* Writing data? */
  if ( wmode ) {

/* Stream the length */
    slen = sptr->len;
    _OS_STRM_i( stream, &slen );

/* Stream bytes if any */
    if ( sptr->len )
      _OS_STRM_write(stream,sptr->len,sptr->data);
    }

  return sizeof(ADIinteger) + sptr->len;
  }


/*
 * Input streaming functions for simple types
 */
#define _GEN_ISTRM(_type,_T) \
  ADIobj ADIistrm##_T( ADIstream *stream, ADIstatus status ) { \
  _type	val; \
  _IS_STRM_read(stream,sizeof(_type),&val); \
  return ADImk##_T( val, status ); \
  }

_GEN_ISTRM(ADIbyte,B)
_GEN_ISTRM(ADIubyte,UB)
_GEN_ISTRM(ADIword,W)
_GEN_ISTRM(ADIuword,UW)
_GEN_ISTRM(ADIinteger,I)
_GEN_ISTRM(ADIreal,R)
_GEN_ISTRM(ADIdouble,D)
_GEN_ISTRM(ADIlogical,L)

ADIobj ADIistrmC( ADIstream *stream, ADIstatus status )
  {
  ADIinteger	len;
  ADIobj	robj;
  ADIstring	str;

/* Read string length */
  _IS_STRM_i( stream, &len );

/* Allocate string manually if length is non-zero */
  if ( len ) {
    str.len = len;
    str.data = strx_alloc( len, status );

    robj = adix_cls_nallocd( _cdef_data(UT_cid_c), 0, 0, &str, status );

    _IS_STRM_read( stream, len, str.data );
    }
  else
    adic_new0c( &robj, status );

  return robj;
  }

static
  int recurse_check = 0;

void adi_init( ADIstatus status )
  {
  DEFINE_PTYPE_TABLE(ptable)
    PTYPE_TENTRY("BYTE",   ADIbyte,    &UT_cid_b, ADIprntb, ADIistrmB, ADIostrmB ),
    PTYPE_TENTRY("UBYTE",  ADIubyte,   &UT_cid_ub,ADIprntub,ADIistrmUB,ADIostrmUB),
    PTYPE_TENTRY("WORD",   ADIword,    &UT_cid_w, ADIprntw, ADIistrmW, ADIostrmW ),
    PTYPE_TENTRY("UWORD",  ADIuword,   &UT_cid_uw,ADIprntuw,ADIistrmUW,ADIostrmUW),
    PTYPE_TENTRY("INTEGER",ADIinteger, &UT_cid_i, ADIprnti, ADIistrmI, ADIostrmI ),
    PTYPE_TENTRY("REAL",   ADIreal,    &UT_cid_r, ADIprntr, ADIistrmR, ADIostrmR ),
    PTYPE_TENTRY("DOUBLE", ADIdouble,  &UT_cid_d, ADIprntd, ADIistrmD, ADIostrmD ),
    PTYPE_TENTRY("LOGICAL",ADIlogical, &UT_cid_l, ADIprntl, ADIistrmL, ADIostrmL ),
    PTYPE_TENTRY("CHAR",   ADIstring,  &UT_cid_c, ADIprntc, ADIistrmC, ADIostrmC ),
    PTYPE_TENTRY("POINTER",ADIpointer, &UT_cid_p, ADIprntp, NULL,NULL),
    PTYPE_TENTRY("STRUC",  ADIobj,     &UT_cid_struc, ADIprntstruc,NULL,NULL),
    PTYPE_TENTRY("OBJREF", ADIobj,     &UT_cid_ref, ADIprntref,NULL,NULL),
    PTYPE_TENTRY("Stream", ADIstream,  &UT_cid_strm, NULL, NULL, NULL),
  END_PTYPE_TABLE;

  static
    ADIobj      obj_defd = ADI__nullid;
  static
    ADIobj      struc_defd = ADI__nullid;
  static
    ADIstring   c_defd = {NULL,0};
  static
    ADIpointer  p_defd = NULL;

  if ( !_ok(status) )                   /* Check status on entry */
    return;

  recurse_check++;

/* Prevent recursive operation */
  if ( recurse_check > 1 ) {
    adic_setecs( ADI__FATAL, "Illegal recursion, probable programming error or corruption", status );
    }

/* If not already initialised and not already tried */
  else if ( ! ADI_G_init && ! ADI_G_init_failed ) {

/* Initialise allocation system */
    ADImemStart();

/* Mark as initialised before any objects are created */
    ADI_G_init = ADI__true;

/* Install built-in primitive types */
    ADIkrnlAddPtypes( ptable, status );

/* Sensible allocation size for streams (they're rather big) */
    adic_defcac( UT_cid_strm, 16, status );

/* Install primitive data initialisation for strings and structures */
    adix_def_pclass_data( _cdef_data(UT_cid_ref), (char *) &obj_defd, status );
    adix_def_pclass_data( _cdef_data(UT_cid_c), (char *) &c_defd, status );
    adix_def_pclass_data( _cdef_data(UT_cid_p), (char *) &p_defd, status );
    adix_def_pclass_data( _cdef_data(UT_cid_struc), (char *) &struc_defd, status );

/* Mark as initialised - this is the earliest point at which non-kernel */
/* objects should be created */
    if ( _ok(status) ) {

/* Install things which can't be declared by kernel static definitions */
/* Currently, this is the kernel object destructors and kernel streaming */
/* functions */
      ADIkrnlDefDestrucKint( &KT_DEFN_mapctrl, adix_delmapctrl, status );
      ADIkrnlDefDestrucKint( &KT_DEFN_mco,     adix_delmco, status );
      ADIkrnlDefDestrucKint( &KT_DEFN_gnrc,    adix_delgen, status );
      ADIkrnlDefDestrucKint( &KT_DEFN_han,     adix_delhan, status );

/* Handle object streamers */
      ADIkrnlDefIstrmKint( &KT_DEFN_han, ADIistrmHan, status );
      ADIkrnlDefOstrmKint( &KT_DEFN_han, ADIostrmHan, status );

/* Destructor for reference type */
      ADIkrnlDefDestrucKint( _cdef_data(UT_cid_ref), adix_delobj, status );
      ADIkrnlDefDestrucKint( _cdef_data(UT_cid_strm), ADIstrmDel, status );

/*  Establish the ADI exit handler. Frees all dynamic memory */
#ifdef use_on_exit
      on_exit( adix_exit, NULL );
#else
      atexit( adix_exit );
#endif
      }
    else
      ADI_G_init_failed = ADI__true;

/* Load various sub-systems */
    ADIaryInit( status );
    ADIcnvInit( status );		/* Define convertor functions */
    strx_init( status );
    lstx_init( status );
    tblx_init( status );

/* Create common string table to hold property names, class member names
   and any other common strings */
    ADI_G_commonstrings = tblx_new( 301, status );

/* Install member names of List and HashTable classes in the newly created
   common string table */
    if ( _ok(status) ) {
      ADIclassDef    *tdef = ADI_G_firstcdef;

/* Add class names to table */
      while ( tdef ) {
	tdef->aname = adix_cmnC( tdef->name, status );
	tdef = tdef->link;
	}

      ADIdefClassConvertNames( _cdef_data(UT_cid_list), status );
      ADIdefClassConvertNames( _cdef_data(UT_cid_tbl), status );
      ADIdefClassMakeDlist( _cdef_data(UT_cid_list), status );
      ADIdefClassMakeDlist( _cdef_data(UT_cid_tbl), status );
      }

/* Install expression system. Create the first frame stack and with it */
/* the first symbol table */
    ADIetnInit( status );
    ADIexprPushFS( 511, ADI__nullid, status );

/* Create constant objects */
    adic_newv0l( ADI__false, &ADIcvFalse, status );
    adic_newv0l( ADI__true, &ADIcvTrue, status );
    ADIcvNulCons = lstx_cell( ADI__nullid, ADI__nullid, status );
    if ( _ok(status) ) {
      _han_readonly(ADIcvTrue) = ADI__true;
      _han_readonly(ADIcvFalse) = ADI__true;
      _han_readonly(ADIcvNulCons) = ADI__true;
      }

/* Initialise sub-systems depending on common string table */
    prsx_init( status );

/* Set the array kernel object ADI name - makes argument testing easier */
    KT_DEFN_ary.aname = K_Array;

/* Create the constant stream object pointing to standard output & error */
    ADI_G_curint->StdIn = ADIstrmExtendFile( ADIstrmNew( "r", status ), ADI__nullid, stdin, status );
    ADI_G_curint->StdOut = ADIstrmExtendFile( ADIstrmNew( "w", status ), ADI__nullid, stdout, status );
    ADI_G_curint->StdErr = ADIstrmExtendFile( ADIstrmNew( "w", status ), ADI__nullid, stderr, status );
    _han_readonly(ADI_G_curint->StdIn) = ADI__true;
    _han_readonly(ADI_G_curint->StdOut) = ADI__true;
    _han_readonly(ADI_G_curint->StdErr) = ADI__true;

/* Install "Standard" method combination */
    adic_defmcf( "Standard", adix_stdmcf, &ADI_G_stdmcf, status );

/* The global base class */
    adic_defcls( "GlobalClass", "", "", &DsysGlobalBase, status );

/* Base class for object linking */
    adic_defcls( "ADIbase", "", "OBJREF ADIlink", &DsysADIbase, status );
    adic_defcac( DsysADIbase, 32, status );

/* Install command language */
    ADIpkgInit( status );

/* Install functions */
    ADIfuncInit( status );

/* Install file system data extensions */
    ADIfsysInit( status );

/* Define variables */
    adic_defvar( "StdErr", ADI__true, ADI_G_curint->StdErr, status );
    adic_defvar( "StdIn", ADI__true, ADI_G_curint->StdIn, status );
    adic_defvar( "StdOut", ADI__true, ADI_G_curint->StdOut, status );
    }

/* Restore recursion checker */
  recurse_check--;

/* Reset error naming */
  _ERR_REP( "adi_init/ADI_INIT", "initialising ADI" );
  }



/*
 *  Locate the allocator block for the named class
 */
ADIclassDef *ADIkrnlFindClsInt( char *cls, int clen, ADIstatus status )
  {
  ADIlogical            found = ADI__false;
  ADIclassDef           *tdef = ADI_G_firstcdef;       /* Cursor */

/* Entry status is ok? */
  if ( _ok(status) ) {

/* Loop over classes comparing class name string with that supplied */
    while ( tdef && ! found ) {
      if ( _valid_q(tdef->aname) ) {
	if ( ! strx_cmpc( cls, clen, tdef->aname ) )
	  found = ADI__true;
        else
	  tdef = tdef->link;
        }
      else if ( ! strncmp( cls, tdef->name, clen ) )
	found = ADI__true;
      else
	tdef = tdef->link;
      }
    }

/* Set function return value */
  return found ? tdef : NULL;
  }

ADIclassDef *ADIkrnlFindClsC( char *cls, int clen, ADIstatus status )
  {
  ADIclassDef   *tdef = NULL;           /* Default return value */

/* Import class name string */
  _GET_NAME(cls,clen);

  if ( (*cls == '*') && (clen==1) )     /* The universal type */
    tdef = _cdef_data(UT_cid_ref);
  else
    tdef = ADIkrnlFindClsInt( cls, clen, status );

  return tdef;
  }

/*
 * Locate class for external language interface. This forbids kernel objects
 */
ADIobj ADIkrnlFindClsExt( char *cls, int clen, ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIclassDef	*tdef = NULL;

  _chk_stat_ret(ADI__nullid);

  _GET_NAME(cls,clen);

  tdef = ADIkrnlFindClsInt( cls, clen, status );

  if ( tdef ) {
    if ( _valid_q(tdef->selfid) )
      rval = tdef->selfid;
    else
      adic_setecs( ADI__INVARG, "Class %*s is a kernel class; no identifier can be returned", status, clen, cls );
    }
  else
    adic_setecs( ADI__INVARG, "No such class %*s is defined", status, clen, cls );

  return rval;
  }


ADIobj ADIkrnlFindClsI( ADIobj name, ADIstatus status )
  {
  ADIclassDef   *tdef;
  ADIstring	*sdat = _str_data(name);

  tdef = ADIkrnlFindClsC( sdat->data, sdat->len, status );

  return tdef ? tdef->selfid : ADI__nullid;
  }


/*
 * Allocate object(s) of user named class
 */
void adix_newn( ADIobj pid, char *name, int nlen, char *cls, int clen,
		int ndim, int dims[], ADIobj *id, ADIstatus status )
  {
  ADIclassDef           *tdef;          /* Class definition */

  _chk_init; _chk_stat;                 /* Standard checks */

/* Locate the class allocator */
  tdef = ADIkrnlFindClsC( cls, clen, status );

/* Invoke creator function with null data values */
  if ( tdef )
    adix_new_n( ADI__true, pid, name, nlen, ndim, dims, NULL,
		&tdef->selfid, 0, id, status );
  }


ADIobj adix_cmn_i( char *str, int len, ADIlogical dstatic, ADIstatus status )
  {
  ADIobj        dpair;                  /* name.value pair in table */
  ADIobj        name;

  _chk_init;

/* Handle C string marker */
  _GET_NAME(str,len);

/* Find or insert in string table */
  dpair = tblx_sadd( &ADI_G_commonstrings, str, len, ADI__nullid, status );

/* Clone the identifier from the name of the dotted pair */
  name = adix_clone( _CAR(dpair), status );

/* Mark string as read-only */
  _han_readonly(name) = ADI__true;

/* Set return value */
  return _ok(status) ? name : ADI__nullid;
  }

/*
 * Common string, length specified, not static data
 */
ADIobj adix_cmn( char *str, int len, ADIstatus status )
  {
  return adix_cmn_i( str, len, ADI__false, status );
  }

/*
 * Common string, nul terminated, not static data
 */
ADIobj adix_cmnC( char *str, ADIstatus status )
  {
  return adix_cmn_i( str, _CSM, ADI__false, status );
  }


/*
 *  Locate insertion point for named item in a property list
 */
void adix_pl_findi( ADIobj pobj, ADIobj *plist, ADIobj property,
		    ADIlogical create, ADIobjRequest *objreq,ADIstatus status )
  {
  ADIobj        hnode;                  /* New element for table hash list */
  ADIobj	*lcar; 			/* &_CAR(*lentry) if found */
  ADIobj        *lentry;                /* List insertion point */
  ADIobj	parent = pobj;		/* Parent id */

  _chk_stat;                            /* Check status */

/* Default return value */
  objreq->data = NULL;

/* Look along list for string. Simply return address if present */
  if ( tblx_scani( plist, property, &lentry, &lcar, status ) )
    objreq->data = &_CDR(*lcar);

/* Create the property if we have permission */
  else if ( create ) {

/* The string.value dotted pair */
    hnode = lstx_cell( adix_clone( property, status ), ADI__nullid, status );

/* The list cell */
    *lentry = lstx_cell( hnode, *lentry, status );

/* The data address is the CDR of the string.value dotted pair */
    objreq->data = &_CDR(hnode);

/* Component creation sets parent data */
    if ( _ok(status) && _valid_q(pobj) )
      _han_set(pobj) = ADI__true;
    }

/* Otherwise do nothing, and zero parent and name id's */
  else
    parent = ADI__nullid;

/* Set parent and name values */
  objreq->parent = parent;
  objreq->name = property;
  objreq->lentry = lentry;
  }


void adix_pl_find( ADIobj pobj, ADIobj *plist, char *property, int plen,
		   ADIlogical create, ADIobjRequest *objreq, ADIstatus status )
  {
  ADIobj	name;			/* Property name */

  _chk_stat;                            /* Check status */

/* Find or insert in common table */
  name = adix_cmn( property, plen, status );

/* Look for property */
  adix_pl_findi( pobj, plist, name, create, objreq, status );

/* Release name string */
  adix_erase( &name, status );
  }


/*
 *  Get property data value if the property exists
 */
ADIobj adix_pl_geti( ADIobj obj, ADIobj prop, ADIstatus status )
  {
  ADIobjRequest	objreq;			/* Object specification */

  _chk_stat_ret(ADI__nullid);           /* Check status */

  adix_pl_findi( obj, &_han_pl(obj), prop, ADI__false, &objreq, status );

  return (objreq.data && _ok(status)) ? *objreq.data : ADI__nullid;
  }

ADIobj adix_pl_fgeti( ADIobj *pobj, ADIobj prop, ADIstatus status )
  {
  ADIobjRequest	objreq;			/* Object specification */

  _chk_stat_ret(ADI__nullid);           /* Check status */

  adix_pl_findi( ADI__nullid, pobj, prop, ADI__false, &objreq, status );

  return (objreq.data && _ok(status)) ? *objreq.data : ADI__nullid;
  }


void adix_pl_seti( ADIobj obj, ADIobj prop, ADIobj valid, ADIstatus status )
  {
  ADIobjRequest	objreq;			/* Object specification */

/* Check status */
  _chk_stat;

  adix_pl_findi( obj, &_han_pl(obj), prop, ADI__true, &objreq, status );

/* Value address is defined? */
  if ( objreq.data ) {
    if ( *objreq.data != valid ) {
      if ( _valid_q(*objreq.data) )
	adix_erase( objreq.data, status );
      *objreq.data = valid;
      }
    }
  }


void adix_nprp( ADIobj id, int *nprp, ADIstatus status )
  {
  ADIobj        plist;                  /* The object property list */
  int           n = 0;                  /* Number of properties */

/* Must be initialised */
  _chk_init_err;

  _chk_han(id); _chk_stat;              /* Has to be a handled object */

  plist = _han_pl(id);                  /* Locate the property list */

  if ( _valid_q(plist) )                /* Check for null list */
    n = lstx_len( _han_pl(id), status );

  if ( _ok(status) )
    *nprp = n;
  }

void adix_indprp( ADIobj id, int index, ADIobj *pid, ADIstatus status )
  {
  ADIobj        plist;                  /* The object property list */
  ADIobj        *pslot;                 /* The property slot */

/* Must be initialised */
  _chk_init_err;

  _chk_han(id); _chk_stat;              /* Has to be a handled object */

  if ( index < 1 )
    adic_setecs( ADI__INVARG, "Property index must be greater than zero", status );

  else {
    plist = _han_pl(id);                /* Locate the property list */

    if ( _valid_q(plist) ) {            /* Not an empty list */
      pslot = lstx_nth( plist, index, status );

      if ( pslot )                      /* Valid list item? */
	*pid = adix_clone( _CDR(*pslot), status );
      else
	adic_setecs( ADI__NOPROP, "Property index is too large", status );
      }
    else
      adic_setecs( ADI__NOPROP, NULL, status );
    }
  }


/*
 * Structures
 */
void adix_ncmp( ADIobj id, int *ncmp, ADIstatus status )
  {
  ADIobj        clist;                  /* The structure component list */
  int           n = 0;                  /* Number of properties */

/* Must be initialised */
  _chk_init_err; _chk_stat;

  if ( ! _struc_q(id) )                 /* Check this is a structure */
    adic_setecs( ADI__ILLOP, "Object is not of type STRUC", status );

  _chk_stat;                            /* Has to be a handled object */

  clist = *_struc_data(id);             /* Locate the component list */

  if ( _valid_q(clist) )                /* Check for null list */
    n = lstx_len( clist, status );

  if ( _ok(status) )
    *ncmp = n;
  }

void adix_indcmp( ADIobj id, int index, ADIobj *cid, ADIstatus status )
  {
  ADIobj        clist;                  /* The structure component list */
  ADIobj        *cslot;                 /* The component slot */

/* Must be initialised */
  _chk_init_err; _chk_stat;

  if ( ! _struc_q(id) )                 /* Check this is a structure */
    adic_setecs( ADI__ILLOP, "Object is not of type STRUC", status );

  _chk_stat;                            /* Has to be a handled object */

  if ( index < 1 )
    adic_setecs( ADI__INVARG, "Component index must be greater than zero", status );

  else {
    clist = *_struc_data(id);           /* Locate the component list */

    if ( _valid_q(clist) ) {            /* Not an empty list */
      cslot = lstx_nth( clist, index, status );

      if ( cslot )                      /* Valid list item? */
	*cid = adix_clone( _CDR(*cslot), status );
      else
	adic_setecs( ADI__NOCOMP, "Component index is too large", status );
      }
    else
      adic_setecs( ADI__NOCOMP, NULL, status );
    }
  }


/* Reference count manipulation
 *
 *  Internal:
 *
 *    adix_refcnt       - retrieve count
 *    adix_refadj       - add offset to reference count
 */
int adix_refcnt( ADIobj id, ADIstatus status )
  {
  int           cnt = 0;

  _chk_init_err; _chk_stat_ret(0);

  if ( _han_q(id) )
    cnt = _han_ref(id);
  else
    adic_setecs( ADI__ILLKOP, NULL, status );

  return cnt;
  }


void adix_refadj( ADIobj id, int offset, ADIstatus status )
  {
  _chk_init_err;

  if ( _valid_q(id) ) {
    char	*data;
    ADIclassDef	*cdef;

    data = adix_iddt( id, &cdef );

    if ( cdef == &KT_DEFN_han )
      ((ADIobjHan *) data)->ref += offset;
    else
      adic_setecs( ADI__ILLKOP, NULL, status );
    }
  }


ADIobj *adix_defmem( ADIobj *id, ADIstatus status )
  {
  ADIobj                *dmem = NULL;
  ADIclassDef           *tdef;

  if ( _ok(status) ) {
    tdef = _DTDEF(*id);
    if ( tdef->prim )
      dmem = id;
    else if ( tdef->defmem == DEF_MEMBER_FLAG_VALUE )
      adic_setecs( ADI__NOMEMB, "No default member defined for class %s",
		status, tdef->name );
    else
      dmem = _class_data(*id) + tdef->defmem;
    }

  return dmem;
  }


void ADIkrnlMtaInit( ADIlogical in, int ndim, int dims[], int vsize,
		     void *value, ADIclassDef *vtdef, ADIlogical clang,
		     ADImta *mta, ADIstatus status )
  {
  int		idim;

  if ( _ok(status) ) {
    mta->ndim = ndim;
    if ( in )
      for( idim=0; idim<ndim; idim++ ) {
	mta->ddims[idim] = mta->udims[idim] = dims[idim];
	mta->uorig[idim] = 1;
	}
    else
      for( idim=0; idim<ndim; idim++ ) {
	mta->ddims[idim] = dims[idim];
	mta->uorig[idim] = 1;
	}
    mta->data = value;
    mta->size = vsize;
    mta->tdef = vtdef;
    mta->clang = clang;
    mta->id = ADI__nullid;
    mta->contig = ADI__true;
    if ( ! in )
      mta->trunc = ADI__false;
    }
  }


/*
 * Create a memory transfer object
 */
ADIobj adix_new_mta( ADIstatus status )
  {
  return adix_cls_nallocd( &KT_DEFN_mta, 0, 0, NULL, status );
  }


void adix_mtacop( ADImta *ind, ADImta *outd, ADIstatus status )
  {
  ADIconvertor	cnv;			/* Conversion procedure */
  ADIlogical    contig = ind->contig;   /* Contiguous tranfer? */
  int           idim;                   /* Loop over dimensions */
  char          *idptr;                 /* Ptr through i/p declared space */
  int           ioffset = 0;            /* Offset from i/p frame to origin */
  int           isec;                   /* Loop over sections to be copied */
  int           isecskip = 1;           /* I/p values to skip per section */
  int           ncdim;                  /* Number of contiguous dimensions */
  int           nerr = 0;               /* Number of conversion errors */
  int           nsec = 1;               /* Number of contiguous areas */
  char          *odptr;                 /* Ptr through o/p declared space */
  int           onval = 1;              /* Values to move per iteration */
  int           ooffset = 0;            /* Offset from o/p frame to origin */
  int           osecskip = 1;           /* O/p values to skip per section */

/* Check inherited status on entry */
  _chk_stat;

/* Output is not truncated by default */
  outd->trunc = ADI__false;

/* Find number of contiguous dimensions if the input is non-contiguous */
  if ( ! contig ) {
    ncdim = 0;
    for( idim=0; idim<ind->ndim && ! ncdim; idim++ )
      if ( ind->udims[idim] != ind->ddims[idim] )
	ncdim = idim;
    }

/* Copying array values? If so, check that output is not truncated */
  if ( ind->ndim ) {
    for( idim=0; idim<ind->ndim; idim++ ) {
      if ( outd->ddims[idim] < ind->udims[idim] ) {
	outd->udims[idim] = outd->ddims[idim];
	outd->trunc = ADI__true;
	}
      else
	outd->udims[idim] = ind->udims[idim];

/* If the output declared data dimension is larger than the input data */
/* dimension to be copied then the output data space is not contiguous, */
/* unless this is the first dimension */
      if ( contig && idim ) {
	if ( ind->udims[idim] != outd->ddims[idim] ) {
	  if ( contig )
	    ncdim = idim;
	  contig = ADI__false;
	  }
	}

/* While contiguous calculate number of elements to be copied per section. */
/* Once non-contiguous, accumlate the number of sections */
      if ( contig ) {
	isecskip *= ind->ddims[idim];
	osecskip *= outd->ddims[idim];
	onval *= outd->udims[idim];
	}
      else
	nsec *= outd->udims[idim];
      }

/* Calculate offsets from start of each declared sub-space to the origin */
/* of the section to be copied */
    ioffset = ADIaryOffset( ind->ndim, ind->ddims, ind->uorig );
    ooffset = ADIaryOffset( outd->ndim, outd->ddims, outd->uorig );
    }

/* Initialise pointers to input and output data. These step through the */
/* declared data spaces, with offsets applied for the origins of the */
/* sections with respect to the declared dimensions */
  idptr = (char *) ind->data + ioffset * ind->size;
  odptr = (char *) outd->data + ooffset * outd->size;

/* Locate the conversion procedure */
  cnv = ADIcnvFind( ind->tdef, outd->tdef, status );

/* Loop over contiguous sections to be copied.  */
  for( isec = nsec; isec--; ) {

/* Convertor defined? If so, invoke it for this segment */
    if ( cnv )
      (*cnv)( ind, onval, idptr, outd, odptr, &nerr, status );

/* No convertor, but types are the same? If so, just move data */
    else if ( ind->tdef == outd->tdef ) {
      _CH_MOVE( odptr, idptr, onval*outd->size );
      }

/* Otherwise tough titty, but only report error once! */
    else if ( _ok(status) ) {
      adic_setecs( ADI__ILLOP,
	"Data conversion not supported from class %s to class %s", status,
		ind->tdef->name, outd->tdef->name );
      nerr += onval;
      }

/* Already reported so simply increment error count */
    else
      nerr += onval;

/* Increment pointers stepping through declared data spaces */
    idptr += isecskip * ind->size;
    odptr += osecskip * outd->size;
    }

  if ( _ok(status) && _valid_q(outd->id) )
    if ( _han_q(outd->id) )
      _han_set(outd->id) = ADI__true;

  if ( nerr )
    adic_setecs( ADI__CONER, "%d data conversion error(s) occurred",
	status, nerr );
  }


void adix_mtaid( ADIobj id, ADImta *mta, ADIstatus status )
  {
  ADIclassDef           *tdef;

  _chk_stat;

  if ( _null_q(id) ) {
    adic_setecs( ADI__INVARG, "No data for data slot", status );
    }
  else if ( _krnl_q(id) && ! _han_q(id) ) {
    adic_setecs( ADI__ILLKOP, "Cannot construct MTA for kernel object", status );
    }
  else
    {
    ADIobj      hid = ADI__nullid;
    ADIlogical	isary = ADI__false;

    if ( _han_q(id) ) {
      hid = _han_id(id);
      isary = _ary_q(hid);
      }

    tdef = _DTDEF(id);                  /* Locate class definition block */

/* Set the transfer type */
    mta->tdef = tdef;

    mta->contig = ADI__true;            /* Some defaults */
    mta->trunc = ADI__false;

    mta->id = id;                       /* Store object being transferred */

/* Array object? */
    if ( isary ) {
      ADIarray       	*adata = _ary_data(hid);
      int               i;
      int               *bdims;
      ADIobj            bdata;

      mta->size = tdef->alloc.size;

      mta->ndim = adata->ndim;
      for( i=0; i<mta->ndim; i++ )      /* The dimensions of this data */
	mta->udims[i] = adata->dims[i];

/* Find origin in the base array, its dimensions and its data address */
      ADIaryBaseInfo( adata, NULL, mta->uorig, &bdims, &bdata, status );

/* Store the base dimensions */
      for( i=0; i<mta->ndim; i++ )
	mta->ddims[i] = bdims[i];

/* Decide whether data is contiguous in memory. The condition for this */
/* to be the case is that all but the last used dimension must be equal */
/* in size to the declared dimension */
      for( i=0; i<(mta->ndim-1); i++ )
	if ( mta->udims[i] != mta->ddims[i] )
	  mta->contig = ADI__false;

/* Must point to original data so that origin and base dimensions can be */
/* applied when copying data to and from slices */
      mta->data = (void *) _DTDAT(bdata);
      }
    else
      {
      mta->data = _DTDAT(id);
      mta->size = tdef->alloc.size;

      mta->ndim = 0;                      /* Flag as scalar */

      mta->udims[0] = 1;
      mta->uorig[0] = 1;
      mta->ddims[0] = 1;
      }
    }
  }

void adix_findmem( ADIobj id, char *mem, int mlen,
		   ADIobjRequest *objreq, ADIstatus status )
  {
  ADIobj                curmem;
  ADIlogical            found = ADI__false;
  int                   imem = 1;
  ADIclassDef           *tdef;

  _chk_stat;

/* Import member name */
  _GET_NAME(mem,mlen);

  tdef = _DTDEF(id);
  curmem = tdef->members;

  while ( ! (_null_q(curmem) || found) ) {
    if ( strncmp( _mdef_name(curmem), mem, mlen ) ) {
      imem++;
      curmem = _mdef_next(curmem);
      }
    else {
      found = ADI__true;
      objreq->parent = id;
      objreq->name = _mdef_aname(curmem);
      objreq->data = _class_data(id) + imem - 1;
      }
    }

/* No such member? */
  if ( ! found )
    adic_setecs( ADI__NOMEMB, "Class %s has no member called %*s",
		 status, tdef->name, mlen, mem );
  }


void adix_chkget( ADIobj *id, ADIobj **lid, ADIstatus status )
  {
/* Check GET operation ok on this object. Allow GETs on primitive data
 * defined by ADI, and on class data where a default member is defined
 */
  if ( ! _valid_q(*id) ) {              /* Valid ADI identifier? */
    adic_setecs( ADI__IDINV, NULL, status );
    }
  else if ( _han_q(*id) )               /* Only scalar object allowed */
    {
    ADIclassDef      *tdef = _DTDEF(*id);

/* Data not set? */
    if ( ! _han_set(*id) ) {
      adic_setecs( ADI__NOTSET, NULL, status );
      }

    else if ( tdef->prim ) {            /* Object is primitive? */
      *lid = id;
      }
    else                                /* User supplied a class object */
      {
      *lid = adix_defmem( id, status );

      if ( _null_q(**lid) ) {
	adic_setecs( ADI__NOTSET, "Default data member has no value", status );
	}
      }
    }
  else {
    adic_setecs( ADI__ILLKOP, "Cannot GET data from kernel objects", status );
    }
  }


void adix_chkput( ADIobj *id, ADIobj **lid, ADIstatus status )
  {
/* Check PUT operation ok on this object. Allow PUTs on primitive data
 * defined by ADI, and on class data where a default member is defined
 */
  if ( _null_q(*id) )                	/* Valid ADI identifier? */
    *lid = id;
  else if ( _han_q(*id) ) {             /* Only scalar object allowed */
    ADIclassDef      *tdef = _DTDEF(*id);

    if ( _han_readonly(*id) ) {
      adic_setecs( ADI__RDONLY, "Illegal write operation attempted", status );
      }

    else if ( tdef->prim ) {            /* Object is primitive? */
      *lid = id;
      }
    else                                /* User supplied a class object */
      *lid = adix_defmem( id, status );
    }
  else if ( _prim_q(*id) )                	/* Valid ADI identifier? */
    *lid = id;
  else
    adic_setecs( ADI__ILLKOP, "Cannot PUT data from kernel objects", status );
  }


void ADIkrnlLocDat( ADIobj *id, char *name, int nlen, int flgs,
		    ADIobjRequest *objreq, ADIstatus status )
  {
  char          *lname = name;		/* Local copies of name address */
  int           lnlen = nlen;		/* and length */
  ADIlogical    iscreate =              /* Create access requested? */
		    (flgs & DA__CREATE);

/* Default field values */
  objreq->data = id;
  objreq->parent = ADI__nullid;
  objreq->name = ADI__nullid;
  objreq->lentry = NULL;

/* Check inherited global status on entry */
  _chk_stat;

/* Decide on component access type from name string. Note we can't */
/* distinguish between class members and structure components yet */
  objreq->ctype = ADIcmpValue;
  if ( name ) {                         /* Decide on mode */
    if ( *name == '.' ) {               /* Property name preceded by period */
      objreq->ctype = ADIcmpProperty;
      lname++;
      if ( lnlen > 0 ) lnlen--;
      }
    else if ( *name )                   /* Don't allow null strings */
      objreq->ctype = ADIcmpMember;

/* Removes trailing spaces */
    _GET_NAME(lname,lnlen);
    }

/* Simple value? */
  if ( objreq->ctype == ADIcmpValue ) {

/* Check write operation ok */
    if ( iscreate )
      adix_chkput( id, &objreq->data, status );

/* Check read operation ok */
    else if ( flgs & DA__SET )
      adix_chkget( id, &objreq->data, status );
    }

  else if ( objreq->ctype == ADIcmpMember ) {  /* Named component */

/* Input object is a structure? */
    if ( _struc_q(*id) ) {

/* Find component insertion point */
      adix_pl_find( *id, _struc_data(*id), lname, lnlen, iscreate,
		    objreq, status );

      if ( (flgs & DA__SET) && ! objreq->data )
	adic_setecs( ADI__NOCOMP, "Structure component /%*s/ does not exist",
			status, lnlen, lname );
      }

/* Find member insertion point */
    else
      adix_findmem( *id, lname, lnlen, objreq, status );
    }

/* Property access */
  else if ( objreq->ctype == ADIcmpProperty ) {

/* Find property insertion point */
    adix_pl_find( *id, &_han_pl(*id), lname, lnlen, iscreate, objreq, status );

    if ( (flgs & DA__SET) && ! objreq->data )
      adic_setecs( ADI__NOPROP, "Property %*s does not exist", status,
		lnlen, lname );
    }

/* Ok so far and address defined? */
  if ( _ok(status) && objreq->data ) {

/* Object is required to be an array */
    if ( _valid_q(*objreq->data) && (flgs & DA__ARRAY)) {
      ADIlogical	isarray = ADI__false;
      if ( _han_q(*objreq->data) )
	isarray = _ary_q(_han_id(*objreq->data));

      if ( ! isarray )
	adic_setecs( ADI__INVARG, "Array object expected", status );
      }
    }
  }



/*
 * Does a component exist?
 */
ADIlogical adix_there( ADIobj id, char *name, int nlen, ADIstatus status )
  {
  ADIobjRequest	objreq;

/* Error if not initialised */
  _chk_init_err; _chk_stat_ret(ADI__false);

/* Find data insertion point */
  ADIkrnlLocDat( &id, name, nlen, DA__DEFAULT, &objreq, status );

/* Status good if component exists */
  if ( _ok(status) ) {
    if ( objreq.data )
      return _valid_q(*objreq.data) ? ADI__true : ADI__false;
    else
      return ADI__false;
    }
  else {
    adix_erranl( status );
    return ADI__false;
    }
  }

/*
 * Locate a component
 */
ADIobj adix_find( ADIobj id, char *name, int nlen, ADIstatus status )
  {
  ADIobjRequest	objreq;

/* Error if not initialised */
  _chk_init_err; _chk_stat_ret(ADI__nullid);

/* Find data insertion point */
  ADIkrnlLocDat( &id, name, nlen, DA__SET, &objreq, status );

  if ( _ok(status) && objreq.data ) {

/* Bump up reference count unless it's a kernel object */
    if ( _han_q(*objreq.data) )
      adix_refadj( *objreq.data, 1, status );
    return *objreq.data;
    }
  else {
    adix_erranl( status );
    return ADI__nullid;
    }
  }

ADIobj adix_findi( ADIobj id, ADIobj name, ADIstatus status )
  {
  ADIstring	*str;

/* Error if not initialised */
  _chk_init_err; _chk_stat_ret(ADI__nullid);

/* Locate string */
  str = _str_data(name);

/* Locate component */
  return adix_find( id, str->data, str->len, status );
  }


ADIobj adix_clone( ADIobj id, ADIstatus status )
  {
/* Must be initialised */
  _chk_init_err; _chk_stat_ret(ADI__nullid);

/* Bump up reference count and */
  if ( _valid_q(id) )
    adix_refadj( id, 1, status );

  return id;
  }

void adix_slice( ADIobj id, char *name, int nlen, int ndim,
		 int diml[], int dimu[], ADIobj *sid, ADIstatus status )
  {
  ADIobjRequest	objreq;

  _chk_init_err; _chk_stat;

/* Find the data address. Must be an array, and its data must be defined */
  ADIkrnlLocDat( &id, name, nlen, DA__ARRAY|DA__SET, &objreq, status );

/* Check not accessed */

/* Everything ok? */
  if ( _ok(status) ) {

/* Locate the array block */
    ADIarray         	*ary = _ary_data(_han_id(*objreq.data));
    int                 idim;

/* Check slice dimensionality */
    if ( ndim > ary->ndim )
      adic_setecs( ADI__INVARG, "Slice dimensionality exceeds that of object",
		   status );
    else {

/* Check slice bounds validity */
      for( idim=0; idim<ndim && _ok(status); idim++ ) {
	if ( (diml[idim] < 1) )
	  adic_setecs( ADI__INVARG, "Slice lower bound is less than one",
		   status );
	else if ( dimu[idim] > ary->dims[idim] )
	  adic_setecs( ADI__INVARG, "Slice upper bound is greater than object dimension",
		   status );
	else if ( diml[idim] > dimu[idim] )
	  adic_setecs( ADI__INVARG, "Slice lower bound is higher than upper bound",
		   status );
	}

      if ( _ok(status) ) {              /* Bounds are ok? */
	int     dims[ADI__MXDIM];
	ADIobj  fdid;
	ADIobj  newid;
	ADIobjHan	*hdata;

/*     Construct slice dimensions from lower and upper bounds */
	for( idim=0; idim<ndim; idim++ )
	  dims[idim] = dimu[idim] - diml[idim] + 1;

/*     Locate the array cell specified by indices supplied */
	fdid = ADIaryCell( ary, diml, status );

/*     Construct new array block, and increment reference count on */
/*     parent object */
	newid = ADIaryNew( ndim, dims, fdid,
			 adix_clone(id, status), status );

/*     Wrap the new array block in a handle */
	newid = ADIkrnlNewHan( newid, ADI__true, &hdata, status );

/*     Inherit state from parent */
	hdata->dataset = _han_set(id);

/*     New object's parent is the sliced object */
	hdata->pid = id;

	 *sid = newid;                  /* Return new object */
	}
      }
    }
  }


void adix_cell( ADIobj id, char *name, int nlen, int ndim,
		int index[], ADIobj *cid, ADIstatus status )
  {
  ADIobjRequest	objreq;

  _chk_init_err; _chk_stat;

/* Find data address */
  ADIkrnlLocDat( &id, name, nlen, DA__ARRAY|DA__DEFAULT, &objreq, status );

  /* Check not accessed */

  if ( _ok(status) ) {                  /* Everything ok? */
    ADIarray         	*ary;
    int                 idim;

/* Locate the array block */
    ary = _ary_data(_han_id(*objreq.data));

/* Check slice dimensionality */
    if ( ndim > ary->ndim )
      adic_setecs( ADI__INVARG, "Index dimensionality exceeds that of object",
		   status );
    else {

/* Check indices validity */
      for( idim=0; idim<ndim && _ok(status); idim++ ) {
	if ( (index[idim] < 1) )
	  adic_setecs( ADI__INVARG, "Index value is less than one",
		   status );
	else if ( index[idim] > ary->dims[idim] )
	  adic_setecs( ADI__INVARG, "Index value is greater than object dimension",
		   status );
	}

      if ( _ok(status) ) {              /* Indices are ok? */
	ADIobj  fdid;
	ADIobj  newid;
	ADIobjHan	*hdata;

/* Locate the array cell */
	fdid = ADIaryCell( ary, index, status );

/* Construct new handle */
	newid = ADIkrnlNewHan( fdid, ADI__true, &hdata, status );
	hdata->dataset = _han_set(id);

/*     New object's parent is the sliced object */
	hdata->pid = id;

	adix_refadj( id, 1, status );   /* Bump up ref count on parent */

	 *cid = newid;                  /* Return new object */
	}
      }
    }
  }


void adix_shape( ADIobj id, char *name, int nlen, int mxndim, int dims[],
		 int *ndim, ADIstatus status )
  {
  ADIobjRequest	objreq;

  _chk_init_err; _chk_stat;

/* Find data address */
  ADIkrnlLocDat( &id, name, nlen, DA__DEFAULT, &objreq, status );

  if ( _ok(status) ) {
    ADIobj              hid = _han_id(*objreq.data);
    int         idim;

    if ( _ary_q(hid) ) {                /* Array object? */
      ADIarray       	*adata = _ary_data(hid);

      if ( adata->ndim <= mxndim ) {
	*ndim = adata->ndim;

	for( idim=0; idim<adata->ndim; idim++ )
	  dims[idim] = adata->dims[idim];

	for( ; idim<mxndim; idim++ )
	  dims[idim] = 0;
	}
      else
	adic_setecs( ADI__EXCEED,
		   "Number of array dimensions exceeds buffer size",
		   status );
      }
    else {
      *ndim = 0;
      for( idim=0; idim<mxndim; idim++ )
	dims[idim] = 0;
      }
    }
  }

void adix_size( ADIobj id, char *name, int nlen, int *nelm, ADIstatus status )
  {
  ADIobjRequest	objreq;

  _chk_init_err; _chk_stat;

/* Find data address */
  ADIkrnlLocDat( &id, name, nlen, DA__DEFAULT, &objreq, status );

  if ( _ok(status) ) {
    ADIobj              hid = _han_id(*objreq.data);

/* Array object? */
    if ( _ary_q(hid) ) {
      ADIarray       	*adata = _ary_data(hid);

      *nelm = ADIaryCountNelm( adata->ndim, adata->dims );
      }
    else
      *nelm = 1;
    }
  }



/* Get value of object, or object component
 */
void adix_get_n( int clang, ADIobj id, char *name, int nlen,
		 int ndim, int mxdims[], ADIobj *clsid, int vsize,
		 void *value, int nactdims[], ADIstatus status )
  {
  int		idim;			/* Loop over dimensions */
  ADImta        imta;                   /* MTA for the object */
  ADIobjRequest	objreq;			/* Object request data */
  ADImta        omta;     		/* Output value MTA */
  ADIclassDef	*vtdef;

/* Error if not initialised */
  _chk_init_err; _chk_stat;

/* Locate definition */
  vtdef = _cdef_data(*clsid);

/* Find data insertion point */
  ADIkrnlLocDat( &id, name, nlen, DA__SET, &objreq, status );

/* Set input channel */
  adix_mtaid( *objreq.data, &imta, status );

/* Set output channel */
  ADIkrnlMtaInit( 0, ndim, mxdims, vsize, value, vtdef, clang, &omta, status );

/* Perform transfer */
  adix_mtacop( &imta, &omta, status );

/* Everything ok? */
  if ( _ok(status) ) {

/* Caller wants actual dimensions back? */
    if ( nactdims )
      for( idim=0; idim<ndim; idim++ )
	nactdims[idim] = omta.udims[idim];/* Actual data used */
    }
  }


/*
 * Get object(s) of user named class
 */
void adix_get_nn( int clang, ADIobj id, char *name, int nlen,
		  char *cls, int clen, int ndim, int mxdims[],
		  void *value, int nactdims[], ADIstatus status )
  {
  ADIclassDef           *tdef;          /* Class definition */

  _chk_init; _chk_stat;                 /* Standard checks */

/* Locate the class allocator */
  tdef = ADIkrnlFindClsC( cls, clen, status );

/* Invoke get function with null data values */
  if ( tdef )
    adix_get_n( clang, id, name, nlen, ndim, mxdims, &tdef->selfid,
                tdef->alloc.size, value, nactdims, status );
  }


void adix_chkmode( char *mode, int mlen, ADIacmode *amode, ADIstatus status )
  {
  _chk_stat;                            /* Check status on entry */

  _GET_NAME(mode,mlen);               /* Import the string */

  if ( ! strx_cmpi2c( mode, mlen, "READ", _MIN(4,mlen) ) )
    *amode = ADI__read;
  else if ( ! strx_cmpi2c( mode, mlen, "WRITE", _MIN(5,mlen) ) )
    *amode = ADI__write;
  else if ( ! strx_cmpi2c( mode, mlen, "UPDATE", _MIN(6,mlen) ) )
    *amode = ADI__update;
  else
    adic_setecs( ADI__INVARG, "Invalid access mode /%*s/", status,
		mlen, mode );
  }

/* Look for map control object with specified mapping type. Return insertion
 * point for new list element if not present, otherwise the the address of
 * of an ADIobj pointing to the list node containing the amp control object */
ADIobj adix_loc_mapctrl( ADIobj id, ADIclassDef *mtype, void *ptr,
			 ADIobj **ipoint, ADIstatus status )
  {
  ADIobj	*car_c, *cdr_c;
  ADIobj        *laddr = &_han_lock(id);
  ADIobj        curo = *laddr;
  ADIobj        lobj = ADI__nullid;

  _chk_stat_ret(ADI__nullid);

/* The default insertion point for new locks */
  *ipoint = laddr;

  while ( _null_q(lobj) && _valid_q(curo) ) {

/* Locate list cell addresses */
    _GET_CARCDR_A( car_c, cdr_c, curo );

/* Get next lock object, and test whether it's a map control */
    lobj = *car_c;
    if ( _mapctrl_q(lobj) ) {
      if ( ptr ) {
	if ( ptr != _mapctrl_dptr(lobj) )
	  lobj = ADI__nullid;
	}
      else if ( mtype != _mapctrl_type(lobj) )
	lobj = ADI__nullid;
      }
    else
      lobj = ADI__nullid;

    if ( _null_q(lobj) ) {
      *ipoint = cdr_c;
      curo = **ipoint;
      }
    }

  if ( _null_q(lobj) )
    *ipoint = laddr;

  return lobj;
  }

ADIobj adix_add_mapctrl( ADIobj id, ADIacmode mode, ADIclassDef *mtype,
			 size_t nbyte, ADIlogical dynamic,
			 ADIstatus status )
  {
  ADIobj        *ipoint;
  ADIobj        lobj;
  ADImapCtrl    *mctrl = NULL;
  ADIobj        newm;                   /* New object */

/* Check inherited status on entry */
  _chk_stat_ret(ADI__nullid);

/* Look for existing map control with similar mapping type. Note that if
 * mode is not ADI__read, then any existing mapping control object on the
 * lock list causes an error */
  lobj = adix_loc_mapctrl( id, mtype, NULL, &ipoint, status );
  if ( _valid_q(lobj) && (mode != ADI__read) ) {
    mctrl = _mapctrl_data(lobj);

    adic_setecs( ADI__MAPPED,
	"Object is already mapped for %s access with type %s",
	status, adix_accname( mctrl->mode ), mctrl->type->name );
    }

/* Allocate new map control if checks were ok */
  if ( _ok(status) && ! mctrl ) {

    newm = adix_cls_nallocd( &KT_DEFN_mapctrl, 0, 0, NULL, status );

    mctrl = _mapctrl_data(newm);
    lobj = ADI__nullid;
    }

/* No access clashes or allocation errors? */
  if ( _ok(status) ) {
    if ( _valid_q(lobj) )
      mctrl->nref++;
    else {
      mctrl->mode = mode;
      mctrl->nbyte = nbyte;
      mctrl->type = mtype;
      mctrl->nref = 1;
      mctrl->dynamic = dynamic;

      if ( dynamic )                    /* Dynamic data is required? */
	mctrl->dptr = (void *) ADImemAlloc( nbyte, status );

/* Append new lock to object's locking list */
      *ipoint = lstx_append( *ipoint,
			lstx_cell( newm, ADI__nullid, status ), status );
      }
    }
  else
    newm = ADI__nullid;

  return newm;                          /* Set return value */
  }


/* Map value of object, or object component
 */
void adix_map_n( int clang, ADIobj id, char *name, int nlen,
		 char *mode, int mlen, ADIobj *clsid, int vsize,
		 void **vptr, ADIstatus status )
  {
  ADIacmode     imode;                  /* Mapping mode */
  ADImta        imta;                   /* MTA for the object */
  int           damode;                 /* Data access mode */
  ADIobj        mctrl;                  /* Map control object */
  ADIobjRequest	objreq;			/* Object request data */
  ADImta        omta;     		/* Output value MTA */
  ADIclassDef	*vtdef;

/* Error if not initialised */
  _chk_init_err; _chk_stat;

/* Locate definition */
  vtdef = _cdef_data(*clsid);

/* Validate the mapping mode string */
  adix_chkmode( mode, mlen, &imode, status );

/* If writing, allow creation, otherwise insist on data being set */
  if ( imode == ADI__write )
    damode = DA__CREATE;
  else
    damode = DA__SET;

/* Find data insertion point */
  ADIkrnlLocDat( &id, name, nlen, damode, &objreq, status );

  if ( _ok(status) ) {
    size_t      nbyte = 0;
    ADIlogical  dynamic = ADI__false;

/* Set input channel */
    adix_mtaid( *objreq.data, &imta, status );

/* Need dynamic memory if different types or if mapped object is */
/* non-contiguous in memory */
    if ( (vtdef != imta.tdef) || ! imta.contig ) {
      dynamic = ADI__true;
      nbyte = vsize * ADIaryCountNelm( imta.ndim, imta.udims );
      }

/* Create the mapping control object */
    mctrl = adix_add_mapctrl( *objreq.data, imode, vtdef, nbyte, dynamic, status );

/* Perform data conversion if dynamic, otherwise just point to the input */
/* data object */
    if ( dynamic ) {

/* Set output channel */
      ADIkrnlMtaInit( 0, imta.ndim, imta.udims, vsize, _mapctrl_dptr(mctrl),
		      vtdef, clang, &omta, status );

/* Perform data transfer */
      adix_mtacop( &imta, &omta, status );
      }
    else
      _mapctrl_dptr(mctrl) = (void *) imta.data;
    }

/* Set value of returned pointer */
  *vptr = _ok(status) ? _mapctrl_dptr(mctrl) : NULL;
  }

void adix_map_t( int clang, ADIobj id, char *name, int nlen,
		 char *cls, int clen, char *mode, int mlen,
		 void **vptr, ADIstatus status )
  {
  ADIclassDef           *tdef;          /* Class definition */

  _chk_init_err; _chk_stat;             /* Standard checks */

/* Locate the allocator object */
  tdef = ADIkrnlFindClsC( cls, clen, status );

  if ( tdef )
    adix_map_n( clang, id, name, nlen, mode, mlen, &tdef->selfid,
		tdef->alloc.size, vptr, status );
  }

void adix_unmap_n( ADIobj id, char *name, int nlen,
		   void *vptr, ADIstatus status )
  {
  ADIobj        *ipoint;
  ADIobjRequest	objreq;			/* Object data specification */
  ADIobj        lobj;                   /* The object lock */

/* Error if not initialised */
  _chk_init_err; _chk_stat;

/* Find data address */
  ADIkrnlLocDat( &id, name, nlen, DA__DEFAULT, &objreq, status );

  lobj = adix_loc_mapctrl( *objreq.data, 0, vptr, &ipoint, status );

  if ( _valid_q(lobj) ) {
    ADImapCtrl  *mctrl = _mapctrl_data(lobj);

    if ( vptr )
      mctrl->nref--;
    else
      mctrl->nref = 0;

    if ( ! mctrl->nref ) {
      ADIobj    linkobj;

/* Do we have to convert the data back to the original object type? */
      if ( mctrl->dynamic ) {
	ADImta  imta;                   /* MTA describing mapped data */
	ADImta  omta;                   /* MTA describing mapping object */

	adix_mtaid( *objreq.data, &omta, status );  /* Set output channel */

	imta = omta;
	imta.tdef = mctrl->type;
	imta.data = mctrl->dptr;

	adix_mtacop( &imta, &omta, status );/* Perform transfer */
	}

/* If the mapping mode was write, the data is now set */
      if ( mctrl->mode == ADI__write )
	_han_set(*objreq.data) = ADI__true;

/* Keep a copy of the next lock object in the chain */
      linkobj = _CDR(*ipoint);
      _CDR(*ipoint) = ADI__nullid;

/* Erase the list element and the mapping lock object */
      adic_erase( ipoint, status );

/* Ensure list links are kept up to date */
      *ipoint = linkobj;
      }
    }
  }

/* Write data to a slot address. If the address is NULL, create a new object
 * otherwise write the data.
 *
 *   Method :
 *
 *    IF (object address holds null) THEN
 *      create object with dimensions from <mta>
 *    ENDIF
 *    IF (constructor defined and object data unset) THEN
 *      invoke constructor
 *      set dataset bit if <mta> data defined
 *    ENDIF
 *
 */
void adix_wdata( ADIobjRequest *objreq, ADImta *mta, ADIstatus status )
  {
  ADIobj	*id = objreq->data;

/* If the data slot is empty create the data object using the class */
/* definition of the data transfer object */
  if ( _null_q(*id) ) {
    *id = adix_cls_nallocd( mta->tdef, mta->ndim, mta->ddims, NULL, status );

/* Set parent object and name if object created ok */
    if ( _ok(status) ) {
      _han_pid(*id) = objreq->parent;
      _han_name(*id) = objreq->name;
      }
    }

/* Input data defined? */
  if ( mta->data ) {
    ADImta		omta;           /* MTA for the object */

    adix_mtaid( *id, &omta, status );   /* Set output channel */

    adix_mtacop( mta, &omta, status );  /* Perform transfer */

    if ( _ok(status) && _han_q(*id) ) {                /* Everything ok? */

      _han_set(*id) = ADI__true;
      if ( _valid_q(objreq->parent) )
	_han_set(objreq->parent) = ADI__true;
      }
    }
  }


/* Object creation
 */
void adix_new_n( ADIlogical clang, ADIobj pid, char *name, int nlen,
		 int ndim, int dims[], void *value, ADIobj *clsid,
		 int vsize, ADIobj *id, ADIstatus status )
  {
  ADImta        imta;     		/* MTA for the object */
  ADIobjRequest	objreq;			/* Object data specification */
  ADIclassDef	*tdef;

/* Check initialised & ok */
  _chk_init; _chk_stat;

/* Locate definition */
  tdef = _cdef_data(*clsid);

/* Find data insertion point */
  if ( _valid_q(pid) )                  /* If structured data */
    ADIkrnlLocDat( &pid, name, nlen, DA__CREATE, &objreq, status );
  else {
    *id = ADI__nullid;
    objreq.data = id;
    objreq.parent = ADI__nullid;
    objreq.name = ADI__nullid;
    }

/* Set up the input channel */
  ADIkrnlMtaInit( 1, ndim, dims, vsize, value, tdef, clang, &imta, status );

/* Write the data */
  adix_wdata( &objreq, &imta, status );

/* Everything went ok, and the caller wants the object address back? */
  if ( _ok(status) && id )
    *id = *objreq.data;
  }


/* Put value of object, or object component
 */
void adix_put_n( int clang, ADIobj id, char *name, int nlen,
		 int ndim, int dims[], ADIobj *clsid,
		 int vsize, void *value, ADIstatus status )
  {
  ADImta        imta;     		/* Input value MTA */
  ADIobjRequest	objreq;			/* Object data specification */
  ADIclassDef	*tdef;

/* Error if not initialised */
  _chk_init_err; _chk_stat;

/* Trap duff identifier */
  if ( name && ! _valid_q(id) ) {
    _GET_NAME(name,nlen);
    adic_setecs( ADI__INVARG, "Attempt to write member %*s of invalid object",
                 status, nlen, name );
    }

  else {

/* Locate definition */
    tdef = _cdef_data(*clsid);

/* Find the data insertion point */
    ADIkrnlLocDat( &id, name, nlen, DA__CREATE, &objreq, status );

/* Set up the input channel */
    ADIkrnlMtaInit( 1, ndim, dims, vsize, value, tdef, clang, &imta, status );

/* Write the data */
    adix_wdata( &objreq, &imta, status );
    }
  }


void adix_put_nn( int clang, ADIobj id, char *name, int nlen,
                  char *cls, int clen, int ndim, int dims[],
		  void *value, ADIstatus status )
  {
  ADIclassDef           *tdef;          /* Class definition */

  _chk_init; _chk_stat;                 /* Standard checks */

/* Locate the class allocator */
  tdef = ADIkrnlFindClsC( cls, clen, status );

/* Invoke put function */
  if ( tdef )
    adix_put_n( clang, id, name, nlen, ndim, dims, &tdef->selfid,
                tdef->alloc.size, value, status );
  }



ADIobj adix_copy( ADIobj id, ADIstatus status )
  {
  ADIarray   	*adata;
  char		*data;
  ADIobj	hid;
  ADIlogical	ishan = ADI__false;
  ADIobj        rval = id;
  ADIlogical	rdonly = ADI__false;
  ADIclassDef	*tdef;
  ADIobj        temp;

  _chk_init_err; _chk_stat_ret(ADI__nullid);

/* Trap null objects */
  if ( _valid_q(id) ) {

/* Get base data and type */
    data = adix_iddt( id, &tdef );

/* If it's a handle, locate the handled object */
    if ( tdef == &KT_DEFN_han ) {
      hid = ((ADIobjHan *) data)->id;
      ishan = ADI__true;
      rdonly = _han_readonly(id);
      }
    else
      hid = id;

/* Object is a handled readonly object? If so, just clone it */
    if ( rdonly )
      rval = adix_clone( id, status );

    else {

/* Locate next level data for handled objects */
      if ( ishan )
	data = adix_iddt( hid, &tdef );

/* Object is an array */
      if ( tdef == &KT_DEFN_ary ) {
	adata = (ADIarray *) data;

/* Create new object */
	temp = adix_cls_nallocd( tdef, adata->ndim, adata->dims, NULL, status );
	}

/* Primitive (ie. non-class) data? */
      else if ( tdef->prim ) {
	ADImta	imta,omta;

/* Create new object */
	temp = adix_cls_nallocd( tdef, 0, 0, NULL, status );

	adix_mtaid( id, &imta, status );
	adix_mtaid( temp, &omta, status );
	adix_mtacop( &imta, &omta, status );

	if ( _ok(status) ) {
	  _han_set(temp) = ADI__true;
	  rval = temp;
	  }
	}

/* Class instance */
      else {
	int     i;
	ADIobj  *iobj,*oobj;

/* Create the new object */
	temp = adix_cls_nallocd( tdef, 0, 0, NULL, status );

	iobj = _class_data(id);
	oobj = _class_data(temp);

	for( i=0; i<tdef->nslot; i++)
	  *oobj++ = adix_copy( *iobj++, status );

	if ( _ok(status) )
	  rval = temp;
	}
      }
    }

  return rval;
  }


/*
 *  Compare objects - doesn't handle arrays
 */
ADIlogical adix_equal( ADIobj id1, ADIobj id2, ADIstatus status )
  {
  ADIlogical	same = ADI__false;
  ADIclassDef	*t1,*t2;
  char		*d1,*d2;
  ADIobj	*cd1,*cd2;
  size_t	ic;
  ADIstring	*s1,*s2;

  _chk_init_err; _chk_stat_ret(ADI__false);

/* Objects are identical? */
  if ( id1 == id2 )
    same = ADI__true;

/* Neither is null */
  else if ( _valid_q(id1) && _valid_q(id2) ) {

/* Types must be the same */
    t1 = _DTDEF(id1);
    t2 = _DTDEF(id2);
    if ( t1 == t2 ) {

/* Locate base data */
      d1 = _DTDAT(id1);
      d2 = _DTDAT(id2);

/* Kernel or simple data */
      same = ADI__true;
      if ( _str_q(id1) ) {
        s1 = (ADIstring *) d1;
        s2 = (ADIstring *) d2;
        same = ! strx_cmp2c( s1->data, s1->len, s2->data, s2->len );
        }
      else if ( t1->prim || t1->kernel ) {
	ic = t1->alloc.size;
	while ( ic-- && same)
	  same = (*d1++ == *d2++);
	}
      else {
	cd1 = (ADIobj *) d1;
	cd2 = (ADIobj *) d2;
	ic = t1->nslot;
	while ( ic-- && same )
	  same = adix_equal( *cd1++, *cd2++, status );
	}
      }
    }

  return same;
  }

/*
 * Copy an object component to another object component
 */
void adix_ccopy( ADIobj in, char *inmem, int inmlen, ADIobj out,
		 char *outmem, int outmlen, ADIstatus status )
  {
  ADIobjRequest	inreq,outreq;

  _chk_init_err; _chk_stat;

/* Locate input and output data slots */
  ADIkrnlLocDat( &in, inmem, inmlen, DA__SET, &inreq, status );
  ADIkrnlLocDat( &out, outmem, outmlen, DA__DEFAULT, &outreq, status );

/* Input data located ok? */
  if ( _ok(status) ) {

/* Non-null copy? */
    if ( *inreq.data != *outreq.data ) {

/* Erase existing data. Should erase name in member */
      if ( _valid_q(*outreq.data) )
	adix_erase( outreq.data, status );

/* Make copy of input */
      *outreq.data = adix_copy( *inreq.data, status );
      }
    }
  }

/*
 * Output streamer. Operates in two modes. In calculate mode only the length of
 * the output data packet is computed. In non-calculate mode the data is written
 * as well.
 */
ADIinteger adix_ostrm_i( ADIstream *stream, ADIobj id, int wmode, ADIstatus status )
  {
  char		*data;
  ADIinteger	len = 1;		/* Length of marker is minimum length */
  char		marker = _S_MARK_nul;
  ADIclassDef	*tdef;
  ADIeproc	*osproc;

/* Null objects get a null marker written */
  if ( _null_q(id) ) {

/* Stream the marker */
    if ( wmode )
      _OS_STRM_tm(stream,&marker);
    }

/* Non-null objects */
  else {

/* Locate data and type */
    data = adix_iddt( id, &tdef );

/* Check streamer function exists */
    if ( _valid_q(tdef->ostrm) ) {

/* Locate streamer procedure data */
      osproc = _eprc_data(tdef->ostrm);

/* Switch on kernel type. For handles and arrays write/account marker */
      if ( wmode ) {

	if ( tdef == &KT_DEFN_han )
	  marker = _S_MARK_han;
	else if ( tdef == &KT_DEFN_ary )
	  marker = _S_MARK_ary;
	else
	  marker = _S_MARK_obj;

/* Stream the marker */
	_OS_STRM_tm(stream,&marker);

/* Non-kernel objects get type id written too */
	if ( ! _krnl_q(id) ) {
	  _OS_STRM_id(stream,&(tdef->selfid));
	  len += sizeof(ADIobj);
	  }
	}

/* Invoke streamer function for this object */
      if ( osproc->c )
	len += (* (ADIcOstreamer)(osproc->prc))( stream, id, data, wmode, status );
      }
    else
      adic_setecs( ADI__ILLOP, "Object class %s is not output streamable", status,
		   tdef->name );
    }

  return len;
  }

/*
 * Calculate total space required to stream the specified objects
 */
ADIinteger adix_ostrmc( int narg, ADIobj ids[], ADIstatus status )
  {
  int		i;
  ADIinteger	len = 0;

  _chk_stat_ret(0);

  len += sizeof(ADIstrmHdr);

  for( i=0; i<narg; i++ )
    len += adix_ostrm_i( NULL, ids[i], 0, status );

  return len;
  }


/*
 * Stream objects to a stream
 */
void adix_ostrm( ADIobj stream, int narg, ADIobj ids[], ADIinteger *tlen,
		 ADIstatus status )
  {
  int		i;
  ADIstrmHdr	hdr = {{'A','D','I',0},
		       ADI_MAJOR_VERSION,
		       ADI_MINOR_VERSION,
		       {' ',' '}};
  ADIstream	*sptr = _strm_data(stream);

  _chk_stat;

/* Length of header */
/*  *tlen = sizeof(hdr); */

/* Write header */
/*  _OS_STRM_write(sptr,sizeof(hdr),&hdr); */

  for( i=0; i<narg; i++ )
    *tlen += adix_ostrm_i( sptr, ids[i], 1, status );
  }


ADIobj adix_istrm_i( ADIstream *stream, ADIstatus status )
  {
  ADIobj	cid;
  ADIeproc	*isproc;
  char		marker;
  ADIobj	newid;
  ADIclassDef	*tdef;

/* Check inherited status on entry */
  _chk_stat_ret(ADI__nullid);

/* Read object marker code */
  marker = ADIreadCharFromStream( stream, status );

/* The null object? */
  if ( marker == _S_MARK_nul )
    newid = ADI__nullid;

/* Non-null object */
  else {

/* Normal non-kernel object */
    if ( marker == _S_MARK_obj ) {
      _IS_STRM_id(stream,&cid);

      tdef = _cdef_data(cid);
      }

/* Object handle */
    else if ( marker == _S_MARK_han )
      tdef = &KT_DEFN_han;

/* Object array */
    else if ( marker == _S_MARK_ary )
      tdef = &KT_DEFN_ary;

/* Otherwise illegal */
    else
      adic_setecs( ADI__SYNTAX, "Illegal item in input stream", status );

    if ( _ok(status) ) {
      if ( _valid_q(tdef->istrm) ) {
	isproc = _eprc_data(tdef->istrm);

	if ( isproc->c )
	  newid = (* (ADIcIstreamer)(isproc->prc))( stream, status );
	}
      else
	adic_setecs( ADI__ILLOP, "Object class %s is not input streamable", status,
		     tdef->name );
      }
    }

  return newid;
  }


ADIobj adix_istrm( ADIobj stream, ADIstatus status )
  {
  _chk_stat_ret(ADI__nullid);

  return adix_istrm_i( _strm_data(stream), status );
  }


void adix_print( ADIobj stream, ADIobj id, int level, ADIlogical value_only,
		 ADIstatus status )
  {
  ADIobj		args[2];	/* Args for invoking methods */
  ADIobj                curmem;
  ADIobj                *iobj;
  ADIclassDef           *tdef;

  _chk_init_err; _chk_stat;

/* Fixed first argument */
  args[0] = stream;

  if ( id == ADI__nullid )
    ADIstrmFprintf( stream, "<null>", status );
  else if ( _han_q(id) ) {
    ADIobj      hid = _han_id(id);

    if ( ! value_only )
      ADIstrmFprintf( stream, "< {%d:%d->%d:%d}, nref=%d, set=%d, ", status,
	    _ID_IBLK(id), _ID_SLOT(id),
	    _ID_IBLK(hid), _ID_SLOT(hid),
	    _han_ref(id), _han_set(id) );

    if ( _han_recur(id) ) {
      ADIstrmFprintf( stream, "CLOSED OBJ LOOP", status );
      return;
      }

/* Mark object to prevent recursive printing */
    _han_recur(id) = 1;

    if ( _ary_q(hid) )
      adix_print( stream, hid, level+1, value_only, status );
    else if ( _krnl_q(hid) )
      adix_print( stream, hid, level+1, ADI__false, status );
    else {
      tdef = _DTDEF(hid);               /* Locate class definition block */

      if ( ! value_only )
	ADIstrmFprintf( stream, "%s", status, tdef->name );
      if ( tdef->prim && ! _struc_q(id) && ! _han_set(id) ) {
	if ( ! value_only )
	  ADIstrmFprintf( stream, ", ", status );
	ADIstrmFprintf( stream, "<not set>", status );
	}
      else if ( _valid_q(tdef->prnt) ) {

	if ( ! value_only )
	  ADIstrmFprintf( stream, ", ", status );
	args[1] = id;
	adix_exemth( ADI__nullid, tdef->prnt, 2, args, status );
	}
      else if ( ! _prim_q(hid) ) {
	iobj = _class_data(id);
	for( curmem = tdef->members; _valid_q(curmem);
	     curmem = _mdef_next(curmem), iobj++ ) {
	  ADIstrmFprintf( stream, "\n  %S = %O", status, _mdef_aname(curmem), *iobj );
	  }
	ADIstrmFprintf( stream, "\n", status );
	}
      }

    if ( _valid_q(_han_pl(id)) && ! value_only ) {
      ADIobj curp = _han_pl(id);
      ADIstrmFprintf( stream, ", props = {", status );
      do {
	ADIobj	car,caar,cdar;

	_GET_CARCDR(car,curp,curp);
	_GET_CARCDR(caar,cdar,car);

	ADIstrmFprintf( stream, "%S = %O%c", status, caar, cdar,
		       _null_q(curp) ? '}' : ',' );
	}
      while ( _valid_q(curp) && _ok(status) );
      }

    if ( ! value_only )
      ADIstrmFprintf( stream, ">", status );

/* Restore recursion flag */
    _han_recur(id) = 0;
    }
  else if ( _cdef_q(id) ) {
    ADIclassDef  *tdef = _cdef_data(id);

    ADIstrmFprintf( stream, "< Class definition %s", status, tdef->name );
    if ( tdef->prim ) {
      ADIstrmFprintf( stream, ", primitive, size = %d bytes",
		     status, tdef->alloc.size );
      }
    else {
      ADIobj            cmem = tdef->members;
      ADIobj            cpar = tdef->superclasses;
      int               imem;

      if ( _null_q(cpar) )
	ADIstrmFprintf( stream, ", base class", status );
      else {
	ADIstrmFprintf( stream, ", superclasses {", status );
	for( ; _valid_q(cpar); cpar = _pdef_next(cpar) ) {
	  ADIstrmFprintf( stream, "0c", status, _pdef_name(cpar),
			 _valid_q(cpar) ? ' ' : '}' );
	  }
	}
      ADIstrmFprintf( stream, ",\n", status );

      for( imem=0; _valid_q(cmem); cmem = _mdef_next(cmem), imem++ ) {
	ADIstrmFprintf( stream, "  ", status );
	if ( _valid_q(_mdef_defcls(cmem) ))
	  ADIstrmFprintf( stream, "%s ", status,
		_cdef_data(_mdef_defcls(cmem))->name );
	ADIstrmFprintf( stream, "%S", status, _mdef_aname(cmem) );
	if ( imem == tdef->defmem )
	  ADIstrmFprintf( stream, "*", status );
	if ( _valid_q(_mdef_cdata(cmem) ))
	  ADIstrmFprintf( stream, " = %O", status, _mdef_cdata(cmem) );
	ADIstrmFprintf( stream, "\n", status );
	}
      }
    ADIstrmFprintf( stream, "  >", status );
    }
  else if ( _ary_q(id) ) {
    ADIarray	*ary = _ary_data(id);
    int         i;

    if ( ! value_only ) {
      if ( _krnl_q(ary->data) )
	ADIstrmFprintf( stream, "generic array [", status );
      else
	ADIstrmFprintf( stream, "%s[", status, _DTDEF(ary->data)->name );

      for( i=0; i<ary->ndim; i++ )
	ADIstrmFprintf( stream, "%d%s", status,
		ary->dims[i], ((i+1)==ary->ndim) ? "] = " : "," );
      }

    ADIaryPrint( stream, ary, status );

    value_only = ADI__true;
    }
  else {
    tdef = _DTDEF(id);               /* Locate class definition block */

    if ( _valid_q(tdef->prnt) ) {
      args[1] = id;
      adix_exemth( ADI__nullid, tdef->prnt, 2, args, status );
      }
    else {
      ADIstrmFprintf( stream, "<%s %d:%d", status, tdef->name,
	    _ID_IBLK(id), _ID_SLOT(id) );
      if ( _eprc_q(id) ) {
	ADIstrmFprintf( stream, ", lang=%s, addr=%p>", status,
		       _eprc_c(id) ? "C" : "Fortran", _eprc_prc(id) );
	}
      else if ( _mthd_q(id) ) {
	ADIstrmFprintf( stream,
		"\n   args = %O\n   form = %O\n   exec = %O\n   >",
		status, _mthd_args(id), _mthd_form(id),
		_mthd_exec(id) );
	}
      else
	ADIstrmFprintf( stream, ">", status );
      }
    value_only = ADI__true;
    }

  if ( ! value_only )
    ADIstrmFprintf( stream, "\n", status );

  if ( ! level )
    ADIstrmFflush( stream, status );
  }



/*
 *  C generic dispatch for methods of form void xx(ADIobj,ADIobj,status)
 *  Used for SetLink
 */
ADIobj adix_cdsp_voo( ADICB rtn, int narg, ADIobj args[], ADIstatus status )
  {
  (*((ADIooCB) rtn))( args[0], args[1], status );

  return ADI__nullid;
  }

ADIobj adix_cdsp_vo( ADICB rtn, int narg, ADIobj args[], ADIstatus status )
  {
  (*((ADIoCB) rtn))( args[0], status );

  return ADI__nullid;
  }

/*
 *  Fortran generic dispatch for methods of form void xx(ADIobj,ADIobj,status)
 *  Used for SetLink
 */
ADIobj adix_fdsp_voo( ADICB *rtn, int *narg, ADIobj args[], ADIstatus status )
  {
  (*((ADIfooCB) *rtn))( args+0, args+1, status );

  return ADI__nullid;
  }

ADIobj adix_fdsp_vo( ADICB *rtn, int *narg, ADIobj args[], ADIstatus status )
  {
  (*((ADIfoCB) *rtn))( args+0, status );

  return ADI__nullid;
  }

ADIobj adix_namei( ADIobj id, ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  if ( _han_q(id) ) {
    if ( _null_q(_han_name(id)) )
      adic_setecs( ADI__NONAME, NULL, status );
    else
      rval = adix_clone( _han_name(id), status );
    }
  else
    adic_setecs( ADI__NONAME, NULL, status );

  return rval;
  }

void adix_name( ADIobj id, ADIlogical clang, char *buf, int blen, ADIstatus status )
  {
  ADIobj	nid;

  _chk_init_err; _chk_init;

  nid = adix_namei( id, status );

  if ( _ok(status) ) {
    ADIstrngExport( nid, clang, buf, blen, status );
    adix_erase( &nid, status );
    }
  }


ADIobj adix_qcls( ADIobj id, char *member, int mlen, ADIstatus status )
  {
  ADIobjRequest	     objreq;

  _chk_init_err; _chk_stat_ret(ADI__nullid);

/* Locate data address. Place no requirements on contents */
  ADIkrnlLocDat( &id, member, mlen, DA__DEFAULT, &objreq, status );

  if ( _ok(status) )
    return _DTDEF(*objreq.data)->aname;
  else
    return ADI__nullid;
  }

ADIlogical adix_state( ADIobj id, char *member, int mlen, ADIstatus status )
  {
  ADIobjRequest		objreq;

  _chk_init_err; _chk_stat_ret(ADI__nullid);

/* Locate data address. Place no requirements on contents */
  ADIkrnlLocDat( &id, member, mlen, DA__DEFAULT, &objreq, status );

  if ( _ok(status) )
    return _krnl_q(*objreq.data) ? _han_set(*objreq.data) : ADI__true;
  else
    return ADI__false;
  }


/* Delete an object component. The component is always removed from its parent's */
/* structure, although the object data will not itself be deleted unless it */
/* reference count is already one */

void adix_cerase( ADIobj id, char *member, int mlen, ADIstatus status )
  {
  ADIobjRequest	objreq;			/* Object data specification */

  _chk_init_err; _chk_stat;

/* Locate data address. Place no requirements on contents */
  ADIkrnlLocDat( &id, member, mlen, DA__DEFAULT, &objreq, status );

/* Data exists? */
  if ( _valid_q(*objreq.data) ) {

/* Reset handled object's name */
    if ( _han_q(*objreq.data) )
      _han_name(*objreq.data) = ADI__nullid;

/* Simple data or class member */
    if ( objreq.ctype == ADIcmpValue || objreq.ctype == ADIcmpMember ) {
      adix_erase( objreq.data, status );
      *objreq.data = ADI__nullid;
      }

/* Property or structure component */
    else if ( objreq.ctype == ADIcmpProperty ||
	      objreq.ctype == ADIcmpStruct ) {
      ADIobj	*o_car, *o_cdr, old_lc;

/* The list cell pointing to dotted pair */
      old_lc = *objreq.lentry;

/* By-pass old list element */
      *objreq.lentry = _CDR(*objreq.lentry);

/* Nullify the list cell field which points to the rest of the association list */
      _GET_CARCDR_A(o_car,o_cdr,old_lc);
      *o_cdr = ADI__nullid;

/* Delete the list cell */
      adix_erase( &old_lc, status );
      }
    }
  }


void adix_putid( ADIobj id, char *name, int nlen, ADIobj value,
		 ADIstatus status )
  {
  ADImta	imta;
  ADIobjHan	*hdata = NULL;
  ADIobjRequest	objreq;			/* Object data specification */

  _chk_init_err; _chk_stat;

/* Check we can write the object. If its already named then we have problems */
  if ( _valid_q(value) ) {

/* Check non-kernel object */
    if ( _han_q(value) ) {
      hdata = _han_data(value);

      if ( _valid_q(hdata->pid) || _valid_q(hdata->pid) ) {
	adic_setecs( ADI__INTGTY,
	"Object is already part of a name hierarchy, and cannot become part of another hierarchy", status );
	return;
	}
      }
    else {
/*      adic_setecs( ADI__INVARG, "Cannot write kernel object as data member", status );
      return; */
      }
    }

/* Find data insertion point */
  ADIkrnlLocDat( &id, name, nlen, DA__CREATE, &objreq, status );

/* Located ok? */
  if ( _ok(status) ) {

/* Set object slot */
    if ( _null_q(*objreq.data) ) {
      *objreq.data = value;

      if ( hdata ) {
	if ( _han_ref(value) == 1 ) {
	  _han_pid(value) = objreq.parent;
	  _han_name(value) = objreq.name;
	  }
	}

      if ( _valid_q(objreq.parent) )
	_han_set(objreq.parent) = ADI__true;
      }
    else {

/* Set output channel */
      adix_mtaid( value, &imta, status );

/* Write the data */
      adix_wdata( &objreq, &imta, status );
      }
    }
  }

void adix_putiid( ADIobj id, ADIobj name, ADIobj value, ADIstatus status )
  {
  ADIstring 	*sptr;

  _chk_init_err; _chk_stat;

  sptr = _str_data(name);               /* Locate string data */

/* Find data insertion point */
  adix_putid( id, sptr->data, sptr->len, value, status );
  }



/* Generic and method handling
 *
 *  adix_locmco         - find method combination given name
 *  adix_newmco         - fill out a new method combination structure
 *  adix_defmco         - high level method combination definition
 *  adix_delmco         - destroy method combination object
 *  adix_stdmcf         - "Standard" method combination
 */

ADIobj adix_locmco( ADIobj name, ADIstatus status )
  {
  ADIobj        mcid = ADI__nullid;
  ADIobj	sbind;

  _chk_stat_ret(ADI__nullid);

  sbind = ADIsymFind( name, -1, ADI__true, ADI__mcf_sb, status );

  if ( _valid_q(sbind) )
    mcid = _sbind_defn(sbind);

  return mcid;
  }


ADIobj adix_newmco( ADIobj name, ADIobj cexec, ADIstatus status )
  {
  ADImethComb	mco;
  ADIobj        newid;                  /* New mco object */

/* Fill in data */
  mco.name = name;
  mco.cexec = cexec;

/* Allocate new method combination descriptor */
  newid = adix_cls_nallocd( &KT_DEFN_mco, 0, 0, &mco, status );

/* Allocation went ok? */
  if ( _ok(status) ) {

    ADIsymAddBind( name, -1,
		   ADIsbindNew( ADI__mcf_sb, newid, status ),
		   status );
    }

  return newid;                         /* Return new block */
  }


ADIobj adix_delmco( int narg, ADIobj args[], ADIstatus status )
  {
  ADImethComb  *dptr = _mco_data(args[0]);

  adix_erase( &dptr->name, status );
  adix_erase( &dptr->cexec, status );

  return ADI__nullid;
  }


void adix_defmcf( char *name, int nlen,
		  ADIobj rtn, ADIobj *id, ADIstatus status )
  {
  ADIobj        aname;                  /* ADI string version of name */
  ADIobj        mcid = ADI__nullid;     /* The new object */

  _chk_init; _chk_stat;                 /* Check status on entry */

  if ( _null_q(rtn) ) {                 /* Check not null routine */
    adic_setecs( ADI__INVARG,
		 "Illegal null method combination executor", status );
    }
  else {
/* Introduce name to table */
    aname = adix_cmn( name, nlen, status );

/* Try and locate it */
    mcid = adix_locmco( aname, status );

    if ( _valid_q(mcid) ) {
      adic_setecs( ADI__EXISTS,
		   "Method combination form /%S/ already exists", status, aname );
      }
    else {

/* Introduce to system */
      mcid = adix_newmco( aname, rtn, status );

      if ( _ok(status) && id )          /* Set return value */
	*id = mcid;
      }
    }
  }


ADIclassDef *adix_loccls( ADIobj name, ADIstatus status )
  {
  ADIclassDef	*tdef = ADI_G_firstcdef;
  ADIlogical    found = ADI__false;

  _chk_stat_ret(NULL);

  while ( tdef && ! found ) {
    if ( tdef->aname == name )
      found = ADI__true;
    else
      tdef = tdef->link;
    }

  return tdef;
  }

/*
 * Does the class c2 exist in the inheritance list of c1?
 */
ADIlogical adix_chkder( ADIclassDef *c1, ADIclassDef *c2, ADIstatus status )
  {
  ADIlogical    derived = ADI__true;    /* True by default */

  _chk_stat_ret(ADI__false);            /* Check status on entry */

/* A class is derived from itself */
  if ( c1 != c2 ) {
    ADIobj curp = c1->superclasses;

/* Loop over superclasses */
    while ( _valid_q(curp) ) {
      ADIclassDef *ptdef;

/* Locate parent class definition */
      ptdef = _cdef_data(_pdef_clsid(curp));

      if ( c2 == ptdef )                /* Match found? */
	return ADI__true;
      else if ( adix_chkder( ptdef,     /* Or in parent classes of parent? */
		c2, status ) )
	return ADI__true;

      curp = _pdef_next(curp);          /* Next superclass */
      }

    derived = ADI__false;               /* Fall though -> not derived */
    }

  return derived;
  }


ADIlogical ADIkrnlChkDerived( ADIobj id, char *name, int nlen,
			      ADIstatus status )
  {
  ADIclassDef	*tdef;

  _chk_init_err; _chk_stat_ret(ADI__false);

  _GET_NAME(name,nlen);

  tdef = ADIkrnlFindClsInt( name, nlen, status );

  return adix_chkder( _DTDEF(id), tdef, status );
  }


ADIobj adix_mthargcls( ADIobj arg, ADIstatus status )
  {
  ADIobj	cls,patobj;
  ADIobj	head,earg,phead,parg;

  if ( _etn_q(arg) ) {
    _GET_HEDARG( head, earg, arg );

/* One of the blank sequences? */
    if ( (head == K_Blank) || (head == K_BlankSeq) ||
			      (head ==K_BlankNullSeq) ) {

/* Locate match class */
      patobj = _CAR(earg);

/* Null pattern object should mean global all-containing class */
      if ( _null_q(patobj) )
	cls = K_WildCard;

/* Expression structured pattern object */
      else if ( _etn_q(patobj) ) {

/* Get bits of pattern expression */
	_GET_HEDARG( phead, parg, patobj );

/* Is it an array specifier? */
	if ( phead == K_ArrayRef ) {

	  ADIobj	nxtarg;

	  _GET_CARCDR( cls, nxtarg, parg );

	  cls = _etn_head(cls);
	  }
	else
	  cls = phead;

	cls = ADIkrnlFindClsI( cls, status );
	}
      }
    else
      cls = UT_cid_etn;
    }
  else
    cls = _DTDEF(arg)->selfid;

  return cls;
  }


/*  Rank a list of methods in descending priority order given a bunch of
 *  arguments */
void adix_primth( int narg, int farg, int nmth,
		  ADIobj *mlist, ADIstatus status )
  {
  ADIclassDef	*acdef;
  ADIobj 	acls;
  ADIobj	cura;			/* Current arg list cell */
  ADIobj	carcura;		/* Current car(cura) */
  ADIobj        curp,cursp,sdselem;
  int           iarg;
  ADIobj	lmlist = *mlist;	/* List of remaining methods */
  ADIobj        newlist = ADI__nullid;  /* The users reordered list */
  int           nleft;                  /* Number of methods remaining */
  ADIobj        *ipoint = &newlist;     /* List insertion point */

  _chk_stat;

/* If nthm = 0 was supplied, then do them all */
  if ( ! nmth ) {
    curp = lmlist;
    while ( _valid_q(curp) ) {
      nmth++;
      curp = _CDR(curp);
      }
    }

/* Check case of only one method */
  if ( nmth > 1 ) {

/* While more input methods and more arguments to process */
    for( nleft=nmth, iarg=farg; iarg<narg && (nleft>1) && _ok(status); iarg++ ) {

      int       imth;
      ADIobj    curp,mthd,nxtmthd;
      ADIobj    dslist = ADI__nullid;   /* Direct superclass list */
      ADIobj  	mclist = ADI__nullid;   /* Method arg class list */
      ADIobj    rlist;                  /* Ranked class list */

/* Gather a list of the classes which appear at this argument position for */
/* each of the remaining methods */
      curp = lmlist;
      imth = 1;
      while ( imth <= nmth ) {
	int     jarg = iarg + 1;

/* Locate current method and the next one */
	_GET_CARCDR(mthd,nxtmthd,curp);
	cura = _mthd_args(mthd);

/* Skip to the iarg'th argument for this method */
	while ( jarg-- )
	  _GET_CARCDR( carcura, cura, cura );

/* Locate class definition object */
	acls = adix_mthargcls( carcura, status );
	acdef = _cdef_data(acls);

/* Add each element of this classes set of direct-superclass lists to our list of */
/* such. The sdslist of a class consists of a list of all the direct superclass */
/* relationships in that class's ancestry */
	if ( _valid_q(acdef->sdslist) ) {
	  cursp = acdef->sdslist;
	  while ( _valid_q(cursp) ) {
	    _GET_CARCDR( sdselem, cursp, cursp );
	    lstx_addtoset( &dslist, sdselem, status );
	    }
	  }

/* Add to our set of list elements */
	lstx_addtoset( &mclist, acdef->aname, status );

/* Next method */
	curp = nxtmthd; imth++;
	}

/* Order the list of classes appearing in mclist into ascending priority. */
/* If there are no direct superclasses, then the ordering is moot */
      if ( _valid_q(dslist) ) {
	rlist = adix_estab_ord( mclist, dslist, status );

/* Destroy the list of direct superclasses */
	lstx_sperase( &dslist, status );

/* Destroy the list of argument classes */
	lstx_sperase( &mclist, status );
	}
      else
	rlist = mclist;

      if ( ! _ok(status) )
        return;

/* Now process the list of methods. Start at the beginning of the ranked
 * list and shuffle the remaining methods with this class to the head of the
 * the list. */
      curp = rlist;
      while ( _valid_q(curp) && (nleft>1) ) {

	int     nmoved = 0;
	ADIobj  curm = lmlist;
	ADIobj  *cpoint = &lmlist;
	int     imth = 0;
	ADIlogical anyout = ADI__false;
	ADIobj	currcls;

/* Locate current class and advance cursor to next one */
	_GET_CARCDR( currcls, curp, curp );

	while ( imth < nleft ) {

	  int     jarg = iarg + 1;
	  ADIobj	*mthdadr;
	  ADIobj  mthd;
	  ADIobj  *anext;
	  ADIobj  next;

/*    Locate list cells */
	  _GET_CARCDR_A(mthdadr,anext,curm);
	  mthd = *mthdadr;
	  cura = _mthd_args(mthd);
	  next = *anext;

/*    Skip to the iarg'th argument for this method */
	  while ( jarg-- )
	    _GET_CARCDR(carcura,cura,cura);

/*    Get method argument class */
          acls = adix_mthargcls( carcura, status );

/*    This method's argument matches the current one in the ranked list?
 *    Shove to the front of the list, unless there are no intervening out
 *    of order methods */
	  if ( currcls == _cdef_data(acls)->aname ) {
	    if ( anyout ) {
	      ADIobj old = lmlist;
	      *cpoint = next;
	      *anext = old;
	      lmlist = curm;
	      }
	    nmoved++;
	    }
	  else {
	    anyout = ADI__true;
	    cpoint = anext;
	    }

	  curm = next; imth++;
	  }

/* All the high priority methods have been transferred to the head of the
 * the list. If more than one method, invoke this routine recursively to
 * order them. */
	if ( (nmoved > 1) && ((farg+1) < narg) )
	  adix_primth( narg, farg+1, nmoved, &lmlist, status );

/* Move all these methods to our output list. They go on the end! Advance
 * the output list insertion point */
	*ipoint = lmlist;
	nleft -= nmoved;
	while ( nmoved-- )
	  ipoint = & _CDR(*ipoint);

/* Loose the first 'nmoved' methods from the current list */
	lmlist = *ipoint;
	}

      }

/* Set user list to reordered list */
    if ( _ok(status) )
      *mlist = newlist;
    }
  }


void adix_rep_nomf( ADIstatype ecode, char *whats, ADIobj name, int narg,
		    ADIobj args[], ADIstatus status )
  {
  char	ebuf[200];
  ADIobj	estr;
  int		i;

  if ( narg ) {
    ems_begin_c( status );
    estr = ADIstrmExtendCst( ADIstrmNew( "w", status ), ebuf, 200, status );
    for( i=0; i<narg; i++ ) {
      if ( _valid_q(args[i]) )
	ADIstrmFprintf( estr, "%S", status, _DTDEF(args[i])->aname );
      else
	ADIstrmFprintf( estr, "_", status );
      if ( i < (narg-1) )
	ADIstrmFprintf( estr, ",", status );
      }

    adix_erase( &estr, status );
    ems_end_c( status );
    }
  else
    ebuf[0] = 0;

  adic_setecs( ecode, "No %s matching signature %S(%s)",
	       status, whats, name, ebuf );
  }


void adix_rep_nomth( ADIobj name, int narg, ADIobj args[],
		     ADIstatus status )
  {
  adix_rep_nomf( ADI__NOMTH, "methods", name, narg, args, status );
  }


/*  Gather applicable methods for the given generic function, given
 *  arguments and the allowable method forms specified. Each method
 *  is processed as follows,
 *
 *  - for each method
 *    - Is the name the same as the generic?
 *      N: next method
 *    - Is the number of arguments the same as the generic?
 *      N: next method
 *    - Are we collecting this method form?
 *      N: next method
 *    - for each argument
 *      - is user arg the same class as the method arg, or a derived
 *        class of the method arg?
 *        N: next method
 *      - add to method list of each form
 *      end for each
 *    end for each
 *  - for each method form
 *    - add the method arg class to the list for this form, and the
 *      direct superclass list for this arg to the list of such for
 *      this form UNLESS the arg is already present in the former list
 *    end for each
 */
void adix_gthmth( ADIobj gen, int narg, ADIobj args[], int nmform,
		  ADIobj *mform[], ADIlogical mfopri[],
		  ADIobj mlist[], ADIstatus status )
  {
  ADIobj        cur;              	/* Cursors over lists */
  ADIlogical    found;                  /* Found method? */
  ADIgeneric    *gdata;                 /* Generic function data block */
  int           i;
  int		iarg;
  int           iform;                  /* Form id of method */
  ADImethod 	*mdata;                 /* Method data block */
  ADIobj        mth;                    /* Method object */
  int           nmth = 0;               /* Number of applicable methods */
  ADIlogical    ok;                     /* Validity test */

/* Check status on entry */
  _chk_stat;

/* Locate generic data */
  gdata = _gnrc_data(gen);

/* Initialise lists */
  for( i=0; i<nmform; i++ )
    mlist[i] = ADI__nullid;

/* This generic's method list */
  cur = gdata->mlist;

/* Store argument classes */
  for ( iarg=0; iarg<narg; iarg++ )
    ADIexprArgClass( iarg, args[iarg], status );

  while ( _valid_q(cur) ) {

/* Locate current method and advance cursor */
    _GET_CARCDR(mth,cur,cur);

/* Does it match? */
    mdata = _mthd_data(mth);

    found = ADI__false;                 /* Suitable form? */
    for( iform=0; iform<nmform && ! found ; )
      if ( *(mform[iform]) == mdata->form )
	found = ADI__true;
      else
	iform++;
    if ( ! found ) continue;

/* Does the binding match the classes of the arguments */
    ok = ADIexprTestBind( mth, narg, args, status );

    if ( ! ok )                         /* Arguments are incompatible */
      continue;

/* Method is applicable! */
    mlist[iform] = lstx_cell( mth, mlist[iform], status );

    nmth++;
    }

/* Check for no methods */
  if ( nmth ) {

/* Loop over forms */
    for ( iform=0; iform<nmform; iform++ )

/* Methods for this form? */
      if ( _valid_q(mlist[iform]) ) {

/*     Order the methods in descending order of priority */
	adix_primth( narg, 0, 0, mlist + iform, status );

/*     Callee wants list in ascending priority order? */
	if ( ! mfopri[iform] )
	  mlist[iform] = lstx_revrsi( mlist[iform], status );
	}
    }

/* Otherwise format an error message */
  else
    adix_rep_nomth( gdata->name, narg, args, status );
  }


/*  Implements "Standard" method combination. The method forms "Around",
 *  "Before" , "Primary" and "After" are all used.
 *
 */
ADIobj adix_stdmcf( ADIobj gen, int narg, ADIobj args[], ADIstatus status )
  {
  static                                /* Forms we want to gather */
    ADIobj *mforms[] = {
    &DnameAround, &DnameBefore, &DnamePrimary, &DnameAfter
    };

  static                                /* Highest priority first per form */
    ADIlogical mfopri[] = {
    ADI__true, ADI__true, ADI__true, ADI__false
    };

  ADIobj        curp;                   /* Cursor over method list */
  ADIlogical	eprot = ADI__false;	/* Error protection required */
  ADIlogical    finished = ADI__false;  /* Quit back to caller? */
  int		i;
  ADIobj        mlists[4];              /* Methods to be executed */
  ADIobj        mresult;                /* Method result */
  ADIobj	mthd;			/* Current method */
  ADIobj        rval = ADI__nullid;     /* Return value */

  _chk_stat_ret(ADI__nullid);

/* Gather the methods */
  adix_gthmth( gen, narg, args, 4, mforms, mfopri, mlists, status );
  _chk_stat_ret(ADI__nullid);

/* First the Around methods */
  curp = mlists[0];
  while ( _valid_q(curp) && ! finished && _ok(status) ) {

/* Locate current method and advance cursor */
    _GET_CARCDR(mthd,curp,curp);

/* Invoke the method */
    mresult = adix_exemth( gen, _mthd_exec(mthd), narg, args, status );

    if ( *status == ADI__CALNXTMTH ) {  /* Invoke the next method? */
      *status = SAI__OK;                /* Should erase result too? */
      }
    else {
      finished = ADI__true;
      rval = mresult;
      }
    }

/* Around methods didn't force exit */
  if ( ! finished ) {

    curp = mlists[1];                   /* Now the Before methods */

    while ( _valid_q(curp) && _ok(status) ) {

/* Locate current method and advance cursor */
      _GET_CARCDR(mthd,curp,curp);

/* Invoke the method */
      mresult = adix_exemth( gen, _mthd_exec(mthd), narg, args, status );

/* Invoke the next method? */
      if ( *status == ADI__CALNXTMTH ) {
	adic_setecs( ADI__MTHERR,
		     "Illegal use of ADI_CALNXT/adic_calnxt", status );
	}
      }

    curp = mlists[2];                   /* Now the Primary methods */

    while ( _valid_q(curp) && ! finished && _ok(status) ) {

/* Locate current method and advance cursor */
      _GET_CARCDR(mthd,curp,curp);

/* Invoke the method */
      mresult = adix_exemth( gen, _mthd_exec(mthd), narg, args, status );

/* Invoke the next method? */
      if ( *status == ADI__CALNXTMTH ) {
	*status = SAI__OK;              /* erase result?? */
	}
      else {
	finished = ADI__true;
	rval = mresult;
	}
      }

    curp = mlists[3];                   /* Now the After methods */

    while ( _valid_q(curp) && _ok(status) ) {

/* Locate current method and advance cursor */
      _GET_CARCDR(mthd,curp,curp);

/* Invoke the method */
      mresult = adix_exemth( gen, _mthd_exec(mthd), narg, args, status );

/* Invoke the next method? */
      if ( *status == ADI__CALNXTMTH )
	adic_setecs( ADI__MTHERR,
		     "Illegal use of ADI_CALNXT/adic_calnxt", status );
      }
    }

/* Destroy method lists */
  if ( ! _ok(status) ) {
    eprot = ADI__true;
    ems_begin_c( status );
    }

  for( i=0; i<4; i++ ) {
    if ( _valid_q(mlists[i]) )
      lstx_sperase( mlists + i, status );
    }

  if ( eprot )
    ems_end_c( status );

  return rval;                          /* Set the return value */
  }


ADIobj adix_locgen( ADIobj name, int narg, ADIstatus status )
  {
  ADIlogical    found = ADI__false;
  ADIobj	curp,cbind;
  ADIobj	glist;
  ADIobj        gnid = ADI__nullid;

  _chk_stat_ret(ADI__nullid);

  glist = ADIsymFind( name, -1, ADI__false, ADI__generic_sb, status );

/* Located any generics with this name? */
  if ( _valid_q(glist) ) {

/* Scan list looking for the first which matches the number of arguments */
    curp = glist;
    while ( _valid_q(curp) && ! found ) {
      _GET_CARCDR( cbind, curp, curp );

      gnid = _sbind_defn(cbind);
      if ( _gnrc_narg(gnid) == narg )
	found = ADI__true;
      }

/* Destroy list of generic symbol bindings */
    lstx_sperase( &glist, status );
    }

  return found ? gnid : ADI__nullid;
  }


/* Execute an external procedure. If the first argument is non-null then
 * the procedure is assumed to be a generic method, and a test is performed
 * to see if generic dispatch is defined. If no dispatch is specified, or
 * if the first argument is null, then the method is invoked with standard
 * argument form for the particular language of the method. Note that
 * methods are never invoked with bad status.
 */
ADIobj adix_exemth( ADIobj generic, ADIobj eproc,
		    int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical	compiled;		/* Compiled procedure? */
  ADIobj        disp;                   /* Dispatch function */
  ADIeproc	*prc;
  ADIobj        res = ADI__nullid;      /* Result of method */

  _chk_stat_ret(ADI__nullid);           /* Check status on entry */

/* Compiled routine? */
  compiled = _eprc_q(eproc);
  if ( compiled )
    prc = _eprc_data(eproc);

/* ADI object procedure. This is just a command list */
  if ( ! compiled ) {

    ADIexprPushFS( 0, eproc, status );

    res = ADIexprEvalList( eproc, ADI__nullid, status );

    adix_erase( &res, status );

    ADIexprPopFS( status );

    res = ADI__nullid;
    }

/* Method executor is C? */
  else if ( prc->c ) {

/* Locate generic C dispatch function */
    disp = _valid_q(generic) ? _gnrc_cdisp(generic) : ADI__nullid;

/* Generic has defined C dispatch? */
    if ( _valid_q(disp) )
      res = ((ADIcGenericDispatchCB) _eprc_prc(disp) )( prc->prc, narg, args, status );
    else                                /* Use default arg passing */
      res = ((ADIcMethodCB) prc->prc )( narg, args, status );
    }

/* Otherwise Fortran */
  else {

/* Locate generic Fortran dispatch function */
    disp = _valid_q(generic) ? _gnrc_fdisp(generic) : ADI__nullid;

/* Generic has defined Fortran dispatch? */
    if ( _valid_q(disp) )
      res = ((ADIfGenericDispatchCB) _eprc_prc(disp) )( &prc->prc, &narg, args, status );
    else                                /* Use default arg passing */
      ((ADIfMethodCB) prc->prc )( &narg, args, &res, status );
    }

/* Return the result */
  return res;
  }


ADIobj adix_execi( ADIobj func, int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj        gen;                    /* Generic function id */
  ADIobj        rval = ADI__nullid;     /* The returned value */

/* Check status on entry */
  _chk_init_err; _chk_stat_ret(ADI__nullid);

/* Locate the generic function */
  gen = adix_locgen( func, narg, status );

/* First potential error is that the generic does not exist */
  if ( _null_q(gen) )
    adix_rep_nomth( func, narg, args, status );

/* Invoke the method combination specified in the generic record */
  else {

/* Locate method combinator */
    ADIobj      mcf = _gnrc_mcomb(gen);

    rval = (*((ADIcMethodCombinationCB) _eprc_prc(_mco_cexec(mcf))))( gen, narg, args, status );
    }

  return rval;                          /* Set return value */
  }


ADIobj adix_exec( char *func, int flen, int narg,
		  ADIobj args[], ADIstatus status )
  {
  ADIobj        fname;                  /* The function name in the table */

  if ( narg )
    _chk_init_err;
  else
    _chk_init;

  _chk_stat_ret(ADI__nullid);

/* Locate in common string table */
  fname = adix_cmn( func, flen, status );

/* and execute the method */
  return adix_execi( fname, narg, args, status );
  }


ADIobj adix_exec2( char *func, int flen, ADIobj arg1, ADIobj arg2,
		   ADIstatus status )
  {
  ADIobj	args[2];

  args[0] = arg1;
  args[1] = arg2;

  return adix_exec( func, flen, 2, args, status );
  }

ADIobj adix_execi2( ADIobj func, ADIobj arg1, ADIobj arg2,
		    ADIstatus status )
  {
  ADIobj	args[2];

  _chk_stat_ret(ADI__nullid);

  args[0] = arg1;
  args[1] = arg2;

  return adix_execi( func, 2, args, status );
  }


void adix_id_flush( char *grp, int glen, ADIstatus status )
  {
  ADIobjRequest	objreq;			/* Object specification */

  _chk_init_err; _chk_stat;

  adix_pl_find( ADI_G_grplist, &ADI_G_grplist, grp, glen, ADI__false,
		&objreq, status );

  if ( objreq.data && _ok(status) ) {
    }
  else
    adic_setecs( ADI__INVARG, "Invalid identifier group /%*s/",
	status, glen, grp );
  }

void adix_id_link( ADIobj id, char *grp, int glen, ADIstatus status )
  {
  ADIobjRequest	objreq;			/* Object specification */

  _chk_init_err; _chk_stat;

  adix_pl_find( ADI_G_grplist, &ADI_G_grplist, grp, glen, ADI__true,
		&objreq, status );

  if ( objreq.data && _ok(status) ) {
    }
  else
    adic_setecs( ADI__INVARG, "Invalid identifier group /%*s/",
	status, glen, grp );
  }

void ADIkrnlAddCommonStrings( ADIcstrTableEntry stable[], ADIstatus status )
  {
  ADIcstrTableEntry	*sit = stable;

  for( ; sit->string; sit++ )
    *(sit->saddr) = adix_cmnC( sit->string, status );
  }

void ADIkrnlAddMethods( ADImthdTableEntry mtable[], ADIstatus status )
  {
  ADImthdTableEntry	*mit = mtable;

  for( ; mit->name; mit++ )
    adic_defmth( mit->name, mit->cb, NULL, status );
  }

void ADIkrnlAddCmdPars( ADIcmdParTableEntry ctable[], ADIstatus status )
  {
  ADIcmdParTableEntry	*cit = ctable;

  for( ; cit->name; cit++ )
    adic_defcpa( cit->name, cit->cb, NULL, status );
  }

void ADIkrnlAddFuncs( ADIfuncTableEntry ftable[], ADIstatus status )
  {
  ADIfuncTableEntry	*fit = ftable;
  ADIobj		fdef;

  for( ; fit->spec; fit++ ) {
    adic_deffun( fit->spec, fit->exec, &fdef, status );
    if ( fit->flags & SA_Listable )
      adix_pl_seti( fdef, K_Listable, adix_clone( ADIcvTrue, status ), status );
    if ( fit->flags & SA_HoldAll )
      adix_pl_seti( fdef, K_HoldAll, adix_clone( ADIcvTrue, status ), status );
    if ( fit->flags & SA_HoldFirst )
      adix_pl_seti( fdef, K_HoldFirst, adix_clone( ADIcvTrue, status ), status );
    if ( fit->flags & SA_HoldRest )
      adix_pl_seti( fdef, K_HoldRest, adix_clone( ADIcvTrue, status ), status );
    }
  }


void ADIkrnlAddGenerics( ADIgnrcTableEntry gtable[], ADIstatus status )
  {
  ADIgnrcTableEntry	*git = gtable;
  ADIobj		gid;

  for( ; git->spec; git++ ) {

/* Define generic and C dispatch */
    adic_defgen( git->spec, "", git->cdisp, &gid, status );

/* Fortran dispatch defined? Note we must use the internal routine */
/* here to fool the system into realising the callback is really a */
/* Fortran routine, and not a C one */
    if ( git->fdisp )
      adix_defgdp( gid,
		   ADIkrnlNewEproc( ADI__false, (ADICB) git->fdisp, status ),
		   status );
    }
  }


/*
 * Execute a void function with a single ADIobj argument
 */
void ADIkrnlExecO( ADIobj rtn, ADIobj arg, ADIstatus status )
  {
  if ( _ok(status) ) {

/* C routine? */
    if ( _eprc_c(rtn) )
      ((ADIoCB) _eprc_prc(rtn))( arg, status );

/* Fortran routine */
    else
      ((ADIfoCB) _eprc_prc(rtn))( &arg, status );
    }
  }


/*
 * Execute a void function with two ADIobj arguments
 */
void ADIkrnlExecOO( ADIobj rtn, ADIobj arg1, ADIobj arg2, ADIstatus status )
  {
  if ( _ok(status) ) {

/* C routine? */
    if ( _eprc_c(rtn) )
      ((ADIooCB) _eprc_prc(rtn))( arg1, arg2, status );

/* Fortran routine */
    else
      ((ADIfooCB) _eprc_prc(rtn))( &arg1, &arg2, status );
    }
  }


void adix_defvar( char *name, int nlen, ADIlogical global, ADIobj value,
		  ADIstatus status )
  {
  _chk_init; _chk_stat;

/* Look for definition binding */
  adix_setvar( adix_cmn( name, nlen, status ), global, value, status );
  }


void adix_setvar( ADIobj name, ADIlogical global, ADIobj value,
		  ADIstatus status )
  {
  ADIobj	dbind;

/* Look for definition binding */
  dbind = ADIsymFind( name, 0, ADI__true, ADI__defn_sb, status );

/* If it exists, delete definition object from existing binding */
  if ( _valid_q(dbind) ) {
    ADIobj      *daddr = &_sbind_defn(dbind);
    adix_erase( daddr, status );
    *daddr = value;
    }

/* Otherwise create a new one */
  else
    ADIsymAddBind( name, 0,
                   ADIsbindNew( ADI__defn_sb, value, status ),
                   status );
  }
