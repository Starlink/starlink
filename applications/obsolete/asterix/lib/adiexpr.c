/*
 *
 * Symbols consist of
 *
 *	1) a name
 *	2) a collection of bindings
 *	3) attributes
 *
 * Attributes are shared across all bindings. The name is implicit in the
 * fact that symbols are user level objects (hence can use the name field
 * in the object handle) - attributes are stored as properties which means
 * the bindings can just be stored as a list. So, a "symbol" is just a list.
 *
 *
 */
#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>

#include "asterix.h"                    /* Asterix definitions */
#include "ems.h"

#include "adi_err.h"
#include "aditypes.h"
#include "adimem.h"
#include "adiarray.h"
#include "adisyms.h"
#include "adilist.h"
#include "adikrnl.h"                    /* Internal ADI kernel */
#include "adimem.h"                     /* Allocation routines */
#include "adicface.h"
#include "adiexpr.h"                    /* Prototypes for this sub-package */
#include "adiparse.h"
#include "aditable.h"

/*
 * The class definition object
 */
ADIclassDef	*cdef_etn;
ADIobj	UT_cid_etn = ADI__nullid;


_DEF_STATIC_CDEF("_SymbolBinding",sbind,128,ADIsymBinding);

/*
 * Declare the EOS value stack and associated useful macros
 */
#define 	vs_push(obj)	(*vs_top++ = (obj))
#define		vs_pop		(*--vs_top)
#define		vs_head		vs_top[-1]
#define 	vs_mark		object *old_vs_top = vs_top
#define		vs_reset	vs_top = old_vs_top;

#define		vs_check	if ( vs_top >= vs_limit) \
				  EOS_ERR( status )

#define 	MAXARG		512
#define         VSSIZE		2048


char		argary[MAXARG];
ADIclassDef	*argcls[MAXARG];
ADIobj		ADI_G_vs[VSSIZE];

#define		vs_org		ADI_G_vs;

ADIobj		*vs_limit;		/* Value stack limit */
ADIobj		*vs_base;		/* Value stack base */
ADIobj		*vs_top;		/* Value stakc top */

int		ss_top;			/* Top of the slot stack */

#define         FSSIZE		256

unsigned char   slots[FSSIZE*2];	/* The slot stack */

ADIstackFrame	ADI_G_fs[FSSIZE];		/* The frame stack */

ADIstackFrame	*fs_top;		/* Frame stack top */


void ADIexprPopFS( ADIstatus status )
  {
  if ( _valid_q(fs_top->syms) )
    adix_erase( &fs_top->syms, 1, status );

  ss_top -= fs_top->nslot;
  fs_top--;
  }

void ADIexprPushFS( int tslot, ADIobj func, ADIstatus status )
  {
  if ( (fs_top - ADI_G_fs + 1) < FSSIZE ) {

/* Go to next stack frame */
    fs_top++;

/* Is this the base frame? */
    if ( fs_top == ADI_G_fs ) {
      fs_top->nslot = 0;

/* Initialise object stack */
      vs_top = vs_base = ADI_G_vs;

/* Initialise slot stack */
      ss_top = 0;
      }

/* Otherwise increment the slot stack pointer */
    else
      ss_top += fs_top->nslot;

/* Define the symbol table if one is required */
    if ( tslot )
      fs_top->syms = tblx_new( tslot, status );
    else
      fs_top->syms = ADI__nullid;

/* Store the invoking function */
    fs_top->func = func;

/* Set the new current value stack position */
    fs_top->top = vs_top;
    }
  else
    adic_setecs( ADI__OUTMEM, "ADI has overflowed its frame stack", status );
  }



/*
 * Raise an ADI exception with an ADI name, a status code and error message
 */
void ADIexecRaiseInt( ADIobj except, ADIstatype code,
		      char *message, int mlen, va_list ap, ADIstatus status )
  {
  char		buf[200];
  int		used;

/* Store exception name and status code */
  ADI_G_curint->exec.name = except;
  ADI_G_curint->exec.code = (code == SAI__OK) ? ADI__UNWIND : code;

/* Import message string */
  _GET_STRING(message,mlen);

/* Load any error text supplied */
  if ( mlen ) {
    ADIstrmCBprintf( buf, 200, message, ap, &used, status );

    adic_newv0c_n( buf, used, &ADI_G_curint->exec.errtext, status );
    }
  else
    ADI_G_curint->exec.errtext = ADI__nullid;

/* Clear unwind stack */
  ADI_G_curint->exec.stack = ADI__nullid;

/* Mark status for unwinding */
  *status = ADI__UNWIND;
  }


/*
 * Raise an ADI exception with an ADI name, a status code and error message
 */
void ADIexecRaiseI( ADIobj except, ADIstatype code,
		    char *message, int mlen, ADIstatus status, ... )
  {
  va_list	ap;

  va_start(ap,status);

  ADIexecRaiseInt( adix_clone( except, status ), code, message, mlen, ap, status );

  va_end(ap);
  }


/*
 * Raise an ADI exception with native name, a status code and error message
 */
void ADIexecRaise( char *exname, int exlen, ADIstatype code, char *message,
		   int mlen, ADIstatus status, ... )
  {
  va_list	ap;
  ADIobj	except;

/* Look up exception name in common table */
  except = adix_cmn( exname, exlen, status );

  va_start(ap,status);

  ADIexecRaiseInt( except, code, message, mlen, ap, status );

  va_end(ap);
  }


void ADIexecReset( ADIstatus status )
  {
  if ( *status == ADI__UNWIND )
    *status = SAI__OK;

  if ( _valid_q(ADI_G_curint->exec.errtext) )
    adix_erase( &ADI_G_curint->exec.errtext, 1, status );

  adix_erase( &ADI_G_curint->exec.name, 1, status );

  ADI_G_curint->exec.name = ADI__nullid;
  }


void ADIexecAcceptI( ADIobj except, ADIstatus status )
  {
  if ( ADI_G_curint->exec.name == except )
    ADIexecReset( status );
  }

void ADIexecAccept( char *except, int elen, ADIstatus status )
  {
  ADIobj	name;

  name = adix_cmn( except, elen, status );

  if ( ADI_G_curint->exec.name == name )
    ADIexecReset( status );
  }


ADIclassDef *ADIexprArgClass( int iarg, ADIobj arg, ADIstatus status )
  {
  ADIclassDef	*acdef;
  char		*data;
  ADIobj	hid;

/* Not array by default */
  argary[iarg] = 0;

/* Valid argument? */
  if ( _valid_q(arg) ) {

/* Get argument class */
    data = adix_iddt( arg, &acdef );
    if ( acdef == &KT_DEFN_han ) {
      hid = ((ADIobjHan *) data)->id;
      acdef = _ID_TYPE(hid);
      if ( acdef == &KT_DEFN_ary ) {
	argary[iarg] = 1;
	argcls[iarg] = acdef = _ID_TYPE(_ary_data(hid)->data);
	}
      else
	argcls[iarg] = acdef;
      }
    else
      argcls[iarg] = acdef;
    }
  else
    argcls[iarg] = acdef = NULL;

  return acdef;
  }


/*
 *  Test a method/function definition against values on the stack. Method
 *  arguments positions define slots - each slot may map on to zero or more
 *  values on the stack. If the binding matches the arguments on the stack
 *  the location of the first stack value, and the number of subsequent
 *  values is stored for each slot. If arguments have names then entries in
 *  the local symbol table are made.
 */
ADIlogical ADIexprTestBind( ADIobj sbind, int narg, ADIobj args[],
                            ADIstatus status )
  {
  ADIobj	arg;			/* Current defined args */
  ADIobj	carg;			/* Cursor over defined args */
  ADIobj	*cur_sp = args;
  ADIinteger	dims[ADI__MXDIM];
  ADIobj	earg;
  ADIobj	head;
  int		iarg;
  ADIlogical	match = ADI__true;
  ADIclassDef   *margc;
  ADIlogical	matcharray;
  ADIobj	mclass,patobj,parg,phead;
  ADIlogical	mmode;			/* Method (or function) mode */
  ADImethod	*mthd;
  int		ndim;
  int		nleft = narg;	        /* Number of values to process */
  int		nslot = 0;
  int		nmin, nmax;		/* Allowed slot value count */
  ADIlogical	ok;
  int		sacnt;			/* Count of stacked values in slot */
  enum {m_pattern, m_compare} mode;

  _chk_stat_ret(ADI__false);

/* Locate the method data */
  mthd = _mthd_data(sbind);

/* Cursor over defined arguments */
  carg = mthd->args;

/* Method mode? In function mode argument matching is exact - derived */
/* class instances are not valid arguments to functions with specified */
/* argument classes */
  mmode = _valid_q( mthd->form );

/* Test no argument case. This is a bit different because the null argument case */
/* is represented as the special null cons cell */
  if ( (carg == ADIcvNulCons) && ! nleft )
    carg = ADI__nullid;

/* For each defined slot, find matching stacked arguments */
  for( iarg=0; _valid_q(carg) && match && nleft; iarg++ ) {

/* Get current arg definition and advance cursor */
    _GET_CARCDR( arg, carg, carg );

/* Initialise for this slot */
    sacnt = 0;
    margc = NULL;

/* Argument spec is an expression node? */
    if ( _etn_q(arg) ) {

/* Test for the various Blank* options */
      _GET_HEDARG( head, earg, arg );
      mode = m_pattern;
      if ( head == K_Blank ) {
	nmin = nmax = 1;
	}
      else if ( head == K_BlankSeq ) {
	nmin = 1;
	nmax = MAXARG;
	}
      else if ( head == K_BlankNullSeq ) {
	nmin = 0;
	nmax = MAXARG;
	}
      else
	adic_setecs( ADI__INVARG, "Invalid argument specification /%O/", status, arg );
      }

/* Argument spec is not an expression tree */
    else {
      nmin = nmax = 1;
      mode = m_compare;
      }

    if ( _ok(status) ) {

      if ( mode == m_pattern ) {

/* Not array by default */
	matcharray = ADI__false;

/* Locate match class */
	patobj = _CAR(earg);

/* Null pattern object should mean global all-containing class */
	if ( _null_q(patobj) )
	  mclass = K_WildCard;

/* Expression structured pattern object */
	else if ( _etn_q(patobj) ) {

/* Get bits of pattern expression */
	  _GET_HEDARG( phead, parg, patobj );

/* Is it an array specifier? */
	  if ( phead == K_ArrayRef ) {

	    ADIobj	nxtarg,dobj;

	    matcharray = ADI__true;
	    ndim = -1;

/* First element of list is type name. If subsequent arguments are present */
/* they specify the dimensions of the array. Both the dimensionality and *
/* dimensions have a special value of -1 meaning "not specified" */
	    _GET_CARCDR( mclass, nxtarg, parg );

	    mclass = _etn_head(mclass);

	    while ( _valid_q(nxtarg) ) {
	      if ( ndim == -1 )
		ndim = 1;
	      else
		ndim++;
	      _GET_CARCDR( dobj, nxtarg, nxtarg );
	      if ( _valid_q(dobj) )
		adic_get0i( dobj, &dims[ndim-1], status );
	      else
		dims[ndim-1] = -1;
	      }
	    }
	  else
	    mclass = phead;
	  }

	else
	  ADIstrmPrintf( "Comp %O and %O\n",status,patobj,argcls[iarg]->aname);
	}

/* Count matching stack values up to a limit of nmax */
      ok = ADI__true;
      while ( (sacnt < nmax) && ok && nleft ) {

	if ( mode == m_compare )
	  ok = adix_equal( arg, *cur_sp, status );

/* Need to check dimensions here too */
	else if ( mclass == argcls[iarg]->aname ) {
	  ok = ( (matcharray  ? 1 : 0) == argary[iarg]);
	  }

	else if ( mclass == K_Symbol ) {
	  ok = _etn_q(*cur_sp);
	  if ( ok ) {
	    ok = _null_q(_etn_args(*cur_sp));
	    }
	  }

	else if ( mclass == K_WildCard )
	  ;

	else if ( mmode ) {

	  if ( ! margc )
	    margc = adix_loccls( mclass, status );

/* Method arg class must exist in the inheritance list of the user arg */
	  ok = adix_chkder( argcls[iarg], margc, status );
	  }

/* Otherwise there is no argument match */
	else
	  ok = ADI__false;

	if ( ok ) {
	  sacnt++;
	  cur_sp++;
	  nleft--;
	  }

	}

/* Number of values matches requirement? */
      match = ((sacnt>=nmin) && (sacnt<=nmax));

/* If good match store the slot details */
      if ( match ) {
	slots[ss_top+iarg] = sacnt;
	}
      }

    nslot++;
    }

/* Check for unused arguments */
  if ( nleft || _valid_q(carg) )
    match = ADI__false;
  else if ( _ok(status) )
    fs_top->nslot = nslot;

  return _ok(status) ? match : ADI__false;
  }

ADIobj ADIexprOwnArg( ADIobj *arg, ADIstatus status )
  {
  ADIobj	marg = ADI__nullid;

  if ( _valid_q(*arg) ) {
    if ( _han_ref(*arg) > 1 )
      marg = adix_copy( *arg, status );
    else {
      marg = *arg;
      *arg = ADI__nullid;
      }
    }

  return marg;
  }


ADIobj ADIexprMapFun( ADIobj head, ADIobj *first, ADIinteger llen,
		      int nlist, ADIstatus status )
  {
  int		ihead;
  int		ilist;			/* Loop over lists */
  ADIobj	*cursor = vs_top;
  ADIobj	*newargl = vs_top + nlist;/* Argument lists for new heads */
  ADIobj 	robj = ADI__nullid;	        /* Returned object */

  if ( _ok(status) ) {

/* Initialise cursors. Inputs may be List() or raw lists */
    for( ilist=0; ilist<nlist; ilist++)
      cursor[ilist] = _list_q(first[ilist]) ? first[ilist] : _etn_args(first[ilist]);

    for( ihead=0; ihead<llen; ihead++ )
      newargl[ihead] = ADI__nullid;

/* Construct new arg lists */
    ihead = 0;
    while( _valid_q(cursor[0]) && _ok(status) ) {

/* Extract next list elements*/
      for( ilist=0; ilist<nlist; ilist++) {
	ADIobj	car_c;

	_GET_CARCDR(car_c,cursor[ilist],cursor[ilist]);

	newargl[ihead] = lstx_cell( adix_clone( car_c, status ),
		newargl[ihead], status );
	}

/* Reverse list elements in situ */
      newargl[ihead] = lstx_revrsi( newargl[ihead], status );

/* Turn this into expression tree */
      newargl[ihead] = ADIetnNew( adix_clone(head,status), newargl[ihead], status );

      ihead++;				/* Next output sub-expression */
      }

/* Join all these sub-expressions into an argument list */
    for( ihead=llen-1; ihead>=0; ihead--)
      robj = lstx_cell( newargl[ihead], robj, status );
    }

  return robj;
  }


/*
 * Locate symbol binding(s) with specified name. If the 2nd argument is
 * true the first binding in the symbol search tree is taken, otherwise
 * a list is returned of all the collected bindings. This list should be
 * be destroyed using lstx_sperase
 */
ADIobj ADIsymFind( ADIobj name, int upvar, ADIlogical takefirst, int forms,
		   ADIstatus status )
  {
  ADIobj	cbind;
  ADIstackFrame	*curfs = fs_top;
  ADIlogical	found = ADI__false;
  ADIobj	nvpair;
  ADIobj	rval = ADI__nullid;
  ADIobj	*ipoint = &rval;
  ADIsymBinding *bind;

  if ( upvar < 0 )
    curfs = ADI_G_fs;
  else
    curfs = fs_top - upvar;

/*  while ( (curfs >= ADI_G_fs) && ! found ) { */

    nvpair = tblx_findi( &curfs->syms, name, status );
    if ( _valid_q(nvpair) ) {
      cbind = _CDR(nvpair);
      while ( _valid_q(cbind) && ! found ) {
	bind = _sbind_data(cbind);

	if ( bind->form & forms ) {
	  if ( takefirst ) {
	    rval = cbind;
	    found = ADI__true;
	    }
	  else
            lstx_inscel( cbind, &ipoint, status );
	  }

	cbind = bind->next;
	}
      }

/*    curfs--;
    } */

  return rval;
  }


ADIobj ADIsymAddBind( ADIobj name, int upvar, ADIobj bind, ADIstatus status )
  {
  ADIsymBinding		*bdata = _sbind_data(bind);
  ADIsymBinding		*cbdata;
  ADIstackFrame		*fs;
  ADIinteger		exclusive_forms = ADI__class_sb|ADI__mcf_sb;
  ADIobj		nvpair;

  if ( upvar < 0 )
    fs = ADI_G_fs;
  else
    fs = fs_top - upvar;

  nvpair = tblx_saddi( &fs->syms, name, ADI__nullid, status );

  if ( _ok(status) && _valid_q(nvpair) ) {
    ADIobj		*ipoint = &_CDR(nvpair);
    ADIsbindForm	iform = bdata->form;
    ADIlogical		ok = ADI__true;
    ADIobj		cbind = *ipoint;

/* Check if binding form demands that only one binding should exist */
    if ( _valid_q(cbind) && (iform & exclusive_forms) ) {
      while ( _valid_q(cbind) && ok ) {
	cbdata = _sbind_data(cbind);
	if ( iform == cbdata->form ) {
	  adic_setecs( ADI__SYMDEF,
		"Symbol already has a binding of requested form", status );
	  }
	else
	  cbind = cbdata->next;
	}
      }

/* Add binding to head of symbol binding list */
    if ( ok ) {
      bdata->next = *ipoint;
      *ipoint = bind;
      }
    }

  return nvpair;
  }


ADIobj ADIsbindNew( ADIsbindForm form, ADIobj data, ADIstatus status )
  {
  ADIsymBinding	bind;

  bind.form = form;
  bind.defn = data;
  bind.next = ADI__nullid;

/* Allocate the new binding object */
  return adix_cls_nallocd( &KT_DEFN_sbind, 0, 0, &bind, status );
  }


/*
 * Construct a new expression tree node
 */
ADIobj ADIetnNew( ADIobj head, ADIobj args, ADIstatus status )
  {
  ADIobj	idata[2];

  idata[0] = head;
  idata[1] = args;

  return adix_cls_nallocd( cdef_etn, 0, 0, idata, status );
  }

void ADIetnPrint( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj          arglink;
  ADIobj	car;
  ADIobj	head;

  if ( _valid_q(args[1]) ) {
    _GET_HEDARG( head, arglink, args[1] );

    adix_print( args[0], head, 1, ADI__true, status );

    if ( _valid_q(arglink) ) {
      ADIstrmFprintf( args[0], "(", status );
      if ( arglink != ADIcvNulCons ) {
	while ( _valid_q(arglink) && _ok(status) ) {
	  _GET_CARCDR( car, arglink, arglink );
	  adix_print( args[0], car, 1, ADI__true, status );
	  if ( _valid_q(arglink) )
	    ADIstrmFprintf( args[0], ", ", status );
	  }
	}
      ADIstrmFprintf( args[0], ")", status );
      }
    }
  }


ADIobj ADIexprEvalMapList( ADIobj head, int narg, ADIobj args[],
			   ADIinteger llen, ADIobj symlist, int level,
			   ADIstatus status )
  {
  ADIlogical	cachanged;
  ADIobj	robj = ADI__nullid;
  ADIobj	*rcar,*rcdr,curp,newval;

  _chk_stat_ret(ADI__nullid);

  robj = curp = ADIexprMapFun( head, args, llen, narg, status );

  while ( _valid_q(curp) ) {
    _GET_CARCDR_A(rcar,rcdr,curp);

    newval = ADIexprEvalInt( *rcar, symlist, level+1, &cachanged, status );

    if ( cachanged ) {
      adix_erase( rcar, 1, status );
      *rcar = newval;
      }

    curp = *rcdr;
    }

  return robj;
  }


/* The expression evaluator.
 *
 *  Step 1 :
 *
 *    Evaluate the head of the expression
 *
 *
 */
ADIobj ADIexprEvalInt( ADIobj expr, ADIobj symlist, int level, ADIlogical *changed,
			ADIstatus status )
  {
  ADIobj                a_bind;         /* Loop over binding list */
  ADIclassDef		*acdef;		/* Argument class definition */
  ADIlogical               all_list;
  ADIlogical               anyargeval;     /* Any arguments changed? */
  ADIlogical               argchanged;
  ADIsymBinding		*bind;
  ADIlogical               onstack = ADI__false;/* Arguments already on stack? */
  ADIobj                carg;           /* Loop over arguments */
  ADIinteger            cllen;          /* Length of list argument */
  ADIobj		earg;
  ADIobj		ae_head,ae_args;
  ADIlogical            defer_error = ADI__false;
  ADIlogical            finished = ADI__false;
  ADIlogical	    	hasargs;
  ADIlogical	    	hasprops;
  ADIobj		head;
  ADIobj		*hpobj;		/* Header property list address */
  int			iarg;
  ADIlogical		is_etn;
  int                   iter = 0;       /* Iterations through eval loop */
  ADIinteger            llen;           /* Length of list argument */
  ADIlogical            match;          /* Binding match found? */
  ADIobj                newval;         /* New argument value */
  unsigned int		*raddr;
  ADIobj                robj;           /* Result object */
  ADIobj                s_bind;
  ADIobj                *old_vs_base,*old_vs_top;
  ADIlogical		this_a_list;
  ADIobj                *vs_first, *vs_last;
  ADIobj                *vs_ptr;        /* Loop over virtual stack */
  ADIlogical       	wereonstack;

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

/* Slot storage for next exec */
  (fs_top+1)->first_slot = ss_top;

/* While expression reduction still taking place */
  while ( (! finished) && _valid_q(expr) && _ok(status) ) {

/* Zero next frame's slot counter */
    (fs_top+1)->nslot = 0;

/* Default is no change */
    *changed = ADI__false;

/* Extract expression head and args if that kind of object */
    if ( _etn_q(expr) ) {
      is_etn = ADI__true;
      _GET_HEDARG( head, earg, expr );
      }
    else
      is_etn = ADI__false;

/* Structured expression? */
    if ( is_etn && _valid_q(earg) ) {

/* Locate property list */
      hpobj = &_han_pl(head);
      hasprops = _valid_q(*hpobj);

      wereonstack = onstack;

/* Stack already initialised? */
      if ( onstack )
	onstack = ADI__false;

/* Set up stack */
      else {
	old_vs_base = vs_base;
	old_vs_top = vs_top;
	vs_base = vs_top;

/* Stack the unevaluated arguments */
	if ( earg == ADIcvNulCons )
	  hasargs = ADI__false;
	else {
	  hasargs = ADI__true;
	  carg = earg;
	  while ( _valid_q(carg) && _ok(status) ) {
	    ADIobj	car;

	    _GET_CARCDR( car, carg, carg );
	    vs_push( car );
	    if ( _valid_q(car) )
	      _han_ref(car)++;
	    }
	  }
	}

/* Default bounds for arg evaluation */
      vs_first = vs_base;
      vs_last = vs_top-1;

/* No arguments evaluated so far */
      anyargeval = ADI__false;

/* Gather the list of bindings of type Function */
      s_bind = ADIsymFind( head, -1, ADI__false, ADI__fun_sb, status );

      if ( _valid_q(s_bind) && hasargs && hasprops ) {

/* HoldAll arguments? */
	if ( _valid_q(adix_pl_fgeti(hpobj, K_HoldAll, status )) )
	  vs_first = vs_last + 1;

	else {

/* HoldFirst argument? */
	  if ( _valid_q(adix_pl_fgeti( hpobj, K_HoldFirst, status )) )
	    vs_first++;

/* HoldRest arguments? */
	  if ( _valid_q(adix_pl_fgeti( hpobj, K_HoldRest, status )) )
	    vs_last = vs_first;
	  }
	}

/* Evaluate arguments 1 at a time */
      for( vs_ptr = vs_first; vs_ptr <= vs_last; vs_ptr++ ) {

	if ( _etn_q(*vs_ptr) ) {

/* Evaluate stacked item */
	  newval = ADIexprEvalInt( *vs_ptr, symlist, level+1, &argchanged, status );

/* Did the value change? */
	  if ( argchanged ) {

/* Replace stacked object with result of expression evaluation */
	    adix_erase( vs_ptr, 1, status );
	    *vs_ptr = newval;

/* Mark arg list as changed */
	    anyargeval = ADI__true;
	    }
	  }
	}

/* Initialise the counter which denotes the next argument whose class is to be */
/* ascertained. By storing that information during the Listable test we don't  */
/* waste it when testing function symbol bindings */
      iarg = 0;

/* Listable form? */
      if ( hasargs && hasprops ) {

/* Are all the arguments lists? */
	all_list = ADI__true;
	_ARGLOOP_1ST_TO_NTH_AND(vs_ptr,all_list) {

	  this_a_list = ADI__false;
	  if ( _valid_q(*vs_ptr) ) {

/* Get argument class */
            acdef = ADIexprArgClass( iarg, *vs_ptr, status );

/* Argument is a raw list */
	    if ( acdef->selfid == UT_cid_list ) {
	      cllen = lstx_len( *vs_ptr, status );
	      if ( iarg )
		this_a_list = (llen == cllen);
	      else {
		this_a_list = ADI__true;
		llen = cllen;
		}
	      }

/* Argument is a list constructor */
	    else if ( acdef == cdef_etn ) {
	      _GET_HEDARG( ae_head, ae_args, *vs_ptr );
	      if ( ae_head == K_List ) {
		cllen = lstx_len( ae_args, status );
		if ( iarg )
		  this_a_list = (llen == cllen);
		else {
		  this_a_list = ADI__true;
		  llen = cllen;
		  }
		}
	      }
	    }
	  else
	    argcls[iarg] = NULL;

	  iarg++;

	  all_list &= this_a_list;
	  }
	}
      else
	all_list = ADI__false;

/* Map over lists? */
      if ( all_list ) {
	robj = ADIexprEvalMapList( head, vs_top-vs_base, vs_base,
			   llen, symlist, level, status );

	*changed = ADI__true;
	}

      else {

	ADIobj	car_ab,cdr_ab;

	if ( _valid_q(s_bind) ) {

/* Put argument classes into argcls */
	  for( vs_ptr = vs_base + iarg; vs_ptr <= vs_last; vs_ptr++, iarg++ )
            ADIexprArgClass( iarg, *vs_ptr, status );

/* Find a matching symbol definition? */
	  match = ADI__false;
	  a_bind = s_bind;
	  while ( _valid_q(a_bind) && ! match ) {

	    _GET_CARCDR( car_ab, cdr_ab, a_bind );
	    bind = _sbind_data(car_ab);
	    match = ADIexprTestBind( bind->defn, _NARG, vs_base, status );
	    if ( ! match )
	      a_bind = cdr_ab;
	    }
	  }
	else
	  match = ADI__false;

	if ( match ) {                   /* Invoke function if match */
	  a_bind = car_ab;
	  if ( bind->form == ADI__trnsfm_sb )
	    robj = adix_copy( bind->defn, status );
	  else {

	    robj = adix_exemth( ADI__nullid, _mthd_exec(bind->defn), _NARG,
				vs_base, status );

	    if ( ADI_G_curint->exec.name == EXC_ReturnValue ) {
	      robj = ADI_G_curint->exec.errtext;
	      ADI_G_curint->exec.errtext = ADI__nullid;
	      ADIexecAcceptI( EXC_ReturnValue, status );
	      }

	    if ( !_ok(status) ) {       /* Bad status from procedure? */
	      defer_error = ADI__true;
	      finished = ADI__true;
	      ems_begin_c( status );
	      }
	    }

	  *changed = ADI__true;              /* Result is always a change */
	  }

/* No args evaluated & no match means we're finished here */
	else if ( ! anyargeval ) {

	  finished = ADI__true;

/* Args were on stack on loop entry means o/p object needs creating */
	  if ( wereonstack ) {
	    ADIobj	alist = ADI__nullid;

/* Ensure we own all the values on the stack. Construct argument list */
/* for the output expression */
	    _ARGLOOP_NTH_TO_1ST(vs_ptr)
	      alist = lstx_cell( ADIexprOwnArg( vs_ptr, status ),
				 alist, status );

/* Copy of head node */
	    robj = ADIetnNew( adix_clone( head, status ), alist, status );

	    *changed = ADI__true;
	    }
	  else
	    robj = adix_clone( expr, status );
	  }
	else                            /* Args changed but no match */
	  {
	  *changed = ADI__true;              /* Args evaluated means change */
	  onstack = ADI__true;               /* Leave them on stack for next time */
	  anyargeval = ADI__false;           /* All dynamic ones used up */
	  robj = expr;
	  }
	}

/* Release stacked items. Input arguments must have a ref count of at least 2 */
/* Evaluated arguments which are being discarded will have a count of only one */
      if ( ! onstack ) {
	for( vs_ptr = vs_base; vs_ptr <= vs_last; vs_ptr++ )
	  if ( _valid_q(*vs_ptr) ) {
	    raddr = &_han_ref(*vs_ptr);
	    if ( *raddr > 1 )
	      (*raddr)--;
	    else
	      adix_erase( vs_ptr, 1, status );
	    }
	  }

/* Remove binding list if defined */
      if ( _valid_q(s_bind) )
	lstx_sperase( &s_bind, status );

      if ( finished || ! onstack ) {
	vs_base = old_vs_base;          /* Restore stack */
	vs_top = old_vs_top;
	}
      }

/* Non-functional expression tree */
    else if ( is_etn ) {

      s_bind = ADIsymFind( head, 0, ADI__true, ADI__defn_sb|ADI__trnsfm_sb, status );

/* Did we match the symbol? */
      if ( _valid_q(s_bind) ) {

/* Access the symbol binding value */
	robj = adix_clone( _sbind_defn(s_bind), status );

	*changed = ADI__true;
	}
      else
	robj = expr;

      finished = ADI__true;
      }

/* Internal data forms. Simply return the object and finish */
    else {
      robj = expr;
      if ( ! iter )
	_han_ref(robj)++;
      finished = ADI__true;
      }

    iter++;                             /* Bump up iteration counter */

    if ( ! finished )                   /* Another round of evaluation? */
      expr = robj;
    }

  *changed |= (iter>1);                 /* Always changed after looping */

  if ( defer_error )                    /* Did we defer error reporting? */
    ems_end_c( status );

  return robj;
  }


ADIobj ADIexprEval( ADIobj expr, ADIobj symlist, ADIlogical ownres, ADIstatus status )
  {
  ADIlogical	changed;
  ADIobj	res;
  ADIobj	rval;

  ADI_G_curint->exec.name = ADI__nullid;

  res = rval = ADIexprEvalInt( expr, symlist, 0, &changed, status );

  if ( ownres && _valid_q(res) ) {
    if ( _han_ref(res) > 1 ) {
      rval = adix_copy( res, status );
      adix_erase( &res, 1, status );
      }
    }

  return rval;
  }


/*
 *  Evaluate list of expressions, returning value of last one evaluated
 */
ADIobj ADIexprEvalList( ADIobj elist, ADIobj symlist, ADIstatus status )
  {
  ADIobj	car,curp;
  ADIobj	robj = ADI__nullid;

  curp = elist;
  while ( _valid_q(curp) && _ok(status) ) {
    _GET_CARCDR(car,curp,curp);

    if ( _valid_q(car) )
      robj = ADIexprEval( car, symlist, ADI__false, status );
    else
      robj = ADI__nullid;

    if ( _valid_q(curp) && _valid_q(robj) )
      adix_erase( &robj, 1, status );
    }

  return robj;
  }


void adix_rep_nofun( ADIobj name, int narg, ADIobj args[],
		     ADIstatus status )
  {
  adix_rep_nomf( ADI__NOMTH, "functions", name, narg, args, status );
  }


ADIobj adix_fexec( char *func, int flen, int narg, ADIobj args[],
		   ADIstatus status )
  {
  ADIobj                a_bind;         /* Loop over binding list */
  ADIclassDef		*acdef;		/* Argument class definition */
  ADIlogical               all_list;
  ADIsymBinding		*bind;
  ADIinteger            cllen;          /* Length of list argument */
  ADIobj		ae_head,ae_args;
  ADIlogical            defer_error = ADI__false;
  ADIlogical	    	hasprops;
  ADIobj		head;
  ADIobj		*hpobj;		/* Header property list address */
  int			iarg;
  ADIinteger            llen;           /* Length of list argument */
  ADIlogical            match;          /* Binding match found? */
  unsigned int		*raddr;
  ADIobj                robj;           /* Result object */
  ADIobj                s_bind;
  ADIobj                *old_vs_base,*old_vs_top;
  ADIlogical		this_a_list;
  ADIobj                *vs_last;
  ADIobj                *vs_ptr;        /* Loop over virtual stack */

/* Check inherited global status */
  if ( !_ok(status) )
    return ADI__nullid;

/* Locate function in common table */
  head = adix_cmn( func, flen, status );

/* Gather the list of bindings of type Function */
  s_bind = ADIsymFind( head, -1, ADI__false, ADI__fun_sb, status );

/* Slot storage for next exec */
  (fs_top+1)->first_slot = ss_top;

/* Zero next frame's slot counter */
  (fs_top+1)->nslot = 0;

/* Set up stack */
  old_vs_base = vs_base;
  old_vs_top = vs_top;
  vs_base = vs_top;

/* Stack the unevaluated arguments */
  for( iarg=0; iarg<narg; iarg ++ ) {
    vs_push( args[iarg] );
    if ( _valid_q(args[iarg]) )
      _han_ref( args[iarg] )++;
    }

/* Default bounds for arg evaluation */
  vs_last = vs_top-1;

/* Initialise the counter which denotes the next argument whose class is to be */
/* ascertained. By storing that information during the Listable test we don't  */
/* waste it when testing function symbol bindings */
  iarg = 0;

/* Locate property list */
  hpobj = &_han_pl(head);
  hasprops = _valid_q(*hpobj);

/* Listable form? */
  if ( hasprops ) {

/* Are all the arguments lists? */
    all_list = ADI__true;
    _ARGLOOP_1ST_TO_NTH_AND(vs_ptr,all_list) {

      this_a_list = ADI__false;
      if ( _valid_q(*vs_ptr) ) {

/* Get argument class */
        acdef = ADIexprArgClass( iarg, *vs_ptr, status );

/* Argument is a raw list */
        if ( acdef->selfid == UT_cid_list ) {
	  cllen = lstx_len( *vs_ptr, status );
	  if ( iarg )
	    this_a_list = (llen == cllen);
	  else {
	    this_a_list = ADI__true;
	    llen = cllen;
	    }
	  }

/* Argument is a list constructor */
        else if ( acdef == cdef_etn ) {
	  _GET_HEDARG( ae_head, ae_args, *vs_ptr );
	  if ( ae_head == K_List ) {
	    cllen = lstx_len( ae_args, status );
	    if ( iarg )
	      this_a_list = (llen == cllen);
	    else {
	      this_a_list = ADI__true;
	      llen = cllen;
	      }
	    }
	  }
        }
      else
        argcls[iarg] = NULL;

      iarg++;

      all_list &= this_a_list;
      }
    }
  else
    all_list = ADI__false;

/* Map over lists? */
  if ( all_list ) {
    robj = ADIexprEvalMapList( head, vs_top-vs_base, vs_base,
				llen, ADI__nullid, 1, status );
    }

  else {

    ADIobj	car_ab,cdr_ab;

    if ( _valid_q(s_bind) ) {

/* Put argument classes into argcls */
      for( vs_ptr = vs_base + iarg; vs_ptr <= vs_last; vs_ptr++, iarg++ )
        ADIexprArgClass( iarg, *vs_ptr, status );

/* Find a matching symbol definition? */
      match = ADI__false;
      a_bind = s_bind;
      while ( _valid_q(a_bind) && ! match ) {

        _GET_CARCDR( car_ab, cdr_ab, a_bind );
	bind = _sbind_data(car_ab);
	match = ADIexprTestBind( bind->defn, _NARG, vs_base, status );
	if ( ! match )
	  a_bind = cdr_ab;
	}
      }
    else
      match = ADI__false;

    if ( match ) {                   /* Invoke function if match */
      a_bind = car_ab;
      if ( bind->form == ADI__trnsfm_sb )
	robj = adix_copy( bind->defn, status );
      else {

	robj = adix_exemth( ADI__nullid, _mthd_exec(bind->defn), _NARG,
				vs_base, status );

	if ( ADI_G_curint->exec.name == EXC_ReturnValue ) {
	  robj = ADI_G_curint->exec.errtext;
	  ADI_G_curint->exec.errtext = ADI__nullid;
	  ADIexecAcceptI( EXC_ReturnValue, status );
	  }

	if ( !_ok(status) ) {       /* Bad status from procedure? */
	  defer_error = ADI__true;
	  }
	}
      }

/* No match is an error in this routine */
    else {
      adix_rep_nofun( head, narg, args, status );
      defer_error = ADI__true;
      }

    if ( defer_error )
      ems_begin_c( status );
    }

/* Release stacked items. Input arguments must have a ref count of at least 2 */
/* Evaluated arguments which are being discarded will have a count of only one */
  for( vs_ptr = vs_base; vs_ptr <= vs_last; vs_ptr++ )
    if ( _valid_q(*vs_ptr) ) {
      raddr = &_han_ref(*vs_ptr);
      if ( *raddr > 1 )
	(*raddr)--;
      else
	adix_erase( vs_ptr, 1, status );
    }

/* Remove binding list if defined */
  if ( _valid_q(s_bind) )
    lstx_sperase( &s_bind, status );

/* Restore stack */
  vs_base = old_vs_base;
  vs_top = old_vs_top;

  if ( defer_error )                    /* Did we defer error reporting? */
    ems_end_c( status );

  return robj;
  }


void ADIetnInit( ADIstatus status )
  {
/* Define the expression node class */
  adic_defcls( "Expression", "", "head,args", &UT_cid_etn, status );

  cdef_etn = _cdef_data(UT_cid_etn);

/* Define the printer */
  adic_defprt( UT_cid_etn, (ADIcMethodCB) ADIetnPrint, status );

/* Initialise frame stack */
  fs_top = ADI_G_fs - 1;
  }


ADIobj ADIexprParseString( char *string, int slen, ADIobj grammar,
			   ADIstatus status )
  {
  ADItokenType	ctok;
  ADIobj	pstream;
  ADIobj	robj;

  _chk_init; _chk_stat_ret(ADI__nullid);

/* Construct a stream from the string supplied */
  pstream = ADIstrmExtendC( ADIstrmNew( "r", status ), string, slen, status );
  ADInextToken( pstream, status );

/* Parse an expression */
  robj = ADIparseExpInt( pstream, grammar, 1, status );

/* Look at last token before we destory the stream */
  ctok = ADIcurrentToken( pstream, status );

/* Destroy the stream */
  adic_erase( &pstream, status );

/* Error unless end of stream met */
  if ( ! ((ctok == TOK__END) || (ctok==TOK__NULL)) )
    adic_setecs( ADI__INVARG, "Extra text following valid expression", status );

  return robj;
  }
