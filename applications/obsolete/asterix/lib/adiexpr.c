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
ADIobj	UT_ALLOC_etn = ADI__nullid;


ADIobj	ADI_G_syms = ADI__nullid;

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

#define         VSSIZE		2048

ADIobj		ADI_G_vs[VSSIZE];

#define		vs_org		ADI_G_vs;

ADIobj		*vs_limit;		/* Value stack limit */
ADIobj		*vs_base;		/* Value stack base */
ADIobj		*vs_top;		/* Value stakc top */
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

int		ss_top;			/* Top of the slot stack */

#define         FSSIZE		256

unsigned char   slots[FSSIZE*2];	/* The slot stack */

ADIstackFrame	ADI_G_fs[FSSIZE];		/* The frame stack */

ADIstackFrame	*fs_top;		/* Frame stack top */

#define		fs_push(afunc)	 (++fs_top)->func = (afunc);  \
				 {ss_top += fs_top->nslot; \
				 fs_top->top = vs_top;}

#define         fs_pop 		 {ss_top -= fs_top->nslot;\
				 (fs_top--);fs_top->exec=fs_top[1].exec;}



ADIobj ADIexprMapFun( ADIobj head, ADIobj *first,
		      int nlist, ADIstatus status )
  {
  int		ihead;
  int		ilist;			/* Loop over lists */
  ADIobj	*cursor = vs_top;
  ADIinteger	llen;			/* List length */
  ADIobj	*newargl = vs_top+nlist;/* Argument lists for new heads */
  ADIobj 	robj = ADI__nullid;	        /* Returned object */

  if ( _ok(status) ) {

/* Initialise cursors */
    for( ilist=0; ilist<nlist; ilist++)
      cursor[ilist] = first[ilist];

/* Length of lists */
    llen = lstx_len( *first, status );

    for( ihead=0; ihead<llen; ihead++ )
      newargl[ihead] = ADI__nullid;

/* Construct new arg lists */
    ihead = 0;
    while( _valid_q(cursor[0]) && _ok(status) ) {

/* Extract next list elements*/
      for( ilist=0; ilist<nlist; ilist++) {

	newargl[ihead] = lstx_cell(
		adix_copy( _CAR(cursor[ilist]), status ),
		newargl[ihead], status );

	cursor[ilist] = _CDR(cursor[ilist]);
	}

/* Reverse list elements in situ */
      newargl[ihead] = lstx_revrsi( newargl[ihead], status );

      newargl[ihead] =			/* Turn this into expression tree */
	ADIetnNew( adix_copy(head,status),
	    newargl[ihead], status );

      ihead++;				/* Next output sub-expression */
      }

/* Join all these sub-expressions into an argument list */
    for( ihead=llen-1; ihead>=0; ihead--)
      robj = lstx_cell( newargl[ihead], robj, status );

/* The top-level expression tree node */
    robj = lstx_cell( adix_clone( K_List, status ), robj, status );
    }

  return robj;
  }


ADIobj ADIsymFind( ADIobj name, ADIstatus status )
  {
  ADIobj	nvpair;

  nvpair = tblx_findi( &ADI_G_syms, name, status );

  return _valid_q(nvpair) ? _CDR(nvpair) : ADI__nullid;
  }

ADIobj ADIsymBinds( ADIobj symbol, int forms, ADIstatus status )
  {
  ADIobj	curp = symbol;
  ADIobj	slist;
  ADIobj	*ipoint = &slist;

  _chk_stat_ret(ADI__nullid);

  while ( curp && _ok(status) ) {
    ADIobj	bind = _CAR(curp);
    ADIsymBinding	*bdata = _sbind_data(bind);

    if ( bdata->form & forms ) {
      ADIobj ncell = lstx_cell( bind,ADI__nullid, status );
      *ipoint = ncell;
      ipoint = &_CDR(ncell);
      }

    curp = _CDR(curp);
    }

  return slist;
  }


/*
 * Construct a new expression tree node
 */
ADIobj ADIetnNew( ADIobj head, ADIobj args, ADIstatus status )
  {
  ADIobj    id = ADI__nullid;

  if ( _ok(status) ) {
    id = adix_cls_alloc( _cdef_data(UT_ALLOC_etn), status );

    if ( _ok(status) ) {
      _etn_head(id) = head; _etn_args(id) = args;
      }
    }

  return id;
  }


void ADIetnPrint( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj          arglink;

  if ( _valid_q(args[1]) ) {
    adix_print( args[0], _etn_head(args[1]), 1, ADI__true, status );

    arglink = _etn_args(args[1]);
    if ( arglink ) {
      ADIstrmPrintf( args[0], "(", status );
      while ( _valid_q(arglink) && _ok(status) ) {
	adix_print( args[0], _CAR(arglink), 1, ADI__true, status );
	arglink = _CDR(arglink);
	if ( _valid_q(arglink) )
	  ADIstrmPrintf( args[0], ", ", status );
	}
      ADIstrmPrintf( args[0], ")", status );
      }
    }
  }


/*
 * Test args on stack against binding
 */
ADIlogical ADIsymTestBind( ADIobj bind, ADIstatus status )
  {
  return _ok(status) ? _valid_q(bind) : ADI__false;
  }


/* The expression evaluator.
 *
 *  Step 1 :
 *
 *    Evaluate the head of the expression
 *
 *
 */
ADIobj ADIexprEvalInt( ADIobj expr, int level, ADIlogical *changed,
			ADIstatus status )
  {
  ADIobj                a_bind;         /* Loop over binding list */
  ADIobj                alist;          /* Argument list */
  ADIlogical               all_list;
  ADIlogical               anyargeval;     /* Any arguments changed? */
  ADIlogical               argchanged;
  ADIlogical               onstack = ADI__false;/* Arguments already on stack? */
  ADIobj                carg;           /* Loop over arguments */
  ADIlogical               defer_error=ADI__false;
  ADIlogical               finished = ADI__false;
  int                   iter = 0;       /* Iterations through eval loop */
  ADIobj                last_head = ADI__nullid;
  ADIinteger              llen;           /* Length of list argument */
  ADIlogical               match;          /* Binding match found? */
  ADIobj                newval;         /* New argument value */
  ADIobj                robj;           /* Result object */
  ADIobj                s_bind;
  ADIobj                symbol;
  ADIobj                *old_vs_base,*old_vs_top;
  ADIobj                *vs_first, *vs_last;
  ADIobj                *vs_ptr;        /* Loop over virtual stack */
  ADIlogical       	wereonstack;

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  (fs_top+1)->first_slot = ss_top;	/* Slot storage for next exec */

/* While expression reduction still taking place */
  while ( (! finished) && _valid_q(expr) && _ok(status) ) {

/* Zero next frame's slot counter */
    (fs_top+1)->nslot = 0;

/* Default is no change */
    *changed = ADI__false;

/* Structured expression? */
    if ( _etn_q(expr) && _valid_q(_etn_args(expr)) ) {

/* Expression head has changed? If so, look up symbol bindings */
      if ( last_head != _etn_head(expr) ) {
	symbol = ADIsymFind( _etn_head(expr), status );
	last_head = _etn_head(expr);
	}

      wereonstack = onstack;
      if ( onstack )                    /* Stack already initialised? */
	{
	onstack = ADI__false;
	}
      else
	{
	old_vs_base = vs_base;          /* Set up stack */
	old_vs_top = vs_top;
	vs_base = vs_top;

	carg = _etn_args(expr);          /* Stack the unevaluated arguments */
	while ( _valid_q(carg) && _ok(status) )
	  {
	  vs_push( _CAR(carg) );
	  carg = _CDR(carg);
	  }
	}

/* Default bounds for arg evaluation */
      vs_first = vs_base;
      vs_last = vs_top-1;

/* No arguments evaluated so far */
      anyargeval = ADI__false;

/* Was a symbol found inthe symbol table? */
      if ( _valid_q(symbol) ) {

/* Gather the list of bindings of type Transform or Definition */
	s_bind = ADIsymBinds( symbol, SBIND__trnsfm|SBIND__defn, status );

	if ( _valid_q(s_bind) ) {

/* HoldAll arguments? */
	  if ( _valid_q(adix_pl_geti(symbol, K_HoldAll, status )) )
	    vs_first = vs_last + 1;

	  else {

/* HoldFirst argument? */
	    if ( _valid_q(adix_pl_geti( symbol, K_HoldFirst, status )) )
	      vs_first++;

/* HoldRest arguments? */
	    if ( _valid_q(adix_pl_geti( symbol, K_HoldRest, status )) )
	      vs_last = vs_first;
	    }
	  }
	}

/* Evaluate arguments 1 at a time */
      for( vs_ptr = vs_first; vs_ptr <= vs_last; vs_ptr++ ) {

	if ( _etn_q(*vs_ptr) ) {

/* Evaluate stacked item */
	  newval = ADIexprEvalInt( *vs_ptr, level+1, &argchanged, status );

	  if ( argchanged )             /* Did the value change? */
	    {
	    adix_erase( vs_ptr, 1, status );

	    *vs_ptr = newval;           /* Replace stacked object */

	    anyargeval = ADI__true;          /* Mark arg list as changed */
	    }
	  }
	}

      if ( _valid_q(symbol) )                     /* Listable form? */
	{
	if ( _valid_q(_etn_args(expr)) )
	  {
	  all_list = ADI__true;              /* Are all the arguments lists? */
	  _ARGLOOP_1ST_TO_NTH_AND(vs_ptr,all_list)
	  if ( *vs_ptr )
	    all_list = (all_list & (_list_q(*vs_ptr)));
	  else
	    all_list = ADI__false;
	  }
	else
	  all_list = ADI__false;

/*      We delay the test for "Listable" until this point because most
 *      functions are listable - we are more likely to reject this test
 *      because the function arguments are not lists, in which case
 *      <all_list> would be ADI__false */
	if ( all_list ) {

/* Listable function? */
	  if ( _valid_q(adix_pl_geti(symbol, K_Listable, status )) ) {

/* Get length of first list */
	    llen = lstx_len( *vs_base, status );

	    _ARGLOOP_2ND_TO_NTH_AND(vs_ptr,all_list)
		all_list = (lstx_len(*vs_ptr,status) == llen);
	    }
	  else
	    all_list = ADI__false;           /* Cancel list processing */
	  }
	}
      else
	all_list = ADI__false;

/* Map over lists? */
      if ( all_list ) {

	robj = ADIexprMapFun( _etn_head(expr),
		vs_base, vs_top-vs_base, status );

	*changed = ADI__true;
	}

      else {
	match = ADI__false;                  /* Find a matching symbol definition? */
	if ( _valid_q(symbol) ) {
	  a_bind = s_bind;
	  while ( _valid_q(a_bind) && ! match ) {
	    match = ADIsymTestBind( _sbind_defn(_CAR(a_bind)), status );
	    if ( ! match )
	      a_bind = _CDR(a_bind);
	    }
	  }

	if ( match )                    /* Invoke function if match */
	  {
	  a_bind = _CAR(a_bind);
	  if ( _sbind_form(a_bind) == SBIND__trnsfm )
	    robj = adix_copy( _sbind_value(a_bind), status );
	  else
	    {
            robj = adix_exemth( ADI__nullid, _sbind_value(a_bind), _NARG,
                                vs_first, status );

	    if ( !_ok(status) )         /* Bad status from procedure? */
	      {
	      defer_error = ADI__true;
	      finished = ADI__true;
	      ems_begin_c( status );
	      }
	    }

	  *changed = ADI__true;              /* Result is always a change */
	  }
	else if ( ! anyargeval )        /* No args evaluated & no match */
	  {                             /* means we're finished here */
	  finished = ADI__true;

	  if ( wereonstack )            /* Args were on stack on loop entry */
	    {                           /* means o/p object needs creating */
	    alist = ADI__nullid;
	    _ARGLOOP_NTH_TO_1ST(vs_ptr) /* Construct argument list */
	      {
/*	      if ( _MARK(*vs_ptr) & O_MARKED )
		{
		_MARK(*vs_ptr) &= O_MARKED_M;
		alist = lstx_cell( *vs_ptr, alist, status );
		}
	      else */
		alist = lstx_cell(
		    adix_copy( *vs_ptr, status ),
		    alist, status );
	      }

/* Copy of head node */
	    robj = ADIetnNew(
		lstx_cell( _etn_head(expr), ADI__nullid, status ),
		  alist, status );

	    *changed = ADI__true;
	    }
	  else
	    robj = expr;
	  }
	else                            /* Args changed but no match */
	  {
	  *changed = ADI__true;              /* Args evaluated means change */
	  onstack = ADI__true;               /* Leave them on stack for next time */
	  anyargeval = ADI__false;           /* All dynamic ones used up */
	  robj = expr;
	  }
	}

      if ( anyargeval )                 /* Free dynamic arguments */
	{
	_ARGLOOP_1ST_TO_NTH(vs_ptr)             /* Free argument list */
	    adix_erase( vs_ptr, 1, status );
	}

      if ( symbol )
	lstx_sperase( &s_bind, status );      /* Remove binding list */

      if ( finished || ! onstack )
	{
	vs_base = old_vs_base;          /* Restore stack */
	vs_top = old_vs_top;
	}
      }

    else if ( _etn_q(expr) )            /* Non-functional expression tree */
      {

/* Locate symbol if present */
      symbol = ADIsymFind( _etn_head(expr), status );

      if ( symbol ) {
	s_bind = ADIsymBinds( _etn_head(expr),
		      SBIND__defn|SBIND__trnsfm, status );

	if ( ! _valid_q(s_bind) )
	  s_bind = ADIsymBinds( _etn_head(expr), SBIND__enum, status );

/* Did we match the symbol? */
	if ( _valid_q(s_bind) ) {

/* Access the symbol binding value */
	  robj = adix_clone( _sbind_value(_CAR(s_bind)), status );

/* Remove symbol bindings */
	  adix_erase( &s_bind, 1, status );
	  *changed = ADI__true;
	  finished = ADI__true;
	  }
	else {
	  robj = expr;
	  finished = ADI__true;
	  }
	}
      else                                      /* Internal data forms */
	{
	robj = expr;                    /* Return unmatched identifier */
	finished = ADI__true;
	}
      }

    else                                /* Internal data forms */
      {
      robj = expr;                      /* Just return the object */
      finished = ADI__true;                  /* Always finish here */
      }

    iter++;                             /* Bump up iteration counter */

    if ( (iter>1) && *changed )         /* Free expression? */
      adix_erase( &expr, 1, status );

    if ( ! finished )                   /* Another round of evaluation? */
      expr = robj;
    }

  *changed |= (iter>1);                 /* Always changed after looping */

  if ( _ok(status) && robj &&           /* Object must be dynamic on */
       (level == 0) )                   /* return to base level */
    {
    if ( _han_ref(robj) > 1 )           /* Is object dynamic? */
      robj = adix_copy( robj, status );  /* Make dynamic copy */
    }

  if ( defer_error )                    /* Did we defer error reporting? */
    ems_end_c( status );

  return robj;
  }


void ADIetnInit( ADIstatus status )
  {
/* Define the expression node class */
  adic_defcls( "Expression", "", "head,args", &UT_ALLOC_etn, status );

/* Define the printer */
  adic_defprt( UT_ALLOC_etn, (ADIcMethodCB) ADIetnPrint, status );

/* Define the symbol table */
  ADI_G_syms = tblx_new( 201, status );

/* Initialise object stack */
  vs_base = ADI_G_vs;
  vs_top = vs_base;

/* Initialise frame stack */
  fs_top = ADI_G_fs;
  fs_top->top = vs_top;
  fs_top->func = NULL;

/* Initialise slot stack */
  ss_top = 0;
  }

