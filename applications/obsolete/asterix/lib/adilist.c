/* ADI list routines
 *
 *  Internal :
 *
 *   lstx_first         - retrieve 'car' cell
 *   lstx_len           - length of list
 *   lstx_nth		- insertion point for N'th item
 *   lstx_rest          - retrieve 'cdr' cell
 *   lstx_cell          - allocate new list cell
 *   lstx_new2          - create a 2 element list
 *
 *  Exported :
 *
 *   Description                                C               Fortran
 *
 *   Create 2 element list from existing        ADInewList2     ADI_NEWLIST2
 *     objects.
 *
 *  Authors :
 *
 *   David J. Allan (JET-X, University of Birmingham)
 *
 *  History :
 *
 *   23 Jul 94 (DJA):
 *     Original version.
 */

#include <stdio.h>
#include <stdarg.h>

#include "asterix.h"                    /* Asterix definitions */

#include "aditypes.h"
#include "adimem.h"                     /* Allocation routines */
#include "adikrnl.h"                    /* Kernel code */
#include "adicface.h"                   /* C programmer interface */
#include "adilist.h"                    /* Prototypes for this sub-package */
#include "adiparse.h"
#include "adiconv.h"
#include "adierror.h"                   /* ADI error handling */

ADIclassDef	*cdef_lst;
ADIobj       UT_cid_list;             /* _List object allocator */

ADIinteger ADIostrmLst( ADIstream *stream, ADIobj id, char *data, int wmode,
			ADIstatus status )
  {
  ADIlist	*lst = (ADIlist *) data;
  ADIinteger	len;

  len = adix_ostrm_i( stream, lst->car, wmode, status );
  len += adix_ostrm_i( stream, lst->cdr, wmode, status );

  return len;
  }

ADIobj ADIistrmLst( ADIstream *stream, ADIstatus status )
  {
  ADIlist	lst;

  lst.car = adix_istrm_i( stream, status );
  lst.cdr = adix_istrm_i( stream, status );

  return adix_cls_nallocd( cdef_lst, 0, 0, &lst, status );
  }

ADIobj lstx_print( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj        curp = args[1];
  ADIobj	stream = args[0];

  _chk_stat_ret(ADI__nullid);

  if ( _valid_q(curp) ) {
    ADIobj	car,cdr;

    _GET_CARCDR(car,cdr,curp);

    ADIstrmFprintf( stream, "{", status );
    if ( _null_q(cdr) ? 1 : _list_q(cdr) ) { /* This is a standard list? */
      do {
	_GET_CARCDR(car,curp,curp);

	ADIstrmFprintf( stream, "%O%s", status, car,
		_valid_q(curp) ? ", " : "" );
	}
      while ( _valid_q(curp) );
      }

/* It's a dotted pair */
    else
      ADIstrmFprintf( stream, "%O.%O", status, car, cdr );

    ADIstrmFprintf( stream, "}", status );
    }

  return ADI__nullid;
  }

void ADIlstCnv( ADImta *idd, int n, char *in, ADImta *odd, char *out,
		int *nerr, ADIstatus status )
  {
  int		i;
  ADIlist	*ilst = (ADIlist *) in;
  ADIlist	*olst = (ADIlist *) out;

  for( i=n; i; i--, ilst++, olst++ ) {
    olst->car = adix_copy( ilst->car, status );
    olst->cdr = adix_copy( ilst->cdr, status );
    }
  }

void lstx_init( ADIstatus status )
  {
  DEFINE_PTYPE_TABLE(ptable)
    PTYPE_TENTRY( "List", ADIlist, &UT_cid_list, lstx_print, ADIistrmLst, ADIostrmLst ),
  END_PTYPE_TABLE;

  _chk_stat;                            /* Check status on entry */

/* Install table type */
  ADIkrnlAddPtypes( ptable, status );

  cdef_lst = _cdef_data(UT_cid_list);

  ADIcnvNew( cdef_lst, cdef_lst, ADIlstCnv, status );
  }


ADIobj *lstx_nth( ADIobj list, ADIinteger n, ADIstatus status )
  {
  ADIobj	curp = list;
  ADIinteger	i = n;

  _chk_stat_ret(NULL);

  while ( (i > 1) && ! _null_q(curp) ) {
    curp = _CDR(curp);
    i--;
    }

  return _null_q(curp) ? NULL : &_CAR(curp);
  }


ADIobj lstx_first( ADIobj list, ADIstatus status )
  {
  ADIobj    rval = ADI__nullid;

  if ( _ok(status) && (list!=ADI__nullid) )
    rval = _CAR(list);

  return rval;
  }

void lstx_push( ADIobj obj, ADIobj *list, ADIstatus status )
  {
  _chk_stat;

  *list = lstx_cell( obj, *list, status );
  }


/*
 *  In situ list reversal
 */
ADIobj lstx_revrsi( ADIobj list, ADIstatus status )
  {
  ADIobj        curp = list;
  ADIobj        last = ADI__nullid;

  if ( _ok(status) )
    {
    while ( _valid_q(curp) ) {
      ADIobj	*cdr = &_CDR(curp);
      ADIobj next = *cdr;
      *cdr = last;

      last = curp;
      curp = next;
      }
    }

  return last;
  }


ADIobj lstx_rest( ADIobj list, ADIstatus status )
  {
  ADIobj    rval = ADI__nullid;

  if ( _ok(status) && (list!=ADI__nullid) )
    rval = _CDR(list);

  return rval;
  }

ADIobj lstx_cell( ADIobj aid, ADIobj bid, ADIstatus status )
  {
  ADIlist	list;

/* Set up initialisation */
  list.car = aid;
  list.cdr = bid;

/* Create object with data */
  return adix_cls_nallocd( cdef_lst, 0, 0, &list, status );
  }

ADIobj lstx_new2( ADIobj aid, ADIobj bid, ADIstatus status )
  {
  return lstx_cell( aid,
                    lstx_cell( bid,
                               ADI__nullid,
                               status ),
                    status );
  }


int lstx_len( ADIobj lid, ADIstatus status )
  {
  int                   llen = 0;
  ADIobj                lptr = ADI__nullid;

  if ( !_ok(status) )                   /* Check status on entry */
    return ADI__nullid;

  _chk_type(lid,UT_cid_list);

  lptr = lid;
  while ( (lptr != ADI__nullid) && _ok(status) )
    {
    llen++;
    lptr = _CDR(lptr);
    }

  return llen;
  }


/* Append an item on to a list held at some address
 */
ADIobj lstx_append( ADIobj lst1, ADIobj lst2, ADIstatus status )
  {
  ADIobj		*curc = &lst1;	/* List insertion point */

  _chk_stat_ret(lst1);

  if ( _valid_q(lst1) )		        /* Non-empty list? */
    {
    do					/* Cursor over existing list */
      {
      curc = &_CDR(*curc);
      }
    while ( ! _null_q(*curc) );
    }

  *curc = lst2;                         /* Join second argument on */

  return lst1;                          /* Simply return first argument */
  }


/*
 * Create a list cell updating an object point to point to the address of
 * _CDR cell.
 */
void lstx_inscel( ADIobj id, ADIobj **ipoint, ADIstatus status )
  {
  ADIobj	lid;
  ADIlist	list;

  list.car = id;
  list.cdr = ADI__nullid;

  lid = adix_cls_nallocd( cdef_lst, 0, 0, &list, status );

  if ( _ok(status) ) {
    **ipoint = lid;
    *ipoint = &_CDR(lid);
    }
  }


/*
 * Special list erase where only list cells are deleted
 */
void lstx_sperase( ADIobj *list, ADIstatus status )
  {
  ADIobj 	curp = *list;
  ADIobj	*car,*cdr;
  ADIobj	last;

  while ( _valid_q(curp) ) {
    _GET_CARCDR_A(car,cdr,curp);

    last = curp;
    *car = ADI__nullid;
    curp = *cdr;
    *cdr = ADI__nullid;
    adix_erase( &last, 1, status );
    }

  *list = ADI__nullid;
  }

void lstx_addtoset( ADIobj *list, ADIobj obj, ADIstatus status )
  {
  ADIobj curp = *list;
  ADIobj *ipoint = list;
  ADIobj	test = (-1);

  if ( _ok(status) ) {
    while ( _valid_q(curp) && (test<0) ) {

      ADIobj	*car,*cdr;

      _GET_CARCDR_A(car,cdr,curp);

/* Compare identifiers */
      test = *car - obj;

/* Not there yet? If so, store insertion point in case next node is past */
/* the point where we'd insert "obj". If test > 0 then ipoint points to */
/* the cell insertion point from the last iteration if any, otherwise the
/* user's list variable */
      if ( test < 0 ) {
	ipoint = cdr;
	curp = *ipoint;
	}
      }

/* Add cell to list if not already present */
    if ( test != 0 )
      *ipoint = lstx_cell( obj, *ipoint, status );
    }
  }


/*
 *  Exported user C routines
 */
ADIobj ADInewList2( ADIobj aid, ADIobj bid, ADIstatus status )
  {
  return lstx_new2( aid, bid, status );
  }

ADIobj adix_mapcar1( ADIobj (*proc)(ADIobj,ADIstatus),
		     ADIobj (*join)(ADIobj,ADIobj,ADIstatus),
		     ADIobj lst, ADIstatus status )
  {
  ADIobj        curp = lst;
  ADIobj	elem;
  ADIobj        rval = ADI__nullid;

  _chk_stat_ret(ADI__nullid);

  while ( _valid_q(curp) ) {
    _GET_CARCDR(elem,curp,curp);

    rval = (*join)( rval, (*proc)( elem, status ), status );
    }

  return rval;
  }

ADIlogical adix_eql_p( ADIobj x, ADIobj y, ADIstatus status )
  {
  return (x==y) ? ADI__true : ADI__false;
  }

ADIlogical adix_member( ADIobj element, ADIobj list,
			ADIlogical (*test)(ADIobj,ADIobj,ADIstatus),
			ADIstatus status )
  {
  ADIobj        curp = list;
  ADIobj	elem;
  ADIlogical    (*ltest)(ADIobj,ADIobj,ADIstatus) = test;
  ADIlogical    rval = ADI__false;

  _chk_stat_ret(ADI__false);

  if ( ! ltest )
    ltest = adix_eql_p;

  while ( _valid_q(curp) && _ok(status) && ! rval ) {

    _GET_CARCDR(elem,curp,curp);

    if ( (*ltest)(element,elem,status) )
      rval = ADI__true;
    }

  return rval;
  }

ADIobj adix_assoc( ADIobj idx, ADIobj lst, ADIstatus status )
  {
  ADIobj curp = lst;
  ADIobj rval = ADI__nullid;

  if ( _ok(status) ) {
    while ( _valid_q(curp) ) {

      ADIobj	car;

      _GET_CARCDR(car,curp,curp);

      if ( idx == _CAR(car) ) {
	rval = car;
	break;
	}
      }
    }

  return rval;
  }

ADIobj adix_removeif( ADIlogical (*test)(ADIobj,ADIobj,ADIstatus),
		      ADIobj args, ADIobj lst,
		      ADIstatus status )
  {
  ADIobj	car;
  ADIobj        curp = lst;
  ADIlogical    (*ltest)(ADIobj,ADIobj,ADIstatus) = test;
  ADIobj        newlist = ADI__nullid;
  ADIobj        *ipoint = &newlist;

  _chk_stat_ret(ADI__false);

  if ( ! ltest )
    ltest = adix_eql_p;

  while ( _valid_q(curp) && _ok(status) ) {
    _GET_CARCDR(car,curp,curp);

    if ( ! (*ltest)(car,args,status) )
      lstx_inscel( adix_copy(car,status), &ipoint, status );
    }

  return newlist;
  }

/*
 *  Exported user Fortran routines
 */
#ifdef ADI_F77
F77_SUBROUTINE(adi_newlist2)( INTEGER(aid), INTEGER(bid), INTEGER(cid),
                              INTEGER(status) )
  {
  GENPTR_INTEGER(aid)
  GENPTR_INTEGER(bid)
  GENPTR_INTEGER(cid)
  GENPTR_INTEGER(status)

  *cid = lstx_new2( (ADIobj) *aid, (ADIobj) *bid, (ADIstatus) status );
  }
#endif
