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

#include "asterix.h"                    /* Asterix definitions */

#include "aditypes.h"
#include "adimem.h"                     /* Allocation routines */
#include "adikrnl.h"                    /* Kernel code */
#include "adicface.h"                   /* C programmer interface */
#include "adilist.h"                    /* Prototypes for this sub-package */
#include "adierror.h"                   /* ADI error handling */


ADIobj       UT_ALLOC_list;             /* _List object allocator */

void lstx_print( ADIobj id, ADIstatus status )
  {
  ADIobj        curp = id;

  _chk_stat;

  if ( _valid_q(id) ) {
    ADIobj	cdr = _CDR(id);

    putchar( '{' );
    if ( _null_q(cdr) ? 1 : _list_q(cdr) ) { /* This is a standard list? */
      do {
	adix_print( _CAR(curp), ADI__true, status );

	curp = _CDR(curp);
	if ( _valid_q(curp) )
	  printf( ", " );
	}
      while ( _valid_q(curp) );
      }
    else {				/* It's a dotted pair */
      adix_print( _CAR(curp), ADI__true, status );
      putchar( '.' );
      adix_print( _CDR(curp), ADI__true, status );
      }
    putchar( '}' );
    }
  }


void lstx_init( ADIstatus status )
  {
  _chk_stat;                            /* Check status on entry */

  adic_defcls( "_List", "",
         "first,rest",
         &UT_ALLOC_list, status );

  adix_def_prnt( UT_ALLOC_list,
	 lstx_print, status );
  }


ADIobj *lstx_nth( ADIobj list, int n, ADIstatus status )
  {
  ADIobj	curp = list;
  int		i = n;

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
    while ( _valid_q(curp) )
      {
      ADIobj next = _CDR(curp);
      _CDR(curp) = last;

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
  ADIobj    lid;

  if ( !_ok(status) )                   /* Check status on entry */
    return ADI__nullid;

  lid = adix_cls_alloc( _cdef_ctrl(UT_ALLOC_list), 0, 0, status );

  if ( _ok(status) )
    {
    _CAR(lid) = aid; _CDR(lid) = bid;
    }

  return lid;
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

  _chk_type(lid,UT_ALLOC_list);

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
 * Special list erase where only list cells are deleted
 */
void lstx_sperase( ADIobj *list, ADIstatus status )
  {
  ADIobj curp = *list;

  while ( _valid_q(curp) ) {
    _CAR(curp) = ADI__nullid;
    curp = _CDR(curp);
    }

  adic_erase( list, status );
  }

void lstx_addtoset( ADIobj *list, ADIobj obj, ADIstatus status )
  {
  ADIobj curp = list;
  ADIobj *ipoint = list;
  ADIobj	test = (-1);

  if ( _ok(status) ) {
    while ( _valid_q(curp) && (test<0) ) {

/* Compare identifiers */
      test = _CAR(curp) - obj;

/* Not there yet? If so, store insertion point in case next node is past */
/* the point where we'd insert "obj". If test > 0 then ipoint points to */
/* the cell insertion point from the last iteration if any, otherwise the
/* user's list variable */
      if ( test < 0 ) {
        ipoint = _CDR(curp);
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
