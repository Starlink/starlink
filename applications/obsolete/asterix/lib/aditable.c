#include <stdio.h>
#include <stdarg.h>

#include "asterix.h"                    /* Asterix definitions */
#include "aditypes.h"
#include "adimem.h"                     /* Allocation routines */
#include "adikrnl.h"                    /* Kernel code */
#include "aditable.h"                   /* Prototypes for this sub-package */
#include "adilist.h"                    /* List manipulations */
#include "adistrng.h"                   /* String manipulations */
#include "adiparse.h"
#include "adicface.h"                   /* C interface */
#include "adierror.h"                   /* ADI error handling */


ADIobj UT_ALLOC_tbl = ADI__nullid;


void tblx_prnt( int narg, ADIobj args[], ADIstatus status )
  {
  ADItable	*tdata = _tbl_data(args[1]);
  ADIobj        *heads;
  ADIinteger    i;

  _chk_stat;

  for( i = tdata->htsize, heads = (ADIobj *) _DTDAT(tdata->heads); i>0; i--, heads++ )
    if ( ! _null_q(*heads) )
      adix_print( args[0], *heads, 1, ADI__true, status );
  }


void tblx_init( ADIstatus status )
  {
  DEFINE_PTYPE_TABLE(ptable)
    PTYPE_TENTRY("HashTable",   ADItable,    &UT_ALLOC_tbl, tblx_prnt ),
  END_PTYPE_TABLE;

  _chk_stat;                            /* Check status on entry */

/* Install table type */
  ADIkrnlAddPtypes( ptable, status );
  }



ADIobj tblx_iterate( ADIobj table,
		     ADIobj (*iterator)(ADIobj,ADIobj,ADIstatus),
		     ADIobj iarg, ADIstatus status )
  {
  ADIinteger	i;		/* Loop over hash heads */
  ADIobj        *chead;         /* Head of has list */
  ADIobj	cptr;		/* Used to pass over list */
  ADIobj	robj = ADI__nullid;	/* Returned value */
  ADIobj	rval;		/* Value returned from iterator */
  ADItable	*tdata = _tbl_data(table);

  if ( _ok(status) ) {
    for ( i=tdata->htsize, chead = (ADIobj *) _DTDAT(tdata->heads); i>0; i--, chead++ )
      {
      cptr = *chead;
      while ( cptr ) {
	ADIobj	car,cdr;

	_GET_CARCDR(car,cdr,cptr);

	rval = (*iterator)( car, iarg, status );
	if ( rval )
	  robj = lstx_cell( rval, robj, status );
	cptr = cdr;
	}
      }
    }

  return robj;
  }


/*
 * Scan a (name,value) list for a specified string. This procedure takes
 * a char * and a string length. If the name string is an ADI string then
 * the tblx_scani routine should be used as it offers efficiency gains.
 *
 *  head	- head of a (name,value) list
 *  str,slen	- the query name
 *  *sptr	- address of variable pointing the (name,value) pair if
 *		  found, otherwise the insertion point
 */
ADIlogical tblx_scan( ADIobj *head, char *str, int slen, ADIobj **sptr,
		      ADIstatus status )
  {
  ADIobj	*car,*cdr;		/* Sides of list cell */
  ADIobj        cstr;                   /* Current dataobj name */
  ADIlogical	found = ADI__false;	/* Return value */
  int           test = 1;               /* String comparison */

/* Check inherited status */
  _chk_stat_ret(ADI__false);

/* Default return values */
  *sptr = head;

/* Loop while end of list not reached and name not found */
/* and not past name in alphabet */
  while ( _valid_q(**sptr) && ! found && (test>=0) ) {

/* Extract links from this list cell */
    _GET_CARCDR_A(car,cdr,**sptr);

/* Current name */
    cstr = _CAR(*car);

/* Compare the two names */
    test = strx_cmpc( str, slen, cstr );

/* Terminate if match found */
    if ( test == 0 )
      found = ADI__true;

/* Next list element if query name precedes current one alphabetically */
    else if ( test > 0 )
      *sptr = cdr;
    }

  return found;
  }


ADIlogical tblx_scani( ADIobj *head, ADIobj str, ADIobj **sptr,
		       ADIobj **ecar, ADIstatus status )
  {
  ADIobj	*car,*cdr;		/* Sides of list cell */
  ADIobj        cstr;                   /* Current dataobj name */
  ADIlogical	found = ADI__false;	/* Return value */

/* Check inherited status */
  _chk_stat_ret(ADI__false);

/* Default return values */
  *sptr = head;

/* Loop while end of list not reached and name not found */
/* and not past name in alphabet */
  while ( _valid_q(**sptr) && ! found ) {

/* Extract links from this list cell */
    _GET_CARCDR_A(car,cdr,**sptr);

/* Current name */
    cstr = _CAR(*car);

/* Compare the two names - fast check for equality */
    if ( cstr == str ) {
      found = ADI__true;
      *ecar = car;
      }
    else
      *sptr = cdr;
    }

  return found;
  }


ADIobj *tblx_lochead( ADIobj *table, char *str, int slen, ADIstatus status )
  {
  ADIobj	*head = table;
  ADItable	*tdata;

  if ( _tbl_q(*table) )	{		/* Table or a-list? */
    tdata = _tbl_data(*table);

    head = (ADIobj *) adix_dtdat(tdata->heads) + strx_hash( str, slen, (int) tdata->htsize );
    }

  return head;
  }


ADIobj tblx_new( int size, ADIstatus status )
  {
  ADItable	tinit;

  if ( !_ok(status) )			/* Check status */
    return ADI__nullid;

  tinit.htsize = size;
  adic_new1( "*", size, &tinit.heads, status );

/* Create new table object */
  return adix_cls_nallocd( _cdef_data(UT_ALLOC_tbl), 0, 0, &tinit, status );
  }


/*
 * Add a cell to a hash table/association list
 */
ADIobj tblx_cell( ADIobj *lentry, ADIobj nstr, ADIobj dataobj,
		  ADIstatus status )
  {
  ADIobj	hnode;
  ADIobj	rval;

/* Create dotted pair */
  rval = lstx_cell( nstr, dataobj, status ),

/* The new hash node */
  hnode = lstx_cell( rval, *lentry, status );

/* Patch node into list */
  *lentry = hnode;

/* Return the dotted pair object */
  return rval;
  }


/*  tblx_sadd - Add entry to table if not already present
 */
ADIobj tblx_sadd( ADIobj *table, char *str, int slen,
		  ADIobj dataobj, ADIstatus status )
  {
  ADIobj        *lentry;                /* List insertion point */
  ADIobj	rval;			/* The associated data object */
  ADIobj	*head;			/* Head of dataobj list */
  ADIobj        nstr;                   /* Newly created ADI string */

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

/* Locate list head */
  head = tblx_lochead( table, str, slen, status );

/* Look along list for string */
  if ( tblx_scan( head, str, slen, &lentry, status ) )
    rval = _CAR(*lentry);
  else {

/* Create new string */
    adic_newv0c_n( str, slen, &nstr, status );

/* Create new entry and return dotted pair */
    rval = tblx_cell( lentry, nstr, dataobj, status );
    }

  return rval;
  }


/*  tblx_saddi - Add entry to table if not already present
 */
ADIobj tblx_saddi( ADIobj *table, ADIobj str,
		   ADIobj dataobj, ADIstatus status )
  {
  ADIobj        *lentry;                /* List insertion point */
  ADIobj	*lcar;			/* &_CAR(*lentry) if found */
  ADIobj	rval;			/* The associated data object */
  ADIobj	*head;			/* Head of dataobj list */
  ADIstring	*sdat = _str_data(str);

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

/* Locate list head */
  head = tblx_lochead( table, sdat->data, sdat->len, status );

/* Look along list for string */
  if ( tblx_scani( head, str, &lentry, &lcar, status ) )
    rval = *lcar;
  else {

/* Create new entry and return dotted pair */
    rval = tblx_cell( lentry, adix_clone( str, status ), dataobj, status );
    }

  return rval;
  }


/*  tblx_add - Add entry to table, report error if already present
 */
ADIobj tblx_add( ADIobj *table, char *str, int slen,
		 ADIobj dataobj, ADIstatus status )
  {
  ADIobj        *lentry;                /* List insertion point */
  ADIobj	rstr = ADI__nullid;	/* The string in the table */

  ADIobj	*head;			/* Head of dataobj list */
  ADIobj        nstr;

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

/* Locate list head */
  head = tblx_lochead( table, str, slen, status );

/* Look along list for string */
  if ( tblx_scan( head, str, slen, &lentry, status ) )
    adic_setecs( ADI__SYMDEF, "Symbol /%*s/ is already in the table",
		 status, slen, str );
  else {
    adic_newv0c_n( str, slen, &nstr, status );

/* Create new entry and return dotted pair */
    rstr = tblx_cell( lentry, nstr, dataobj, status );
    }

  return rstr;
  }


ADIobj tblx_find( ADIobj *table, char *str, int slen, ADIstatus status )
  {
  ADIobj	*head;			/* Head of list */
  ADIobj        *lentry;                /* List insertion point */

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

/* Locate list head */
  head = tblx_lochead( table, str, slen, status );

/* Look along list for string */
  if ( tblx_scan( head, str, slen, &lentry, status ) )
    return _CAR(*lentry);
  else
    return ADI__nullid;
  }


ADIobj tblx_findi( ADIobj *table, ADIobj str, ADIstatus status )
  {
  ADIobj	*head;			/* Head of list */
  ADIobj	*lcar; 			/* &_CAR(*lentry) if found */
  ADIobj        *lentry;                /* List insertion point */
  ADIstring	*sdat = _str_data(str);

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

/* Locate list head */
  head = tblx_lochead( table, sdat->data, sdat->len, status );

/* Look along list for string */
  if ( tblx_scani( head, str, &lentry, &lcar, status ) )
    return *lcar;
  else
    return ADI__nullid;
  }


void tblx_hstats( ADIobj table, ADIstatus status )
  {
  ADIobj	*baddr;
  ADIinteger	bcount[11] = {0,0,0,0,0,0,0,0,0,0,0};
  ADIinteger	big = 0;
  ADIinteger	i;
  ADIinteger	llen;
  ADIinteger	total = 0;
  ADItable	*tdata = _tbl_data(table);

  _chk_init_err; _chk_stat;

/* Loop over buckets, accumulating counts */
  for( baddr = (ADIobj *) _DTDAT(tdata->heads), i = tdata->htsize; i; i--, baddr++ ) {

/* Find length of chain */
    if ( _valid_q(*baddr) )
      llen = lstx_len( *baddr, status );
    else
      llen = 0;

/* Add this count */
    total += llen;
    if ( llen > 10 )
      big++;
    else
      bcount[llen]++;
    }

/* Print results */
  for( i=0; i<11; i++ )
    if ( bcount[i] )
      ADIstrmPrintf( "Buckets with %I entries (%I)\n", status, i, bcount[i] );
  ADIstrmPrintf( "Buckets with > 10 entries (%I)\n\n", status, big );
  ADIstrmPrintf( "Average search depth %f\n", status,
	((float) total)/((float) (tdata->htsize-bcount[0])) );
  ADIstrmFlush( status );
  }
