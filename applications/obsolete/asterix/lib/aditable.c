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
  ADIobj        *heads = _tbl_hash(args[1]);
  ADIinteger    i;

  _chk_stat;

  for( i=_tbl_htsize(args[1]); i>0; i--, heads++ )
    if ( ! _null_q(*heads) )
      adix_print( args[0], *heads, 1, ADI__true, status );
  }


void tblx_init( ADIstatus status )
  {
  _chk_stat;                            /* Check status on entry */

  adic_defcls( "HashTable", "", "htsize,head", &UT_ALLOC_tbl, status );

  adic_defcac( UT_ALLOC_tbl, 8, status );

  adic_defprt( UT_ALLOC_tbl, (ADIcMethodCB) tblx_prnt, status );
  }



ADIobj ECItableIterate( ADIobj	table,
			ADIobj 	(*iterator)(ADIobj,ADIobj,ADIstatus),
			ADIobj	iarg, ADIstatus status )
  {
  ADIinteger	i;		/* Loop over hash heads */
  ADIobj        *chead;         /* Head of has list */
  ADIobj	cptr;		/* Used to pass over list */
  ADIobj	robj = ADI__nullid;	/* Returned value */
  ADIobj	rval;		/* Value returned from iterator */

  if ( _ok(status) ) {
    for ( i=_tbl_htsize(table), chead = _tbl_hash(table); i>0; i--, chead++ )
      {
      cptr = *chead;
      while ( cptr ) {
	rval = (*iterator)( _CAR(cptr), iarg, status );
	if ( rval )
	  robj = lstx_cell( rval, robj, status );
	cptr = _CDR(cptr);
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

/* Current name */
    cstr = _CAAR(**sptr);

/* Compare the two names */
    test = strx_cmpc( str, slen, cstr );

/* Terminate if match found */
    if ( test == 0 )
      found = ADI__true;

/* Next list element if query name precedes current one alphabetically */
    else if ( test > 0 )
      *sptr = &_CDR(**sptr);
    }

  return found;
  }


ADIlogical tblx_scani( ADIobj *head, ADIobj str, ADIobj **sptr,
		       ADIstatus status )
  {
  ADIobj        cstr;                   /* Current dataobj name */
  ADIlogical	found = ADI__false;	/* Return value */
  ADIstring	*sdat = _str_data(str);	/* String data block */
  int           test = 1;               /* String comparison */

/* Check inherited status */
  _chk_stat_ret(ADI__false);

/* Default return values */
  *sptr = head;

/* Loop while end of list not reached and name not found */
/* and not past name in alphabet */
  while ( _valid_q(**sptr) && ! found && (test>=0) ) {

/* Current name */
    cstr = _CAAR(**sptr);

/* Compare the two names - fast check for equality */
    if ( cstr == str )
      found = ADI__true;

/* Otherwise brute force comparison */
    else {
      test = strx_cmpc( sdat->data, sdat->len, cstr );

/* Terminate if match found */
      if ( test == 0 )
	found = ADI__true;

/* Next list element if query name precedes current one alphabetically */
      else if ( test > 0 )
	*sptr = &_CDR(**sptr);
      }
    }

  return found;
  }


ADIobj tblx_new( int size, ADIstatus status )
  {
  ADIobj       table = ADI__nullid;     /* New table */

  if ( !_ok(status) )			/* Check status */
    return ADI__nullid;

/* Create new table object */
  table = adix_cls_alloc( _cdef_data(UT_ALLOC_tbl), status );

  if ( _ok(status) ) {			/* Check status */
    adic_newv0i( size, &_tbl_htsize(table), status );
    adic_new1( "*", size, &_tbl_head(table), status );
    }

  return table;
  }



/*  tblx_sadd - Add entry to table if not already present
 */
ADIobj tblx_sadd( ADIobj *table, char *str, int slen,
		  ADIobj dataobj, ADIstatus status )
  {
  ADIobj        *lentry;                /* List insertion point */
  ADIobj	rval;			/* The associated data object */
  int           hcode;                  /* Table hashing code */
  ADIobj	*head;			/* Head of dataobj list */
  ADIobj        hnode;                  /* New element for table hash list */
  int           hval;                   /* String hash value */
  ADIinteger    lhcode = 0;
  ADIobj        nstr;                   /* Newly created ADI string */

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  if ( _tbl_q(*table) )	{		/* Table or a-list? */
    adic_get0i( _tbl_htsize(*table), &lhcode, status );
    hcode = (int) lhcode;

/* Find string's hash value */
    strx_hash( str, slen, hcode, &hval, status );

    head = _tbl_hash(*table) + hval;
    }
  else
    head = table;

/* Look along list for string */
  if ( tblx_scan( head, str, slen, &lentry, status ) )
    rval = _CAR(*lentry);
  else {

/* Create new string */
    adic_newv0c_n( str, slen, &nstr, status );

    hnode = lstx_cell(                  /* The new hash node */
		lstx_cell( nstr, dataobj, status ),
		*lentry, status );

    *lentry = hnode;                    /* Patch node into list */

    rval = _CAR(hnode);                 /* Return the dotted pair object */
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
  int           hcode;
  ADIobj        hnode;                  /* New element for table hash list */
  int           hval;                   /* String hash value */
  ADIobj        nstr;
  ADIinteger    lhcode;                  /* Table hashing code */

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

/* Table or a-list? */
  if ( _tbl_q(*table) )	{
    adic_get0i( _tbl_htsize(*table), &lhcode, status );

    hcode = (int) lhcode;

/* Find string's hash value */
    strx_hash( str, slen, hcode, &hval, status );

    head = _tbl_hash(*table) + hval;
    }
  else
    head = table;

/* Look along list for string */
  if ( tblx_scan( head, str, slen, &lentry, status ) )
    adic_setecs( ADI__SYMDEF, "Symbol /%*s/ is already in the table",
		 status, slen, str );
  else {
    adic_newv0c_n( str, slen, &nstr, status );

/* The new hash node */
    hnode = lstx_cell(
		lstx_cell( nstr, dataobj, status ),
		*lentry, status );

    *lentry = hnode;                    /* Patch node into list */

    rstr = _CAR(hnode);                 /* Return the dotted pair address */
    }

  return rstr;
  }


ADIobj tblx_find( ADIobj *table, char *str, int slen, ADIstatus status )
  {
  int           hcode;
  ADIobj	*head;			/* Head of list */
  int           hval;                   /* String hash value */
  ADIobj        *lentry;                /* List insertion point */
  ADIinteger    lhcode;                  /* Table hashing code */

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  if ( _tbl_q(*table) )	{		/* Table or a-list? */
    adic_get0i( _tbl_htsize(*table), &lhcode, status );
    hcode = (int) lhcode;

/* Find string's hash value */
    strx_hash( str, slen, hcode, &hval, status );

    head = _tbl_hash(*table) + hval;
    }
  else
    head = table;

/* Look along list for string */
  if ( tblx_scan( head, str, slen, &lentry, status ) )
    return *lentry;
  else
    return ADI__nullid;
  }


ADIobj tblx_findi( ADIobj *table, ADIobj str, ADIstatus status )
  {
  int           hcode;
  ADIobj	*head;			/* Head of list */
  int           hval;                   /* String hash value */
  ADIobj        *lentry;                /* List insertion point */
  ADIinteger    lhcode;                  /* Table hashing code */
  ADIstring	*sdat = _str_data(str);

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  if ( _tbl_q(*table) )	{		/* Table or a-list? */
    adic_get0i( _tbl_htsize(*table), &lhcode, status );
    hcode = (int) lhcode;

/* Find string's hash value */
    strx_hash( sdat->data, sdat->len, hcode, &hval, status );

    head = _tbl_hash(*table) + hval;
    }
  else
    head = table;

/* Look along list for string */
  if ( tblx_scani( head, str, &lentry, status ) )
    return *lentry;
  else
    return ADI__nullid;
  }

