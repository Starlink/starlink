#include <stdio.h>

#include "asterix.h"                    /* Asterix definitions */
#include "aditypes.h"
#include "adimem.h"                     /* Allocation routines */
#include "adikrnl.h"                    /* Kernel code */
#include "aditable.h"                   /* Prototypes for this sub-package */
#include "adilist.h"                    /* List manipulations */
#include "adistrng.h"                   /* String manipulations */
#include "adicface.h"                   /* C interface */

#include "adierror.h"                   /* ADI error handling */


ADIobj UT_ALLOC_tbl = ADI__nullid;


void tblx_prnt( ADIobj tbl, ADIstatus status )
  {
  ADIobj        *heads = _tbl_hash(tbl);
  ADIinteger    i;

  _chk_stat;

  for( i=_tbl_htsize(tbl); i>0; i--, heads++ )
    if ( ! _null_q(*heads) )
      {
      adic_print( *heads, status );
      }
  }


void tblx_init( ADIstatus status )
  {
  _chk_stat;                            /* Check status on entry */

  adic_defcls( "_HashTable", "",
               "htsize,flags,head",
               &UT_ALLOC_tbl, status );

  adix_def_prnt( UT_ALLOC_tbl, tblx_prnt, status );
  }



ADIobj ECItableIterate( ADIobj	table,
			ADIobj 	(*iterator)(ADIobj,ADIobj,ADIstatus),
			ADIobj	iarg,
			ADIstatus	status )
  {
  ADIinteger	i;		/* Loop over hash heads */
  ADIobj        *chead;         /* Head of has list */
  ADIobj	cptr;		/* Used to pass over list */
  ADIobj	robj = ADI__nullid;	/* Returned value */
  ADIobj	rval;		/* Value returned from iterator */

  if ( _ok(status) )
    {
    for ( i=_tbl_htsize(table),
          chead = _tbl_hash(table);
          i>0; i--, chead++ )
      {
      cptr = *chead;
      while ( cptr )
	{
	rval = (*iterator)( _CAR(cptr), iarg, status );
	if ( rval )
	  robj = lstx_cell( rval, robj, status );
	cptr = _CDR(cptr);
	}
      }
    }

  return robj;
  }


void tblx_scan( ADIobj    *head,        /* Head of a NODE list */
                char      *str,         /* String to find */
                int       slen,         /* Length of string */
                ADIobj    **sptr,       /* Addr of pointer to dataobj */
                ADIlogical  *found,    /* Was a dataobj found? */
                ADIstatus   status )
  {
  ADIobj        cstr;                   /* Current dataobj name */
  int           test = 1;               /* String comparison */

  if ( !_ok(status) )                   /* Check status */
    return;

  *found = ADI__false;                  /* Default return values */
  *sptr = head;

  while ( ((**sptr)!=ADI__nullid) && 	/* Loop while end of list not reached */
	  !(*found) &&  		/*   and dataobj not found */
	  (test>=0) )			/*   and not past dataobj in alphabet */
    {
    cstr = _CAAR(**sptr);               /* Current dataobj name */

    test = strx_cmpc( str, slen,        /* Compare the two names */
                      cstr );
    if ( test == 0 )			/* Terminate if match found */
      *found = ADI__true;
    else if ( test > 0 )                /* Next list element if dataobj name */
      *sptr = &_CDR(**sptr);	        /* precedes current one alph'ically */
    }
  }


ADIobj tblx_new( int size,
                 int flags,
                 ADIstatus status )
  {
  ADIobj       table = ADI__nullid;     /* New table */

  if ( !_ok(status) )			/* Check status */
    return ADI__nullid;

  table = adix_cls_alloc(               /* Create a new table */
              _cdef_ctrl(UT_ALLOC_tbl),
                        0, 0, status );

  if ( _ok(status) )			/* Check status */
    {
    adic_newv0i( size,
               &_tbl_htsize(table), status );
    adic_newv0i( flags,
               &_tbl_flags(table), status );

    adic_new1( "*", size,
              &_tbl_head(table), status );
    }

  return table;
  }



/*  tblx_sadd - Add entry to table if not already present
 */
ADIobj tblx_sadd( ADIobj    *table,
                  char      *str, int slen,
                  ADIobj    dataobj,
                  ADIstatus     status )
  {
  ADIobj        *lentry;                /* List insertion point */
  ADIobj	rval;			/* The associated data object */
  int           hcode;                  /* Table hashing code */
  ADIobj	*head;			/* Head of dataobj list */
  ADIobj        hnode;                  /* New element for table hash list */
  int           hval;                   /* String hash value */
  ADIinteger    lhcode = 0;
  ADIobj        nstr;                   /* Newly created ADI string */
  ADIlogical    there;                  /* String in list? */

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  if ( _tbl_q(*table) )			/* Table or a-list? */
    {
    adic_get0i( _tbl_htsize(*table),
		&lhcode, status );
    hcode = (int) lhcode;

    strx_hash( str, slen, hcode,	/* Find string's hash value */
		&hval, status );

    head = _tbl_hash(*table) + hval;
    }
  else
    head = table;

  tblx_scan( head, str,	slen,	        /* Look along list for string */
      &lentry, &there, status );

  if ( there )
    rval = _CAR(*lentry);
  else
    {
    adic_newv0c_n( str, slen, &nstr,    /* Create new string */
                   status );

    hnode = lstx_cell(                  /* The new hash node */
                lstx_cell( nstr,
                  dataobj, status ),
                *lentry,
                status );

    *lentry = hnode;                    /* Patch node into list */

    rval = _CAR(hnode);                 /* Return the dotted pair address */
    }

  return rval;
  }


/*  ECItableAdd - Add entry to table, report error if already present
 */
ADIobj tblx_add( ADIobj    *table,
                 char      *str, int slen,
                 ADIobj    dataobj,
                 ADIstatus status )
  {
  ADIobj        *lentry;                /* List insertion point */
  ADIobj	rstr = ADI__nullid;	/* The string in the table */

  ADIobj	*head;			/* Head of dataobj list */
  int           hcode;
  ADIobj        hnode;                  /* New element for table hash list */
  int           hval;                   /* String hash value */
  ADIobj        nstr;
  ADIinteger    lhcode;                  /* Table hashing code */
  ADIlogical      there;                  /* String in list? */

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  if ( _tbl_q(*table) )			/* Table or a-list? */
    {
    adic_get0i( _tbl_htsize(*table),
                &lhcode, status );
    hcode = (int) lhcode;

    strx_hash( str, slen, hcode,	/* Find string's hash value */
               &hval, status );

    head = _tbl_hash(*table) + hval;
    }
  else
    head = table;

  tblx_scan( head, str,	slen,	        /* Look along list for string */
             &lentry, &there, status );

  if ( there )
    {
    adic_setetc( "NAME", str, slen );
    adic_setecs( ADI__SYMDEF, "Symbol /^NAME/ is already in the table",
		 status );
    }
  else
    {
    adic_newv0c_n( str, slen,
                   &nstr, status );

    hnode = lstx_cell(                  /* The new hash node */
                lstx_cell( nstr,
                  dataobj, status ),
                *lentry,
                status );

    *lentry = hnode;                    /* Patch node into list */

    rstr = _CAR(hnode);                 /* Return the dotted pair address */
    }

  return rstr;
  }


ADIobj tblx_find( ADIobj    *table,
                  char  *str, int slen,
                  ADIstatus     status )
  {
  int           hcode;
  ADIobj	*head;			/* Head of list */
  int           hval;                   /* String hash value */
  ADIobj        *lentry;                /* List insertion point */
  ADIinteger    lhcode;                  /* Table hashing code */
  ADIlogical    there;

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  if ( _tbl_q(*table) )			/* Table or a-list? */
    {
    adic_get0i( _tbl_htsize(*table),
                &lhcode, status );
    hcode = (int) lhcode;

    strx_hash( str, slen, hcode,	/* Find string's hash value */
               &hval, status );

    head = _tbl_hash(*table) + hval;
    }
  else
    head = table;

  tblx_scan( head, str, slen, &lentry,	/* Look along list for string */
		  &there, status );

  if ( there )
    return _CAR(*lentry);
  else
    return ADI__nullid;
  }
