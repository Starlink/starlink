/*  adistrng - ADI string operations
 *
 *  Internal ADI routines :
 *
 *   strx_init          - subpackage initialiser
 *
 *   strx_cmp           - case sensitive compare
 *   strx_cmpi          - case blind compare
 *   strx_expc          - export characters to C string
 *   strx_expf          - export characters to Fortran string
 *   strx_hash          - hash a string
 *   strx_tok           - string value to message system token
 */
#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <stdarg.h>

#include "asterix.h"                    /* Asterix definitions */

#include "aditypes.h"
#include "adikrnl.h"                    /* Internal ADI kernel */
#include "adimem.h"                     /* Allocation routines */
#include "adicface.h"
#include "adierror.h"                   /* ADI error handling */
#include "adistrng.h"                   /* Prototypes for this sub-package */


#ifndef __MSDOS__
#ifndef _toupper			/* Some ctype's don't supply this */
#define _toupper(c) ((c)-'a'+'A')
#define _tolower(c) ((c)-'A'+'a')
#endif
#endif

extern ADIobj UT_ALLOC_c;

typedef
  struct StrStoreTag *StrStorePtr;

#define SS_CHARS (4096-sizeof(StrStorePtr)-2*sizeof(short))

typedef
  struct StrStoreTag
    {
    short               nfree;
    short               nhit;
    StrStorePtr         link;
    char                data[SS_CHARS];
    }
  StrStore;

StrStorePtr     ss_first = NULL;        /* Start of string store list */
StrStorePtr     *ss_insert = &ss_first; /* List inseration point */

char *strx_alloc( int len, ADIstatus status )
  {
  StrStorePtr   curp = ss_first;
  int           found = ADI__false;
  char          *rval = NULL;

  while ( curp && ! found ) {           /* Scan string stores for room */
    if ( curp->nfree >= len )
      found = ADI__true;
    else
      curp = curp->link;
    }

/* No space found ? */
  if ( ! found ) {

/* Allocate a new one */
    curp = (StrStorePtr) ADImemAlloc( sizeof(StrStore), status );

/* Allocated new block ok? */
    if ( curp ) {
      curp->nfree = SS_CHARS;
      curp->nhit = 0;
      curp->link = NULL;
      memset( curp->data, 0, SS_CHARS );
      *ss_insert = curp;
      ss_insert = &curp->link;
      }
    }

/* We have a valid block now? */
  if ( curp ) {
    rval = curp->data + SS_CHARS - curp->nfree;
    curp->nfree -= len;
    curp->nhit++;
    }

  return rval;
  }

#include <stdio.h>

void strx_free( char *ptr, int len, ADIstatus status )
  {
  StrStorePtr   curp = ss_first;
  ADIlogical    found = ADI__false;
  StrStorePtr   lcurp = NULL;
  int		llen = len;
  long          pdif;

  _chk_stat;

/* Scan string stores for pointer */
  while ( curp && ! found ) {

/* Distance from data block start */
    pdif = ptr - curp->data;

    if ( (pdif>=0) && (pdif<SS_CHARS) )
      found = ADI__true;
    else {
      lcurp = curp; curp = curp->link;
      }
    }

/* Found the pointer? */
  if ( found ) {
    char *cptr = ptr;
    while ( llen-- )
      *cptr++ = 0;

    curp->nhit--;                       /* Decrease reference count on block */

    if ( ! curp->nhit ) {               /* Deallocate if gone to zero */
      StrStorePtr       dblock = curp;  /* The dead block */

      if ( ! lcurp )                    /* Is this the first block? */
	ss_first = curp->link;          /* Replace list head */
      else
	lcurp->link = curp->link;       /* Unhook block from list */

      if ( ! curp->link )               /* Last block? */
	{
	if ( lcurp )
	  ss_insert = &lcurp->link;
	else
	  ss_insert = &ss_first;
	}

/* Deallocate memory */
      ADImemFree( (char *) dblock, sizeof(StrStore), status );
      }
    }
  else
    adic_setecs( ADI__FATAL, "String memory segment not found", status );
  }

/*
 *  Erase string data
 */

ADIobj strx_dstrc( int narg, ADIobj args[], ADIstatus status )
  {
  ADIstring         *sptr = _str_data(args[0]);

  if ( sptr->len )
    strx_free( sptr->data, sptr->len, status );

  return ADI__nullid;
  }

void strx_init( ADIstatus status )
  {
  _chk_stat;

/* Install the string destructor */
  adic_defdes( UT_ALLOC_c, strx_dstrc, status );
  }


char *strx_dup( char *str )
  {
  ADIstatype    status = SAI__OK;
  char          *news;

  news = strx_alloc(strlen(str)+1,&status);
  strcpy(news,str);
  return news;
  }

char *strx_dupl( char *str, int len )
  {
  char *news;
  ADIstatype    status = SAI__OK;

  news = strx_alloc(len+1,&status);
  memcpy(news,str,len);
  news[len] = 0;
  return news;
  }


int strx_cmp2c( char *str1, int len1, char *str2, int len2 )
  {
  int           cp;
  int           minlen;
  char          *sc1 = str1;
  char          *sc2 = str2;
  int           test = 0;

  if ( sc1 == sc2 )                     /* Could be the same strings */
    return 0;

  minlen = _MIN(len1,len2);		/* Length for compare */

  for( cp=1; (cp<=minlen) && !test; cp++ )
    test = (*sc1++) - (*sc2++);

  if ( test || (len1==len2) )
    return test;
  else
    return (len1 >len2) ? 1 : -1;
  }


int strx_cmpc( char *str1, int len1, ADIobj str2 )
  {
  ADIstring	*s2 = _str_data(str2);

  return strx_cmp2c( str1, len1, s2->data, s2->len );
  }


int strx_cmp( ADIobj str1, ADIobj str2 )
  {
  if ( str1 == str2 )
    return 0;
  else {
    ADIstring	*s1 = _str_data(str1);
    ADIstring	*s2 = _str_data(str2);

/* Compare strings' data */
    return strx_cmp2c( s1->data, s1->len, s2->data, s2->len );
    }
  }

int strx_cmpi2c( char *str1, int len1, char *str2, int len2 )
  {
  int           cp;
  int           minlen;
  char		c1,c2;
  char          *sc1,*sc2;
  int           test = 0;

  if ( str1 == str2 )
    return test;

  minlen = _MIN(len1,len2);		/* Minimum of input lengths */

  for( cp=1,sc1 = str1,sc2 = str2; (cp<=minlen) && !test; cp++ ) {
    c1 = *sc1++; c2 = *sc2++;
    if ( islower(c1) )
      c1 = _toupper(c1);
    if ( islower(c2) )
      c2 = _toupper(c2);
    test = c1-c2;
    }

  if ( test || (len1==len2) )
    return test;
  else
    return (len1 > len2) ? 1 : -1;
  }


int strx_cmpi( ADIobj str1, ADIobj str2 )
  {
  if ( str1 == str2 )
    return 0;
  else {
    ADIstring	*s1 = _str_data(str1);
    ADIstring	*s2 = _str_data(str2);

/* Compare strings' data */
    return strx_cmpi2c( s1->data, s1->len, s2->data, s2->len );
    }
  }

int strx_cmpic( char *str1, int len1, ADIobj str2 )
  {
  ADIstring	*s2 = _str_data(str2);

  return strx_cmpi2c( str1, len1, s2->data, s2->len );
  }


/* adix_str_hash - case sensitive string hash
 *
 */
int strx_hash( char *str, int slen, int tsize )
  {
  register int     cp;			/* Loop over string */
  register char    *sptr;
  register unsigned int	   lhash = 0;

/* Calculate the hash value */
  for ( cp = slen, sptr = str; cp; cp-- )
    lhash += (lhash<<3) + *sptr++;

  return lhash % tsize;
  }

void strx_exit( ADIstatus status )
  {
  StrStorePtr   curp = ss_first,ncurp;

  while ( curp ) {
    ncurp = curp->link;
    ADImemFree( (char *) curp, sizeof(StrStore), status );
    curp = ncurp;
    }
  }



void ADIstrngGetLen( ADIobj id, ADIinteger *rval, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

  if ( _valid_q(id) && _str_q(id) )
    *rval = _str_len(id);
  else
    adic_setecs( ADI__INVARG, "Object is not a character string", status );
  }


void ADIstrngExport( ADIobj id, int clang, char *buf, int blen,
		     ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIstring	*nstr = _str_data(id);
    _CH_MOVE(buf,nstr->data,_MIN(nstr->len,blen));
    if ( nstr->len < blen ) {
      if ( clang )
	buf[nstr->len] = 0;
      else
	memset( buf + nstr->len, ' ', blen - nstr->len );
      }
    }
  }

ADIobj ADIstrngEnsureNterm( ADIobj id, ADIstatus status )
  {
  ADIlogical	clone = ADI__true;
  ADIstring	*ndat;
  ADIobj	rval;
  ADIstring	*sdat = _str_data(id);

  if ( sdat->data )
    if ( sdat->data[sdat->len-1] )
      clone = ADI__false;

  if ( clone )
    rval = adix_clone( id, status );
  else {
    adic_new0c( &rval, status );
    ndat = _str_data(rval);

    ndat->data = strx_alloc( sdat->len+1, status );
    ndat->len = sdat->len + 1;
    memcpy(ndat->data,sdat->data,sdat->len);
    ndat->data[sdat->len] = 0;
    }

  return rval;
  }
