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
 *   strx_newmat        - fill in string data from MTA
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

  while ( curp && ! found )             /* Scan string stores for room */
    {
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


void strx_free( char *ptr, ADIstatus status )
  {
  StrStorePtr   curp = ss_first;
  ADIlogical    found = ADI__false;
  StrStorePtr   lcurp = NULL;
  long          pdif;

  _chk_stat;

  while ( curp && ! found )             /* Scan string stores for pointer */
    {
    pdif = ptr - curp->data;            /* Distance from data block start */

    if ( (pdif>=0) && (pdif<SS_CHARS) )
      found = ADI__true;
    else
      {
      lcurp = curp; curp = curp->link;
      }
    }

  if ( found )                          /* Found the pointer? */
    {
    char *cptr = ptr;
    while ( *cptr )
      {
      *cptr++ = 0;
      }

    curp->nhit--;                       /* Decrease reference count on block */

    if ( ! curp->nhit )                 /* Deallocate if gone to zero */
      {
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


void strx_init( ADIstatus status )
  {
  _chk_stat;

/* Install the string destructor */
  adix_def_destruc( UT_ALLOC_c, strx_dstrc, status );
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

  for( cp=1;
       (cp<=minlen) && !test; cp++ )
       test = (*sc1++) - (*sc2++);

  if ( test || (len1==len2) )
    return test;
  else
    return (len1 >len2) ? 1 : -1;
  }


int strx_cmpc( char *str1, int len1, ADIobj str2 )
  {
  return strx_cmp2c( str1, len1,	/* Access 2nd string data */
	      _str_dat(str2),
	      _str_len(str2) );
  }


int strx_cmp( ADIobj str1, ADIobj str2 )
  {
  return strx_cmp2c( _str_dat(str1),	/* Access 2nd string data */
	      _str_len(str1),
	      _str_dat(str2),
	      _str_len(str2) );
  }



void strx_expc( int inlen, char *in, int outlen, char *out )
  {
  int nc = _MIN(inlen,outlen);

  memcpy(out,in,nc);			/* Copy characters */

  if ( inlen < outlen )			/* Null terminate string if space */
    out[inlen] = 0;
  }


void strx_expf( int inlen, char *in, int outlen, char *out )
  {
  int nc = _MIN(inlen,outlen);

  memcpy(out,in,nc);			/* Copy characters */

/* Pad with spaces */
  if ( inlen < outlen )
    memset( out + inlen, ' ', outlen - nc );
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

  for( cp=1,sc1 = str1,sc2 = str2;
       (cp<=minlen) && !test; cp++ )
    {
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
  return strx_cmpi2c( _str_dat(str1),
		      _str_len(str1),
		      _str_dat(str2),
		      _str_len(str2) );
  }

int strx_cmpic( char *str1, int len1, ADIobj str2 )
  {
  return strx_cmpi2c( str1, len1, _str_dat(str2), _str_len(str2) );
  }


/* adix_str_hash - case sensitive string hash
 *
 */
void strx_hash( char       *str,
                int        slen,
                int        tsize,        /* Hash list length */
                int        *hash,        /* Hash value */
                ADIstatus  status )
  {
  register int     cp;			/* Loop over string */
  register char    *sptr;

  _chk_stat;                            /* Check status */

  *hash = 0;                            /* Calculate the hash value */
  for ( cp = slen, sptr = str;
	cp; cp-- )
    *hash = *hash + *sptr++;
  *hash = *hash % tsize;
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

/*  Load data into newly created string instance
 *
 */

void strx_dstrc( ADIobj str, ADIstatus status )
  {
  ADIstring         *sptr = _str_data(str);

  strx_free( sptr->data, status );
  }


void ADIstrngGetLen( ADIobj id, ADIinteger *rval, ADIstatus status )
  {
  _chk_stat;

  if ( _valid_q(id) && _str_q(id) )
    *rval = _str_len(id);
  else
    adic_setecs( ADI__INVARG, "Object is not a character string", status );
  }

