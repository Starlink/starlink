#include <string.h>
#include <ctype.h>

#include "adi.h"
#include "adi_err.h"
#include "adierror.h"

#ifndef NOEMS
#include "ems.h"
#endif


/*
 * Global variables, externally visible
 */
char	*ADI_G_err_rtn = NULL;		/* Current error routine */


/* Routines to support non-EMS error reporting
 */
#ifdef  NOEMS
#define	MAXTOK	10			/* Maximum number of tokens */
#define TOKLEN  80

typedef
  struct {
    char	name[20];
    char 	dat[TOKELEN];
    int		len;
    }
  etoken;

etoken  ADI_G_err_toks[MAXTOK];		/* Error tokens */
int	ADI_G_err_ntok = 0;		/* Token count */
char	ADI_G_err_ctx[200];		/* Current contextual error */
#endif

void adix_errcnl( ADIstatus status )
  {
#ifndef NOEMS
  ems_annul_c( status );
#else
  *status = SAI__OK;
#endif
  }

void adix_setetc( char *tok, char *val, int vlen )
  {
  if ( vlen < 1 )
    vlen = strlen(val);

#ifndef NOEMS
  ems_setc_c( tok, val, vlen );
#else
  if ( ADI_G_err_ntok < MAXTOK )
    {
    strcpy( ADI_G_err_toks[ADI_G_err_ntok].name, tok );
    strncpy( ADI_G_err_toks[ADI_G_err_ntok].dat, val, TOKLEN );
    ADI_G_err_toks[ADI_G_err_ntok].len = vlen;

    ADI_G_err_ntok++;
    }
#endif
  }


void adix_seteti( char *tok, int val )
  {
#ifndef NOEMS
  ems_seti_c( tok, val );
#else
  char	buf[20];
  int	nc;
  nc = sprintf( buf, "%ld", val );
  adix_setetc( tok, buf, nc );
#endif
  }


/*
 * Routine to expand message tokens for the error context string
 */
#ifdef  NOEMS
void adix_errexp( char *mes, char *obuf )
  {
  ADIboolean	found;
  int		itok;
  char		*mptr = obuf;
  char		*sptr = mes;
  int		tlen,vlen;

  for ( sptr = mes; *sptr; ) {
    if ( *sptr == '^' )
      {
      sptr++;
      for( tlen=0; isalnum(*(sptr+tlen)); tlen++ );
      if ( tlen )
	{
	for ( itok=0, found = ADI__false;
	      (itok<ADI_G_err_ntok) && !found ; itok++ )
	  found = !strncmp(sptr,ADI_G_err_toks[itok].name,tlen);
	if ( found )
	  {
	  itok--;
	  vlen = ADI_G_err_toks[itok].len;
	  memcpy( mptr, ADI_G_err_toks[itok].dat, vlen );
	  mptr += vlen;
	  }
	else {
	  *mptr++ = '<';
	  memcpy( mptr, sptr, tlen );
	  mptr += tlen;
	  *mptr++ = '>';
	  }
	sptr += tlen;
	}
      else
	*mptr++ = '^';
      }
    else
      *mptr++ = *sptr++;
    }

  *mptr = '\0';
  }
#endif


/*
 *  Set status to an error code
 */
void adix_setec( ADIstatype code, ADIstatus status )
  {
  char		*emsg;			/* Standard error message */

  *status = code;			/* Set the error code */

#ifndef NOEMS
  emsg = adix_errmsg( code, NULL, 0 );	/* Get text message address */

  if ( emsg )                           /* Set the error */
    ems_rep_c( " ", emsg, status );
#endif
  }


/*
 *  Set the contextual message string
 */
void adix_setes( char *ctx, int clen, ADIstatus status )
  {
  char		lctx[200];		/* Local contextual error */
  char		*cptr = NULL;		/* Context message */
  int		lrtn;			/* Length of named routine name */

  if ( clen <= 0 )			/* Import string */
    clen = strlen(ctx);

  if ( *ctx && (*ctx!=' ') ) {		/* User supplied a context string? */
    if ( ADI_G_err_rtn ) {		/* Error routine specified? */
      lrtn = strlen( ADI_G_err_rtn );
      strcpy( lctx, ADI_G_err_rtn );
      strcpy( lctx + lrtn, " : " );
      strncpy( lctx + lrtn + 3, ctx, clen );
      cptr = lctx;
      }
    else
      cptr = ctx;
    }

#ifndef NOEMS
  if ( cptr )				/* Any contextual error? */
    ems_rep_c( " ", cptr, status );

#else
  adix_errexp( cptr, ADI_G_err_ctx );	/* Expand tokens */

  ADI_G_err_ntok = 0;			/* Reset token count */
#endif
  }


/*
 *  Set the error and contextual message string
 */
void adix_setecs( ADIstatype code, char *ctx, int clen, ADIstatus status )
  {
  adix_setec( code, status );           /* Set the error code */
#ifndef NOEMS
  ems_renew_c();			/* Renew tokens for context error */
#endif
  adix_setes( ctx, clen, status );	/* Set the context string */
  }


#ifdef  NOEMS
void adix_errctx( char *buf, int buflen )
  {
  strncpy( buf, ADI_G_err_ctx, buflen );
  }
#endif


char *adix_errmsg( ADIstatype code, char *buf, int buflen )
  {
  static
    char *emsg = "Unrecognised ADI error code";

  static
    struct
      {
      ADIstatype	code;
      char		*msg;
      }
    etable[] =
      {
      {ADI__CONER,	"Data conversion error"},
      {ADI__TRUNC,	"Text truncated"},
      {ADI__INVARG,	"Invalid argument"},
      {ADI__SYMDEF,	"Symbol already defined"},
      {ADI__FATAL,	"Fatal internal ADI error"},
      {ADI__IDINV,	"Invalid ADI identifier"},
      {ADI__ILLKOP,	"Illegal operation on kernel object"},
      {ADI__NOTSET,	"Object data is not set"},
      {ADI__EXISTS,	"Object already exists"},
      {ADI__NOMTH,	"No applicable methods"},
      {ADI__MTHERR,	"Error in method"},
      {ADI__NOMEMB,	"No such class member"},
      {ADI__ISPRIM,	"Object is primitive"},
      {ADI__ILLOP,	"Illegal operation"},
      {ADI__OUTMEM,	"Unable to allocate dynamic memory"},
      {ADI__NOTACT,	"ADI is not active"},
      {ADI__NOPROP,	"No such property"},
      {ADI__NOCOMP,	"No such structure component"},
      {ADI__EXCEED,	"Storage space in buffer exceeded"},
      {ADI__NONAME,	"Object is not a named object"},
      {SAI__ERROR,	"Error"},
      {SAI__OK,		""}
      };

  int		i;
  char 		*sptr;
  ADIboolean	found = ADI__false;

  for( i=0; etable[i].code && ! found ; )
    if ( code == etable[i].code )
      found = ADI__true;
    else
      i++;

  if ( found )
    sptr = etable[i].msg;
  else
    sptr = emsg;

  if ( buflen )
    strncpy( buf, sptr, buflen );

  return sptr;
  }
