#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <stdio.h>

#include "asterix.h"                    /* Asterix definitions */

#include "aditypes.h"
#include "adimem.h"
#include "adikrnl.h"                    /* Internal ADI kernel */
#include "adimem.h"                     /* Allocation routines */
#include "adistrng.h"
#include "adicface.h"
#include "adiparse.h"                   /* Prototypes for this sub-package */

#include "adi_err.h"                    /* ADI error codes */

void prsx_init( ADIstatus status )
  {
  _chk_stat;
  }


ADIobj prsx_symname( ADIstreamPtr stream, ADIstatus status )
  {
  _chk_stat_ret(ADI__nullid);

  return
    adix_cmn( stream->ctok.dat,
	      stream->ctok.nc,
	      status );
  }

/*
 * Parse a constant value
 */
ADIobj prsx_cvalue( ADIstreamPtr stream, ADIstatus status )
  {
  ADItokenType  ctok;
  ADIobj    	rval;

  _chk_stat_ret(ADI__nullid);

  ctok = stream->ctok.t;

  switch( ctok )
    {
    case TOK__CONST:
      adix_new_n( ADI__true, ADI__nullid, NULL,	/* Create object of correct type */
		  0, 0, NULL, NULL, _cdef_ctrl(stream->ctok.dt),
		 0, &rval, status );

      adic_put0c( rval, stream->ctok.dat, status );
      break;

    case TOK__SYM:
      if ( ADIisTokenCstring( stream, "True", status ) )
	adic_newv0l( ADI__true, &rval, status );
      else if ( ADIisTokenCstring( stream, "False", status ) )
	adic_newv0l( ADI__false, &rval, status );
      else {
	}
      break;

    default:
      rval = ADI__nullid;
      adic_setecs( SAI__ERROR, "Invalid constant", status );
    }

  ADInextTokenFromStream( stream, 	/* Next token if no error */
                        status );

  return rval;
  }


/*
 * Parse identifier=value pairs until no more. Values are written as
 * components of the structure object 'id'
 */
void prsx_namvalcmp( ADIstreamPtr str, ADIobj id, ADIstatus status )
  {
  ADIobj    	cnam;
  ADIobj    	cval;

  _chk_stat;

  while ( (str->ctok.t == TOK__SYM) && 	/* While more identifiers */
                  _ok(status) ) {

    cnam = prsx_symname( str, status ); /* Take the component name from stream */

    ADInextTokenFromStream( str, status );

    if ( ADImatchTokenFromStream( str, 	/* Assignment bit name=value ? */
                 TOK__ASSIGN, status )) {
      cval = prsx_cvalue( str,		/* Take component value from stream */
                     status );

      adix_cputiid( id, cnam, cval,	/* Write new component to structure */
                          status );
      }
    else {				/* We've hit the end */
      adic_erase( &cnam, status );
      }
    }
  }


void ADIaddDeviceToStream( ADIstreamPtr str, ADIdeviceType type, ADIstatus status )
  {
  ADIdevicePtr     dev;

  if ( _ok(status) )
    {
    if ( str->dev )
      dev = (ADIdevicePtr)
        adix_mem_alloc( sizeof(ADIdevice), status );
    else
      dev = &str->basedev;

    dev->last = str->dev;
    dev->ptr = NULL;
    dev->type = type;
    str->dev = dev;
    }
  }


void ADIclearStream( ADIstreamPtr str, ADIstatus status )
  {
  if ( _ok(status) )
    {
    str->nc_pb = 0;
    str->nt_pb = 0;
    str->dev = NULL;
    str->flags = ADI_STREAM__DEFAULT;
    }
  }


int ADIgetStreamAttrs( ADIstreamPtr str, ADIstatus status )
  {
  return _ok(status) ? str->flags : 0;
  }


void ADIputStreamAttrs( ADIstreamPtr str, int flags, ADIstatus status )
  {
  if ( _ok(status) )
    str->flags = flags;
  }

int ADIsetStreamAttr( ADIstreamPtr str, int flag, ADIstatus status )
  {
  int		oflags = 0;

  if ( _ok(status) ) {
    oflags = str->flags;
    str->flags |= flag;
    }

  return oflags;
  }


ADIstreamPtr ADInewStream( ADIstatus status )
  {
  ADIstreamPtr	str = NULL;

  if ( _ok(status) )
    {
    str = (ADIstreamPtr) adix_mem_alloc( sizeof(ADIstream), status );
    ADIclearStream( str, status );
    }

  return str;
  }


void ADIdropDevice( ADIstreamPtr str, ADIstatus status )
  {
  ADIdevicePtr     dev = str->dev;

  if ( _ok(status) )
    {
    if ( dev )
      {
      if ( dev->type == ADIdevFile )
	fclose( dev->f );
      else if ( dev->type == ADIdevCin )
        adix_mem_free( dev->buf, dev->bufsiz, status );
      str->dev = dev->last;
      if ( dev != &str->basedev )
        adix_mem_free( dev->buf, sizeof(ADIdevice), status );
/*      if ( str->dev )
	ADInextTokenFromStream( str, status ); */
      }
    else
      *status = SAI__ERROR;
    }
  }


void ADIresetStream( ADIstreamPtr str, ADIstatus status )
  {
  while ( _ok(status) && str->dev )
    ADIdropDevice( str, status );

  ADIclearStream( str, status );
  }


void ADIdropStream( ADIstreamPtr str, ADIstatus status )
  {
  if ( str->dev )
    ADIresetStream( str, status );

  adix_mem_free( str, sizeof(ADIstream), status );
  }


void ADIextendStreamC( ADIstreamPtr str, char *buf, int blen, ADIstatus status )
  {
  if ( _ok(status) )
    {
    ADIaddDeviceToStream( str, ADIdevCstring, status );
    str->dev->ptr = buf;
    str->dev->bufsiz = blen;
    str->dev->cmode = 'r';
    }
  }


void ADIextendStreamCin( ADIstreamPtr str, int bufsiz, ADIstatus status )
  {
  if ( _ok(status) )
    {
    ADIaddDeviceToStream( str, ADIdevCin, status );
    str->dev->bufsiz = bufsiz;
    str->dev->buf = (char *) adix_mem_alloc( (size_t) bufsiz, status );
    str->dev->buf[0] = '\0';
    str->dev->ptr = str->dev->buf;
/*    str->dev->cmode = EOS_STR__DYNAMIC; */
    }
  }


void ADIextendStreamFile( ADIstreamPtr str, FILE *f, ADIstatus status )
  {
  if ( _ok(status) )
    {
    ADIaddDeviceToStream( str, ADIdevFile, status );
    str->dev->f = f;
/*    str->dev->cmode = EOS_STR__DYNAMIC; */
    }
  }


char ADIreadCharFromStream( ADIstreamPtr str, ADIstatus status )
  {
  char          ch = '\0';
  int           gotit = ADI__false;

  while ( _ok(status) && !gotit )
    {
    if ( str->nc_pb ) {
      ch = str->c_pb[--str->nc_pb]; gotit = ADI__true;
      }
    else if ( str->dev )
      {
      switch( str->dev->type )
	{
	case ADIdevFile:
	  if ( feof(str->dev->f) )
	    ADIdropDevice( str, status );
	  else
	    {
	    ch = fgetc( str->dev->f );
	    gotit = ! feof(str->dev->f);
	    }
	  break;
	case ADIdevCin:
	  if ( *str->dev->ptr )
	    {
	    ch = *(str->dev->ptr++); gotit = ADI__true;
	    }
	  else
	    {
/*          if ( str->dev->ptr == str->dev->buf )
	      {*/
	      str->dev->ptr = str->dev->buf;
	      printf( "> " );
	      fgets( str->dev->buf, str->dev->bufsiz, stdin );
/*            }
	    else
	      {
	      str->dev->ptr = str->dev->buf;
	      str->dev->buf[0] = '\0'; ch = '\n'; gotit = ADI__true;
	      } */
	    }
	  break;
	case ADIdevCstring:
	  ch = *(str->dev->ptr++); gotit = ADI__true;
	  break;
	}
      }
    else
      *status = SAI__ERROR;
    }

  str->ctok.dat[str->ctok.nc++] = ch;
  return ch;
  }


void ADIreturnCharToStream( ADIstreamPtr str, char ch, ADIstatus status )
  {
  if ( _ok(status) )
    {
    str->c_pb[str->nc_pb++] = ch;
    str->ctok.nc--;
    }
  }


void ADIdescribeToken( ADItokenType tok, char **str, int *len )
  {
  static
    struct
      {
      ADItokenType      tok;
      char              *desc;
      }
    tdesc[] =
      {
      {TOK__COLON,      "colon"},
      {TOK__COMMA,	"comma"},
      {TOK__CONST,	"constant"},
      {TOK__RBRAK,	"right bracket ]"},
      {TOK__RBRACE,	"right brace }"},
      {TOK__RPAREN,	"right parenthesis"},
      {TOK__SEMICOLON,  "semi-colon"},
      {TOK__SYM,        "symbol"},
      {TOK__END,	"end of line"},
      {TOK__NOTATOK,	"unrecognised data"},
      {TOK__TRAP,       " "}
      };

  int           it;                     /* Loop over token description table */

  for ( it =0;
	((tdesc[it].tok!=TOK__TRAP) &&
	 (tdesc[it].tok!=tok));
	it++ );

  *str = tdesc[it].desc;                /* Return description */
  *len = strlen( *str );
  }


ADIboolean ADIisTokenCstring( ADIstreamPtr str, char *string, ADIstatus status )
  {
  _chk_stat_ret(ADI__false);

  if ( strx_cmp2c( str->ctok.dat, str->ctok.nc,
		   string, strlen(string) ) )
    return ADI__false;
  else
    return ADI__true;
  }


ADItokenType ADIisTokenInSet( ADIstreamPtr str,
			ADItokenType tlist[],
			ADIstatus status )
  {
  int                   it;
  ADItokenType  rtype = TOK__NULL;

  if ( !_ok(status) )
    return rtype;                       /* Check status */

  for ( it =0;
	((tlist[it]!=TOK__TRAP) && (tlist[it]!=str->ctok.t)) ;
	it++ ) ;

  if ( tlist[it]==TOK__TRAP )
    return TOK__NULL;
  else
    return tlist[it];
  }



#define EOS -1

ADItokenType ADInextTokenFromStream( ADIstreamPtr str, ADIstatus status )
  {
  ADItokenType		etok;
  ADItokenType		tok = TOK__NOTATOK;
  char           	ch;
  ADIboolean        	inquotes = ADI__true;

  if ( !_ok(status) )
    return tok;

/* The token to return if a logical end of line is met. If end-of-lines are */
/* ignored then the flag token is chosen, which forces more parsing */
  if ( str->flags & ADI_STREAM__EOLISP )
    etok = TOK__NOTATOK;
  else
    etok = TOK__END;

  while ( _ok(status) && (tok==TOK__NOTATOK) )
    {
    str->ctok.nc = 0;
    str->ctok.dt = 0;
    ch = ADIreadCharFromStream( str, status );

    if ( (ch==' ') || ch==9 )
      {;}
    else if ( ! _ok(status) ) {
      *status = EOS;
      continue;
      }
    else if ( ch == '\n' ) {
      tok = etok;
      }
    else if ( isalpha(ch) || (ch=='%') || (ch=='_') )
      {
      str->ctok.ptr0 = str->dev->ptr - 1;
      while( isalpha(ch) || (ch=='%') || isdigit(ch) || (ch=='_')) {
	ch = ADIreadCharFromStream( str, status );
	}
      ADIreturnCharToStream( str, ch, status );
      str->ctok.dat[str->ctok.nc] = '\0';
      tok = TOK__SYM;
      str->ctok.dt = UT_ALLOC_c;
      }

    else if ( isdigit(ch) )
      {
      str->ctok.ptr0 = str->dev->ptr - 1;
      while( isalnum(ch) )
	ch = ADIreadCharFromStream( str, status );

      if ( ch == '.' )                	/* Decimal point found */
	{
	int	ndig = 0;

	ch = ADIreadCharFromStream( str, status );
	while( isdigit(ch) ) {
	  ndig++;
	  ch = ADIreadCharFromStream( str, status );
	  }

	if ( ndig > 7 )
	  str->ctok.dt = UT_ALLOC_d;
	else
	  str->ctok.dt = UT_ALLOC_r;
	}
      else
	str->ctok.dt = UT_ALLOC_i;

      ADIreturnCharToStream( str, ch, status );
      str->ctok.dat[str->ctok.nc] = '\0';
      tok = TOK__CONST;
      }

    else if ( ch == '#' )		/* Comments return TOK__END */
      {
      do
	ch = ADIreadCharFromStream( str, status );
      while ( (ch != '\n') && _ok(status) );
      tok = etok;
      }
    else if ( ch == '"' )
      {
      str->ctok.nc = 0;
      while( inquotes )
	{
	ch = ADIreadCharFromStream( str, status );
	switch( ch )
	  {
	  case '\\':
	    ch = ADIreadCharFromStream( str, status );
	    switch (ch)
	      {
	      case '\\':
		str->ctok.dat[--str->ctok.nc] = '\\';
		break;
	      case 'n':
		str->ctok.dat[--str->ctok.nc] = '\n';
		break;
	      case '"':
		str->ctok.nc--;
		break;
	      case 't':
		str->ctok.dat[--str->ctok.nc] = '\t';
		break;
	      default:
		break;
	      }
	    break;
	  case '"':
	    inquotes = ADI__false;
	    break;
	  default:
	    break;
	  }
	}
      str->ctok.dat[str->ctok.nc-1] = '\0';
      tok = TOK__CONST;
      str->ctok.dt = UT_ALLOC_c;
      }
    else
      switch (ch)
	{
	case ',':
	  tok = TOK__COMMA;
	  break;
	case '{':
	  tok = TOK__LBRACE;
	  break;
	case '}':
	  tok = TOK__RBRACE;
	  break;
	case '[':
	  tok = TOK__LBRAK;
	  break;
	case ']':
	  tok = TOK__RBRAK;
	  break;
	case '(':
	  tok = TOK__LPAREN;
	  break;
	case ')':
	  tok = TOK__RPAREN;
	  break;
	case '.':
	  tok = TOK__PERIOD;
	  break;
	case '=':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '=' )
	    tok = TOK__EQ;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__ASSIGN;
	    }
	  break;
	case '_':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '_' )
	    {
	    ch = ADIreadCharFromStream( str, status );
	    if ( ch == '_' )
	      tok = TOK__UND3;
	    else
	      {
	      ADIreturnCharToStream( str, ch, status );
	      tok = TOK__UND2;
	      }
	    }
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__UND1;
	    }
	  break;
	case '+':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '+' )
	    tok = TOK__INCR;
	  else if ( ch == '=' )
	    tok = TOK__PLUSEQ;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__PLUS;
	    }
	  break;
	case '-':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '-' )
	    tok = TOK__DECR;
	  else if ( ch == '=' )
	    tok = TOK__MINUSEQ;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__MINUS;
	    }
	  break;
	case '*':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '*' )
	    tok = TOK__CARAT;
	  else if ( ch == '=' )
	    tok = TOK__MULEQ;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__MUL;
	    }
	  break;
	case '?':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '?' )
	    tok = TOK__QUERY2;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__QUERY;
	    }
	  break;
	case '$':
	  tok = TOK__DOLLAR;
	  break;
	case '/':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '=' )
	    tok = TOK__DIVEQ;
	  else if ( ch == '/' )
	    tok = TOK__CONC;
	  else if ( ch == '@' )
	    tok = TOK__SLASHAT;
	  else if ( ch == ';' )
	    tok = TOK__COND;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__DIV;
	    }
	  break;
	case ';':
	  ch = ADIreadCharFromStream( str, status );
          if ( ch == ';' ) {
            do
	      ch = ADIreadCharFromStream( str, status );
            while ( (ch != '\n') && _ok(status) );
            tok = etok;
            }
	  else {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__SEMICOLON;
	    }
	  break;
	case '@':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '@' )
	    tok = TOK__AT;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__AT;
	    }
	  break;
	case ':':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == ':' )
	    tok = TOK__SCOPE;
	  else if ( ch == '=' )
	    tok = TOK__COLONEQ;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__COLON;
	    }
	  break;
	case '|':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '|' )
	    tok = TOK__OR;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__BAR;
	    }
	  break;
	case '!':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '=' )
	    tok = TOK__NE;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__NOT;
	    }
	  break;
	case '&':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '&' )
	    tok = TOK__AND;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
/*	    EOS_ERR( EOS__INVEXPSYN, status ); */
	    }
	  break;
	case '>':
	  ch = ADIreadCharFromStream( str, status );
	  switch( ch )
	    {
	    case '=':
	      tok = TOK__GE;
	      break;
	    case '>':
	      ch = ADIreadCharFromStream( str, status );
	      if ( ch == '>' )
		tok = TOK__RCHEV3;
	      else
		tok = TOK__RCHEV;
	      break;
	    default:
	      ADIreturnCharToStream( str, ch, status );
	      tok = TOK__GT;
	      break;
	    }
	  break;
	case '\\':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '\n' )		/* Continuation character */
	    {;}
	  else if ( ch == '\\' )
	    tok = TOK__BSLASH;
	  break;
	case '<':
	  ch = ADIreadCharFromStream( str, status );
	  switch ( ch )
	    {
	    case '>':
	      tok = TOK__NE;
	      break;
	    case '<':
	      tok = TOK__LCHEV;
	      break;
	    case '=':
	      tok = TOK__LE;
	      break;
	    default:
	      ADIreturnCharToStream( str, ch, status );
	      tok = TOK__LT;
	    }
	  break;
	case '^':
	  ch = ADIreadCharFromStream( str, status );
	  if ( ch == '^' )
	    tok = TOK__CARAT2;
	  else
	    {
	    ADIreturnCharToStream( str, ch, status );
	    tok = TOK__CARAT;
	    }
	  break;
	case '\n':
	case '\0':
          tok = etok;
	  break;
	}
    }

  if ( *status == EOS )
    *status = SAI__OK;

  str->ctok.t = tok;
  return tok;
  }

ADIboolean ADImatchTokenFromStream( ADIstreamPtr str,
				    ADItokenType t, ADIstatus status )
  {
  if ( str->ctok.t == t )
    {
    (void) ADInextTokenFromStream( str, status );
    return ADI__true;
    }
  else
    return ADI__false;
  }
