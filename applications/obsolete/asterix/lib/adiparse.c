#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>

#include "asterix.h"                    /* Asterix definitions */

#include "adi_err.h"
#include "aditypes.h"
#include "adimem.h"
#include "adisyms.h"
#include "adilist.h"
#include "adikrnl.h"                    /* Internal ADI kernel */
#include "adimem.h"                     /* Allocation routines */
#include "adistrng.h"
#include "adicface.h"
#include "adiexpr.h"
#include "adiparse.h"                   /* Prototypes for this sub-package */

#include "adi_err.h"                    /* ADI error codes */


/*
 * The allocator object
 */
_DEF_STATIC_CDEF("_Stream",strm,16,ADIstrmDel,NULL);


/*
 * The keyword strings
 */
ADIobj	EXC_ArrayBound = ADI__nullid;
ADIobj  EXC_BoolExp = ADI__nullid;
ADIobj  EXC_ControlC = ADI__nullid;
ADIobj  EXC_Error = ADI__nullid;
ADIobj  EXC_ExceedMaxRecurse = ADI__nullid;
ADIobj  EXC_InvalidArg = ADI__nullid;
ADIobj  EXC_NoSuchField = ADI__nullid;
ADIobj  EXC_ScopeBreak = ADI__nullid;
ADIobj  EXC_SyntaxError = ADI__nullid;

ADIobj  K_AddTo = ADI__nullid;
ADIobj  K_Alternatives = ADI__nullid;
ADIobj  K_And = ADI__nullid;
ADIobj  K_Apply = ADI__nullid;
ADIobj  K_Array = ADI__nullid;
ADIobj  K_ArrayRef = ADI__nullid;

ADIobj  K_Blank = ADI__nullid;
ADIobj  K_BlankSeq = ADI__nullid;
ADIobj  K_BlankNullSeq = ADI__nullid;
ADIobj  K_Break = ADI__nullid;

ADIobj  K_Catch = ADI__nullid;
ADIobj  K_Concat = ADI__nullid;
ADIobj  K_Condition = ADI__nullid;

ADIobj  K_DefEnum = ADI__nullid;
ADIobj  K_Divide = ADI__nullid;
ADIobj  K_DivideBy = ADI__nullid;
ADIobj  K_Dot = ADI__nullid;
ADIobj  K_DoWhile = ADI__nullid;

ADIobj  K_Echo = ADI__nullid;
ADIobj  K_Equal = ADI__nullid;
ADIobj  K_Factorial = ADI__nullid;
ADIobj  K_Finally = ADI__nullid;
ADIobj  K_Foreach = ADI__nullid;

ADIobj  K_Get = ADI__nullid;
ADIobj  K_GE = ADI__nullid;
ADIobj  K_GT = ADI__nullid;

ADIobj  K_HoldAll = ADI__nullid;
ADIobj  K_HoldFirst = ADI__nullid;
ADIobj  K_HoldRest = ADI__nullid;

ADIobj  K_If = ADI__nullid;

ADIobj  K_List = ADI__nullid;
ADIobj  K_Listable = ADI__nullid;
ADIobj  K_LE = ADI__nullid;
ADIobj  K_LT = ADI__nullid;

ADIobj  K_Map = ADI__nullid;
ADIobj  K_Multiply = ADI__nullid;
ADIobj  K_MultiplyBy = ADI__nullid;

ADIobj  K_Negate = ADI__nullid;
ADIobj  K_Not = ADI__nullid;
ADIobj  K_NotEqual = ADI__nullid;

ADIobj  K_Or = ADI__nullid;

ADIobj  K_Pattern = ADI__nullid;
ADIobj  K_PatternTest = ADI__nullid;
ADIobj  K_Plus = ADI__nullid;
ADIobj  K_PostDec = ADI__nullid;
ADIobj  K_PostInc = ADI__nullid;
ADIobj  K_Power = ADI__nullid;
ADIobj  K_PreDec = ADI__nullid;
ADIobj  K_PreInc = ADI__nullid;
ADIobj  K_Put = ADI__nullid;

ADIobj  K_Query = ADI__nullid;

ADIobj  K_Raise = ADI__nullid;
ADIobj  K_Range = ADI__nullid;
ADIobj  K_ReRaise = ADI__nullid;

ADIobj  K_Set = ADI__nullid;
ADIobj  K_SetDelayed = ADI__nullid;
ADIobj  K_Subtract = ADI__nullid;
ADIobj  K_SubtractFrom = ADI__nullid;
ADIobj  K_Switch = ADI__nullid;

ADIobj  K_Try = ADI__nullid;

ADIobj  K_While = ADI__nullid;

void prsx_init( ADIstatus status )
  {
  DEFINE_CSTR_TABLE(stringtable)
    DEFINE_CSTR_TABLE_ENTRY(EXC_ArrayBound,"ArrayBound"),
    DEFINE_CSTR_TABLE_ENTRY(EXC_BoolExp,"BooleanExpected"),
    DEFINE_CSTR_TABLE_ENTRY(EXC_ControlC,"ControlC"),
    DEFINE_CSTR_TABLE_ENTRY(EXC_Error,"Error"),
    DEFINE_CSTR_TABLE_ENTRY(EXC_ExceedMaxRecurse,"ExceedMaxRecurse"),
    DEFINE_CSTR_TABLE_ENTRY(EXC_InvalidArg,"InvalidArg"),
    DEFINE_CSTR_TABLE_ENTRY(EXC_NoSuchField,"NoSuchField"),
    DEFINE_CSTR_TABLE_ENTRY(EXC_ScopeBreak,"ScopeBreak"),
    DEFINE_CSTR_TABLE_ENTRY(EXC_SyntaxError, "SyntaxError"),

    DEFINE_CSTR_TABLE_ENTRY(K_AddTo,"AddTo"),
    DEFINE_CSTR_TABLE_ENTRY(K_Alternatives,"Alternatives"),
    DEFINE_CSTR_TABLE_ENTRY(K_And,"And"),
    DEFINE_CSTR_TABLE_ENTRY(K_Apply,"Apply"),
    DEFINE_CSTR_TABLE_ENTRY(K_Array,"Array"),
    DEFINE_CSTR_TABLE_ENTRY(K_ArrayRef,"ArrayRef"),

    DEFINE_CSTR_TABLE_ENTRY(K_Blank,"Blank"),
    DEFINE_CSTR_TABLE_ENTRY(K_BlankSeq,"BlankSeq"),
    DEFINE_CSTR_TABLE_ENTRY(K_BlankNullSeq,"BlankNullSeq"),
    DEFINE_CSTR_TABLE_ENTRY(K_Break,"Break"),

    DEFINE_CSTR_TABLE_ENTRY(K_Catch,"Catch"),
    DEFINE_CSTR_TABLE_ENTRY(K_Concat,"Concat"),
    DEFINE_CSTR_TABLE_ENTRY(K_Condition,"Condition"),

    DEFINE_CSTR_TABLE_ENTRY(K_DefEnum,"DefEnum"),
    DEFINE_CSTR_TABLE_ENTRY(K_Divide,"Divide"),
    DEFINE_CSTR_TABLE_ENTRY(K_DivideBy,"DivideBy"),
    DEFINE_CSTR_TABLE_ENTRY(K_Dot,"Dot"),
    DEFINE_CSTR_TABLE_ENTRY(K_DoWhile,"DoWhile"),

    DEFINE_CSTR_TABLE_ENTRY(K_Echo,"Echo"),
    DEFINE_CSTR_TABLE_ENTRY(K_Equal,"Equal"),
    DEFINE_CSTR_TABLE_ENTRY(K_Factorial,"Factorial"),
    DEFINE_CSTR_TABLE_ENTRY(K_Finally,"Finally"),
    DEFINE_CSTR_TABLE_ENTRY(K_Foreach,"Foreach"),

    DEFINE_CSTR_TABLE_ENTRY(K_Get,"Get"),
    DEFINE_CSTR_TABLE_ENTRY(K_GE,"GreaterThanOrEqual"),
    DEFINE_CSTR_TABLE_ENTRY(K_GT,"GreaterThan"),

    DEFINE_CSTR_TABLE_ENTRY(K_HoldAll,"HoldAll"),
    DEFINE_CSTR_TABLE_ENTRY(K_HoldFirst,"HoldFirst"),
    DEFINE_CSTR_TABLE_ENTRY(K_HoldRest,"HoldRest"),

    DEFINE_CSTR_TABLE_ENTRY(K_If,"If"),

    DEFINE_CSTR_TABLE_ENTRY(K_List,"List"),
    DEFINE_CSTR_TABLE_ENTRY(K_Listable,"Listable"),
    DEFINE_CSTR_TABLE_ENTRY(K_LE,"LessThanOrEqual"),
    DEFINE_CSTR_TABLE_ENTRY(K_LT,"LessThan"),

    DEFINE_CSTR_TABLE_ENTRY(K_Map,"Map"),
    DEFINE_CSTR_TABLE_ENTRY(K_Multiply,"Multiply"),
    DEFINE_CSTR_TABLE_ENTRY(K_MultiplyBy,"MultiplyBy"),

    DEFINE_CSTR_TABLE_ENTRY(K_Negate,"Negate"),
    DEFINE_CSTR_TABLE_ENTRY(K_Not,"Not"),
    DEFINE_CSTR_TABLE_ENTRY(K_NotEqual,"NotEqual"),

    DEFINE_CSTR_TABLE_ENTRY(K_Or,"Or"),

    DEFINE_CSTR_TABLE_ENTRY(K_Pattern,"Pattern"),
    DEFINE_CSTR_TABLE_ENTRY(K_PatternTest,"PatternTest"),
    DEFINE_CSTR_TABLE_ENTRY(K_Plus,"Plus"),
    DEFINE_CSTR_TABLE_ENTRY(K_PostDec,"PostDecrement"),
    DEFINE_CSTR_TABLE_ENTRY(K_PostInc,"PostIncrement"),
    DEFINE_CSTR_TABLE_ENTRY(K_Power,"Power"),
    DEFINE_CSTR_TABLE_ENTRY(K_PreDec,"PreDecrement"),
    DEFINE_CSTR_TABLE_ENTRY(K_PreInc,"PreIncrement"),
    DEFINE_CSTR_TABLE_ENTRY(K_Put,"Put"),

    DEFINE_CSTR_TABLE_ENTRY(K_Query,"Query"),

    DEFINE_CSTR_TABLE_ENTRY(K_Raise,"Raise"),
    DEFINE_CSTR_TABLE_ENTRY(K_Range,"Range"),
    DEFINE_CSTR_TABLE_ENTRY(K_ReRaise,"ReRaise"),

    DEFINE_CSTR_TABLE_ENTRY(K_Set,"Set"),
    DEFINE_CSTR_TABLE_ENTRY(K_SetDelayed,"SetDelayed"),
    DEFINE_CSTR_TABLE_ENTRY(K_Subtract,"Subtract"),
    DEFINE_CSTR_TABLE_ENTRY(K_SubtractFrom,"SubtractFrom"),
    DEFINE_CSTR_TABLE_ENTRY(K_Switch,"Switch"),

    DEFINE_CSTR_TABLE_ENTRY(K_Try,"Try"),

    DEFINE_CSTR_TABLE_ENTRY(K_While,"While"),
  END_CSTR_TABLE;

  ADIkrnlAddCommonStrings( stringtable, status );

  }


#define FILEBUF 256

ADIobj prsx_symname( ADIobj stream, ADIstatus status )
  {
  ADIstream	*str = _strm_data(stream);

  _chk_stat_ret(ADI__nullid);

  return adix_cmn( str->ctok.dat, str->ctok.nc, status );
  }

/*
 * Parse a constant value
 */
ADIobj prsx_cvalue( ADIobj stream, ADIstatus status )
  {
  ADIstream	*str = _strm_data(stream);
  ADItokenType  ctok;
  ADIobj    	rval = ADI__nullid;

  _chk_stat_ret(ADI__nullid);

  ctok = str->ctok.t;

  switch( ctok ) {
    case TOK__CONST:
      adix_new_n( ADI__true, ADI__nullid, NULL,	/* Create object of correct type */
		  0, 0, NULL, NULL, _cdef_data(str->ctok.dt),
		  0, &rval, status );

      adic_put0c( rval, str->ctok.dat, status );
      break;

    case TOK__SYM:
      if ( ADIisTokenCstring( stream, "True", status ) )
	adic_newv0l( ADI__true, &rval, status );
      else if ( ADIisTokenCstring( stream, "Yes", status ) )
	adic_newv0l( ADI__true, &rval, status );
      else if ( ADIisTokenCstring( stream, "False", status ) )
	adic_newv0l( ADI__false, &rval, status );
      else if ( ADIisTokenCstring( stream, "No", status ) )
	adic_newv0l( ADI__false, &rval, status );
      else {
	}
      break;

    default:
      rval = ADI__nullid;
      adic_setecs( SAI__ERROR, "Invalid constant", status );
    }

/* Next token if no error */
  ADInextToken( stream, status );

  return rval;
  }


/*
 * Parse identifier=value pairs until no more. Values are written as
 * components of the structure object 'id'
 */
void prsx_namvalcmp( ADIobj stream, ADIobj id, ADIstatus status )
  {
  ADIobj    	cnam;
  ADIobj    	cval;
  ADIstream	*str = _strm_data(stream);

  _chk_stat;

  while ( (str->ctok.t == TOK__SYM) && 	/* While more identifiers */
		  _ok(status) ) {

    cnam = prsx_symname( stream, status ); /* Take the component name from stream */

    ADInextToken( stream, status );

    if ( ADIifMatchToken( stream, 	/* Assignment bit name=value ? */
		 TOK__ASSIGN, status )) {
      cval = prsx_cvalue( stream,		/* Take component value from stream */
		     status );

      adix_cputiid( id, cnam, cval,	/* Write new component to structure */
			  status );
      }
    else {				/* We've hit the end */
      adic_erase( &cnam, status );
      }
    }
  }


void ADIaddDeviceToStream( ADIobj stream, ADIdeviceType type, ADIstatus status )
  {
  ADIdevicePtr     dev;
  ADIstream	*str = _strm_data(stream);

  if ( _ok(status) ) {
    if ( str->dev )
      dev = (ADIdevicePtr)
	ADImemAlloc( sizeof(ADIdevice), status );
    else
      dev = &str->basedev;

    dev->last = str->dev;
    dev->ptr = NULL;
    dev->type = type;
    dev->dstatic = ADI__false;
    dev->pstatic = ADI__false;
    dev->bufsiz = dev->bnc = 0;
    str->dev = dev;
    }
  }


void ADIclearStreamInt( ADIstream *stream, ADIstatus status )
  {
  if ( _ok(status) ) {
    stream->nc_pb = 0;
    stream->nt_pb = 0;
    stream->dev = NULL;
    stream->flags &= (ADI_STREAM__MODEBITS^0xFFFF);
    }
  }

void ADIclearStream( ADIobj stream, ADIstatus status )
  {
  ADIclearStreamInt( _strm_data(stream), status );
  }


int ADIgetStreamAttrs( ADIobj stream, ADIstatus status )
  {
  return _ok(status) ? _strm_data(stream)->flags : 0;
  }


void ADIputStreamAttrs( ADIobj stream, int flags, ADIstatus status )
  {
  if ( _ok(status) )
    _strm_data(stream)->flags = flags;
  }

int ADIsetStreamAttr( ADIobj stream, int flag, ADIstatus status )
  {
  int		oflags = 0;

  if ( _ok(status) ) {
    oflags = _strm_data(stream)->flags;
    _strm_data(stream)->flags |= flag;
    }

  return oflags;
  }


/*
 * Construct a new stream of undefined type
 */
ADIobj ADIstrmNew( char *mode, ADIstatus status )
  {
  int		modemask = 0;
  ADIobj	str = ADI__nullid;

  if ( _ok(status) ) {
    str = adix_cls_alloc( &KT_DEFN_strm, status );
    ADIclearStream( str, status );
    if ( (*mode == 'r') || (*mode == 'R') )
      modemask = ADI_STREAM__IN;
    else if ( (*mode == 'w') || (*mode == 'W') )
      modemask = ADI_STREAM__OUT;
    _strm_data(str)->flags = modemask;
    }

  return str;
  }


void ADIstrmGetTokenData( ADIobj stream, char **data, int *len,
			  ADIstatus status )
  {
  ADIstream	*str = _strm_data(stream);
  *data = str->ctok.dat;
  if ( len ) *len = str->ctok.nc;
  }


/*
 * Flush a device
 */
void ADIstrmFlushDev( ADIdevice *dev, ADIstatus status )
  {
/* Null terminate so that we can use the fputs call rather than */
/* many fputc's. This is always safe to do because the buffer is */
/* always allocated one byte bigger than dev->bufsiz */
  dev->buf[dev->bnc] = 0;
  switch( dev->type ) {
    case ADIdevFile:
      fputs( dev->buf, dev->f );
      break;
    }

  dev->bnc = 0;
  }


/*
 * Flush a stream
 */
void ADIstrmFlush( ADIobj stream, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIstream	*pstr = _strm_data(stream);

    if ( pstr->dev->bufsiz )
      ADIstrmFlushDev( pstr->dev, status );
    }
  }

void ADIstrmPutInt( ADIdevice *dev, char *str, int slen, ADIstatus status )
  {
  char	*buf = dev->buf;
  int	i;
  int	eblen = dev->bufsiz - 1;	/* Useful buffer length */
  char	*scur = str;
  int	sleft = eblen - dev->bnc;	/* Amount of space left */

/* Null terminated input? */
  if ( slen == _CSM ) {
    while ( *scur ) {
      while ( *scur && (dev->bnc < eblen) ) {
	buf[dev->bnc++] = *scur++;
	}
      if ( *scur )
	ADIstrmFlushDev( dev, status );
      }
    }

/* Fast check if we won't fill the device buffer */
  else if ( slen <= sleft ) {
    for( i=0; i<slen; i++ )
      buf[dev->bnc++] = *scur++;
    }

/* We will fill the buffer (and maybe more than once) */
  else {
    int		nmove,nleft = slen;

    while ( nleft > 0 ) {
      nmove = _MIN(sleft,nleft);
      for( i=0; i<nmove; i++ )
	buf[dev->bnc++] = *scur++;
      ADIstrmFlushDev( dev, status );
      nleft -= sleft;
      sleft = eblen;
      }
    }
  }

void ADIstrmPutCh( ADIobj stream, char ch, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIdevice	*dev = _strm_data(stream)->dev;

    if ( dev->bnc == (dev->bufsiz-1) )
      ADIstrmFlushDev( dev, status );
    dev->buf[dev->bnc++] = ch;
    }
  }

void ADIstrmPutBlnk( ADIdevice *dev, int nb, ADIstatus status )
  {
  static char *blanks = "                                        ";
  int	lnb = nb;

  while ( lnb > 0 ) {
    ADIstrmPutInt( dev, blanks, _MIN(40,lnb), status );
    lnb -= 40;
    }
  }


/*
 * 'printf' like formatting function. Supports the following formatting
 * codes
 *
 *   %%		- the percent character
 *   %d		- decimal integer
 *   %c		- a single character
 *   %s		- null terminated C string
 *   %O		- any ADI object
 *   %S		- an ADI string
 *   %I		- ADIinteger
 *   \n		- new line
 *   \t		- tab character
 *
 * Writes data into user supplied buffer. If the buffer fills up then
 * the user supplied flushing function is invoked
 */
void ADIstrmVprintf( ADIobj stream, char *format, int flen,
		     va_list ap, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIinteger	ai_arg;
    ADIobj	as_arg;
    ADIobj	*asp_arg;
    char	*cp_arg;
    double	d_arg;
    char	fc;
    char	fct;
    char	*fptr = format;
    int		i_arg;
    long	l_arg;
    char	*lbegin = NULL;
    char	lbuf[30];
    int		nb;
    int		nlit = 0;
    int		npr;
    int		prec;
    ADIstream	*pstr = _strm_data(stream);
    ADIdevice	*dev = pstr->dev;
    ADIstring	*sdata;
    short	sh_arg;
    void	*vp_arg;

/* Loop over the format */
    while ( *fptr ) {

/* Extract the next format character */
      fc = *fptr++;

/* Formatting? */
      if ( fc == '%' ) {

/* No trailing blanks by default */
	nb = 0;

/* Extract the next format character */
	fct = *fptr++;

/* Flush literal data to buffer */
	if ( nlit ) {
	  ADIstrmPutInt( dev, lbegin, nlit, status ); nlit = 0;
	  }

/* Is precision being specified? */
	if ( fct == '*' ) {
	  prec = va_arg(ap,int);
	  fct = *fptr++;
	  }
	else if ( isdigit( fct ) ) {
	  int	lprec = 0;
	  do {
	    lprec = lprec*10 + (fct-'0');
	    fct = *fptr++;
	    }
	  while ( isdigit(fct) );
	  prec = lprec;
	  }
	else
	  prec = 0;

/* Switch on the control character */
	switch ( fct ) {
	  case 's':
	    cp_arg = va_arg(ap,char *);
	    if ( prec ) {
	      npr = strlen( cp_arg );
	      if ( prec > npr )
		nb = prec - npr;
	      else
		npr = prec;
	      }
	    else
	      npr = _CSM;
	    if ( npr )
	      ADIstrmPutInt( dev, cp_arg, npr, status );
	    break;
	  case 'S':
	    as_arg = va_arg(ap,ADIobj);
	    sdata = _str_data(as_arg);
	    npr = sdata->len;
	    if ( prec ) {
	      if ( prec > npr )
		nb = prec - npr;
	      else
		npr = prec;
	      }
	    if ( npr )
	      ADIstrmPutInt( dev, sdata->data, npr, status );
	    break;
	  case 'd':
	    i_arg = va_arg(ap,int);
	    sprintf( lbuf, "%d", i_arg );
	    ADIstrmPutInt( dev, lbuf, _CSM, status );
	    break;
	  case 'c':
	    sh_arg = va_arg(ap,short);
	    fct = sh_arg;
	    ADIstrmPutInt( dev, &fct, 1, status );
	    break;
	  case 'O':
	    as_arg = va_arg(ap,ADIobj);
	    adix_print( stream, as_arg, 1, ADI__true, status );
	    break;
	  case 'p':
	    vp_arg = va_arg(ap,void *);
	    sprintf( lbuf, "%p", vp_arg );
	    ADIstrmPutInt( dev, lbuf, _CSM, status );
	    break;
	  case 'I':
	    ai_arg = va_arg(ap,ADIinteger);
	    sprintf( lbuf, "%ld", ai_arg );
	    ADIstrmPutInt( dev, lbuf, _CSM, status );
	    break;
	  case 'f':
	    d_arg = va_arg(ap,double);
	    sprintf( lbuf, "%f", d_arg );
	    ADIstrmPutInt( dev, lbuf, _CSM, status );
	    break;
	  case '%':
	    ADIstrmPutInt( dev, &fct, 1, status );
	    break;
	  case 'L':
            nitem = va_arg(ap,int);
	    asp_arg = va_arg(ap,ADIobj *);
            while ( nitem-- ) {
	      adix_print( stream, asp_arg++, 1, ADI__true, status );
              if ( nitem ) {
                fct = ',';
	        ADIstrmPutInt( dev, fct, 1, status );
                }
              }
	    break;
	  }

	if ( nb )
	  ADIstrmPutBlnk( dev, nb, status );
	}

/* Special characters */
      else if ( fc == '\\' ) {

/* Flush literal data to buffer */
	if ( nlit ) {
	  ADIstrmPutInt( dev, lbegin, nlit, status ); nlit = 0;
	  }

	}

/* Otherwise literal */
      else {
	if ( ! nlit++ )
	  lbegin = fptr - 1;
	}
      }

/* Flush literal data */
    if ( nlit ) {
      ADIstrmPutInt( dev, lbegin, nlit, status ); nlit = 0;
      }
    }
  }


void ADIstrmPrintf( ADIobj stream, char *format, ADIstatus status, ... )
  {
  va_list	ap;

/* Start variable arg processing */
  va_start(ap,status);

/* Invoke the internal routine */
  ADIstrmVprintf( stream, format, _CSM, ap, status );

/* End variable arg list processing */
  va_end(ap);
  }


void ADIdropDeviceInt( ADIstream *str, ADIstatus status )
  {
  ADIdevicePtr     dev = str->dev;

  if ( _ok(status) ) {
    if ( dev ) {
      if ( dev->type == ADIdevFile ) {
	if ( dev->bufsiz ) {
	  if ( dev->bnc )
	    ADIstrmFlushDev( dev, status );
	  ADImemFree( dev->buf, dev->bufsiz, status );
	  }
	if ( ! dev->pstatic )
	  fclose( dev->f );
	}
      else if ( dev->type == ADIdevCin )
	ADImemFree( dev->buf, dev->bufsiz, status );
      str->dev = dev->last;
      if ( dev != &str->basedev )
	ADImemFree( dev->buf, sizeof(ADIdevice), status );
/*      if ( str->dev )
	ADInextToken( str, status ); */
      }
    else
      *status = SAI__ERROR;
    }
  }

void ADIdropDevice( ADIobj stream, ADIstatus status )
  {
  ADIdropDeviceInt( _strm_data(stream), status );
  }

void ADIresetStreamInt( ADIstream *stream, ADIstatus status )
  {
  while ( _ok(status) && stream->dev )
    ADIdropDeviceInt( stream, status );

  ADIclearStreamInt( stream, status );
  }

void ADIresetStream( ADIobj stream, ADIstatus status )
  {
  ADIresetStreamInt( _strm_data(stream), status );
  }


void ADIstrmDel( ADIobj stream, ADIstatus status )
  {
  ADIstream	*str = _strm_data(stream);

  if ( str->dev )
    ADIresetStreamInt( str, status );
  }


ADIobj ADIstrmExtendC( ADIobj stream, char *buf, int blen, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIstream	*str = _strm_data(stream);

    ADIaddDeviceToStream( stream, ADIdevCstring, status );
    str->dev->ptr = str->dev->buf = buf;
    str->dev->bufsiz = blen;
    str->dev->pstatic = ADI__true;
    }

  return stream;
  }

ADIobj ADIstrmExtendCst( ADIobj stream, char *buf, int blen, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIstream	*str = _strm_data(stream);

    ADIaddDeviceToStream( stream, ADIdevCstring, status );
    str->dev->ptr = str->dev->buf = buf;
    str->dev->bufsiz = blen;
    str->dev->pstatic = ADI__true;
    str->dev->dstatic = ADI__true;
    }

  return stream;
  }


ADIobj ADIstrmExtendCin( ADIobj stream, int bufsiz, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIstream	*str = _strm_data(stream);

    ADIaddDeviceToStream( stream, ADIdevCin, status );
    str->dev->bufsiz = bufsiz;
    str->dev->buf = (char *) ADImemAlloc( (size_t) bufsiz, status );
    str->dev->buf[0] = '\0';
    str->dev->ptr = str->dev->buf;
    }

  return stream;
  }


ADIobj ADIstrmExtendFile( ADIobj stream, FILE *f, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIstream	*str = _strm_data(stream);

    ADIaddDeviceToStream( stream, ADIdevFile, status );

    str->dev->f = f;
    str->dev->pstatic = ADI__true;
    str->dev->buf = ADImemAlloc( FILEBUF, status );
    str->dev->bufsiz = FILEBUF;
    }

  return stream;
  }


char ADIreadCharFromStream( ADIobj stream, ADIstatus status )
  {
  ADIstream	*str = _strm_data(stream);
  char          ch = '\0';
  int           gotit = ADI__false;

  while ( _ok(status) && ! gotit ) {
    if ( str->nc_pb ) {
      ch = str->c_pb[--str->nc_pb]; gotit = ADI__true;
      }
    else if ( str->dev ) {
      switch( str->dev->type ) {
	case ADIdevCstring:
	  ch = *(str->dev->ptr++); gotit = ADI__true;
	  break;
	case ADIdevFile:
	  if ( feof(str->dev->f) )
	    ADIdropDevice( stream, status );
	  else {
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
	}
      }
    else
      *status = SAI__ERROR;
    }

  str->ctok.dat[str->ctok.nc++] = ch;
  return ch;
  }


void ADIreturnCharToStream( ADIobj stream, char ch, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIstream	*str = _strm_data(stream);
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


ADIlogical ADIisTokenCstring( ADIobj stream, char *string, ADIstatus status )
  {
  ADIstream	*str = _strm_data(stream);

  _chk_stat_ret(ADI__false);

  if ( strx_cmp2c( str->ctok.dat, str->ctok.nc,
		   string, strlen(string) ) )
    return ADI__false;
  else
    return ADI__true;
  }


ADItokenType ADIisTokenInSet( ADIobj stream, ADItokenType tlist[],
			      ADIstatus status )
  {
  int                   it;
  ADItokenType  	ct;
  ADItokenType  	rtype = TOK__NULL;

  if ( !_ok(status) )
    return rtype;                       /* Check status */

  ct = ADIcurrentToken( stream, status );

  for ( it =0;
	((tlist[it]!=TOK__TRAP) && (tlist[it]!=ct)) ;
	it++ ) ;

  if ( tlist[it]==TOK__TRAP )
    return TOK__NULL;
  else
    return tlist[it];
  }

int ADIparseTokenInOpSet( ADIobj stream, ADIoperatorDescrip tlist[],
			  ADIstatus status )
  {
  int                   it;
  ADIstream		*str = _strm_data(stream);

  if ( !_ok(status) )                   /* Check status */
    return 0;

  for ( it =0;
	((tlist[it].tok!=TOK__TRAP) && (tlist[it].tok!=str->ctok.t)) ;
	it++ ) ;

  if ( tlist[it].tok==TOK__TRAP )
    return 0;
  else
    return it+1;
  }


#define EOS -1

ADItokenType ADIcurrentToken( ADIobj stream, ADIstatus status )
  {
  return _ok(status) ? _strm_data(stream)->ctok.t : TOK__NOTATOK;
  }


ADItokenType ADInextToken( ADIobj stream, ADIstatus status )
  {
  ADItokenType		etok;
  ADItokenType		tok = TOK__NOTATOK;
  char           	ch;
  ADIlogical        	inquotes = ADI__true;
  ADIstream		*str = _strm_data(stream);

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
    ch = ADIreadCharFromStream( stream, status );

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
	ch = ADIreadCharFromStream( stream, status );
	}
      ADIreturnCharToStream( stream, ch, status );
      str->ctok.dat[str->ctok.nc] = '\0';
      tok = TOK__SYM;
      str->ctok.dt = UT_ALLOC_c;
      }

    else if ( isdigit(ch) )
      {
      str->ctok.ptr0 = str->dev->ptr - 1;
      while( isalnum(ch) )
	ch = ADIreadCharFromStream( stream, status );

      if ( ch == '.' )                	/* Decimal point found */
	{
	int	ndig = 0;

	ch = ADIreadCharFromStream( stream, status );
	while( isdigit(ch) ) {
	  ndig++;
	  ch = ADIreadCharFromStream( stream, status );
	  }

	if ( ndig > 7 )
	  str->ctok.dt = UT_ALLOC_d;
	else
	  str->ctok.dt = UT_ALLOC_r;
	}
      else
	str->ctok.dt = UT_ALLOC_i;

      ADIreturnCharToStream( stream, ch, status );
      str->ctok.dat[str->ctok.nc] = '\0';
      tok = TOK__CONST;
      }

    else if ( ch == '#' )		/* Comments return TOK__END */
      {
      do
	ch = ADIreadCharFromStream( stream, status );
      while ( (ch != '\n') && _ok(status) );
      tok = etok;
      }
    else if ( ch == '"' )
      {
      str->ctok.nc = 0;
      while( inquotes )
	{
	ch = ADIreadCharFromStream( stream, status );
	switch( ch )
	  {
	  case '\\':
	    ch = ADIreadCharFromStream( stream, status );
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
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '=' )
	    tok = TOK__EQ;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__ASSIGN;
	    }
	  break;
	case '_':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '_' )
	    {
	    ch = ADIreadCharFromStream( stream, status );
	    if ( ch == '_' )
	      tok = TOK__UND3;
	    else {
	      ADIreturnCharToStream( stream, ch, status );
	      tok = TOK__UND2;
	      }
	    }
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__UND1;
	    }
	  break;
	case '+':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '+' )
	    tok = TOK__INCR;
	  else if ( ch == '=' )
	    tok = TOK__PLUSEQ;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__PLUS;
	    }
	  break;
	case '-':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '-' )
	    tok = TOK__DECR;
	  else if ( ch == '=' )
	    tok = TOK__MINUSEQ;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__MINUS;
	    }
	  break;
	case '*':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '*' )
	    tok = TOK__CARAT;
	  else if ( ch == '=' )
	    tok = TOK__MULEQ;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__MUL;
	    }
	  break;
	case '?':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '?' )
	    tok = TOK__QUERY2;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__QUERY;
	    }
	  break;
	case '$':
	  tok = TOK__DOLLAR;
	  break;
	case '/':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '=' )
	    tok = TOK__DIVEQ;
	  else if ( ch == '/' )
	    tok = TOK__CONC;
	  else if ( ch == '@' )
	    tok = TOK__SLASHAT;
	  else if ( ch == ';' )
	    tok = TOK__COND;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__DIV;
	    }
	  break;
	case ';':
	  ch = ADIreadCharFromStream( stream, status );
          if ( ch == ';' ) {
            do
	      ch = ADIreadCharFromStream( stream, status );
            while ( (ch != '\n') && _ok(status) );
            tok = etok;
            }
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__SEMICOLON;
	    }
	  break;
	case '@':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '@' )
	    tok = TOK__AT;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__AT;
	    }
	  break;
	case ':':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == ':' )
	    tok = TOK__SCOPE;
	  else if ( ch == '=' )
	    tok = TOK__COLONEQ;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__COLON;
	    }
	  break;
	case '|':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '|' )
	    tok = TOK__OR;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__BAR;
	    }
	  break;
	case '!':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '=' )
	    tok = TOK__NE;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
	    tok = TOK__NOT;
	    }
	  break;
	case '&':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '&' )
	    tok = TOK__AND;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
/*	    EOS_ERR( EOS__INVEXPSYN, status ); */
	    }
	  break;
	case '>':
	  ch = ADIreadCharFromStream( stream, status );
	  switch( ch ) {
	    case '=':
	      tok = TOK__GE;
	      break;
	    case '>':
	      ch = ADIreadCharFromStream( stream, status );
	      if ( ch == '>' )
		tok = TOK__RCHEV3;
	      else
		tok = TOK__RCHEV;
	      break;
	    default:
	      ADIreturnCharToStream( stream, ch, status );
	      tok = TOK__GT;
	      break;
	    }
	  break;
	case '\\':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '\n' )		/* Continuation character */
	    {;}
	  else if ( ch == '\\' )
	    tok = TOK__BSLASH;
	  break;
	case '<':
	  ch = ADIreadCharFromStream( stream, status );
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
	      ADIreturnCharToStream( stream, ch, status );
	      tok = TOK__LT;
	    }
	  break;
	case '^':
	  ch = ADIreadCharFromStream( stream, status );
	  if ( ch == '^' )
	    tok = TOK__CARAT2;
	  else {
	    ADIreturnCharToStream( stream, ch, status );
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


void ADIparseError( ADIobj stream, int control,
		    char *message, ADIstatus status )
  {
  char          *str;
  int           len;

  if ( control ) {
    ADIdescribeToken( _strm_data(stream)->ctok.t, &str, &len );
    adic_setetc( "CTOK", str, len );
    }

/* Report the error */
  adic_setecs( ADI__SYNTAX, message, status );
  }

void ADImatchToken( ADIobj stream, ADItokenType tok, ADIstatus status )
  {
  char          *str;
  int           len;

  if ( _ok(status) ) {
    ADItokenType	ctok = _strm_data(stream)->ctok.t;

    if ( ctok == tok )
      ADInextToken( stream, status );
    else {
      ADIdescribeToken( ctok, &str, &len );
      adic_setetc( "TTOK", str, len );
      ADIparseError( stream, 1, "^CTOK found where ^TTOK expected.", status );
      }
    }
  }

ADIlogical ADIifMatchToken( ADIobj str, ADItokenType t, ADIstatus status )
  {
  if ( _strm_data(str)->ctok.t == t ) {
    (void) ADInextToken( str, status );
    return ADI__true;
    }
  else
    return ADI__false;
  }


/*  ADIparseComDelList - Parses a comma separated list of expressions
 *
 *  Parses a list of expressions separated by commas, and terminated by
 *  'endtok'. The list may be empty.
 */
ADIobj ADIparseComDelList( ADIobj pstream, ADItokenType endtok,
			   ADIstatus status )
  {
  ADIobj	robj = ADI__nullid;	/* Returned object */
  ADIobj     	*lastlinkadr = &robj;   /* Address of last list link cell */
  ADIlogical    more = ADI__true;	/* More items in list */
  ADIobj     	thisarg;                /* Running argument ptr */
  int		tlen;			/* Length of token description */
  char		*tstr;		        /* Token description */

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  do {
    if ( _strm_data(pstream)->ctok.t == endtok ) {
      if ( _null_q(robj) )
	robj = lstx_cell( ADI__nullid, ADI__nullid, status );
      more = ADI__false;
      }
    else {
      thisarg = lstx_cell( ADI__nullid,	/* Create list cell */
		ADI__nullid, status );

      *lastlinkadr = thisarg;
      lastlinkadr = &_CDR(thisarg);

      _CAR(thisarg) = ADIparseExpInt(	/* Parse list element */
	       pstream, 10, status );

      if ( ! ADIifMatchToken( pstream, TOK__COMMA, status ) ) {
	more = ADI__false;
	if ( _strm_data(pstream)->ctok.t != endtok ) {
	  ADIdescribeToken( endtok, &tstr, &tlen );
	  adic_setetc( "TOK", tstr, tlen );
	  adic_setecs( ADI__SYNTAX, "Error reading list - comma or ^TOK expected",
							       status );
	  }
	}

      if ( !_ok(status) )               /* Remove partially parsed list */
	adic_erase( &robj, status );
      }
    }
  while ( more && _ok(status) );

  ADImatchToken( pstream, endtok, status );

  return robj;
  }


ADIobj ADIparseBlankExp( ADIobj stream, ADIstatus status )
  {
  ADIobj                arg;            /* Argument to blank function */
  int                   nbi;            /* Number of blanks minus one */
  ADIobj                nstr;           /* Name string for _,__,___ */
  ADIstream		*str = _strm_data(stream);
  ADIobj                subexp = ADI__nullid;  /* Sub-expression */

  if ( str->ctok.t == TOK__UND1 )       /* Choose Blank form */
    {nbi = 0; nstr = K_Blank;}
  else if ( str->ctok.t == TOK__UND2 )
    {nbi = 1; nstr = K_BlankSeq;}
  else
    {nbi = 2; nstr = K_BlankNullSeq;}

/* Skip the blank sequence */
  ADInextToken( stream, status );

  if ( str->ctok.t == TOK__SYM )
    arg = lstx_cell(
	    ADIparseExpInt( stream, 960, status ),
	    ADI__nullid, status );
  else
    arg = adix_clone( ADIcvNulCons, status );

/* Single _x type node? */
  if ( _valid_q(_CAR(arg)) && _null_q(_CDR(arg)) )
    if ( _etn_q(_CAR(arg)) ) {          /* Check <x> is an expression node */
      ADIobj			cid;	/* Class definition */

/* Locate class definition of named class, if any */
      cid = ADIkrnlFindClsI( _etn_head(_CAR(arg)), status );
      if ( _valid_q(cid) ) {

/* Locate address of the slot for nbi'th underscore expression */
	ADIobj	*saddr = _cdef_data(cid)->bexpr + nbi;

	subexp = *saddr;

/* Sub-expression already exists? */
	if ( *saddr )
	  adic_erase( &arg, status );
	else
	  *saddr = ADIetnNew( nstr, arg, status );

	subexp = adix_clone( *saddr, status );
	}
      }

/* Set return value, either the common sub-expression or a new */
/* expression node */
  if ( _valid_q(subexp) )
    return subexp;
  else
    return ADIetnNew( nstr, arg, status );
  }

ADIinteger ADIparseScanInt( char *data, int base, ADIstatus status )
  {
  char		*cp = data;
  int		dig;
  ADIinteger	result = 0;

  if ( !_ok(status) )
    return 0;

  while ( *cp && _ok(status) ) {
    if ( (base>10) && (*cp>'9') ) {
      if ( isalpha(*cp) )
	dig = (_toupper(*cp)-'A')+10;
      else {
	adic_setecs( ADI__SYNTAX,
	  "Character /%c/ is not valid digit in any base", status, cp );
	}
      }
    else
      dig = *cp++ - '0';
    if ( dig >= base ) {
      adic_setecs( ADI__SYNTAX, "Digit /%c/ is not a valid base %d digit",
	status, cp, base );
      }
    else
      result = (result*base + dig);
    }

  return result;
  }


ADIobj ADIparseExpInt( ADIobj stream, int priority, ADIstatus status )
  {
  DEFINE_OP_TABLE(PreUnary)
    DEFINE_OP_TABLE_ENTRY(TOK__LCHEV,  950,  None,  K_Get ),
    DEFINE_OP_TABLE_ENTRY(TOK__DECR,   900,  None,  K_PreDec),
    DEFINE_OP_TABLE_ENTRY(TOK__INCR,   900,  None,  K_PreInc),
    DEFINE_OP_TABLE_ENTRY(TOK__MINUS,  810,  None,  K_Negate),
    DEFINE_OP_TABLE_ENTRY(TOK__PLUS,   800,  None,  K_Plus),
    DEFINE_OP_TABLE_ENTRY(TOK__NOT,    560,  None,  K_Not),
  END_OP_TABLE;

  DEFINE_OP_TABLE(Binary)
    DEFINE_OP_TABLE_ENTRY(TOK__QUERY,  930,  None,  K_PatternTest),
    DEFINE_OP_TABLE_ENTRY(TOK__SLASHAT,890,  RtoL,  K_Map),
    DEFINE_OP_TABLE_ENTRY(TOK__ATAT,   890,  RtoL,  K_Apply),
    DEFINE_OP_TABLE_ENTRY(TOK__PERIOD, 860,  None,  K_Dot),
    DEFINE_OP_TABLE_ENTRY(TOK__CARAT,  850,  RtoL,  K_Power),
    DEFINE_OP_TABLE_ENTRY(TOK__DIV,    770,  LtoR,  K_Divide),
    DEFINE_OP_TABLE_ENTRY(TOK__MUL,    750,  LtoR,  K_Multiply),
    DEFINE_OP_TABLE_ENTRY(TOK__PLUS,   700,  LtoR,  K_Plus),
    DEFINE_OP_TABLE_ENTRY(TOK__MINUS,  690,  LtoR,  K_Subtract),
    DEFINE_OP_TABLE_ENTRY(TOK__EQ,     600,  None,  K_Equal),
    DEFINE_OP_TABLE_ENTRY(TOK__NE,     600,  None,  K_NotEqual),
    DEFINE_OP_TABLE_ENTRY(TOK__LE,     600,  None,  K_LE),
    DEFINE_OP_TABLE_ENTRY(TOK__LT,     600,  None,  K_LT),
    DEFINE_OP_TABLE_ENTRY(TOK__GE,     600,  None,  K_GE),
    DEFINE_OP_TABLE_ENTRY(TOK__GT,     600,  None,  K_GT),
    DEFINE_OP_TABLE_ENTRY(TOK__AND,    450,  None,  K_And),
    DEFINE_OP_TABLE_ENTRY(TOK__OR,     440,  None,  K_Or),
    DEFINE_OP_TABLE_ENTRY(TOK__BAR,    430,  None,  K_Alternatives),
    DEFINE_OP_TABLE_ENTRY(TOK__COND,   420,  None,  K_Condition),
    DEFINE_OP_TABLE_ENTRY(TOK__PLUSEQ, 230,  None,  K_AddTo),
    DEFINE_OP_TABLE_ENTRY(TOK__DIVEQ,  230,  None,  K_DivideBy),
    DEFINE_OP_TABLE_ENTRY(TOK__MINUSEQ,220,  None,  K_SubtractFrom),
    DEFINE_OP_TABLE_ENTRY(TOK__MULEQ,  210,  None,  K_MultiplyBy),
    DEFINE_OP_TABLE_ENTRY(TOK__ASSIGN, 100,  RtoL,  K_Set),
    DEFINE_OP_TABLE_ENTRY(TOK__COLONEQ,95,   None,  K_SetDelayed),
    DEFINE_OP_TABLE_ENTRY(TOK__RCHEV,  50,   None,  K_Put),
    DEFINE_OP_TABLE_ENTRY(TOK__CONC,   25,   RtoL,  K_Concat),
  END_OP_TABLE;

  DEFINE_OP_TABLE(PostUnary)
    DEFINE_OP_TABLE_ENTRY(TOK__DECR,   910,  None,  K_PostDec),
    DEFINE_OP_TABLE_ENTRY(TOK__INCR,   910,  None,  K_PostInc),
    DEFINE_OP_TABLE_ENTRY(TOK__NOT,    880,  None,  K_Factorial),
  END_OP_TABLE;

  int          base;                    /* Base for reading integers */
  ADIdouble    dval;                    /* Double precision constant */
  ADIobj       expr = ADI__nullid;      /* Expression to be returned */
  ADIlogical   finished = ADI__false;
  ADIobj       first;
  ADItoken     idata;
  ADIinteger   ival;                    /* Integer constant */
  int          ltp;
  ADIlogical 	next;
  ADIstream    *str = _strm_data(stream);
  ADIobj       symbol;
  int          tp;

  if ( !_ok(status) )                   /* Check status */
    return ADI__nullid;

  switch ( str->ctok.t ) {
    case TOK__UND1:
    case TOK__UND2:
    case TOK__UND3:
      expr = ADIparseBlankExp( stream, status );
      break;

    case TOK__SYM:
    case TOK__SCOPE:
      if ( str->dev->dstatic )
	expr = ADIetnNew(
		adix_cmn_i( str->ctok.ptr0, str->ctok.nc,
			    ADI__true, status ),
		ADI__nullid, status );
      else
	expr = ADIetnNew(
		adix_cmn( str->ctok.dat, str->ctok.nc, status ),
		ADI__nullid, status );
      ADInextToken( stream, status );

/* Left parenthesis indicates start of function argument list */
      if ( ADIifMatchToken( stream, TOK__LPAREN, status ) )
	_etn_args(expr) = ADIparseComDelList( stream, TOK__RPAREN, status );

/* Underscore sequences indicate start of argument constraint spec */
      else if ( (str->ctok.t == TOK__UND1) ||
		(str->ctok.t == TOK__UND2) ||
		(str->ctok.t == TOK__UND2) ) {
	first = expr;
	expr = ADIparseBlankExp( stream, status );

	expr = ADIetnNew( K_Pattern, lstx_new2( first, expr, status),
		     status );
	}

/* Left square bracket indicates start of array reference */
      else if ( ADIifMatchToken( stream, TOK__LBRAK, status ) ) {
	symbol = expr;                  /* Store symbol for later */

	expr = ADIetnNew( K_ArrayRef,
	      ADIparseComDelList( stream, TOK__RBRAK, status ),
	      status );

/* Splice symbol into arg list */
	_etn_args(expr) = lstx_cell( symbol, _etn_args(expr), status );
	}
      break;

    case TOK__LBRAK:
      ADInextToken( stream, status );
      expr = ADIetnNew( K_List,
	       ADIparseComDelList( stream,   /* Parse list elements */
		 TOK__RBRAK, status ),
	       status );
      expr = ADIetnNew( K_Array,
		     lstx_cell( expr, ADI__nullid, status ),
		     status );
      break;

    case TOK__LBRACE:
      ADInextToken( stream, status );
      expr = ADIetnNew( K_List, /* Create List(e1..) constructor */
	  ADIparseComDelList( stream,
	    TOK__RBRACE, status ),
	  status );
      break;

    case TOK__LPAREN:
      ADInextToken( stream, status );
      expr = ADIparseExpInt( stream, 0, status );
      ADImatchToken( stream, TOK__RPAREN, status );
      break;

    case TOK__END:
      finished = ADI__true;
      break;

    case TOK__CONST:
      next = ADI__true;
      switch ( str->ctok.dt ) {
	case UT_CODE_i:
	  base = 10;
	  idata = str->ctok;
	  ADInextToken( stream, status );
	  if ( str->ctok.t == TOK__CARAT2 ) {
	    ADInextToken( stream, status );
	    if ( (str->ctok.t != TOK__CONST) ||
		 (str->ctok.dt != UT_CODE_i ) )
	      adic_setecs( ADI__SYNTAX, "Numeric base expected", status );
	    else {
	      base = (int) ADIparseScanInt( str->ctok.dat, 10, status );
	      if ( _ok(status) && (base<2) && (base>36) ) {
		adic_seteti( "B", base );
		adic_setecs( ADI__SYNTAX, "Invalid base specifier /^B/ - must lie between 2 and 36", status );
		}
	      }
	    }
	  else
	    next = ADI__false;
	  ival = ADIparseScanInt( idata.dat, base, status );
	  adic_newv0i( ival, &expr, status );
	  break;
	case UT_CODE_d:
	  if ( ! sscanf(str->ctok.dat,"%lf",&dval) )
	    *status = SAI__ERROR;
	  else
	    adic_newv0d( dval, &expr, status );
	  break;
	case UT_CODE_c:
	  adic_newv0c( str->ctok.dat, &expr, status );
	  break;
	default:
	  adic_newv0c( str->ctok.dat, &expr, status );
	  break;
	}
      if ( next )
	ADInextToken( stream, status );
      break;
    default:
      tp = ADIparseTokenInOpSet( stream, PreUnary, status);
      if ( tp ) {
	if ( PreUnary[tp-1].priority >= priority ) {
	  ADInextToken( stream, status );
	  expr = ADIetnNew( *PreUnary[tp-1].key,
		     lstx_cell(
		       ADIparseExpInt( stream, PreUnary[tp-1].priority, status ),
		       ADI__nullid, status ),
		     status );
	  }
	}
      break;
    }

/* First argument ok? */
  if ( _valid_q(expr) ) {
    tp = ADIparseTokenInOpSet( stream, Binary, status );
    while ( tp && _ok(status) && ! finished ) {
      if ( Binary[tp-1].priority >= priority ) {
	ltp = tp;
	first = ADI__nullid;
	while ( (ltp == tp) && _ok(status) ) {
	  expr = ADIetnNew(
		     *Binary[tp-1].key,
		     lstx_cell( expr, ADI__nullid, status ),
		     status );
	  ADInextToken( stream, status );
	  if ( Binary[tp-1].assoc == RtoL ) {
	    if ( _null_q(first) )
	      first = expr;
	    _CDR(_etn_args(expr)) = lstx_cell(
		  ADIparseExpInt( stream, Binary[tp-1].priority, status ),
		  ADI__nullid, status );
	    }
	  else
	    _CDR(_etn_args(expr)) = lstx_cell(
		  ADIparseExpInt( stream, Binary[tp-1].priority+1, status ),
		  ADI__nullid, status );

	  tp = ADIparseTokenInOpSet( stream, Binary, status );
	  }
	if ( Binary[ltp-1].assoc == RtoL )
	  expr = first;
	}
      else
	finished = ADI__true;
      }

    tp = ADIparseTokenInOpSet( stream, PostUnary, status);
    finished = ADI__false;
    while ( tp && _ok(status) && ! finished ) {
      if ( PostUnary[tp-1].priority >= priority ) {
	ADInextToken( stream, status );
	expr = ADIetnNew(
		 *PostUnary[tp-1].key,
		 lstx_cell( expr, ADI__nullid, status ),
		 status );
	tp = ADIparseTokenInOpSet( stream, PostUnary, status);
	}
      else
	finished = ADI__true;
      }

    }

  if ( ! _ok(status) )       		/* Remove partially parsed expr */
    adic_erase( &expr, status );

  return expr;
  }
