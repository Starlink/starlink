#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <math.h>
#include <limits.h>
#include <stdio.h>
#include <stdarg.h>
#ifdef VAX
#   include <strmac.h>
#endif
#ifdef __MSDOS__
#include "io.h"
#endif
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
ADIobj	K_WildCard = ADI__nullid;

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
		  0, 0, NULL, NULL, &str->ctok.dt,
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

/* While more identifiers */
  while ( (str->ctok.t == TOK__SYM) && _ok(status) ) {

/* Take the component name from stream */
    cnam = prsx_symname( stream, status );

    ADInextToken( stream, status );

/* Assignment bit name=value ? */
    if ( ADIifMatchToken( stream, TOK__ASSIGN, status )) {

/* Take component value from stream */
      cval = prsx_cvalue( stream, status );

/* Write new component to structure */
      adix_putiid( id, cnam, cval, status );
      }

/* We've hit the end */
    else
      adic_erase( &cnam, status );
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
    dev->isatty = ADI__false;
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
    adic_new0( "Stream", &str, status );
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
  if ( _ok(status) ) {
    ADIstream	*str = _strm_data(stream);
    *data = str->ctok.dat;
    if ( len ) *len = str->ctok.nc;
    }
  }


/*
 * Flush a device
 */
void ADIstrmFlushDev( ADIdevice *dev, ADIstatus status )
  {
  _chk_stat;

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

#ifdef __MSDOS__
#define finite(_x) (1==1)
#define isnan(_x)  ((_x)==UT_BAD_d)
#endif


/*
 *+			E r s V S P r i n t f

 *  Function name:
      ErsVSPrintf

 *  Function:
	A safe version of the C RTL vsprintf function.


 *  Description:
	The standard C RTL version of vsprintf is unsafe as nothing
	limits the length of the output string.  It is easy to overwrite
	the stack.  By providing a length argument string argument, this
	routine implements a safe version of vsprintf.

	When not under VxWorks, this module is based upon the Berkeley Unix
	vprintf.c module.  (based on version 5.47, 22-Mar-1991).  The header
	for this file and the appropriate copyright appears below.

 *  Language:
      C

 *  Call:
	(int) = ErsVSPrintf(length, string, format, arg)

 *  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
	(>) length  (int) The length of string.
	(<) string  (char *) The pointer to the output string
	(>) format  (char *) A format specification
	(>) arg	    (va_list) Variable argument list


 *  Function value:
	EOF indicates the format string exceeds the length available,
	otherwise, the number of characters output.

 *  Include files: Ers.h, stdio.h

 *  External values used:
	    none

 *  Prior requirements:
	    none

 *  Support: Tony Farrell, AAO

 *-

 *  History:
      23-Nov-1992 - TJF - Original version, based on Berkeley Unix
		    vprintf.c, version 5.47, 22-Mar-1991.
      27-Nov-1992 - TJF - Under VxWorks, use fioFormatV.
      29-Sep-1993 - TJF - Add Sccs id
      29-Apr-1994 - TJF - Solaris 2 does not have isinf, so use !finite
      21-Oct-1994 - TJF - Osf does not have isinf either - it appears to have
				been droped from ieee.  We use finite instead.
      {@change entry@}


 *  Sccs Id:  ersvsprintf.c, Release 1.9, 10/21/94

 */


/*
 *  Original Berkeley copyright notice.
 *
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Macros for converting digits to letters and vice versa
 */
#define	to_digit(c)	((c) - '0')
#define is_digit(c)	((unsigned)to_digit(c) <= 9)
#define	to_char(n)	((n) + '0')

/*
 * Flags used during conversion.
 */
#define	LONGINT		0x01		/* long integer */
#define	LONGDBL		0x02		/* long double; unimplemented */
#define	SHORTINT	0x04		/* short integer */
#define	ALT		0x08		/* alternate form */
#define	LADJUST		0x10		/* left adjustment */
#define	ZEROPAD		0x20		/* zero (as opposed to blank) pad */
#define	HEXPREFIX	0x40		/* add 0x or 0X prefix */

#define MAXEXPON  40
#define MAXFRACT  14

static char *Cnv_Exponent( register char *p, register int exp, int fmtch )
  {
  register char *t;
  char expbuf[MAXEXPON];

  *p++ = fmtch;
  if (exp < 0) {
    exp = -exp;
    *p++ = '-';
    }

  else
    *p++ = '+';

  t = expbuf + MAXEXPON;
  if ( exp > 9 ) {
    do {
      *--t = to_char(exp % 10);
      }
    while ((exp /= 10) > 9);
    *--t = to_char(exp);
    for (; t < expbuf + MAXEXPON; *p++ = *t++);
    }
  else {
    *p++ = '0';
    *p++ = to_char(exp);
    }
  return (p);
  }



static char * Cnv_Round( double fract, int *exp,
	register char *start, register char *end, char ch, char *signp )
  {
  double	tmp;

  if ( fract )
    (void) modf(fract * 10, &tmp);
  else
    tmp = to_digit(ch);

  if (tmp > 4)
    for (;; --end) {
      if (*end == '.')
	--end;

      if (++*end <= '9')
	break;

      *end = '0';
      if (end == start) {
	if (exp) {	/* e/E; increment exponent */
	  *end = '1';
	  ++*exp;
	  }
	else {		/* f; add extra digit */
	  *--end = '1';
	  --start;
	  }
	break;
	}
      }
	/* ``"%.3f", (double)-0.0004'' gives you a negative 0. */
  else if (*signp == '-')
    for (;; --end) {
      if (*end == '.')
	--end;
      if (*end != '0')
	break;
      if (end == start)
	*signp = 0;
      }

  return (start);
  }


static int Cnv_Cvt( double number, register int prec, int flags,
	char *signp, int fmtch, char *startp, char *endp )
  {
  register char *p, *t;
  register double fract;
  int dotrim, expcnt, gformat;
  double integer, tmp;

  dotrim = expcnt = gformat = 0;
  if (number < 0) {
    number = -number;
    *signp = '-';
    }
  else
    *signp = 0;

  fract = modf(number, &integer);

/* get an extra slot for rounding. */
  t = ++startp;

/*
 * get integer portion of number; put into the end of the buffer; the
 * .01 is added for modf(356.0 / 10, &integer) returning .59999999...
 */
  for (p = endp - 1; integer; ++expcnt) {
    tmp = modf(integer / 10, &integer);
    *p-- = to_char((int)((tmp + .01) * 10));
    }

  switch (fmtch) {
    case 'f':

/* reverse integer into beginning of buffer */
      if (expcnt)
	for (; ++p < endp; *t++ = *p);
      else
	*t++ = '0';

/* if precision required or alternate flag set, add in a decimal point. */
      if (prec || flags&ALT)
	*t++ = '.';

/* if requires more precision and some fraction left */
      if (fract) {
	if (prec)
	  do {
	    fract = modf(fract * 10, &tmp);
	    *t++ = to_char((int)tmp);
	    }
	  while (--prec && fract);

	if (fract)
	  startp = Cnv_Round(fract, (int *)NULL, startp,
				    t - 1, (char)0, signp);
	}

      for (; prec--; *t++ = '0');
      break;

    case 'e':
    case 'E':
eformat:
      if (expcnt) {
	*t++ = *++p;
	if (prec || flags&ALT)
	  *t++ = '.';

/* if requires more precision and some integer left */
	for (; prec && ++p < endp; --prec)
	  *t++ = *p;

/* if done precision and more of the integer component,
 * round using it; adjust fract so we don't re-round later.*/
	if (!prec && ++p < endp) {
	  fract = 0;
	  startp = Cnv_Round((double)0, &expcnt, startp,
				    t - 1, *p, signp);
	  }

/* adjust expcnt for digit in front of decimal */
	--expcnt;
	}

/* until first fractional digit, decrement exponent */
      else if (fract) {

/* adjust expcnt for digit in front of decimal */
	for (expcnt = -1;; --expcnt) {
	  fract = modf(fract * 10, &tmp);
	  if (tmp)
	    break;
	  }

	*t++ = to_char((int)tmp);
	if (prec || flags&ALT)
	  *t++ = '.';
	}

      else {
	*t++ = '0';
	if (prec || flags&ALT)
	  *t++ = '.';
	}

/* if requires more precision and some fraction left */
      if (fract) {
	if (prec)
	  do {
	    fract = modf(fract * 10, &tmp);
	    *t++ = to_char((int)tmp);
	    }
	  while (--prec && fract);

	if (fract)
	  startp = Cnv_Round(fract, &expcnt, startp,
				    t - 1, (char)0, signp);
	}

/* if requires more precision */
      for (; prec--; *t++ = '0');

/* unless alternate flag, trim any g/G format trailing 0's */
      if (gformat && !(flags&ALT)) {
	while (t > startp && *--t == '0');
	if (*t == '.')
	  --t;
	++t;
	}

      t = Cnv_Exponent(t, expcnt, fmtch);
      break;

    case 'g':
    case 'G':

/* a precision of 0 is treated as a precision of 1. */
      if (!prec)
	++prec;

/*
 * ``The style used depends on the value converted; style e
 * will be used only if the exponent resulting from the
 * conversion is less than -4 or greater than the precision.''
 *	-- ANSI X3J11
 */
      if (expcnt > prec || (!expcnt && fract && fract < .0001)) {

/*
 * g/G format counts "significant digits, not digits of
 * precision; for the e/E format, this just causes an
 * off-by-one problem, i.e. g/G considers the digit
 * before the decimal point significant and e/E doesn't
 * count it as precision.
 */
	--prec;
	fmtch -= 2;		/* G->E, g->e */
	gformat = 1;
	goto eformat;
	}

/* reverse integer into beginning of buffer, note, decrement precision */
      if (expcnt)
	for (; ++p < endp; *t++ = *p, --prec);
      else
	*t++ = '0';

/* if precision required or alternate flag set, add in a
 * decimal point.  If no digits yet, add in leading 0. */
      if (prec || flags&ALT) {
	dotrim = 1;
	*t++ = '.';
	}
      else
	dotrim = 0;

/* if requires more precision and some fraction left */
      if (fract) {
	if (prec) {
	  do {
	    fract = modf(fract * 10, &tmp);
	    *t++ = to_char((int)tmp);
	    }
	  while(!tmp);

	  while (--prec && fract) {
	    fract = modf(fract * 10, &tmp);
	    *t++ = to_char((int)tmp);
	    }
	  }

	if (fract)
	  startp = Cnv_Round(fract, (int *)NULL, startp,
				    t - 1, (char)0, signp);
	}

/* alternate format, adds 0's for precision, else trim 0's */
      if (flags&ALT)
	for (; prec--; *t++ = '0');
      else if (dotrim) {
	while (t > startp && *--t == '0');
	if (*t != '.')
	  ++t;
	}
      }

  return (t - startp);
  }


int ADIstrmPutInt( ADIdevice *dev, char *str, int slen,
		   int check, ADIstatus status )
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
    if ( check )
      for( i=0; i<slen; i++, scur++ )
	buf[dev->bnc++] = (*scur < ' ') ? '.' : *scur;
    else
      for( i=0; i<slen; i++ )
	buf[dev->bnc++] = *scur++;
    }

/* We will fill the buffer (and maybe more than once) */
  else {
    int		nmove,nleft = slen;

    while ( nleft > 0 ) {
      nmove = _MIN(sleft,nleft);
      if ( check )
	for( i=0; i<nmove; i++, scur++ )
	  buf[dev->bnc++] = (*scur < ' ') ? '.' : *scur;
      else
	for( i=0; i<nmove; i++ )
	  buf[dev->bnc++] = *scur++;
      ADIstrmFlushDev( dev, status );
      nleft -= sleft;
      sleft = eblen;
      }
    }

  return _ok(status);
  }


#define	BUF		40
#define	DEFPREC		6

int ADIstrmVprintf( ADIobj stream, char *fmt0, va_list ap, ADIstatus status )
  {
  ADIstream		*pstr = _strm_data(stream);
  ADIdevice		*dev = pstr->dev;
  register char 	*fmt;		/* format string */
  register int 		ch;		/* character from fmt */
  register int 		n;		/* handy integer (short term usage) */
  register char		*cp;		/* handy char pointer (short term usage) */
  register int 		flags;		/* flags as above */
  ADIstring		*astr;		/* ADI string data */
  ADIobj		aid;		/* ADI object */
  int			checkit = 0;	/* Check insert text for non-prints */
  int 			ret;		/* return value accumulator */
  int 			width;		/* width from format (%8d), or 0 */
  int 			prec;		/* precision from format (%.3d), or -1 */
  char 			sign;		/* sign prefix (' ', '+', '-', or \0) */
  char 			softsign;	/* temporary negative sign for floats */
  double 		_double;	/* double precision arguments %[eEfgG] */
  int 			fpprec;		/* `extra' floating precision in [eEfgG] */
  unsigned long 	_ulong ;	/* integer arguments %[diouxX] */
  enum { OCT, DEC, HEX } base;		/* base for [diouxX] conversion */
  int 			dprec;		/* a copy of prec if [diouxX], 0 otherwise */
  int 			fieldsz;	/* field size expanded by sign, etc */
  int 			realsz;		/* field size expanded by dprec */
  int 			size;		/* size of converted field or string */
  char 			*xdigs="";	/* digits for [xX] conversion */
  char 			buf[BUF];	/* space for %c, %[diouxX], %[eEfgG] */
  char 			ox[2];		/* space for 0x hex-prefix */

	/*
	 * Choose PADSIZE to trade efficiency vs size.  If larger
	 * printf fields occur frequently, increase PADSIZE (and make
	 * the initialisers below longer).
	 */
#define	PADSIZE	16		/* pad chunk size */
	static char blanks[PADSIZE] =
	 {' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '};
	static char zeroes[PADSIZE] =
	 {'0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0'};

	/*
	 * BEWARE, these `goto error' on error, and PAD uses `n'.
	 */
#define	PRINT(ptr, len) { \
	if (!ADIstrmPutInt(dev,(ptr),(len),checkit,status)) \
	    return(EOF);\
}
#define	PAD(howmany, with) { \
	if ((n = (howmany)) > 0) { \
		while (n > PADSIZE) { \
			PRINT(with, PADSIZE); \
			n -= PADSIZE; \
		} \
		PRINT(with, n); \
	} \
}
#define	FLUSH()

	/*
	 * To extend shorts properly, we need both signed and unsigned
	 * argument extraction methods.
	 */
#define	SARG() \
	(flags&LONGINT ? va_arg(ap, long) : \
	    flags&SHORTINT ? (long)(short)va_arg(ap, int) : \
	    (long)va_arg(ap, int))
#define	UARG() \
	(flags&LONGINT ? va_arg(ap, unsigned long) : \
	    flags&SHORTINT ? (unsigned long)(unsigned short)va_arg(ap, int) : \
	    (unsigned long)va_arg(ap, unsigned int))

	fmt = fmt0;
	ret = 0;

	/*
	 * Scan the format for conversions (`%' character).
	 */
	for (;;) {
		checkit = 0;

		for (cp = fmt; (ch = *fmt) != '\0' && ch != '%'; fmt++)
			/* void */;
		if ((n = fmt - cp) != 0) {
			PRINT(cp, n);
			ret += n;
		}
		if (ch == '\0')
			goto done;
		fmt++;		/* skip over '%' */

		flags = 0;
		dprec = 0;
		fpprec = 0;
		width = 0;
		prec = -1;
		sign = '\0';

rflag:		ch = *fmt++;
reswitch:	switch (ch) {
		case ' ':
			/*
			 * ``If the space and + flags both appear, the space
			 * flag will be ignored.''
			 *	-- ANSI X3J11
			 */
			if (!sign)
				sign = ' ';
			goto rflag;
		case '#':
			flags |= ALT;
			goto rflag;
		case '*':
			/*
			 * ``A negative field width argument is taken as a
			 * - flag followed by a positive field width.''
			 *	-- ANSI X3J11
			 * They don't exclude field widths read from args.
			 */
			if ((width = va_arg(ap, int)) >= 0)
				goto rflag;
			width = -width;
			/* FALLTHROUGH */
		case '-':
			flags |= LADJUST;
			goto rflag;
		case '+':
			sign = '+';
			goto rflag;
		case '.':
			if ((ch = *fmt++) == '*') {
				n = va_arg(ap, int);
				prec = n < 0 ? -1 : n;
				goto rflag;
			}
			n = 0;
			while (is_digit(ch)) {
				n = 10 * n + to_digit(ch);
				ch = *fmt++;
			}
			prec = n < 0 ? -1 : n;
			goto reswitch;
		case '0':
			/*
			 * ``Note that 0 is taken as a flag, not as the
			 * beginning of a field width.''
			 *	-- ANSI X3J11
			 */
			flags |= ZEROPAD;
			goto rflag;
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			n = 0;
			do {
				n = 10 * n + to_digit(ch);
				ch = *fmt++;
			} while (is_digit(ch));
			width = n;
			goto reswitch;
		case 'L':
			flags |= LONGDBL;
			goto rflag;
		case 'h':
			flags |= SHORTINT;
			goto rflag;
		case 'l':
			flags |= LONGINT;
			goto rflag;
/*		case 'I':
			flags |= LONGINT;
			goto rflag; */
		case 'c':
			*(cp = buf) = va_arg(ap, int);
			size = 1;
			sign = '\0';
			break;
		case 'D':
			flags |= LONGINT;
			/*FALLTHROUGH*/
		case 'd':
		case 'i':
			_ulong = SARG();
			if ((long)_ulong < 0) {
				_ulong = -_ulong;
				sign = '-';
			}
			base = DEC;
			goto number;
		case 'e':
		case 'E':
		case 'f':
		case 'g':
		case 'G':
			_double = va_arg(ap, double);

#ifndef VMS
			/* do this before tricky precision changes */
			/* (this checks for infinity and not a number) */
			/* (not necessary on VAX/VMS)		*/
			if (!finite(_double)) {
				if (_double < 0)
					sign = '-';
				cp = "Inf";
				size = 3;
				break;
			}
			if (isnan(_double)) {
				cp = "NaN";
				size = 3;
				break;
			}
#endif
			/*
			 * don't do unrealistic precision; just pad it with
			 * zeroes later, so buffer size stays rational.
			 */
			if (prec > MAXFRACT) {
				if ((ch != 'g' && ch != 'G') || (flags&ALT))
					fpprec = prec - MAXFRACT;
				prec = MAXFRACT;
			} else if (prec == -1)
				prec = DEFPREC;
			/*
			 * cvt may have to round up before the "start" of
			 * its buffer, i.e. ``intf("%.2f", (double)9.999);'';
			 * if the first character is still NUL, it did.
			 * softsign avoids negative 0 if _double < 0 but
			 * no significant digits will be shown.
			 */
			cp = buf;
			*cp = '\0';
			size = Cnv_Cvt(_double, prec, flags, &softsign, ch,
			    cp, buf + sizeof(buf));
			if (softsign)
				sign = '-';
			if (*cp == '\0')
				cp++;
			break;
		case 'n':
			if (flags & LONGINT)
				*va_arg(ap, long *) = ret;
			else if (flags & SHORTINT)
				*va_arg(ap, short *) = ret;
			else
				*va_arg(ap, int *) = ret;
			continue;	/* no output */
		case 'I':
			_ulong = va_arg(ap, ADIinteger);
			base = DEC;
			goto number;
		case 'O':
			aid = va_arg(ap, ADIobj);
			adix_print( stream, aid, 1, ADI__true, status );
			continue;
/*		case 'O':
			flags |= LONGINT;
			/*FALLTHROUGH */
		case 'o':
			_ulong = UARG();
			base = OCT;
			goto nosign;
		case 'p':
			/*
			 * ``The argument shall be a pointer to void.  The
			 * value of the pointer is converted to a sequence
			 * of printable characters, in an implementation-
			 * defined manner.''
			 *	-- ANSI X3J11
			 */
			/* NOSTRICT */
			_ulong = (unsigned long)va_arg(ap, void *);
			base = HEX;
			xdigs = "0123456789abcdef";
			flags |= HEXPREFIX;
			ch = 'x';
			goto nosign;
		case 'S':
			aid = va_arg(ap, ADIobj);
			if ( _valid_q(aid) ) {
			  astr = _str_data(aid);
			  cp = astr->data;
			  if ( ! cp ) {
			    cp = "\"\"";
			    aid = ADI__nullid;
			    }
			  }
			else
			  cp = "<null>";

			if ( _valid_q(aid) )
			  size = astr->len;
			else
			  size = strlen(cp);
			if ( prec >= 0 )
			  size = _MIN(size,prec);
			sign = '\0';
			checkit = 1;
			break;
		case 's':
			if ((cp = va_arg(ap, char *)) == NULL)
				cp = "(null)";
			if (prec >= 0) {
				/*
				 * can't use strlen; can only look for the
				 * NUL in the first `prec' characters, and
				 * strlen() will go further.
				 */
				char *p = (char *) memchr(cp, 0, prec);

				if (p != NULL) {
					size = p - cp;
					if (size > prec)
						size = prec;
				} else
					size = prec;
			} else
				size = strlen(cp);
			sign = '\0';
			break;
		case 'U':
			flags |= LONGINT;
			/*FALLTHROUGH*/
		case 'u':
			_ulong = UARG();
			base = DEC;
			goto nosign;
		case 'X':
			xdigs = "0123456789ABCDEF";
			goto hex;
		case 'x':
			xdigs = "0123456789abcdef";
hex:			_ulong = UARG();
			base = HEX;
			/* leading 0x/X only if non-zero */
			if (flags & ALT && _ulong != 0)
				flags |= HEXPREFIX;

			/* unsigned conversions */
nosign:			sign = '\0';
			/*
			 * ``... diouXx conversions ... if a precision is
			 * specified, the 0 flag will be ignored.''
			 *	-- ANSI X3J11
			 */
number:			if ((dprec = prec) >= 0)
				flags &= ~ZEROPAD;

			/*
			 * ``The result of converting a zero value with an
			 * explicit precision of zero is no characters.''
			 *	-- ANSI X3J11
			 */
			cp = buf + BUF;
			if (_ulong != 0 || prec != 0) {
				/*
				 * unsigned mod is hard, and unsigned mod
				 * by a constant is easier than that by
				 * a variable; hence this switch.
				 */
				switch (base) {
				case OCT:
					do {
						*--cp = to_char(_ulong & 7);
						_ulong >>= 3;
					} while (_ulong);
					/* handle octal leading 0 */
					if (flags & ALT && *cp != '0')
						*--cp = '0';
					break;

				case DEC:
					/* many numbers are 1 digit */
					while (_ulong >= 10) {
						*--cp = to_char(_ulong % 10);
						_ulong /= 10;
					}
					*--cp = to_char(_ulong);
					break;

				case HEX:
					do {
						*--cp = xdigs[_ulong & 15];
						_ulong >>= 4;
					} while (_ulong);
					break;

				default:
					cp = "bug in vfprintf: bad base";
					size = strlen(cp);
					goto skipsize;
				}
			}
			size = buf + BUF - cp;
		skipsize:
			break;
		default:	/* "%?" prints ?, unless ? is NUL */
			if (ch == '\0')
				goto done;
			/* pretend it was %c with argument ch */
			cp = buf;
			*cp = ch;
			size = 1;
			sign = '\0';
			break;
		}

		/*
		 * All reasonable formats wind up here.  At this point,
		 * `cp' points to a string which (if not flags&LADJUST)
		 * should be padded out to `width' places.  If
		 * flags&ZEROPAD, it should first be prefixed by any
		 * sign or other prefix; otherwise, it should be blank
		 * padded before the prefix is emitted.  After any
		 * left-hand padding and prefixing, emit zeroes
		 * required by a decimal [diouxX] precision, then print
		 * the string proper, then emit zeroes required by any
		 * leftover floating precision; finally, if LADJUST,
		 * pad with blanks.
		 */

		/*
		 * compute actual size, so we know how much to pad.
		 * fieldsz excludes decimal prec; realsz includes it
		 */
		fieldsz = size + fpprec;
		if (sign)
			fieldsz++;
		else if (flags & HEXPREFIX)
			fieldsz += 2;
		realsz = dprec > fieldsz ? dprec : fieldsz;

		/* right-adjusting blank padding */
		if ((flags & (LADJUST|ZEROPAD)) == 0)
			PAD(width - realsz, blanks);

		/* prefix */
		if (sign) {
			PRINT(&sign, 1);
		} else if (flags & HEXPREFIX) {
			ox[0] = '0';
			ox[1] = ch;
			PRINT(ox, 2);
		}

		/* right-adjusting zero padding */
		if ((flags & (LADJUST|ZEROPAD)) == ZEROPAD)
			PAD(width - realsz, zeroes);

		/* leading zeroes from decimal precision */
		PAD(dprec - fieldsz, zeroes);

		/* the string or number proper */
		PRINT(cp, size);

		/* trailing f.p. zeroes */
		PAD(fpprec, zeroes);

		/* left-adjusting padding (always blank) */
		if (flags & LADJUST)
			PAD(width - realsz, blanks);

		/* finally, adjust ret */
		ret += width > realsz ? width : realsz;

		FLUSH();	/* copy out the I/O vectors */
	}
done:
	FLUSH();
	return (ret);
	/* NOTREACHED */
}


void ADIstrmPrintf( ADIobj stream, char *format, ADIstatus status, ... )
  {
  va_list	ap;

/* Start variable arg processing */
  va_start(ap,status);

/* Invoke the internal routine */
  ADIstrmVprintf( stream, format, ap, status );

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


ADIobj ADIstrmDel( int narg, ADIobj args[], ADIstatus status )
  {
  ADIstream	*str = _strm_data(args[0]);

  if ( str->dev )
    ADIresetStreamInt( str, status );

  return ADI__nullid;
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

ADIobj ADIstrmExtendFile( ADIobj stream, FILE *f, ADIstatus status )
  {
  if ( _ok(status) ) {
    ADIstream	*str = _strm_data(stream);

    ADIaddDeviceToStream( stream, ADIdevFile, status );

    str->dev->f = f;
    str->dev->pstatic = ADI__true;
    str->dev->buf = ADImemAlloc( FILEBUF, status );
    str->dev->bufsiz = FILEBUF;
    str->dev->isatty = isatty(fileno(f));
    str->dev->buf[0] = '\0';
    str->dev->ptr = str->dev->buf;
    }

  return stream;
  }


char ADIreadCharFromStream( ADIobj stream, ADIstatus status )
  {
  ADIstream	*str = _strm_data(stream);
  char          ch = '\0';
  int           gotit = ADI__false;

  while ( _ok(status) && ! gotit ) {
    ADIdevice	*dev = str->dev;

    if ( str->nc_pb ) {
      ch = str->c_pb[--str->nc_pb]; gotit = ADI__true;
      }
    else if ( dev ) {
      switch( dev->type ) {
	case ADIdevCstring:
	  ch = *(str->dev->ptr++); gotit = ADI__true;
	  break;
	case ADIdevFile:
	  if ( feof(dev->f) )
	    ADIdropDevice( stream, status );
	  else {
	    if ( *dev->ptr ) {
	      ch = *(dev->ptr++); gotit = ADI__true;
	      }
	    else {
	      dev->ptr = dev->buf;
	      if ( dev->isatty )
		printf( "> " );
	      fgets( dev->buf, dev->bufsiz, dev->f );
	      if ( feof(dev->f) )
		ADIdropDevice( stream, status );
	      else {
		ch = *(dev->ptr++); gotit = ADI__true;
		}
	      }
	    }
	  break;
	}
      }
    else
      *status = SAI__ERROR;
    }

  if ( gotit ) str->ctok.dat[str->ctok.nc++] = ch;
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
  ADIstream		*str;

  if ( !_ok(status) )
    return tok;

  str = _strm_data(stream);

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


void ADImatchToken( ADIobj stream, ADItokenType tok, ADIstatus status )
  {
  char          *gstr;
  int           glen;
  char          *estr;
  int           elen;

  if ( _ok(status) ) {
    ADItokenType	ctok = _strm_data(stream)->ctok.t;

    if ( ctok == tok )
      ADInextToken( stream, status );
    else {
      ADIdescribeToken( ctok, &gstr, &glen );
      ADIdescribeToken( tok, &estr, &elen );
      adic_setecs( ADI__SYNTAX, "Parse error, %*s found where %*s expected.",
		     status, glen, gstr, elen, estr );
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
	  adic_setecs( ADI__SYNTAX,
		"Error reading list - comma or %*s expected",
		status, tlen, tstr );
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
  ADIobj                carg;           /* _CAR(arg) */
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

  carg = _CAR(arg);

/* Single _x type node? */
  if ( _valid_q(carg) && _null_q(_CDR(arg)) )

/* Check <x> is an expression node */
    if ( _etn_q(carg) ) {
      ADIobj			cid;	/* Class definition */

/* Locate class definition of named class, if any */
      cid = ADIkrnlFindClsI( _etn_head(carg), status );
      if ( _valid_q(cid) ) {

/* Locate address of the slot for nbi'th underscore expression */
	ADIobj	*saddr = _cdef_data(cid)->bexpr + nbi;

	subexp = *saddr;

/* Sub-expression already exists? */
	if ( *saddr )
	  adic_erase( &arg, status );
	else
	  *saddr = ADIetnNew( adix_clone(nstr, status), arg, status );

	subexp = adix_clone( *saddr, status );
	}
      }

/* Set return value, either the common sub-expression or a new */
/* expression node */
  if ( _valid_q(subexp) )
    return subexp;
  else
    return ADIetnNew( adix_clone(nstr, status), arg, status );
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

	expr = ADIetnNew( adix_clone( K_Pattern, status ),
			  lstx_new2( first, expr, status),
			  status );
	}

/* Left square bracket indicates start of array reference */
      else if ( ADIifMatchToken( stream, TOK__LBRAK, status ) ) {
	symbol = expr;                  /* Store symbol for later */

	expr = ADIetnNew( adix_clone( K_ArrayRef, status ),
	      ADIparseComDelList( stream, TOK__RBRAK, status ),
	      status );

/* Splice symbol into arg list */
	_etn_args(expr) = lstx_cell( symbol, _etn_args(expr), status );
	}
      break;

    case TOK__LBRAK:
      ADInextToken( stream, status );
      expr = ADIetnNew( adix_clone( K_List, status ),
	       ADIparseComDelList( stream,   /* Parse list elements */
		 TOK__RBRAK, status ),
	       status );
      expr = ADIetnNew( adix_clone( K_Array, status ),
		     lstx_cell( expr, ADI__nullid, status ),
		     status );
      break;

    case TOK__LBRACE:
      ADInextToken( stream, status );
      expr = ADIetnNew( adix_clone( K_List, status ),/* Create List(e1..) constructor */
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
      if ( str->ctok.dt == UT_ALLOC_i ) {
	  base = 10;
	  idata = str->ctok;
	  ADInextToken( stream, status );
	  if ( str->ctok.t == TOK__CARAT2 ) {
	    ADInextToken( stream, status );
	    if ( (str->ctok.t != TOK__CONST) ||
		 (str->ctok.dt != UT_ALLOC_i ) )
	      adic_setecs( ADI__SYNTAX, "Numeric base expected", status );
	    else {
	      base = (int) ADIparseScanInt( str->ctok.dat, 10, status );
	      if ( _ok(status) && (base<2) && (base>36) ) {
		adic_setecs( ADI__SYNTAX, "Invalid base specifier /%d/ - must lie between 2 and 36", status, base );
		}
	      }
	    }
	  else
	    next = ADI__false;
	  ival = ADIparseScanInt( idata.dat, base, status );
	  adic_newv0i( ival, &expr, status );
	}
      else if ( str->ctok.dt == UT_ALLOC_d ) {
	  if ( ! sscanf(str->ctok.dat,"%lf",&dval) )
	    *status = SAI__ERROR;
	  else
	    adic_newv0d( dval, &expr, status );
	  break;
	}
      else if ( str->ctok.dt == UT_ALLOC_c ) {
	  adic_newv0c( str->ctok.dat, &expr, status );
	}
      else
	adic_newv0c( str->ctok.dat, &expr, status );
      if ( next )
	ADInextToken( stream, status );
      break;
    default:
      tp = ADIparseTokenInOpSet( stream, PreUnary, status);
      if ( tp ) {
	if ( PreUnary[tp-1].priority >= priority ) {
	  ADInextToken( stream, status );
	  expr = ADIetnNew( adix_clone( *PreUnary[tp-1].key, status ),
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
		     adix_clone( *Binary[tp-1].key, status ),
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
		 adix_clone( *PostUnary[tp-1].key, status ),
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


void prsx_init( ADIstatus status )
  {
  DEFINE_CSTR_TABLE(stringtable)
    CSTR_TABLE_ENTRY(EXC_ArrayBound,"ArrayBound"),
    CSTR_TABLE_ENTRY(EXC_BoolExp,"BooleanExpected"),
    CSTR_TABLE_ENTRY(EXC_ControlC,"ControlC"),
    CSTR_TABLE_ENTRY(EXC_Error,"Error"),
    CSTR_TABLE_ENTRY(EXC_ExceedMaxRecurse,"ExceedMaxRecurse"),
    CSTR_TABLE_ENTRY(EXC_InvalidArg,"InvalidArg"),
    CSTR_TABLE_ENTRY(EXC_NoSuchField,"NoSuchField"),
    CSTR_TABLE_ENTRY(EXC_ScopeBreak,"ScopeBreak"),
    CSTR_TABLE_ENTRY(EXC_SyntaxError, "SyntaxError"),

    CSTR_TABLE_ENTRY(K_AddTo,"AddTo"),
    CSTR_TABLE_ENTRY(K_Alternatives,"Alternatives"),
    CSTR_TABLE_ENTRY(K_And,"And"),
    CSTR_TABLE_ENTRY(K_Apply,"Apply"),
    CSTR_TABLE_ENTRY(K_Array,"Array"),
    CSTR_TABLE_ENTRY(K_ArrayRef,"ArrayRef"),

    CSTR_TABLE_ENTRY(K_Blank,"Blank"),
    CSTR_TABLE_ENTRY(K_BlankSeq,"BlankSeq"),
    CSTR_TABLE_ENTRY(K_BlankNullSeq,"BlankNullSeq"),
    CSTR_TABLE_ENTRY(K_Break,"Break"),

    CSTR_TABLE_ENTRY(K_Catch,"Catch"),
    CSTR_TABLE_ENTRY(K_Concat,"Concat"),
    CSTR_TABLE_ENTRY(K_Condition,"Condition"),

    CSTR_TABLE_ENTRY(K_DefEnum,"DefEnum"),
    CSTR_TABLE_ENTRY(K_Divide,"Divide"),
    CSTR_TABLE_ENTRY(K_DivideBy,"DivideBy"),
    CSTR_TABLE_ENTRY(K_Dot,"Dot"),
    CSTR_TABLE_ENTRY(K_DoWhile,"DoWhile"),

    CSTR_TABLE_ENTRY(K_Echo,"Echo"),
    CSTR_TABLE_ENTRY(K_Equal,"Equal"),
    CSTR_TABLE_ENTRY(K_Factorial,"Factorial"),
    CSTR_TABLE_ENTRY(K_Finally,"Finally"),
    CSTR_TABLE_ENTRY(K_Foreach,"Foreach"),

    CSTR_TABLE_ENTRY(K_Get,"Get"),
    CSTR_TABLE_ENTRY(K_GE,"GreaterThanOrEqual"),
    CSTR_TABLE_ENTRY(K_GT,"GreaterThan"),

    CSTR_TABLE_ENTRY(K_HoldAll,"HoldAll"),
    CSTR_TABLE_ENTRY(K_HoldFirst,"HoldFirst"),
    CSTR_TABLE_ENTRY(K_HoldRest,"HoldRest"),

    CSTR_TABLE_ENTRY(K_If,"If"),

    CSTR_TABLE_ENTRY(K_List,"List"),
    CSTR_TABLE_ENTRY(K_Listable,"Listable"),
    CSTR_TABLE_ENTRY(K_LE,"LessThanOrEqual"),
    CSTR_TABLE_ENTRY(K_LT,"LessThan"),

    CSTR_TABLE_ENTRY(K_Map,"Map"),
    CSTR_TABLE_ENTRY(K_Multiply,"Multiply"),
    CSTR_TABLE_ENTRY(K_MultiplyBy,"MultiplyBy"),

    CSTR_TABLE_ENTRY(K_Negate,"Negate"),
    CSTR_TABLE_ENTRY(K_Not,"Not"),
    CSTR_TABLE_ENTRY(K_NotEqual,"NotEqual"),

    CSTR_TABLE_ENTRY(K_Or,"Or"),

    CSTR_TABLE_ENTRY(K_Pattern,"Pattern"),
    CSTR_TABLE_ENTRY(K_PatternTest,"PatternTest"),
    CSTR_TABLE_ENTRY(K_Plus,"Plus"),
    CSTR_TABLE_ENTRY(K_PostDec,"PostDecrement"),
    CSTR_TABLE_ENTRY(K_PostInc,"PostIncrement"),
    CSTR_TABLE_ENTRY(K_Power,"Power"),
    CSTR_TABLE_ENTRY(K_PreDec,"PreDecrement"),
    CSTR_TABLE_ENTRY(K_PreInc,"PreIncrement"),
    CSTR_TABLE_ENTRY(K_Put,"Put"),

    CSTR_TABLE_ENTRY(K_Query,"Query"),

    CSTR_TABLE_ENTRY(K_Raise,"Raise"),
    CSTR_TABLE_ENTRY(K_Range,"Range"),
    CSTR_TABLE_ENTRY(K_ReRaise,"ReRaise"),

    CSTR_TABLE_ENTRY(K_Set,"Set"),
    CSTR_TABLE_ENTRY(K_SetDelayed,"SetDelayed"),
    CSTR_TABLE_ENTRY(K_Subtract,"Subtract"),
    CSTR_TABLE_ENTRY(K_SubtractFrom,"SubtractFrom"),
    CSTR_TABLE_ENTRY(K_Switch,"Switch"),

    CSTR_TABLE_ENTRY(K_Try,"Try"),

    CSTR_TABLE_ENTRY(K_While,"While"),
    CSTR_TABLE_ENTRY(K_WildCard,"WildCard"),
  END_CSTR_TABLE;

  ADIkrnlAddCommonStrings( stringtable, status );

  }
