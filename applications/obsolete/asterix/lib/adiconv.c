#include <limits.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#ifdef VAX
#   include <strmac.h>
#endif

#include "asterix.h"
#include "sae_par.h"

#include "aditypes.h"
#include "adiconv.h"
#include "adikrnl.h"
#include "adistrng.h"
#include "adisyms.h"

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


/*
 *  Copy a string to the output string. If the output string exceeds the
 *  length required, return false, otherwise true.
 */

int Cnv_SPrintf (
		const int outLength,/* Max number allowed in string */
		const char *ptr, /* Point to string to print	    */
		const int inLength,/* Length of string to print    */
		char *string,		    /* Output string		    */
		int *outNext)		    /* Current length of string	    */
  {
  register int i=0;
/*
 *  We use outLength-1 to ensure we leave space for the null terminator.
 */
    while ((*outNext < outLength-1)&&(i < inLength))
	string[(*outNext)++] = ptr[i++];

    string[*outNext] = '\0';	/* Null terminate	*/
/*
 *  If we managed to output everything, return 1, else 0.
 */
    if (i == inLength)
	return(1);
    else
	return(0);
}



#define	BUF		40
#define	DEFPREC		6

int ADIcnvPrintf( const int maxLength, char *string ,
		  char *fmt0, va_list ap )
  {
  register char 	*fmt;		/* format string */
  register int 		ch;		/* character from fmt */
  register int n;		/* handy integer (short term usage) */
	register char *cp;/* handy char pointer (short term usage) */
	register int flags;	/* flags as above */
	int ret;		/* return value accumulator */
	int width;		/* width from format (%8d), or 0 */
	int prec;		/* precision from format (%.3d), or -1 */
	char sign;		/* sign prefix (' ', '+', '-', or \0) */
	char softsign;		/* temporary negative sign for floats */
	double _double;		/* double precision arguments %[eEfgG] */
	int fpprec;		/* `extra' floating precision in [eEfgG] */
	unsigned long _ulong ;	/* integer arguments %[diouxX] */
	enum { OCT, DEC, HEX } base;/* base for [diouxX] conversion */
	int dprec;		/* a copy of prec if [diouxX], 0 otherwise */
	int fieldsz;		/* field size expanded by sign, etc */
	int realsz;		/* field size expanded by dprec */
	int size;		/* size of converted field or string */
	char *xdigs="";		/* digits for [xX] conversion */
	char buf[BUF];		/* space for %c, %[diouxX], %[eEfgG] */
	char ox[2];		/* space for 0x hex-prefix */

	int current;		/* Pointer to current length */

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
	if (!Cnv_SPrintf(maxLength,(ptr),(len),(string),&current)) \
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

	if ((maxLength < 0)||(string == 0))
	    return(EOF);

	fmt = fmt0;
	ret = 0;
	current = 0;

	/*
	 * Scan the format for conversions (`%' character).
	 */
	for (;;) {
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
		case 'O':
			flags |= LONGINT;
			/*FALLTHROUGH*/
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
		case 's':
			if ((cp = va_arg(ap, char *)) == NULL)
				cp = "(null)";
			if (prec >= 0) {
				/*
				 * can't use strlen; can only look for the
				 * NUL in the first `prec' characters, and
				 * strlen() will go further.
				 */
				char *p = memchr(cp, 0, prec);

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



typedef
  struct {
    ADIclassDef		*to;		/* Type procedure converts to */
    ADIconvertor	func;		/* The convertor procedure */
    ADIobj		next;	        /* Next convertor in list */
    }
  ADIcnvEntry;

#define         KT_CTYPE_cnv     ADIcnvEntry
#define		KT_DEFN_cnv      ADI_G_tdef_cnv
#define         KT_CODE_cnv      (-17)
#define         _cnv_data(_x)    ((ADIcnvEntry *) _ID_DATA(_x))
#define         _cnv_to(_x)      (_cnv_data(_x)->to)
#define         _cnv_func(_x)    (_cnv_data(_x)->func)
#define         _cnv_next(_x)    (_cnv_data(_x)->next)

/*
 *  Allocation control for convertor function list elements
 */
_DEF_STATIC_CDEF("_ConvertorFunction",cnv,128,NULL,NULL);


/*
 *  Look up convertor function from a class 'from' to a class 'to'
 */
ADIconvertor ADIcnvFind( ADIclassDef *from, ADIclassDef *to,
		ADIstatus status )
  {
  ADIobj       	crec = ADI__nullid;
  ADIobj	cur = from->cnvs;

  while ( _valid_q(cur) && _null_q(crec) ) {
    if ( _cnv_to(cur) == to )
      crec = cur;
    else
      cur = _cnv_next(cur);
    }

  return _ok(status) && _valid_q(crec) ? _cnv_func(crec) : NULL;
  }


void ADIcnvNew( ADIclassDef *from, ADIclassDef *to,
		ADIconvertor func, ADIstatus status )
  {
  ADIobj	newid = ADI__nullid;

  newid = adix_cls_alloc( &KT_DEFN_cnv, status );

/* Allocated ok */
  if ( _ok(status) ) {
    ADIobj	*ipoint = &from->cnvs;

/* Look for insertion point */
    while ( _valid_q(*ipoint) )
      ipoint = &_cnv_next(*ipoint);

/* Insert at end of list */
    *ipoint = newid;

/* Set the fields */
    _cnv_to(newid) = to;
    _cnv_func(newid) = func;
    _cnv_next(newid) = ADI__nullid;
    }
  }


/* Variables required to traverse character strings of the 3 types
 */
#define _declare_c_tr(_prefix,_mta) \
  int		_prefix##_is_adi = _valid_q(_mta->id); \
  int		_prefix##_nterm = (_mta->size == _CSM); \
  int		_prefix##_clen = _mta->size; \
  int		_prefix##_blen = _mta->size; \
  char          **_prefix##dptr; \
  char		*_prefix##_buf; \
  char          *_prefix##ptr; \
  ADIstring     *_prefix##sptr = NULL;\
  int		_prefix##_nulterm = _mta->clang

/* Macro sets up one of the 3 pointer forms to traverse character data
 */
#define _start_c_tr(_pref,_data) \
  if ( _pref##_nterm ) _pref##dptr = (char **) _data; \
  else if ( _pref##_is_adi ) _pref##sptr = (ADIstring *) _data; \
  else _pref##ptr = (char *) _data;

#define _setbuf_c_tr(_pref) \
    if ( _pref##_nterm ) \
      {_pref##_buf=*_pref##dptr; _pref##_blen=strlen(_pref##_buf);}\
    else if ( _pref##_is_adi ) \
      {_pref##_buf=_pref##sptr->data;_pref##_blen=_pref##sptr->len;} \
    else \
      _pref##_buf = _pref##ptr;

#define _coppad_c_tr(_pref,_source) \
  {char *sptr;int ic; \
  if ( _pref##_nterm ) {_pref##_buf=*_pref##dptr;} \
  else if ( _pref##_is_adi ) {_pref##_buf=_pref##sptr->data;\
	_pref##_blen=_pref##sptr->len;} \
  else _pref##_buf = _pref##ptr; \
  for(sptr=_source,ic=0;*sptr && (ic<_pref##_blen);sptr++,ic++) \
    _pref##_buf[ic] = *sptr;\
  if ( *sptr ) (*nerr)++;\
  else \
    {if ( _pref##_nterm && ((ic+1)<_pref##_blen)) _pref##_buf[ic]=0; \
     else if ( (ic+1) < _pref##_blen ) \
       {if (_pref##_nulterm) _pref##_buf[ic]=0;else \
       memset(_pref##_buf+ic,' ',_pref##_blen-ic);}}\
  }

#define _advance_c_tr(_prefix) \
    if ( _prefix##_nterm ) _prefix##dptr++; \
    else { if ( _prefix##_is_adi ) _prefix##sptr++; \
    else _prefix##ptr += _prefix##_clen;}


/*
 *  Convert CHAR -> CHAR
 */
void ADIcnvCC( ADImta *idd, int nval, char *in, ADImta *odd,
		   char *out, int *nerr, ADIstatus status )
  {
  _declare_c_tr(i,idd);			/* Cursor over inputs */
  _declare_c_tr(o,odd);
  int           ic;                     /* Loop over converted data */
  int           ival = nval;            /* Loop over input values */

  _chk_stat;                            /* Check status on entry */

  _start_c_tr(i,in);
  _start_c_tr(o,out);

/* Loop over input strings */
  for( ; ival--; ) {
    _setbuf_c_tr(i);

    if ( o_is_adi ) {                 /* Allowed length for output */
      if ( ! osptr->data && ! osptr->len ) {
        if ( i_blen > 0 ) {
          osptr->data = strx_alloc( i_blen, status );
          if ( _ok(status) )
            osptr->len = i_blen;
          }
        }
      o_buf = osptr->data;
      o_blen = osptr->len;
      }
    else if ( o_nterm ) {
      o_buf = *odptr;
      o_blen = 999;
      }
    else
      o_buf = optr;

    for( ic=0; ic<_MIN(i_blen,o_blen); ic++ )
      o_buf[ic] = i_buf[ic];

    if ( i_blen < o_blen ) {              /* Null terminate or pad */
      if ( o_nulterm )
	o_buf[i_blen] = 0;
      else
	memset( o_buf + i_blen, ' ', o_blen - i_blen );
      }

/* Advance pointers */
    _advance_c_tr(i);
    _advance_c_tr(o);
    }
  }


#define _cnv_to_c(_T,_t,_caste,_fmt) \
void ADIcnv##_T##C( ADImta *idd, int n, char *in, ADImta *odd, \
		    char *out, int *nerr, ADIstatus status ) { \
  char          buf[30]; \
  _TM_ctype(_t)	*iptr = (_TM_ctype(_t) *) in; \
  int           ival = n; \
 _declare_c_tr(o,odd); \
  _chk_stat; \
  _start_c_tr(o,out); \
  for( ; ival--; ) { _caste val = *iptr++;  \
    sprintf( buf, _fmt, val ); \
    _setbuf_c_tr(o); _coppad_c_tr(o,buf); _advance_c_tr(o); } \
  }

_cnv_to_c(UB,ub,int,"%d")
_cnv_to_c(B,b,int,"%d")
_cnv_to_c(UW,uw,int,"%d")
_cnv_to_c(W,w,int,"%d")
_cnv_to_c(I,i,ADIinteger,"%ld")
_cnv_to_c(R,r,ADIdouble,"%g")
_cnv_to_c(D,d,ADIdouble,"%g")
_cnv_to_c(P,p,ADIpointer,"%x")


int ADIcnvCtoD( char *dat, int dlen, ADIdouble *val )
  {
  char		buf[30];
  char		*dptr = dat;
  int		ic;
  int		len = dlen;
  int		ok = 0;
  ADIdouble	r = 0;
  char		*uptr = dat;

/* Check string for Fortran type 'd' or 'D' characters. If found, copy */
/* the string to our buffer. Also copy to buffer if not null-terminated */
  if ( len > 0 ) {
    _CH_MOVE( buf, dat, len );
    buf[len] = 0;
    for( ic=0; ic<len; ic++ )
      if ( (buf[ic] == 'd') || (buf[ic] == 'D') )
	buf[ic] = 'e';
    uptr = buf;
    }
  else if ( len == _CSM ) {
    int	dp=0;

    for( ic=0; dptr[ic]; ic++ )
      if ( (buf[ic] == 'd') || (buf[ic] == 'D') )
	dp = ic;

    if ( dp ) {
      len = ic - 1;
      _CH_MOVE( buf, dat, len );
      buf[len] = 0;
      uptr = buf;
      }
    }

  if ( len ) {
    ok = sscanf( uptr, "%lg", val );
    }

  return ok;
  }


int ADIcnvCtoI( char *dat, int dlen, ADIinteger *val )
  {
  char		*dptr = dat;
  int		i = dlen;
  int		ok = 0;
  ADIinteger	r = 0;

  if ( dlen > 0 ) {
    while ( i-- && isdigit(*dptr) ) {
      r = r*10 + (*dptr - '0');
      dptr++;
      }
    ok = (!i);
    }
  else if ( dlen == _CSM ) {
    while ( isdigit(*dptr) ) {
      r = r*10 + (*dptr - '0');
      dptr++;
      }
    ok = ! *dptr;
    }

  *val = r;

  return ok;
  }

#define _chk_scan_2(_convt,_ctp,_ct) \
  {\
  _convt      out;\
  if ( ADIcnvCto##_ctp( i_buf, i_blen, &out ) ) {\
  if ( out<((_convt) _TM_min(_ct)) || out>((_convt) _TM_max(_ct)) )\
    {*optr = _TM_bad(_ct); (*nerr)++;}\
  else \
    *optr = (_TM_ctype(_ct)) out;} \
  else \
    {*optr = _TM_bad(_ct); (*nerr)++;} \
  }

#define _chk_scan_1(_convt,_ctp,_ct) {\
  _convt      out;\
  if ( ADIcnvCto##_ctp( i_buf, i_blen, &out ) ) \
    *optr = (_convt) out; \
  else \
    {*optr = _TM_bad(_ct); (*nerr)++;} \
  }

void ADIcnvCUB( ADImta *idd, int nval, char *in, ADImta *odd,
		char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIubyte	*optr = (ADIubyte *)out;/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIinteger,I,ub);
    _advance_c_tr(i);
    }
  }

void ADIcnvCB( ADImta *idd, int nval, char *in, ADImta *odd,
		char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIbyte	*optr = (ADIbyte *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIinteger,I,b);
    _advance_c_tr(i);
    }
  }

void ADIcnvCUW( ADImta *idd, int nval, char *in, ADImta *odd,
		char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIuword	*optr = (ADIuword *)out;/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIinteger,I,uw);
    _advance_c_tr(i);
    }
  }

void ADIcnvCW( ADImta *idd, int nval, char *in, ADImta *odd,
	       char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIword	*optr = (ADIword *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIinteger,I,w);
    _advance_c_tr(i);
    }
  }

void ADIcnvCI( ADImta *idd, int nval, char *in, ADImta *odd,
	       char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIinteger	*optr = (ADIinteger *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIdouble,D,i);
    _advance_c_tr(i);
    }
  }

void ADIcnvCR( ADImta *idd, int nval, char *in, ADImta *odd,
	       char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIreal	*optr = (ADIreal *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIdouble,D,r);
    _advance_c_tr(i);
    }
  }

void ADIcnvCD( ADImta *idd, int nval, char *in, ADImta *odd,
	       char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIdouble	*optr = (ADIdouble *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_1(ADIdouble,D,d);
    _advance_c_tr(i);
    }
  }



/*
 *  Convert LOGICAL -> LOGICAL
 */
#ifdef ADI_F77
void ADIcnvLL( ADImta *idd, int n, char *in, ADImta *odd,
		   char *out, int *nerr, ADIstatus status )
  {
  int		in_c = idd->clang;
  int		out_c = odd->clang;
  int           i = n;
  ADIlogical    *iptr = (ADIlogical *) in;
  ADIlogical    *optr = (ADIlogical *) out;

  if ( in_c && ! out_c ) {
    for( ; i-- ; )
      *optr++ = (*iptr++) ? F77_TRUE : F77_FALSE;
    }
  else if ( out_c && ! in_c ) {
    for( ; i-- ; )
      *optr++ = ((*iptr++) == F77_FALSE) ? ADI__false : ADI__true;
    }
  else
    _CH_MOVE( out, in, n*sizeof(ADIlogical) );
  }
#endif

#define _do_case_chk(_it,_ot) \
      _TM_ctype(_it) *iptr = (_TM_ctype(_it) *) in; \
      _TM_ctype(_ot) *optr = (_TM_ctype(_ot) *) out; \
      int	ival = n; \
      _chk_stat; \
      for( ; ival--; )\
	*optr++ = *iptr++;

#define _do_case_chk_min(_it,_ot) \
      _TM_ctype(_it) *iptr = (_TM_ctype(_it) *) in; \
      _TM_ctype(_ot) *optr = (_TM_ctype(_ot) *) out; \
      int	ival = n; \
      _chk_stat; \
      for( ; ival--; iptr++, optr++ ) {\
	if ( *iptr < ((_TM_ctype(_it)) _TM_min(_ot)) )\
	  {*optr = _TM_bad(_ot); (*nerr)++;}\
	else \
	  *optr = (_TM_ctype(_ot)) *iptr;} \

#define _do_case_chk_max(_it,_ot) \
      _TM_ctype(_it) *iptr = (_TM_ctype(_it) *) in; \
      _TM_ctype(_ot) *optr = (_TM_ctype(_ot) *) out; \
      int	ival = n; \
      _chk_stat; \
      for( ; ival--; iptr++, optr++ ) {\
	if ( *iptr > ((_TM_ctype(_it)) _TM_max(_ot)) )\
	  {*optr = _TM_bad(_ot); (*nerr)++;}\
	else \
	  *optr = (_TM_ctype(_ot)) *iptr;} \

#define _do_case_chk_bth(_it,_ot) \
      _TM_ctype(_it) *iptr = (_TM_ctype(_it) *) in; \
      _TM_ctype(_ot) *optr = (_TM_ctype(_ot) *) out; \
      int	ival = n; \
      _chk_stat; \
      for( ; ival--; iptr++, optr++ ) {\
	if ( *iptr < ((_TM_ctype(_it)) _TM_min(_ot)) || *iptr > ((_TM_ctype(_it)) _TM_max(_ot)) )\
	  {*optr = _TM_bad(_ot); (*nerr)++;}\
	else \
	  *optr = (_TM_ctype(_ot)) *iptr;} \

/*
 * Caste to _UBYTE
 */
void ADIcnvBUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(b,ub);}
void ADIcnvWUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(w,ub);}
void ADIcnvUWUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(uw,ub);}
void ADIcnvIUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(i,ub);}
void ADIcnvRUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,ub);}
void ADIcnvDUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,ub);}


/*
 * Caste to _BYTE
 */
void ADIcnvUBB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(ub,b);}
void ADIcnvWB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(w,b);}
void ADIcnvUWB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(uw,b);}
void ADIcnvIB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(i,b);}
void ADIcnvRB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,b);}
void ADIcnvDB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,b);}


/*
 * Caste to _WORD
 */
void ADIcnvUBW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,w);}
void ADIcnvBW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(b,w);}
void ADIcnvUWW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(uw,w);}
void ADIcnvIW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(i,w);}
void ADIcnvRW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,w);}
void ADIcnvDW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,w);}


/*
 * Caste to _UWORD
 */
void ADIcnvUBUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,uw);}
void ADIcnvBUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_min(b,uw);}
void ADIcnvWUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(w,uw);}
void ADIcnvIUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(i,uw);}
void ADIcnvRUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,uw);}
void ADIcnvDUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,uw);}

/*
 * Caste to _INTEGER
 */
void ADIcnvUBI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,i);}
void ADIcnvBI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(b,i);}
void ADIcnvUWI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(uw,i);}
void ADIcnvWI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(w,i);}
void ADIcnvRI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,i);}
void ADIcnvDI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,i);}

/*
 * Caste to REAL
 */
void ADIcnvUBR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,r);}
void ADIcnvBR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(b,r);}
void ADIcnvUWR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(uw,r);}
void ADIcnvWR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(w,r);}
void ADIcnvIR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(i,r);}
void ADIcnvDR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,r);}

/*
 * Caste to _DOUBLE
 */
void ADIcnvUBD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,d);}
void ADIcnvBD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(b,d);}
void ADIcnvUWD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(uw,d);}
void ADIcnvWD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(w,d);}
void ADIcnvID( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(i,d);}
void ADIcnvRD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(r,d);}


typedef
  enum {ub=0,b=1,uw=2,w=3,i=4,r=5,d=6,c=7,l=8,p=9}
  tcode;

void ADIcnvInit( ADIstatus status )
  {
  static struct {
    tcode t1,t2;
    ADIconvertor func;
    }
    ctable[] = {
    {ub, c, ADIcnvUBC },
    { b, c, ADIcnvBC },
    {uw, c, ADIcnvUWC },
    { w, c, ADIcnvWC },
    { i, c, ADIcnvIC },
    { r, c, ADIcnvRC },
    { d, c, ADIcnvDC },

    { c, c, ADIcnvCC },

    { c,ub, ADIcnvCUB },
    { c, b, ADIcnvCB },
    { c,uw, ADIcnvCUW },
    { c, w, ADIcnvCW },
    { c, i, ADIcnvCI },
    { c, r, ADIcnvCR },
    { c, d, ADIcnvCD },

    { b,ub, ADIcnvBUB },
    {uw,ub, ADIcnvUWUB },
    { w,ub, ADIcnvWUB },
    { i,ub, ADIcnvIUB },
    { r,ub, ADIcnvRUB },
    { d,ub, ADIcnvDUB },

    {ub, b, ADIcnvUBB },
    {uw, b, ADIcnvUWB },
    { w, b, ADIcnvWB },
    { i, b, ADIcnvIB },
    { r, b, ADIcnvRB },
    { d, b, ADIcnvDB },

    {ub,uw, ADIcnvUBUW },
    { b,uw, ADIcnvBUW },
    { w,uw, ADIcnvWUW },
    { i,uw, ADIcnvIUW },
    { r,uw, ADIcnvRUW },
    { d,uw, ADIcnvDUW },

    {ub, w, ADIcnvUBW },
    { b, w, ADIcnvBW },
    {uw, w, ADIcnvUWW },
    { i, w, ADIcnvIW },
    { r, w, ADIcnvRW },
    { d, w, ADIcnvDW },

    {ub, i, ADIcnvUBI },
    { b, i, ADIcnvBI },
    {uw, i, ADIcnvUWI },
    { w, i, ADIcnvWI },
    { r, i, ADIcnvRI },
    { d, i, ADIcnvDI },

    {ub, r, ADIcnvUBR },
    { b, r, ADIcnvBR },
    {uw, r, ADIcnvUWR },
    { w, r, ADIcnvIR },
    { i, r, ADIcnvIR },
    { d, r, ADIcnvDR },

    {ub, d, ADIcnvUBD },
    { b, d, ADIcnvBD },
    {uw, d, ADIcnvUWD },
    { w, d, ADIcnvID },
    { i, d, ADIcnvID },
    { r, d, ADIcnvRD },

#ifdef ADI_F77
    { l, l, ADIcnvLL },
#endif
    {ub,ub,NULL}
    };

  ADIclassDef	*cdef[10];
  int		it;

  cdef[ub] = _cdef_data(UT_ALLOC_ub);
  cdef[b]  = _cdef_data(UT_ALLOC_b);
  cdef[uw] = _cdef_data(UT_ALLOC_uw);
  cdef[w]  = _cdef_data(UT_ALLOC_w);
  cdef[i]  = _cdef_data(UT_ALLOC_i);
  cdef[r]  = _cdef_data(UT_ALLOC_r);
  cdef[d]  = _cdef_data(UT_ALLOC_d);
  cdef[c]  = _cdef_data(UT_ALLOC_c);
  cdef[l]  = _cdef_data(UT_ALLOC_l);
  cdef[p]  = _cdef_data(UT_ALLOC_p);

/* Define convertor functions */
  for( it=0; ctable[it].func; it++ )
    ADIcnvNew( cdef[ctable[it].t1], cdef[ctable[it].t2],
	       ctable[it].func, status );

  }
