/*** File libwcs/hget.c
 *** September 29, 1998
 *** By Doug Mink, Harvard-Smithsonian Center for Astrophysics

 * Module:	hget.c (Get FITS Header parameter values)
 * Purpose:	Extract values for variables from FITS header string
 * Subroutine:	hgeti2 (hstring,keyword,ival) returns short integer
 * Subroutine:	hgeti4 (hstring,keyword,ival) returns long integer
 * Subroutine:	hgetr4 (hstring,keyword,rval) returns real
 * Subroutine:	hgetra (hstring,keyword,ra) returns double RA in degrees
 * Subroutine:	hgetdec (hstring,keyword,dec) returns double Dec in degrees
 * Subroutine:	hgetr8 (hstring,keyword,dval) returns double
 * Subroutine:	hgetl  (hstring,keyword,lval) returns logical int (0=F, 1=T)
 * Subroutine:	hgets  (hstring,keyword, lstr, str) returns character string
 * Subroutine:	hgetm  (hstring,keyword, lstr, str) returns multi-keyword string
 * Subroutine:	hgetdate (hstring,keyword,date) returns date as fractional year
 * Subroutine:  hgetndec (hstring, keyword, ndec) returns number of dec. places
 * Subroutine:	hgetc  (hstring,keyword) returns character string
 * Subroutine:	blsearch (hstring,keyword) returns pointer to blank lines
		before keyword
 * Subroutine:	ksearch (hstring,keyword) returns pointer to header string entry
 * Subroutine:	str2ra (in) converts string to right ascension in degrees
 * Subroutine:	str2dec (in) converts string to declination in degrees
 * Subroutine:	strsrch (s1, s2) finds string s2 in null-terminated string s1
 * Subroutine:	strnsrch (s1, s2, ls1) finds string s2 in ls1-byte string s1
 * Subroutine:	hlength (header,lhead) sets length of FITS header for searching
 * Subroutine:  isnum (string) returns 1 if number, else 0
 * Subroutine:  notnum (string) returns 0 if number, else 1

 * Copyright:   1995-1998 Smithsonian Astrophysical Observatory
 *              You may do anything you like with this file except remove
 *              this copyright.  The Smithsonian Astrophysical Observatory
 *              makes no representations about the suitability of this
 *              software for any purpose.  It is provided "as is" without
 *              express or implied warranty.

*/

#include <string.h>		/* NULL, strlen, strstr, strcpy */
#include <stdio.h>
#include "fitshead.h"	/* FITS header extraction subroutines */
#include <stdlib.h>
#ifndef VMS
#include <values.h>
#else
#define MAXINT  2147483647 /* Biggest number that can fit in long */
#define MAXSHORT 32767
#endif

#ifdef USE_SAOLIB
static int use_saolib=0;
#endif

char *hgetc ();

char val[30];


/* Set the length of the header string, if not terminated by NULL */

static int lhead0 = 0;
int
hlength (header, lhead)
char	*header; /* FITS header */
int	lhead;	/* Maximum length of FITS header */
{
    char *hend;
    lhead0 = lhead;
    if (lhead < 1) {
	hend = ksearch (header,"END");
	lhead0 = hend + 80 - header;
	}
    return (lhead0);
}


/* Extract long value for variable from FITS header string */

int
hgeti4 (hstring,keyword,ival)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
int *ival;
{
char *value;
double dval;
int minint;

/* Get value and comment from header string */
	value = hgetc (hstring,keyword);

/* Translate value from ASCII to binary */
	if (value != NULL) {
	    minint = -MAXINT - 1;
	    strcpy (val, value);
	    dval = atof (val);
	    if (dval+0.001 > MAXINT)
		*ival = MAXINT;
	    else if (dval >= 0)
		*ival = (int) (dval + 0.001);
	    else if (dval-0.001 < minint)
		*ival = minint;
	    else
		*ival = (int) (dval - 0.001);
	    return (1);
	    }
	else {
	    return (0);
	    }
}


/* Extract integer*2 value for variable from fits header string */

int
hgeti2 (hstring,keyword,ival)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
short *ival;
{
char *value;
double dval;
int minshort;

/* Get value and comment from header string */
	value = hgetc (hstring,keyword);

/* Translate value from ASCII to binary */
	if (value != NULL) {
	    strcpy (val, value);
	    dval = atof (val);
	    minshort = -MAXSHORT - 1;
	    if (dval+0.001 > MAXSHORT)
		*ival = MAXSHORT;
	    else if (dval >= 0)
		*ival = (short) (dval + 0.001);
	    else if (dval-0.001 < minshort)
		*ival = minshort;
	    else
		*ival = (short) (dval - 0.001);
	    return (1);
	    }
	else {
	    return (0);
	    }
}

/* Extract real value for variable from FITS header string */

int
hgetr4 (hstring,keyword,rval)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
float *rval;
{
	char *value;

/* Get value and comment from header string */
	value = hgetc (hstring,keyword);

/* translate value from ASCII to binary */
	if (value != NULL) {
	    strcpy (val, value);
	    *rval = (float) atof (val);
	    return (1);
	    }
	else {
	    return (0);
	    }
}


/* Extract real*8 right ascension in degrees from FITS header string */

int
hgetra (hstring,keyword,dval)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
double *dval;	/* Right ascension in degrees (returned) */
{
	char *value;

/* Get value from header string */
	value = hgetc (hstring,keyword);

	    *dval = *dval * 15.0;
/* Translate value from ASCII colon-delimited string to binary */
	if (value != NULL) {
	    *dval = str2ra (value);
	    return (1);
	    }
	else
	    return (0);
}


/* Extract real*8 declination in degrees from FITS header string */

int
hgetdec (hstring,keyword,dval)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
double *dval;	/* Right ascension in degrees (returned) */
{
	char *value;

/* Get value from header string */
	value = hgetc (hstring,keyword);

/* Translate value from ASCII colon-delimited string to binary */
	if (value != NULL) {
	    *dval = str2dec (value);
	    return (1);
	    }
	else
	    return (0);
}


/* Extract real*8 value for variable from FITS header string */

int
hgetr8 (hstring,keyword,dval)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
double *dval;
{
	char *value,val[30];

/* Get value and comment from header string */
	value = hgetc (hstring,keyword);

/* Translate value from ASCII to binary */
	if (value != NULL) {
	    strncpy (val, value, sizeof(val)-1);
	    *dval = atof (val);
	    return (1);
	    }
	else {
	    return (0);
	    }
}


/* Extract logical value for variable from FITS header string */

int
hgetl (hstring,keyword,ival)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
int *ival;
{
	char *value;

/* Get value and comment from header string */
	value = hgetc (hstring,keyword);

/* Translate value from ASCII to binary */
	if (value != NULL) {
	    strcpy (val, value);
            value = &val[0];
	    if (value[0] == 't' || value[0] == 'T')
		*ival = 1;
	    else
		*ival = 0;
	    return (1);
	    }
	else {
	    return (0);
	    }
}


/* Extract real*8 date from FITS header string (dd/mm/yy or dd-mm-yy) */

int
hgetdate (hstring,keyword,dval)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
double *dval;
{
	double yeardays, seconds, fday;
	char *value,*sstr, *dstr, *tstr, *cstr, *nval;
	int year, month, day, yday, i, hours, minutes;
	static int mday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

/* Get value and comment from header string */
	value = hgetc (hstring,keyword);

/* Translate value from ASCII to binary */
	if (value != NULL) {
	    sstr = strchr (value,'/');
	    dstr = strchr (value,'-');

	/* Original FITS date format: dd/mm/yy */
	    if (sstr > value) {
		*sstr = '\0';
		day = (int) atof (value);
		nval = sstr + 1;
		sstr = strchr (nval,'/');
		if (sstr == NULL)
		    sstr = strchr (nval,'-');
		if (sstr > value) {
		    *sstr = '\0';
		    month = (int) atof (nval);
		    nval = sstr + 1;
		    year = (int) atof (nval);
		    if (year >= 0 && year <= 49)
			year = year + 2000;
		    else if (year < 100)
			year = year + 1900;
		    if ((year % 4) == 0)
			mday[1] = 29;
		    else
			mday[1] = 28;
		    if ((year % 100) == 0 && (year % 400) != 0)
			mday[1] = 28;
		    if (day > mday[month-1])
			day = mday[month-1];
		    else if (day < 1)
			day = 1;
		    if (mday[1] == 28)
			yeardays = 365.0;
		    else
			yeardays = 366.0;
		    yday = day - 1;
		    for (i = 0; i < month-1; i++)
			yday = yday + mday[i];
		    *dval = (double) year + ((double)yday / yeardays);
		    return (1);
		    }
		else
		    return (0);
		}

	/* New FITS date format: yyyy-mm-ddThh:mm:ss[.sss] */
	    else if (dstr > value) {
		*dstr = '\0';
		year = (int) atof (value);
		nval = dstr + 1;
		dstr = strchr (nval,'-');
		month = 1;
		day = 1;
		tstr = NULL;
		if (dstr > value) {
		    *dstr = '\0';
		    month = (int) atof (nval);
		    nval = dstr + 1;
		    tstr = strchr (nval,'T');
		    if (tstr > value)
			*tstr = '\0';
		    day = (int) atof (nval);
		    }

		/* If year is < 32, it is really day of month in old format */
		if (year < 32) {
		    i = year;
		    year = day + 1900;
		    day = i;
		    }
		    
		if ((year % 4) == 0)
		    mday[1] = 29;
		else
		    mday[1] = 28;
		if ((year % 100) == 0 && (year % 400) != 0)
		    mday[1] = 28;
		if (day > mday[month-1])
		    day = mday[month-1];
		else if (day < 1)
		    day = 1;
		if (mday[1] == 28)
		    yeardays = 365.0;
		else
		    yeardays = 366.0;
		yday = day - 1;
		for (i = 0; i < month-1; i++)
		    yday = yday + mday[i];
		*dval = (double) year + ((double)yday / yeardays);

	/* Extract time, if it is present */
		if (tstr > value) {
		    nval = tstr + 1;
		    hours = 0.0;
		    minutes = 0.0;
		    seconds = 0.0;
		    cstr = strchr (nval,':');
		    if (cstr > value) {
			*cstr = '\0';
			hours = (int) atof (nval);
			nval = cstr + 1;
			cstr = strchr (nval,':');
			if (cstr > value) {
			    minutes = (int) atof (nval);
			    nval = cstr + 1;
			    cstr = strchr (nval,':');
			    if (cstr > value)
				seconds = atof (nval);
			    }
			}
		    fday = ((3.6e3 * (double)hours) + (6.e1 * (double)minutes) +
			   seconds) / 8.64e4;
		    *dval = *dval + (fday / yeardays);
		    }
		return (1);
		}
	    else
		return (0);
	    }
	else
	    return (0);
}


/* Extract IRAF multiple-keyword string value from FITS header string */

int
hgetm (hstring, keyword, lstr, str)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the root name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
int lstr;	/* Size of str in characters */
char *str;	/* String (returned) */
{
    char *value;
    char *stri;
    char keywordi[16];
    int lval, lstri, ikey;

    stri = str;
    lstri = lstr;

    /* Loop through sequentially-named keywords */
    for (ikey = 1; ikey < 20; ikey++) {
	sprintf (keywordi, "%s_%03d", keyword, ikey);

	/* Get value for this keyword */
	value = hgetc (hstring, keywordi);
	if (value != NULL) {
	    lval = strlen (value);
	    if (lval < lstri)
		strcpy (stri, value);
	    else if (lstri > 1) {
		strncpy (stri, value, lstri-1);
		break;
		}
	    else {
		str[0] = value[0];
		break;
		}
	    }
	else
	    break;
	stri = stri + lval;
	lstri = lstri - lval;
	}

    /* Return 1 if any keyword found, else 0 */
    if (ikey > 1)
	return (1);
    else
	return (0);
}


/* Extract string value for variable from FITS header string */

int
hgets (hstring, keyword, lstr, str)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
int lstr;	/* Size of str in characters */
char *str;	/* String (returned) */
{
	char *value;
	int lval;

/* Get value and comment from header string */
	value = hgetc (hstring,keyword);

	if (value != NULL) {
	    lval = strlen (value);
	    if (lval < lstr)
		strcpy (str, value);
	    else if (lstr > 1)
		strncpy (str, value, lstr-1);
	    else
		str[0] = value[0];
	    return (1);
	    }
	else
	    return (0);
}


/* Extract number of decimal places for value in FITS header string */

int
hgetndec (hstring, keyword, ndec)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
int *ndec;	/* Number of decimal places in keyword value */
{
    char *value,val[30];
    int i, nchar;

    /* Get value and comment from header string */
    value = hgetc (hstring,keyword);

    /* Find end of string and count backward to decimal point */
    *ndec = 0;
    if (value != NULL) {
	nchar = strlen (value);
	for (i = nchar-1; i >= 0; i--) {
	    if (value[i] == '.')
		return (1);
	    *ndec = *ndec + 1;
	    }
	}
    else
	return (0);
}


/* Extract character value for variable from FITS header string */

char *
hgetc (hstring,keyword0)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword0;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
{
	static char cval[80];
	char *value;
	char cwhite[2];
	char squot[2], dquot[2], lbracket[2], rbracket[2], slash[2], comma[2];
	char keyword[81]; /* large for ESO hierarchical keywords */
	char line[100];
	char *vpos, *cpar;
	char *q1, *q2, *v1, *v2, *c1, *brack1, *brack2;
	int ipar, i;

#ifdef USE_SAOLIB
	int iel=1, ip=1, nel, np, ier;
	char *get_fits_head_str();

	if( !use_saolib ){
#endif

	squot[0] = 39;
	squot[1] = 0;
	dquot[0] = 34;
	dquot[1] = 0;
	lbracket[0] = 91;
	lbracket[1] = 0;
	comma[0] = 44;
	comma[1] = 0;
	rbracket[0] = 93;
	rbracket[1] = 0;
	slash[0] = 47;
	slash[1] = 0;

/* Find length of variable name */
	strncpy (keyword,keyword0, sizeof(keyword)-1);
	brack1 = strsrch (keyword,lbracket);
	if (brack1 == NULL)
	    brack1 = strsrch (keyword,comma);
	if (brack1 != NULL) {
	    *brack1 = '\0';
	    brack1++;
	    }

/* Search header string for variable name */
	vpos = ksearch (hstring,keyword);

/* Exit if not found */
	if (vpos == NULL) {
	    return (NULL);
	    }

/* Initialize line to nulls */
	 for (i = 0; i < 100; i++)
	    line[i] = 0;

/* In standard FITS, data lasts until 80th character */

/* Extract entry for this variable from the header */
	strncpy (line,vpos,80);

/* check for quoted value */
	q1 = strsrch (line,squot);
	c1 = strsrch (line,slash);
	if (q1 != NULL) {
	    if (c1 != NULL && q1 < c1)
		q2 = strsrch (q1+1,squot);
	    else if (c1 == NULL)
		q2 = strsrch (q1+1,squot);
	    else
		q1 = NULL;
	    }
	else {
	    q1 = strsrch (line,dquot);
	    if (q1 != NULL) {
		if (c1 != NULL && q1 < c1)
		    q2 = strsrch (q1+1,dquot);
		else if (c1 == NULL)
		    q2 = strsrch (q1+1,dquot);
		else
		    q1 = NULL;
		}
	    else {
		q1 = NULL;
		q2 = line + 10;
		}
	    }

/* Extract value and remove excess spaces */
	if (q1 != NULL) {
	    v1 = q1 + 1;;
	    v2 = q2;
	    c1 = strsrch (q2,"/");
	    }
	else {
	    v1 = strsrch (line,"=") + 1;
	    c1 = strsrch (line,"/");
	    if (c1 != NULL)
		v2 = c1;
	    else
		v2 = line + 79;
	    }

/* Ignore leading spaces */
	while (*v1 == ' ' && v1 < v2) {
	    v1++;
	    }

/* Drop trailing spaces */
	*v2 = '\0';
	v2--;
	while (*v2 == ' ' && v2 > v1) {
	    *v2 = '\0';
	    v2--;
	    }

	if (!strcmp (v1, "-0"))
	    v1++;
	strcpy (cval,v1);
	value = cval;

/* If keyword has brackets, extract appropriate token from value */
	if (brack1 != NULL) {
	    brack2 = strsrch (brack1,rbracket);
	    if (brack2 != NULL)
		*brack2 = '\0';
	    ipar = atoi (brack1);
	    if (ipar > 0) {
		cwhite[0] = ' ';
		cwhite[1] = '\0';
		for (i = 1; i <= ipar; i++) {
		    cpar = strtok (v1,cwhite);
		    v1 = NULL;
		    }
		if (cpar != NULL) {
		    strcpy (cval,cpar);
		    }
		else
		    value = NULL;
		}
	    }

	return (value);
#ifdef USE_SAOLIB
	} else {
	    return(get_fits_head_str(keyword0, iel, ip, &nel, &np, &ier, hstring));
	}
#endif
}


/* Find beginning of fillable blank line before FITS header keyword line */

char *
blsearch (hstring,keyword)

/* Find entry for keyword keyword in FITS header string hstring.
   (the keyword may have a maximum of eight letters)
   NULL is returned if the keyword is not found */

char *hstring;	/* character string containing fits-style header
		information in the format <keyword>= <value> {/ <comment>}
		the default is that each entry is 80 characters long;
		however, lines may be of arbitrary length terminated by
		nulls, carriage returns or linefeeds, if packed is true.  */
char *keyword;	/* character string containing the name of the variable
		to be returned.  ksearch searches for a line beginning
		with this string.  The string may be a character
		literal or a character variable terminated by a null
		or '$'.  it is truncated to 8 characters. */
{
    char *loc, *headnext, *headlast, *pval, *lc, *line;
    char *bval;
    int icol, nextchar, lkey, nleft, lhstr;

    pval = 0;

    /* Search header string for variable name */
    if (lhead0)
	lhstr = lhead0;
    else {
	lhstr = 0;
	while (lhstr < 57600 && hstring[lhstr] != 0)
	    lhstr++;
	}
    headlast = hstring + lhstr;
    headnext = hstring;
    pval = NULL;
    while (headnext < headlast) {
	nleft = headlast - headnext;
	loc = strnsrch (headnext, keyword, nleft);

	/* Exit if keyword is not found */
	if (loc == NULL) {
	    break;
	    }

	icol = (loc - hstring) % 80;
	lkey = strlen (keyword);
	nextchar = (int) *(loc + lkey);

	/* If this is not in the first 8 characters of a line, keep searching */
	if (icol > 7)
	    headnext = loc + 1;

	/* If parameter name in header is longer, keep searching */
	else if (nextchar != 61 && nextchar > 32 && nextchar < 127)
	    headnext = loc + 1;

	/* If preceeding characters in line are not blanks, keep searching */
	else {
	    line = loc - icol;
	    for (lc = line; lc < loc; lc++) {
		if (*lc != ' ')
		    headnext = loc + 1;
		}

	/* Return pointer to start of line if match */
	    if (loc >= headnext) {
		pval = line;
		break;
		}
	    }
	}

    /* Return NULL to calling program if keyword is not found */
    if (pval == NULL)
	return (pval);

    /* Find last nonblank line before requested keyword */
    bval = pval - 80;
    while (!strncmp (bval,"        ",8))
	bval = bval - 80;
    bval = bval + 80;

    /* Return pointer to calling program if blank lines found */
    if (bval < pval)
	return (bval);
    else
	return (NULL);
}


/* Find FITS header line containing specified keyword */

char *ksearch (hstring,keyword)

/* Find entry for keyword keyword in FITS header string hstring.
   (the keyword may have a maximum of eight letters)
   NULL is returned if the keyword is not found */

char *hstring;	/* character string containing fits-style header
		information in the format <keyword>= <value> {/ <comment>}
		the default is that each entry is 80 characters long;
		however, lines may be of arbitrary length terminated by
		nulls, carriage returns or linefeeds, if packed is true.  */
char *keyword;	/* character string containing the name of the variable
		to be returned.  ksearch searches for a line beginning
		with this string.  The string may be a character
		literal or a character variable terminated by a null
		or '$'.  it is truncated to 8 characters. */
{
    char *loc, *headnext, *headlast, *pval, *lc, *line;
    int icol, nextchar, lkey, nleft, lhstr;

#ifdef USE_SAOLIB
	int iel=1, ip=1, nel, np, ier;
	char *get_fits_head_str();

	if( !use_saolib ){
#endif

    pval = 0;

/* Search header string for variable name */
    if (lhead0)
	lhstr = lhead0;
    else {
	lhstr = 0;
	while (lhstr < 57600 && hstring[lhstr] != 0)
	    lhstr++;
	}
    headlast = hstring + lhstr;
    headnext = hstring;
    pval = NULL;
    while (headnext < headlast) {
	nleft = headlast - headnext;
	loc = strnsrch (headnext, keyword, nleft);

	/* Exit if keyword is not found */
	if (loc == NULL) {
	    break;
	    }

	icol = (loc - hstring) % 80;
	lkey = strlen (keyword);
	nextchar = (int) *(loc + lkey);

	/* If this is not in the first 8 characters of a line, keep searching */
	if (icol > 7)
	    headnext = loc + 1;

	/* If parameter name in header is longer, keep searching */
	else if (nextchar != 61 && nextchar > 32 && nextchar < 127)
	    headnext = loc + 1;

	/* If preceeding characters in line are not blanks, keep searching */
	else {
	    line = loc - icol;
	    for (lc = line; lc < loc; lc++) {
		if (*lc != ' ')
		    headnext = loc + 1;
		}

	/* Return pointer to start of line if match */
	    if (loc >= headnext) {
		pval = line;
		break;
		}
	    }
	}

/* Return pointer to calling program */
	return (pval);

#ifdef USE_SAOLIB
	}
	else {
	    if (get_fits_head_str(keyword,iel,ip,&nel,&np,&ier,hstring) != NULL)
		return(hstring);
	    else
		return(NULL);
	    }
#endif
}


/* Read the right ascension, ra, in sexagesimal hours from in[] */

double
str2ra (in)

char	*in;	/* Character string */

{
    double ra;	/* Right ascension in degrees (returned) */

    ra = str2dec (in);
    if (strsrch (in,":"))
	ra = ra * 15.0;

    return (ra);
}


/* Read the declination, dec, in sexagesimal degrees from in[] */

double
str2dec (in)

char	*in;	/* Character string */

{
    double dec;		/* Declination in degrees (returned) */
    double deg, min, sec, sign;
    char *value, *c1;

    dec = 0.0;

    /* Translate value from ASCII colon-delimited string to binary */
    if (in[0]) {
	value = in;
	if (!strsrch (value,"-"))
	    sign = 1.0;
	else {
	    sign = -1.0;
	    value = strsrch (value,"-") + 1;
	    }
	if ((c1 = strsrch (value,":")) != NULL) {
	    *c1 = 0;
	    deg = (double) atoi (value);
	    *c1 = ':';
	    value = c1 + 1;
	    if ((c1 = strsrch (value,":")) != NULL) {
		*c1 = 0;
		min = (double) atoi (value);
		*c1 = ':';
		value = c1 + 1;
		sec = atof (value);
		}
	    else {
		sec = 0.0;
		if ((c1 = strsrch (value,".")) != NULL)
		    min = atof (value);
		if (strlen (value) > 0)
		    min = (double) atoi (value);
		}
	    dec = sign * (deg + (min / 60.0) + (sec / 3600.0));
	    }
	else if ((c1 = strsrch (value,".")) != NULL)
	    dec = sign * atof (value);
	else 
	    dec = sign * (double) atoi (value);
	}
    return (dec);
}


/* Find string s2 within null-terminated string s1 */

char *
strsrch (s1, s2)

char *s1;	/* String to search */
char *s2;	/* String to look for */

{
    int ls1;
    ls1 = strlen (s1);
    return (strnsrch (s1, s2, ls1));
}


/* Find string s2 within string s1 */

char *
strnsrch (s1, s2, ls1)

char	*s1;	/* String to search */
char	*s2;	/* String to look for */
int	ls1;	/* Length of string being searched */

{
    char *s,*s1e;
    char cfirst,clast;
    int i,ls2;

    /* Return null string if either pointer is NULL */
    if (s1 == NULL || s2 == NULL)
	return (NULL);

    /* A zero-length pattern is found in any string */
    ls2 = strlen (s2);
    if (ls2 ==0)
	return (s1);

    /* Only a zero-length string can be found in a zero-length string */
    if (ls1 ==0)
	return (NULL);

    cfirst = s2[0];
    clast = s2[ls2-1];
    s1e = s1 + ls1 - ls2 + 1;
    s = s1;
    while (s < s1e) { 

	/* Search for first character in pattern string */
	if (*s == cfirst) {

	    /* If single character search, return */
	    if (ls2 == 1)
		return (s);

	    /* Search for last character in pattern string if first found */
	    if (s[ls2-1] == clast) {

		/* If two-character search, return */
		if (ls2 == 2)
		    return (s);

		/* If 3 or more characters, check for rest of search string */
		i = 1;
		while (i < ls2 && s[i] == s2[i])
		    i++;

		/* If entire string matches, return */
		if (i >= ls2)
		    return (s);
		}
	    }
	s++;
	}
    return (NULL);
}


int
notnum (string)

char *string;	/* Character string */
{
    if (isnum (string))
	return (0);
    else
	return (1);
}


int
isnum (string)

char *string;	/* Character string */
{
    int lstr, i, nd;
    char cstr;

    lstr = strlen (string);
    nd = 0;
    for (i = 0; i < lstr; i++) {
	cstr = string[i];
	if ((cstr < 48 || cstr > 57) &&
	    cstr != '+' && cstr != '-' &&
	    cstr != 'D' && cstr != 'd' &&
	    cstr != 'E' && cstr != 'e' &&
	    cstr != '.')
	    return (0);
	else if (cstr >= 47 && cstr <= 57)
	    nd++;
	}
    if (nd > 0)
	return (1);
    else
	return (0);
}


#ifdef USE_SAOLIB
int set_saolib(hstring)
    void *hstring;
{
    if( *((int *)hstring) == 142857 )
	use_saolib = 1;
    else
	use_saolib = 0;
}

#endif

/* Oct 28 1994	New program
 *
 * Mar  1 1995	Search for / after second quote, not first one
 * May  2 1995	Initialize line in HGETC; deal with logicals in HGETL better
 * May  4 1995	Declare STRSRCH in KSEARCH
 * Aug  7 1995  Fix line initialization in HGETC
 * Dec 22 1995	Add HGETRA and HGETDEC to get degrees from xx:xx:xx.xxx string
 *
 * Jan 26 1996	Fix HGETL to not crash when parameter is not present
 * Feb  1 1996	Fix HGETC to deal with quotes correctly
 * Feb  1 1996	Fix HGETDEG to deal with sign correctly
 * Feb  6 1996	Add HGETS to update character strings
 * Feb  8 1996	Fix STRSRCH to find final characters in string
 * Feb 23 1996	Add string to degree conversions
 * Apr 26 1996	Add HGETDATE to get fractional year from date string
 * May 22 1996	Fix documentation; return double from STR2RA and STR2DEC
 * May 28 1996	Fix string translation of RA and Dec when no seconds
 * Jun 10 1996	Remove unused variables after running lint
 * Jun 17 1996	Fix bug which failed to return single character strings
 * Jul  1 1996	Skip sign when reading declination after testing for it
 * Jul 19 1996	Do not divide by 15 if RA header value is already in degrees
 * Aug  5 1996	Add STRNSRCH to search strings which are not null-terminated
 * Aug  6 1996	Make minor changes after lint
 * Aug  8 1996	Fix ksearch bug which finds wrong keywords
 * Aug 13 1996	Fix sign bug in STR2DEC for degrees
 * Aug 26 1996	Drop unused variables ICOL0, NLINE, PREVCHAR from KSEARCH
 * Sep 10 1996	Fix header length setting code
 * Oct 15 1996	Clean up loops and fix ICOL assignment
 * Nov 13 1996	Handle integer degrees correctly in STR2DEC
 * Nov 21 1996	Make changes for Linux thanks to Sidik Isani
 * Dec 12 1996	Add ISNUM to check to see whether strings are numbers
 *
 * Jan 22 1997	Add ifdefs for Eric Mandel (SAOtng)
 * Jan 27 1997	Convert to integer through ATOF so exponents are recognized
 * Jul 25 1997	Implement FITS version of ISO date format
 * 
 * Feb 24 1998	Implement code to return IRAF multiple-keyword strings
 * Mar 12 1998	Add subroutine NOTNUM
 * Mar 27 1998	Add changes to match SKYCAT version
 * Apr 30 1998	Add BLSEARCH() to find blank lines before END
 * May 27 1998	Add HGETNDEC() to get number of decimal places in entry
 * Jun  1 1998	Add VMS patch from Harry Payne at StSci
 * Jun 18 1998	Fix code which extracts tokens from string values
 * Jul 21 1998	Drop minus sign for values of -0
 * Sep 29 1998	Treat hyphen-separated date as old format if 2-digit year
 */
