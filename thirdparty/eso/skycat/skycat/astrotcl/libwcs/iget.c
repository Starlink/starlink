/*** File libwcs/iget.c
 *** July 9, 1998
 *** By Doug Mink, Harvard-Smithsonian Center for Astrophysics

 * Module:	iget.c (Get IRAF FITS Header parameter values)
 * Purpose:	Extract values for variables from IRAF keyword value string
 * Subroutine:	mgeti4 (hstring,mkey,keyword,ival) returns long integer
 * Subroutine:	mgetr8 (hstring,mkey,keyword,dval) returns double
 * Subroutine:	mgets  (hstring,mkey,keyword,lstr,str) returns character string
 * Subroutine:	igeti4 (hstring,keyword,ival) returns long integer
 * Subroutine:	igetr4 (hstring,keyword,rval) returns real
 * Subroutine:	igetr8 (hstring,keyword,dval) returns double
 * Subroutine:	igets  (hstring,keyword,lstr,str) returns character string
 * Subroutine:	igetc  (hstring,keyword) returns character string
 * Subroutine:	isearch (hstring,keyword) returns pointer to header string entry

 * Copyright:   1998 Smithsonian Astrophysical Observatory
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

static char *igetc();
static char *isearch();
static char val[30];

/* Extract long value for variable from IRAF multiline keyword value */

int
mgeti4 (hstring, mkey, keyword, ival)

char *hstring;	/* Character string containing FITS or IRAF header information
		   in the format <keyword>= <value> ... */
char *mkey;	/* Character string containing the name of the multi-line
		   keyword, the string value of which contains the desired
		   keyword, the value of which is returned. */
char *keyword;	/* Character string containing the name of the keyword
		   within the multiline IRAF keyword */
int *ival;	/* Integer value returned */
{
    char *mstring;

    mstring = malloc (600);

    if (hgetm (hstring, mkey, 600, mstring)) {
	if (igeti4 (mstring, keyword, ival)) {
	    free (mstring);
	    return (1);
	    }
	else {
	    free (mstring);
	    return (0);
	    }
	}
    else {
	free (mstring);
	return (0);
	}
}

/* Extract double value for variable from IRAF multiline keyword value */

int
mgetr8 (hstring, mkey, keyword, dval)

char	*hstring; /* Character string containing FITS or IRAF header information
		   in the format <keyword>= <value> ... */
char	*mkey;	  /* Character string containing the name of the multi-line
		   keyword, the string value of which contains the desired
		   keyword, the value of which is returned. */
char	*keyword; /* Character string containing the name of the keyword
		   within the multiline IRAF keyword */
double	*dval;	  /* Integer value returned */
{
    char *mstring;
    mstring = malloc (600);

    if (hgetm (hstring, mkey, 600, mstring)) {
	if (igetr8 (mstring, keyword, dval)) {
	    free (mstring);
	    return (1);
	    }
	else {
	    free (mstring);
	    return (0);
	    }
	}
    else {
	free (mstring);
	return (0);
	}
}


/* Extract string value for variable from IRAF keyword value string */

int
mgets (hstring, mkey, keyword, lstr, str)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> {/ <comment>} */
char *mkey;	/* Character string containing the name of the multi-line
		   keyword, the string value of which contains the desired
		   keyword, the value of which is returned. */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
int lstr;	/* Size of str in characters */
char *str;	/* String (returned) */
{
    char *mstring;
    mstring = malloc (600);

    if (hgetm (hstring, mkey, 600, mstring)) {
	if (igets (mstring, keyword, lstr, str)) {
	    free (mstring);
	    return (1);
	    }
	else {
	    free (mstring);
	    return (0);
	    }
	}
    else {
	free (mstring);
	return (0);
	}
}


/* Extract long value for variable from IRAF keyword value string */

int
igeti4 (hstring, keyword, ival)

char *hstring;	/* character string containing IRAF header information
		   in the format <keyword>= <value> ... */
char *keyword;	/* character string containing the name of the keyword
		   the value of which is returned.  hget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
int *ival;	/* Integer value returned */
{
char *value;
double dval;
int minint;

/* Get value from header string */
	value = igetc (hstring,keyword);

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


/* Extract integer*2 value for variable from IRAF keyword value string */

int
igeti2 (hstring,keyword,ival)

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

/* Get value from header string */
	value = igetc (hstring,keyword);

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

/* Extract real value for variable from IRAF keyword value string */

int
igetr4 (hstring,keyword,rval)

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

/* Get value from header string */
	value = igetc (hstring,keyword);

/* Translate value from ASCII to binary */
	if (value != NULL) {
	    strcpy (val, value);
	    *rval = (float) atof (val);
	    return (1);
	    }
	else {
	    return (0);
	    }
}


/* Extract real*8 value for variable from IRAF keyword value string */

int
igetr8 (hstring,keyword,dval)

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

/* Get value from header string */
	value = igetc (hstring,keyword);

/* Translate value from ASCII to binary */
	if (value != NULL) {
	    strcpy (val, value);
	    *dval = atof (val);
	    return (1);
	    }
	else {
	    return (0);
	    }
}


/* Extract string value for variable from IRAF keyword value string */

int
igets (hstring, keyword, lstr, str)

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

/* Get value from header string */
	value = igetc (hstring,keyword);

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


/* Extract character value for variable from IRAF keyword value string */

static char *
igetc (hstring,keyword0)

char *hstring;	/* character string containing IRAF keyword value string
		   in the format <keyword>= <value> {/ <comment>} */
char *keyword0;	/* character string containing the name of the keyword
		   the value of which is returned.  iget searches for a
		   line beginning with this string.  if "[n]" is present,
		   the n'th token in the value is returned.
		   (the first 8 characters must be unique) */
{
	static char cval[500];
	char *value;
	char cwhite[8];
	char squot[2],dquot[2],lbracket[2],rbracket[2],slash[2];
	char keyword[16];
	char line[500];
	char *vpos,*cpar;
	char *q1, *q2, *v1, *v2, *c1, *brack1, *brack2;
	int ipar, i;

	squot[0] = 39;
	squot[1] = 0;
	dquot[0] = 34;
	dquot[1] = 0;
	lbracket[0] = 91;
	lbracket[1] = 0;
	rbracket[0] = 93;
	rbracket[1] = 0;
	slash[0] = 47;
	slash[1] = 0;

/* Find length of variable name */
	strcpy (keyword,keyword0);
	brack1 = strsrch (keyword,lbracket);
	if (brack1 != NULL) *brack1 = '\0';

/* Search header string for variable name */
	vpos = isearch (hstring,keyword);

/* Exit if not found */
	if (vpos == NULL) {
	    return (NULL);
	    }

/* Initialize returned value to nulls */
	 for (i = 0; i < 500; i++)
	    line[i] = 0;

/* If quoted value, copy until second quote is reached */
	i = 0;
	if (*vpos == '"') {
	     vpos++;
	     while (*vpos != '"')
		line[i++] = *vpos++;
	     }

/* Otherwise copy until next space or tab */
	else {
	     while (*vpos != ' ' && *vpos != (char)9 && *vpos > 0)
		line[i++] = *vpos++;
	     }

/* If keyword has brackets, extract appropriate token from value */
	if (brack1 != NULL) {
	    c1 = (char *) (brack1 + 1);
	    brack2 = strsrch (c1, rbracket);
	    if (brack2 != NULL) {
		*brack2 = '\0';
		ipar = atoi (c1);
		if (ipar > 0) {
		    cwhite[0] = ' ';
		    cwhite[1] = ',';
		    cwhite[2] = '\0';
		    cpar = strtok (line, cwhite);
		    for (i = 1; i < ipar; i++) {
			cpar = strtok (NULL, cwhite);
			}
		    if (cpar != NULL) {
			strcpy (cval,cpar);
			}
		    else
			value = NULL;
		    }
		}
	    }
	else
	    strcpy (cval, line);

	value = cval;

	return (value);
}


/* Find value for specified IRAF keyword */

static char *
isearch (hstring,keyword)

/* Find entry for keyword keyword in IRAF keyword value string hstring.
   NULL is returned if the keyword is not found */

char *hstring;	/* character string containing fits-style header
		information in the format <keyword>= <value> {/ <comment>}
		the default is that each entry is 80 characters long;
		however, lines may be of arbitrary length terminated by
		nulls, carriage returns or linefeeds, if packed is true.  */
char *keyword;	/* character string containing the name of the variable
		to be returned.  isearch searches for a line beginning
		with this string.  The string may be a character
		literal or a character variable terminated by a null
		or '$'.  it is truncated to 8 characters. */
{
    char *loc, *headnext, *headlast, *pval;
    int lastchar, nextchar, lkey, nleft, lhstr;

/* Search header string for variable name */
    lhstr = 0;
    while (lhstr < 57600 && hstring[lhstr] != 0)
	lhstr++;
    headlast = hstring + lhstr;
    headnext = hstring;
    pval = NULL;
    lkey = strlen (keyword);
    while (headnext < headlast) {
	nleft = headlast - headnext;
	loc = strnsrch (headnext, keyword, nleft);

	/* Exit if keyword is not found */
	if (loc == NULL) {
	    break;
	    }

	nextchar = (int) *(loc + lkey);
	lastchar = (int) *(loc - 1);

	/* If parameter name in header is longer, keep searching */
	if (nextchar != 61 && nextchar > 32 && nextchar < 127)
	    headnext = loc + 1;

	/* If start of string, keep it */
	else if (loc == hstring) {
	    pval = loc;
	    break;
	    }

	/* If preceeded by a blank or tab, keep it */
	else if (lastchar == 32 || lastchar == 9) {
	    pval = loc;
	    break;
	    }

	else
	    headnext = loc + 1;
	}

    /* Find start of value string for this keyword */
    if (pval != NULL) {
	pval = pval + lkey;
	while (*pval == ' ' || *pval == '=')
	    pval++;
	}

    /* Return pointer to calling program */
    return (pval);

}

/* Mar 12 1998	New subroutines
 * Apr 15 1998	Set IGET() and ISEARCH() static when defined
 * Apr 24 1998	Add MGETI4(), MGETR8(), and MGETS() for single step IRAF ext.
 * Jun  1 1998	Add VMS patch from Harry Payne at STScI
 * Jul  9 1998	Fix bracket token extraction after Paul Sydney
 */
