/*** File libwcs/hput.c
 *** September 28, 1998
 *** By Doug Mink

 * Module:	hput.c (Put FITS Header parameter values)
 * Purpose:	Implant values for parameters into FITS header string
 * Subroutine:	hputi2 (hstring,keyword,ival) sets integer*2 ival
 * Subroutine:	hputi4 (hstring,keyword,ival) sets int ival
 * Subroutine:	hputr4 (hstring,keyword,rval) sets real*4 rval
 * Subroutine:	hputr8 (hstring,keyword,dval) sets real*8 dval
 * Subroutine:	hputnr8 (hstring,keyword,ndec,dval) sets real*8 dval
 * Subroutine:	hputra (hstring,keyword,lval) sets right ascension as string
 * Subroutine:	hputdec (hstring,keyword,lval) sets declination as string
 * Subroutine:	hputl  (hstring,keyword,lval) sets logical lval
 * Subroutine:	hputs  (hstring,keyword,cval) sets character string adding ''
 * Subroutine:	hputc  (hstring,keyword,cval) sets character string cval
 * Subroutine:	hdel   (hstring,keyword) deletes entry for keyword keyword
 * Subroutine:	hadd   (hplace,keyword) adds entry for keyword at hplace
 * Subroutine:	hchange (hstring,keyword1,keyword2) changes keyword for entry
 * Subroutine:	hputcom (hstring,keyword,comment) sets comment for parameter keyword
 * Subroutine:	ra2str (out, lstr, ra, ndec) converts RA from degrees to string
 * Subroutine:	dec2str (out, lstr, dec, ndec) converts Dec from degrees to string
 * Subroutine:	deg2str (out, lstr, deg, ndec) converts degrees to string
 * Subroutine:	num2str (out, num, field, ndec) converts number to string
 * Subroutine:  getltime () returns current local time as ISO-style string
 * Subroutine:  getutime () returns current UT as ISO-style string

 * Copyright:   1998 Smithsonian Astrophysical Observatory
 *              You may do anything you like with this file except remove
 *              this copyright.  The Smithsonian Astrophysical Observatory
 *              makes no representations about the suitability of this
 *              software for any purpose.  It is provided "as is" without
 *              express or implied warranty.
 */
#include <sys/time.h>
#include <string.h>             /* NULL, strlen, strstr, strcpy */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "fitshead.h"

static int verbose=0;	/* Set to 1 to print error messages and other info */
void hputc();


/*  HPUTI4 - Set int keyword = ival in FITS header string */

void
hputi4 (hstring,keyword,ival)

  char *hstring;	/* character string containing FITS-style header
			   information in the format
			   <keyword>= <value> {/ <comment>}
			   each entry is padded with spaces to 80 characters */

  char *keyword;		/* character string containing the name of the variable
			   to be returned.  hput searches for a line beginning
			   with this string, and if there isn't one, creates one.
		   	   The first 8 characters of keyword must be unique. */
  int ival;		/* int number */
{
    char value[30];

    /* Translate value from binary to ASCII */
    sprintf (value,"%d",ival);

    /* Put value into header string */
    hputc (hstring,keyword,value);

    /* Return to calling program */
    return;
}


/*  HPUTI2 - Set short keyword = ival in FITS header string */

void
hputi2 (hstring,keyword,ival)

  char *hstring;	/* FITS header string */
  char *keyword;		/* Keyword name */
  short ival;		/* short number */

{
    char value[30];

    /* Translate value from binary to ASCII */
    sprintf (value,"%d",ival);

    /* Put value into header string */
    hputc (hstring,keyword,value);

    /* Return to calling program */
    return;
}


/*  HPUTR4 - Set float keyword = rval in FITS header string */

void
hputr4 (hstring,keyword,rval)

char *hstring;		/* FITS header string */
char *keyword;		/* Keyword name */
float rval;		/* float number */
{
    char value[30];

    /* Translate value from binary to ASCII */
    sprintf (value,"%f",rval);

    /* Put value into header string */
    hputc (hstring,keyword,value);

    /* Return to calling program */
    return;
}


/*  HPUTR8 - Set double keyword = dval in FITS header string */

void
hputr8 (hstring,keyword,dval)

char	*hstring;	/* FITS header string */
char	*keyword;	/* Keyword name */
double	dval;		/* double number */
{
    char value[30];

    /* Translate value from binary to ASCII */
    sprintf (value,"%g",dval);

    /* Put value into header string */
    hputc (hstring,keyword,value);

    /* Return to calling program */
    return;
}


/*  HPUTNR8 - Set double keyword = dval in FITS header string */

void
hputnr8 (hstring,keyword,ndec,dval)

char	*hstring;	/* FITS header string */
char	*keyword;	/* Keyword name */
int	ndec;		/* Number of decimal places to print */
double	dval;		/* double number */
{
    char value[30];
    char format[8];
    int i;

    /* Translate value from binary to ASCII */
    if (ndec < 0) {
	sprintf (format, "%%.%dg", -ndec);
	sprintf (value, format, dval);
	for (i = 0; i < strlen (value); i++)
	    if (value[i] == 'e') value[i] = 'E';
	}
    else {
	sprintf (format, "%%.%df", ndec);
	sprintf (value, format, dval);
	}

    /* Put value into header string */
    hputc (hstring,keyword,value);

    /* Return to calling program */
    return;
}


/*  HPUTRA - Set double keyword = hh:mm:ss.sss in FITS header string */

void
hputra (hstring,keyword, ra)

char *hstring;		/* FITS header string */
char *keyword;		/* Keyword name */
double ra;		/* Right ascension in degrees */
{
    char value[30];

    /* Translate value from binary to ASCII */
    ra2str (value, 30, ra, 3);

    /* Put value into header string */
    hputs (hstring,keyword,value);

    /* Return to calling program */
    return;
}


/*  HPUTDEC - Set double keyword = dd:mm:ss.sss in FITS header string */

void
hputdec (hstring, keyword, dec)

char *hstring;		/* FITS header string */
char *keyword;		/* Keyword name */
double dec;		/* Declination in degrees */
{
    char value[30];

    /* Translate value from binary to ASCII */
    dec2str (value, 30, dec, 2);

    /* Put value into header string */
    hputs (hstring,keyword,value);

    /* Return to calling program */
   return;
}



/*  HPUTL - Set keyword = F if lval=0, else T, in FITS header string */

void
hputl (hstring, keyword,lval)

char *hstring;		/* FITS header */
char *keyword;		/* Keyword name */
int lval;		/* logical variable (0=false, else true) */
{
    char value[8];

    /* Translate value from binary to ASCII */
    if (lval)
	strcpy (value, "T");
    else
	strcpy (value, "F");

    /* Put value into header string */
    hputc (hstring,keyword,value);

    /* Return to calling program */
    return;
}


/*  HPUTS - Set character string keyword = 'cval' in FITS header string */

void
hputs (hstring,keyword,cval)

char *hstring;	/* FITS header */
char *keyword;	/* Keyword name */
char *cval;	/* character string containing the value for variable
		   keyword.  trailing and leading blanks are removed.  */
{
    char squot = 39;
    char value[70];
    int lcval;

    /*  find length of variable string */

    lcval = strlen (cval);
    if (lcval > 67)
	lcval = 67;

    /* Put quotes around string */
    value[0] = squot;
    strncpy (&value[1],cval,lcval);
    value[lcval+1] = squot;
    value[lcval+2] = 0;

    /* Put value into header string */
    hputc (hstring,keyword,value);

    /* Return to calling program */
    return;
}


/*  HPUTC - Set character string keyword = value in FITS header string */

void
hputc (hstring,keyword,value)

char *hstring;
char *keyword;
char *value;	/* character string containing the value for variable
		   keyword.  trailing and leading blanks are removed.  */
{
    char squot = 39;
    char line[100];
    char newcom[50];
    char blank[80];
    char *v, *vp, *v1, *v2, *q1, *q2, *c1, *ve;
    int lkeyword, lcom, lval, lc, i;
    char *blsearch();

    for (i = 0; i < 80; i++)
	blank[i] = ' ';

    /*  find length of keyword and value */
    lkeyword = strlen (keyword);
    lval = strlen (value);

    /*  If COMMENT or HISTORY, always add it just before the END */
    if (lkeyword == 7 && (strncmp (keyword,"COMMENT",7) == 0 ||
	strncmp (keyword,"HISTORY",7) == 0)) {

	/* Find end of header */
	v1 = ksearch (hstring,"END");
	v2 = v1 + 80;

	/* Move END down one line */
	strncpy (v2, v1, 80);

	/* Insert keyword */
	strncpy (v1,keyword,7);

	/* Pad with spaces */
	for (vp = v1+lkeyword; vp < v2; vp++)
	    *vp = ' ';

	/* Insert comment */
	strncpy (v1+9,value,lval);
	return;
	}

    /* Otherwise search for keyword */
    else
	v1 = ksearch (hstring,keyword);

    /*  If parameter is not found, find a place to put it */
    if (v1 == NULL) {
	
	/* First look for blank lines before END */
        v1 = blsearch (hstring, "END");
    
	/*  Otherwise, create a space for it at the end of the header */
	if (v1 == NULL) {
	    ve = ksearch (hstring,"END");
	    v1 = ve;
	    v2 = v1 + 80;
	    strncpy (v2, ve, 80);
	    }
	else
	    v2 = v1 + 80;
	lcom = 0;
	newcom[0] = 0;
	}

    /*  Otherwise, extract the entry for this keyword from the header */
    else {
	strncpy (line, v1, 80);
	line[80] = 0;
	v2 = v1 + 80;

	/*  check for quoted value */
	q1 = strchr (line, squot);
	if (q1 != NULL)
	    q2 = strchr (q1+1,squot);
	else
	    q2 = line;

	/*  extract comment and remove trailing spaces */

	c1 = strchr (q2,'/');
	if (c1 != NULL) {
	    lcom = 80 - (c1 - line);
	    strncpy (newcom, c1+1, lcom);
	    vp = newcom + lcom - 1;
	    while (vp-- > newcom && *vp == ' ')
		*vp = 0;
	    lcom = strlen (newcom);
	    }
	else {
	    newcom[0] = 0;
	    lcom = 0;
	    }
	}

    /* Fill new entry with spaces */
    for (vp = v1; vp < v2; vp++)
	*vp = ' ';

    /*  Copy keyword to new entry */
    strncpy (v1, keyword, lkeyword);

    /*  Add parameter value in the appropriate place */
    vp = v1 + 8;
    *vp = '=';
    vp = v1 + 9;
    *vp = ' ';
    vp = vp + 1;
    if (*value == squot) {
	strncpy (vp, value, lval);
	if (lval+12 > 31)
	    lc = lval + 12;
	else
	    lc = 30;
	}
    else {
	vp = v1 + 30 - lval;
	strncpy (vp, value, lval);
	lc = 30;
	}

    /* Add comment in the appropriate place */
	if (lcom > 0) {
	    if (lc+2+lcom > 80)
		lcom = 78 - lc;
	    vp = v1 + lc + 2;     /* Jul 16 1997: was vp = v1 + lc * 2 */
	    *vp = '/';
	    vp = vp + 1;
	    strncpy (vp, newcom, lcom);
	    for (v = vp + lcom; v < v2; v++)
		*v = ' ';
	    }

	if (verbose) {
	    if (lcom > 0)
		printf ("HPUT: %s  = %s  / %s\n",keyword, value, newcom);
	    else
		printf ("HPUT: %s  = %s\n",keyword, value);
	    }

	return;
}


/*  HPUTCOM - Set comment for keyword or on line in FITS header string */

void
hputcom (hstring,keyword,comment)

  char *hstring;
  char *keyword;
  char *comment;
{
	char squot;
	char line[100];
	int lkeyword, lcom;
	char *vp, *v1, *v2, *c0, *c1, *q1, *q2;
	char *ksearch();

	squot = 39;

/*  Find length of variable name */
	lkeyword = strlen (keyword);

/*  If COMMENT or HISTORY, always add it just before the END */
	if (lkeyword == 7 && (strncmp (keyword,"COMMENT",7) == 0 ||
	    strncmp (keyword,"HISTORY",7) == 0)) {

	/* Find end of header */
	    v1 = ksearch (hstring,"END");
	    v2 = v1 + 80;
	    strncpy (v2, v1, 80);

	/*  blank out new line and insert keyword */
	    for (vp = v1; vp < v2; vp++)
		*vp = ' ';
	    strncpy (v1, keyword, lkeyword);
	    }

/* search header string for variable name */
	else {
	    v1 = ksearch (hstring,keyword);
	    v2 = v1 + 80;

	/* if parameter is not found, return without doing anything */
	    if (v1 == NULL) {
		if (verbose)
		    printf ("HPUTCOM: %s not found\n",keyword);
		return;
		}

	/* otherwise, extract entry for this variable from the header */
	    strncpy (line, v1, 80);
	    line[80] = '\0'; /* Null-terminate linebefore strchr call */

	/* check for quoted value */
	    q1 = strchr (line,squot);
	    if (q1 != NULL)
		q2 = strchr (q1+1,squot);
	    else
		q2 = NULL;

	    if (q2 == NULL || q2-line < 31)
		c0 = v1 + 31;
	    else
		c0 = v1 + (q2-line) + 2; /* allan: 1997-09-30, was c0=q2+2 */

	    strncpy (c0, "/ ",2);
	    }

/* create new entry */
	lcom = strlen (comment);

	if (lcom > 0) {
	    c1 = c0 + 2;
	    if (c1+lcom > v2)
		lcom = v2 - c1;
	    strncpy (c1, comment, lcom);
	    }

	if (verbose) {
	    printf ("HPUTCOM: %s / %s\n",keyword,comment);
	    }
}


/*  HDEL - Set character string keyword = value in FITS header string
 *	    returns 1 if entry deleted, else 0
 */

int
hdel (hstring,keyword)

char *hstring;		/* FITS header */
char *keyword;		/* Keyword of entry to be deleted */
{
    char *v, *v1, *v2, *ve;
    char *ksearch();

    /* Search for keyword */
    v1 = ksearch (hstring,keyword);

    /*  If keyword is not found, return header unchanged */
    if (v1 == NULL) {
	return (0);
	}

    /*  Find end of header */
    ve = ksearch (hstring,"END");

    /* Shift rest of header up one line */
    for (v = v1; v < ve; v = v + 80) {
	v2 = v + 80;
	strncpy (v, v2, 80);
	}

    /* Cover former last line with spaces */
    v2 = ve + 80;
    for (v = ve; v < v2; v++)
	*v = ' ';

    return (1);
}


/*  HADD - Add character string keyword = value to FITS header string
 *	    returns 1 if entry added, else 0
 *	    Call hputx() to put value into entry
 */

int
hadd (hplace, keyword)

char *hplace;		/* FITS header position for new keyword */
char *keyword;		/* Keyword of entry to be deleted */
{
    char *v, *v1, *v2, *ve;
    char *ksearch();
    int i, lkey;

    /*  Find end of header */
    ve = ksearch (hplace,"END");

    /*  If END is not found, return header unchanged */
    if (ve == NULL) {
	return (0);
	}

    v1 = hplace;

    /* Shift rest of header down one line */
    for (v = ve; v > v1; v = v - 80) {
	v2 = v + 80;
	strncpy (v2, v, 80);
	}

    /* Cover former first line with new keyword */
    lkey = strlen (keyword);
    strncpy (hplace, keyword, lkey);
    if (lkey < 8) {
	for (i = lkey; i < 8; i++)
	    hplace[i] = ' ';
	hplace[8] = '=';
	}
    for (i = 9; i < 80; i++)
	hplace[i] = ' ';

    return (1);
}


/*  HCHANGE - Changes keyword for entry from keyword1 to keyword2 in FITS
              header string
 *	      returns 1 if entry changed, else 0
 */

int
hchange (hstring, keyword1, keyword2)

char *hstring;		/* FITS header */
char *keyword1;		/* Keyword to be changed */
char *keyword2;		/* New keyword name */
{
    char *v, *v1, *v2;
    int lv2, i;
    char *ksearch();

    /* Search for keyword */
    v1 = ksearch (hstring,keyword1);

    /*  If keyword is not found, return header unchanged */
    if (!v1)
	return (0);

    else {
	lv2 = strlen (keyword2);
	v = v1;
	v2 = keyword2;
	for (i = 0; i < 8; i++) {
	    if (i < lv2)
		v[i] = v2[i];
	    else
		v[i] = ' ';
	    }
	}

    return (1);
}


/* Write the right ascension ra in sexagesimal format into string*/

void
ra2str (string, lstr, ra, ndec)

char	*string;	/* Character string (returned) */
int	lstr;		/* Maximum number of characters in string */
double	ra;		/* Right ascension in degrees */
int	ndec;		/* Number of decimal places in seconds */

{
    double a,b;
    double seconds;
    char tstring[64];
    int hours;
    int minutes;
    int isec;
    double dsgn;

    /* Keep RA between 0 and 360 */
    if (ra < 0.0 ) {
	ra = -ra;
	dsgn = -1.0;
	}
    else
	dsgn = 1.0;
    ra = fmod(ra, 360.0);
    ra *= dsgn;
    if (ra < 0.0)
	ra = ra + 360.0;

    a = ra / 15.0;

    /* Convert to hours */
    hours = (int) a;

    /* Compute minutes */
    b =  (a - (double)hours) * 60.0;
    minutes = (int) b;

    /* Compute seconds */
    seconds = (b - (double)minutes) * 60.0;
    isec = (int)(seconds + 0.5);

    if (ndec > 5) {
	if (seconds > 59.999999) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    hours = hours + 1;
	    }
	hours = hours % 24;
	(void) sprintf (tstring,"%02d:%02d:%09.6f",hours,minutes,seconds);
	}
    else if (ndec > 4) {
	if (seconds > 59.99999) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    hours = hours + 1;
	    }
	hours = hours % 24;
	(void) sprintf (tstring,"%02d:%02d:%08.5f",hours,minutes,seconds);
	}
    else if (ndec > 3) {
	if (seconds > 59.9999) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    hours = hours + 1;
	    }
	hours = hours % 24;
	(void) sprintf (tstring,"%02d:%02d:%07.4f",hours,minutes,seconds);
	}
    else if (ndec > 2) {
	if (seconds > 59.999) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    hours = hours + 1;
	    }
	hours = hours % 24;
	(void) sprintf (tstring,"%02d:%02d:%06.3f",hours,minutes,seconds);
	}
    else if (ndec > 1) {
	if (seconds > 59.99) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    hours = hours + 1;
	    }
	hours = hours % 24;
	(void) sprintf (tstring,"%02d:%02d:%05.2f",hours,minutes,seconds);
	}
    else if (ndec > 0) {
	if (seconds > 59.9) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    hours = hours + 1;
	    }
	hours = hours % 24;
	(void) sprintf (tstring,"%02d:%02d:%04.1f",hours,minutes,seconds);
	}
    else if (ndec > -1) {
	if (isec > 59) {
	    isec = 0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    hours = hours + 1;
	    }
	hours = hours % 24;
	(void) sprintf (tstring,"%02d:%02d:%04.1f",hours,minutes,seconds);
	}

    /* Move formatted string to returned string */
    if (strlen (tstring) < lstr-1)
	strcpy (string, tstring);
    else {
	strncpy (string, tstring, lstr-1);
	string[lstr-1] = 0;
	}
    return;
}


/* Write the variable a in sexagesimal format into string */

void
dec2str (string, lstr, dec, ndec)

char	*string;	/* Character string (returned) */
int	lstr;		/* Maximum number of characters in string */
double	dec;		/* Declination in degrees */
int	ndec;		/* Number of decimal places in arcseconds */

{
    double a,b;
    double seconds;
    char sign;
    int degrees;
    int minutes;
    int isec;
    char tstring[64];

    /* Set declinations outside of +-90 to the closest limit */
    if (dec > 90.0) dec = 90.0;
    if (dec < -90.0) dec = -90.0;

    a = dec;

    /* Set sign and do all the rest with a positive */
    if (a < 0) {
	sign = '-';
	a = -a;
	}
    else
	sign = '+';

    /* Convert to degrees */
    degrees = (int) a;

    /* Compute minutes */
    b =  (a - (double)degrees) * 60.0;
    minutes = (int) b;

    /* Compute seconds */
    seconds = (b - (double)minutes) * 60.0;
    isec = (int)(seconds + 0.5);

    if (ndec > 5) {
	if (seconds > 59.999999) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    degrees = degrees + 1;
	    }
	(void) sprintf (tstring,"%c%02d:%02d:%09.6f",sign,degrees,minutes,seconds);
	}
    else if (ndec > 4) {
	if (seconds > 59.99999) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    degrees = degrees + 1;
	    }
	(void) sprintf (tstring,"%c%02d:%02d:%08.5f",sign,degrees,minutes,seconds);
	}
    else if (ndec > 3) {
	if (seconds > 59.9999) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    degrees = degrees + 1;
	    }
	(void) sprintf (tstring,"%c%02d:%02d:%07.4f",sign,degrees,minutes,seconds);
	}
    else if (ndec > 2) {
	if (seconds > 59.999) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    degrees = degrees + 1;
	    }
	(void) sprintf (tstring,"%c%02d:%02d:%06.3f",sign,degrees,minutes,seconds);
	}
    else if (ndec > 1) {
	if (seconds > 59.99) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    degrees = degrees + 1;
	    }
	(void) sprintf (tstring,"%c%02d:%02d:%05.2f",sign,degrees,minutes,seconds);
	}
    else if (ndec > 0) {
	if (seconds > 59.9) {
	    seconds = 0.0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    degrees = degrees + 1;
	    }
	(void) sprintf (tstring,"%c%02d:%02d:%04.1f",sign,degrees,minutes,seconds);
	}
    else if (ndec > -1) {
	if (isec > 59) {
	    isec = 0;
	    minutes = minutes + 1;
	    }
	if (minutes > 59) {
	    minutes = 0;
	    degrees = degrees + 1;
	    }
	(void) sprintf (tstring,"%c%02d:%02d:%04.1f",sign,degrees,minutes,seconds);
	}

    /* Move formatted string to returned string */
    if (strlen (tstring) < lstr-1)
	strcpy (string, tstring);
    else {
	strncpy (string, tstring, lstr-1);
	string[lstr-1] = 0;
	}
   return;
}


/* Write the angle a in decimal format into string */

void
deg2str (string, lstr, deg, ndec)

char	*string;	/* Character string (returned) */
int	lstr;		/* Maximum number of characters in string */
double	deg;		/* Angle in degrees */
int	ndec;		/* Number of decimal places in degree string */

{
    char degform[8];
    int field;
    char tstring[64];
    double deg1;
    double dsgn;

    /* Keep angle between -180 and 360 degrees */
    deg1 = deg;
    if (deg1 < 0.0 ) {
	deg1 = -deg1;
	dsgn = -1.0;
	}
    else
	dsgn = 1.0;
    deg1 = fmod(deg1, 360.0);
    deg1 *= dsgn;
    if (deg1 <= -180.0)
	deg1 = deg1 + 360.0;

    /* Write angle to string, adding 4 digits to number of decimal places */
    field = ndec + 4;
    if (ndec > 0) {
	sprintf (degform, "%%%d.%df", field, ndec);
	sprintf (tstring, degform, deg1);
	}
    else {
	sprintf (degform, "%%%4d", field);
	sprintf (tstring, degform, (int)deg1);
	}

    /* Move formatted string to returned string */
    if (strlen (tstring) < lstr-1)
	strcpy (string, tstring);
    else {
	strncpy (string, tstring, lstr-1);
	string[lstr-1] = 0;
	}
    return;
}


/* Write the variable a in decimal format into field-character string  */

void
num2str (string, num, field, ndec)

char	*string;	/* Character string (returned) */
double	num;		/* Number */
int	field;		/* Number of characters in output field (0=any) */
int	ndec;		/* Number of decimal places in degree string */

{
    char numform[8];

    if (field > 0) {
	if (ndec > 0) {
	    sprintf (numform, "%%%d.%df", field, ndec);
	    sprintf (string, numform, num);
	    }
	else {
	    sprintf (numform, "%%%dd", field);
	    sprintf (string, numform, (int)num);
	    }
	}
    else {
	if (ndec > 0) {
	    sprintf (numform, "%%.%df", ndec);
	    sprintf (string, numform, num);
	    }
	else {
	    sprintf (string, "%d", (int)num);
	    }
	}
    return;
}


/* Return current local time in ISO-style string */
char *
getltime ()

/*   Return current local time as string
 *
 */
{
    time_t clock;
    /* char *tstr, *ctime(); */
    int i;
    struct tm *localtime();
    struct tm *time;
    struct timeval tp;
    struct timezone tzp;
    int month, day, year, hour, minute, second;
    char *isotime;

    gettimeofday (&tp,&tzp);
    clock = tp.tv_sec;

    time = localtime (&clock);
    /* tstr = ctime (&clock);
    printf ("time is %s\n",tstr); */

    year = time->tm_year;
    if (year < 1000)
	year = year + 1900;
    month = time->tm_mon + 1;
    day = time->tm_mday;
    hour = time->tm_hour;
    minute = time->tm_min;
    second = time->tm_sec; 

    isotime = (char *) calloc (1,32);

    sprintf (isotime, "%04d-%02d-%02d %02d:%02d:%02d",
		      year, month, day, hour, minute, second);

    return (isotime);
}

/*   Return current UT as an ISO-format string */

char *
getutime ()

{
    int year, month, day, hour, minute, second;
    long tsec;
    struct timeval tp;
    struct timezone tzp;
    struct tm *ts;
    char *isotime;

    gettimeofday (&tp,&tzp);

    tsec = tp.tv_sec;
    ts = gmtime (&tsec);

    year = ts->tm_year;
    if (year < 1000)
	year = year + 1900;
    month = ts->tm_mon + 1;
    day = ts->tm_mday;
    hour = ts->tm_hour;
    minute = ts->tm_min;
    second = ts->tm_sec; 

    isotime = (char *) calloc (1,32);

    sprintf (isotime, "%04d-%02d-%02dT%02d:%02d:%02d",
		      year, month, day, hour, minute, second);

    return (isotime);
}

/* Dec 14 1995	Original subroutines

 * Feb  5 1996	Added HDEL to delete keyword entry from FITS header
 * Feb  7 1996	Add EOS to LINE in HPUTC
 * Feb 21 1996	Add RA2STR and DEC2STR string routines
 * Jul 19 1996	Add HPUTRA and HPUTDEC
 * Jul 22 1996	Add HCHANGE to change keywords
 * Aug  5 1996	Add HPUTNR8 to save specific number of decimal places
 * Oct 15 1996	Fix spelling
 * Nov  1 1996	Add DEG2STR to set specific number of decimal places
 * Nov  1 1996	Allow DEC2STR to handle upt to 6 decimal places
 *
 * Mar 20 1997	Fix format error in DEG2STR
 * Jul  7 1997	Fix 2 errors in HPUTCOM found by Allan Brighton
 * Jul 16 1997	Fix error in HPUTC found by Allan Brighton
 * Jul 17 1997	Fix error in HPUTC found by Allan Brighton
 * Sep 30 1997	Fix error in HPUTCOM found by Allan Brighton
 * Dec 15 1997	Fix minor bugs after lint
 * Dec 31 1997	Always put two hour digits in RA2STR
 *
 * Feb 25 1998	Add HADD to insert keywords at specific locations
 * Mar 27 1998	If n is negative, write g format in HPUTNR8()
 * Apr 24 1998	Add NUM2STR() for easy output formatting
 * Apr 30 1998	Use BLSEARCH() to overwrite blank lines before END
 * May 27 1998	Keep Dec between -90 and +90 in DEC2STR()
 * May 28 1998	Keep RA between 0 and 360 in RA2STR()
 * Jun  2 1998	Fix bug when filling in blank lines before END
 * Jun 24 1998	Add string length to ra2str(), dec2str(), and deg2str()
 * Jun 25 1998	Make string converstion subroutines more robust
 * Aug 31 1998	Add getltime() and getutime()
 * Sep 28 1998	Null-terminate comment in HPUTCOM (Allan Brighton)
 */
