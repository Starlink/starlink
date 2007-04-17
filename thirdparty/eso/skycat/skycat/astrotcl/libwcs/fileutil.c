/*** File libwcs/fileutil.c
 *** September 29, 2004
 *** By Doug Mink, dmink@cfa.harvard.edu
 *** Harvard-Smithsonian Center for Astrophysics
 *** Copyright (C) 1999-2004
 *** Smithsonian Astrophysical Observatory, Cambridge, MA, USA

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Correspondence concerning WCSTools should be addressed as follows:
           Internet email: dmink@cfa.harvard.edu
           Postal address: Doug Mink
                           Smithsonian Astrophysical Observatory
                           60 Garden St.
                           Cambridge, MA 02138 USA

 * Module:      fileutil.c (ASCII file utilities)
 * Purpose:     Find out things about ASCII files
 * Subroutine:	getfilelines (filename)
 *		Return number of lines in an ASCII file
 * Subroutine:	getfilebuff (filename)
 *		Return entire file contents in a character string
 * Subroutine:	getfilesize (filename)
 *		Return size of a binary or ASCII file
 * Subroutine:	isimlist (filename)
 *		Return 1 if file is list of FITS or IRAF image files, else 0
 * Subroutine:	isimlistd (filename, rootdir)
 *		Return 1 if file is list of FITS or IRAF image files, else 0
 * Subroutine:	isfilelist (filename, rootdir)
 *		Return 1 if file is list of readable files, else 0
 * Subroutine:	isfile (filename)
 *		Return 1 if file is a readable file, else 0
 * Subroutine:	first_token (diskfile, ncmax, token)
 *		Return first token from the next line of an ASCII file
 * Subroutine:  stc2s (spchar, string)
 *		Replace character in string with space
 * Subroutine:  sts2c (spchar, string)
 *		Replace spaces in string with character
 */

#include <stdlib.h>
#ifndef VMS
#include <unistd.h>
#endif
#include <stdio.h>
#include <fcntl.h>
#include <sys/file.h>
#include <errno.h>
#include <string.h>
#include "fitsfile.h"
#include <sys/types.h>
#include <sys/stat.h>


/* GETFILELINES -- return number of lines in one file */

int
getfilelines (filename)

char    *filename;      /* Name of file for which to find number of lines */
{

    char *buffer, *bufline;
    int nlines = 0;
    char newline = 10;

    /* Read file */
    buffer = getfilebuff (filename);

    /* Count lines in file */
    if (buffer != NULL) {
	bufline = buffer;
	nlines = 0;
	while ((bufline = strchr (bufline, newline)) != NULL) {
            bufline = bufline + 1;
            nlines++;
	    }
	free (buffer);
	return (nlines);
	}
    else {
	return (0);
	}
}


/* GETFILEBUFF -- return entire file contents in one character string */

char *
getfilebuff (filename)

char    *filename;      /* Name of file for which to find number of lines */
{

    FILE *diskfile;
    int lfile, nr, lbuff, ipt, ibuff;
    char *buffer, *newbuff, *nextbuff;

    /* Treat stdin differently */
    if (!strcmp (filename, "stdin")) {
	lbuff = 5000;
	lfile = lbuff;
	buffer = NULL;
	ipt = 0;
	for (ibuff = 0; ibuff < 10; ibuff++) {
	    if ((newbuff = realloc (buffer, lfile+1)) != NULL) {
		buffer = newbuff;
		nextbuff = buffer + ipt;
        	nr = fread (nextbuff, 1, lbuff, stdin);
		if (nr == lbuff)
		    break;
		else {
		    ipt = ipt + lbuff;
		    lfile = lfile + lbuff;
		    }
		}
	    else {
		fprintf (stderr,"GETFILEBUFF: No room for %d-byte buffer\n",
			 lfile);
		break;
		}
	    }
	return (buffer);
	}

    /* Open file */
    if ((diskfile = fopen (filename, "rb")) == NULL)
        return (NULL);

   /* Find length of file */
    if (fseek (diskfile, 0, 2) == 0)
        lfile = ftell (diskfile);
    else
        lfile = 0;
    if (lfile < 1) {
	fprintf (stderr,"GETFILEBUFF: File %s is empty\n", filename);
	fclose (diskfile);
	return (NULL);
	}

    /* Allocate buffer to hold entire file and read it */
    if ((buffer = calloc (1, lfile+1)) != NULL) {
 	fseek (diskfile, 0, 0);
        nr = fread (buffer, 1, lfile, diskfile);
	if (nr < lfile) {
	    fprintf (stderr,"GETFILEBUFF: File %s: read %d / %d bytes\n",
		     filename, nr, lfile);
	    free (buffer);
	    fclose (diskfile);
	    return (NULL);
	    }
	buffer[lfile] = (char) 0;
	fclose (diskfile);
	return (buffer);
	}
    else {
	fprintf (stderr,"GETFILEBUFF: File %s: no room for %d-byte buffer\n",
		 filename, lfile);
	fclose (diskfile);
	return (NULL);
	}
}


/* GETFILESIZE -- return size of one file in bytes */

int
getfilesize (filename)

char    *filename;      /* Name of file for which to find size */
{
    struct stat statbuff;

    if (stat (filename, &statbuff))
	return (0);
    else
	return ((int) statbuff.st_size);
}

int
getfilesize0 (filename)

char    *filename;      /* Name of file for which to find size */
{
    FILE *diskfile;
    long filesize;

    /* Open file */
    if ((diskfile = fopen (filename, "rb")) == NULL)
        return (-1);

    /* Move to end of the file */
    if (fseek (diskfile, 0, 2) == 0)

        /* Position is the size of the file */
        filesize = ftell (diskfile);

    else
        filesize = -1;

    fclose (diskfile);

    return ((int) filesize);
}


/* ISIMLIST -- Return 1 if list of FITS or IRAF files, else 0 */
int
isimlist (filename)

char    *filename;      /* Name of possible list file */
{
    FILE *diskfile;
    char token[256];
    int ncmax = 254;

    if ((diskfile = fopen (filename, "r")) == NULL)
	return (0);
    else {
	first_token (diskfile, ncmax, token);
	fclose (diskfile);
	if (isfits (token) | isiraf (token))
	    return (1);
	else
	    return (0);
	}
}


/* ISIMLISTD -- Return 1 if list of FITS or IRAF files, else 0 */
int
isimlistd (filename, rootdir)

char    *filename;	/* Name of possible list file */
char    *rootdir;	/* Name of root directory for files in list */
{
    FILE *diskfile;
    char token[256];
    char filepath[256];
    int ncmax = 254;

    if ((diskfile = fopen (filename, "r")) == NULL)
	return (0);
    else {
	first_token (diskfile, ncmax, token);
	fclose (diskfile);
	if (rootdir != NULL) {
	    strcpy (filepath, rootdir);
	    strcat (filepath, "/");
	    strcat (filepath, token);
	    }
	else
	    strcpy (filepath, token);
	if (isfits (filepath) | isiraf (filepath))
	    return (1);
	else
	    return (0);
	}
}


/* ISFILELIST -- Return 1 if list of readable files, else 0 */
int
isfilelist (filename, rootdir)

char    *filename;      /* Name of possible list file */
char    *rootdir;	/* Name of root directory for files in list */
{
    FILE *diskfile;
    char token[256];
    char filepath[256];
    int ncmax = 254;

    if ((diskfile = fopen (filename, "r")) == NULL)
	return (0);
    else {
	first_token (diskfile, ncmax, token);
	fclose (diskfile);
	if (rootdir != NULL) {
	    strcpy (filepath, rootdir);
	    strcat (filepath, "/");
	    strcat (filepath, token);
	    }
	else
	    strcpy (filepath, token);
	if (isfile (filepath))
	    return (1);
	else
	    return (0);
	}
}


/* ISFILE -- Return 1 if file is a readable file, else 0 */

int
isfile (filename)

char    *filename;      /* Name of file to check */
{
    if (!strcasecmp (filename, "stdin"))
	return (1);
    else if (access (filename, R_OK))
	return (0);
    else
	return (1);
}


static char *token1;


/* FIRST_TOKEN -- Return first token from the next line of an ASCII file */

int
first_token (diskfile, ncmax, token)

FILE	*diskfile;		/* File descriptor for ASCII file */
int	ncmax;			/* Maximum number of characters returned */
char	*token;			/* First token on next line (returned) */
{
    char *lastchar, *lspace;

    /* If line can be read, add null at the end of the first token */
    if (fgets (token, ncmax, diskfile) != NULL) {
	if (token[0] == '#') {
	    fgets (token, ncmax, diskfile);
	    }
	lastchar = token + strlen (token) - 1;

	/* Remove trailing spaces or control characters */
	while (*lastchar <= 32)
	    *lastchar-- = 0;

	if ((lspace = strchr (token, ' ')) != NULL) {
	    *lspace = (char) 0;
	    token1 = lspace + 1;
	    }
	else
	    token1 = NULL;
	return (1);
	}
    else
	return (0);
}


/* Replace character in string with space */

int
stc2s (spchar, string)

char	spchar;	/* Character to replace with spaces */
char	*string;
{
    int i, lstr, n;
    lstr = strlen (string);
    n = 0;
    for (i = 0; i < lstr; i++) {
	if (string[i] == spchar) {
	    n++;
	    string[i] = ' ';
	    }
	}
    return (n);
}


/* Replace spaces in string with character */

int
sts2c (spchar, string)

char	spchar;	/* Character wth which to replace spaces */
char	*string;
{
    int i, lstr, n;
    lstr = strlen (string);
    n = 0;
    for (i = 0; i < lstr; i++) {
	if (string[i] == ' ') {
	    n++;
	    string[i] = spchar;
	    }
	}
    return (n);
}


/*
 * Jul 14 1999	New subroutines
 * Jul 15 1999	Add getfilebuff()
 * Oct 15 1999	Fix format eror in error message
 * Oct 21 1999	Fix declarations after lint
 * Dec  9 1999	Add next_token(); set pointer to next token in first_token
 *
 * Sep 25 2001	Add isfilelist(); move isfile() from catutil.c
 *
 * Jan  4 2002	Allow getfilebuffer() to read from stdin
 * Jan  8 2002	Add sts2c() and stc2s() for space-replaced strings
 * Mar 22 2002	Clean up isfilelist()
 * Aug  1 2002	Return 1 if file is stdin in isfile()
 *
 * Feb  4 2003	Open catalog file rb instead of r (Martin Ploner, Bern)
 * Mar  5 2003	Add isimlistd() to check image lists with root directory
 * May 27 2003	Use file stat call in getfilesize() instead of opening file
 * Jul 17 2003	Add root directory argument to isfilelist()
 *
 * Sep 29 2004	Drop next_token() to avoid conflict with subroutine in catutil.c
 */
