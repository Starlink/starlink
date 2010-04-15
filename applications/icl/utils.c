/******************************************************************************
 *
 *	U T I L S . C
 *
 * Character related and other utility functions for ICL
 *
 *	History
 *	Created :	S.K.Robinson	7/11/91
 *	Edited  :	S.K.Robinson    28/4/92
 *			Fix bug in iclidentityequal with nil
 *			parameters
 *                      A.J.Chipperfield 21/4/93
 *                      Fix bug in remove_parens
 *	Tidied, improve comments and indentation : B.K.McIlwrath 14/07/93
 *	Modified :	B.K.McIlwrath 3/08/93
 *			Add expand_name()
 *      Modified:       A.J.Chipperfield 24/12/93
 *                      Generalise expand_name
 *      Modified:       A.J.Chipperfield 28/11/96
 *                      Add restore_adamstring
 *                      Increase string enquoting buffer to 260 chars
 *
 ******************************************************************************
 */
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <sys/wait.h>
#include "utils.h"

char *CHARNIL = (char *) 0;
char ENDOFSTRING = '\0';

/******************************************************************************
 *
 *	P A D (char *string, int stringlen, char padchar)
 *
 * Fill the string 'string' with the character 'padchar'to a length 'stringlen'
 *
 ******************************************************************************
 */
void
pad(char *string, int stringlen, char padchar)
{
    int i;

    for (i = 0; i < stringlen; ++i)
	string[i] = padchar;
    return;
}

/******************************************************************************
 *
 *	I C L I D E N T I T Y E Q U A L (char *s, char *t, int n)
 *
 * Compare two strings ignoring the case of letters.
 *
 * At most n characters are compared. If n is <=0 then we return 1 (ie equal)
 * If either (but not both) string pointers are nil then we return 0(unequal)
 * but if both nil we return 1 (equal).
 *
 * Returns  1 if equal, 0 if not equal.
 *
 * Standard functions:
 * isupper(s) tests for s being an upper case letter.
 * tolower(s) converts an upper case letter to the corresponding upper
 *            case letter
 *
 ******************************************************************************
 */
int
iclidentityequal(char *s, char *t, int n)
{
    char ss, tt;

    if (n <= 0)
	return (1);
    else if (s == CHARNIL)
	if (t == CHARNIL)
	    return (1);
	else
	    return (0);
    else if (t == CHARNIL)
	return (0);
    else {
	for (ss = *s, tt = *t;
	     (n-- > 0) && ss != ENDOFSTRING && tt != ENDOFSTRING;
	     ss = *s++, tt = *t++)
	/* note the careful use of n--
         * The predecrement value of n is tested */
	    if (ss != tt) {
		if (isupper(ss))
		    ss = tolower(ss);
		if (isupper(tt))
		    tt = tolower(tt);
		if (ss != tt)
		    break;
	    }
    /* if n exhausted then n would be now negative due to n-- */
	if (n < 0)
	    return (1);
	else if (ss - tt == 0)
	    return (1);
	else
	    return (0);
    }
}

/******************************************************************************
 *
 *	S T R C O P Y (const char *s)
 *
 * Copy a string into new storage.
 *
 * Error conditions. If we are asked to copy a nil pointer or if malloc fails
 * to allocate space we return a nil pointer.
 *
 * Standard functions:
 * strlen(s)   returns the length of the string pointed
 *             to by s excluding the terminating null character.
 * malloc(m)   gets m bytes of storage from the system and returns
 *             a pointer to the first such byte or a null pointer
 *             if it fails.
 * strcpy(s,t) copies the string t including the terminating null
 *             to the string s, overwriting s.
 *
 ******************************************************************************
 */
char *
strcopy(const char *s)
{
    char *new;

    if (s == CHARNIL)
	return CHARNIL;
    else if ((new = (char *) malloc(((unsigned int) (strlen(s)+1))))
	    == CHARNIL)
	return CHARNIL;
    else
	return (strcpy(new, s));
}

/******************************************************************************
 *
 *	S T R C O N C A T (char *s, char *t)
 *
 * Concatenate two strings into new storage.
 *
 * If either of the two strings to be concatenated are nil then we use
 * strcopy() to create a copy of the non-nil string.  If both are nil we
 * return nil (handled in strcopy())
 *
 * Standard functions:
 * strlen(s)   returns the length of the string pointed
 *             to by s excluding the terminating null character.
 * malloc(m)   gets m bytes of storage from the system and returns
 *             a pointer to the first such byte or a null pointer
 *             if it fails.
 * strcpy(s,t) copies the string t including the terminating null to the
 *             string s, overwriting s.
 * strcat(s,t) concatenates the string t onto the end of s, overwriting the
 *             null terminator of s.  Includes copying the null of t.
 *
 ******************************************************************************
 */
char *
strconcat(char *s, char *t)
{
    char *new;

    if (s == CHARNIL || s == "")
	return (strcopy(t));
    else if (t == CHARNIL || t == "")
	return (strcopy(s));
    else {
	if ((new = (char *) malloc(((unsigned int)
		(strlen(s) + strlen(t) + 1)))) == CHARNIL)
	    return CHARNIL;
	else
	    return (strcat(strcpy(new, s), t));
    }
}

/******************************************************************************
 *
 *	S T R T R I M (char *st)
 *
 * Removes trailing blanks and tabs from the string 's'. The conversion is done
 * IN PLACE
 *
 ******************************************************************************
 */
char *
strtrim(char *st, int st_len)
{
    char *s;

    if (st != CHARNIL) {
	for(s = st + st_len - 1; *s <= ' '; s--)
	    if (s < st)
		break;
	*(++s) = '\0';
    }
    return st;
}

/******************************************************************************
 *
 *	U P P E R C A S E (char *st)
 *
 * Converts the string 'st' so that all lowercase letters are converted to
 * uppercase IN SITU. A pointer to the original string is returned so that
 * this function can be used in string expressions. If a nil string is given
 * then we return nil.
 *
 * Standard functions:
 * islower(s) tests for s being a lower case letter.
 * toupper(s) converts a lower case letter to the corresponding upper case
 *	      letter.
 *
 ******************************************************************************
 */
char *
uppercase(char *st)
{
    char *s;

    if (st != CHARNIL)
	for (s = st; *s != ENDOFSTRING; s++)
	    if (islower(*s))
		*s = toupper(*s);
    return st;
}

/******************************************************************************
 *
 *	L O W E R C A S E (char *st)
 *
 * Converts the string 'st' so that all uppercase letters are converted to
 * lowercase IN SITU. A pointer to the original string is returned so that
 * this function can be used in string expressions. If a nil string is given
 * then we return nil.
 *
 * Standard functions:
 * isupper(s) tests for s being a lower case letter.
 * tolower(s) converts a lower case letter to the corresponding upper case
 *	      letter.
 *
 ******************************************************************************
 */
char *
lowercase(char *st)
{
    char *s;

    if (st != CHARNIL)
	for (s = st; *s != ENDOFSTRING; s++)
	    if (isupper(*s))
		*s = tolower(*s);
    return st;
}

/******************************************************************************
 *
 *	E X P A N D _ N A M E (char **absname, char *name)
 *
 * This routine expands a filename to include the translation of environment
 * variables etc. using the c-shell.
 *
 * Returns either:
 *	TRUE and the expanded name string on success
 *	OR
 *	FALSE and any error text produced by the shell on error
 *
 ******************************************************************************
 */
int
expand_name(char **fullfilename, char *filename)
{
    int pid, fd[2], nchars, status;
    static char buffer[200];
    struct sigaction oact;

    *fullfilename = buffer;
    if (*filename == '~' || strchr(filename, '$') != NULL) {
	/* Use the C shell to expand the filename */
	if ( (pipe(fd)) < 0) {
	    strcpy(buffer, "expand_name() pipe error");
	    return 0;
	} else {
	    sigaction(SIGCHLD, NULL, &oact);
            signal(SIGCHLD, SIG_DFL);
	    if ( (pid = fork()) < 0) {
		strcpy(buffer, "expand_name() fork error");
		sigaction(SIGCHLD, &oact, NULL);
		return 0;
	    } else if (pid == 0) {	/* Child */
		close(fd[0]);
    		dup2(fd[1], 1);	/* stdout now the write pipe to icl */
		dup2(fd[1], 2); /* stderr output also sent to pipe  */
		close(fd[1]);
		if (execlp("csh","csh", "-fc", strconcat("echo ", filename),
			    (char *) 0) < 0)
			perror("expand_name() csh exec error");
		exit(1);
	    } else {			/* Parent */
		close(fd[1]);
		waitpid(pid, &status, 0);
		sigaction(SIGCHLD, &oact, NULL);
		nchars = read(fd[0], buffer, 200);
		close(fd[0]);
	        if (nchars <= 0)
		    buffer[0] = '\0';
		else
		    buffer[nchars-1] = '\0';	/* Remove newline */
	    }
	}
    } else {
	strcpy(buffer, filename);
	status = 0;
    }
    if (WIFEXITED(status))
	return 1; /* TRUE */
    else
	return 0; /* FALSE */
}

/******************************************************************************
 *
 *	W H I C H _ E X E C (char *st)
 *
 * This routine returns the full path of the executable 'filename' which
 * would be loaded by one of the execp() type system services
 *
 * Returns a pointer to the expanded name string or CHARNIL if error
 *
 ******************************************************************************
 */
char *
which_exec(char *filename)
{
    char *p, *dir, *path, *tmppath;
    static char buffer[200];

    if ( (path = getenv("PATH")) == CHARNIL)
	return CHARNIL;
    if ((tmppath = strcopy(path)) == CHARNIL)
	return CHARNIL;
    for (p = tmppath; dir = strtok(p, ":"); p = CHARNIL) {
	if (*dir) {
	    strcpy(buffer, dir);
	    strcat(buffer, "/");
	}
	strcat(buffer, filename);
	if (access(buffer, X_OK) == 0) {
	    break;
	}
    }
    free(tmppath);
    if (dir == CHARNIL)
	return CHARNIL;
    else
	return buffer;
}

/******************************************************************************
 *
 *	R E S T O R E _ I C L S T R I N G (char *iclstring)
 *
 * Printing internally stored strings is elaborate as we need to regenerate
 * the quotes in such a way that they will parse correctly to the same
 * internal string on re-input. This means choosing one of ' or " so that
 * any contained quotes of the other kind are innocuous, or duplicating
 * internal quotes if both kinds of quoting are used in the string.
 * In the latter case we arbitrarily choose to duplicate ' and print
 * the string enclosed in '. A nil string to be printed prints nothing.
 *
 * One side effect of this is that printed string literals may not use the
 * same quotes as the user typed them in with.
 * Open strings have no such problems.
 *
 * This function returns the quoted string given, as input, 'iclstring'
 *
 * Standard functions:
 * strchr(char *s, char t) returns a pointer to the first occurence of the
 * character t in the string s or nil if not present.
 *
 ******************************************************************************
 */
char *
restore_iclstring(char *iclstring)
{
    char buff[260];

    if (iclstring == CHARNIL)
	return CHARNIL;
    if (strchr(iclstring, '\'') != CHARNIL)
	if (strchr(iclstring, '"') != CHARNIL) {
	    char *p, *buf1 = buff, ch;

	    *buf1++ = '\'';
	    for (p = iclstring; (ch = *p) != ENDOFSTRING; p++)
		if (ch == '\'') {
		    *buf1++ = '\'';
		    *buf1++ = '\'';
		} else
		    *buf1++ = ch;
	    *buf1++ = '\'';
            *buf1++ = ENDOFSTRING;
	} else
	    sprintf(buff, "\"%s\"", iclstring);
    else
	sprintf(buff, "'%s'", iclstring);
    return strcopy(buff);	/* copy buff into malloc()ed storage */
}

/******************************************************************************
 *
 *	R E S T O R E _ A D A M S T R I N G (char *iclstring)
 *
 * Restore internally stored strings to a form suitable for passing to ADAM
 * tasks. Strings are enclosed in ' ' and any internal ' or " are doubled up
 *
 * One side effect of this is that printed string literals may not use the
 * same quotes as the user typed them in with.
 * Open strings have no such problems.
 *
 * This function returns the quoted string given, as input, 'iclstring'
 *
 * Standard functions:
 * strchr(char *s, char t) returns a pointer to the first occurence of the
 * character t in the string s or nil if not present.
 *
 ******************************************************************************
 */
char *
restore_adamstring(char *iclstring)
{
    char buff[260];

    if (iclstring == CHARNIL)
	return CHARNIL;
    if ( (strchr(iclstring, '\'') != CHARNIL) ||
         (strchr(iclstring, '"') != CHARNIL) ) {

       char *p, *buf1 = buff, ch;

       *buf1++ = '\'';
       for (p = iclstring; (ch = *p) != ENDOFSTRING; p++) {
          if ( (ch == '\'') || ( ch == '"' ) ) *buf1++ = ch;
          *buf1++ = ch;
       }
       *buf1++ = '\'';
       *buf1++ = ENDOFSTRING;
    } else
       sprintf(buff, "'%s'", iclstring);

    return strcopy(buff);	/* copy buff into malloc()ed storage */
}

/******************************************************************************
 *
 *	R E M O V E _ P A R E N S (char *name)
 *
 * Remove parentheses from within a string of the form COM(MAND)
 * returning a pointer to the newly created edited string.
 *
 * Commands can be specified using COM(MAND) in DEFPROC and DEFSTRING
 * and this routine aids processing by removing the parantheses.
 *
 * Given a nil string, strcopy returns nil which this routine also returns.
 *
 ******************************************************************************
 */
char *
remove_parens(char *name)
{
    char *p, *p1, *p2, ch;

    if ((p1 = strcopy(name)) == CHARNIL)
	return p1;
    p2 = p1;
    for (p = name; (ch = *p) != ENDOFSTRING; ++p)
	if (ch != '(' && ch != ')')
	    *(p1++) = ch;
    *p1 = '\0';
    return p2;
}

/******************************************************************************
 *
 *	M A K E _ W I D T H (char *buf, int bufwidth, int width)
 *
 * Adds zeros to a string to make it 'width' characters wide.
 *
 * 'buf' contains a right-justified string of 'bufwidth' characters wide.
 *
 * DEC(), HEX(), BIN() and OCT() use this.
 *
 ******************************************************************************
 */
char *
make_width(char *buf, int bufwidth, int width)
{
    char *res;

    if (width < 0)
	width = 0;
    if (width > bufwidth) {
	char *prefix;
	int i;

	if ((prefix = (char *) malloc((unsigned)
				      (width - bufwidth))) == CHARNIL)
	    return CHARNIL;
	else {
	    for (i = 0; i < width - bufwidth; i++)
		prefix[i] = '0';
	    res = strconcat(prefix, buf);	/* may fail in which case we
						 * return CHARNIL from
						 * strconcat */
	    free(prefix);
	    return res;
	}
    } else
	res = strcopy(buf + bufwidth - width);	/* may fail in which case we
						 * return CHARNIL form
						 * strcopy */
    return res;
}

/******************************************************************************
 *
 *	S T R I P _ P A T H (char *str)
 *
 * Given a string possibly comprising a full pathname returns a pointer to the
 * start of the final component (or the whole string if no components) - this
 * enables this function to be used in string handling functions.
 *
 ******************************************************************************
 */
char *
strip_path(char *str)
{
    int i;

    for (i=strlen(str)+1; i>0; i--) if (str[i-1] =='/') break;
    return &str[i];
}

/******************************************************************************
 *
 *	S T R I P _ A L L _ Z E R O E S (char *buf1)
 *
 * Given a buffer buf1, this functions strips all leading '0' replacing with
 * spaces.  This is done in-situ.
 *
 ******************************************************************************
 */
char *
strip_all_zeros(char *buf1)
{
    char *buf = buf1;

    while ((*buf) == '0')
	*(buf++) = ' ';
    return buf1;
}

/******************************************************************************
 *
 *	S T R I P _ Z E R O E S (char *buf1, int m)
 *
 * Given a buffer buf1 this routine replaces up to m leading '0' with spaces.
 * This is done in-situ
 *
 ******************************************************************************
 */
char *
strip_zeros(char *buf1, int m)
{
    char *buf = buf1;

    while (m--)
	if ((*buf) == '0')
	    (*(buf++)) = ' ';
	else
	    return buf1;
    return buf1;
}

/******************************************************************************
 *
 *	N O N _ L E A D _ Z E R O _ C O U N T (char *buf)
 *
 * Given a buffer buf, this function counts the number of digits in the buffer
 * after the first non-zero digit.
 *
 ******************************************************************************
 */
int
non_lead_zero_count(char *buf)
{
    int j;

    while ((*(buf++)) == '0');
    j = 1;
    while ((*(buf++)) != '\0')
	j = j + 1;
    return j;
}

/******************************************************************************
 *
 *	B I N (int num, int width, int sig)
 *
 * Converts the integer I into binary with an n char string result with m
 * digits.
 *
 ******************************************************************************
 */
char *
bin(int num, int width, int sig)
{
    char *buf, *buf1;
    int i, j, jon, m, signmask;

    m = sizeof(int) * 8;
    if ((buf = (char *) malloc((unsigned) (m + 1))) == CHARNIL)
	return CHARNIL;

/* Form a mask with sign bit set, the rest zero */

    signmask = 1;
    for (i = 1; i != m; ++i)
	signmask = signmask << 1;
/*
 * Convert num to a string of '1' and '0' counting number of digits after
 * leading non-zero bit in j
 */
    jon = 0;
    j = 0;
    for (i = 0; i != m; i++) {
	buf[i] = num & signmask ? '1' : '0';
	if (!jon) {
	    if (buf[i] == '1') {
		jon = 1;
		j = 1;
	    }
	} else
	    j = j + 1;
	num <<= 1;
    }
    buf[m] = '\0';
/*
 * We have a string m bytes long, with m-j leading zeros First of all make it
 * width chars long (adding leading zeros if need-be) otherwise left
 * truncating
 */
    if ((buf1 = make_width(buf, m, width)) == CHARNIL) {
	(void) free(buf);
	return CHARNIL;
    }
    (void) free(buf);
/*
 * if the user has asked to see sig digits and we have j and sig > j then
 * replace (width-sig) leading zeros with spaces otherwise we can strip all
 * leading zeros causing the user to see all digits other than leading zeros
 */
    if (sig > j)
	return (strip_zeros(buf1, width - sig));
    else
	return (strip_all_zeros(buf1));
}
