/*+
 *                      G E N _ E X E D I R _ U N I X
 *
 *  Routine name:
 *     GEN_EXEDIR
 *
 *  Function:
 *     Determines the directory from which the current program comes.
 *
 *  Description:
 *     This routine returns the full name of the directory containing
 *     the file that is being executed by the current process. This is
 *     provided as a way of locating other files associated with the
 *     program being run, which are often most conveniently held in the
 *     same directory. The string returned is such that a file called
 *     say, 'NAME.EXT' would have a full file name DIRECTORY(:NCH)//'NAME.EXT'
 *     So, for example, under VMS this routine returns a string of the form
 *     'device:[directory.subdir]' and under UNIX returns a string of the
 *     form '/dir/subdir/subsubdir/' (the final '/' being needed under UNIX
 *     for the simple concatenation of directory name and file name to work).
 *     If for some reason the directory cannot be determined, a blank string
 *     is returned and NCH is set to zero.
 *
 *  Language:
 *     C, to be called from FORTRAN
 *
 *  Call:
 *     CALL GEN_EXEDIR (DIRECTORY,NCH)
 *
 *  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
 *
 *     (<) DIRECTORY     (Fixed string,descr) The directory name.
 *                       Should be at least 39 characters long.
 *     (<) NCH           (Integer,ref) Number of characters used in
 *                       DIRECTORY.
 *
 *  External variables used:  None.
 *
 *  Prior requirements: None.
 *
 *  Support: Keith Shortridge, AAO
 *
 *  Version date: 11th Jan 1993.
 *-
 *  History:
 *     11th Jan 1993.   KS/AAO. Original version. Most of the work is done by
 *                      the routine findx(), originally written by Greg Limes
 *                      (limes@eng.sun.com).
 *  Note:
 *     For this to work from Fortran, the routine getarg() has to be
 *     available. This is the case for SUNOS and for ULTRIX.
 *+
 */

/*  The following is the body of the code of the routine findx() as written
 *  by Greg Lines. I wanted to include it to keep this file self-contained,
 *  but I didn't want to dive into it making minor changes and so propagate
 *  a 'slightly' different version if I could help it. I have added a
 *  'static' to the declaration of findx() itself in order to hide the name
 *  and keep it a local routine. Other than that this is the code as supplied.
 *  The original release includes a man page which documents the routine fully,
 *  but I have not included that. Note that the main() routine declared in the
 *  code is not normally compiled. GEN_EXEDIR itself follows.
 */

/*  ----------------------------------------------------------------------- */

#include <string.h>
#include <errno.h>
#include <sys/param.h>

#ifndef	X_OK
#define	X_OK	1			/* for "access" */
#endif

#ifndef	MAXPATHLEN
#define	MAXPATHLEN	1024
#endif

#ifndef	ENAMETOOLONG
#define	ENAMETOOLONG	EINVAL
#endif

int                     findx ();	/* get location of directory */
static int              resolve ();	/* get link resolution name */
#ifdef	FINDX_TESTMAIN
/*
 * An example of how to use this routine.
 */
extern char            *getenv ();	/* read value from environment */
char                   *pn = (char *) 0;/* program name */
char                   *rn = (char *) 0;/* run name */
char                    rd[MAXPATHLEN];	/* run directory */
char                    wd[MAXPATHLEN] = ".";	/* working directory */

int
main (argc, argv)
    int                     argc;
    char                  **argv;
{
    findx (*argv, wd, rd, &pn, &rn, getenv ("PATH"));
    printf ("%s: %s running in %s from %s\n", pn, rn, wd, rd);
    return 0;
}
#endif

int
findx (cmd, cwd, dir, pgm, run, path)
    char                   *cmd;
    char                   *cwd;
    char                   *dir;
    char                  **pgm;
    char                  **run;
    char                   *path;
{
    int                     rv = 0;
    char                   *f,
                           *s;
    if (!cmd || !*cmd || !cwd || !dir) {
	errno = EINVAL;			/* stupid arguments! */
	return -1;
    }
    if (!path || !*path)		/* missing or null path */
	path = ".";			/* assume sanity */

    if (*cwd != '/')
	if (!(getcwd (cwd, MAXPATHLEN)))
	    return -1;			/* cant get working directory */

    f = strrchr (cmd, '/');
    if (pgm)				/* user wants program name */
	*pgm = f ? f + 1 : cmd;

    if (dir) {				/* user wants program directory */
	rv = -1;
	if (*cmd == '/')		/* absname given */
	    rv = resolve ("", cmd + 1, dir, run);
	else if (f)			/* relname given */
	    rv = resolve (cwd, cmd, dir, run);
	else if (f = path) {		/* from searchpath */
	    rv = -1;
	    errno = ENOENT;		/* errno gets this if path empty */
	    while (*f && (rv < 0)) {
		s = f;
		while (*f && (*f != ':'))
		    ++f;
		if (*f)
		    *f++ = 0;
		if (*s == '/')
		    rv = resolve (s, cmd, dir, run);
		else {
		    char                    abuf[MAXPATHLEN];
		    sprintf (abuf, "%s/%s", cwd, s);
		    rv = resolve (abuf, cmd, dir, run);
		}
	    }
	}
    }
    return rv;
}
/*
 * resolve - check for specified file in specified directory. sets up dir,
 * following symlinks. returns zero for success, or -1 for error (with
 * errno set properly)
 */
static int
resolve (indir, cmd, dir, run)
    char                   *indir;	/* search directory */
    char                   *cmd;	/* search for name */
    char                   *dir;	/* directory buffer */
    char                  **run;	/* resultion name ptr ptr */
{
    char                   *p;
#ifdef	ELOOP
    int                     sll;
    char                    symlink[MAXPATHLEN + 1];
#endif

    errno = ENAMETOOLONG;
    if (strlen (indir) + strlen (cmd) + 2 > MAXPATHLEN)
	return -1;

    sprintf (dir, "%s/%s", indir, cmd);
    if (access (dir, X_OK) < 0)
	return -1;			/* not an executable program */

#ifdef	ELOOP
    while ((sll = readlink (dir, symlink, MAXPATHLEN)) >= 0) {
	symlink[sll] = 0;
	if (*symlink == '/')
	    strcpy (dir, symlink);
	else
	    sprintf (strrchr (dir, '/'), "/%s", symlink);
    }
    if (errno != EINVAL)
	return -1;
#endif

    p = strrchr (dir, '/');
    *p++ = 0;
    if (run)				/* user wants resolution name */
	*run = p;
    return 0;
}

/*  ----------------------------------------------------------------------- */

/*  The following is my (KS) wrap-up of findx(), producing the UNIX version of
 *  GEN_EXEDIR. This is a Fortran-callable routine which a) sets up a call to
 *  findx() to get the name of the 'exe' directory, b) manipulates the result
 *  slightly for aesthetic reasons and to make sure the result ends with a
 *  '/' character, and c) returns it in the Fortran character string
 *  DIRECTORY. The manipulation of the string returned by findx() is because
 *  the findx() algorithm tends to produce strings such as '/home/aatssb/ks/.'
 *  or '/home/aatssb/ks/work/../scrap/.' which are valid but messy if the
 *  result is ever to be output.
 */

void gen_exedir_ (Directory,Nch,LDir)
   char *Directory;                     /* String to receive directory name */
   int *Nch;                            /* Receives length of directory name */
   int LDir;                            /* Length of passed Fortran string */
{
   /*  Local variables  */

   char Arg0[MAXPATHLEN];         /* Used to hold the 0th argument */
   char *ArgPtr;                  /* Pointer used to work through Arg0 */
   int Back;                      /* Number of '/' chars to work back through */
   char Char;                     /* Character in directory name */
   char CurrentDir[MAXPATHLEN];   /* Work space used for current directory */
   int Dots;                      /* Dot count between '/' characters */
   char ExeDir[MAXPATHLEN];       /* Receives the exe directory from findx() */
   int Ichar;                     /* Index into ExeDir */
   int Ioutchar;                  /* Index into returned directory string */
   char LastChar;                 /* Last character written into Directory */
   int Zero;                      /* Used to pass 0 by address to getarg */

   /*  We need to get the name of the zeroth command line argument. Perversely,
    *  since we are too deep down to have access to the argv array passed
    *  when the program is run, even though this is a C routine, we get it
    *  by calling the Fortran library routine GETARG (getarg_ from C). This
    *  will return a string that will be either nul-terminated or blank-filled.
    *  If the latter, we need to make sure it is nul-terminated before passing
    *  it on to findx().
    */

   Zero = 0;
   getarg_ (&Zero,Arg0,sizeof(Arg0));
   ArgPtr = Arg0;
   do {
      if (*ArgPtr == ' ') *ArgPtr = '\0';
   } while (*ArgPtr++);

   /*  Set up the call to findx() to get the directory name  */

   Ioutchar = 0;
   if (findx(Arg0,CurrentDir,ExeDir,(char *)0,(char *)0,getenv("PATH")) == 0) {

      /*  The call to findx() worked, so we now have a directory string. We
       *  combine the copying of it into the Directory string and the tidying
       *  up. We copy it character by character, keeping a count of the number
       *  of dots after each '/', clearing the count on any ordinary character.
       *  If we come to a new '/' or the end of the string with a positive
       *  dot count, we skip back by a number of '/' chars equal to the
       *  number of dots we found.  This has the effect of turning a '/./'
       *  sequence into a '/' and a 'xx/../yy' sequence into 'yy'.
       */

      Char = '\0';
      Ichar = 0;
      Dots = 0;
      do {
         Back = 0;
         LastChar = Char;
         Char = ExeDir[Ichar++];
         Directory[Ioutchar] = Char;
         if (Char == '\0') {
            Back = Dots;
            Ioutchar--;
         } else if (Char == '/') {
            Back = Dots;
            Dots = 0;
         } else if (Char == '.') {
            Dots++;
         } else {
            Dots = 0;
         }
         if (Back) LastChar = '/';
         while (Back) {
            if (Directory[--Ioutchar] == '/') Back--;
            if (Ioutchar <= 0) break;
         }
         if (Ioutchar < LDir) Ioutchar++;
      } while (Char);
      if (LastChar != '/') Directory[Ioutchar++] = '/';
   }

   /*  Now, since the Directory string is a Fortran string, we need to
    *  blank pad it.  And we return the number of significant characters
    *  in Nch.
    */

   *Nch = Ioutchar;
   while (Ioutchar < LDir) Directory[Ioutchar++] = ' ';
}
