#if USE_SCCSID
static char Sccsid[] = {"@(#) getline.c 1.1 01/23/95"};
#endif /* USE_SCCSID */

/*

$Revision$

Author: Remo Tilanus  (rpt@jach.hawaii.edu)
__________________________________________       _________________________
UK/NL/CAN Joint Astronomy Centre          \__^__/    JJ   CCC  MM MM  TTTT
660 N. Aohoku Pl., Hilo, HI 96720           [J]      JJ  CC    M M M   TT
ph.: (808) 935-4332; FAX: 961-6516          /|\     JJ    CCC  M   M   TT
__________________________________________________________________________

 25-JUL-2004 (Tim Jenness):
    Import into autoconf system


*/
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
** The parameters for GET_ALINE(_) are:
** GET_ALINE(_) is a simple interface routine into the GNU library
** READLINE which provides 'command'-line editing and history for user
** build routines under UNIX. The arrow keys and EMACS key-bindings
** are both fully supported. See the documentation of READLINE for
** customization and further information on features.
**
** GET_ALINE(_) is easy to incorporate into new and existing programmes,
** although some there are a few 'rules of behaviour' to follow, many
** of which originate from the requirement that I wanted to call the
** routine from C as well as FORTRAN. I only have tried it with the
** Solaris 2.x version of F77.
**
**
**       line       *char  (out)     Response from user to prompt
**       prompt     *char  (in)      Prompt to present user with
**       terminate  *int   (in)      0: if necessary, pad line with blanks
**                                      NO C STRING-TERMINATOR ADDED.
**                                  !0: properly terminated string
**       C only:
**       -------
**       llen       int    (in)      declaration length line
**       plen       int    (in)      declaration length prompt
**
** The routine return an integer indicating the number of characters
** in line. EOF returns a -1, a truncated line -2.
**
** SPECIAL NOTES:
**
** C programmers:   Please note that 'terminate' is a pointer and that
**                  'llen' and 'plen' are required. Almost certainly you
**                   want to set '*terminate' to 1.
**
** F77 programmers: 'Terminate' should be passed in via a variable and
**                  not by specifying a value within the call.
**                  'llen' and 'plen' are supplied by the compiler
**                  (at least under Solaris F77).
**                  For a FORTRAN-like string to be returned set terminate
**                  to 0. Since FORTRAN does not recognize a C
**                  string-terminator your code may choke on the remainder
**                  of the line. Alternatively, use the returned length
**                  (e.g. slen) to delimit the line to: line(1:slen) in
**                  all operations.
**                  Finally: omit the trailing underscore from the call,
**                  it is also being supplied by the compiler.
**
**
**
** C example:
** ----------
**
** #include <stdio.h>
**
** #define SLEN 81
**
** main()
** {
**   char line[SLEN], prompt[SLEN];
**
**   int  llen;
**
**   static int  itrm = 1;
**
**   llen = get_aline_(line, "Hello World (over): ", &itrm, SLEN, 20);
**
**   if (llen == 0 || llen == -1)
**     printf("....Nobody home I guess.\n");
**   else
**    {
**      printf("World replied:\n");
**      printf("'%s'\n",line);
**      if (llen == -2)
**        printf("....*choke* remainder truncated\n");
**    }
**
**   printf("\nTry history (up-arrow) and some editing next!\n");
**   sprintf(prompt,"Hello Moon too (over): ");
**
**   llen = get_aline_(line, prompt, &itrm, SLEN, strlen(prompt));
**
**   if (llen == 0 || llen == -1)
**     printf("....Still a dusty and lonely place.\n");
**   else
**     {
**       printf("Moon replied:\n");
**       printf("'%s'\n",line);
**       if (llen == -2)
** 	printf("....*choke* remainder truncated\n");
**     }
**   printf("\n");
**
** }
**
**
**
**
** F77 example:
** ------------
**
**         program hello
**
**         character  line*132, prompt*80
**         integer    llen, itrm
**
**         integer    get_aline
**         external   get_aline
**
**         itrm = 0
**
**         llen = get_aline(line, 'Hello World (over): ', itrm)
**
**         if (llen .eq. 0 .or. llen .eq. -1) then
**           write(6,'('' ....Nobody home I guess.'')')
**         else
**           write(6,'('' World replied:'')')
**           write(6,'('' -'',A,''-'')') line(1:llen)
**           if (llen .eq. -2)
**     &       write(6,'('' ....*choke* remainder truncated.'')')
**         endif
**
**         write(6,'(/,''Try history (up-arrow) and some editing next!\n'')')
**         write(6,'('' '')')
**         write(prompt,'(''Hello Moon too (over): '',$)')
**
**         llen = get_aline(line, prompt, itrm)
**
**         if (llen .eq. 0 .or. llen .eq. -1) then
**           write(6,'('' ....Still a dusty and lonely place.'')')
**         else
**           write(6,'('' Moon replied:'')')
**         write(6,'('' -'',A,''-'')') line(1:llen)
**           if (llen .eq. -2)
**     &        write(6,'('' ....*choke* remainder truncated.'')')
**         endif
**         write(6,'('' '')')
**
**         stop
**         end
**
** ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if HAVE_STRINGS_H
# include <strings.h>
#endif

#if HAVE_READLINE_READLINE_H
# include <readline/readline.h>
#endif
#if HAVE_READLINE_HISTORY_H
# include <readline/history.h>
#endif

/* Only say we have readline if we have all of it including the libraries */
#if HAVE_READLINE_READLINE_H && HAVE_LIBREADLINE
#define HAVE_READLINE 1
#else
#define HAVE_READLINE 0
#endif


#define PLEN 256

int get_aline_(line, prompt, terminate, llen, plen)
/*----------------------------------------------------------------------*/
/*
** Function to get a line from stdin. This routine interfaces with
** GNU readline and offers full editing and history of the 'command'
** line.
**
** NOTE: this routine returns the length of the reply and pads any
**       remaining characters of line with 'blanks'. In case *terminate
**       is not 0 a C terminator (NUL char) is written in the last
**       position.
**
** Calling from FORTRAN: if called from a FORTRAN main (with line and
** prompt declared as 'character line*##, prompt*##') llen and plen should
** NOT be included in the call. FORTRAN will supply them automatically.
**
** Return values:
**                len: length of reply from user
**                 -1: EOF detected (crl-D)
**                 -2: line truncated
**
** Arguments:
*/
char line[];         /*     (out)    Line read                          */
char prompt[];       /*     (in)     Prompt to use                      */
int  *terminate;     /*     (in)     Terminate line upon return?        */
int  llen, plen;     /*     (in)     Length of line and prompt. Answer  */
                     /*              will be truncated; last position   */
                     /*              used for \0 if *terminate = 1      */
/*----------------------------------------------------------------------*/
{
  static char *line_read;
  char lprompt[PLEN];

  char *do_gets();
  int   i, len, slen;

  line[llen-1] = '\0';         /* make sure string is terminated  */

                                          /* copy and terminate prompt  */
  for (slen = plen-1; slen > 0 && *(prompt+slen-1) == ' '; slen--);
  if (slen >= PLEN-1) slen = PLEN - 2;

  strncpy(lprompt,prompt,slen);
  if (slen < plen && *(prompt+slen) == ' ')           /* put blank back */
    {
      lprompt[slen] = ' ';
      slen++;
    }
  lprompt[slen] = '\0';

                                    /* get answer from stdin */
#if HAVE_READLINE
  if ( (line_read = do_gets(lprompt)) == NULL )
    {
      return(-1);
    }
  len = (int) strlen(line_read);
  strncpy(line, line_read, llen);
#else
  printf("%s", lprompt);
  if ( (line_read = fgets(line, llen, stdin)) == (char *)EOF ||
        line_read == NULL )
    {
      return(-1);
    }
  len = (int) strlen(line);
  if (line[len-1] == '\n')
    {
      len = len - 1;
      line[len] = '\0';
    }
#endif

  if (*terminate != 0)
    {
      line[llen] = '\0';            /* make sure it's terminated  */
      if (len < llen) line[len] = '\0';
    }
  else
    {
      for (i = len; i < llen; i++)                  /* pad with blanks  */
	line[i] = ' ';
    }

  if ( len >= llen )                      /* result is trunctated!      */
    {
      return(-2);
    }

  return(len);
}

#if HAVE_READLINE

char *do_gets(prompt)
/*----------------------------------------------------------------------*/
/*
** Function to get an 'answer' from the user. This functions supports
** full command-line editting (e.g. Emacs key-bindings) and history.
** It can be substituted where 'gets' is being used normally.
** It uses the GNU Readline library distributed with Bash.
**
** Note: the terminating carriage-return is omitted from the answer.
**
** Return values:
**                normally: pointer to the string
**                else:     NULL or EOF as for gets (man gets).
** Arguments:
*/
char *prompt;             /*   (in)   Prompt to use  (can be empty: "") */
/*----------------------------------------------------------------------*/
{
  static char *line_read;

  /* If the buffer has already been allocated, return the memory
     to the free pool. */
  if (line_read != NULL)
    {
      free (line_read);
      line_read = NULL;
    }

  /* Get a line from the user. */
  line_read = (char *)readline (prompt);

  /* If the line has any text in it, save it on the history. */
  if (line_read && *line_read)
    add_history (line_read);

  return(line_read);
}

/*
** Optionally build rindex fallback routine (can assume we have strchr!)
*/

#if ! HAVE_INDEX

char *index(char * s, int c)
{
   return( (char *)strchr(s,c) );
}

#endif

#if ! HAVE_RINDEX

char *rindex(char * s, int c)
{
   return( (char *)strrchr(s,c) );
}

#endif

#endif
