/*
 **GRUSER -- get user name (POSIX)
 *+
 *     SUBROUTINE GRUSER(STRING, L)
 *     CHARACTER*(*) STRING
 *     INTEGER L
 *
 * Return the name of the user running the program.
 *
 * Arguments:
 *  STRING : receives user name, truncated or extended with
 *           blanks as necessary.
 *  L      : receives the number of characters in VALUE, excluding
 *           trailing blanks.
 *--
 * 08-Nov-1994
 *-----------------------------------------------------------------------
 */

#ifdef PG_PPU
#define GRUSER gruser_
#else
#define GRUSER gruser
#endif

char *getlogin();

void GRUSER(string, length, maxlen)
     char *string;
     int *length;
     int maxlen;
{
  int i;
/*
 * Get the login name of the PGPLOT user.
 */
  char *user = getlogin();
/*
 * If the user name is not available substitute an empty string.
 */
  if(!user)
    user = "";
/*
 * Copy the user name to the output string.
 */
  for(i=0; i<maxlen && user[i]; i++)
    string[i] = user[i];
/*
 * Return the un-padded length of the user name string.
 */
  *length = i;
/*
 * Pad to the end of the output string with spaces.
 */
  for( ; i<maxlen; i++)
    string[i] = ' ';
  return;
}
