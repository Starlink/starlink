#include <string.h>
#include "help.h"
#include "hlpsys.h"
int hlpRepsub ( int ( * ) ( int, char*, int, char* ),
                int ( * ) ( char* ), int, int,
                char*, char*, char*, char* );

int hlpHelp ( int ( * outsub ) ( char* ), int lout, char *ipline,
              char *lib, int jflags,
              int ( * insub ) ( char*, char*, int* ),
              int ( * nametr ) ( int, char*, int, char* ) )
/*
**  - - - - - - - -
**   h l p H e l p
**  - - - - - - - -
**
**  Perform a HELP enquiry and, optionally, carry out an interactive
**  HELP session.
**
**  Given:
**     *outsub     func   user-supplied output subroutine (note 2)
**     lout        int    maximum record length accepted by outsub
**     *ipline     char   string specifying required HELP text (note 3)
**     *lib        char   name of HELP library (note 4)
**     jflags      int    flags (note 5)
**     *insub      func   user-supplied interactive input routine (note 6)
**     *nametr     func   user-supplied name translation routine (note 7)
**
**  Returned (function value):
**                 int   status:  +1 = OK
**                 hlp_ILLEGAL_LEVEL = illegal current level
**               hlp_LINE_OUTPUT_BAD = outsub reported error
**                hlp_LINE_INPUT_BAD = insub reported error
**               hlp_STRING_OVERFLOW = string overflowed
**               hlp_TRANSLATE_ERROR = other errors
**
**  The error codes are defined in the header file hlpsys.h.
**
**  Notes:
**
**  1)  This routine is similar, though not identical, in its arguments
**      and function to the Digital Equipment Corporation VAX/VMS
**      LBR$OUTPUT_HELP routine.
**
**  2)  The user-supplied outsub routine is an int function which
**      accepts one argument, the string to be output, and returns a
**      status of +1 if OK.  outsub is responsible for knowing where to
**      write the information, how to handle pagination, and so on.
**
**  3)  The ipline string contains the series of HELP keywords,
**      separated by spaces.  It may contain leading as well as
**      embedded and trailing spaces.  The applications help command
**      itself (for example "help"), is not included in this string,
**      nor any command qualifiers.  A maximum of LEVMAX (defined below)
**      keywords is accepted;  any more are ignored.  Keywords longer
**      than LKWMAX (defined below) are truncated.
**
**  4)  The library specified by lib is in a special format, produced by
**      the crehlp program.  The hlp routines are not compatible with
**      VMS .HLB files, though the source form of the library is very
**      similar.  The name is subject to environment-dependent
**      translation at open time via the user-supplied nametr routine.
**
**  5)  At present, jflags consists of one flag only, though more options
**      may be added in the future.  An odd jflags value, the usual case,
**      means that once the topic specified in the ipline string has been
**      reported, an interactive session ensues, with prompted input and
**      the opportunity to explore the help tree.  If jflags is even the
**      output is unaffected, but control is passed back to the caller
**      immediately the topic specified in the ipline string has been
**      reported, without any ensuing interactive phase.
**
**  6)  The user-supplied interactive input routine insub is an int
**      function which has arguments string, prompt, *l, where string
**      receives the line input, prompt is the string to output prior to
**      reading the line, and l the number of characters input.  If the
**      call is successful, a function value of +1 is returned.  (Note
**      that insub is never called if jflags is even.)
**
**  7)  The user-supplied name translation routine nametr is an int
**      function.  It has arguments command, string1, lstring2 and
**      string2, and returns a status value.  command (given) is an
**      int which the HELP system always sets to zero;  the application
**      may use other values of command, for example to perform
**      initializations or enquiries.  string1 (given) is, for command=0,
**      the HELP library name to be translated;  this name can be the
**      value of the lib argument to the present routine (identifying the
**      root library) or the name following the '@' symbol in a HELP
**      source file (identifying another library grafted onto the current
**      library).  string2 (returned, length lstring2) is, for command=0,
**      the translation of string1 into a filename for use in fopen
**      statements.  The status returned as the function value is zero to
**      show success and various -ve values to report failure.  The status
**      values may be translated into text by means of the hlpErrmes
**      routine.
**
**  Called:  hlpHinit, hlpHopenr, hlpHtellx, hlpHreadx, hlpHchkl,
**           hlpHseekx, hlpSplit, hlpHleap, hlpLength, hlpUpcase,
**           hlpComstr, hlpLinout, hlpHreadd, hlpRepsub, hlpHclose,
**           hlpCopyn
**
**  Last revision:   11 June 2008
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define TRUE 1
#define FALSE 0

/* Wildcard and ellipsis codes. */
#define WILDN "*"
#define ELLIPS "..."

/* Maximum length of a keyword. */
#define LKWMAX 64

/* Length of HELP text output buffer. */
#define LOBUF 132

/* Length of HELP library file input buffer. */
#define LHBUF 132

/* Length of command-line input buffer. */
#define LIBUF 132

/* Maximum level number. */
#define LEVMAX 9

/* Maximum length of filenames. */
#define LFILE 100

{
/* Certain larger arrays are declared static to avoid stack      */
/* overflow on some small systems.  The static specifier can     */
/* be removed for systems where stack overflow is not a problem. */

/* HELP text output buffer. */
   char outbuf [ LOBUF ];

/* HELP library file input buffer. */
   char hlpbuf [ LHBUF ];

/* Command-line input buffer. */
   char inbuf [ LIBUF ];

/* Level numbers:  the top of the hierarchy is level zero;  deeper   */
/* levels are 1 to LEVMAX inclusive.  levcur is the current level    */
/* in the tree-walk.  level is temporary storage for a level number. */
   int levcur, level;
 
/* The tree-walk context:  HELP library names, index address, logical     */
/* level number, keyword for the latest match at each level, and a flag   */
/* which shows if a perfect match has yet been found at this level (in    */
/* which case any further matches are ignored).  Apart from the keyword   */
/* and exact-match entries, each entry is double, because the given point */
/* in the HELP tree may be where a subsidiary HELP library has been       */
/* grafted on, thereby requiring two sets of information.  The first      */
/* value refers to the host and the second to the graft;  in the most     */
/* common case, where no graft exists, both sets of information are the   */
/* same.  Note that although the possibility exists that the keyword may  */
/* differ across a graft this does not yield useful results and should    */
/* be avoided when preparing the source files.                            */
   static char hlname [ LEVMAX + 1 ] [ 2 ] [ LFILE ];
   long levpos [ LEVMAX + 1 ] [ 2 ];
   int loglev [ LEVMAX + 1 ] [ 2 ];
   static char levels [ LEVMAX + 1 ] [ LKWMAX + 1 ];
   int exact [ LEVMAX + 1 ];

/* Scratch entries for temporary storage of context. */
   static char fname [ LFILE ];
   long iadr;
   int logl;

/* Number of entries completely satisfying the latest input. */
   int nfound;
 
/* Work strings for containing level names. */
   static char name1 [ LKWMAX ], name2 [ LKWMAX ];
 
/* List of keywords supplied and how many (note space for '?' endmark) */
   static char pars [ LEVMAX + 1 ] [ LKWMAX ];
   int npars;
 
/* Status:  <0 = fatal error                */
/*           0 = proceeding normally        */
/*          +1 = no more HELP at this level */
   int jstat;
 
   int i, nc, ic, j, i1, i2, lipsis, ip, maxbuf,
       iactiv, output, wild, eos, found, subtop;
   char name [LKWMAX], cue[11];


 
/*
** Preliminaries
** -------------
*/

/* Init variables to prevent compiler warnings */
   subtop = 0;

/* Extract flags. */
   iactiv = jflags & 1;
 
/* Initialize the HELP system and open the top-level HELP library. */
   hlpHinit( lib );
   if ( ( jstat = hlpHopenr ( nametr ) ) ) return jstat;

/* Initialize the context. */

/* Reset the "exact match" flags. */
   for ( levcur = 0; levcur <= LEVMAX; exact[levcur++] = FALSE );

/* Level 0 topic has just been read: note the library name and the */
/* address.                                                        */
   for ( i = 0; i <= 1; i++ )
      hlpHtellx ( hlname[0][i],
                & levpos[0][i],
                & loglev[0][i] );

/* Read the index entry to get the top-level topic name and check that */
/* the level is zero.                                                  */
   if ( ( jstat = hlpHreadx ( nametr, 'd', LHBUF, hlpbuf, &nc ) ) )
      return jstat;
   hlpHchkl ( hlpbuf, &levcur, levels[0] );

/* Copy the command string, substituting '?' if null. */
   strncpy ( inbuf, ( ( hlpLength ( ipline ) ) ? ipline : "?" ), LIBUF );

/*
** Main loop: respond to input until we run out of levels
** ------------------------------------------------------
*/
   while ( levcur >= 0 ) {

   /* Any response? */
      if ( hlpLength ( inbuf ) ) {

      /* Yes: enable output. */
         output = TRUE;
      } else {
 
      /* No: disable output and up a level. */
         output= FALSE;
         levcur--;
      }
 
   /* Position the HELP file index at the last match at this level. */
      hlpHseekx ( hlname[levcur][1],
                  levpos[levcur][1],
                  loglev[levcur][1] );

   /* Skip the already-matched record.  Leave the file pointing to */
   /* the next entry down the branch.                              */
      if ( ( jstat = hlpHreadx ( nametr, 'd', LHBUF, hlpbuf, &nc ) ) < 0 )
         return jstat;

   /* Split the input line into separate, uppercase parameters. */
 
   /* Initialize the string and parameter pointers, and the */
   /* parameter count.                                      */
      ic = 0;
      i = 0;
      npars = 0;
 
   /* Copy the parameters one by one. */
      while ( i >= 0  && npars < LEVMAX ) {

      /* Find the start and end of the next parameter. */
         hlpSplit ( inbuf, ic, &i, &j );
 
      /* Was there a parameter? */
         if ( i >= 0 ) {
 
         /* Yes: increment the count. */
            npars++;
 
         /* Update the inbuf pointer. */
            ic = j + 1;
 
         /* Copy the keyword and fold to uppercase. */
            j = ( j < LIBUF ) ? j : LIBUF - 1 ;
            i1 = j - i + 1;
            i2 = LKWMAX - 1;
            hlpUpcase ( hlpCopyn ( pars[npars-1],
                                   inbuf + i,
                                   ( i1 < i2 ) ? i1 : i2 ) );
         }
 
      /* Next parameter. */
      }

   /* Endmark the list with a question mark. */
      strcpy ( pars[npars], "?" );
 
   /* Reset the wildcard/ellipsis flag. */
      wild = FALSE;
 
   /* Point to the first parameter. */
      npars = 0;
 
   /* Loop until we reach the end marker. */
      while ( strcmp ( pars[npars], "?" ) ) {

      /* Pick up the current parameter. */
         strncpy ( name, pars[npars], LKWMAX );
 
      /* Wildcard? */
         if ( ! strncmp ( name, WILDN, 3 ) ) {
 
         /* Yes: set the wildcard/ellipsis flag. */
            wild = TRUE;
 
         /* Next parameter. */
            npars++;
         } else {
 
         /* Not a wildcard: ellipsis? */
            i = hlpLength ( name );
            if ( i >= 3 && ! strncmp ( name + i - 3, ELLIPS, 3 ) ) {
 
            /* Yes: set the wildcard/ellipsis flag. */
               wild = TRUE;
 
            /* Is the ellipsis tagged onto the previous parameter? */
               if ( i > 3 ) {
 
               /* Yes: remove it and make it the next parameter. */
                  hlpCopyn ( pars[npars], name, i-3 );
                  npars++;
                  strcpy ( pars[npars], ELLIPS );
               }
 
            /* Make the next parameter the end marker. */
               npars++;
               strcpy ( pars[npars], "?" );
            } else {
 
            /* Neither a wildcard nor an ellipsis: next parameter. */
               npars++;
            }
         }
      }

   /* Initialize the ellipsis flag (the level at which the ellipsis */
   /* was first detected).                                          */
      lipsis = -1;
 
   /* Reset the count of topics output as a result of the latest input. */
      nfound = 0;
 
   /* Initialize the pointer to the list of nominated topics. */
      ip = 0;
 
   /* Loop until the whole subtree from the existing unique match at    */
   /* the level one above the current starting level has been searched. */
      while ( ip >= 0 ) {

      /* Pick up the name of the topic (uppercase) and check for */
      /* ellipsis.                                               */
         if ( ! strcmp ( strcpy ( name, pars[ip] ), ELLIPS ) ) {

         /* Yes: has it already been seen? */
            if ( lipsis < 0 ) {

            /* No: note the current level, and interpret the ellipsis */
            /* on this first occasion as if it were the question mark */
            /* which follows it.                                      */
               lipsis = levcur;
               ip++;
               strcpy ( name, "?" );
            } else {

            /* We are already handling the ellipsis: interpret it as a */
            /* wildcard.                                               */
               strcpy ( name, WILDN );
            }

      /* Not an ellipsis: if it isn't the question mark, switch off */
      /* any ellipsis handling.                                     */
         } else if ( strcmp ( name, "?" ) ) {
            lipsis = -1;
         }

      /* Question mark? */
         if ( strcmp ( name, "?" ) ) {

         /* No: search for the requested topic at the next level down. */

         /* Down a level. */
            ip++;
            if ( levcur >= LEVMAX ) return hlp_ILLEGAL_LEVEL;
            levcur++;

         /* Reset the "perfect match" flag. */
            exact[levcur] = FALSE;

         /* Initialize the "end of subtree" and "subtopic found" flags. */
            eos = FALSE;
            found = FALSE;
            while ( ! found && ! eos ) {

            /* Satisfy any pending switch to another HELP library. */
               if ( ( jstat = hlpHleap ( nametr, LHBUF, hlpbuf,
                                         hlname[levcur][1],
                                       & levpos[levcur][1],
                                       & loglev[levcur][1] ) ) )
                  return jstat;

            /* Save the context. */
               hlpHtellx ( fname, &iadr, &logl );

            /* Read the next HELP library index entry, leaving the file */
            /* positioned at the next entry for the current level.      */
               if ( ( jstat = hlpHreadx ( nametr, 'a', LHBUF,
                                             hlpbuf, &nc ) ) < 0 )
                  return jstat;
               eos = jstat;
 
            /* Unless we have reached end-of-subtree, get the level and */
            /* name for the latest index entry.                         */
               if ( ! eos ) {
                  hlpHchkl ( hlpbuf, &level, name1 );

               /* Examine the level just encountered. */
                  if ( level < levcur ) {
 
                  /* Higher level: stop looking. */
                     eos = TRUE;
 
                  } else if ( level == levcur ) {

                  /* Check if the name of the level matches the   */
                  /* requested name, ignoring any further matches */
                  /* once a perfect match has been detected.      */
                     hlpUpcase ( strcpy ( name2, name1 ) );
                     if ( ( ! exact[levcur] ) &&
                        hlpComstr ( name2, name ) ) {

                     /* Match: if perfect, set the flag. */
                        if ( ! strcmp ( name2, name ) )
                           exact[levcur] = TRUE;

                     /* Store the context for this level. */
                        for ( i = 0; i <= 1; i++ ) {
                           strcpy ( hlname[levcur][i], fname );
                           levpos[levcur][i] = iadr;
                           loglev[levcur][i] = logl;
                        }
                        strcpy ( levels[levcur], name1 );
 
                     /* Set flag to show we found the topic. */
                        found = TRUE;
                     }
                  }
               }
            }

         /* If the topic has not been found, up two levels, */
         /* resetting the most recent "exact match" flag.   */
            if ( ! found ) {
               exact[levcur] = FALSE;
               ip -= 2;
               levcur -= 2;
 
            /* Unless about to terminate, reposition the file. */
               if ( levcur >= 0 )
                  hlpHseekx ( hlname[levcur+1][0],
                              levpos[levcur+1][0],
                              loglev[levcur+1][0] );

            /* Skip the already-matched record, leaving the file  */
            /* positioned at the next entry at the current level. */
               if ( ( jstat = hlpHreadx ( nametr, 'a', LHBUF,
                                        hlpbuf, &nc ) ) < 0 )
                  return jstat;

            /* If we are handling an ellipsis, and if we have not yet   */
            /* finished doing so, correct the parameter pointer so that */
            /* the search continues.                                    */
               if ( lipsis >= 0 && levcur >= lipsis ) ip++;
            } else {
 
            /* We have found the topic, but the index is now positioned */
            /* at the start of the next subtree.  Reposition the index  */
            /* one entry down from the latest find, with the data       */
            /* pointer set to the keyword record for the latest find.   */
               hlpHseekx ( hlname[levcur][1],
                           levpos[levcur][1],
                           loglev[levcur][1] );
               if ( ( jstat = hlpHreadx (nametr, 'd', LHBUF,
                                       hlpbuf, &nc ) ) < 0 )
                  return jstat;

            /* Satisfy any pending switch to another HELP library. */
               if ( ( jstat = hlpHleap ( nametr, LHBUF, hlpbuf,
                                         hlname[levcur][1],
                                         & levpos[levcur][1],
                                         & loglev[levcur][1] ) ) )
                  return jstat;
            }
         } else {

         /* We have a match to report. */
 
         /* Increment the count of topics/subtopics output. */
            nfound++;
 
         /* If output is enabled, display the topic. */
            if ( output ) {

            /* Output the heading (n.b. level 0 only at the very start). */
               for ( level = 0; level <= levcur; level++ ) {
                  if ( level || ! levcur ) {
                     if ( outsub ( "\0 " ) != 1 ) return hlp_LINE_OUTPUT_BAD;
                     if ( hlpLinout ( outsub, lout, 2 *(level-1),
                                      strncpy ( outbuf,
                                                levels[level],
                                                LOBUF ) ) != 1 )
                        return hlp_LINE_OUTPUT_BAD;
                  }
               }
               if ( outsub ( "\0 " ) != 1 ) return hlp_LINE_OUTPUT_BAD;
 
            /* Display the current level's text and subtopics. */
 
            /* The next record to be read from the data section of the   */
            /* HELP library is the keyword record for the current level: */
            /* skip it.                                                  */
               if ( ( jstat = hlpHreadd ( LHBUF, hlpbuf, &nc ) ) < 0 )
                  return jstat;

            /* Reset the "there were subtopics" flag. */
               subtop = FALSE;
 
            /* Now read from the data section of the HELP library and  */
            /* output until we come across a new level (or end of text */
            /* at this level).                                         */
               eos = FALSE;
               level = -1;
               while ( ! eos && level < 0 ) {

               /* Read the current line from the HELP library. */
                  if ( ( jstat = hlpHreadd ( LHBUF, hlpbuf, &nc ) ) < 0 )
                     return jstat;

               /* If not EOF, start of new level? */
                  if ( ! jstat ) {
                     hlpHchkl ( hlpbuf, &level, name1 );
                     if ( level >= 0 ) {
 
                     /* Yes: examine the level. */
                        if ( level <= levcur ) {
 
                        /* Same or higher level: finished outputting */
                        /* text.                                     */
                           eos = TRUE;
                        } else {
 
                        /* Deeper: set the "there are subtopics" flag. */
                           subtop = TRUE;
                        }
                     } else {
 
                     /* Not a new level: output the line. */
                        if ( hlpLinout ( outsub, lout,
                                         2 * ( ( levcur >= 1 ) ? levcur : 1 ),
                                         hlpbuf ) != 1 )
                           return hlp_LINE_OUTPUT_BAD;
                     }
                  } else {
 
                  /* EOF: counts as end of text at this level. */
                     eos = TRUE;
                  }
 
               /* Read the next record. */
               }

            /* Unless wildcarding or handling an ellipsis, list any */
            /* subtopics.                                           */
               strcpy ( name, pars[( ip > 1 ) ? ip - 1 : 0] );
               if ( strcmp ( name, WILDN ) &&
                    strcmp ( name, ELLIPS ) &&
                    subtop)
                  if ( ( jstat = hlpRepsub ( nametr, outsub, lout, levcur,
                                             fname, name1, outbuf, hlpbuf )
                                                                    ) < 0 )
                     return jstat;
 
            /* If EOF, flag no more to be found. */
               found = ( jstat == 0 );

            /* Position the HELP library index at the last match at */
            /* this level.                                          */
               hlpHseekx ( hlname[levcur][0],
                           levpos[levcur][0],
                           loglev[levcur][0] );

            /* Ellipsis? */
               if ( lipsis < 0 ) {
 
               /* No: skip the already-matched record, leaving the file */
               /* pointing to the next entry for the current level.     */
                  if ( ( jstat = hlpHreadx ( nametr, 'a', LHBUF,
                                             hlpbuf, &nc ) ) < 0 )
                     return jstat;
               } else {
 
            /* Yes: skip the already-matched record, leaving the file */
            /* pointing to the next entry down the branch.            */
                  if ( ( jstat = hlpHreadx ( nametr, 'd', LHBUF,
                                             hlpbuf, &nc ) ) < 0 )
                     return jstat;
               }
            } else {
 
            /* Output is disabled.  This means that we are returning    */
            /* through successively higher levels, and so we know there */
            /* are subtopics.                                           */
               subtop = TRUE;
            }
 
         /* Output a blank line. */
            if ( outsub ( "\0" ) != 1 ) return hlp_LINE_OUTPUT_BAD;
 
         /* Point to the previous parameter. */
            ip--;
 
         /* Unless we are handling an ellipsis, up one level. */
            if ( lipsis < 0 ) levcur--;
         }
      }

   /* The current level is now one above where it was when the last */
   /* input line was supplied; down a level to where it was.        */
      levcur++;
 
   /* React to how many matches were found. */
      if ( nfound < 1 ) {
 
      /* Nothing found: issue error report. */

      /* Output the heading. */
         for ( level = 1; level <= levcur; level++ ) {
            if ( outsub ( "\0" ) != 1 ) return hlp_LINE_OUTPUT_BAD;
            if ( hlpLinout ( outsub, lout, 2 * ( level - 1 ),
                             strncpy ( outbuf,
                                       levels[level],
                                       LOBUF ) ) != 1 )
               return hlp_LINE_OUTPUT_BAD;
         }
         if ( outsub ( "\0" ) != 1 ) return hlp_LINE_OUTPUT_BAD;
 
      /* Load the output record with the level names. */
         maxbuf = LOBUF - hlpLength ( name ) - 1;
         strcpy ( outbuf, "Sorry, no documentation on" );
         ic = hlpLength ( outbuf ) + 2;
         outbuf[ic-2] = (char) ' ';

      /* First batch of level names from the HELP library. */
         i = 1;
         while ( i <= levcur && ic <= maxbuf ) {

         /* Is there room for the level name? */
            if ( ( ic + hlpLength ( levels[i] ) ) <= maxbuf ) {
 
            /* There is: load it and convert it to uppercase. */
               outbuf[ic-2] = (char) ' ';
               hlpUpcase ( hlpCopyn ( outbuf + ic - 1,
                                      levels[i],
                                      LOBUF - ic - 1 ) );
 
            /* Advance the pointer. */
               ic = hlpLength ( outbuf ) + 2;
            }
 
         /* Do the next level. */
            i++;
         }
 
      /* Second batch of level names as supplied in input line. */
         i = 1;
         while ( i <= npars && ic <= maxbuf ) {

         /* Is there room for the level name? */
            if ( ( ic + hlpLength ( pars[i-1] ) ) <= maxbuf ) {
 
            /* There is: load it and convert it to uppercase. */
               outbuf[ic-2] = (char) ' ';
               hlpUpcase ( hlpCopyn ( outbuf + ic - 1,
                                      pars[i-1],
                                      LOBUF - ic - 1 ) );
 
            /* Advance the pointer. */
               ic = hlpLength ( outbuf ) + 2;
            }
 
         /* Do the next level. */
            i++;
         }
 
      /* Output the warning. */
         if ( hlpLinout ( outsub, lout, 2 * ( ( levcur > 1 ) ? levcur : 1 ),
                          outbuf) != 1 ) return hlp_LINE_OUTPUT_BAD;

      /* Position the HELP file index at the first subtopic. */
         hlpHseekx ( hlname[levcur][1],
                     levpos[levcur][1],
                     loglev[levcur][1] );
         if ( ( jstat = hlpHreadx ( nametr, 'd', LHBUF, hlpbuf, &nc ) ) )
            return jstat;

      /* List subtopics at the prompt level. */
         if ( ( jstat = hlpRepsub ( nametr, outsub, lout, levcur, fname,
                                    name1, outbuf, hlpbuf ) ) < 0 )
            return jstat;
         if ( outsub ( "\0" ) != 1) return hlp_LINE_OUTPUT_BAD;
 
      /* Single match (except as the result of wildcard)? */
      } else if ( nfound == 1 && ! wild ) {

      /* Yes: adjust the current level to that of the match. */
         levcur += npars;
 
      /* Up a level if no subtopics found. */
         if ( ! subtop ) levcur--;
      }
 
   /* Interactive prompting in effect? */
      if ( iactiv ) {
 
      /* Yes: get next input. */
 
      /* Load the output buffer with the level names. */
         i = 1;
         ic = 0;
         while ( i <= levcur && ic < LOBUF ) {

         /* Is there room for the level name? */
            if ( ( ic + hlpLength ( levels[i] ) ) < LOBUF ) {
 
            /* There is: load it. */
               if ( ic > 0 ) outbuf[ic-1] = (char) ' ';
               hlpCopyn ( outbuf + ic, levels[i], LOBUF - ic - 1 );
            } else {
 
            /* There isn't: overflow to stop the loop. */
               i = LOBUF;
            }
 
         /* Place for next level name. */
            ic = hlpLength ( outbuf ) + 1;
 
         /* Do the next level. */
            i++;
         }
 
      /* Complete the prompt string. */
         strcpy ( cue, ( levcur < 1 ) ? "Topic? " : "Subtopic? " );
         i = (int) strlen ( cue );
         if ( ic + i >= LOBUF ) ic = 0;
         hlpCopyn ( outbuf + ic, cue, i );

      /* Eliminate any included nulls. */
         for ( i += ic - 1; i >= 0; i-- ) {
            if ( ! outbuf[i] ) outbuf[i] = (char) ' ';
         }

      /* Ask for the next topic. */
         if ( insub ( inbuf, outbuf, &nc ) != 1)
            return hlp_LINE_INPUT_BAD;
 
      /* If we are about to search from the top, terminate instead. */
         if ( levcur <= 0 && ! hlpLength ( inbuf ) ) levcur = -1;
      } else {
 
      /* Interactive prompting not in effect: trigger termination. */
         levcur = -1;
      }
 
   /* Handle the latest input line if any. */
   }
 
/* OK return. */
   return 1;
}

int hlpRepsub ( int ( * nametr ) ( int, char*, int, char* ),
                int ( * outsub ) ( char* ),
                int lout, int levcur, char *fname, char *name,
                char *outbuf, char *hlpbuf )
/*
**  - - - - - - - - - -
**   h l p R e p s u b
**  - - - - - - - - - -
**
**  Report subtopic names.
**
**  Given:
**     *nametr    func   user-supplied name translation routine
**     *outsub    func   user-supplied output routine
**     lout       int    maximum record length accepted by outsub
**     levcur     int    current level number
**
**  Workspace:
**     *fname     char   filename
**     *name      char   keyword
**     *outbuf    char   output buffer
**     *hlpbuf    char   input buffer
**
**  Returned (function value):
**                int    status:    0 = OK
**                hlp_LINE_OUTPUT_BAD = outsub error
**                               else = HELP file I/O status
**
**  Notes:
**
**  1) See the hlpHelp source for information on the nametr and
**     outsub functions.
**
**  2) Prior to calling the present routine, the HELP file index must be
**     positioned so that the index entry for the first subtopic will be
**     input next.  On leaving the routine, the HELP file index is
**     positioned so that the next record to be input is the first index
**     entry that is not part of the current subtree.
**
**  Last revision:   14 January 2008
**
**  Copyright P.T.Wallace.   All rights reserved.
*/

/* Increment between topic names on the output line. */
#define IPITCH 11

{
   int jstat, indent, maxind, nc, level, levsub, ic, logl;
   long iadr;
 

/* Output a heading for the subtopics. */
   if ( outsub ( "\0" ) != 1 ) return hlp_LINE_OUTPUT_BAD;
   indent = 2 * ( ( levcur > 1 ) ? levcur : 1 );
   maxind = ( ( lout < LOBUF ) ? lout : LOBUF ) - indent;
   if ( hlpLinout ( outsub, lout, indent,
                    strcpy(outbuf,
                           "Additional information available:") ) != 1 )
       return hlp_LINE_OUTPUT_BAD;
   if ( outsub ( "" ) != 1 ) return hlp_LINE_OUTPUT_BAD;

/* Read the first subtopic name from the index and get its name.  Set */
/* the file pointer so that the next item to be read will be the next */
/* index entry at the current level.                                  */
   if ( ( jstat = hlpHreadx ( nametr, 'a', LHBUF, hlpbuf, &nc ) ) )
      return jstat;
   hlpHchkl ( hlpbuf, & level, name );

/* Continue reading the HELP library index entries for the current level */
/* and for the current subtree until we are returned to a higher level   */
/* or reach end-of-index.                                                */
   levsub = levcur + 1;
   level = levsub;
   for ( ic = 0; ic < LOBUF - 1; outbuf[ic++] = (char) ' ' );
   outbuf[LOBUF-1] = (char) '\0';
   ic = 0;
   while ( jstat == 0 && level >= levsub ) {

   /* Is the current subtopic at the right level? */
      if ( level == levsub ) {
 
      /* Yes: load the output buffer with the current subtopic's name. */

      /* First check if there will be room for it. */
         if ( ic + hlpLength ( name ) >= maxind ) {

         /* There is not: display the current contents of the buffer. */
            if ( hlpLinout ( outsub, lout, indent, hlpTrim ( outbuf ) ) != 1 )
               return hlp_LINE_OUTPUT_BAD;

         /* Reset the buffer to spaces. */
            for ( ic = 0; ic < LOBUF - 1; outbuf[ic++] = (char) ' ' );
            outbuf[LOBUF-1] = (char) '\0';
            ic = 0;
         }
 
      /* Now load the current name into the buffer. */
         strncpy ( outbuf + ic, name, hlpLength ( name ) );

      /* Work out where the next name is to go. */
         ic = hlpLength ( outbuf ) + 2;
         if ( ic % IPITCH )
            ic = ( ic / IPITCH + 1 ) * IPITCH;
      }
 
   /* Read the next index record, leaving the file pointer set so that */
   /* the next item to be read is at the current level or is the first */
   /* item of the next subtree.                                        */
      hlpHtellx ( fname, &iadr, &logl );
      if ( ( jstat = hlpHreadx ( nametr, 'a', LHBUF, hlpbuf, &nc ) ) < 0 )
         return jstat;

   /* If not EOF get the keyword name and level. */
      if ( ! jstat ) hlpHchkl ( hlpbuf, &level, name );
 
/* Load the new level's name (if any). */
   }
 
/* Output. */
   if ( hlpLinout ( outsub, lout, indent, hlpTrim ( outbuf ) ) != 1 )
      return hlp_LINE_OUTPUT_BAD;
 
/* Reposition the file so that the next index record to be read is the */
/* first one that is nothing to do with the topic just reported.       */
   hlpHseekx ( fname, iadr, logl );
   return 0;
}
