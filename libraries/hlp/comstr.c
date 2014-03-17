#include <string.h>
#include "hlpsys.h"
int hlpComstr ( char *fulstr, char *str )
/*
**  - - - - - - - - - -
**   h l p C o m s t r
**  - - - - - - - - - -
**
**  This routine returns TRUE (1) if the string str is a valid
**  abbreviation of the string fulstr, or FALSE (0) if it isn't.
**
**  The rules defining what is a valid abbreviation are as follows:
**
**  1)  The strings can be made up of one or more "words" separated
**  by the character WDSEP, defined below.  Each word in str can be
**  abbreviated individually.
**
**  2)  The characters WILDA and WILDN (defined below) in the string
**  str match, respectively, any single non-space character in fulstr
**  or any sequence of such characters.
**
**  3)  The character ESCAPE (defined below) in str is itself ignored
**  but causes the next character to be accepted literally.
**
**  4)  If a word from str contains no "match n" wildcards it can be
**  truncated, so that for example "SAM" would be a valid match for
**  "SAMPLE".  However, if there are are such wildcards truncation is
**  not acceptable; "S*E" would match "SAMPLE" but not "S*PL".  This
**  feature allows abbreviations to be used which unambiguously specify
**  the end of the string as well as the beginning.
**
**  5)  Neither fulstr nor str may contain leading or embedded spaces.
**  However, if both are blank, a true result is returned.
**
**  6)  The test is case-sensitive.  If a case-insensitive test is
**  required, folding to lowercase or uppercase must be done outside
**  this routine.
**
**  Given:
**     *fulstr      char      the full string
**     *str         char      the abbreviated string
**
**  Returned (function value):
**                  int       1 for a match, otherwise 0
**
**  Called:  hlpLength
**
**  Last revision:   7 January 1996
**                   3 January 2006 (TIMJ)
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/

#define TRUE 1
#define FALSE 0

/* Metacharacters */
#define WDSEP  "_"           /* word separator (Note 1) */
#define WILDA  '%'           /* match 1 (Note 2) */
#define WILDN  '*'           /* match n (Note 2) */
#define ESCAPE '\\'          /* escape (Note 3) */

{
/* "Still matched" flag:  we give up as soon as this goes false. */
   int match;

/* Lengths of fulstr and str, excluding trailing spaces */
   int lenf, lens;

/* Indices to fulstr and str */
   int jf, is;

/* Indices to the ends of the current words within fulstr and str */
   int lwbf, lwbs;

/* "Match n" wildcard flags: within this word, and last character */
   int matchn, wild;

/* Current and previous characters from str */
   char cs, cl;

/*
** The remaining declarations relate to the "moving substring" logic:
** str characters lying between two "match n" wildcards (or,
** superfluously) between a "match n" wildcard and the end of the word,
** constitute a "moving substring" which can be matched anywhere in the
** region of fulstr yet to be used.
*/

/* str indices defining the beginning and end of the movable substring, */
/* and their saved values; movbeg=-1 means "no movable substring".      */
   int movbeg = 0;
   int movend = 0;
   int mb = 0;
   int me = 0;

/* Movable substring not yet matched - keep searching fulstr */
   int notfnd;

/* Movable substring partially matched at current fulstr trial position */
   int sofar;

/* Local fulstr and str indices used during movable substring search */
   int mif, mis;

/* Current and previous str characters */
   char mcl, mcs;


/* Assume match to start with */
   match = TRUE;

/* Reset the "previous character was a match n wildcard" flag */
   wild = FALSE;

/* Obtain the effective lengths of the two strings. */
   lenf = hlpLength ( fulstr );
   lens = hlpLength ( str );

/* Proceed only if at least one string has non-blanks. */
   if ( lenf + lens ) {

   /* Initially set the character indices and result. */
      jf = 0;
      is = 0;

   /* Now compare each word in turn. */
      while ( match && is < lens ) {

      /* Locate the next word boundary in both strings. */
         lwbf = hlpIndex ( fulstr + jf, WDSEP ) + jf - 1;
         lwbs = hlpIndex ( str + is, WDSEP ) + is - 1;

      /* Handle the futile case of a word separator preceded by an
                                                              "escape". */
         if ( is > 0 &&
              lwbs > 0 &&
              lwbs < lens &&
              str [ lwbs ] == ESCAPE )
                 lwbs = hlpIndex ( str + lwbs + 2, WDSEP ) + lwbs;

      /* Test if there is a word boundary in str which is not */
      /* present in fulstr                                    */
         if ( lwbf < jf && lwbs >= is ) {

         /* There is: the strings do not match. */
            match = FALSE;
         } else {

         /* There is not: set the word boundaries to the end of the */
         /* strings if there are no following words.                */
            if ( lwbf < jf ) lwbf = lenf - 1;
            if ( lwbs < is ) lwbs = lens - 1;

         /* Reset the "current word contained match n wildcards" flag. */
            matchn = FALSE;

         /* Cancel the movable substring. */
            movbeg = -1;
            mb = -1;

         /* Initialize the "last character from str". */
            cl = ' ';

         /* Compare the full and abbreviated versions of the current */
         /* word until the match fails or the abbreviated string is  */
         /* used up and there is no outstanding movable substring to */
         /* search for.                                              */
            while ( match &&
                  ( is <= lwbs || movbeg >= 0 ) &&
                    jf <= lwbf ) {

            /* Examine the next character from STR if any, handling */
            /* any "escape" characters.                             */
               if ( is <= lwbs ) {
                  cs = str [ is ];
                  if ( cs == ESCAPE ) {
                     is++;
                     if ( is > lwbs ) is = lwbs;
                     cl = cs;
                     cs = str [ is ];
                  }
               } else {
                  cs = ' ';
               }
               if ( ( cs == WILDN && cl != ESCAPE ) || is > lwbs ) {

               /* We either have a "match n" wildcard or we have run out */
               /* of str characters.  If the former, set the "latest str */
               /* character was a match n wildcard" and "match n         */
               /* wildcard during current word" flags.                   */
                  wild = cs == WILDN && cl != ESCAPE;
                  if ( wild ) matchn = TRUE;

               /* If there is a movable substring under construction, */
               /* the "match n" wildcard or the exhaustion of str     */
               /* means that it is now ready to be searched for?      */
                  if ( movbeg >= 0 ) {

                  /* Yes: search fulstr for the movable substring, */
                  /* until the match succeeds or we reach the end  */
                  /* of fulstr.                                    */
                     notfnd = TRUE;
                     while ( notfnd && jf <= lwbf ) {

                     /* Set "match so far" flag. */
                        sofar = TRUE;

                     /* Initialize a local index to fulstr. */
                        mif = jf;

                     /* Point to the beginning of the movable substring. */
                        mis = movbeg;

                     /* Initialize "last character from movable substring"*/
                        mcl = ' ';

                     /* Compare until the match fails, or we reach the */
                     /* end of the movable substring.                  */
                        while ( sofar && mis <= movend ) {

                        /* Next character from movable substring. */
                           mcs = str [ mis ];

                        /* Proceed unless "escape". */
                           if ( mcs != ESCAPE || mcl != ESCAPE ) {

                           /* Compare one character. */
                              sofar = mcs == WILDA ||
                                      mcs == fulstr [ mif ];

                           /* Increment the local fulstr index. */
                              mif += 1;
                           }

                        /* Increment the movable substring index. */
                           mis += 1;

                        /* Next comparison. */
                        }

                     /* Has the match succeeded? */
                        if ( sofar ) {

                        /* A match has been found for the movable */
                        /* substring: stop searching.             */
                           notfnd = FALSE;

                        /* Remember then cancel the movable substring. */
                           mb = movbeg;
                           me = movend;
                           movbeg = -1;

                        /* Set the global fulstr index to the         */
                        /* character following the matched substring. */
                           jf = mif;
                        } else {

                        /* No match was found at the current fulstr     */
                        /* position: increment the global fulstr index. */
                           jf += 1;

                        /* Fail if we've used up all of fulstr. */
                           match = jf <= lwbf;
                        }

                     /* Continue the search if appropriate. */
                     }
                  }
               } else if ( is <= lwbs ) {

               /* We have either a "match 1" wildcard or an ordinary */
               /* literal character, and we have not run out of str  */
               /* characters.                                        */
                  if ( wild ) {

                  /* Previous character was a "match n" wildcard: reset */
                  /* the flag that told us this and start a movable     */
                  /* substring.                                         */
                     wild = FALSE;
                     movbeg = is;
                     movend = is;
                  } else if ( movbeg >= 0 ) {

                  /* A movable substring is under construction: update */
                  /* the end index.                                    */
                     movend = is;
                  } else {

                  /* No movable substring is under construction: do an */
                  /* immediate comparison.                             */
                     match = cs == WILDA ||
                             cs == fulstr [ jf ];

                  /* Increment the fulstr index. */
                     jf += 1;
                  }
               }

            /* Increment the str index. */
               is += 1;

            /* Remember the last character. */
               cl = cs;

            /* Next str character if any. */
            }

         /* If we haven't used up all of the abbreviation, make sure all */
         /* that is left is one or more "match n" wildcards.             */
            if ( is <= lwbs ) {
               while ( match && is <= lwbs ) {
                  if ( str [ is ] != WILDN ) {
                     match = FALSE;
                  } else {
                     is += 1;
                  }
               }

            } else if ( jf <= lwbf && cl != WILDN && matchn ) {

            /* We have used up all the abbreviation.  We haven't used up */
            /* all of the full string, and the final abbreviation        */
            /* character isn't a "match n" wildcard.  However, there     */
            /* were "match n" wildcards earlier.  This is acceptable     */
            /* only if the most recent movable substring, which was      */
            /* successfully matched with a sequence inside fulstr, is a  */
            /* case of mistaken identity and it exists also at the end   */
            /* of the full string.  Search backwards from the ends of    */
            /* the strings to verify that this is so.                    */

            /* Reset the last and current str characters. */
               cl = ' ';
               cs = ' ';

            /* Point to the final character of the full string. */
               mif = lwbf;

            /* Point to the final character of the movable substring. */
               mis = me;

            /* Loop until the match fails or we run out of moving */
            /* substring.                                         */
               while ( match && mis >= mb ) {

               /* Pick up the next character from movable substring. */
                  mcs = str [ mis ];

               /* Also pick up the one before that (if any). */
                  if ( mis > mb ) {
                     mcl = str [ mis - 1 ];

                  /* If it was an escape, correct the index. */
                     if ( ( mcl = ESCAPE ) ) mis -= 1;
                  } else {
                     mcl = ' ';
                  }

               /* Compare the characters from each of the two strings. */
                  if ( mcs == fulstr [ mif ] ||
                     ( mcs == WILDA && mcl != ESCAPE ) ) {

                  /* Match so far: decrement the indices. */
                     mif -= 1;
                     mis -= 1;
                  } else {

                  /* The match fails. */
                     match = FALSE;
                  }

               /* Next character. */
               }
            }

         /* Set the character indices to the start of the next word. */
            jf = lwbf + 2;
            is = lwbs + 2;
         }

      /* Compare the next word. */
      }
   }

/* Return the result. */
   return match;
}
