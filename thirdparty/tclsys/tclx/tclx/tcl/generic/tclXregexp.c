/*
 * tclXregexp.c --
 *
 * Tcl regular expression pattern matching utilities.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1997 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * Boyer-Moore code from: 
 *     torek-boyer-moore/27-Aug-90 by
 *     chris@mimsy.umd.edu (Chris Torek)
 *-----------------------------------------------------------------------------
 * $Id: tclXregexp.c,v 8.9.2.2 1998/07/29 07:59:10 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"

/*
 * Structure used to return pre-parse infomation about a regular expression.
 */
typedef struct {
    int   meta;
    int   numSubExprs;
    char *largestNonMeta;
    int   largestNonMetaLen;
} preParseInfo_t;


/*
 * Meta-characters for regular expression
 */
#define TCLX_REXP_META                   "^$.[()|?+*\\"
#define TCLX_REXP_META_NO_BRACKET_NO_OR  "^$.()?+*\\"

#ifndef UCHAR_MAX
#    define UCHAR_MAX 255
#endif

/*
 * Prototypes of internal functions.
 */

static char *
BoyerMooreCompile _ANSI_ARGS_((char *pat,
                                  int patlen));

static char *
BoyerMooreExecute _ANSI_ARGS_((char     *text,
                               unsigned  textlen,
                               char     *compPtr,
                               unsigned *patLenP));

static int
PreParseRegExp _ANSI_ARGS_((char           *expression,
                            preParseInfo_t *infoPtr));


/*
 * Boyer-Moore search: input is `text' (a string) and its length,
 * and a `pattern' (another string) and its length.
 *
 * The linear setup cost of this function is approximately 256 + patlen.
 * Afterwards, however, the average cost is O(textlen/patlen), and the
 * worst case is O(textlen+patlen).
 *
 * The Boyer-Moore algorithm works by observing that, for each position
 * in the text, if the character there does *not* occur somewhere in the
 * search pattern, no comparisons including that character will match.
 * That is, given the text "hello world..." and the pattern "goodbye", the
 * `w' in `world' means that none of `hello w', `ello wo', `llo wor',
 * `lo worl', `o world', ` world.', and `world..' can match.  In fact,
 * exactly patlen strings are certain not to match.  We can discover this
 * simply by looking at the patlen'th character.  Furthermore, even if
 * the text character does occur, it may be that it rules out some number
 * of other matches.  Again, we can discover this by doing the match
 * `backwards'.
 *
 * We set up a table of deltas for each possible character, with
 * delta[character] being patlen for characters not in the pattern,
 * less for characters in the pattern, growing progressively smaller
 * as we near the end of the pattern.  Matching then works as follows:
 *
 *       0         1         2         3
 *       01234567890123456789012345678901234567
 *      "Here is the string being searched into"        (text)
 *       ------                                         (pos = [0..5])
 *      "string"                                        (pat)
 *      654321-                                         (deltas)
 *
 * (the delta for `-' will be derived below).
 *
 * Positions 0..5 end with `i', which is not the `g' we want.  `i' does
 * appear in `string', but two characters before the end.  We skip
 * forward so as to make the `i's match up:
 *
 *      "Here is the string being searched into"        (text)
 *        "string"                                      (pos = [2..7])
 *
 * Next we find that ` ' and `g' do not match.  Since ` ' does not appear
 * in the pattern at all, we can skip forward 6:
 *
 *      "Here is the string being searched into"        (text)
 *              "string"                                (pos = [8..13])
 *
 * Comparing `t' vs `g', we again find no match, and so we obtain the
 * delta for `t', which is 4.  We skip to position 17:
 *
 *      "Here is the string being searched into"        (text)
 *                  "string"                            (pos = [12..17])
 *
 * It thus takes only four steps to move the search point forward to the
 * match, in this case.
 *
 * If the pattern has a recurring character, we must set the delta for
 * that character to the distance of the one closest to the end:
 *
 *      "befuddle the cat"      (text)
 *      "fuddle"                (pos = [0..5])
 *      654321-                 (delta)
 *
 * We want the next search to line the `d's up like this:
 *
 *      "befuddle the cat"      (text)
 *        "fuddle"              (pos = [2..7])
 *
 * and not like this:
 *
 *      "befuddle the cat"      (text)
 *         "fuddle"             (pos = [3..8])
 *
 * so we take the smaller delta for d, i.e., 2.
 *
 * The last task is computing the delta we have noted above as `-':
 *
 *      "candlesticks"          (text)
 *      "hand"                  (pos = [0..3])
 *      4321-                   (delta)
 *
 * Here the `d' in `hand' matches the `d' in `candlesticks', but the
 * strings differ.  Since there are no other `d's in `hand', we know
 * that none of (cand,andl,ndle,dles) can match, and thus we want this
 * delta to be 4 (the length of the pattern).  But if we had, e.g.:
 *
 *      "candlesticks"          (text)
 *      "deed"                  (pos = [0..3])
 *      4321-                   (delta)
 *
 * then we should advance to line up the other `d':
 *
 *      "candlesticks"          (text)
 *         "deed"               (pos = [3..6])
 *
 * As this suggests, the delta should be that for the `d' nearest the
 * end, but not including the end.  This is easily managed by setting up
 * a delta table as follows:
 *
 *      for int:c in [0..255] { delta[c] = patlen; };
 *      for int:x in [0..patlen-1) { delta[pat[x]] = patlen - (x + 1); };
 *
 * delta[pat[patlen-1]] is never written, so the last letter inherits the
 * delta from an earlier iteration or from the previous loop.
 *
 * NB: the nonsense with `deltaspace' below exists merely because gcc
 * does a horrible job of common subexpression elimination (it does not
 * notice that the array is at a constant stack address).
 */

struct compiled_search_struct {
        unsigned patlen;
        unsigned deltaspace[UCHAR_MAX + 1];
};


static char *
BoyerMooreCompile (pat, patlen)
    char *pat;
    int   patlen;
{
        register unsigned char *p;
        register unsigned i, p1, *delta;
        struct compiled_search_struct *cp;
        int alloc_len;

        /*
         * Algorithm fails if pattern is empty.
         */
        if ((p1 = patlen) == 0)
                return (NULL);

        alloc_len = sizeof(struct compiled_search_struct) + patlen + 1;
        cp = (struct compiled_search_struct *) ckalloc (alloc_len);

        strncpy((char *)cp+sizeof(struct compiled_search_struct), pat, patlen);
        *((char *)cp+alloc_len-1) = '\0';

        /* set up deltas */
        delta = cp->deltaspace;

        for (i = 0; i <= UCHAR_MAX; i++)
                delta[i] = p1;

        for (p = (unsigned char *)pat, i = p1; --i > 0;)
                delta[*p++] = i;

        cp->patlen = patlen;
        return((char*) cp);
}

static char *
BoyerMooreExecute (text, textlen, compPtr, patLenP)
        char     *text;
        unsigned  textlen;
        char     *compPtr;
        unsigned *patLenP;
{
        register unsigned char *p, *t;
        struct compiled_search_struct *csp = 
        	(struct compiled_search_struct*) compPtr;
        unsigned i, j, *delta = csp->deltaspace;
        int p1;
        char *pat;
        unsigned patlen;

        *patLenP = p1 = patlen = csp->patlen;
        /* code below fails (whenever it is unsigned) if pattern too long */
        if (p1 > (int) textlen)
                return (NULL);

        pat = (char *)csp + sizeof(struct compiled_search_struct);
        /*
         * From now on, we want patlen - 1.
         * In the loop below, p points to the end of the pattern,
         * t points to the end of the text to be tested against the
         * pattern, and i counts the amount of text remaining, not
         * including the part to be tested.
         */
        p1--;
        p = (unsigned char *)pat + p1;
        t = (unsigned char *)text + p1;
        i = textlen - patlen;
        for (;;) {
                if (*p == *t && 
                    memcmp((p - p1), (t - p1), p1) == 0)
                        return ((char *)t - p1);
                j = delta[*t];
                if (i < j)
                        break;
                i -= j;
                t += j;
        }
        return (NULL);
}


/*
 *-----------------------------------------------------------------------------
 *
 * TclX_RegExpClean --
 *     Free all resources associated with a regular expression info 
 *     structure..
 *
 *-----------------------------------------------------------------------------
 */
void
TclX_RegExpClean (regExpPtr)
    TclX_regexp *regExpPtr;
{
    if (regExpPtr->progPtr != NULL)
    	ckfree ((char *) regExpPtr->progPtr);
    if (regExpPtr->boyerMoorePtr != NULL)
    	ckfree ((char *) regExpPtr->boyerMoorePtr);
}

/*
 *-----------------------------------------------------------------------------
 *
 * PreParseRegExp --
 *
 *   Do preparsing of the regular expression to allow us to do various
 * optimizations outside of the regular expression parsing.  This includes
 * detecting if there are any regular expression meta-characters and what
 * the largest substring is for boyer-moore matching.
 *
 * Parameters:
 *   o expression (I) - Expression to parse.
 *   o infoPtr (O) - Information is returned in this structure.  The following
 *     fields are returned:
 *     o meta - TRUE if there are any meta characters, FALSE otherwise.
 *     o numSubExprs - The number of subexpressions in the expression.
 *     o largestNonMeta - Pointer to the beginning of the largest string
 *       without any meta characters.
 *     o largestNonMetaLen - Length of the largest non-meta character string.
 * Returns:
 *    TRUE if the parse succeeded, FALSE if there appears to be an error.
 * Call TclRegComp to get a real error message.
 *-----------------------------------------------------------------------------
 */
static int
PreParseRegExp (expression, infoPtr)
    char           *expression;
    preParseInfo_t *infoPtr;
{
    register char *scanPtr  = expression;
    register char *nonMetaStart = NULL;
    register int   nonMetaCnt, gotMeta, parenNest;

    infoPtr->meta = FALSE;
    infoPtr->numSubExprs = 0;
    infoPtr->largestNonMeta = NULL;
    infoPtr->largestNonMetaLen = 0;

    nonMetaCnt = 0;

    /*
     * Scan counting subexpressions and searching for the longest string
     * containing no meta-characters.  Note that backslashed characters
     * are considered meta for this purpose as they will not behave as
     * exprected in the Boyer-Moore matching code.  Strings in subexpressions
     * are not counted.
     */
    while (*scanPtr != '\0') {
        switch (*scanPtr++) {
          case '^':
          case '$':
          case '.':
          case '?':
          case '+':
          case '*':
          case '|':
            gotMeta = TRUE;
            break;
          case '[':
            gotMeta = TRUE;
            if (*scanPtr == ']')
                scanPtr++;  /* ] as first character. */
            while ((*scanPtr != '\0') && (*scanPtr != ']')) {
                scanPtr++;
            }
            if (*scanPtr == '\0') {
                return FALSE;
            }
            scanPtr++;
            break;
          case '(':
            gotMeta = TRUE;
            infoPtr->numSubExprs++;
            parenNest = 1;
            while (parenNest > 0 && *scanPtr != '\0') {
                switch (*scanPtr++) {
                  case '\\':
                    if (scanPtr == '\0') {
                        return FALSE;
                    }
                    scanPtr++;
                    break;
                  case '(':
                    infoPtr->numSubExprs++;
                    parenNest++;
                    break;
                  case ')':
                    parenNest--;
                    break;
                  case '[':
                    /* Handle [] inside of () */
                    if (*scanPtr == ']')
                        scanPtr++;  /* ] as first character. */
                    while ((*scanPtr != '\0') && (*scanPtr != ']')) {
                        scanPtr++;
                    }
                    if (*scanPtr == '\0') {
                        return FALSE;
                    }
                    scanPtr++;
                }
            }
            if ((*scanPtr == '\0') && (parenNest > 0)) {
                return FALSE;
            }
            break;
          case '\\':
            gotMeta = TRUE;
            if (*scanPtr == '\0') {
                return FALSE;
            }
            scanPtr++;
            break;
          default:
            gotMeta = FALSE;
            if (nonMetaCnt == 0) {
                nonMetaStart = scanPtr - 1;
            }
            nonMetaCnt++;
        }
        /*
         * If we just hit a meta character, save the non-meta substring
         * if its the longest one we have hit.
         */
        if (gotMeta && (nonMetaCnt > infoPtr->largestNonMetaLen)) {
                infoPtr->largestNonMeta = nonMetaStart;
                infoPtr->largestNonMetaLen = nonMetaCnt;
        }
        if (gotMeta) {
            infoPtr->meta = TRUE;
            nonMetaCnt = 0;
        }
    }

    /*
     * Handle counting non-meta substring at the end of the string.
     */
    if (nonMetaCnt > infoPtr->largestNonMetaLen) {
        infoPtr->largestNonMeta = nonMetaStart;
        infoPtr->largestNonMetaLen = nonMetaCnt;
    }

    return TRUE;
}

/*
 *-----------------------------------------------------------------------------
 *
 * TclX_RegExpCompileObj --
 *     Compile a regular expression.
 *
 * Parameters:
 *     o regExpPtr - Used to hold info on this regular expression.  If the
 *       structure is being reused, it TclX_RegExpClean should be called first.
 *     o expression - Regular expression to compile.
 *     o flags - The following flags are recognized:
 *         o TCLX_REXP_NO_CASE - Comparison will be regardless of case.
 *         o TCLX_REXP_BOTH_ALGORITHMS - If specified, a Boyer-Moore expression
 *           is compiled for the largest substring of the expression that does
 *           not contain any meta-characters.  This is slows compiling, but
 *           speeds up large searches.
 *
 * FIX: Not binary clean.  Need regexps to be binary clean first.
 *-----------------------------------------------------------------------------
 */
int
TclX_RegExpCompileObj (interp, regExpPtr, expressionObj, flags)
    Tcl_Interp  *interp;
    TclX_regexp *regExpPtr;
    Tcl_Obj     *expressionObj;
    int          flags;
{
    char           *expression;
    char           *expBuf;
    int             preParseOk;
    int             expressionLen;
    preParseInfo_t  preParseInfo;

    expression = Tcl_GetStringFromObj (expressionObj, &expressionLen);
    if (expressionLen == 0) {
        TclX_AppendObjResult (interp, "Null regular expression",
                              (char *) NULL);
        return TCL_ERROR;
    }

    regExpPtr->progPtr = NULL;
    regExpPtr->boyerMoorePtr = NULL;
    regExpPtr->noCase = flags & TCLX_REXP_NO_CASE;

    if (flags & TCLX_REXP_NO_CASE) {
        expBuf = ckalloc (strlen (expression) + 1);
        TclX_DownShift (expBuf, expression);
    } else {
        expBuf = expression;
    }

    /*
     * Pre-parse the expression to gain information used for Boyer-Moore
     * optimization and returning subexpression results.  If an error occurs,
     * force the expression compilation anyway to get a real error message.
     */
    preParseOk = PreParseRegExp (expBuf,
                                 &preParseInfo);
    if (!preParseOk) {
        preParseInfo.meta = TRUE;
        preParseInfo.largestNonMetaLen = -1;
    }

#if 1
    /* FIXME: there are still some problems with the pre-parser and
     * finding the largest non-meta expression.  The only real way to
     * to this is look at the compiled expression.  For now, if there are
     * meta, just disable the boyer-moore is there is any meta.
     */
    if (preParseInfo.meta) {
        preParseInfo.largestNonMeta = NULL;
        preParseInfo.largestNonMetaLen = -1;
    }
#endif
    regExpPtr->numSubExprs = preParseInfo.numSubExprs;

    /*
     * Build a Boyer-Moore on the largest non-meta substring, if requested,
     * If less than three characters in the string and there are meta 
     * characters, don't use B-M, as it seems not optimal at this point.
     */
    if ((flags & TCLX_REXP_BOTH_ALGORITHMS) && 
        (preParseInfo.largestNonMetaLen >= (preParseInfo.meta ? 3 : 0))) {
            regExpPtr->boyerMoorePtr = 
                BoyerMooreCompile (preParseInfo.largestNonMeta,
                                   preParseInfo.largestNonMetaLen);
    }
    
    /*
     * Compile meta-character containing regular expression or ones with
     * errors to generate a useful error message.
     */
    if (preParseInfo.meta) {
        regExpPtr->progPtr = TclRegComp (expBuf);
        
        /*
         * If the preparsing reported an error, but the compile didn't,
         * we have a bug in the pre-parser.
         */
        if ((!preParseOk) && (regExpPtr->progPtr != NULL))
            panic ("scanmatch preparse bug");
        
        if (regExpPtr->progPtr == NULL) {
            TclX_AppendObjResult (interp, "error in regular expression: ", 
                                  TclGetRegError (), (char *) NULL);
            if (flags & TCLX_REXP_NO_CASE)
                ckfree (expBuf);
            TclX_RegExpClean (regExpPtr);
            return TCL_ERROR;
        }
    }

    if (flags & TCLX_REXP_NO_CASE)
        ckfree (expBuf);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * TclX_RegExpExecute --
 *     Execute a regular expression compiled with Boyer-Moore and/or 
 *     regexp.
 *
 * Parameters:
 *     o regExpPtr - Used to hold info on this regular expression.
 *     o matchStrIn - String to match against the regular expression.
 *     o matchStrLower - Optional lower case version of the string.  If
 *       multiple no case matches are being done, time can be saved by
 *       down shifting the string in advance.  NULL if not a no-case 
 *       match or this procedure is to do the down shifting.
 *     o subMatchInfo - A array of entries containing the indexs and
 *       lengths of each submatch.  Teminated by an entry of -1, -1.
 * Results:
 *     TRUE if a match, FALSE if it does not match.
 *
 *-----------------------------------------------------------------------------
 */
int
TclX_RegExpExecute (interp, regExpPtr, matchStrIn, matchStrLower, subMatchInfo)
    Tcl_Interp       *interp;
    TclX_regexp      *regExpPtr;
    char             *matchStrIn;
    char             *matchStrLower;
    Tcl_SubMatchInfo  subMatchInfo;
{
    char   *matchStr;
    int     result, idx;
    regexp *progPtr;

    if (regExpPtr->noCase) {
        if (matchStrLower == NULL) {
            matchStr = ckalloc (strlen (matchStrIn) + 1);
            TclX_DownShift (matchStr, matchStrIn);
        } else
            matchStr = matchStrLower;
    } else {
        matchStr = matchStrIn;
    }

    /*
     * If a Boyer-Moore pattern has been compiled, use that algorithm to test
     * against the text.  If that passes, then test with the regexp if we have
     * it.
     */
    if (regExpPtr->boyerMoorePtr != NULL) {
        char     *startPtr;
        unsigned  matchLen;

        startPtr = BoyerMooreExecute (matchStr, strlen (matchStr), 
                                      regExpPtr->boyerMoorePtr, &matchLen);
        if (startPtr == NULL) {
            result = FALSE;
            goto exitPoint;
        }
        /* 
         * If no regexp, its a match!
         */
        if (regExpPtr->progPtr == NULL) {
            result = TRUE; 
            goto exitPoint;
        }
    }
    
    /*
     * Give it a go with full regular expressions
     */
    progPtr = regExpPtr->progPtr;
    result = TclRegExec (progPtr, matchStr, matchStr);

    /*
     * Return submatches if we found it.
     */
    if (result) {
        for (idx = 1; idx < regExpPtr->numSubExprs + 1; idx++) {
            if (progPtr->startp [idx] == NULL) {
                subMatchInfo [idx - 1].start = -1;
                subMatchInfo [idx - 1].end = -1;
            } else {
                subMatchInfo [idx - 1].start =
                    progPtr->startp [idx] - matchStr;
                subMatchInfo [idx - 1].end =
                    progPtr->endp [idx] - matchStr - 1;
            }
        }
    }

    /*
     * Clean up and return status here.
     */
exitPoint:
    if ((regExpPtr->noCase) && (matchStrLower == NULL))
        ckfree (matchStr);
    return result;
}


