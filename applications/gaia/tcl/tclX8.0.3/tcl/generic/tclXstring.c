/* 
 * tclXstring.c --
 *
 *      Extended TCL string and character manipulation commands.
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
 * $Id: tclXstring.c,v 8.15 1997/12/14 21:33:00 markd Exp $
 *-----------------------------------------------------------------------------
 */

/*FIX: Add creplace to overwrite characters in a string. */

#include "tclExtdInt.h"


/*
 * Prototypes of internal functions.
 */
static unsigned int
ExpandString _ANSI_ARGS_((unsigned char *inStr,
                          int            inLength,
                          unsigned char  outStr [],
                          int           *outLengthPtr));

static int 
TclX_CindexObjCmd _ANSI_ARGS_((ClientData clientData,
                               Tcl_Interp *interp,
                               int         objc,
                               Tcl_Obj   *CONST objv[]));

static int 
TclX_ClengthObjCmd _ANSI_ARGS_((ClientData clientData,
                                Tcl_Interp *interp,
                                int         objc,
                                Tcl_Obj   *CONST objv[]));

static int
TclX_CconcatObjCmd _ANSI_ARGS_((ClientData clientData,
                                Tcl_Interp *interp,
                                int         objc,
                                Tcl_Obj   *CONST objv[]));

static int 
TclX_CrangeObjCmd _ANSI_ARGS_((ClientData clientData,
                               Tcl_Interp *interp,
                               int         objc,
                               Tcl_Obj   *CONST objv[]));

static int 
TclX_CcollateObjCmd _ANSI_ARGS_((ClientData clientData,
                                 Tcl_Interp *interp,
                                 int         objc,
                                 Tcl_Obj   *CONST objv[]));

static int 
TclX_ReplicateObjCmd _ANSI_ARGS_((ClientData clientData,
                                  Tcl_Interp *interp,
                                  int         objc,
                                  Tcl_Obj   *CONST objv[]));

static int 
TclX_TranslitObjCmd _ANSI_ARGS_((ClientData clientData,
                                 Tcl_Interp *interp,
                                 int         objc,
                                 Tcl_Obj   *CONST objv[]));

static int 
TclX_CtypeObjCmd _ANSI_ARGS_((ClientData clientData,
                              Tcl_Interp *interp,
                              int         objc,
                              Tcl_Obj   *CONST objv[]));

static int 
TclX_CtokenObjCmd _ANSI_ARGS_((ClientData clientData,
                               Tcl_Interp *interp,
                               int         objc,
                               Tcl_Obj   *CONST objv[]));

static int 
TclX_CequalObjCmd _ANSI_ARGS_((ClientData clientData,
                               Tcl_Interp *interp,
                               int         objc,
                               Tcl_Obj   *CONST objv[]));


/*-----------------------------------------------------------------------------
 * TclX_CindexObjCmd --
 *     Implements the cindex Tcl command:
 *         cindex string indexExpr
 *
 * Results:
 *      Returns the character indexed by  index  (zero  based)  from string. 
 *-----------------------------------------------------------------------------
 */
static int
TclX_CindexObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    int stringLen, idx;
    char *stringPtr;

    if (objc != 3)
        return TclX_WrongArgs (interp, objv[0], "string indexExpr");
    
    stringPtr = Tcl_GetStringFromObj (objv[1], &stringLen);

    if (TclX_RelativeExpr (interp, objv [2], stringLen, &idx) != TCL_OK) {
        return TCL_ERROR;
    }

    if ((idx < 0) || (idx >= stringLen))
        return TCL_OK;

    Tcl_SetStringObj (Tcl_GetObjResult (interp), stringPtr + idx, 1);
    return TCL_OK;
}


/*-----------------------------------------------------------------------------
 * TclX_ClengthObjCmd --
 *     Implements the clength Tcl command:
 *         clength string
 *
 * Results:
 *      Returns the length of string in characters. 
 *-----------------------------------------------------------------------------
 */
static int
TclX_ClengthObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    int length;

    if (objc != 2)
        return TclX_WrongArgs (interp, objv[0], "string");

    Tcl_GetStringFromObj (objv[1], &length);

    Tcl_SetIntObj (Tcl_GetObjResult (interp), length);
    return TCL_OK;
}



/*-----------------------------------------------------------------------------
 * TclX_CconcatObjCmd --
 *     Implements the cconcat TclX command:
 *         cconcat ?string? ?string? ?...?
 *
 * Results:
 *      The arguments concatenated.
 *-----------------------------------------------------------------------------
 */
static int
TclX_CconcatObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    Tcl_Obj    *resultPtr = Tcl_GetObjResult (interp);
    int         idx;
    int         stringLength;
    char       *stringPtr;

    /*
     * FIX: It would be faster if we calculated up how much space we needed all
     * at once.  Also we could iterate a pointer into objv until NULL
     */
    for (idx = 1; idx < objc; idx++) {
	stringPtr = Tcl_GetStringFromObj (objv [idx], &stringLength);
	Tcl_AppendToObj (resultPtr, stringPtr, stringLength);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * TclX_CrangeObjCmd --
 *     Implements the crange and csubstr Tcl commands:
 *         crange string firstExpr lastExpr
 *         csubstr string firstExpr lengthExpr
 *
 * Results:
 *      Standard Tcl result.
 * Notes:
 *   If clientData is TRUE its the range command, if its FALSE its csubstr.
 *-----------------------------------------------------------------------------
 */
static int
TclX_CrangeObjCmd (clientData, interp, objc, objv)
    ClientData   clientData;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    int first, subLen, fullLen;
    int isRange = (int) clientData;
    char *targetString;

    if (objc != 4) {
        if (isRange)
            return TclX_WrongArgs (interp, objv[0], 
                                   "string firstExpr lastExpr");
        else
            return TclX_WrongArgs (interp, objv[0], 
                                   "string firstExpr lengthExpr");
    }

    targetString = Tcl_GetStringFromObj (objv [1], &fullLen);

    if (TclX_RelativeExpr (interp, objv [2], fullLen, &first) != TCL_OK) {
        return TCL_ERROR;
    }

    if ((first < 0) || (first >= fullLen))
        return TCL_OK;

    if (TclX_RelativeExpr (interp, objv [3], fullLen, &subLen) != TCL_OK) {
        return TCL_ERROR;
    }
        
    if (isRange) {
        if (subLen < first)
            return TCL_OK;
        subLen = subLen - first +1;
    }

    if (first + subLen > fullLen)
        subLen = fullLen - first;

    Tcl_SetObjResult (interp,
                      Tcl_NewStringObj (targetString + first,
                                        subLen));
    return TCL_OK;
}


/*-----------------------------------------------------------------------------
 * TclX_CcollateObjCmd --
 *     Implements ccollate Tcl commands:
 *         ccollate [-local] string1 string2
 *
 * Results:
 *      Standard Tcl result.
 *-----------------------------------------------------------------------------
 */
static int
TclX_CcollateObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    int argIndex, result, local = FALSE;
    char *optionString;
    char *string1;
    int string1Len;
    char *string2;
    int string2Len;

    if ((objc < 3) || (objc > 4))
        return TclX_WrongArgs (interp, objv[0], "?options? string1 string2");

    if (objc == 4) {
        optionString = Tcl_GetStringFromObj (objv [1], NULL);
        if (!STREQU (optionString, "-local")) {
            TclX_AppendObjResult (interp, "Invalid option \"", optionString,
                                  "\", expected \"-local\"", (char *) NULL);
            return TCL_ERROR;
        }
        local = TRUE;
    }
    argIndex = objc - 2;
    
    string1 = Tcl_GetStringFromObj (objv [argIndex], &string1Len);
    string2 = Tcl_GetStringFromObj (objv [argIndex + 1], &string2Len);
    if ((strlen (string1) != (size_t) string1Len) ||
	(strlen (string1) != (size_t) string1Len)) {
        TclX_AppendObjResult (interp, "The " ,
                              Tcl_GetStringFromObj (objv [0], NULL),
                              " command does not support binary data",
                              (char *) NULL);
        return TCL_ERROR;
    }
    if (local) {
#ifndef NO_STRCOLL
        result = strcoll (string1, string2);
#else
        result = strcmp (string1, string2);
#endif
    } else {
        result = strcmp (string1, string2);
    }
    Tcl_SetIntObj (Tcl_GetObjResult (interp),
                   ((result == 0) ? 0 : ((result < 0) ? -1 : 1)));
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * TclX_ReplicateObjCmd --
 *     Implements the replicate Tcl command:
 *         replicate string countExpr
 *
 * Results:
 *      Returns string replicated count times.
 *-----------------------------------------------------------------------------
 */
static int
TclX_ReplicateObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj     *CONST objv[];
{
    Tcl_Obj     *resultPtr = Tcl_GetObjResult (interp);
    long         count;
    long         repCount;
    char        *stringPtr;
    int          stringLength;

    if (objc != 3)
        return TclX_WrongArgs (interp, objv[0], "string countExpr");

    if (Tcl_GetLongFromObj (interp, objv [2], &repCount) != TCL_OK)
        return TCL_ERROR;

    stringPtr = Tcl_GetStringFromObj (objv [1], &stringLength);
    for (count = 0; count < repCount; count++) {
        Tcl_AppendToObj (resultPtr, stringPtr, stringLength);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * TclX_CtokenObjCmd --
 *     Implements the clength Tcl command:
 *         ctoken strvar separators
 *
 * Results:
 *      Returns the first token and removes it from the string variable.
 * FIX: Add command to make a list.  Better yet, a new cparse command thats
 * more flexable and includes this functionallity.
 *-----------------------------------------------------------------------------
 */
static int
TclX_CtokenObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    Tcl_Obj      *varValueObj;
    Tcl_DString   string;
    char         *varValue;
    char         *startPtr;
    char         *tokenString;
    int           tokenLen;
    int           varValueLen;
    int           tokenStrLen;
    Tcl_Obj      *newVarValueObj;

    if (objc != 3)
        return TclX_WrongArgs (interp, objv[0], "strvar separators");
    
    varValueObj = Tcl_ObjGetVar2 (interp, objv [1], (Tcl_Obj *) NULL,
                                  TCL_LEAVE_ERR_MSG | TCL_PARSE_PART1);

    varValue = Tcl_GetStringFromObj (varValueObj, &varValueLen);

    if (varValue == NULL)
        return TCL_ERROR;

    Tcl_DStringInit (&string);
    Tcl_DStringAppend (&string, varValue, varValueLen);

    tokenString = Tcl_GetStringFromObj (objv [2], &tokenStrLen);

    if ((strlen (varValue) != (size_t) varValueLen) ||
        (strlen (tokenString) != (size_t) tokenStrLen)) {
        TclX_AppendObjResult (interp, "The ",
                              Tcl_GetStringFromObj (objv [0], NULL),
                              " command does not support binary data",
                              (char *) NULL);
        return TCL_ERROR;
    }

    startPtr = string.string + strspn (string.string, tokenString);
    tokenLen = strcspn (startPtr, tokenString);

    newVarValueObj = Tcl_NewStringObj (startPtr + tokenLen, -1);

    if (Tcl_ObjSetVar2 (interp, objv [1], (Tcl_Obj *) NULL, newVarValueObj,
                        TCL_LEAVE_ERR_MSG | TCL_PARSE_PART1) == NULL) {
        Tcl_DStringFree (&string);
        Tcl_DecrRefCount (newVarValueObj);
        return TCL_ERROR;
    }

    Tcl_AppendToObj (Tcl_GetObjResult (interp), startPtr, tokenLen);
    Tcl_DStringFree (&string);
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * TclX_CequalObjCmd --
 *     Implements the cexpand Tcl command:
 *         cequal string1 string2
 *
 * Results:
 *   "0" or "1".
 *-----------------------------------------------------------------------------
 */
static int
TclX_CequalObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    char *string1Ptr;
    int string1Len;
    char *string2Ptr;
    int string2Len;

    if (objc != 3)
        return TclX_WrongArgs (interp, objv[0], "string1 string2");

    string1Ptr = Tcl_GetStringFromObj (objv[1], &string1Len);
    string2Ptr = Tcl_GetStringFromObj (objv[2], &string2Len);

    Tcl_SetBooleanObj (Tcl_GetObjResult (interp),
                       ((string1Len == string2Len) &&
                        (*string1Ptr == *string2Ptr) &&
                        (memcmp (string1Ptr, string2Ptr, string1Len) == 0)));
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * ExpandString --
 *  Build an expand version of a translit range specification.
 *
 * Results:
 *  The number of characters in the expansion buffer or < 0 if the maximum
 * expansion has been exceeded.
 *-----------------------------------------------------------------------------
 */
#define MAX_EXPANSION 255

static unsigned int
ExpandString (inStr, inLength, outStr, outLengthPtr)
    unsigned char *inStr;
    int            inLength;
    unsigned char  outStr [];
    int           *outLengthPtr;
{
    int i, j;
    unsigned char *s = inStr;
    unsigned char *inStrLimit = inStr + inLength;

    i = 0;
    while((s < inStrLimit) && (i < MAX_EXPANSION)) {
        if ((s [1] == '-') && (s [2] > s [0])) {
            for (j = s [0]; j <= s [2]; j++) {
                outStr [i++] = j;
            }
            s += 3;
        } else {
            outStr [i++] = *s++;
        }
    }
    *outLengthPtr = i;
    return (i < MAX_EXPANSION);
}

/*-----------------------------------------------------------------------------
 * TclX_TranslitObjCmd --
 *     Implements the Tcl translit command:
 *     translit inrange outrange string
 *
 * Results:
 *  Standard Tcl results.
 *-----------------------------------------------------------------------------
 */
static int
TclX_TranslitObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    unsigned char from [MAX_EXPANSION+1];
    int           fromLen;
    unsigned char to   [MAX_EXPANSION+1];
    int           toLen;
    short         map [MAX_EXPANSION+1];
    unsigned char *s;
    char          *fromString;
    int            fromStringLen;
    char          *toString;
    int            toStringLen;
    Tcl_Obj       *transStringObj;
    char          *transString;
    int            transStringLen;
    int            idx;
    int            stringIndex;

    if (objc != 4)
        return TclX_WrongArgs (interp, objv[0], "from to string");

    /*
     * Expand ranges into descrete values.
     */
    fromString = Tcl_GetStringFromObj (objv[1], &fromStringLen);
    if (!ExpandString ((unsigned char *) fromString, fromStringLen,
                       from, &fromLen)) {
        TclX_AppendObjResult (interp, "inrange expansion too long",
                              (char *) NULL);
        return TCL_ERROR;
    }

    toString = Tcl_GetStringFromObj (objv [2], &toStringLen);
    if (!ExpandString ((unsigned char *) toString, toStringLen,
                       to, &toLen)) {
        TclX_AppendObjResult (interp, "outrange expansion too long",
                              (char *) NULL);
        return TCL_ERROR;
    }

    if (fromLen > toLen) {
        TclX_AppendObjResult (interp, "inrange longer than outrange", 
                              (char *) NULL);
        return TCL_ERROR;
    }

    /*
     * Build map.  Entries of -1 discard the char.  All other values are
     * positive (hence its a short).
     */
    for (idx = 0; idx <= MAX_EXPANSION; idx++) {
        map [idx] = idx;
    }
    for (idx = 0; (idx < toLen) && (idx < fromLen); idx++) {
        map [from [idx]] = to [idx];
    }
    for (; idx < fromLen; idx++)
        map [from [idx]] = -1;

    transStringObj = Tcl_DuplicateObj (objv [3]);
    transString = Tcl_GetStringFromObj (transStringObj, &transStringLen);

    for (s = (unsigned char *) transString, stringIndex = 0; 
         stringIndex < transStringLen; stringIndex++) {
        if (map [*s] >= 0) {
            *s = (unsigned char) map [*s];
            s++;
        }
    }

    Tcl_SetObjResult (interp, transStringObj);
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * TclX_CtypeObjCmd --
 *
 *      This function implements the 'ctype' command:
 *      ctype ?-failindex? class string ?failIndexVar?
 *
 *      Where class is one of the following:
 *        digit, xdigit, lower, upper, alpha, alnum,
 *        space, cntrl,  punct, print, graph, ascii, char or ord.
 *
 * Results:
 *       One or zero: Depending if all the characters in the string are of
 *       the desired class.  Char and ord provide conversions and return the
 *       converted value.
 * FIX: Add check for legal number (can be negative, hex, etc).
 *-----------------------------------------------------------------------------
 */
static int
TclX_CtypeObjCmd (dummy, interp, objc, objv)
    ClientData   dummy;
    Tcl_Interp  *interp;
    int          objc;
    Tcl_Obj    *CONST objv[];
{
    int             failIndex = FALSE;
    register char  *class;
    register char  *scanPtr;
    int             scanStringLen;
    int             classLen;
    int             optStrLen;

    Tcl_Obj        *failVarObj = NULL;
    Tcl_Obj        *classObj;
    Tcl_Obj        *stringObj;

    char           *optionString;
    int             index;

    if (objc < 3)
        goto wrongNumArgs;

    optionString = Tcl_GetStringFromObj (objv [1], &optStrLen);
    if (*optionString == '-') {
        if (STREQU (optionString, "-failindex")) {
            failIndex = TRUE;
        } else {
            int len;
            TclX_AppendObjResult (interp, "invalid option \"",
                                  Tcl_GetStringFromObj (objv [1], &len),
                                  "\", must be -failindex", (char *) NULL);
            return TCL_ERROR;
        }
    }
    if (failIndex) {
        if (objc != 5) 
            goto wrongNumArgs;
        failVarObj = objv [2];
        classObj = objv [3];
        stringObj = objv [4];
    } else {
        if (objc != 3) 
            goto wrongNumArgs;
        classObj = objv [1];
        stringObj = objv [2];
    }
    scanPtr = Tcl_GetStringFromObj (stringObj, &scanStringLen);
    class = Tcl_GetStringFromObj (classObj, &classLen);

    /*
     * Handle conversion requests.
     */
    if (STREQU (class, "char")) {
        long number;
        char myChar;

        if (failIndex) 
          goto failInvalid;
        if (Tcl_GetLongFromObj (interp, stringObj, &number) != TCL_OK)
            return TCL_ERROR;
        if ((number < 0) || (number > 255)) {
            TclX_AppendObjResult (interp,
                                  "number must be in the range 0..255", 
                                  (char *) NULL);
            return TCL_ERROR;
        }

        myChar = (char) number;
        Tcl_SetStringObj (Tcl_GetObjResult (interp),
                          &myChar, 1);
        return TCL_OK;
    }

    if (STREQU (class, "ord")) {
        if (failIndex) 
          goto failInvalid;

        /*
         * Mask to prevent sign extension.
         */
        Tcl_SetIntObj (Tcl_GetObjResult (interp), 
                       0xff & scanPtr [0]);
        return TCL_OK;
    }

    /*
     * The remainder of cases scan the string, stoping when their test case
     * fails.  The value of `index' after the loops indicating if it succeeds
     * or fails and where it fails.
     */
    if (STREQU (class, "alnum")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isalnum (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "alpha")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isalpha (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "ascii")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isascii (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "cntrl")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!iscntrl (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "digit")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isdigit (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "graph")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isgraph (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "lower")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!islower (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "print")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isprint (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "punct")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!ispunct (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "space")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isspace (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "upper")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isupper (UCHAR (*(scanPtr + index))))
                break;
        }
    } else if (STREQU (class, "xdigit")) {
        for (index = 0; index < scanStringLen; index++) {
            if (!isxdigit (UCHAR (*(scanPtr + index))))
                break;
        }
    } else {
        TclX_AppendObjResult (interp, "unrecognized class specification: \"",
                              class,
                              "\", expected one of: alnum, alpha, ascii, ",
                              "char, cntrl, digit, graph, lower, ord, ",
                              "print, punct, space, upper or xdigit",
                              (char *) NULL);
        return TCL_ERROR;
    }
    
    /*
     * Return true or false, depending if the end was reached.  Always return 
     * false for a null string.  Optionally return the failed index if there
     * is no match.
     */
    if ((index != 0) && (*(scanPtr + index) == 0))
        Tcl_SetBooleanObj (Tcl_GetObjResult (interp), TRUE);
    else {
        /*
         * If the fail index was requested, set the variable here.
         */
        if (failIndex) {
            Tcl_Obj *iObj = Tcl_NewIntObj (index);

            if (Tcl_ObjSetVar2 (interp, failVarObj, (Tcl_Obj *) NULL, 
                    iObj, TCL_LEAVE_ERR_MSG | TCL_PARSE_PART1) == NULL) {
                Tcl_DecrRefCount (iObj);
                return TCL_ERROR;
            }
        }
        Tcl_SetBooleanObj (Tcl_GetObjResult (interp), FALSE);
    }
    return TCL_OK;

  wrongNumArgs:
    return TclX_WrongArgs (interp, objv[0], "?-failindex var? class string");
    
  failInvalid:
    TclX_AppendObjResult (interp, "-failindex option is invalid for class \"",
                          class, "\"", (char *) NULL);
    return TCL_ERROR;
}

/*-----------------------------------------------------------------------------
 * TclX_StringInit --
 *   Initialize the list commands in an interpreter.
 *
 * Parameters:
 *   o interp - Interpreter to add commands to.
 *-----------------------------------------------------------------------------
 */
void
TclX_StringInit (interp)
    Tcl_Interp *interp;
{
    Tcl_CreateObjCommand (interp, 
			  "cindex",
                          TclX_CindexObjCmd, 
			  (ClientData) 0, 
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp, 
			  "clength",
                          TclX_ClengthObjCmd, 
			  (ClientData) 0,
                          (Tcl_CmdDeleteProc *)NULL);

    Tcl_CreateObjCommand (interp, 
			  "cconcat",
                          TclX_CconcatObjCmd, 
			  (ClientData) 0,
                          (Tcl_CmdDeleteProc *)NULL);

    Tcl_CreateObjCommand (interp, 
			  "crange",
                          TclX_CrangeObjCmd, 
			  (ClientData) TRUE, 
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp, 
			  "csubstr",
                          TclX_CrangeObjCmd,
			  (ClientData) FALSE, 
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp, 
			  "ccollate",
                          TclX_CcollateObjCmd,
			  (ClientData) 0,
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp,
			  "replicate",
                          TclX_ReplicateObjCmd, 
			  (ClientData) 0, 
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp, 
			  "translit",
                          TclX_TranslitObjCmd,
			  (ClientData) 0, 
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp, 
			  "ctype",
                          TclX_CtypeObjCmd,
			  (ClientData) 0, 
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp, 
			  "ctoken",
                          TclX_CtokenObjCmd,
			  (ClientData) 0, 
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp, 
			  "cequal",
			  TclX_CequalObjCmd,
			  (ClientData) 0, 
                          (Tcl_CmdDeleteProc*) NULL);

}


