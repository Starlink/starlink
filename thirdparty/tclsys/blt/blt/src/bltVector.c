/*
 * bltVector.c --
 *
 *	This module implements vector data objects.
 *
 * Copyright 1995-1998 Lucent Technologies, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the names
 * of Lucent Technologies any of their entities not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Lucent Technologies disclaims all warranties with regard to this
 * software, including all implied warranties of merchantability and
 * fitness.  In no event shall Lucent Technologies be liable for any
 * special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in
 * an action of contract, negligence or other tortuous action, arising
 * out of or in connection with the use or performance of this
 * software.
 */

/*
 * TODO:  
 *	o Add H. Kirsch's vector binary read operation
 *		x fread file0
 *		x fread -file file0
 *
 *	o Add ASCII/binary file reader
 *		x read fileName
 *	o Allow Tcl client notifications.
 *		vector x
 *		x notify call Display 
 *		x notify delete Display
 *		x notify reorder #1 #2
 */
#include "bltInt.h"
#include <ctype.h>

#ifndef TCL_NAMESPACE_ONLY
#define TCL_NAMESPACE_ONLY TCL_GLOBAL_ONLY
#endif

#define TRACE_ALL	(TCL_TRACE_WRITES | TCL_TRACE_READS | TCL_TRACE_UNSETS)
#define VECTOR_MAGIC	((unsigned int) 0x46170277)
#define DEF_ARRAY_SIZE  (64*sizeof(double))

#define SPECIAL_INDEX	-2

/* These defines allow parsing of different types of indices */

#define ALLOW_SPECIAL	(1<<0)	/* Recognize "min", "max", and "++end" as
				 * valid indices */
#define ALLOW_COLON	(1<<1)	/* Also recognize a range of indices separated
				 * by a colon */
#define CHECK_RANGE	(1<<2)	/* Verify that the specified index or range
				 * of indices are within limits */
#ifdef HAVE_DRAND48
    extern double drand48 _ANSI_ARGS_((void));
#endif

/*
 * Vector --
 *
 *	A vector is an array of double precision values.  It can be
 *	accessed through a Tcl command, a Tcl array variable, or C
 *	API. The storage for the array points initially to a
 *	statically allocated buffer, but to malloc-ed memory if more
 *	is necessary.
 *
 *	Vectors can be shared by several clients (for example, two
 *	different graph widgets).  The data is shared. When a client
 *	wants to use a vector, it allocates a vector identifier, which
 *	identifies the client.  Clients use this ID to specify a
 *	callback routine to be invoked whenever the vector is modified
 *	or destroyed.  Whenever the vector is updated or destroyed,
 *	each client is notified of the change by their callback
 *	routine.
 */

typedef struct {

    /*
     * If you change these fields, make sure you change the definition
     * of Blt_Vector in bltInt.h and blt.h too.
     */

    double *valueArr;		/* Array of values (malloc-ed) */

    int numValues;		/* Number of values in the array */

    int arraySize;		/* Number of bytes allocated for value array */

    double min, max;		/* Minimum and maximum values in the vector */

    int dirty;			/* Indicates if the vector has been updated */

    int reserved;

    /* The following fields are local to this module  */

    Tk_Uid nameId;		/* Name of the vector. This and the
				 * interpreter key the search for
				 * vectors. */

    Tcl_Interp *interp;		/* Interpreter associated with the
				 * vector */

    Tcl_HashEntry *hashPtr;	/* If non-NULL, pointer in a hash table to
				 * track the vectors in use. */

    Tcl_FreeProc *freeProc;	/* Address of procedure to call to
				 * release storage for the value
				 * array, Optionally can be one of the
				 * following: TCL_STATIC, TCL_DYNAMIC,
				 * or TCL_VOLATILE. */

    char *arrayName;		/* The name of the Tcl array variable
				 * mapped to the vector
				 * (malloc'ed). If NULL, indicates
				 * that the vector isn't mapped to any
				 * variable */

    Tcl_Namespace *varNsPtr;	/* Namespace context of the Tcl variable
				 * associated with the vector. This is
				 * needed to reset the indices of the array
				 * variable. */

    Tcl_Namespace *nsPtr;	/* Namespace context of the vector itself. */

    int offset;			/* Offset from zero of the vector's
				 * starting index */

    Tcl_Command cmdToken;	/* Token for vector's Tcl command. */

    double staticSpace[DEF_ARRAY_SIZE];

    Blt_List clientList;	/* List of clients using this vector */

    int flags;			/* Notification flags. See definitions
				 * below */

    int freeOnUnset;		/* For backward compatibility only: If non-zero,
				 * free the vector when its variable is unset. */

    int first, last;		/* Selected region of vector. This is used
				 * mostly for the math routines */

} Vector;

#define NOTIFY_UPDATED		((int)BLT_VECTOR_NOTIFY_UPDATE)
#define NOTIFY_DESTROYED	((int)BLT_VECTOR_NOTIFY_DESTROY)

#define NOTIFY_NEVER		(1<<3)	/* Never notify clients of updates to
					* the vector */
#define NOTIFY_ALWAYS		(1<<4)	/* Notify clients after each update
					* of the vector is made */
#define NOTIFY_WHENIDLE		(1<<5)	/* Notify clients at the next idle point
					* that the vector has been updated. */

#define NOTIFY_PENDING		(1<<6)	/* A do-when-idle notification of the
					* vector's clients is pending. */
#define NOTIFY_NOW		(1<<7)	/* Notify clients of changes once
					 * immediately */

#define NOTIFY_WHEN_MASK	(NOTIFY_NEVER|NOTIFY_ALWAYS|NOTIFY_WHENIDLE)

#define UPDATE_LIMITS		(1<<9)	/* The data of the vector has changed.
					* Update the min and max limits when
					* they are needed */
/*
 * ClientInfo --
 *
 *	A vector can be shared by several clients.  Each client allocates
 *	this structure which acts as a key for using the vector.  Clients
 *	can designate a callback routine which is executed by the server
 *	when the vector is updated or destroyed.
 *
 *	The structure contains a pointer to the master information of the
 *	vector held by the server.  If this pointer is NULL, it indicates
 *	that the vector has been destroyed (but as of yet, this client
 *	hasn't recognized it).
 */
typedef struct {
    unsigned int magic;		/* Magic value designating whether this
				 * really is a vector token or not */
    Vector *serverPtr;		/* Pointer to the master record of the vector
				 * If NULL, indicates that vector has been
				 * destroyed. */
    Blt_VectorChangedProc *proc;/* Routine to call when the contents of
				 * the vector change */
    ClientData clientData;	/* Data to pass on ChangedProc call */

    Blt_ListItem item;		/* Pointer to client information item in
				 * server's linked list. */

} ClientInfo;

/*
 * VectorKey
 *
 *	Trees are keyed by their name and their interpreter. This
 *	lets us use two different vectors, residing in different
 *	interpreters, that have the same name. In the future I'd like
 *	to have vectors belong to a particular namespace.  But until
 *	there's a mechanism in Tcl to notify you when a namespace is
 *	destroyed, this isn't feasible.
 */
typedef struct VectorKey {

    Tk_Uid id;
    Tcl_Namespace *nsPtr;

} VectorKey;

/*
 * Built-in math functions:
 */
typedef int (GenericMathProc) _ANSI_ARGS_(ANYARGS);

/*
 * Three types of math functions:
 *
 *	ComponentProc		Function is applied in multiple calls to
 *				each component of the vector.
 *	VectorProc		Entire vector is passed, each component is
 *				modified.
 *	ScalarProc		Entire vector is passed, single scalar value
 *				is returned.
 */

typedef double (ComponentProc) _ANSI_ARGS_((double value));
typedef int (VectorProc) _ANSI_ARGS_((Vector *vPtr));
typedef double (ScalarProc) _ANSI_ARGS_((Vector *vPtr));

typedef struct {
    Tk_Uid nameId;		/* Name of the math function. */
    GenericMathProc *proc;	/* Procedure that implements this function. */
    ClientData clientData;	/* Argument to pass when invoking the function. */
    int dynAlloc;		/* Indicates if this record was dynamically
				 * allocated. */
} MathFunction;

/*
 * Macros for testing floating-point values for certain special cases:
 *
 *	IS_NAN	Test for not-a-number by comparing a value against itself
 *	IF_INF	Test for infinity by comparing against the largest floating
 *		point value.
 */

#define IS_NAN(v) ((v) != (v))

#ifdef DBL_MAX
#   define IS_INF(v) (((v) > DBL_MAX) || ((v) < -DBL_MAX))
#else
#   define IS_INF(v) 0
#endif

/*
 * The data structure below is used to describe an expression value,
 * which can be either a double-precision floating-point value,
 * or a string.  A given number has only one value at a time.
 */

#define STATIC_STRING_SPACE 150

/*
 * The token types are defined below.  In addition, there is a table
 * associating a precedence with each operator.  The order of types
 * is important.  Consult the code before changing it.
 */

enum Tokens {
    VALUE, OPEN_PAREN, CLOSE_PAREN, COMMA, END, UNKNOWN,
    MULT = 8, DIVIDE, MOD, PLUS, MINUS,
    LEFT_SHIFT, RIGHT_SHIFT,
    LESS, GREATER, LEQ, GEQ, EQUAL, NEQ,
    OLD_BIT_AND, EXPONENT, OLD_BIT_OR, OLD_QUESTY, OLD_COLON,
    AND, OR, UNARY_MINUS, OLD_UNARY_PLUS, NOT, OLD_BIT_NOT
};

/*
 * The following data structure is used by various parsing procedures
 * to hold information about where to store the results of parsing
 * (e.g. the substituted contents of a quoted argument, or the result
 * of a nested command).  At any given time, the space available
 * for output is fixed, but a procedure may be called to expand the
 * space available if the current space runs out.
 */
typedef struct ParseValue {
    char *buffer;		/* Address of first character in
				 * output buffer. */
    char *next;			/* Place to store next character in
				 * output buffer. */
    char *end;			/* Address of the last usable character
				 * in the buffer. */
    void (*expandProc) _ANSI_ARGS_((struct ParseValue * pvPtr, int needed));
    /* Procedure to call when space runs out;
				 * it will make more space. */
    ClientData clientData;	/* Arbitrary information for use of
				 * expandProc. */
} ParseValue;

typedef struct {
    Vector *vPtr;
    char staticSpace[STATIC_STRING_SPACE];
    ParseValue pv;		/* Used to hold a string value, if any. */
} Value;

/*
 * The data structure below describes the state of parsing an expression.
 * It's passed among the routines in this module.
 */
typedef struct {
    char *expr;			/* The entire right-hand side of the expression,
				 * as originally passed to Blt_ExprVector. */

    char *nextPtr;		/* Position of the next character to be
				 * scanned from the expression string. */

    enum Tokens token;		/* Type of the last token to be parsed from
				 * nextPtr.  See below for definitions.
				 * Corresponds to the characters just
				 * before nextPtr. */

} ParseInfo;

/*
 * Precedence table.  The values for non-operator token types are ignored.
 */
static int precTable[] =
{
    0, 0, 0, 0, 0, 0, 0, 0,
    12, 12, 12,			/* MULT, DIVIDE, MOD */
    11, 11,			/* PLUS, MINUS */
    10, 10,			/* LEFT_SHIFT, RIGHT_SHIFT */
    9, 9, 9, 9,			/* LESS, GREATER, LEQ, GEQ */
    8, 8,			/* EQUAL, NEQ */
    7,				/* OLD_BIT_AND */
    13,				/* EXPONENTIATION */
    5,				/* OLD_BIT_OR */
    4,				/* AND */
    3,				/* OR */
    2,				/* OLD_QUESTY */
    1,				/* OLD_COLON */
    14, 14, 14, 14		/* UNARY_MINUS, OLD_UNARY_PLUS, NOT,
					 * OLD_BIT_NOT */
};

#ifdef notdef
/*
 * Mapping from operator numbers to strings;  used for error messages.
 */

static char *operatorStrings[] =
{
    "VALUE", "(", ")", ",", "END", "UNKNOWN", "6", "7",
    "*", "/", "%", "+", "-", "<<", ">>", "<", ">", "<=",
    ">=", "==", "!=", "&", "^", "|", "&&", "||", "?", ":",
    "-", "+", "!", "~",
};
#endif

static Tcl_HashTable vectorTable;	/* Table of vectors */
static Tcl_HashTable mathProcTable;	/* Table of vector math functions */
static Tcl_HashTable indexProcTable;
static int refCount = 0;
static int initialized = 0;

/*
 * Forward declarations.
 */
static int NextValue _ANSI_ARGS_((Tcl_Interp *interp, ParseInfo * infoPtr,
	int prec, Value * valuePtr));

static char *VariableProc _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, char *part1, char *part2, int flags));

static int VectorInstCmd _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	int argc, char **argv));

extern int TclParseBraces _ANSI_ARGS_((Tcl_Interp *interp, char *string,
	char **termPtr, ParseValue * pvPtr));

extern int TclParseNestedCmd _ANSI_ARGS_((Tcl_Interp *interp, char *string,
	int flags, char **termPtr, ParseValue * pvPtr));

extern int TclParseQuotes _ANSI_ARGS_((Tcl_Interp *interp, char *string,
	int termChar, int flags, char **termPtr, ParseValue * pvPtr));

extern void TclExpandParseValue _ANSI_ARGS_((ParseValue * pvPtr, int needed));

#ifdef __STDC__
static Tcl_IdleProc NotifyClients;
static Tcl_CmdDeleteProc DestroyVectorInstCmd;
static Tcl_CmdProc VectorCmd, VectorInstCmd;
static Tcl_CmdDeleteProc VectorDeleteCmd;
static Blt_VectorIndexProc Min, Max, Mean, Sum, Product;
extern void srand48(long int);
#endif /* __STDC__ */

/*
 * ----------------------------------------------------------------------
 *
 * FindLimits --
 *
 *	Determines the minimum and maximum values in the vector.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The min and max fields of the vector are updated.
 *
 * ----------------------------------------------------------------------
 */
static void
FindLimits(array, first, last, minPtr, maxPtr)
    double array[];
    int first, last;
    double *minPtr, *maxPtr;
{
    register double min, max;
    register int i;

    if (first > last) {
	min = max = 0.0;
    } else {
	min = max = array[first];
	for (i = first + 1; i <= last; i++) {
	    if (min > array[i]) {
		min = array[i];
	    } else if (max < array[i]) {
		max = array[i];
	    }
	}
    }
    *minPtr = min, *maxPtr = max;
}

static void
UpdateLimits(vPtr)
    Vector *vPtr;
{
    FindLimits(vPtr->valueArr, 0, vPtr->numValues - 1, &(vPtr->min),
	&(vPtr->max));
    vPtr->flags &= ~UPDATE_LIMITS;
}

/*
 * ----------------------------------------------------------------------
 *
 * GetIndex --
 *
 *	Converts the string representing an index in the vector, to
 *	its numeric value.  A valid index may be an numeric string of
 *	the string "end" (indicating the last element in the string).
 *
 * Results:
 *	A standard Tcl result.  If the string is a valid index, TCL_OK
 *	is returned.  Otherwise TCL_ERROR is returned and interp->result
 *	will contain an error message.
 *
 * ----------------------------------------------------------------------
 */
static int
GetIndex(vPtr, string, indexPtr, flags, procPtrPtr)
    Vector *vPtr;
    char *string;
    int *indexPtr;
    int flags;
    Blt_VectorIndexProc **procPtrPtr;
{
    char c;
    long int value;

    c = string[0];

    /* Treat the index "end" like a numeric index.  */

    if ((c == 'e') && (strcmp(string, "end") == 0)) {
	if (vPtr->numValues < 1) {
	    Tcl_AppendResult(vPtr->interp,
		"bad index \"end\": vector is empty", (char *)NULL);
	    return TCL_ERROR;
	}
	*indexPtr = vPtr->numValues - 1;
	return TCL_OK;
    } else if ((c == '+') && (strcmp(string, "++end") == 0)) {
	*indexPtr = vPtr->numValues;
	return TCL_OK;
    }
    if (procPtrPtr != NULL) {
	Tcl_HashEntry *hPtr;

	hPtr = Tcl_FindHashEntry(&indexProcTable, string);
	if (hPtr != NULL) {
	    *indexPtr = SPECIAL_INDEX;
	    *procPtrPtr = (Blt_VectorIndexProc *) Tcl_GetHashValue(hPtr);
	    return TCL_OK;
	}
    }
    if (Tcl_ExprLong(vPtr->interp, string, &value) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Correct the index by the current value of the offset. This makes
     * all the numeric indices non-negative, which is how we distinguish
     * the special non-numeric indices.
     */
    value -= vPtr->offset;

    if ((value < 0) || ((flags & CHECK_RANGE) && (value >= vPtr->numValues))) {
	Tcl_AppendResult(vPtr->interp, "index \"", string,
	    "\" is out of range", (char *)NULL);
	return TCL_ERROR;
    }
    *indexPtr = (int)value;
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * GetIndex2 --
 *
 *	Converts the string representing an index in the vector, to
 *	its numeric value.  A valid index may be an numeric string of
 *	the string "end" (indicating the last element in the string).
 *
 * Results:
 *	A standard Tcl result.  If the string is a valid index, TCL_OK
 *	is returned.  Otherwise TCL_ERROR is returned and interp->result
 *	will contain an error message.
 *
 * ----------------------------------------------------------------------
 */
static int
GetIndex2(vPtr, string, flags, procPtrPtr)
    Vector *vPtr;
    char *string;
    int flags;
    Blt_VectorIndexProc **procPtrPtr;
{
    Blt_VectorIndexProc *procPtr;
    int newIndex;
    char *colon;

    colon = NULL;
    if (flags & ALLOW_COLON) {
	colon = strchr(string, ':');
    }
    if (colon != NULL) {
	if (string == colon) {
	    vPtr->first = 0;	/* Default to the first index */
	} else {
	    int result;

	    *colon = '\0';
	    result = GetIndex(vPtr, string, &newIndex, flags,
		(Blt_VectorIndexProc **) NULL);
	    *colon = ':';
	    if (result != TCL_OK) {
		return TCL_ERROR;
	    }
	    vPtr->first = newIndex;
	}
	if (*(colon + 1) == '\0') {
	    /* Default to the last index */
	    vPtr->last = (vPtr->numValues > 0) ? vPtr->numValues - 1 : 0;
	} else {
	    if (GetIndex(vPtr, colon + 1, &newIndex, flags,
		    (Blt_VectorIndexProc **) NULL) != TCL_OK) {
		return TCL_ERROR;
	    }
	    vPtr->last = newIndex;
	}
	if (vPtr->first > vPtr->last) {
	    Tcl_AppendResult(vPtr->interp, "bad range \"", string,
		"\" (first > last)", (char *)NULL);
	    return TCL_ERROR;
	}
    } else {
	if (GetIndex(vPtr, string, &newIndex, flags, &procPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	vPtr->last = vPtr->first = newIndex;
	if (procPtrPtr != NULL) {
	    *procPtrPtr = procPtr;
	}
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * LookupVector --
 *
 *	Searches for the vector associated with the name given.
 *	Allow for a range specification.
 *
 * Results:
 *	Returns a pointer to the vector if found, otherwise NULL.
 *	If the name is not associated with a vector and the
 *	TCL_LEAVE_ERR_MSG flag is set, and interp->result will
 *	contain an error message.
 *
 * ----------------------------------------------------------------------
 */
static Vector *
LookupVector(interp, name)
    Tcl_Interp *interp;
    char *name;
{
    Tcl_HashEntry *hPtr;
    VectorKey key;
    char *vecName;
    Tcl_Namespace *nsPtr;

    nsPtr = NULL;
    vecName = name;

#if ALLOW_NAMESPACE_QUALIFIERS
    if (Blt_ParseQualifiedName(interp, name, &nsPtr, &vecName) != TCL_OK) {
	return NULL;		/* Can't find namespace. */
    }
#endif /* ALLOW_NAMESPACE_QUALIFIERS */

    if (nsPtr == NULL) {
	nsPtr = Tcl_GetCurrentNamespace(interp);
    }
    key.nsPtr = nsPtr;
    key.id = Blt_FindUid(vecName);
    if (key.id == NULL) {
	return NULL;
    }
    hPtr = Tcl_FindHashEntry(&vectorTable, (char *)&key);
    if (hPtr == NULL) {
	return NULL;
    }
    return (Vector *) Tcl_GetHashValue(hPtr);
}


static Vector *
ParseVector(interp, start, endPtr)
    Tcl_Interp *interp;
    char *start;
    char **endPtr;
{
    register char *p;
    char saved;
    Vector *vPtr;

    p = start;
    while (isspace(UCHAR(*p))) {
	p++;
    }
    start = p;

    /* Check the vector name */
#ifdef ALLOW_NAMESPACE_QUALIFIERS
    while ((isalnum(UCHAR(*p))) || (*p == '_') || (*p == ':') || (*p == '@')) {
	p++;
    }
#else
    while ((isalnum(UCHAR(*p))) || (*p == '_') || (*p == '@')) {
	p++;
    }
#endif /* ALLOW_NAMESPACE_QUALIFIERS */
    saved = *p;
    *p = '\0';
    vPtr = LookupVector(interp, start);
    if (vPtr == NULL) {
	Tcl_AppendResult(interp, "can't find a vector \"", start, "\"",
	    (char *)NULL);
	*p = saved;
	return NULL;
    }
    *p = saved;
    vPtr->first = 0;
    vPtr->last = vPtr->numValues - 1;
    if (*p == '(') {
	int count, result;

	start = p + 1;
	p++;

	/* Find the matching right parenthesis */
	count = 1;
	while (*p != '\0') {
	    if (*p == ')') {
		count--;
		if (count == 0) {
		    break;
		}
	    } else if (*p == '(') {
		count++;
	    }
	    p++;
	}
	if (count > 0) {
	    Tcl_AppendResult(interp, "unbalanced parentheses \"", start, "\"",
		(char *)NULL);
	    return NULL;
	}
	saved = *p;
	*p = '\0';
	result = GetIndex2(vPtr, start, (ALLOW_COLON | CHECK_RANGE),
	    (Blt_VectorIndexProc **) NULL);
	*p = ')';
	if (result != TCL_OK) {
	    return NULL;
	}
	p++;
    }
    *endPtr = p;
    return vPtr;
}

/*
 * ----------------------------------------------------------------------
 *
 * NotifyClients --
 *
 *	Notifies each client of the vector that the vector has changed
 *	(updated or destroyed) by calling the provided function back.
 *	The function pointer may be NULL, in that case the client is
 *	not notified.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The results depend upon what actions the client callbacks
 *	take.
 *
 * ----------------------------------------------------------------------
 */
static void
NotifyClients(clientData)
    ClientData clientData;
{
    Vector *vPtr = (Vector *)clientData;
    Blt_ListItem item;
    ClientInfo *clientPtr;
    Blt_VectorNotify notify;

    notify = (vPtr->flags & NOTIFY_DESTROYED)
	? BLT_VECTOR_NOTIFY_DESTROY : BLT_VECTOR_NOTIFY_UPDATE;
    vPtr->flags &= ~(NOTIFY_UPDATED | NOTIFY_DESTROYED | NOTIFY_PENDING);

    if (vPtr->flags & UPDATE_LIMITS) {
	UpdateLimits(vPtr);
    }
    for (item = Blt_ListFirstItem(&(vPtr->clientList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	clientPtr = (ClientInfo *)Blt_ListGetValue(item);
	if (clientPtr->proc != NULL) {
	    (*clientPtr->proc) (vPtr->interp, clientPtr->clientData, notify);
	}
    }
    /*
     * Some clients may not handle the "destroy" callback properly
     * (they should call Blt_FreeVectorId to release the client
     * identifier), so we need to mark any remaining clients to
     * indicate that vector's server has gone away.
     */
    if (notify == BLT_VECTOR_NOTIFY_DESTROY) {
	for (item = Blt_ListFirstItem(&(vPtr->clientList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    clientPtr = (ClientInfo *)Blt_ListGetValue(item);
	    clientPtr->serverPtr = NULL;
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * UpdateClients --
 *
 *	Notifies each client of the vector that the vector has changed
 *	(updated or destroyed) by calling the provided function back.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The results depend upon what actions the client callbacks
 *	take.
 *
 * ----------------------------------------------------------------------
 */
static void
UpdateClients(vPtr)
    Vector *vPtr;
{
    vPtr->dirty++;
    if (vPtr->flags & NOTIFY_NEVER) {
	return;
    }
    vPtr->flags |= NOTIFY_UPDATED;
    if (vPtr->flags & NOTIFY_ALWAYS) {
	NotifyClients((ClientData)vPtr);
	return;
    }
    if (!(vPtr->flags & NOTIFY_PENDING)) {
	vPtr->flags |= NOTIFY_PENDING;
	Tk_DoWhenIdle(NotifyClients, (ClientData)vPtr);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * FlushCache --
 *
 *	Unsets all the elements of the Tcl array variable associated
 *	with the vector, freeing memory associated with the variable.
 *	This includes both the hash table and the hash keys.  The down
 *	side is that this effectively flushes the caching of vector
 *	elements in the array.  This means that the subsequent reads
 *	of the array will require a decimal to string conversion.
 *
 *	This is needed when the vector changes its values, making
 *	the array variable out-of-sync.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	All elements of array variable (except one) are unset, freeing
 *	the memory associated with the variable.
 *
 * ----------------------------------------------------------------------
 */
static void
FlushCache(vPtr)
    Vector *vPtr;
{
    int globalFlags;
    Tcl_CallFrame *framePtr;

    if (vPtr->arrayName == NULL) {
	return;			/* Doesn't use the variable API */
    }
    framePtr = NULL;
    if (vPtr->varNsPtr != NULL) {
	framePtr = Blt_EnterNamespace(vPtr->interp, vPtr->varNsPtr);
    }
    /*
     * Turn off the trace temporarily so that we can unset all the
     * elements in the array.
     */
    globalFlags = (vPtr->varNsPtr != NULL)
	? (TCL_NAMESPACE_ONLY | TCL_GLOBAL_ONLY) : 0;
    Tcl_UntraceVar2(vPtr->interp, vPtr->arrayName, (char *)NULL,
	TRACE_ALL | globalFlags, VariableProc, (ClientData)vPtr);

    /* Unset the entire array */
    Tcl_UnsetVar2(vPtr->interp, vPtr->arrayName, (char *)NULL, globalFlags);

    /* Restore the "end" index by default and the trace on the entire array */
    Tcl_SetVar2(vPtr->interp, vPtr->arrayName, "end", "", globalFlags);
    Tcl_TraceVar2(vPtr->interp, vPtr->arrayName, (char *)NULL,
	TRACE_ALL | globalFlags, VariableProc, (ClientData)vPtr);

    if ((vPtr->varNsPtr != NULL) && (framePtr != NULL)) {
	Blt_LeaveNamespace(vPtr->interp, framePtr);	/* Go back to current */
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * FindVector --
 *
 *	Searches for the vector associated with the name given.
 *	Allow for a range specification.
 *
 * Results:
 *	Returns a pointer to the vector if found, otherwise NULL.
 *	If the name is not associated with a vector and the
 *	TCL_LEAVE_ERR_MSG flag is set, and interp->result will
 *	contain an error message.
 *
 * ----------------------------------------------------------------------
 */
static Vector *
FindVector(interp, vecName, flags)
    Tcl_Interp *interp;
    char *vecName;
    unsigned int flags;
{
    Vector *vPtr;
    char *endPtr;

    vPtr = ParseVector(interp, vecName, &endPtr);
    if (flags & TCL_LEAVE_ERR_MSG) {
	if (vPtr == NULL) {
	    return NULL;
	}
	if (*endPtr != '\0') {
	    Tcl_AppendResult(interp, "extra characters after vector name",
		(char *)NULL);
	    return NULL;
	}
    }
    Tcl_ResetResult(interp);
    return vPtr;
}

/*
 * ----------------------------------------------------------------------
 *
 * DeleteCommand --
 *
 *	Deletes the Tcl command associated with the vector, without
 *	triggering a callback to "DestroyVectorInstCmd".
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
DeleteCommand(vPtr)
    Vector *vPtr;		/* Vector associated with the Tcl command. */
{
    char *name;			/* Name of Tcl command. */
    Tcl_CmdInfo cmdInfo;

    name = Tcl_GetCommandName(vPtr->interp, vPtr->cmdToken);
    if (Tcl_GetCommandInfo(vPtr->interp, name, &cmdInfo)) {
	cmdInfo.deleteProc = NULL;	/* Disable the callback before deleting the
				    * Tcl command.*/
	Tcl_SetCommandInfo(vPtr->interp, name, &cmdInfo);
	Tcl_DeleteCommand(vPtr->interp, name);
    }
    vPtr->cmdToken = 0;
}

/*
 * ----------------------------------------------------------------------
 *
 * UnmapVariable --
 *
 *	Destroys the trace on the current Tcl variable designated
 *	to access the vector.
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
UnmapVariable(vPtr)
    Vector *vPtr;
{
    int globalFlags;
    Tcl_CallFrame *framePtr;

    framePtr = NULL;
    if (vPtr->varNsPtr != NULL) {	/* Activate namespace */
	framePtr = Blt_EnterNamespace(vPtr->interp, vPtr->varNsPtr);
    }
    globalFlags = (vPtr->varNsPtr != NULL)
	? (TCL_NAMESPACE_ONLY | TCL_GLOBAL_ONLY) : 0;

    /* Unset the entire array */
    Tcl_UntraceVar2(vPtr->interp, vPtr->arrayName, (char *)NULL,
	(TRACE_ALL | globalFlags), VariableProc, (ClientData)vPtr);
    Tcl_UnsetVar2(vPtr->interp, vPtr->arrayName, (char *)NULL, globalFlags);

    if ((vPtr->varNsPtr != NULL) && (framePtr != NULL)) {
	/* Go back to current namespace */
	Blt_LeaveNamespace(vPtr->interp, framePtr);
    }
    free((char *)vPtr->arrayName);
    vPtr->arrayName = NULL;
    vPtr->varNsPtr = NULL;
}

/*
 * ----------------------------------------------------------------------
 *
 * MapVariable --
 *
 *	Sets up traces on a Tcl variable to access the vector.
 *
 *	If another variable is already mapped, it's first untraced
 *	and removed.  Don't do anything else for variables named ""
 *	(even though Tcl allows this pathology). Saves the name of
 *	the new array variable.
 *
 * Results:
 *	A standard Tcl result. If an error occurs setting the variable
 *	TCL_ERROR is returned and an error message is left in the
 *	interpreter.
 *
 * Side effects:
 *	Traces are set for the new variable. The new variable
 *	name is saved in a malloc'ed string in vPtr->arrayName.
 *	If this variable is non-NULL, it indicates that a Tcl variable
 *	has been mapped to this vector.
 *
 * ----------------------------------------------------------------------
 */
static int
MapVariable(interp, vPtr, name)
    Tcl_Interp *interp;
    Vector *vPtr;
    char *name;
{
    int globalFlags;
    Tcl_Namespace *nsPtr;
    Tcl_CallFrame *framePtr;
    char *varName, *result;

    if (vPtr->arrayName != NULL) {
	UnmapVariable(vPtr);
    }
    if ((name == NULL) || (name[0] == '\0')) {
	return TCL_OK;		/* If the variable name is the empty string,
				 * simply return after destroying any existing
				 * variable. */
    }
    framePtr = NULL;
    /*
     * Get the variable name (without the namespace qualifier).
     * [incr Tcl] doesn't like qualifiers, so enter the namespace
     * if one was designated.
     */
    if (Blt_ParseQualifiedName(interp, name, &nsPtr, &varName) != TCL_OK) {
	Tcl_AppendResult(interp, "can't find namespace in \"", name, "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if (nsPtr != NULL) {
	framePtr = Blt_EnterNamespace(interp, nsPtr);
    }
    /*
     * To play it safe, delete the variable first.  This has
     * side-effect of unmapping the variable from any vector
     * that may be currently using it.
     */
    Tcl_UnsetVar2(interp, varName, (char *)NULL, 0);

    /*
     * Set the index "end" in the array.  This will create the
     * variable, that we need to immediately exist so that we can
     * check its namespace context.
     */
    result = Tcl_SetVar2(interp, varName, "end", "", TCL_LEAVE_ERR_MSG);

    /*
     * Determine if the variable is global or not.  If there wasn't
     * a namespace qualifier, it still may be global.  We need to
     * look inside the Var structure to see what it's namespace field
     * says.  NULL indicates that it's local.
     */
    vPtr->varNsPtr = Blt_NamespaceOfVariable(interp, varName);

    globalFlags = (vPtr->varNsPtr != NULL)
	? (TCL_NAMESPACE_ONLY | TCL_GLOBAL_ONLY) : 0;

    if (result != NULL) {
	/* Trace the array on reads, writes, and unsets */
	Tcl_TraceVar2(interp, varName, (char *)NULL, (TRACE_ALL | globalFlags),
	    VariableProc, (ClientData)vPtr);
    }
    if ((nsPtr != NULL) && (framePtr != NULL)) {
	Blt_LeaveNamespace(interp, framePtr);	/* Go back to current */
    }
    vPtr->arrayName = strdup(varName);
    return (result == NULL) ? TCL_ERROR : TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * ResizeVector --
 *
 *	Resizes the vector to the new size.
 *
 *	The new size of the vector is computed by doubling the
 *	size of the vector until it fits the number of slots needed
 *	(designated by *length*).
 *
 *	If the new size is the same as the old, simply adjust the
 *	length of the vector.  Otherwise we're copying the data from
 *	one memory location to another.  Either the new size fits the
 *	static storage of the vector *staticSpace* or it's malloc'ed.
 *	The trailing elements of the vector need to be reset to zero.
 *
 *	If the storage changed memory locations, free up the old
 *	location if it was dynamically allocated.
 *
 * Results:
 *	A standard Tcl result.  If the reallocation is successful,
 *	TCL_OK is returned, otherwise TCL_ERROR.
 *
 * Side effects:
 *	Memory for the array is reallocated.
 *
 * ----------------------------------------------------------------------
 */

static int
ResizeVector(vPtr, length)
    Vector *vPtr;
    int length;
{
    int arraySize;		/* Size of array in bytes */
    double *newArr;
    int wanted, used;

    wanted = length * sizeof(double);
    used = vPtr->numValues * sizeof(double);

    /* Compute the new size by doubling old size until it's big enough */
    arraySize = DEF_ARRAY_SIZE;
    if (wanted > DEF_ARRAY_SIZE) {
	while (arraySize < wanted) {
	    arraySize += arraySize;
	}
    }
    if (arraySize == vPtr->arraySize) {
	newArr = vPtr->valueArr;/* Same size, use current array. */
    } else {
	if (arraySize > DEF_ARRAY_SIZE) {
	    /*
	     * The new size is too big for the static array, so
	     * dynamically allocate a new array.
	     */
	    newArr = (double *)malloc(arraySize);
	    if (newArr == NULL) {
		char errMsg[200];

		sprintf(errMsg, "failed to resize \"%s\" to %d bytes",
		    vPtr->nameId, arraySize);
		Tcl_AppendResult(vPtr->interp, errMsg, (char *)NULL);
		return TCL_ERROR;
	    }
	} else {
	    /*
	     * The only way we can get here is if the new size is smaller
	     * and we cross the threshold back to static space
	     */
	    assert(vPtr->valueArr != vPtr->staticSpace);

	    assert(arraySize < vPtr->arraySize);

	    /* Otherwise, we'll use the static space. */
	    newArr = vPtr->staticSpace;
	}
	if (wanted < used) {
	    used = wanted;
	}
	/* Copy any previous data */
	if (used > 0) {
	    memcpy((char *)newArr, (char *)vPtr->valueArr, used);
	}
    }
    /* Clear any new slots that we're now using in the array */
    if (wanted > used) {
	memset((char *)newArr + used, 0, wanted - used);
    }
    if (newArr != vPtr->valueArr) {
	/*
	 * We're not using the old storage anymore, so free it if
	 * it's not static.  It can be static either because it's
	 * using the default staticSpace field of the vector, or
	 * because the user previously reset the vector with a
	 * statically allocated array (setting freeProc to
	 * TCL_STATIC).
	 */
	if ((vPtr->valueArr != vPtr->staticSpace) &&
	    (vPtr->freeProc != TCL_STATIC)) {
	    if (vPtr->freeProc == TCL_DYNAMIC) {
		free((char *)vPtr->valueArr);
	    } else {
		(*vPtr->freeProc) ((char *)vPtr->valueArr);
	    }
	}
	/* Set the type of the new storage */
	vPtr->freeProc = (newArr == vPtr->staticSpace)
	    ? TCL_STATIC : TCL_DYNAMIC;
    }
    vPtr->valueArr = newArr;
    vPtr->arraySize = arraySize;
    vPtr->numValues = length;
    vPtr->first = 0;
    vPtr->last = length - 1;
    return TCL_OK;
}

static Vector *
NewVector(interp, nameId)
    Tcl_Interp *interp;
    Tk_Uid nameId;
{
    Vector *vPtr;

    vPtr = (Vector *)calloc(1, sizeof(Vector));
    assert(vPtr);
    vPtr->flags = NOTIFY_WHENIDLE;
    vPtr->freeProc = TCL_STATIC;
    vPtr->nameId = nameId;
    vPtr->valueArr = vPtr->staticSpace;
    vPtr->arraySize = DEF_ARRAY_SIZE;
    vPtr->interp = interp;
    vPtr->hashPtr = NULL;
    Blt_InitList(&(vPtr->clientList), TCL_ONE_WORD_KEYS);
    return vPtr;
}

/*
 * ----------------------------------------------------------------------
 *
 * FreeVector --
 *
 *	Removes the memory and frees resources associated with the
 *	vector.
 *
 *	o Removes the trace and the Tcl array variable and unsets
 *	  the variable.
 *	o Notifies clients of the vector that the vector is being
 *	  destroyed.
 *	o Removes any clients that are left after notification.
 *	o Frees the memory (if necessary) allocated for the array.
 *	o Removes the entry from the hash table of vectors.
 *	o Frees the memory allocated for the name.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *
 * ----------------------------------------------------------------------
 */
static void
FreeVector(vPtr)
    Vector *vPtr;
{
    Blt_ListItem item;
    ClientInfo *clientPtr;

    if (vPtr->cmdToken != 0) {
	DeleteCommand(vPtr);
    }
    if (vPtr->arrayName != NULL) {
	UnmapVariable(vPtr);
    }
    vPtr->numValues = 0;

    /* Immediately notify clients that vector is going away */
    if (vPtr->flags & NOTIFY_PENDING) {
	vPtr->flags &= ~NOTIFY_PENDING;
	Tk_CancelIdleCall(NotifyClients, (ClientData)vPtr);
    }
    vPtr->flags |= NOTIFY_DESTROYED;
    NotifyClients((ClientData)vPtr);

    for (item = Blt_ListFirstItem(&(vPtr->clientList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	clientPtr = (ClientInfo *)Blt_ListGetValue(item);
	free((char *)clientPtr);
    }
    Blt_ListReset(&(vPtr->clientList));
    if ((vPtr->valueArr != vPtr->staticSpace) &&
	(vPtr->freeProc != TCL_STATIC)) {
	if (vPtr->freeProc == TCL_DYNAMIC) {
	    free((char *)vPtr->valueArr);
	} else {
	    (*vPtr->freeProc) ((char *)vPtr->valueArr);
	}
    }
    if (vPtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(vPtr->hashPtr);
    }
#ifdef ALLOW_NAMESPACE_QUALIFIERS
    if (vPtr->nsPtr != NULL) {
	Blt_DestroyNsDeleteNotify(vPtr->nsPtr, (ClientData)vPtr);
    }
#endif /* ALLOW_NAMESPACE_QUALIFIERS */
    if (vPtr->nameId != NULL) {
	Blt_FreeUid(vPtr->nameId);
    }
    free((char *)vPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyVectorInstCmd --
 *
 *	Deletes the command associated with the vector.  This is called
 *	only when the command associated with the vector is destroyed.
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyVectorInstCmd(clientData)
    ClientData clientData;
{
    Vector *vPtr = (Vector *)clientData;

    vPtr->cmdToken = 0;
    FreeVector(vPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateVector --
 *
 *	Creates a vector structure and the following items:
 *
 *	o Tcl command
 *	o Tcl array variable and establishes traces on the variable
 *	o Adds a  new entry in the vector hash table
 *
 * Results:
 *	A pointer to the new vector structure.  If an error occurred
 *	NULL is returned and an error message is left in
 *	interp->result.
 *
 * Side effects:
 *	A new Tcl command and array variable is added to the
 *	interpreter.
 *
 * ----------------------------------------------------------------------
 */
static Vector *
CreateVector(interp, name, cmdName, varName, newPtr)
    Tcl_Interp *interp;
    char *name;			/* Name of the vector */
    char *cmdName;		/* Name of the Tcl command mapped to
				 * the vector */
    char *varName;		/* Name of the Tcl array mapped to the
				 * vector */
    int *newPtr;
{
    Vector *vPtr;
    char string[200];
    register char *p;
    int isNew;

    isNew = 0;

    /*
     * -----------------------------------------------------------------
     *
     *	Right now, namespace qualifiers, make no sense for vectors.
     *	They do for the Tcl command or variable associated with the
     *	vector, but not the vector name itself since they aren't linked
     *	to a particular namespace.  There's no association with a
     *  namespace, because you can't detect when a namespace is
     *  destroyed.
     *
     * -----------------------------------------------------------------
     */
    if ((name[0] == '#') && (strcmp(name, "#auto") == 0)) {
	static int nextId = 0;

	name = string;
	/* Automatically generate a unique vector names */
	do {
	    sprintf(string, "vector%d", nextId++);
	} while (FindVector(interp, string, 0) != NULL);
	vPtr = NULL;
    } else {
	for (p = name; *p != '\0'; p++) {
#ifdef ALLOW_NAMESPACE_QUALIFIERS
	    if ((!isalnum(UCHAR(*p))) &&
		(*p != '_') && (*p != ':') && (*p != '@')) {
#else
	    if ((!isalnum(UCHAR(*p))) && (*p != '_') && (*p != '@')) {
#endif /* ALLOW_NAMESPACE_QUALIFIERS */
		Tcl_AppendResult(interp, "bad vector name \"", name,
		    "\": must contain digits, letters, or underscore",
		    (char *)NULL);
		return NULL;
	    }
	}
	vPtr = FindVector(interp, name, 0);
    }
    if (vPtr == NULL) {
	VectorKey key;
	Tcl_HashEntry *hPtr;
	char *vecName;
	Tcl_Namespace *nsPtr;

	nsPtr = NULL;
	vecName = name;

#if ALLOW_NAMESPACE_QUALIFIERS
	if (Blt_ParseQualifiedName(interp, name, &nsPtr, &vecName) != TCL_OK) {
	    Tcl_AppendResult(interp, "can't find namespace in \"", name, "\"",
		(char *)NULL);
	    return NULL;
	}
#endif /* ALLOW_NAMESPACE_QUALIFIERS */

	if (nsPtr == NULL) {
	    nsPtr = Tcl_GetCurrentNamespace(interp);
	}
	key.nsPtr = nsPtr;
	key.id = Blt_GetUid(vecName);
	key.nsPtr = nsPtr;
	hPtr = Tcl_CreateHashEntry(&vectorTable, (char *)&key, &isNew);
	vPtr = NewVector(interp, key.id);
	vPtr->hashPtr = hPtr;
	vPtr->nsPtr = nsPtr;
#ifdef ALLOW_NAMESPACE_QUALIFIERS
	Blt_CreateNsDeleteNotify(nsPtr, (ClientData)vPtr, DestroyVectorInstCmd);
#endif /* ALLOW_NAMESPACE_QUALIFIERS */
	Tcl_SetHashValue(hPtr, (char *)vPtr);
    }
    if (cmdName != NULL) {
	Tcl_CmdInfo cmdInfo;

	if (Tcl_GetCommandInfo(interp, cmdName, &cmdInfo)) {
	    if ((ClientData)vPtr != cmdInfo.clientData) {
		Tcl_AppendResult(interp, "command \"", cmdName,
		    "\" already exists", (char *)NULL);
		goto error;
	    }
	    /* We get here only if the old name is the same as the new. */
	    goto checkVariable;
	}
    }
    if (vPtr->cmdToken != 0) {
	DeleteCommand(vPtr);	/* Command already exists, delete old first */
    }
    if (cmdName != NULL) {
	vPtr->cmdToken = Blt_CreateCommand(interp, cmdName, VectorInstCmd,
	    (ClientData)vPtr, DestroyVectorInstCmd);
    }
  checkVariable:
    if (varName != NULL) {
	if (MapVariable(interp, vPtr, varName) != TCL_OK) {
	    goto error;
	}
    }
    *newPtr = isNew;
    return vPtr;

  error:
    FreeVector((ClientData)vPtr);
    return NULL;
}

static void
GetValues(vPtr, first, last, dStrPtr)
    Vector *vPtr;
    int first, last;
    Tcl_DString *dStrPtr;
{
    register int i;
    char string[TCL_DOUBLE_SPACE + 1];

    for (i = first; i <= last; i++) {
	Tcl_PrintDouble(vPtr->interp, vPtr->valueArr[i], string);
	Tcl_DStringAppendElement(dStrPtr, string);
    }
}

static void
SetValues(vPtr, first, last, value)
    Vector *vPtr;
    int first, last;
    double value;
{
    register int i;

    /* Set possibly an entire range of values */
    for (i = first; i <= last; i++) {
	vPtr->valueArr[i] = value;
    }
    vPtr->flags |= UPDATE_LIMITS;
}

/*
 * ----------------------------------------------------------------------
 *
 * VariableProc --
 *
 * Results:
 *	Always returns NULL.  Only called from a variable trace.
 *
 * Side effects:
 *
 * ----------------------------------------------------------------------
 */
static char *
VariableProc(clientData, interp, part1, part2, flags)
    ClientData clientData;	/* File output information. */
    Tcl_Interp *interp;
    char *part1, *part2;
    int flags;
{
    Vector *vPtr = (Vector *)clientData;
    char string[TCL_DOUBLE_SPACE + 1];
    static char errorMesg[200];
    Blt_VectorIndexProc *indexProc;
    int first, last;

    if (part2 == NULL) {
	if (flags & TCL_TRACE_UNSETS) {
	    free((char *)vPtr->arrayName);
	    vPtr->arrayName = NULL;
	    vPtr->varNsPtr = NULL;
	    if (vPtr->freeOnUnset) {
		FreeVector(vPtr);
	    }
	}
	return NULL;
    }
    if (vPtr->flags & UPDATE_LIMITS) {
	UpdateLimits(vPtr);
    }
    if (GetIndex2(vPtr, part2, (ALLOW_SPECIAL | CHECK_RANGE | ALLOW_COLON),
	    &indexProc) != TCL_OK) {
	static char errMsg[200];

	strcpy(errMsg, Tcl_GetStringResult(vPtr->interp));
	return errMsg;
    }
    first = vPtr->first, last = vPtr->last;
    if (flags & TCL_TRACE_WRITES) {
	double value;
	char *newValue;

	if (first == SPECIAL_INDEX) {
	    return NULL;	/* Tried to set "min" or "max" */
	}
	newValue = Tcl_GetVar2(interp, part1, part2, 0);
	if (newValue == NULL) {
	    return "can't read current vector value";
	}
	if (Tcl_ExprDouble(interp, newValue, &value) != TCL_OK) {
	    if ((last == first) && (first >= 0)) {
		/* Single numeric index. Reset the array element to
                   its old value on errors */
		Tcl_PrintDouble(interp, vPtr->valueArr[first], string);
		Tcl_SetVar2(interp, part1, part2, string, 0);
	    }
	    return "bad value for vector element";
	}
	if (first == vPtr->numValues) {
	    if (ResizeVector(vPtr, vPtr->numValues + 1) != TCL_OK) {
		return "error resizing vector";
	    }
	}
	/* Set possibly an entire range of values */
	SetValues(vPtr, first, last, value);
    } else if (flags & TCL_TRACE_READS) {
	double value;

	if (first == vPtr->numValues) {
	    return NULL;	/* Can't read from index "++end" */
	}
	if (first == last) {
	    if (first >= 0) {
		value = vPtr->valueArr[first];
	    } else {
		vPtr->first = 0, vPtr->last = vPtr->numValues - 1;
		value = (*indexProc) ((Blt_Vector *) vPtr);
	    }
	    Tcl_PrintDouble(interp, value, string);
	    if (Tcl_SetVar2(interp, part1, part2, string, 0) == NULL) {
		sprintf(errorMesg, "error setting \"%s(%s)\" on read",
		    part1, part2);
		return errorMesg;
	    }
	} else {
	    Tcl_DString dString;
	    char *result;

	    Tcl_DStringInit(&dString);
	    GetValues(vPtr, first, last, &dString);
	    result = Tcl_SetVar2(interp, part1, part2,
		Tcl_DStringValue(&dString), 0);
	    Tcl_DStringFree(&dString);
	    if (result == NULL) {
		sprintf(errorMesg, "error setting \"%s(%s)\" on read",
		    part1, part2);
		return errorMesg;
	    }
	}
    } else if (flags & TCL_TRACE_UNSETS) {
	register int i, j;

	if ((first == vPtr->numValues) || (first == SPECIAL_INDEX)) {
	    sprintf(errorMesg,
		"can't unset \"%s(%s)\": special vector index",
		part1, part2);
	    return errorMesg;
	}
	/*
	 * Collapse the vector from the point of the first unset element.
	 * Also flush any array variable entries so that the shift is
	 * reflected when the array variable is read.
	 */
	for (i = first, j = last + 1; j < vPtr->numValues; i++, j++) {
	    vPtr->valueArr[i] = vPtr->valueArr[j];
	}
	vPtr->numValues -= ((last - first) + 1);
	FlushCache(vPtr);
	UpdateLimits(vPtr);
    } else {
	return "unknown variable flags";
    }
    if (flags & (TCL_TRACE_UNSETS | TCL_TRACE_WRITES)) {
	UpdateClients(vPtr);
    }
    return NULL;
}

/*ARGSUSED*/
static int
CopyList(vPtr, numElem, elemArr)
    Vector *vPtr;
    int numElem;
    char **elemArr;
{
    register int i;
    double value;

    if (ResizeVector(vPtr, numElem) != TCL_OK) {
	return TCL_ERROR;
    }
    for (i = 0; i < numElem; i++) {
	if (Tcl_GetDouble(vPtr->interp, elemArr[i], &value) != TCL_OK) {
	    vPtr->numValues = i;
	    return TCL_ERROR;
	}
	vPtr->valueArr[i] = value;
    }
    return TCL_OK;
}

static int
CopyVector(destPtr, srcPtr)
    Vector *destPtr, *srcPtr;
{
    int numBytes;
    int length;

    length = srcPtr->last - srcPtr->first + 1;
    if (ResizeVector(destPtr, length) != TCL_OK) {
	return TCL_ERROR;
    }
    numBytes = length * sizeof(double);
    memcpy(destPtr->valueArr, srcPtr->valueArr + srcPtr->first, numBytes);
    if (srcPtr->flags & UPDATE_LIMITS) {
	UpdateLimits(srcPtr);
    }
    UpdateLimits(destPtr);
    destPtr->offset = srcPtr->offset;
    return TCL_OK;
}

static int
AppendVector(destPtr, srcPtr)
    Vector *destPtr, *srcPtr;
{
    int numBytes;
    int oldSize, newSize;

    oldSize = destPtr->numValues;
    newSize = oldSize + srcPtr->last - srcPtr->first + 1;
    if (ResizeVector(destPtr, newSize) != TCL_OK) {
	return TCL_ERROR;
    }
    numBytes = (newSize - oldSize) * sizeof(double);
    memcpy((char *)&(destPtr->valueArr[oldSize]),
	srcPtr->valueArr + srcPtr->first, numBytes);
    destPtr->flags |= UPDATE_LIMITS;
    return TCL_OK;
}

static int
AppendList(vPtr, numElem, elemArr)
    Vector *vPtr;
    int numElem;
    char **elemArr;
{
    int count;
    register int i;
    double value;
    int oldSize;

    oldSize = vPtr->numValues;
    if (ResizeVector(vPtr, vPtr->numValues + numElem) != TCL_OK) {
	return TCL_ERROR;
    }
    count = oldSize;
    for (i = 0; i < numElem; i++) {
	if (Tcl_ExprDouble(vPtr->interp, elemArr[i], &value) != TCL_OK) {
	    vPtr->numValues = count;
	    return TCL_ERROR;
	}
	vPtr->valueArr[count++] = value;
    }
    vPtr->flags |= UPDATE_LIMITS;
    return TCL_OK;
}

static double *sortArr;		/* Pointer to the array of values currently
				 * being sorted. */
static int reverse;		/* Indicates the ordering of the sort. If
				 * non-zero, the vectors are sorted in
				 * decreasing order */

static int
CompareVector(a, b)
    void *a;
    void *b;
{
    double delta;
    int result;

    delta = sortArr[*(int *)a] - sortArr[*(int *)b];
    if (delta < 0.0) {
	result = -1;
    } else if (delta > 0.0) {
	result = 1;
    } else {
	return 0;
    }
    if (reverse) {
	result = -result;
    }
    return result;
}

static int *
SortIndex(vPtr)
    Vector *vPtr;
{
    int *indexArr;
    register int i;

    indexArr = (int *)malloc(sizeof(int) * vPtr->numValues);
    assert(indexArr);
    for (i = 0; i < vPtr->numValues; i++) {
	indexArr[i] = i;
    }
    sortArr = vPtr->valueArr;
    qsort((char *)indexArr, vPtr->numValues, sizeof(int),
	    (QSortCompareProc *)CompareVector);
    return indexArr;
}

static int
Sort(vPtr)
    Vector *vPtr;
{
    int *indexArr;
    double *tempArr;
    register int i;

    indexArr = SortIndex(vPtr);
    tempArr = (double *)malloc(sizeof(double) * vPtr->numValues);
    assert(tempArr);
    for (i = 0; i < vPtr->numValues; i++) {
	tempArr[i] = vPtr->valueArr[indexArr[i]];
    }
    free((char *)indexArr);
    if (ResizeVector(vPtr, vPtr->numValues) != TCL_OK) {
	return TCL_ERROR;
    }
    for (i = 0; i < vPtr->numValues; i++) {
	vPtr->valueArr[i] = tempArr[i];
    }
    free((char *)tempArr);
    return TCL_OK;
}

static double
Length(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    int numValues;

    numValues = vPtr->last - vPtr->first + 1;
    return (double) numValues;
}

/* ARGSUSED */
static double
Min(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    double min, max;

    FindLimits(vPtr->valueArr, vPtr->first, vPtr->last, &min, &max);
    return min;
}

static double
Max(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    double min, max;

    FindLimits(vPtr->valueArr, vPtr->first, vPtr->last, &min, &max);
    return max;
}

static double
Mean(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    register int i;
    double sum;

    sum = 0.0;
    for (i = 0; i < vPtr->numValues; i++) {
	sum += vPtr->valueArr[i];
    }
    return sum / (double)vPtr->numValues;
}

/*
 *  var = 1/N Sum( (x[i] - mean)^2 )
 */
static double
Variance(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    register double dx, var, mean;
    register int i;

    if (vPtr->numValues < 2) {
	return 0.0;
    }
    mean = Mean(vecPtr);
    var = 0.0;
    for (i = 0; i < vPtr->numValues; i++) {
	dx = vPtr->valueArr[i] - mean;
	var += dx * dx;
    }
    var /= (double)(vPtr->numValues - 1);
    return var;
}

/*
 *  skew = Sum( (x[i] - mean)^3 ) / (var^3/2)
 */
static double
Skew(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    register double diff, var, skew, mean, diffsq;
    register int i;

    if (vPtr->numValues < 2) {
	return 0.0;
    }
    mean = Mean(vecPtr);
    var = skew = 0.0;
    for (i = 0; i < vPtr->numValues; i++) {
	diff = vPtr->valueArr[i] - mean;
	diff = FABS(diff);
	diffsq = diff * diff;
	var += diffsq;
	skew += diffsq * diff;
    }
    var /= (double)(vPtr->numValues - 1);
    skew /= vPtr->numValues * var * sqrt(var);
    return skew;
}

static double
StdDeviation(vecPtr)
    Blt_Vector *vecPtr;
{
    double var;

    var = Variance(vecPtr);
    if (var > 0.0) {
	return sqrt(var);
    }
    return 0.0;
}


static double
AvgDeviation(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    register double diff, avg, mean;
    register int i;

    if (vPtr->numValues < 2) {
	return 0.0;
    }
    mean = Mean(vecPtr);
    avg = 0.0;
    for (i = 0; i < vPtr->numValues; i++) {
	diff = vPtr->valueArr[i] - mean;
	avg += FABS(diff);
    }
    avg /= (double)(vPtr->numValues);
    return avg;
}


static double
Kurtosis(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    register double diff, diffsq, kurt, var, mean;
    register int i;

    if (vPtr->numValues < 2) {
	return 0.0;
    }
    mean = Mean(vecPtr);
    var = kurt = 0.0;
    for (i = 0; i < vPtr->numValues; i++) {
	diff = vPtr->valueArr[i] - mean;
	diffsq = diff * diff;
	var += diffsq;
	kurt += diffsq * diffsq;
    }
    var /= (double)(vPtr->numValues - 1);
    if (var == 0.0) {
	return 0.0;
    }
    kurt /= (vPtr->numValues * var * var);
    return kurt - 3.0;		/* Fisher Kurtosis */
}


static double
Median(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    int *indexArr;
    double median;
    int mid;

    if (vPtr->numValues == 0) {
	return 0.0;
    }
    indexArr = SortIndex(vPtr);
    mid = (vPtr->numValues - 1) / 2;
    if (vPtr->numValues & 1) {
	/* Odd number of values, median is given */
	median = vPtr->valueArr[indexArr[mid]];
    } else {
	/* Even number of values, median is average. */
	median = (vPtr->valueArr[indexArr[mid]] +
	    vPtr->valueArr[indexArr[mid + 1]]) * 0.5;
    }
    free((char *)indexArr);
    return median;
}

static double
Q1(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    int *indexArr;
    double q1;
    int mid, quart;

    indexArr = SortIndex(vPtr);
    mid = (vPtr->numValues - 1) / 2;
    quart = mid / 2;
    if (mid & 1) {
	/* Even number of elements in bottom half [0..mid] */
	q1 = (vPtr->valueArr[indexArr[quart]] +
	    vPtr->valueArr[indexArr[quart + 1]]) * 0.5;
    } else {
	/* Odd number of elements in bottom half [0..mid], q1 is value */
	q1 = vPtr->valueArr[indexArr[quart]];
    }
    free((char *)indexArr);
    return q1;
}

static double
Q3(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    int *indexArr;
    double q3;
    int mid, quart;

    indexArr = SortIndex(vPtr);
    mid = (vPtr->numValues - 1) / 2;
    quart = ((vPtr->numValues - 1) + mid) / 2;
    if (mid & 1) {
	/* Even number of elements in upper half [mid..n-1] */
	q3 = (vPtr->valueArr[indexArr[quart]] +
	    vPtr->valueArr[indexArr[quart + 1]]) * 0.5;
    } else {
	/* Odd number of elements in upper half [mid..n-1], q1 is value */
	q3 = vPtr->valueArr[indexArr[quart]];
    }
    free((char *)indexArr);
    return q3;
}


static int
Norm(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    double norm, range, min, max;
    register int i;

    min = Min(vecPtr);
    max = Max(vecPtr);
    range = max - min;
    for (i = 0; i < vPtr->numValues; i++) {
	norm = (vPtr->valueArr[i] - min) / range;
	vPtr->valueArr[i] = norm;
    }
    return TCL_OK;
}

static double
Product(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    register int i;
    register double prod;

    prod = 1.0;
    for (i = 0; i < vPtr->numValues; i++) {
	prod *= vPtr->valueArr[i];
    }
    return prod;
}

static double
Sum(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    register int i;
    double sum;

    sum = 0.0;
    for (i = 0; i < vPtr->numValues; i++) {
	sum += vPtr->valueArr[i];
    }
    return sum;
}

static double
Nonzeros(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;
    register int i;
    int count;

    count = 0;
    for (i = 0; i < vPtr->numValues; i++) {
	if (vPtr->valueArr[i] == 0.0) {
	    count++;
	}
    }
    return (double) count;
}

static double
Fabs(value)
    double value;
{
    if (value < 0.0) {
	return -value;
    }
    return value;
}

static double
Round(value)
    double value;
{
    if (value < 0.0) {
	return ceil(value - 0.5);
    } else {
	return floor(value + 0.5);
    }
}

static double
Fmod(x, y)
    double x, y;
{
    if (y == 0.0) {
	return 0.0;
    }
    return x - Round(x / y) * y;
}

/* Vector instance option commands */

/*
 * -----------------------------------------------------------------------
 *
 * AppendOp --
 *
 *	Appends one of more Tcl lists of values, or vector objects
 *	onto the end of the current vector object.
 *
 * Results:
 *	A standard Tcl result.  If a current vector can't be created,
 *      resized, any of the named vectors can't be found, or one of
 *	lists of values is invalid, TCL_ERROR is returned.
 *
 * Side Effects:
 *	Clients of current vector will be notified of the change.
 *
 * -----------------------------------------------------------------------
 */
static int
AppendOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register int i;
    int result;
    Vector *v2Ptr;

    for (i = 2; i < argc; i++) {
	v2Ptr = FindVector(interp, argv[i], 0);
	if (v2Ptr != NULL) {
	    result = AppendVector(vPtr, v2Ptr);
	} else {
	    int numElem;
	    char **elemArr;

	    if (Tcl_SplitList(interp, argv[i], &numElem, &elemArr) != TCL_OK) {
		return TCL_ERROR;
	    }
	    result = AppendList(vPtr, numElem, elemArr);
	    free((char *)elemArr);
	}
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    if (argc > 2) {
	FlushCache(vPtr);
	UpdateClients(vPtr);
    }
    if (vPtr->flags & UPDATE_LIMITS) {
	UpdateLimits(vPtr);
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * ClearOp --
 *
 *	Deletes all the accumulated array indices for the Tcl array
 *	associated will the vector.  This routine can be used to
 *	free excess memory from a large vector.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 * Side Effects:
 *	Memory used for the entries of the Tcl array variable is freed.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ClearOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FlushCache(vPtr);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * DeleteOp --
 *
 *	Deletes the given indices from the vector.  If no indices are
 *	provided the entire vector is deleted.
 *
 * Results:
 *	A standard Tcl result.  If any of the given indices is invalid,
 *	interp->result will an error message and TCL_ERROR is returned.
 *
 * Side Effects:
 *	The clients of the vector will be notified of the vector
 *	deletions.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DeleteOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    unsigned char *unsetArr;
    register int i, j;
    register int count;

    if (argc == 2) {
	FreeVector(vPtr);
	return TCL_OK;
    }
    /*
     * Allocate an "unset" bitmap the size of the vector.  We should
     * try to use bit fields instead of a character array, since
     * memory may be an issue if the vector is large.
     */
    unsetArr = (unsigned char *)calloc(sizeof(unsigned char), vPtr->numValues);
    assert(unsetArr);
    for (i = 2; i < argc; i++) {
	if (GetIndex2(vPtr, argv[i], (ALLOW_COLON | CHECK_RANGE),
		(Blt_VectorIndexProc **) NULL) != TCL_OK) {
	    free((char *)unsetArr);
	    return TCL_ERROR;
	}
	for (j = vPtr->first; j <= vPtr->last; j++) {
	    unsetArr[j] = TRUE;
	}
    }
    count = 0;
    for (i = 0; i < vPtr->numValues; i++) {
	if (unsetArr[i]) {
	    continue;
	}
	if (count < i) {
	    vPtr->valueArr[count] = vPtr->valueArr[i];
	}
	count++;
    }
    free((char *)unsetArr);
    vPtr->numValues = count;
    FlushCache(vPtr);
    UpdateClients(vPtr);
    UpdateLimits(vPtr);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * DupOp --
 *
 *	Creates one or more duplicates of the vector object.
 *
 * Results:
 *	A standard Tcl result.  If a new vector can't be created,
 *      or and existing vector resized, TCL_ERROR is returned.
 *
 * Side Effects:
 *	Clients of existing vectors will be notified of the change.
 *
 * -----------------------------------------------------------------------
 */
static int
DupOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Vector *v2Ptr;
    int isNew;
    register int i;

    for (i = 2; i < argc; i++) {
	v2Ptr = CreateVector(interp, argv[i], argv[i], argv[i], &isNew);
	if (v2Ptr == NULL) {
	    return TCL_ERROR;
	}
	if (v2Ptr == vPtr) {
	    continue;
	}
	if (CopyVector(v2Ptr, vPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (!isNew) {
	    FlushCache(vPtr);
	    UpdateClients(v2Ptr);
	}
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * IndexOp --
 *
 *	Sets or reads the value of the index.  This simulates what the
 *	vector's variable does.
 *
 * Results:
 *	A standard Tcl result.  If the index is invalid,
 *	interp->result will an error message and TCL_ERROR is returned.
 *	Otherwise interp->result will contain the values.
 *
 * -----------------------------------------------------------------------
 */
static int
IndexOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int first, last;

    if (GetIndex2(vPtr, argv[2], (ALLOW_SPECIAL | CHECK_RANGE | ALLOW_COLON),
	    (Blt_VectorIndexProc **) NULL) != TCL_OK) {
	return TCL_ERROR;
    }
    first = vPtr->first, last = vPtr->last;
    if (argc == 3) {
	Tcl_DString dString;

	if (first == vPtr->numValues) {
	    Tcl_AppendResult(interp, "can't get index \"", argv[2], "\"",
		(char *)NULL);
	    return TCL_ERROR;	/* Can't read from index "++end" */
	}
	Tcl_DStringInit(&dString);
	GetValues(vPtr, first, last, &dString);
	Tcl_DStringResult(interp, &dString);
	Tcl_DStringFree(&dString);
    } else {
	char string[TCL_DOUBLE_SPACE + 1];
	double value;

	if (first == SPECIAL_INDEX) {
	    Tcl_AppendResult(interp, "can't set index \"", argv[2], "\"",
		(char *)NULL);
	    return TCL_ERROR;	/* Tried to set "min" or "max" */
	}
	if (Tcl_ExprDouble(interp, argv[3], &value) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (first == vPtr->numValues) {
	    if (ResizeVector(vPtr, vPtr->numValues + 1) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
	SetValues(vPtr, first, last, value);
	Tcl_PrintDouble(interp, value, string);
	Tcl_SetResult(interp, string, TCL_VOLATILE);
	FlushCache(vPtr);
	UpdateClients(vPtr);
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * LengthOp --
 *
 *	Returns the length of the vector.  If a new size is given, the
 *	vector is resized to the new vector.
 *
 * Results:
 *	A standard Tcl result.  If the new length is invalid,
 *	interp->result will an error message and TCL_ERROR is returned.
 *	Otherwise interp->result will contain the length of the vector.
 *
 * -----------------------------------------------------------------------
 */
static int
LengthOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (argc == 3) {
	int size;

	if (Tcl_GetInt(interp, argv[2], &size) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (size < 0) {
	    Tcl_AppendResult(interp, "bad vector size \"", argv[3], "\"",
		(char *)NULL);
	    return TCL_ERROR;
	}
	if (ResizeVector(vPtr, size) != TCL_OK) {
	    return TCL_ERROR;
	}
	FlushCache(vPtr);
	UpdateClients(vPtr);
    }
    Tcl_SetResult(interp, Blt_Int(vPtr->numValues), TCL_VOLATILE);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * MapOp --
 *
 *	Queries or sets the offset of the array index from the base
 *	address of the data array of values.
 *
 * Results:
 *	A standard Tcl result.  If the source vector doesn't exist
 *	or the source list is not a valid list of numbers, TCL_ERROR
 *	returned.  Otherwise TCL_OK is returned.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
MapOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{

    if (MapVariable(interp, vPtr, argv[2]) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, vPtr->arrayName, TCL_STATIC);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * MergeOp --
 *
 *	Merges the values from the given vectors to the current vector.
 *
 * Results:
 *	A standard Tcl result.  If any of the given vectors differ in size,
 *	TCL_ERROR is returned.  Otherwise TCL_OK is returned and the
 *	vector data will contain merged values of the given vectors.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
MergeOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Vector *v2Ptr;
    Vector **vecArr;
    register Vector **vPtrPtr;
    int refSize, length;
    register int i;

    /*
     * Allocate an array of vector pointers of each vector
     * to be merged in the current vector.
     */
    vecArr = (Vector **)malloc(sizeof(Vector *) * argc);
    assert(vecArr);
    vPtrPtr = vecArr;
    *vPtrPtr = vPtr;		/* Initialize the list with the first vector */
    vPtrPtr++;

    refSize = vPtr->numValues;
    for (i = 2; i < argc; i++) {
	v2Ptr = FindVector(interp, argv[i], TCL_LEAVE_ERR_MSG);
	if (v2Ptr == NULL) {
	    free((char *)vecArr);
	    return TCL_ERROR;
	}
	/* Check that all the vectors are the same length */
	length = v2Ptr->last - v2Ptr->first + 1;
	if (length != refSize) {
	    Tcl_AppendResult(vPtr->interp, "vectors \"", vPtr->nameId,
		"\" and \"", v2Ptr->nameId, "\" differ in length",
		(char *)NULL);
	    free((char *)vecArr);
	    return TCL_ERROR;
	}
	*vPtrPtr = v2Ptr;
	vPtrPtr++;
    }
    *vPtrPtr = NULL;

    /* Merge the values from each of the vectors into the current vector */
    for (i = 0; i < refSize; i++) {
	for (vPtrPtr = vecArr; *vPtrPtr != NULL; vPtrPtr++) {
	    Tcl_AppendElement(interp,
		Blt_Double(interp, (*vPtrPtr)->valueArr[i + (*vPtrPtr)->first]));
	}
    }
    free((char *)vecArr);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * NormalizeOp --
 *
 *	Normalizes the vector.
 *
 * Results:
 *	A standard Tcl result.  If the density is invalid, TCL_ERROR
 *	is returned.  Otherwise TCL_OK is returned.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
NormalizeOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register int i;
    double range;

    range = vPtr->max - vPtr->min;
    if (argc > 2) {
	Vector *v2Ptr;
	int isNew;

	v2Ptr = CreateVector(interp, argv[2], argv[2], argv[2], &isNew);
	if (v2Ptr == NULL) {
	    return TCL_ERROR;
	}
	if (ResizeVector(v2Ptr, vPtr->numValues) != TCL_OK) {
	    return TCL_ERROR;
	}
	for (i = 0; i < vPtr->numValues; i++) {
	    v2Ptr->valueArr[i] = (vPtr->valueArr[i] - vPtr->min) / range;
	}
	UpdateLimits(v2Ptr);
	if (!isNew) {
	    FlushCache(vPtr);
	    UpdateClients(v2Ptr);
	}
    } else {
	double norm;

	for (i = 0; i < vPtr->numValues; i++) {
	    norm = (vPtr->valueArr[i] - vPtr->min) / range;
	    Tcl_AppendElement(interp, Blt_Double(interp, norm));
	}
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * NotifyOp --
 *
 *	Notify clients of vector.
 *
 * Results:
 *	A standard Tcl result.  If any of the given vectors differ in size,
 *	TCL_ERROR is returned.  Otherwise TCL_OK is returned and the
 *	vector data will contain merged values of the given vectors.
 *
 *  x vector notify now
 *  x vector notify always
 *  x vector notify whenidle
 *  x vector notify update {}
 *  x vector notify delete {}
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
NotifyOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    char c;
    int length;

    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 'a') && (length > 1)
	&& (strncmp(argv[2], "always", length) == 0)) {
	vPtr->flags &= ~NOTIFY_WHEN_MASK;
	vPtr->flags |= NOTIFY_ALWAYS;
    } else if ((c == 'n') && (length > 2)
	&& (strncmp(argv[2], "never", length) == 0)) {
	vPtr->flags &= ~NOTIFY_WHEN_MASK;
	vPtr->flags |= NOTIFY_NEVER;
    } else if ((c == 'w') && (length > 1)
	&& (strncmp(argv[2], "whenidle", length) == 0)) {
	vPtr->flags &= ~NOTIFY_WHEN_MASK;
	vPtr->flags |= NOTIFY_WHENIDLE;
    } else if ((c == 'n') && (length > 2)
	&& (strncmp(argv[2], "now", length) == 0)) {
	/* How does this play when an update is pending? */
	NotifyClients(vPtr);
    } else if ((c == 'c') && (length > 1)
	&& (strncmp(argv[2], "cancel", length) == 0)) {
	if (vPtr->flags & NOTIFY_PENDING) {
	    vPtr->flags &= ~NOTIFY_PENDING;
	    Tk_CancelIdleCall(NotifyClients, (ClientData)vPtr);
	}
    } else if ((c == 'p') && (length > 1)
	&& (strncmp(argv[2], "pending", length) == 0)) {
	Tcl_SetResult(interp, (vPtr->flags & NOTIFY_PENDING) ? "1" : "0",
	    TCL_STATIC);
    } else {
	Tcl_AppendResult(interp, "bad qualifier \"", argv[2], "\": should be \
\"always\", \"never\", \"whenidle\", \"now\", \"cancel\", or \"pending\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * PopulateOp --
 *
 *	Creates or resizes a new vector based upon the density specified.
 *
 * Results:
 *	A standard Tcl result.  If the density is invalid, TCL_ERROR
 *	is returned.  Otherwise TCL_OK is returned.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
PopulateOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Vector *v2Ptr;
    int size, density;
    int isNew;
    register int i, j;
    double slice, range;
    register double *valuePtr;
    int count;

    v2Ptr = CreateVector(interp, argv[2], argv[2], argv[2], &isNew);
    if (v2Ptr == NULL) {
	return TCL_ERROR;
    }
    if (vPtr->numValues == 0) {
	return TCL_OK;		/* Source vector is empty. */
    }
    if (Tcl_GetInt(interp, argv[3], &density) != TCL_OK) {
	return TCL_ERROR;
    }
    if (density < 1) {
	Tcl_AppendResult(interp, "bad density \"", argv[3], "\"", (char *)NULL);
	return TCL_ERROR;
    }
    size = (vPtr->numValues - 1) * (density + 1) + 1;
    if (ResizeVector(v2Ptr, size) != TCL_OK) {
	return TCL_ERROR;
    }
    count = 0;
    valuePtr = v2Ptr->valueArr;
    for (i = 0; i < (vPtr->numValues - 1); i++) {
	range = vPtr->valueArr[i + 1] - vPtr->valueArr[i];
	slice = range / (double)(density + 1);
	for (j = 0; j <= density; j++) {
	    *valuePtr = vPtr->valueArr[i] + (slice * (double)j);
	    valuePtr++;
	    count++;
	}
    }
    count++;
    *valuePtr = vPtr->valueArr[i];
    assert(count == v2Ptr->numValues);
    UpdateLimits(v2Ptr);
    if (!isNew) {
	FlushCache(v2Ptr);
	UpdateClients(v2Ptr);
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * RangeOp --
 *
 *	Returns a Tcl list of the range of vector values specified.
 *
 * Results:
 *	A standard Tcl result.  If the given range is invalid, TCL_ERROR
 *	is returned.  Otherwise TCL_OK is returned and interp->result
 *	will contain the list of values.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
RangeOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    int first, last;
    register int i;

    if ((GetIndex(vPtr, argv[2], &first, CHECK_RANGE,
		(Blt_VectorIndexProc **) NULL) != TCL_OK) ||
	(GetIndex(vPtr, argv[3], &last, CHECK_RANGE,
		(Blt_VectorIndexProc **) NULL) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (first > last) {
	/* Return the list reversed */
	for (i = last; i <= first; i++) {
	    Tcl_AppendElement(interp, Blt_Double(interp, vPtr->valueArr[i]));
	}
    } else {
	for (i = first; i <= last; i++) {
	    Tcl_AppendElement(interp, Blt_Double(interp, vPtr->valueArr[i]));
	}
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * OutOfRange --
 *
 *	Determines if a value does not lie within a given range.
 *
 *	The value is normalized and compared against the interval
 *	[0..1], where 0.0 is the minimum and 1.0 is the maximum.
 *	DBL_EPSILON is the smallest number that can be represented
 *	on the host machine, such that (1.0 + epsilon) != 1.0.
 *
 *	Please note, *max* can't equal *min*.
 *
 * Results:
 *	Returns whether the value lies outside of the given range.
 *	If value is outside of the interval [min..max], 1 is returned;
 *	0 otherwise.
 *
 * ----------------------------------------------------------------------
 */
INLINE static int
OutOfRange(value, min, max)
    register double value, min, max;
{
    register double norm;

    norm = (value - min) / (max - min);
    return (((norm - 1.0) > DBL_EPSILON) || (((1.0 - norm) - 1.0) > DBL_EPSILON));
}

/*
 * -----------------------------------------------------------------------
 *
 * SearchOp --
 *
 *	Searchs for a value in the vector. Returns the indices of all
 *	vector elements matching a particular value.
 *
 * Results:
 *	Always returns TCL_OK.  interp->result will contain a list
 *	of the indices of array elements matching value. If no elements
 *	match, interp->result will contain the empty string.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
SearchOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    double min, max;
    register int i;
    int needValue = 0;

    if ((argv[2][0] == '-') && (strcmp(argv[2], "-value") == 0)) {
	needValue++;
	argv++, argc--;
    }
    if (Tcl_ExprDouble(interp, argv[2], &min) != TCL_OK) {
	return TCL_ERROR;
    }
    max = min;
    if ((argc > 3) && (Tcl_ExprDouble(interp, argv[3], &max) != TCL_OK)) {
	return TCL_ERROR;
    }
#ifdef notdef
    if (min > max) {
	double temp;

	temp = max, max = min, min = temp;
    }
#endif
    if (min != max) {
	for (i = 0; i < vPtr->numValues; i++) {
	    if (!OutOfRange(vPtr->valueArr[i], min, max)) {
		if (needValue) {
		    Tcl_AppendElement(interp,
			Blt_Double(interp, vPtr->valueArr[i + vPtr->offset]));
		} else {
		    Tcl_AppendElement(interp, Blt_Int(i + vPtr->offset));
		}
	    }
	}
    } else {
	for (i = 0; i < vPtr->numValues; i++) {
	    if (vPtr->valueArr[i] == min) {
		if (needValue) {
		    Tcl_AppendElement(interp,
			Blt_Double(interp, vPtr->valueArr[i + vPtr->offset]));
		} else {
		    Tcl_AppendElement(interp, Blt_Int(i + vPtr->offset));
		}
	    }
	}
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * OffsetOp --
 *
 *	Queries or sets the offset of the array index from the base
 *	address of the data array of values.
 *
 * Results:
 *	A standard Tcl result.  If the source vector doesn't exist
 *	or the source list is not a valid list of numbers, TCL_ERROR
 *	returned.  Otherwise TCL_OK is returned.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
OffsetOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    if (argc == 3) {
	int newOffset;

	if (Tcl_GetInt(interp, argv[2], &newOffset) != TCL_OK) {
	    return TCL_ERROR;
	}
	vPtr->offset = newOffset;
    }
    Tcl_SetResult(interp, Blt_Int(vPtr->offset), TCL_VOLATILE);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * RandomOp --
 *
 *	Generates random values for the length of the vector.
 *
 * Results:
 *	A standard Tcl result.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
RandomOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
#ifdef HAVE_DRAND48
    register int i;

    for (i = 0; i < vPtr->numValues; i++) {
	vPtr->valueArr[i] = drand48();
    }
#endif /* HAVE_DRAND48 */
    FlushCache(vPtr);
    UpdateLimits(vPtr);
    UpdateClients(vPtr);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * SequenceOp --
 *
 *	Generates a sequence of values in the vector.
 *
 * Results:
 *	A standard Tcl result.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
SequenceOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    register int i;
    double start, finish, step;
    int fillVector;
    int numSteps;

    if (Tcl_ExprDouble(interp, argv[2], &start) != TCL_OK) {
	return TCL_ERROR;
    }
    fillVector = FALSE;
    if ((argv[3][0] == 'e') && (strcmp(argv[3], "end") == 0)) {
	fillVector = TRUE;
    } else if (Tcl_ExprDouble(interp, argv[3], &finish) != TCL_OK) {
	return TCL_ERROR;
    }
    step = 1.0;
    if ((argc > 4) && (Tcl_ExprDouble(interp, argv[4], &step) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (fillVector) {
	numSteps = vPtr->numValues;
    } else {
	numSteps = (int)((finish - start) / step) + 1;
    }
    if (numSteps > 0) {
	if (ResizeVector(vPtr, numSteps) != TCL_OK) {
	    return TCL_ERROR;
	}
	for (i = 0; i < numSteps; i++) {
	    vPtr->valueArr[i] = start + (step * (double)i);
	}
	UpdateLimits(vPtr);
	FlushCache(vPtr);
	UpdateClients(vPtr);
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * SetOp --
 *
 *	Sets the data of the vector object from a list of values.
 *
 * Results:
 *	A standard Tcl result.  If the source vector doesn't exist
 *	or the source list is not a valid list of numbers, TCL_ERROR
 *	returned.  Otherwise TCL_OK is returned.
 *
 * Side Effects:
 *	The vector data is reset.  Clients of the vector are notified.
 *	Any cached array indices are flushed.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
SetOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    int result;
    Vector *v2Ptr;
    int numElem;
    char **elemArr;

    /*
     * The source can be either a list of expressions of another vector.
     */
    if (Tcl_SplitList(interp, argv[2], &numElem, &elemArr) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * If there's only one element, check to see whether it's the name
     * of a vector.  Otherwise, treat it as a single numeric expression.
     */
    if ((numElem == 1) && ((v2Ptr = FindVector(interp, argv[2], 0)) != NULL)) {
	result = CopyVector(vPtr, v2Ptr);
    } else {
	result = CopyList(vPtr, numElem, elemArr);
    }
    free((char *)elemArr);

    if (result == TCL_OK) {
	/*
	 * The vector has changed; so flush the array indices (they're
	 * wrong now), find the new limits of the data, and notify
	 * the vector's clients that it's been modified.
	 */
	FlushCache(vPtr);
	UpdateLimits(vPtr);
	UpdateClients(vPtr);
    }
    return result;
}

/*
 * -----------------------------------------------------------------------
 *
 * SortOp --
 *
 *	Sorts the vector object and any other vectors according to
 *	sorting order of the vector object.
 *
 * Results:
 *	A standard Tcl result.  If any of the auxiliary vectors are
 *	a different size than the sorted vector object, TCL_ERROR is
 *	returned.  Otherwise TCL_OK is returned.
 *
 * Side Effects:
 *	The vectors are sorted.
 *
 * -----------------------------------------------------------------------
 */

static int
SortOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int *indexArr;
    double *mergeArr;
    Vector *v2Ptr;
    int refSize, numBytes;
    int result;
    register int i, n;

    reverse = FALSE;
    if ((argc > 2) && (argv[2][0] == '-')) {
	int length;

	length = strlen(argv[2]);
	if ((length > 1) && (strncmp(argv[2], "-reverse", length) == 0)) {
	    reverse = TRUE;
	} else {
	    Tcl_AppendResult(interp, "unknown flag \"", argv[2],
		"\": should be \"-reverse\"", (char *)NULL);
	    return TCL_ERROR;
	}
	argc--, argv++;
    }
    indexArr = SortIndex(vPtr);

    refSize = vPtr->numValues;

    /*
     * Create an array to store a copy of the current values of the
     * vector. We'll merge the values back into the vector based upon
     * the indices found in the index array.
     */
    numBytes = sizeof(double) * refSize;
    mergeArr = (double *)malloc(numBytes);
    assert(mergeArr);
    memcpy((char *)mergeArr, (char *)vPtr->valueArr, numBytes);
    for (n = 0; n < refSize; n++) {
	vPtr->valueArr[n] = mergeArr[indexArr[n]];
    }
    FlushCache(vPtr);
    UpdateClients(vPtr);

    /*
     * Now sort any other vectors in the same fashion.  The vectors
     * must be the same size as the indexArr though.
     */
    result = TCL_ERROR;
    for (i = 2; i < argc; i++) {
	v2Ptr = FindVector(interp, argv[i], TCL_LEAVE_ERR_MSG);
	if (v2Ptr == NULL) {
	    goto error;
	}
	if (v2Ptr->numValues != refSize) {
	    Tcl_AppendResult(interp, "vector \"", v2Ptr->nameId,
		"\" is not the same size as \"", vPtr->nameId, "\"",
		(char *)NULL);
	    goto error;
	}
	memcpy((char *)mergeArr, (char *)v2Ptr->valueArr, numBytes);
	for (n = 0; n < refSize; n++) {
	    v2Ptr->valueArr[n] = mergeArr[indexArr[n]];
	}
	UpdateClients(v2Ptr);
	FlushCache(v2Ptr);
    }
    result = TCL_OK;
  error:
    free((char *)mergeArr);
    free((char *)indexArr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * InstExprOp --
 *
 *	Computes the result of the expression which may be
 *	either a scalar (single value) or vector (list of values).
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
InstExprOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (Blt_ExprVector(interp, argv[2], (Blt_Vector *) vPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    FlushCache(vPtr);
    UpdateLimits(vPtr);
    UpdateClients(vPtr);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * ArithOp --
 *
 * Results:
 *	A standard Tcl result.  If the source vector doesn't exist
 *	or the source list is not a valid list of numbers, TCL_ERROR
 *	returned.  Otherwise TCL_OK is returned.
 *
 * Side Effects:
 *	The vector data is reset.  Clients of the vector are notified.
 *	Any cached array indices are flushed.
 *
 * -----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ArithOp(vPtr, interp, argc, argv)
    Vector *vPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    register double value;
    register int i;
    Vector *v2Ptr;

    v2Ptr = FindVector(interp, argv[2], 0);
    if (v2Ptr != NULL) {
	register int j;
	int length;

	length = v2Ptr->last - v2Ptr->first + 1;
	if (length != vPtr->numValues) {
	    Tcl_AppendResult(interp, "vectors \"", argv[0], "\" and \"",
		argv[2], "\" are not the same length", (char *)NULL);
	    return TCL_ERROR;
	}
	switch (argv[1][0]) {
	case '*':
	    for (i = 0, j = v2Ptr->first; i < vPtr->numValues; i++, j++) {
		value = vPtr->valueArr[i] * v2Ptr->valueArr[j];
		Tcl_AppendElement(interp, Blt_Double(interp, value));
	    }
	    break;

	case '/':
	    for (i = 0, j = v2Ptr->first; i < vPtr->numValues; i++, j++) {
		value = vPtr->valueArr[i] / v2Ptr->valueArr[j];
		Tcl_AppendElement(interp, Blt_Double(interp, value));
	    }
	    break;

	case '-':
	    for (i = 0, j = v2Ptr->first; i < vPtr->numValues; i++, j++) {
		value = vPtr->valueArr[i] - v2Ptr->valueArr[j];
		Tcl_AppendElement(interp, Blt_Double(interp, value));
	    }
	    break;

	case '+':
	    for (i = 0, j = v2Ptr->first; i < vPtr->numValues; i++, j++) {
		value = vPtr->valueArr[i] + v2Ptr->valueArr[j];
		Tcl_AppendElement(interp, Blt_Double(interp, value));
	    }
	    break;
	}
    } else {
	double scalar;

	if (Tcl_ExprDouble(interp, argv[2], &scalar) != TCL_OK) {
	    return TCL_ERROR;
	}
	switch (argv[1][0]) {
	case '*':
	    for (i = 0; i < vPtr->numValues; i++) {
		value = vPtr->valueArr[i] * scalar;
		Tcl_AppendElement(interp, Blt_Double(interp, value));
	    }
	    break;

	case '/':
	    for (i = 0; i < vPtr->numValues; i++) {
		value = vPtr->valueArr[i] / scalar;
		Tcl_AppendElement(interp, Blt_Double(interp, value));
	    }
	    break;

	case '-':
	    for (i = 0; i < vPtr->numValues; i++) {
		value = vPtr->valueArr[i] - scalar;
		Tcl_AppendElement(interp, Blt_Double(interp, value));
	    }
	    break;

	case '+':
	    for (i = 0; i < vPtr->numValues; i++) {
		value = vPtr->valueArr[i] + scalar;
		Tcl_AppendElement(interp, Blt_Double(interp, value));
	    }
	    break;
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * VectorInstCmd --
 *
 *	Parses and invokes the appropriate vector instance command
 *	option.
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static Blt_OpSpec vectorInstOps[] =
{
    {"*", 1, (Blt_Operation)ArithOp, 3, 3, "item",},	/*Deprecated*/
    {"+", 1, (Blt_Operation)ArithOp, 3, 3, "item",},	/*Deprecated*/
    {"-", 1, (Blt_Operation)ArithOp, 3, 3, "item",},	/*Deprecated*/
    {"/", 1, (Blt_Operation)ArithOp, 3, 3, "item",},	/*Deprecated*/
    {"append", 1, (Blt_Operation)AppendOp, 3, 0, "item ?item...?",},
    {"clear", 1, (Blt_Operation)ClearOp, 2, 2, "",},
    {"delete", 2, (Blt_Operation)DeleteOp, 2, 0, "index ?index...?",},
    {"dup", 2, (Blt_Operation)DupOp, 3, 0, "vecName",},
    {"expr", 1, (Blt_Operation)InstExprOp, 3, 3, "expression",},
    {"index", 1, (Blt_Operation)IndexOp, 3, 4, "index ?value?",},
    {"length", 1, (Blt_Operation)LengthOp, 2, 3, "?newSize?",},
    {"merge", 1, (Blt_Operation)MergeOp, 3, 0, "vecName ?vecName...?",},
    {"normalize", 3, (Blt_Operation)NormalizeOp, 2, 3, "?vecName?",},	/*Deprecated*/
    {"notify", 3, (Blt_Operation)NotifyOp, 3, 3, "keyword",},
    {"offset", 2, (Blt_Operation)OffsetOp, 2, 3, "?offset?",},
    {"populate", 1, (Blt_Operation)PopulateOp, 4, 4, "vecName density",},
    {"random", 4, (Blt_Operation)RandomOp, 2, 2, "",},	/*Deprecated*/
    {"range", 4, (Blt_Operation)RangeOp, 4, 4, "first last",},
    {"search", 3, (Blt_Operation)SearchOp, 3, 4, "?-value? value ?value?",},
    {"seq", 3, (Blt_Operation)SequenceOp, 4, 5, "start end ?step?",},
    {"set", 3, (Blt_Operation)SetOp, 3, 3, "list",},
    {"sort", 2, (Blt_Operation)SortOp, 2, 0, "?-reverse? ?vecName...?",},
    {"variable", 1, (Blt_Operation)MapOp, 3, 3, "varName",},
};

static int numInstOps = sizeof(vectorInstOps) / sizeof(Blt_OpSpec);

static int
VectorInstCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;
    Vector *vPtr = (Vector *)clientData;

    vPtr->first = 0;
    vPtr->last = vPtr->numValues - 1;
    if (vPtr->flags & UPDATE_LIMITS) {
	UpdateLimits(vPtr);
    }
    proc = Blt_GetOperation(interp, numInstOps, vectorInstOps, BLT_OPER_ARG1,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    return (*proc) (vPtr, interp, argc, argv);
}

/*
 *----------------------------------------------------------------------
 *
 * VectorNamesOp --
 *
 *	Reports the names of all the current vectors in the interpreter.
 *
 * Results:
 *	A standard Tcl result.  interp->result will contain a list of
 *	all the names of the vector instances.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
VectorNamesOp(clientData, interp, argc, argv)
    ClientData clientData;	/* Unused */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Vector *vPtr;
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    register int i;

    /* No arguments. List the names of all vectors */

    for (hPtr = Tcl_FirstHashEntry(&vectorTable, &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	vPtr = (Vector *)Tcl_GetHashValue(hPtr);
	if (argc == 2) {
	    Tcl_AppendElement(interp, vPtr->nameId);
	    continue;
	}
	for (i = 2; i < argc; i++) {
	    if (Tcl_StringMatch(vPtr->nameId, argv[i])) {
		Tcl_AppendElement(interp, vPtr->nameId);
		break;
	    }
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * VectorCreateOp --
 *
 *	Creates a Tcl command, and array variable representing an
 *	instance of a vector.
 *
 *	vector a
 *	vector b(20)
 *	vector c(-5:14)
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
VectorCreateOp(clientData, interp, argc, argv)
    ClientData clientData;	/* Unused */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int argStart = (int)clientData;
    Vector *vPtr;
    char *leftParen, *rightParen;
    int isNew, size, first, last;
    char *cmdName, *varName;
    int length;
    int inspectFlags, freeOnUnset;
    char **nameArr;
    int count;
    register int i;

    /*
     * Handle switches to the vector command and collect the vector
     * name arguments into an array.
     */
    varName = cmdName = NULL;
    freeOnUnset = 0;
    nameArr = (char **)malloc(sizeof(char *) * argc);
    assert(nameArr);

    inspectFlags = TRUE;
    count = 0;
    vPtr = NULL;
    for (i = argStart; i < argc; i++) {
	if ((inspectFlags) && (argv[i][0] == '-')) {
	    length = strlen(argv[i]);
	    if ((length > 1) &&
		(strncmp(argv[i], "-variable", length) == 0)) {
		if ((i + 1) == argc) {
		    Tcl_AppendResult(interp,
			"no variable name supplied with \"",
			argv[i], "\" switch", (char *)NULL);
		    goto error;
		}
		i++;
		varName = argv[i];
	    } else if ((length > 1) &&
		(strncmp(argv[i], "-command", length) == 0)) {
		if ((i + 1) == argc) {
		    Tcl_AppendResult(interp,
			"no command name supplied with \"",
			argv[i], "\" switch", (char *)NULL);
		    goto error;
		}
		i++;
		cmdName = argv[i];
	    } else if ((length > 1) &&
		(strncmp(argv[i], "-watchunset", length) == 0)) {
		int bool;

		if ((i + 1) == argc) {
		    Tcl_AppendResult(interp, "no value name supplied with \"",
			argv[i], "\" switch", (char *)NULL);
		    goto error;
		}
		i++;
		if (Tcl_GetBoolean(interp, argv[i], &bool) != TCL_OK) {
		    goto error;
		}
		freeOnUnset = bool;
	    } else if ((length > 1) && (argv[i][1] == '-') &&
		(argv[i][2] == '\0')) {
		inspectFlags = FALSE;	/* Allow vector names to start with - */
	    } else {
		Tcl_AppendResult(interp, "bad vector switch \"", argv[i], "\"",
		    (char *)NULL);
		goto error;
	    }
	} else {
	    nameArr[count++] = argv[i];
	}
    }
    if (count == 0) {
	Tcl_AppendResult(interp, "no vector names supplied", (char *)NULL);
	goto error;
    }
    if (count > 1) {
	if ((cmdName != NULL) && (cmdName[0] != '\0')) {
	    Tcl_AppendResult(interp,
		"can't specify more than one vector with \"-command\" switch",
		(char *)NULL);
	    goto error;
	}
	if ((varName != NULL) && (varName[0] != '\0')) {
	    Tcl_AppendResult(interp,
		"can't specify more than one vector with \"-variable\" switch",
		(char *)NULL);
	    goto error;
	}
    }
    for (i = 0; i < count; i++) {
	size = first = last = 0;
	leftParen = strchr(nameArr[i], '(');
	rightParen = strchr(nameArr[i], ')');
	if (((leftParen != NULL) && (rightParen == NULL)) ||
	    ((leftParen == NULL) && (rightParen != NULL)) ||
	    (leftParen > rightParen)) {
	    Tcl_AppendResult(interp, "bad vector specification \"", nameArr[i],
		"\"", (char *)NULL);
	    goto error;
	}
	if (leftParen != NULL) {
	    int result;
	    char *colon;

	    *rightParen = '\0';
	    colon = strchr(leftParen + 1, ':');
	    if (colon != NULL) {

		/* Specification is in the form vecName(first:last) */
		*colon = '\0';
		result = Tcl_GetInt(interp, leftParen + 1, &first);
		if ((*(colon + 1) != '\0') && (result == TCL_OK)) {
		    result = Tcl_GetInt(interp, colon + 1, &last);
		    if (first > last) {
			Tcl_AppendResult(interp, "bad vector range \"",
			    nameArr[i], "\"", (char *)NULL);
			result = TCL_ERROR;
		    }
		    size = (last - first) + 1;
		}
		*colon = ':';
	    } else {
		/* Specification is in the form vecName(size) */
		result = Tcl_GetInt(interp, leftParen + 1, &size);
	    }
	    *rightParen = ')';
	    if (result != TCL_OK) {
		goto error;
	    }
	    if (size < 0) {
		Tcl_AppendResult(interp, "bad vector size \"", nameArr[i], "\"",
		    (char *)NULL);
		goto error;
	    }
	}
	if (leftParen != NULL) {
	    *leftParen = '\0';
	}
	/*
	 * By default, we create a Tcl command by the name of the vector.
	 */
	vPtr = CreateVector(interp, nameArr[i],
	    (cmdName == NULL) ? nameArr[i] : cmdName,
	    (varName == NULL) ? nameArr[i] : varName,
	    &isNew);
	if (leftParen != NULL) {
	    *leftParen = '(';
	}
	if (vPtr == NULL) {
	    goto error;
	}
	vPtr->freeOnUnset = freeOnUnset;
	vPtr->offset = first;
	if (size > 0) {
	    if (ResizeVector(vPtr, size) != TCL_OK) {
		goto error;
	    }
	}
	if (!isNew) {
	    FlushCache(vPtr);
	    UpdateClients(vPtr);
	}
    }
    free((char *)nameArr);
    if (vPtr != NULL) {
	/* Return the name of the last vector created  */
	Tcl_SetResult(interp, vPtr->nameId, TCL_STATIC);
    }
    return TCL_OK;
  error:
    free((char *)nameArr);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * VectorDestroyOp --
 *
 *	Destroys the vector and its related Tcl command and array
 *	variable (if they exist).
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Deletes the vector.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
VectorDestroyOp(clientData, interp, argc, argv)
    ClientData clientData;	/* Unused */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Vector *vPtr;
    register int i;

    for (i = 2; i < argc; i++) {
	vPtr = FindVector(interp, argv[i], TCL_LEAVE_ERR_MSG);
	if (vPtr == NULL) {
	    return TCL_ERROR;
	}
	FreeVector((ClientData)vPtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * VectorExprOp --
 *
 *	Computes the result of the expression which may be
 *	either a scalar (single value) or vector (list of values).
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
VectorExprOp(clientData, interp, argc, argv)
    ClientData clientData;	/* Unused */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    return Blt_ExprVector(interp, argv[2], (Blt_Vector *) NULL);
}

static Blt_OpSpec vectorCmdOps[] =
{
    {"create", 1, (Blt_Operation)VectorCreateOp, 3, 0,
	"vecName ?vecName...? ?switches...?",},
    {"destroy", 1, (Blt_Operation)VectorDestroyOp, 3, 0,
	"vecName ?vecName...?",},
    {"expr", 1, (Blt_Operation)VectorExprOp, 3, 3, "expression",},
    {"names", 1, (Blt_Operation)VectorNamesOp, 2, 3, "?pattern?...",},
};

static int numCmdOps = sizeof(vectorCmdOps) / sizeof(Blt_OpSpec);

/*ARGSUSED*/
static int
VectorCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Not used */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;

    /*
     * Try to replicate the old vector command's behavior:
     */
    if (argc > 1) {
	char c;
	register int i;
	register Blt_OpSpec *specPtr;

	c = argv[1][0];
	for (specPtr = vectorCmdOps, i = 0; i < numCmdOps; i++, specPtr++) {
	    if ((c == specPtr->name[0]) &&
		(strcmp(argv[1], specPtr->name) == 0)) {
		goto doOperation;
	    }
	}
	/*
	 * The first argument is not an operation, so assume that its
	 * actually the name of a vector to be created
	 */
	return VectorCreateOp((ClientData)1, interp, argc, argv);
    }
  doOperation:
    /* Do the usual vector operation lookup now. */
    proc = Blt_GetOperation(interp, numCmdOps, vectorCmdOps, BLT_OPER_ARG1,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    return (*proc) ((ClientData)2, interp, argc, argv);
}

/*
 * -----------------------------------------------------------------------
 *
 * VectorDeleteCmd --
 *
 *	This is called when the "vector" command is deleted from the
 *	interpreter.  It will delete all vectors without a Tcl command
 *	or variable associated with them.  Most vectors will be
 *	destroyed along with the interpreter.  However, if the vector
 *	is "anonymous" then there's nothing to trigger the clean up.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Destroys all the vectors not connected to a variable or command.
 *
 * ------------------------------------------------------------------------
 */
/* ARGSUSED */
static void
VectorDeleteCmd(clientData)
    ClientData clientData;	/* Unused */
{
    /*
     * This command may have been created in more than one namespace. So don't
     * delete the vector entries until we know it's the last one.
     *
     * FIXME: Remove vectors with regard to the namespace.
     */
    refCount--;
    if ((initialized) && (refCount == 0)) {
	Tcl_HashEntry *hPtr;
	Tcl_HashSearch cursor;
	Vector *vPtr;
	Blt_List removeLst;
	Blt_ListItem item;

	/*
	 * First save the vectors in a list. We can't delete any hashtable
	 * entries while we're doing a walk of the hash table itself.
	 */
	Blt_InitList(&removeLst, TCL_ONE_WORD_KEYS);
	for (hPtr = Tcl_FirstHashEntry(&vectorTable, &cursor);
	    hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	    vPtr = (Vector *)Tcl_GetHashValue(hPtr);
	    Blt_ListAppend(&removeLst, (char *)vPtr, (ClientData)vPtr);
	}
	for (item = Blt_ListFirstItem(&removeLst); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    vPtr = (Vector *)Blt_ListGetValue(item);
	    FreeVector((ClientData)vPtr);
	}
	Blt_ListReset(&removeLst);
	initialized = 0;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * MathError --
 *
 *	This procedure is called when an error occurs during a
 *	floating-point operation.  It reads errno and sets
 *	interp->result accordingly.
 *
 * Results:
 *	Interp->result is set to hold an error message.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
MathError(interp, value)
    Tcl_Interp *interp;		/* Where to store error message. */
    double value;		/* Value returned after error;  used to
				 * distinguish underflows from overflows. */
{
    if ((errno == EDOM) || (value != value)) {
	Tcl_AppendResult(interp, "domain error: argument not in valid range",
	    (char *)NULL);
	Tcl_SetErrorCode(interp, "ARITH", "DOMAIN", interp->result,
	    (char *)NULL);
    } else if ((errno == ERANGE) || IS_INF(value)) {
	if (value == 0.0) {
	    Tcl_AppendResult(interp, "floating-point value too small to represent",
		(char *)NULL);
	    Tcl_SetErrorCode(interp, "ARITH", "UNDERFLOW", interp->result,
		(char *)NULL);
	} else {
	    Tcl_AppendResult(interp, "floating-point value too large to represent",
		(char *)NULL);
	    Tcl_SetErrorCode(interp, "ARITH", "OVERFLOW", interp->result,
		(char *)NULL);
	}
    } else {
	char buf[20];

	sprintf(buf, "%d", errno);
	Tcl_AppendResult(interp, "unknown floating-point error, ",
	    "errno = ", buf, (char *)NULL);
	Tcl_SetErrorCode(interp, "ARITH", "UNKNOWN", interp->result,
	    (char *)NULL);
    }
}

/*
 *--------------------------------------------------------------
 *
 * ParseString --
 *
 *	Given a string (such as one coming from command or variable
 *	substitution), make a Value based on the string.  The value
 *	will be a floating-point or integer, if possible, or else it
 *	will just be a copy of the string.
 *
 * Results:
 *	TCL_OK is returned under normal circumstances, and TCL_ERROR
 *	is returned if a floating-point overflow or underflow occurred
 *	while reading in a number.  The value at *valuePtr is modified
 *	to hold a number, if possible.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int
ParseString(interp, string, valuePtr)
    Tcl_Interp *interp;		/* Where to store error message. */
    char *string;		/* String to turn into value. */
    Value *valuePtr;		/* Where to store value information.
				 * Caller must have initialized pv field. */
{
    char *endPtr;
    double value;
    Vector *vPtr;

    errno = 0;
    value = strtod(string, &endPtr);
    if ((endPtr != string) && (*endPtr == '\0')) {
	if (errno != 0) {
	    Tcl_ResetResult(interp);
	    MathError(interp, value);
	    return TCL_ERROR;
	}
	if (ResizeVector(valuePtr->vPtr, 1) != TCL_OK) {
	    return TCL_ERROR;
	}
	valuePtr->vPtr->valueArr[0] = value;
	return TCL_OK;
    }
    vPtr = ParseVector(interp, string, &endPtr);
    if (vPtr == NULL) {
	return TCL_ERROR;
    }
    if (*endPtr != '\0') {
	Tcl_AppendResult(interp, "extra characters after vector",
	    (char *)NULL);
	return TCL_ERROR;
    }
    CopyVector(valuePtr->vPtr, vPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ParseAndExecuteFunction --
 *
 *	This procedure is invoked to parse a math function from an
 *	expression string, carry out the function, and return the
 *	value computed.
 *
 * Results:
 *	TCL_OK is returned if all went well and the function's value
 *	was computed successfully.  If the name doesn't match any
 *	known math function, returns TCL_RETURN. And if a format error
 *	was found, TCL_ERROR is returned and an error message is left
 *	in interp->result.
 *
 *	After a successful return infoPtr will be updated to point to
 *	the character just after the function call, the token is set
 *	to VALUE, and the value is stored in valuePtr.
 *
 * Side effects:
 *	Embedded commands could have arbitrary side-effects.
 *
 *----------------------------------------------------------------------
 */
static int
ParseAndExecuteFunction(interp, start, infoPtr, valuePtr)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    char *start;		/* Start of string to parse */
    ParseInfo *infoPtr;		/* Describes the state of the parse.
				 * infoPtr->nextPtr must point to the
				 * first character of the function's
				 * name. */
    Value *valuePtr;		/* Where to store value, if that is
				 * what's parsed from string.  Caller
				 * must have initialized pv field
				 * correctly. */
{
    Tcl_HashEntry *hPtr;
    MathFunction *mathPtr;	/* Info about math function. */
    register char *p;
    Tk_Uid nameId;

    /*
     * Find the end of the math function's name and lookup the
     * record for the function.
     */
    p = start;
    while (isspace(UCHAR(*p))) {
	p++;
    }
    infoPtr->nextPtr = p;
    while (isalnum(UCHAR(*p)) || (*p == '_')) {
	p++;
    }
    if (*p != '(') {
	return TCL_RETURN;	/* Must start with open parenthesis */
    }
    *p = '\0';
    nameId = Blt_FindUid(infoPtr->nextPtr);
    hPtr = NULL;
    if (nameId != NULL) {
	hPtr = Tcl_FindHashEntry(&mathProcTable, nameId);
    }
    *p = '(';
    if (hPtr == NULL) {
	return TCL_RETURN;	/* Name doesn't match any known function */
    }
    /* Pick up the single value as the argument to the function */
    infoPtr->token = OPEN_PAREN;
    infoPtr->nextPtr = p + 1;
    valuePtr->pv.next = valuePtr->pv.buffer;
    if (NextValue(interp, infoPtr, -1, valuePtr) != TCL_OK) {
	return TCL_ERROR;	/* Parse error */
    }
    if (infoPtr->token != CLOSE_PAREN) {
	return TCL_ERROR;	/* Missing right parenthesis */
    }
    mathPtr = (MathFunction *) Tcl_GetHashValue(hPtr);
    if ((*mathPtr->proc) (mathPtr->clientData, interp, valuePtr->vPtr)
	!= TCL_OK) {
	return TCL_ERROR;	/* Function invocation error */
    }
    infoPtr->token = VALUE;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NextToken --
 *
 *	Lexical analyzer for expression parser:  parses a single value,
 *	operator, or other syntactic element from an expression string.
 *
 * Results:
 *	TCL_OK is returned unless an error occurred while doing lexical
 *	analysis or executing an embedded command.  In that case a
 *	standard Tcl error is returned, using interp->result to hold
 *	an error message.  In the event of a successful return, the token
 *	and field in infoPtr is updated to refer to the next symbol in
 *	the expression string, and the expr field is advanced past that
 *	token;  if the token is a value, then the value is stored at
 *	valuePtr.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static int
NextToken(interp, infoPtr, valuePtr)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    ParseInfo *infoPtr;		/* Describes the state of the parse. */
    Value *valuePtr;		/* Where to store value, if that is
				 * what's parsed from string.  Caller
				 * must have initialized pv field
				 * correctly. */
{
    register char *p;
    char *endPtr;
    char *var;
    int result;

    p = infoPtr->nextPtr;
    while (isspace(UCHAR(*p))) {
	p++;
    }
    if (*p == '\0') {
	infoPtr->token = END;
	infoPtr->nextPtr = p;
	return TCL_OK;
    }
    /*
     * Try to parse the token as a floating-point number. But check
     * that the first character isn't a "-" or "+", which "strtod"
     * will happily accept as an unary operator.  Otherwise, we might
     * accidently treat a binary operator as unary by mistake, which
     * will eventually cause a syntax error.
     */
    if ((*p != '-') && (*p != '+')) {
	double value;

	errno = 0;
	value = strtod(p, &endPtr);
	if (endPtr != p) {
	    if (errno != 0) {
		MathError(interp, value);
		return TCL_ERROR;
	    }
	    infoPtr->token = VALUE;
	    infoPtr->nextPtr = endPtr;

	    /*
	     * Save the single floating-point value as an 1-component vector.
	     */
	    if (ResizeVector(valuePtr->vPtr, 1) != TCL_OK) {
		return TCL_ERROR;
	    }
	    valuePtr->vPtr->valueArr[0] = value;
	    return TCL_OK;
	}
    }
    infoPtr->nextPtr = p + 1;
    switch (*p) {
    case '$':
	infoPtr->token = VALUE;
	var = Tcl_ParseVar(interp, p, &endPtr);
	if (var == NULL) {
	    return TCL_ERROR;
	}
	infoPtr->nextPtr = endPtr;
	Tcl_ResetResult(interp);
	result = ParseString(interp, var, valuePtr);
	return result;

    case '[':
	infoPtr->token = VALUE;
	result = TclParseNestedCmd(interp, p + 1, 0, &endPtr, &(valuePtr->pv));
	if (result != TCL_OK) {
	    return result;
	}
	infoPtr->nextPtr = endPtr;
	Tcl_ResetResult(interp);
	result = ParseString(interp, valuePtr->pv.buffer, valuePtr);
	return result;

    case '"':
	infoPtr->token = VALUE;
	result = TclParseQuotes(interp, p + 1, '"', 0, &endPtr, &(valuePtr->pv));
	if (result != TCL_OK) {
	    return result;
	}
	infoPtr->nextPtr = endPtr;
	Tcl_ResetResult(interp);
	result = ParseString(interp, valuePtr->pv.buffer, valuePtr);
	return result;

    case '{':
	infoPtr->token = VALUE;
	result = TclParseBraces(interp, p + 1, &endPtr, &valuePtr->pv);
	if (result != TCL_OK) {
	    return result;
	}
	infoPtr->nextPtr = endPtr;
	Tcl_ResetResult(interp);
	result = ParseString(interp, valuePtr->pv.buffer, valuePtr);
	return result;

    case '(':
	infoPtr->token = OPEN_PAREN;
	break;

    case ')':
	infoPtr->token = CLOSE_PAREN;
	break;

    case ',':
	infoPtr->token = COMMA;
	break;

    case '*':
	infoPtr->token = MULT;
	break;

    case '/':
	infoPtr->token = DIVIDE;
	break;

    case '%':
	infoPtr->token = MOD;
	break;

    case '+':
	infoPtr->token = PLUS;
	break;

    case '-':
	infoPtr->token = MINUS;
	break;

    case '^':
	infoPtr->token = EXPONENT;
	break;

    case '<':
	switch (*(p + 1)) {
#ifdef later
	case '<':
	    infoPtr->nextPtr = p + 2;
	    infoPtr->token = LEFT_SHIFT;
	    break;
#endif
	case '=':
	    infoPtr->nextPtr = p + 2;
	    infoPtr->token = LEQ;
	    break;
	default:
	    infoPtr->token = LESS;
	    break;
	}
	break;

    case '>':
	switch (*(p + 1)) {
#ifdef later
	case '>':
	    infoPtr->nextPtr = p + 2;
	    infoPtr->token = RIGHT_SHIFT;
	    break;
#endif
	case '=':
	    infoPtr->nextPtr = p + 2;
	    infoPtr->token = GEQ;
	    break;
	default:
	    infoPtr->token = GREATER;
	    break;
	}
	break;

    case '=':
	if (*(p + 1) == '=') {
	    infoPtr->nextPtr = p + 2;
	    infoPtr->token = EQUAL;
	} else {
	    infoPtr->token = UNKNOWN;
	}
	break;

    case '&':
	if (*(p + 1) == '&') {
	    infoPtr->nextPtr = p + 2;
	    infoPtr->token = AND;
	} else {
	    infoPtr->token = UNKNOWN;
	}
	break;

    case '|':
	if (*(p + 1) == '|') {
	    infoPtr->nextPtr = p + 2;
	    infoPtr->token = OR;
	} else {
	    infoPtr->token = UNKNOWN;
	}
	break;

    case '!':
	if (*(p + 1) == '=') {
	    infoPtr->nextPtr = p + 2;
	    infoPtr->token = NEQ;
	} else {
	    infoPtr->token = NOT;
	}
	break;

    default:
	infoPtr->token = VALUE;
	result = ParseAndExecuteFunction(interp, p, infoPtr, valuePtr);
	if ((result == TCL_OK) || (result == TCL_ERROR)) {
	    return result;
	} else {
	    Vector *vPtr;

	    vPtr = ParseVector(interp, p, &endPtr);
	    if (vPtr == NULL) {
		return TCL_ERROR;
	    }
	    CopyVector(valuePtr->vPtr, vPtr);
	    infoPtr->nextPtr = endPtr;
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NextValue --
 *
 *	Parse a "value" from the remainder of the expression in infoPtr.
 *
 * Results:
 *	Normally TCL_OK is returned.  The value of the expression is
 *	returned in *valuePtr.  If an error occurred, then interp->result
 *	contains an error message and TCL_ERROR is returned.
 *	InfoPtr->token will be left pointing to the token AFTER the
 *	expression, and infoPtr->nextPtr will point to the character just
 *	after the terminating token.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static int
NextValue(interp, infoPtr, prec, valuePtr)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    register ParseInfo *infoPtr;/* Describes the state of the parse
				  * just before the value (i.e. NextToken will
				  * be called to get first token of value). */
    int prec;			/* Treat any un-parenthesized operator
				 * with precedence <= this as the end
				 * of the expression. */
    Value *valuePtr;		/* Where to store the value of the expression.
				 * Caller must have initialized pv field. */
{
    Value value2;		/* Second operand for current operator.  */
    int operator;		/* Current operator (either unary or binary). */
    int gotOp;			/* Non-zero means already lexed the operator
				 * (while picking up value for unary operator).
				 * Don't lex again. */
    int result;
    Vector *vPtr, *v2Ptr;
    register int i;

    /*
     * There are two phases to this procedure.  First, pick off an initial
     * value.  Then, parse (binary operator, value) pairs until done.
     */

    vPtr = valuePtr->vPtr;
    v2Ptr = NewVector(interp, (char *)NULL);

    gotOp = 0;
    value2.vPtr = v2Ptr;
    value2.pv.buffer = value2.pv.next = value2.staticSpace;
    value2.pv.end = value2.pv.buffer + STATIC_STRING_SPACE - 1;
    value2.pv.expandProc = TclExpandParseValue;
    value2.pv.clientData = (ClientData)NULL;

    result = NextToken(interp, infoPtr, valuePtr);
    if (result != TCL_OK) {
	goto done;
    }
    if (infoPtr->token == OPEN_PAREN) {

	/* Parenthesized sub-expression. */

	result = NextValue(interp, infoPtr, -1, valuePtr);
	if (result != TCL_OK) {
	    goto done;
	}
	if (infoPtr->token != CLOSE_PAREN) {
	    Tcl_AppendResult(interp, "unmatched parentheses in expression \"",
		infoPtr->expr, "\"", (char *)NULL);
	    result = TCL_ERROR;
	    goto done;
	}
    } else {
	if (infoPtr->token == MINUS) {
	    infoPtr->token = UNARY_MINUS;
	}
	if (infoPtr->token >= UNARY_MINUS) {
	    operator = infoPtr->token;
	    result = NextValue(interp, infoPtr, precTable[operator], valuePtr);
	    if (result != TCL_OK) {
		goto done;
	    }
	    gotOp = 1;
	    /* Process unary operators. */
	    switch (operator) {
	    case UNARY_MINUS:
		for (i = 0; i < vPtr->numValues; i++) {
		    vPtr->valueArr[i] = -(vPtr->valueArr[i]);
		}
		break;

	    case NOT:
		for (i = 0; i < vPtr->numValues; i++) {
		    vPtr->valueArr[i] = (double)(!vPtr->valueArr[i]);
		}
		break;
	    default:
		Tcl_AppendResult(interp, "unknown operator", (char *)NULL);
		goto error;
	    }
	}
    }
    if (!gotOp) {
	result = NextToken(interp, infoPtr, &value2);
	if (result != TCL_OK) {
	    goto done;
	}
    }
    /*
     * Got the first operand.  Now fetch (operator, operand) pairs.
     */
    for (;;) {
	operator = infoPtr->token;

	value2.pv.next = value2.pv.buffer;
	if ((operator < MULT) || (operator >= UNARY_MINUS)) {
	    if ((operator == END) || (operator == CLOSE_PAREN)) {
		result = TCL_OK;
		goto done;
	    } else {
		Tcl_AppendResult(interp, "bad operator", (char *)NULL);
		goto error;
	    }
	}
	if (precTable[operator] <= prec) {
	    result = TCL_OK;
	    goto done;
	}
	result = NextValue(interp, infoPtr, precTable[operator], &value2);
	if (result != TCL_OK) {
	    goto done;
	}
	if ((infoPtr->token < MULT) && (infoPtr->token != VALUE) &&
	    (infoPtr->token != END) && (infoPtr->token != CLOSE_PAREN)) {
	    Tcl_AppendResult(interp, "unexpected token in expression",
		(char *)NULL);
	    goto error;
	}
	/*
	 * At this point we've got two vectors and an operator.
	 */

	if (v2Ptr->numValues == 1) {
	    register double *opnd;
	    register double scalar;

	    /*
	     * 2nd operand is a scalar.
	     */
	    scalar = v2Ptr->valueArr[0];
	    opnd = vPtr->valueArr;
	    switch (operator) {
	    case MULT:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] *= scalar;
		}
		break;

	    case DIVIDE:
		if (scalar == 0.0) {
		    Tcl_AppendResult(interp, "divide by zero", (char *)NULL);
		    goto error;
		}
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] /= scalar;
		}
		break;

	    case PLUS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] += scalar;
		}
		break;

	    case MINUS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] -= scalar;
		}
		break;

	    case EXPONENT:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = pow(opnd[i], scalar);
		}
		break;

	    case MOD:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = Fmod(opnd[i], scalar);
		}
		break;

	    case LESS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] < scalar);
		}
		break;

	    case GREATER:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] > scalar);
		}
		break;

	    case LEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] >= scalar);
		}
		break;

	    case GEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] <= scalar);
		}
		break;

	    case EQUAL:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] == scalar);
		}
		break;

	    case NEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] != scalar);
		}
		break;

	    case AND:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] && scalar);
		}
		break;

	    case OR:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] || scalar);
		}
		break;

	    default:
		Tcl_AppendResult(interp, "unknown operator in expression",
		    (char *)NULL);
		goto error;
	    }

	} else if (vPtr->numValues == 1) {
	    register double *opnd;
	    register double scalar;

	    /*
	     * 1st operand is a scalar.
	     */
	    scalar = vPtr->valueArr[0];
	    CopyVector(vPtr, v2Ptr);
	    opnd = vPtr->valueArr;
	    switch (operator) {
	    case MULT:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] *= scalar;
		}
		break;

	    case PLUS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] += scalar;
		}
		break;

	    case DIVIDE:
		for (i = 0; i < vPtr->numValues; i++) {
		    if (opnd[i] == 0.0) {
			Tcl_AppendResult(interp, "divide by zero",
			    (char *)NULL);
			goto error;
		    }
		    opnd[i] = (scalar / opnd[i]);
		}
		break;

	    case MINUS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = scalar - opnd[i];
		}
		break;

	    case EXPONENT:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = pow(scalar, opnd[i]);
		}
		break;

	    case MOD:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = Fmod(scalar, opnd[i]);
		}
		break;

	    case LESS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(scalar < opnd[i]);
		}
		break;

	    case GREATER:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(scalar > opnd[i]);
		}
		break;

	    case LEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(scalar >= opnd[i]);
		}
		break;

	    case GEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(scalar <= opnd[i]);
		}
		break;

	    case EQUAL:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] == scalar);
		}
		break;

	    case NEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] != scalar);
		}
		break;

	    case AND:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] && scalar);
		}
		break;

	    case OR:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd[i] = (double)(opnd[i] || scalar);
		}
		break;

	    default:
		Tcl_AppendResult(interp, "unknown operator in expression",
		    (char *)NULL);
		goto error;
	    }
	} else {
	    register double *opnd1, *opnd2;
	    /*
	     * Carry out the function of the specified operator.
	     */
	    if (vPtr->numValues != v2Ptr->numValues) {
		Tcl_AppendResult(interp, "vectors are different lengths",
		    (char *)NULL);
		goto error;
	    }
	    opnd1 = vPtr->valueArr, opnd2 = v2Ptr->valueArr;
	    switch (operator) {
	    case MULT:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] *= opnd2[i];
		}
		break;

	    case DIVIDE:
		for (i = 0; i < vPtr->numValues; i++) {
		    if (opnd2[i] == 0.0) {
			Tcl_AppendResult(interp,
			    "can't divide by 0.0 vector component",
			    (char *)NULL);
			goto error;
		    }
		    opnd1[i] /= opnd2[i];
		}
		break;

	    case PLUS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] += opnd2[i];
		}
		break;

	    case MINUS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] -= opnd2[i];
		}
		break;

	    case MOD:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = Fmod(opnd1[i], opnd2[i]);
		}
		break;

	    case EXPONENT:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = pow(opnd1[i], opnd2[i]);
		}
		break;

	    case LESS:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = (double)(opnd1[i] < opnd2[i]);
		}
		break;

	    case GREATER:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = (double)(opnd1[i] > opnd2[i]);
		}
		break;

	    case LEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = (double)(opnd1[i] <= opnd2[i]);
		}
		break;

	    case GEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = (double)(opnd1[i] >= opnd2[i]);
		}
		break;

	    case EQUAL:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = (double)(opnd1[i] == opnd2[i]);
		}
		break;

	    case NEQ:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = (double)(opnd1[i] != opnd2[i]);
		}
		break;

	    case AND:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = (double)(opnd1[i] && opnd2[i]);
		}
		break;

	    case OR:
		for (i = 0; i < vPtr->numValues; i++) {
		    opnd1[i] = (double)(opnd1[i] || opnd2[i]);
		}
		break;

	    default:
		Tcl_AppendResult(interp, "unknown operator in expression",
		    (char *)NULL);
		goto error;
	    }
	}
    }
  done:
    if (value2.pv.buffer != value2.staticSpace) {
	free(value2.pv.buffer);
    }
    FreeVector(v2Ptr);
    return TCL_OK;

  error:
    if (value2.pv.buffer != value2.staticSpace) {
	free(value2.pv.buffer);
    }
    FreeVector(v2Ptr);
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * EvaluateExpression --
 *
 *	This procedure provides top-level functionality shared by
 *	procedures like Tcl_ExprInt, Tcl_ExprDouble, etc.
 *
 * Results:
 *	The result is a standard Tcl return value.  If an error
 *	occurs then an error message is left in interp->result.
 *	The value of the expression is returned in *valuePtr, in
 *	whatever form it ends up in (could be string or integer
 *	or double).  Caller may need to convert result.  Caller
 *	is also responsible for freeing string memory in *valuePtr,
 *	if any was allocated.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
static int
EvaluateExpression(interp, string, valuePtr)
    Tcl_Interp *interp;		/* Context in which to evaluate the
					 * expression. */
    char *string;		/* Expression to evaluate. */
    Value *valuePtr;		/* Where to store result.  Should
					 * not be initialized by caller. */
{
    ParseInfo info;
    int result;
    Vector *vPtr;
    register int i;

    info.expr = info.nextPtr = string;
    valuePtr->pv.buffer = valuePtr->pv.next = valuePtr->staticSpace;
    valuePtr->pv.end = valuePtr->pv.buffer + STATIC_STRING_SPACE - 1;
    valuePtr->pv.expandProc = TclExpandParseValue;
    valuePtr->pv.clientData = (ClientData)NULL;

    result = NextValue(interp, &info, -1, valuePtr);
    if (result != TCL_OK) {
	return result;
    }
    if (info.token != END) {
	Tcl_AppendResult(interp, ": syntax error in expression \"",
	    string, "\"", (char *)NULL);
	return TCL_ERROR;
    }
    vPtr = valuePtr->vPtr;

    /* Check for NaN's and underflows. */
    for (i = 0; i < vPtr->numValues; i++) {
	if ((IS_NAN(vPtr->valueArr[i]) || IS_INF(vPtr->valueArr[i]))) {
	    /*
	     * IEEE floating-point error.
	     */
	    MathError(interp, vPtr->valueArr[i]);
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Math Functions --
 *
 *	This page contains the procedures that implement all of the
 *	built-in math functions for expressions.
 *
 * Results:
 *	Each procedure returns TCL_OK if it succeeds and places result
 *	information at *resultPtr.  If it fails it returns TCL_ERROR
 *	and leaves an error message in interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static int
ComponentFunc(clientData, interp, vPtr)
    ClientData clientData;	/* Contains address of procedure that
					 * takes one double argument and
					 * returns a double result. */
    Tcl_Interp *interp;
    Vector *vPtr;
{
    ComponentProc *procPtr = (ComponentProc *) clientData;
    register int i;

    errno = 0;
    for (i = 0; i < vPtr->numValues; i++) {
	vPtr->valueArr[i] = (*procPtr) (vPtr->valueArr[i]);
	if (errno != 0) {
	    MathError(interp, vPtr->valueArr[i]);
	    return TCL_ERROR;
	}
	if ((IS_NAN(vPtr->valueArr[i]) || IS_INF(vPtr->valueArr[i]))) {
	    /*
	     * IEEE floating-point error.
	     */
	    MathError(interp, vPtr->valueArr[i]);
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
}

static int
ScalarFunc(clientData, interp, vPtr)
    ClientData clientData;
    Tcl_Interp *interp;
    Vector *vPtr;
{
    double value;
    ScalarProc *procPtr = (ScalarProc *) clientData;

    errno = 0;
    value = (*procPtr) (vPtr);
    if (errno != 0) {
	MathError(interp, value);
	return TCL_ERROR;
    }
    if (ResizeVector(vPtr, 1) != TCL_OK) {
	return TCL_ERROR;
    }
    vPtr->valueArr[0] = value;
    return TCL_OK;
}

/*ARGSUSED*/
static int
VectorFunc(clientData, interp, vPtr)
    ClientData clientData;
    Tcl_Interp *interp;		/* not used */
    Vector *vPtr;
{
    VectorProc *procPtr = (VectorProc *) clientData;

    return (*procPtr) (vPtr);
}


static MathFunction mathFunctions[] =
{
    {"abs", (GenericMathProc *) ComponentFunc, (ClientData)Fabs},
    {"acos", (GenericMathProc *) ComponentFunc, (ClientData)acos},
    {"asin", (GenericMathProc *) ComponentFunc, (ClientData)asin},
    {"atan", (GenericMathProc *) ComponentFunc, (ClientData)atan},
    {"adev", (GenericMathProc *) ScalarFunc, (ClientData)AvgDeviation},
    {"ceil", (GenericMathProc *) ComponentFunc, (ClientData)ceil},
    {"cos", (GenericMathProc *) ComponentFunc, (ClientData)cos},
    {"cosh", (GenericMathProc *) ComponentFunc, (ClientData)cosh},
    {"exp", (GenericMathProc *) ComponentFunc, (ClientData)exp},
    {"floor", (GenericMathProc *) ComponentFunc, (ClientData)floor},
    {"kurtosis", (GenericMathProc *) ScalarFunc, (ClientData)Kurtosis},
    {"length", (GenericMathProc *) ScalarFunc, (ClientData)Length},
    {"log", (GenericMathProc *) ComponentFunc, (ClientData)log},
    {"log10", (GenericMathProc *) ComponentFunc, (ClientData)log10},
    {"max", (GenericMathProc *) ScalarFunc, (ClientData)Max},
    {"mean", (GenericMathProc *) ScalarFunc, (ClientData)Mean},
    {"median", (GenericMathProc *) ScalarFunc, (ClientData)Median},
    {"min", (GenericMathProc *) ScalarFunc, (ClientData)Min},
    {"norm", (GenericMathProc *) VectorFunc, (ClientData)Norm},
    {"nz", (GenericMathProc *) ScalarFunc, (ClientData)Nonzeros},
    {"q1", (GenericMathProc *) ScalarFunc, (ClientData)Q1},
    {"q3", (GenericMathProc *) ScalarFunc, (ClientData)Q3},
    {"prod", (GenericMathProc *) ScalarFunc, (ClientData)Product},
#ifdef HAVE_DRAND48
    {"random", (GenericMathProc *) ComponentFunc, (ClientData)drand48},
#endif
    {"round", (GenericMathProc *) ComponentFunc, (ClientData)Round},
    {"sdev", (GenericMathProc *) ScalarFunc, (ClientData)StdDeviation},
    {"sin", (GenericMathProc *) ComponentFunc, (ClientData)sin},
    {"sinh", (GenericMathProc *) ComponentFunc, (ClientData)sinh},
    {"skew", (GenericMathProc *) ScalarFunc, (ClientData)Skew},
    {"sort", (GenericMathProc *) VectorFunc, (ClientData)Sort},
    {"sqrt", (GenericMathProc *) ComponentFunc, (ClientData)sqrt},
    {"sum", (GenericMathProc *) ScalarFunc, (ClientData)Sum},
    {"tan", (GenericMathProc *) ComponentFunc, (ClientData)tan},
    {"tanh", (GenericMathProc *) ComponentFunc, (ClientData)tanh},
    {"var", (GenericMathProc *) ScalarFunc, (ClientData)Variance},
    {(char *)NULL,},
};

static void
InstallSpecialIndices()
{
    Blt_InstallIndexProc("min", Min);
    Blt_InstallIndexProc("max", Max);
    Blt_InstallIndexProc("mean", Mean);
    Blt_InstallIndexProc("sum", Sum);
    Blt_InstallIndexProc("prod", Product);
}

static void
InstallMathFunctions()
{
    Tcl_HashEntry *hPtr;
    register MathFunction *mathPtr;
    int dummy;

    if (mathProcTable.numEntries > 0) {
	Tcl_HashSearch cursor;

	/*
	 * If any math functions were previously installed, remove them
	 * before adding the new functions.
	 */
	for (hPtr = Tcl_FirstHashEntry(&mathProcTable, &cursor); hPtr != NULL;
	    hPtr = Tcl_NextHashEntry(&cursor)) {
	    mathPtr = (MathFunction *) Tcl_GetHashValue(hPtr);
	    Blt_FreeUid(mathPtr->nameId);
	    if (mathPtr->dynAlloc) {
		free((char *)mathPtr);
	    }
	}
    }
    Tcl_InitHashTable(&mathProcTable, TCL_ONE_WORD_KEYS);
    for (mathPtr = mathFunctions; mathPtr->nameId != NULL; mathPtr++) {
	mathPtr->nameId = Blt_GetUid(mathPtr->nameId);
	hPtr = Tcl_CreateHashEntry(&mathProcTable, mathPtr->nameId, &dummy);
	Tcl_SetHashValue(hPtr, (ClientData)mathPtr);
    }
}

/*
 * -----------------------------------------------------------------------
 *
 * Blt_VectorInit --
 *
 *	This procedure is invoked to initialize the "vector" command.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Creates the new command and adds a new entry into a global Tcl
 *	associative array.
 *
 * ------------------------------------------------------------------------
 */
int
Blt_VectorInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec =
    {"vector", VectorCmd, VectorDeleteCmd,};

    /* Flag this so this routine can be run more than once */
    if (refCount == 0) {
	if (!initialized) {
	    Tcl_InitHashTable(&vectorTable, sizeof(VectorKey) / sizeof(int));
	    initialized++;
	}
	Tcl_InitHashTable(&indexProcTable, TCL_STRING_KEYS);
	Tcl_InitHashTable(&mathProcTable, TCL_STRING_KEYS);
#ifdef HAVE_SRAND48
	srand48(1234L);
#endif
    }
    refCount++;
    InstallMathFunctions();
    InstallSpecialIndices();

    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/* C Application interface to vectors */

/*
 * -----------------------------------------------------------------------
 *
 * Blt_CreateVector --
 *
 *	Creates a new vector by the name and size.
 *
 * Results:
 *	A standard Tcl result.  If the new array size is invalid or a
 *	vector already exists by that name, TCL_ERROR is returned.
 *	Otherwise TCL_OK is returned and the new vector is created.
 *
 * Side Effects:
 *	Memory will be allocated for the new vector.  A new Tcl command
 *	and Tcl array variable will be created.
 *
 * -----------------------------------------------------------------------
 */

/*LINTLIBRARY*/
int
Blt_CreateVector2(interp, vecName, cmdName, varName, initialSize, vecPtrPtr)
    Tcl_Interp *interp;
    char *vecName;
    char *cmdName, *varName;
    int initialSize;
    Blt_Vector **vecPtrPtr;
{
    Vector *vPtr;
    int isNew;

    if (!initialized) {
	Tcl_InitHashTable(&vectorTable, sizeof(VectorKey) / sizeof(int));
	initialized = TRUE;
    }
    if (initialSize < 0) {
	Tcl_AppendResult(interp, "bad vector size \"", Blt_Int(initialSize),
	    "\"", (char *)NULL);
	return TCL_ERROR;
    }
    vPtr = CreateVector(interp, vecName, cmdName, varName, &isNew);
    if (vPtr == NULL) {
	return TCL_ERROR;
    }
    if (initialSize > 0) {
	if (ResizeVector(vPtr, initialSize) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    if (vecPtrPtr != NULL) {
	*vecPtrPtr = (Blt_Vector *) vPtr;
    }
    return TCL_OK;
}

int
Blt_CreateVector(interp, name, size, vecPtrPtr)
    Tcl_Interp *interp;
    char *name;
    int size;
    Blt_Vector **vecPtrPtr;
{
    return Blt_CreateVector2(interp, name, name, name, size, vecPtrPtr);
}

/*
 * -----------------------------------------------------------------------
 *
 * Blt_DeleteVector --
 *
 *	Deletes the vector of the given name.  All clients with
 *	designated callback routines will be notified.
 *
 * Results:
 *	A standard Tcl result.  If no vector exists by that name,
 *	TCL_ERROR is returned.  Otherwise TCL_OK is returned and
 *	vector is deleted.
 *
 * Side Effects:
 *	Memory will be released for the new vector.  Both the Tcl
 *	command and array variable will be deleted.  All clients which
 *	set call back procedures will be notified.
 *
 * -----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
int
Blt_DeleteVector(vecPtr)
    Blt_Vector *vecPtr;
{
    Vector *vPtr = (Vector *)vecPtr;

    FreeVector(vPtr);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * Blt_DeleteVectorByName --
 *
 *	Deletes the vector of the given name.  All clients with
 *	designated callback routines will be notified.
 *
 * Results:
 *	A standard Tcl result.  If no vector exists by that name,
 *	TCL_ERROR is returned.  Otherwise TCL_OK is returned and
 *	vector is deleted.
 *
 * Side Effects:
 *	Memory will be released for the new vector.  Both the Tcl
 *	command and array variable will be deleted.  All clients which
 *	set call back procedures will be notified.
 *
 * -----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
int
Blt_DeleteVectorByName(interp, vecName)
    Tcl_Interp *interp;
    char *vecName;
{
    Vector *vPtr;

    if (!initialized) {
	Tcl_InitHashTable(&vectorTable, sizeof(VectorKey) / sizeof(int));
	initialized++;
    }
    vPtr = FindVector(interp, vecName, TCL_LEAVE_ERR_MSG);
    if (vPtr == NULL) {
	return TCL_ERROR;
    }
    FreeVector(vPtr);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_VectorExists --
 *
 *	Returns whether the vector associated with the client token
 *	still exists.
 *
 * Results:
 *	Returns 1 is the vector still exists, 0 otherwise.
 *
 * ----------------------------------------------------------------------
 */
int
Blt_VectorExists(interp, vecName)
    Tcl_Interp *interp;
    char *vecName;
{
    Vector *vPtr;

    if (!initialized) {
	Tcl_InitHashTable(&vectorTable, sizeof(VectorKey) / sizeof(int));
	initialized++;
    }
    vPtr = FindVector(interp, vecName, 0);
    return (vPtr != NULL);
}

/*
 * -----------------------------------------------------------------------
 *
 * Blt_GetVector --
 *
 *	Returns a pointer to the vector associated with the given name.
 *
 * Results:
 *	A standard Tcl result.  If there is no vector "name", TCL_ERROR
 *	is returned.  Otherwise TCL_OK is returned and vecPtrPtr will
 *	point to the vector.
 *
 * -----------------------------------------------------------------------
 */
int
Blt_GetVector(interp, vecName, vecPtrPtr)
    Tcl_Interp *interp;
    char *vecName;
    Blt_Vector **vecPtrPtr;
{
    Vector *vPtr;

    if (!initialized) {
	Tcl_InitHashTable(&vectorTable, sizeof(VectorKey) / sizeof(int));
	initialized++;
    }
    vPtr = FindVector(interp, vecName, TCL_LEAVE_ERR_MSG);
    if (vPtr == NULL) {
	return TCL_ERROR;
    }
    if (vPtr->flags & UPDATE_LIMITS) {
	UpdateLimits(vPtr);
    }
    *vecPtrPtr = (Blt_Vector *) vPtr;
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * Blt_ResetVector --
 *
 *	Resets the vector data.  This is called by a client to
 *	indicate that the vector data has changed.  The vector does
 *	not need to point to different memory.  Any clients of the
 *	vector will be notified of the change.
 *
 * Results:
 *	A standard Tcl result.  If the new array size is invalid,
 *	TCL_ERROR is returned.  Otherwise TCL_OK is returned and the
 *	new vector data is recorded.
 *
 * Side Effects:
 *	Any client designated callbacks will be posted.  Memory may
 *	be changed for the vector array.
 *
 * -----------------------------------------------------------------------
 */
int
Blt_ResetVector(vecPtr, dataArr, numValues, arraySize, freeProc)
    Blt_Vector *vecPtr;
    double *dataArr;
    int numValues;
    int arraySize;		/* Size of the array in bytes */
    Tcl_FreeProc *freeProc;	/* Address of memory deallocation routine
				 * for the array of values.  Can also be
				 * TCL_STATIC, TCL_DYNAMIC, or TCL_VOLATILE. */
{
    Vector *vPtr;

    vPtr = (Vector *)vecPtr;

    if (arraySize < 0) {
	Tcl_AppendResult(vPtr->interp, "bad array size", (char *)NULL);
	return TCL_ERROR;
    }
    if (vPtr->valueArr != dataArr) {

	/*
	 * New array of values is in different memory than the current
	 * vector.
	 */

	if ((dataArr == NULL) || (arraySize == 0)) {
	    /* Empty array. Set up default values */
	    freeProc = TCL_STATIC;
	    dataArr = vPtr->staticSpace;
	    arraySize = DEF_ARRAY_SIZE;
	    numValues = 0;
	} else if (freeProc == TCL_VOLATILE) {
	    double *newArr;

	    /*
	     * Data is volatile. Make a copy of the value array.
	     */
	    newArr = (double *)malloc(arraySize);
	    memcpy((char *)newArr, (char *)dataArr, sizeof(double) * numValues);

	    dataArr = newArr;
	    freeProc = TCL_DYNAMIC;
	}
	/*
	 * Old data was dynamically allocated. Free it before attaching
	 * new data.
	 */

	if ((vPtr->valueArr != vPtr->staticSpace) &&
	    (vPtr->freeProc != TCL_STATIC)) {
	    if (vPtr->freeProc == TCL_DYNAMIC) {
		free((char *)vPtr->valueArr);
	    } else {
		(*freeProc) ((char *)vPtr->valueArr);
	    }
	}
	vPtr->freeProc = freeProc;
	vPtr->valueArr = dataArr;
	vPtr->arraySize = arraySize;
    }
    vPtr->numValues = numValues;
    FlushCache(vPtr);
    UpdateLimits(vPtr);
    UpdateClients(vPtr);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * Blt_ResizeVector --
 *
 *	Changes the size of the vector.  All clients with designated
 *	callback routines will be notified of the size change.
 *
 * Results:
 *	A standard Tcl result.  If no vector exists by that name,
 *	TCL_ERROR is returned.  Otherwise TCL_OK is returned and
 *	vector is resized.
 *
 * Side Effects:
 *	Memory may be reallocated for the new vector size.  All clients
 *	which set call back procedures will be notified.
 *
 * -----------------------------------------------------------------------
 */
int
Blt_ResizeVector(vecPtr, length)
    Blt_Vector *vecPtr;
    int length;
{
    Vector *vPtr = (Vector *)vecPtr;

    if (ResizeVector(vPtr, length) != TCL_OK) {
	Tcl_AppendResult(vPtr->interp, "can't resize vector \"", vPtr->nameId,
	    "\"", (char *)NULL);
	return TCL_ERROR;
    }
    FlushCache(vPtr);
    UpdateLimits(vPtr);
    UpdateClients(vPtr);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Blt_AllocVectorId --
 *
 *	Creates an identifier token for an existing vector.  The
 *	identifier is used by the client routines to get call backs
 *	when (and if) the vector changes.
 *
 * Results:
 *	A standard Tcl result.  If "vecName" is not associated with
 *	a vector, TCL_ERROR is returned and interp->result is filled
 *	with an error message.
 *
 *--------------------------------------------------------------
 */
Blt_VectorId
Blt_AllocVectorId(interp, vecName)
    Tcl_Interp *interp;
    char *vecName;
{
    Vector *vPtr;
    ClientInfo *clientPtr;
    Blt_VectorId clientId;

    if (!initialized) {
	Tcl_InitHashTable(&vectorTable, sizeof(VectorKey) / sizeof(int));
	initialized++;
    }
    vPtr = FindVector(interp, vecName, TCL_LEAVE_ERR_MSG);
    if (vPtr == NULL) {
	return (Blt_VectorId) 0;
    }
    /* Allocate a new client structure */
    clientPtr = (ClientInfo *)calloc(1, sizeof(ClientInfo));
    assert(clientPtr);
    clientPtr->magic = VECTOR_MAGIC;

    /*
     * Add the new client to the server's list of clients
     */
    clientPtr->item = Blt_ListAppend(&(vPtr->clientList), (char *)clientPtr,
	(ClientData)clientPtr);
    clientPtr->serverPtr = vPtr;
    clientId = (Blt_VectorId) clientPtr;
    return clientId;
}

/*
 * -----------------------------------------------------------------------
 *
 * Blt_SetVectorChangedProc --
 *
 *	Sets the routine to be called back when the vector is changed
 *	or deleted.  *clientData* will be provided as an argument. If
 *	*proc* is NULL, no callback will be made.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The designated routine will be called when the vector is changed
 *	or deleted.
 *
 * -----------------------------------------------------------------------
 */
void
Blt_SetVectorChangedProc(clientId, proc, clientData)
    Blt_VectorId clientId;	/* Client token identifying the vector */
    Blt_VectorChangedProc *proc;/* Address of routine to call when the contents
				 * of the vector change. If NULL, no routine
				 * will be called */
    ClientData clientData;	/* One word of information to pass along when
				 * the above routine is called */
{
    ClientInfo *clientPtr = (ClientInfo *)clientId;

    if (clientPtr->magic != VECTOR_MAGIC) {
	return;			/* Not a valid token */
    }
    clientPtr->clientData = clientData;
    clientPtr->proc = proc;
}

/*
 *--------------------------------------------------------------
 *
 * Blt_FreeVectorId --
 *
 *	Releases the token for an existing vector.  This indicates
 *	that the client is no longer interested the vector.  Any
 *	previously specified callback routine will no longer be
 *	invoked when (and if) the vector changes.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Any previously specified callback routine will no longer be
 *	invoked when (and if) the vector changes.
 *
 *--------------------------------------------------------------
 */
void
Blt_FreeVectorId(clientId)
    Blt_VectorId clientId;	/* Client token identifying the vector */
{
    ClientInfo *clientPtr = (ClientInfo *)clientId;

    if (clientPtr->magic != VECTOR_MAGIC) {
	return;			/* Not a valid token */
    }
    if (clientPtr->serverPtr != NULL) {
	/* Remove the client from the server's list */
	Blt_ListDeleteItem(clientPtr->item);
    }
    free((char *)clientPtr);
}

/*
 *--------------------------------------------------------------
 *
 * Blt_NameOfVectorId --
 *
 *	Returns the name of the vector (and array variable).
 *
 * Results:
 *	The name of the array variable is returned.
 *
 *--------------------------------------------------------------
 */
char *
Blt_NameOfVectorId(clientId)
    Blt_VectorId clientId;	/* Client token identifying the vector */
{
    ClientInfo *clientPtr = (ClientInfo *)clientId;

    if ((clientPtr->magic != VECTOR_MAGIC) || (clientPtr->serverPtr == NULL)) {
	return NULL;
    }
    return clientPtr->serverPtr->nameId;
}

/*
 *--------------------------------------------------------------
 *
 * Blt_VectorNotifyPending --
 *
 *	Returns the name of the vector (and array variable).
 *
 * Results:
 *	The name of the array variable is returned.
 *
 *--------------------------------------------------------------
 */
int
Blt_VectorNotifyPending(clientId)
    Blt_VectorId clientId;	/* Client token identifying the vector */
{
    ClientInfo *clientPtr = (ClientInfo *)clientId;

    if ((clientPtr->magic != VECTOR_MAGIC) || (clientPtr->serverPtr == NULL)) {
	return 0;
    }
    return (clientPtr->serverPtr->flags & NOTIFY_PENDING);
}

/*
 * -----------------------------------------------------------------------
 *
 * Blt_GetVectorById --
 *
 *	Returns a pointer to the vector associated with the client
 *	token.
 *
 * Results:
 *	A standard Tcl result.  If the client token is not associated
 *	with a vector any longer, TCL_ERROR is returned. Otherwise,
 *	TCL_OK is returned and vecPtrPtr will point to vector.
 *
 * -----------------------------------------------------------------------
 */
int
Blt_GetVectorById(interp, clientId, vecPtrPtr)
    Tcl_Interp *interp;
    Blt_VectorId clientId;	/* Client token identifying the vector */
    Blt_Vector **vecPtrPtr;
{
    ClientInfo *clientPtr = (ClientInfo *)clientId;

    if (clientPtr->magic != VECTOR_MAGIC) {
	Tcl_AppendResult(interp, "bad vector token", (char *)NULL);
	return TCL_ERROR;
    }
    if (clientPtr->serverPtr == NULL) {
	Tcl_AppendResult(interp, "vector no longer exists", (char *)NULL);
	return TCL_ERROR;
    }
    if (clientPtr->serverPtr->flags & UPDATE_LIMITS) {
	UpdateLimits(clientPtr->serverPtr);
    }
    *vecPtrPtr = (Blt_Vector *) clientPtr->serverPtr;
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Blt_ExprVector --
 *
 *	Evaluates an vector expression and returns its value(s).
 *
 * Results:
 *	Each of the procedures below returns a standard Tcl result.
 *	If an error occurs then an error message is left in
 *	interp->result.  Otherwise the value of the expression,
 *	in the appropriate form, is stored at *resultPtr.  If
 *	the expression had a result that was incompatible with the
 *	desired form then an error is returned.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
int
Blt_ExprVector(interp, string, vecPtr)
    Tcl_Interp *interp;		/* Context in which to evaluate the
					 * expression. */
    char *string;		/* Expression to evaluate. */
    Blt_Vector *vecPtr;		/* Where to store result. */
{
    Vector *vPtr = (Vector *)vecPtr;
    Value value;

    value.vPtr = NewVector(interp, (char *)NULL);
    if (EvaluateExpression(interp, string, &value) != TCL_OK) {
	FreeVector(value.vPtr);
	return TCL_ERROR;
    }
    if (vPtr != NULL) {
	CopyVector(vPtr, value.vPtr);
    } else {
	register int i;
	/*
	 * No result vector, so put values in interp->result.
	 */
	for (i = 0; i < value.vPtr->numValues; i++) {
	    Tcl_AppendElement(interp, Blt_Double(interp, value.vPtr->valueArr[i]));
	}
    }
    FreeVector(value.vPtr);
    return TCL_OK;
}

int
Blt_InstallMathFunction(interp, name, type, clientData)
    Tcl_Interp *interp;
    char *name;
    Blt_MathFuncType type;
    ClientData clientData;
{
    Tcl_HashEntry *hPtr;
    MathFunction *mathPtr;
    GenericMathProc *proc;
    Tk_Uid nameId;
    int isNew;

    nameId = Blt_GetUid(name);
    hPtr = Tcl_CreateHashEntry(&mathProcTable, nameId, &isNew);
    if (!isNew) {
	Blt_FreeUid(nameId);
	mathPtr = (MathFunction *) Tcl_GetHashValue(hPtr);
	if (mathPtr->dynAlloc) {
	    free((char *)mathPtr);
	}
    }
    if (clientData == (ClientData)NULL) {
	Tcl_DeleteHashEntry(hPtr);
	return TCL_OK;
    }
    switch (type) {
    case BLT_MATH_FUNC_SCALAR:
	proc = (GenericMathProc *) ScalarFunc;
	break;

    case BLT_MATH_FUNC_VECTOR:
	proc = (GenericMathProc *) VectorFunc;
	break;

    default:
	Tcl_AppendResult(interp, "unknown function return type", (char *)NULL);
	return TCL_ERROR;
    }
    mathPtr = (MathFunction *) malloc(sizeof(MathFunction));
    assert(mathPtr);
    mathPtr->nameId = nameId;
    mathPtr->proc = proc;
    mathPtr->clientData = clientData;
    mathPtr->dynAlloc = TRUE;
    Tcl_SetHashValue(hPtr, (ClientData)mathPtr);
    return TCL_OK;
}

/*LINTLIBRARY*/
void
Blt_InstallIndexProc(indexName, procPtr)
    char *indexName;
    Blt_VectorIndexProc *procPtr;	/* Pointer to function to be called
				   * when the vector finds the named index.
				   * If NULL, this indicates to remove
				   * the index from the table.
				   */
{
    Tcl_HashEntry *hPtr;
    int dummy;

    hPtr = Tcl_CreateHashEntry(&indexProcTable, indexName, &dummy);
    if (procPtr == NULL) {
	Tcl_DeleteHashEntry(hPtr);
    } else {
	Tcl_SetHashValue(hPtr, (ClientData)procPtr);
    }
}
