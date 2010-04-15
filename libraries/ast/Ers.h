#ifndef ERSINC
#define ERSINC
#ifdef __cplusplus
extern "C" {
#endif


/*			E r s . h

 *  Module name:
      Ers.h

 *  Function:
	Function header for the Ers routines

 *  Description:
     Should be included by all files using the Ers routines.

 *  Language:
      C

 *  Support: Tony Farrell, AAO

 *  Copyright (c) Anglo-Australian Telescope Board, 1995.
    Not to be used for commercial purposes without AATB permission.

 *     @(#) $Id: Ers.h,v 1.3 2005/05/17 22:21:19 rkackley Exp $


 *  History:
      04-Aug-1992 - TJF - Original version
      25-Sep-1992 - TJF - Update comments
      06-Oct-1992 - TJF - Rewrite for complete Ers package.
      04-Aug-1993 - TJF - maxsize argument to ErsSPrintf needs a type
      28-Sep-1993 - TJF - Use GNUC attribute to flag the ers calls
			  as printf style.  Use drama.h for configuration
			  stuff.

      29-Sep-1993 - TJF - Add Sccs id
      06-Mar-1994 - TJF - Add Task Id stuff.
      05-Feb-1995 - TJF - Add BROADCAST flag
      06-Aug-1996 - TJF - Add const to strings arguments of ErsVSPrintf
      30-May-2001 - TJF - Add ErsSetLogRoutine.
      15-Jun-2001 - TJF - Add ErsGetTaskId.
      {@change entry@}


 */

#ifdef ERS_STANDALONE
/*
 *  DRAMA macros and types used by Ers.  They are defined here when we
 *  are building ers standalone.
 */
#define DVOID void
#define DVOIDP void *
#define DPUBLIC extern
#define DPRIVATE static
#define DCONSTV const
#define DCONSTR const
#define STATUS__OK 0
#define DPROTOTYPES_OK
#define DFLOAT_OK
#define DCONST_I
typedef long int StatusType;
#define StatusOkP(_value_)  (*(_value_) == STATUS__OK)
#else
/*
 *  Include the drama.h file for configuration macros.
 */

#include "drama.h"

#include "status.h"		/* For StatusType etc	*/
#endif

/*
 *  Get around problems in Sparc include files, they are not ANSI compatible
 */
#if defined(__sparc__) && !defined(sparc)
#define sparc 1
#endif

/*
 *  Floating point stuff.  Only used in ErsVSPrintf.
 */
#ifdef DFLOAT_OK
/*
 *  These values taken from bsd floatio.h
 */
#    define ERS_MAXEXP 308
#    define ERS_MAXFRACT 39
#endif

/*
 *  Constants
 */

#define ERS_C_LEN 200		/* Maximum length of reported messages */
#define ERS_C_MAXMSG 30		/* Maximum number of reported messages */

#define ERS_M_NOFMT (1<<0)	/* Message flag masks		*/
#define ERS_M_HIGHLIGHT (1<<1)
#define ERS_M_BELL (1<<2)
#define ERS_M_ALARM (1<<3)
#define ERS_M_BROADCAST (1<<4)


/*
 *  This structure is used to store details of a message
 */
typedef struct {
	    StatusType mesStatus;	    /* Status of message    */
	    unsigned int context;	    /* Context message was written at */
	    int    flags;		    /* Message flags	    */
	    char   message[ERS_C_LEN];	    /* The formated message */
	    } ErsMessageType;

typedef DVOIDP ErsTaskIdType;

#ifdef DPROTOTYPES_OK
/*
 *  This type is that required for log routines - called on each call to
 *  ErsRep with details of a single message.
 *
 *  The argument "logArg" is a user value supplied when ErsStart is called.
 *  It enables the user to pass any appropriate value to the log routine.
 */
typedef DVOID (*ErsLogRoutineType)(
		    DVOIDP logArg,	    /* Supplied to ErsStart  */
		    DCONSTV ErsMessageType * message,/* The message  */
		    StatusType * status);
/*
 *  The type is that requried for the output routine - called to output
 *  the messages to the user.  An array of message may be output by one
 *  call, with count being the number of message to output.
 *
 *  The argument "outArg" is a user value supplied when ErsStart is called.
 *  It enables the user to pass any appropriate value to the log routine.
 */
typedef DVOID (*ErsOutRoutineType)(
		    DVOIDP outArg,	    /* Supplied to ErsStart */
		    unsigned int count,	    /* Number of messages   */
		    DCONSTV ErsMessageType messages[],/* Array of messages */
		    StatusType * status);


/*
 *  Function prototypes.
 *
 *
 *  We can't define these prorotype in the Ers main module unless we have
 *  stdarg.h.
 */
#if !defined(ERS_MAIN) || defined(DSTDARG__OK)
    DPUBLIC DVOID ErsRep(DCONSTV int flags, StatusType * status,
		       DCONSTV char * string , ...)
#ifdef __GNUC__
	__attribute__ ((format (printf, 3, 4)))
#endif
		;
    DPUBLIC DVOID ErsOut(DCONSTV int flags, StatusType * status,
		          DCONSTV char * string, ...)
#ifdef __GNUC__
	__attribute__ ((format (printf, 3, 4)))
#endif
		;
    DPUBLIC int  ErsSPrintf(DCONSTV int maxLength,
			char *string,
			DCONSTV char * fmt,...)
#ifdef __GNUC__
	__attribute__ ((format (printf, 3, 4)))
#endif
		;

#endif /* DSTDARG_OK */

DPUBLIC ErsTaskIdType ErsStart(
		ErsOutRoutineType outRoutine,
		DVOIDP outArg,
		ErsLogRoutineType logRoutine,
		DVOIDP logArg,
		StatusType * status);
DPUBLIC DVOID ErsStop(StatusType * status);
DPUBLIC DVOID ErsPush(void);
DPUBLIC DVOID ErsAnnul(StatusType * status);
DPUBLIC DVOID ErsFlush(StatusType * status);
DPUBLIC DVOID ErsClear(StatusType * status);
DPUBLIC DVOID ErsPop(void);
DPUBLIC DVOID ErsSetLogRoutine(
    ErsLogRoutineType logRoutine,
    DVOIDP logArg,
    ErsLogRoutineType *oldLogRoutine,
    DVOIDP *oldLogArg,
    StatusType * status);

DPUBLIC ErsTaskIdType ErsGetTaskId(StatusType *status);
DPUBLIC DVOID ErsEnableTask(ErsTaskIdType TaskId,
			    ErsTaskIdType * SavedTaskId);
DPUBLIC DVOID ErsRestoreTask(ErsTaskIdType TaskId);


#ifdef DSTDARG_OK
#   include <stdarg.h>
#else
#   include <varargs.h>
#endif
DPUBLIC int ErsVSPrintf(
		 int maxLength,
		 char *string ,
		 DCONSTV char * fmt0,
		 va_list ap);
#else
/* Don't use prorotypes */
typedef DVOID (*ErsLogRoutineType)();
typedef DVOID (*ErsOutRoutineType)();

DPUBLIC DVOID ErsRep();
DPUBLIC DVOID ErsOut();

DPUBLIC DVOID ErsStart();
DPUBLIC DVOID ErsStop();
DPUBLIC DVOID ErsPush();
DPUBLIC DVOID ErsPop();
DPUBLIC DVOID ErsAnnul();
DPUBLIC DVOID ErsFlush();
DPUBLIC DVOID ErsClear();
DPUBLIC DVOID ErsSetLogRoutine();
DPUBLIC ErsTaskIdType ErsGetTaskId();

DPUBLIC int ErsVSPrintf();
DPUBLIC int ErsSPrintf();

DPUBLIC DVOID ErsEnableTask();
DPUBLIC DVOID ErsRestoreTask();


#endif


#ifdef __cplusplus
}
#endif

#endif
