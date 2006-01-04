#ifndef HLPSYSIC
#define HLPSYSIC

#ifdef __cplusplus
extern "C" {
#endif

/*
**  - - - - - - - - -
**   h l p s y s . h
**  - - - - - - - - -
**
**  HELP system functions, macros and global data include file.
**
**  Last revision:   27 January 2000
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

/* Function prototypes and error status codes */
#include "help1.h"
#include "help.h"

/* Maximum length of HELP library names (including '\0') */
#define LFNAME 100

/*
** State of HELP system:
**  -1 = initialized and HELP library file closed
**  +1 = open for writing
**  +2 = open for reading
*/
   extern int jhelp;

/* File pointer for HELP library file */
   extern FILE *fphl;

/* Name of currently-open HELP library */
   extern char hlopen[LFNAME];

/* Name of HELP library for next access */
   extern char hlnext[LFNAME];

/* Index & data addresses for next sequential read or write */
   extern long nextx, nextd;

/* Level number for first topic in current and next HELP library */
   extern int levoff, loffnu;

/* Number of characters in HELP library file */
   extern long nchh;

#ifdef __cplusplus
}
#endif

#endif
