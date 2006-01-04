#ifndef HELPIC
#define HELPIC

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

/*
**  - - - - - - -
**   h e l p . h
**  - - - - - - -
**
**  Public HELP system function prototypes
**
**  Last revision:   30-DEC-2005 (TIMJ)
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

int hlpCreh ( int ( * ) ( int, char*, int, char* ), char*, char* );
char* hlpErrmes ( int );
int hlpHelp ( int ( * ) ( char* ), int, char*, char*, int,
              int ( * ) ( char*, char*, int* ),
              int ( * ) ( int, char*, int, char* ) );

  /* Default callbacks */
int hlpNametr ( int, char*, int, char* );
int hlpOutsub ( char* );
int hlpInsub ( char*, char*, int* );


#ifdef __cplusplus
}
#endif

#endif
