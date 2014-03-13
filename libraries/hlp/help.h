#ifndef HELPIC
#define HELPIC

/*
**  - - - - - - -
**   h e l p . h
**  - - - - - - -
**
**  HELP system function prototypes and error codes include file.
**
**  Last revision:   12 March 2014
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#include <stdlib.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

int hlpComstr ( char*, char* );
int hlpCreh ( int ( * ) ( int, char*, int, char* ), char*, char* );
long hlpDec ( char*, int* );
char* hlpErrmes ( int );
int hlpFopr ( int ( * ) ( int, char*, int, char* ), char*, FILE** );
void hlpHchkl ( char*, int*, char* );
int hlpHclose ( void );
int hlpHdread ( int, long*, char*, int* );
int hlpHdwrit ( char*, long* );
int hlpHelp ( int ( * ) ( char* ), int, char*, char*, int,
              int ( * ) ( char*, char*, int* ),
              int ( * ) ( int, char*, int, char* ) );
void hlpHinit ( char* );
int hlpHleap ( int ( * ) ( int, char*, int, char* ),
                                   int, char*, char*, long*, int* );
int hlpHopenr ( int ( * ) ( int, char*, int, char* ) );
int hlpHopenw ( int ( * ) ( int, char*, int, char* ), long );
int hlpHreadd ( int, char*, int* );
int hlpHreadx ( int ( * ) ( int, char*, int, char* ),
                                            int, int, char*, int* );
void hlpHseekx ( char*, long, int );
void hlpHtellx ( char*, long*, int* );
int hlpIndex ( char*, char* );
int hlpInsub ( char*, char*, int* );
int hlpLength ( char* );
int hlpLinout ( int ( * ) ( char* ), int, int, char* );
int hlpNametr ( int, char*, int, char* );
int hlpOutsub ( char* );
void hlpSplit ( char*, int, int*, int* );
char* hlpStrncp ( char*, const char*, size_t );
char *hlpTrim ( char* );
char *hlpUpcase ( char* );

/* Status codes */
#define hlp_ILLEGAL_STATE     -1
#define hlp_OPEN_ERROR        -2
#define hlp_WRITE_ERROR       -3
#define hlp_READ_ERROR        -4
#define hlp_CLOSE_ERROR       -5
#define hlp_WRITE_WIDE        -6
#define hlp_READ_WIDE         -7
#define hlp_RECORD_OVERSIZE   -8
#define hlp_CREATION_FAILURE  -9
#define hlp_INTERNAL_ERROR   -10
#define hlp_ILLEGAL_LEVEL    -11
#define hlp_LINE_OUTPUT_BAD  -12
#define hlp_LINE_INPUT_BAD   -13
#define hlp_BAD_INDEX        -14
#define hlp_SELF_REF         -15
#define hlp_STRING_OVERFLOW  -16
#define hlp_TRANSLATE_ERROR  -17

#ifdef __cplusplus
}
#endif

#endif
