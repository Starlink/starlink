#ifndef HELP1IC
#define HELP1IC

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

/*
**  - - - - - - -
**   h e l p 1 . h
**  - - - - - - -
**
**  Private HELP system function prototypes and error codes include file.
**
**  Last revision:   30-DEC-2005 (TIMJ)
**
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

int hlpComstr ( char*, char* );
char* hlpCopyn ( char*, char*, int );
long hlpDec ( char*, int* );
int hlpFopr ( int ( * ) ( int, char*, int, char* ), char*, FILE** );
void hlpHchkl ( char*, int*, char* );
int hlpHclose ( void );
int hlpHdread ( int, long*, char*, int* );
int hlpHdwrit ( char*, long* );

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
int hlpLength ( char* );
int hlpLinout ( int ( * ) ( char* ), int, int, char* );
void hlpSplit ( char*, int, int*, int* );
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
