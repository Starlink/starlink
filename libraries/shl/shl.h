/*
 * Name:
 *    shl.h
 *
 * Purpose:
 *    Header file for defining SHL C function prototypes
 *
 * Language:
 *    ANSI C
 *
 * Authors:
 *    TIMJ: Tim Jenness (JAC, Hawaii)
 *    {enter_new_authors_here}
 *
 * History:
 *    24-JUL-2004 (TIMJ):
 *       Original version.
 *    28-JUL-2004 (TIMJ):
 *       Add isenv argument to shl_standalone.
 *       Return value indicates success or failure.
 *    18-JAN-2011 (TIMJ):
 *       Use const for shl_standalone
 *    {enter_further_changes_here}
 *
 */

/* size_t */
#include <stddef.h>

/* Pure C interfaces */

int shl_standalone( const char * hlp_library, int isenv, int argc, char ** argv );

/* Non-adam interfaces */

void shlTrnvar( const char libnam[], int isenv, char libray[],
		size_t libraylen, int * status);

void shlGethlp( const char helplb[], const char keywrd[],
		int inter, int * status );

/* ADAM interfaces */

void shlAdam( const char libnam[], int isenv, int * status );
