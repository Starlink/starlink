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
 *    {enter_further_changes_here}
 *
 */

int shl_standalone( char * hlp_library, int isenv, int argc, char ** argv );

