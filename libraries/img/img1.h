/*
 * Name:
 *    img1.h
 *
 * Purpose:
 *    Header file for defining internal IMG function prototypes.
 *
 * Language:
 *    ANSI C
 *
 * Authors:
 *    PDRAPER: P.W. Draper (STARLINK - Durham University)
 *    {enter_new_authors_here}
 *
 * History:
 *    17-MAY-1996 (PDRAPER):
 *       Original version.
 *    {enter_further_changes_here}
 *    
 */

#define MAXFSTRING 132     /*  Maximum length of Fortran string */

void img1StoreArrayString( const char *string, const char *id,
                           const int position,  char *stringarray[],
                           int *status );

void img1FreeStringArray( const char *id, int *status );

void img1ExtractParam( const char *string, const int n, char *value,
                       int *status );

int img1CountParams( const char *string, int *status );

/* $Id$ */
