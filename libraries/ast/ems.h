/*+
 *  Name:
 *     ems.h

 *  Purpose:
 *     EMS_ C interface header file.

 *  Language:
 *     Starlink ANSI C

 *  Description:
 *     This include file contains the function prototypes for all
 *     EMS C interface routines and defines EMS__VERSN to be the major
 *     version number

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     19-SEP-1990 (PCTR):
 *        Original version.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     5-OCT-1993 (PCTR):
 *        Updated for Vn. 1.2-3
 *     28-SEP-1994 (AJC):
 *        V1.4 Added ems_facer_c and ems_errno_c
 *     21-JUN-1995 (AJC):
 *        V1.5 Added ems1_starf_c
 *     13-MAY-1999 (AJC):
 *        Added the emsXxx form of name
 *        and #define old_names = new_names
 *        Removed ems_tune/gtune/show/_c
 *        Added ems1_get_facility_error
 *     27-JUL-2001 (AJC):
 *        Removed emsFmtx
 *        Add emsExpnd, emsTune
 *     13-AUG-2001 (AJC):
 *        Removed emsFioer
 *        #define EMS__VERSN
 *     20-SEP-2001 (AJC):
 *        Added emsSetnc and point ems_setc_c at it
 *      3-MAR-2006 (TIMJ):
 *        Add emsSetu / emsSetp / emsSeti64
 *     30-JUL-2008 (PWD):
 *        Added emsGtune.
 *     31-JUL-2008 (PWD):
 *        Added emsStune and changed emsGtune to return the value as a result.
 *        Marked emsTune as deprecated.
 *     10-SEP-2008 (TIMJ):
 *        Remove fortran prototypes. Should not be in a public include file.
 *     16-SEP-2008 (TIMJ):
 *        Remove 3 arg version of emsSetc
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *- */

#ifndef EMS_DEFINED
#define EMS_DEFINED

/* ANSI types */
#include <stdarg.h>
#include <stddef.h>
#include <inttypes.h>


/* EMS Major Version */
#define EMS__VERSN 2

/* Function Prototypes: */
void emsAnnul( int *status );

void emsBegin( int *status );

void emsEload( char *param,
               int *parlen,
               char *opstr,
               int *oplen,
               int *status );

void emsEnd( int * status );

void emsErrno( const char *token,
               int errval );

void emsExpnd( const char *text,
               char *opstr,
               const int maxlen,
               int *oplen,
               int *status );

void emsFacer( const char *token,
               int status );

int emsGtune( const char *key,
              int *status );

void emsLevel( int *level );

void emsMark( void );

void emsMload( const char *msg,
               const char *text,
               char *opstr,
               int *oplen,
               int *status );

void emsRenew( void );

void emsRep( const char *err,
             const char *text,
             int *status );

void emsRlse( void );

void emsSetc( const char *token,
              const char *cvalue );

void emsSetnc( const char *token,
               const char *cvalue,
               int mxchar );

void emsSetd( const char *token,
              double dvalue );

void emsSeti( const char *token,
              int ivalue );

void emsSeti64( const char *token,
                int64_t ivalue );

void emsSetl( const char *token,
              int lvalue );

void emsSetr( const char *token,
              float rvalue );

void emsSetp( const char *token,
              void * pvalue );

void emsSetu( const char *token,
              unsigned int ivalue );

void emsStat( int *status );

void emsSyser( const char *token,
               int systat );

int emsStune( const char *key,
              const int value,
              int *status );

/*  Deprecated function. */
void emsTune( const char *key,
              const int value,
              int *status );

/* Internal Functions */
/* Not for general use */
int ems1Starf( const char *envar,
               const char *relpath,
               const char *acmode,
               char **filename,
               int *pathlen );

void ems1_get_facility_error( unsigned int errcode,
                              char **facility_name,
                              char **error_ident,
                              char **error_text );

/* Required by MERS. Not to be used by anyone else */

void ems1Rform( const char *text, const int maxlen, int *iposn, char *string, int *strlength  );

void ems1Gesc( const char *escchr, const char *string, int *iposn );

void ems1Gnam( const char *string, int *iposn, char *name, int *namlen, int *status);

#endif	/* EMS_DEFINED */
