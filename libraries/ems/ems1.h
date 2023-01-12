/*+
 *  Name:
 *     ems1.h

 *  Purpose:
 *     EMS_ C internal header file.

 *  Language:
 *     Starlink ANSI C

 *  Description:
 *     This include file contains the function prototypes for all
 *     EMS C internal routines.

 *  Authors:
 *     RTP: R.T.Platon (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     14-FEB-2001 (RTP):
 *        Original version.
 *      2-MAR-2001 (AJC):
 *        Correct various returned variables to pointers
 *        Add maxlen to ems1Putc
 *     13-AUG-2001 (AJC):
 *        Correct ems1Flush(int status) to (int *status)
 *     15-MAY-2008 (PWD):
 *        Add various new functions to support threading.
 *     21-JUL-2008 (TIMJ):
 *        Fix void prototypes
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#ifndef EMS1_DEFINED
#define EMS1_DEFINED

#include <stdarg.h>
#include "ems_sys.h"
#include "ems_defs.h"

Logical ems1Gepnd ( void );
Logical ems1Gtok( const char *namstr, char *tokval, int *tkvlen );
char *ems1Gthreadbuf( void );
ems_msgtab_t *ems1Gmsgtab( void );
ems_msgtab_t *ems1Gmsgtab2( void );
ems_thread_data_t *ems1Ithreaddata( void );
ems_toktab_t *ems1Gtoktab( void );

void ems1Emark( void );
void ems1Erlse( void );
void ems1Estor( const char *param, int plen, const char *msg, int mlen,int *status);
void ems1Estor1( ems_msgtab_t *msgtab, const char *param, int plen, const char *msg, int mlen,int *status);
void ems1Flush( int *status );
void ems1Form( const char *text, const int, Logical esctokval, Logical clean,
               char *opstr, int *oplen, int *status );
void ems1Fthreaddata( void *ptr );
void ems1Imsgtab( ems_msgtab_t *msgtab );
void ems1Itoktab( ems_toktab_t *toktab );
void ems1Kerr( void );
void ems1Ktok (void);
void ems1Mform( const char *text, int iposn, char *string, int strlength  );
void ems1Mpop( void );
void ems1Mpush( void );
void ems1Mrerr( const char *text, int *status );
void ems1Mutc( const char *cvalue, char *string, int iposn, int *status );
void ems1Prerr( const char *text, int *status );
void ems1Putc( const char *cvalue, const int maxlen, char *string, int *iposn, int *status );
void ems1Rep( const char * err, const char * text, Logical useformat,
               va_list args, int *status);
void ems1Serr( char *errstr, int errlen, int *errnum_p );
void ems1Stok( const char *token, const char *string );
void ems1Tblk( void );
void ems1Eblk( void );
Logical ems1Iepnd( void );

#endif	/* EMS1_DEFINED */
