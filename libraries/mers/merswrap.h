/*
*+
*  Name:
*     merswrap.h

*  Purpose:
*     Prototypes for public interface.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: Alan Chipperfield (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     16-APR-2006 (TIMJ):
*        Add prolog.
*     12-SEP-2008 (TIMJ):
*        No point in publishing errFioer

*-
*/


#ifndef MERSWRAP_DEFINED
#define MERSWRAP_DEFINED

#include "msg_par.h"
#include <stdarg.h>

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

void errAnnul( int *status );

void errBegin( int *status );

void errClear( int *status );

void errEnd( int *status );

void errFacer( const char *token,
               int status );

void errFlbel( int *status );

void errFlush( int *status );

void errLevel( int *level );

void errLoad( char *param,
              int param_length,
              int *parlen,
              char *opstr,
              int opstr_length,
              int *oplen,
              int *status );

void errMark( void );

void errOut( const char *param,
             const char *text,
             int *status );

void errRep( const char *param,
             const char *text,
             int *status );

void errRepf( const char *param,
              const char *text,
              int *status,
              ... ) __attribute__((format (printf, 2, 4 )));

void errRlse( void );

void errStart( void );

void errStat( int *status );

void errStop( int *status );

void errSyser( const char *token,
               int systat );

void errTune( const char *param,
              int value,
              int *status );

void msgBell( int *status );

void msgBlank( int *status );

void msgBlankif( msglev_t prior, int *status );

int msgFlevok( msglev_t  filter, int *status );

void msgFlusherr( int * status );

/* Gnu compiler can check for format consistency at compile time */
void msgFmt( const char *token,
             const char *format,
             ...) __attribute__((format (printf, 2, 3 )));

void msgIfget( int *status );

void msgIfgetenv( int * status );

msglev_t msgIflev( char * filstr, int * status );

void msgIfset( msglev_t filter,
               int *status );

void msgLoad( const char *param,
              const char *text,
              char *opstr,
              int opstr_length,
              int *oplen,
              int *status );

void msgOut( const char *param,
             const char *text,
             int *status );

void msgOutf( const char *param,
              const char *text,
              int *status,
              ... ) __attribute__((format (printf, 2, 4 )));

void msgOutif( msglev_t prior,
               const char *param,
               const char *text,
               int *status );

void msgOutiff( msglev_t prior,
                const char *param,
                const char *text,
                int *status,
                ... ) __attribute__((format (printf, 3, 5 )));

void msgOutifv( msglev_t prior,
		const char * param,
		const char * text,
		va_list args,
		int * status) __attribute__((format (printf, 3, 0 )));

void msgRenew( void );

void msgSetc( const char *token,
              const char *cvalue );

void msgSetd( const char *token,
              double dvalue );

void msgSeti( const char *token,
              int ivalue );

void msgSetl( const char *token,
              int lvalue );

void msgSetr( const char *token,
              float rvalue );

void msgSync( int *status );

void msgTune( const char *param,
              int value,
              int *status );

#endif  /* MERSWRAP_DEFINED */
