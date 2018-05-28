#if !defined( CHR_INCLUDED )   /* Protect against multiple inclusion*/
#define CHR_INCLUDED 1

/*
*  Name:
*     prm.h

*  Purpose:
*     Defines the public interface for the CHR library.

*  Description:
*     This file defines all the public function prototypes
*     provided by the C version of CHR.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     15-MAY-2018 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

#include <string.h>

int chrIsalm( char cvalue );
int chrIsnam( const char *string );
int chrSimlr( const char *str1, const char *str2 );
size_t chrLen( const char *string );
void chrAppnd( const char *str1, char *str2, size_t str2_length, size_t *iposn );
void chrClean( char *string );
void chrCtod( const char *string, double *dvalue, int *status );
void chrCtoi( const char *string, int *ivalue, int *status );
void chrCtor( const char *string, float *rvalue, int *status );
void chrFandl( const char *string, size_t *index1, size_t *index2 );
void chrFill( char cvalue, char *string, size_t string_length );
void chrFparx( const char *str, char oppar, char clpar, size_t *f, size_t *l );
void chrItoc( int ivalue, char *string, size_t string_length, size_t *nchar );
void chrLdblk( char *string );
void chrPutc( const char *str1, char *str2, size_t str2_length, size_t *iposn );
void chrPuti( int ivalue, char *string, size_t string_length, size_t *iposn );
void chrRmblk( char *string );
void chrSizetoc( size_t value, char *string, size_t string_length, size_t *nchar );
void chrUcase( char *string );

#endif
