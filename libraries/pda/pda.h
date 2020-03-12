#if !defined( PDA_INCLUDED )  /* Include this file only once */
#define PDA_INCLUDED
/*
*+
*  Name:
*     pda.h

*  Purpose:
*     Define the C interface to the PDA library.

*  Language:
*     Starlink C

*  Description:
*     This module defines the C interface to the functions of the PDA
*     library. The file pda.c contains C wrappers for the Fortran
*     PDA routines.

*  Notes:
*     - Given the size of the PDA library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that
*     people who want to use PDA from C extend this file (and
*     pda.c) to include any functions which they need but which are
*     not already included.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David .S. Berry (UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-OCT-2005 (DSB):
*        Original version.
*     31-OCT-2005 (DSB):
*        Added pdaRand and pdaRnnor.
*     11-MAY-2006 (TIMJ):
*        Fix pdaRand prototype.
*     21-OCT-2009 (DSB):
*        Added pdaRnpoi.
*     {enter_further_changes_here}

*-
*/

void pdaErmsg( const char *, int, int, const char *, char );
void pdaXermsg( const char *, const char *, const char *, int, int, int * );
void pdaSumsl( int, double *, double *,
               void (*)( int, double *, int *, double * ),
               void (*)( int, double *, int *, double * ),
               int *, int, int, double * );
void pdaDeflt( int, int *, int, int, double * );
float pdaRand( void );
float pdaRnnor( float, float );
double pdaD1mach( int );
int pdaRnpoi( float );
double pdaDdot( long, double[], long, double[], long );
long pdaIdamax( long, double[], long );
void pdaDaxpy( long, double, double[], long, double[], long );
void pdaDgefa( double*, long, long, long[], long* );
void pdaDgefs( double*, long, long, double*, long, long, long[], long* );
void pdaDgesl( double*, long, long, long[], double[], long );
void pdaDgesld( double*, long, long, long[], double[] );
void pdaDscal( long, double, double[], long );

#endif
