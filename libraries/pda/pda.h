#if !defined( PDA_INCLUDED )  /* Include this file only once */
#define PDA_INCLUDED
/*
*  Name:
*     pda.h

*  Purpose:
*     Define the C interface to the PDA library.

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

*  Authors:
*     DSB: David .S. Berry

*  History:
*     12-OCT-2005 (DSB):
*        Original version.
*     31-OCT-2005 (DSB):
*        Added pdaRand and pdaRnnor.
*/

void pdaSumsl( int, double *, double *, 
               void (*)( int, double *, int *, double * ),
               void (*)( int, double *, int *, double * ),
               int *, int, int, double * );


float pdaRand();
float pdaRnnor( float, float );

#endif
