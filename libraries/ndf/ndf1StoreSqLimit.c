#include "ndf1.h"

/* Declare the global variables to hold the required values. */
double Ndf_SQLIMD;
float Ndf_SQLIMF;
int Ndf_SQLIMI;
int64_t Ndf_SQLIMK;
short int Ndf_SQLIMW;
unsigned short int Ndf_SQLIMUW;
char Ndf_SQLIMB;
unsigned char Ndf_SQLIMUB;

void ndf1StoreSqLimit( void ){
/*
*+
*  Name:
*     ndf1StoreSqlimit

*  Purpose:
*     Store the highest value that can be squared without overflow for
*     each generic data type.

*  Synopsis:
*     void ndf1StoreSqLimit( void )

*  Description:
*     For each generic data type, this function calculates the highest
*     value that can be squared without overflow and stores the result in
*     global variables. It should be called once only as part of the
*     process of initialising the NDF library.

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version.

*-
*/

   ndf1SqLimitD( &Ndf_SQLIMD );
   ndf1SqLimitF( &Ndf_SQLIMF );
   ndf1SqLimitI( &Ndf_SQLIMI );
   ndf1SqLimitK( &Ndf_SQLIMK );
   ndf1SqLimitW( &Ndf_SQLIMW );
   ndf1SqLimitUW( &Ndf_SQLIMUW );
   ndf1SqLimitB( &Ndf_SQLIMB );
   ndf1SqLimitUB( &Ndf_SQLIMUB );

}
