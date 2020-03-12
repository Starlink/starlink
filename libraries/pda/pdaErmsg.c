#include "ems.h"

void pdaErmsg( const char *subnam, int indic, int level, const char *messg,
               char flag ){
/*
*+
*  Name:
*     pdaErmsg

*  Purpose:
*     Process error messages for SLATEC and other libraries.

*  Synopsis:
*     void pdaErmsg( const char *subnam, int indic, int level, const char *messg,
*                    char flag, int *status )

*  Description:
*     Reports an error using EMS.

*  Parameters:
*     subnam
*        Pointer to a null terminated string holding the name of the
*        subfunction calling "pdaErmsg". This will form part of the
*        message put out.
*     indic
*        Ignored.
*     level
*        Ignored.
*     messg
*        Pointer to a null terminated string holding the principal error or
*        warning message.
*     flag
*        Ignored.

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     12-MAR-2020 (DSB):
*        Original version.

*-
*/

/* Set the local status. */
   int status = 1;

/* Report the error. */
   emsSetc( "pdaErmsg", subnam );
   emsSetc( "pdaErmsg", ": " );
   emsSetc( "pdaErmsg", messg );
   emsRep( " ", "^pdaErmsg", &status );

}

