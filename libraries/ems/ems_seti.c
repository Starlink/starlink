/*+
 *  Name:
 *     emsSeti
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* EMS internal constants */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_seti) ( CHARACTER(token), INTEGER(ivalue)
         TRAIL(token) )
{
   char ctok[EMS__SZNAM+1];

   GENPTR_CHARACTER(token)
   GENPTR_INTEGER(ivalue)

   cnfImpn( token, token_length, EMS__SZNAM, ctok );
   emsSeti( ctok, *ivalue );

   return;
}
