/*+
 *  Name:
 *     emsSetl
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* EMS internal constants */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_setl) ( CHARACTER(token), LOGICAL(lvalue)
         TRAIL(token) )
{
   char ctok[EMS__SZNAM+1];       /* Imported token name */

   GENPTR_CHARACTER(token)
   GENPTR_LOGICAL(lvalue)

   cnfImpn( token, token_length, EMS__SZNAM, ctok );
   
   emsSetl( ctok, F77_ISTRUE(*lvalue) );

   return;
}
