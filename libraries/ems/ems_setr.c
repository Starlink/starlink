/*+
 *  Name:
 *     emsSetr
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* EMS internal constants */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_setr) ( CHARACTER(token), REAL(rvalue) TRAIL(token) )
{
   char ctok[EMS__SZNAM+1];       /* Imported token name */

   GENPTR_CHARACTER(token)
   GENPTR_REAL(rvalue)

   cnfImpn( token, token_length, EMS__SZNAM, ctok );
   emsSetr( ctok, *rvalue );

   return;
}
