/*+
 *  Name:
 *     emsSyser
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* EMS internal constants */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE (ems_syser)( CHARACTER(token), INTEGER(systat) TRAIL(token) )
{
   char ctok[ EMS__SZNAM + 1];

   GENPTR_CHARACTER(token)
   GENPTR_INTEGER(systat)

   cnfImpn( token, token_length, EMS__SZNAM, ctok );

   emsSyser( ctok, *systat );
   
   return;
}
