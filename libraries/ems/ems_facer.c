/*+
 *  Name:
 *     emsFacer
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* EMS internal constants */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_facer) ( CHARACTER(token), INTEGER(fstat) TRAIL(token) )
{
   char ctok[EMS__SZNAM+1];      /* Imported token name */

   GENPTR_CHARACTER(token)
   GENPTR_INTEGER(fstat)

   cnfImpn( token, token_length, EMS__SZNAM, ctok );

   emsFacer( ctok, *fstat );

   return;
}
