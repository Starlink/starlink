/*+
 *  Name:
 *     emsEload
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_par.h"                   /* EMS constants            */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_eload) ( CHARACTER(param), INTEGER(parlen), CHARACTER(opstr),
            INTEGER(oplen), INTEGER(status) TRAIL(param) TRAIL(opstr)) {

   GENPTR_CHARACTER(param)
   GENPTR_INTEGER(parlen)
   GENPTR_CHARACTER(opstr)
   GENPTR_INTEGER(oplen)
   GENPTR_INTEGER(status)

   char str1[EMS__SZMSG+1];
   char str2[EMS__SZMSG+1];

   emsEload(str1, parlen, str2, oplen, status);
   cnfExprt( str1, param, param_length );
   cnfExprt( str2, opstr, opstr_length );

   return;
}
