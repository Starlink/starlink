/*+
 *  Name:
 *     emsSetc
 *
 *  Fortran callable routine
*/
#include <string.h>
#include "ems.h"                       /* ems_ function prototypes */
#include "ems1.h"                      /* ems1_ function prototypes */
#include "ems_par.h"                   /* EMS constants */
#include "ems_sys.h"                   /* EMS internal constants */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_setc) ( CHARACTER(token), CHARACTER(cvalue)
         TRAIL(token) TRAIL(cvalue) )
{
   char ctok[EMS__SZNAM+1];      /* Imported token name */
   char ccval[EMS__SZTOK+1];     /* Imported char value */

   GENPTR_CHARACTER(token)
   GENPTR_CHARACTER(cvalue)

   cnfImpn( token, token_length, EMS__SZNAM, ctok );
   cnfImpn( cvalue, cvalue_length, EMS__SZTOK, ccval );

/* Ensure minimum 1 space */
   if ( ! strlen( ccval ) ) {
      strcpy( ccval, " " );
   }

/* Now set the token string */
   ems1Stok( ctok, ccval );

   return;
}
