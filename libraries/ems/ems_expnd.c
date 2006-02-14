/*+
 *  Name:
 *     emsExpnd
 *
 *  Fortran callable routine
*/
#include <stdlib.h>
#include "ems.h"                       /* ems_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_expnd) ( CHARACTER(text), CHARACTER(opstr),
      INTEGER(oplen), INTEGER(status) TRAIL(text) TRAIL(opstr) )
{
   GENPTR_CHARACTER(text)
   GENPTR_CHARACTER(opstr)
   GENPTR_INTEGER(oplen)
   GENPTR_INTEGER(status)

   char *str;                 /* Buffer for expanded string */
   char *ctext;               /* Imported text string */

/* Import the given strings */
   ctext = cnfCreim( text, text_length );
   str = cnfCreat( opstr_length );

   emsExpnd( ctext, str, opstr_length, oplen, status );

   cnfExprt( str, opstr, opstr_length );
   cnfFree( ctext );
   cnfFree( str );
}
