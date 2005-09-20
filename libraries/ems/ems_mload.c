/*+
 *  Name:
 *     emsMload
 *
 *  Fortran callable routine
*/
#include <stdlib.h>
#include "ems.h"                       /* ems_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_mload) ( CHARACTER(msg), CHARACTER(text), CHARACTER(opstr),
      INTEGER(oplen), INTEGER(status)
      TRAIL(msg) TRAIL(text) TRAIL(opstr) )
{
   char *str;                 /* Buffer for expanded string */
   char *ctext;               /* Imported text string */

   GENPTR_CHARACTER(msg)
   GENPTR_CHARACTER(text)
   GENPTR_CHARACTER(opstr)
   GENPTR_INTEGER(oplen)
   GENPTR_INTEGER(status)

/* Import the given strings
*  We don't need to import msg because it's not used at lower levels
*/    
   ctext = cnfCreim( text, text_length );
   str = cnfCreat( opstr_length );

   emsMload( msg, ctext, str, oplen, status );

   cnfExprt( str, opstr, opstr_length );
   free( ctext );
   free( str );
}
