/*+
 *  Name:
 *     emsEnd
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_end) ( INTEGER(status) )
{
   GENPTR_INTEGER(status)

   emsEnd( status );

   return;
}
