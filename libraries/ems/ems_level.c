/*+
 *  Name:
 *     emsLevel
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_level) ( INTEGER(level) )
{
   GENPTR_INTEGER(level)

   emsLevel( level );

   return;
}
