/*+
 *  Name:
 *     ems_annul
 *
 *  Fortran callable routine
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_annul) ( INTEGER(status) ) {

   GENPTR_INTEGER(status)

   emsAnnul(status);
   return;
}
