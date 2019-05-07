#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpLen

*  Purpose:
*     Component length enquiry.
*
*  Synopsis:
*     void cmpLen( HDSLoc *struc, const char *comp, size_t *len, int *status)

*  Description:
*     A length enquiry is made for a structure component.

*  Parameters:
*     struc
*        Locator associated with a structured data object.
*     comp
*        The component name of a primitive object contained in the structure.
*     len
*        The number of bytes per element.
*     status
*        Global status value.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     07-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.
*-
*/

void cmpLen( HDSLoc *struc, const char *comp, size_t *len, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datLen(loc, len, status);
   datAnnul(&loc, status);
}
