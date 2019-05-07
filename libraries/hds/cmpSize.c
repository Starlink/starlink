#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpSize

*  Purpose:
*     Component size enquiry.
*
*  Synopsis:
*     void cmpSize( HDSLoc *struc, const char *comp, size_t *size, int *status )

*  Description:
*     A size enquiry is made for a structure component.

*  Parameters:
*     struc
*        Locator associated with a structured data object.
*     comp
*        The component name of a primitive object contained in the structure.
*     size
*        Component size.
*     status
*        Global status value.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     07-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.
*-
*/

void cmpSize( HDSLoc *struc, const char *comp, size_t *size, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datSize(loc, size, status);
   datAnnul(&loc, status);
}
