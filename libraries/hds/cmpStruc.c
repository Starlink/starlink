#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpStruc

*  Purpose:
*     Enquire if component is a structure.
*
*  Synopsis:
*     void cmpStruc( HDSLoc *strucloc, const char *comp, hdsbool_t *struc, int *status )

*  Description:
*     A enquiry is made as to whether a structure component is a structure.

*  Parameters:
*     strucloc
*        Locator associated with a structured data object.
*     comp
*        The component name of a primitive object contained in the structure.
*     struc
*        Whether or not the component is a structure.
*     status
*        Global status value.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     07-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.
*-
*/

void cmpStruc( HDSLoc *strucloc, const char *comp, hdsbool_t *struc, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(strucloc, comp, &loc, status);
   datStruc(loc, struc, status);
   datAnnul(&loc, status);
}
