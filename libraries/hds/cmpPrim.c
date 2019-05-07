#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpPrim

*  Purpose:
*     Enquire if component is primitive.
*
*  Synopsis:
*     void cmpPrim( HDSLoc *struc, const char *comp, hdsbool_t *prim, int *status )

*  Description:
*     An enquiry is made as to whether or not the structure component
*     is a primitive object.

*  Parameters:
*     struc
*        Locator associated with a structured data object.
*     comp
*        The component name of a primitive object contained in the structure.
*     prim
*        Whether or not the structure component is primitive.
*     status
*        Global status value.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     07-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.
*-
*/

void cmpPrim( HDSLoc *struc, const char *comp, hdsbool_t *prim, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPrim(loc, prim, status);
   datAnnul(&loc, status);
}
