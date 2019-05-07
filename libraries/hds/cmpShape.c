#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpShape

*  Purpose:
*     Enquire component shape.
*
*  Synopsis:
*     void cmpShape( HDSLoc *struc, const char *comp, int maxdim,
*                    hdsdim dims[], int *actdim, int *status )

*  Description:
*     A shape enquiry is made for a structure component.

*  Parameters:
*     struc
*        Locator associated with a structured data object.
*     comp
*        The component name of a primitive object contained in the structure.
*     maxdim
*        Allocated size of dims[].
*     dims
*        Component dimensions.
*     actdim
*        Actual number of dimensions.
*     status
*        Global status value.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     07-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.
*-
*/

void cmpShape( HDSLoc *struc, const char *comp, int maxdim,
               hdsdim dims[], int *actdim, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datShape(loc, maxdim, dims, actdim, status);
   datAnnul(&loc, status);
}
