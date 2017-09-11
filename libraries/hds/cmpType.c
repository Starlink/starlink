#include "sae_par.h"
#include "dat_par.h"
#include "hds.h"

void cmpType( HDSLoc *struc, const char *comp, char type[DAT__SZTYP+1],
              int *status ) {
/*
*+
*  Name:
*     cmpType

*  Purpose:
*     Component Type enquiry.

*  Synopsis:
*     void cmpType( HDSLoc *struc, const char *comp, char type[DAT__SZTYP+1],
*                   int *status )

*  Description:
*     A type enquiry is made for a structure component.

*  Parameters:
*     struc
*        A locator associated with a structured data object.
*     name
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     type
*        Variable to receive the data type.
*     status
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     03-AUG-2017 (DSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

   HDSLoc *tloc = NULL;
   datFind( struc, comp, &tloc, status );
   datType( tloc, type, status );
   datAnnul( &tloc, status );

}


