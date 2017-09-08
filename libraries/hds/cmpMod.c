#include <string.h>
#include "sae_par.h"
#include "hds1.h"
#include "hds.h"
#include "dat_par.h"

void cmpMod( HDSLoc *struc, const char *comp, const char *type,
             int ndim, const hdsdim *dims, int *status ) {
/*
*+
*  Name:
*     cmpMod

*  Purpose:
*     Create new object, or alter existing one to match specification.

*  Synopsis:
*     void cmpMod( HDSLoc *struc, const char *comp, const char *type,
*                  int ndim, const hdsdim *dims, int *status )

*  Description:
*     A structure component with the specified type and dimensions
*     is procured.   If no such component exists, then one is created.
*     If an unsuitable object exists, it is altered or replaced.

*  Parameters:
*     struc
*        Locator associated with a structured data object.
*     name
*        The name of the component to be created in the structure.
*     type
*        The required data type.
*     ndim
*        The number of object dimensions.
*     dims
*        Point to array containing the object dimensions.
*     staus
*        Global status.

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     03-AUG-2017 (DSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

/* Local variables: */
   HDSLoc *loc = NULL;
   char atype[ DAT__SZTYP + 1 ];
   hdsdim adims[ DAT__MXDIM ];
   int i;
   int nadim;
   int there;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If a component with the reqired name does not already exists, create
   one. */
   datThere( struc, comp, &there, status );
   if( !there ){
      datNew( struc, comp, type, ndim, dims, status );

/* Otherwise, get the type and shape of the existing component. */
   } else {
      datFind( struc, comp, &loc, status );
      datType( loc, atype, status );
      datShape( loc, DAT__MXDIM, adims, &nadim, status );
      datAnnul( &loc, status );

/* See if the type and shape of the existing component match the required
   type and shape. */
      int ok = 1;
      if( strcasecmp( atype, type ) ){
         ok = 0;
      } else if( nadim != ndim ){
         ok = 0;
      } else {
         for( i = 0; i < ndim; i++ ){
            if( adims[ i ] != dims[ i ] ){
               ok = 0;
               break;
            }
         }
      }

/* If the type or shape of the existing component does not match the
   required type and shape, erase the existing component and create a new
   one. */
      if( !ok ){
         datErase( struc, comp, status );
         datNew( struc, comp, type, ndim, dims, status );
      }
   }
}


