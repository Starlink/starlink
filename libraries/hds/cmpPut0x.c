#include "sae_par.h"
#include "hds1.h"
#include "hds.h"
#include "dat_par.h"

void cmpPut0C( HDSLoc *struc, const char *comp, const char *value,
               int *status ) {
/*
*+
*  Name:
*     cmpPut0C

*  Purpose:
*     Write scalar structure component value.

*  Synopsis:
*     void cmpPut0C( HDSLoc *struc, const char *comp, const char *value,
*                    int *status )

*  Description:
*     This routine writes a value into a scalar primitive component
*     of a structure. There is a routine for each access type:
*
*        cmpPut0D    double precision
*        cmpPut0F    real
*        cmpPut0I    integer
*        cmpPut0L    logical
*        cmpPut0C    character[*N]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Parameters:
*     struc
*        Locator associated with a structured data object.
*     name
*        The component name of a primitive object contained in the structure.
*     value
*        The value to be written.
*     status
*        Global status value.

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     03-AUG-2017 (DSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datPut0C( loc, value, status );
   datAnnul( &loc, status );
}


void cmpPut0D( HDSLoc *struc, const char *comp, double value,
               int *status ) {
   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datPut0D( loc, value, status );
   datAnnul( &loc, status );
}


void cmpPut0I( HDSLoc *struc, const char *comp, int value,
               int *status ) {
   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datPut0I( loc, value, status );
   datAnnul( &loc, status );
}

void cmpPut0L( HDSLoc *struc, const char *comp, int value,
               int *status ) {
   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datPut0L( loc, value, status );
   datAnnul( &loc, status );
}

void cmpPut0R( HDSLoc *struc, const char *comp, float value,
               int *status ) {
   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datPut0R( loc, value, status );
   datAnnul( &loc, status );
}

