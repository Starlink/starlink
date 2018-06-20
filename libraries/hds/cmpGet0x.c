#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

void cmpGet0C( HDSLoc *struc, const char *comp, char *value,
               size_t value_length, int *status ) {
/*
*+
*  Name:
*     cmpGet0C

*  Purpose:
*     Read scalar structure component value.

*  Synopsis:
*     void cmpGet0C( HDSLoc *struc, const char *comp, char *value,
*                    size_t value_length, int *status )

*  Description:
*     This routine writes a value into a scalar primitive component
*     of a structure. There is a routine for each access type:
*
*        cmpGet0D    double precision
*        cmpGet0R    real
*        cmpGet0I    integer
*        cmpGet0L    logical
*        cmpGet0C    character[*N]

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
   datGet0C( loc, value, value_length, status );
   datAnnul( &loc, status );
}


void cmpGet0D( HDSLoc *struc, const char *comp, double *value,
               int *status ) {
   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datGet0D( loc, value, status );
   datAnnul( &loc, status );
}


void cmpGet0I( HDSLoc *struc, const char *comp, int *value,
               int *status ) {
   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datGet0I( loc, value, status );
   datAnnul( &loc, status );
}

void cmpGet0L( HDSLoc *struc, const char *comp, int *value,
               int *status ) {
   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datGet0L( loc, value, status );
   datAnnul( &loc, status );
}

void cmpGet0R( HDSLoc *struc, const char *comp, float *value,
               int *status ) {
   HDSLoc *loc = NULL;
   if( *status != SAI__OK ) return;
   datFind( struc, comp, &loc, status );
   datGet0R( loc, value, status );
   datAnnul( &loc, status );
}

