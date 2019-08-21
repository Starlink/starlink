#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpPutVC

*  Purpose:
*     Write structure component values as if it were a vector.

*  Synopsis:
*     void cmpPutVC( HDSLoc *struc, const char *comp,
*                    size_t nval, const char *values[], int *status )

*  Description:
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,
*
*        cmpPutVD    double precision
*        cmpPutVR    real
*        cmpPutVI    integer
*        cmpPutVL    logical
*        cmpPutVC    character[*N]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Parameters:
*     struc
*        Locator associated with a structured data object.
*     comp
*        The component name of a primitive object contained in the structure.
*     nval
*        The number of values.
*     values
*        The values to be written.
*     status
*        Global status value.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     07-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

void cmpPutVC( HDSLoc *struc, const char *comp,
               size_t nval, const char *values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPutVC(loc, nval, values, status);
   datAnnul(&loc, status);
}

void cmpPutVD( HDSLoc *struc, const char *comp,
               size_t nval, const double values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPutVD(loc, nval, values, status);
   datAnnul(&loc, status);
}

void cmpPutVI( HDSLoc *struc, const char *comp,
               size_t nval, const int values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPutVI(loc, nval, values, status);
   datAnnul(&loc, status);
}

void cmpPutVL( HDSLoc *struc, const char *comp,
               size_t nval, const hdsbool_t values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPutVL(loc, nval, values, status);
   datAnnul(&loc, status);
}

void cmpPutVR( HDSLoc *struc, const char *comp,
               size_t nval, const float values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPutVR(loc, nval, values, status);
   datAnnul(&loc, status);
}
