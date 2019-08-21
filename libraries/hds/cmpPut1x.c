#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpPut1C

*  Purpose:
*     Write vector structure component values.

*  Synopsis:
*     void cmpPut1C( HDSLoc *struc, const char *comp,
*                    size_t nval, const char *values[], int *status )

*  Description:
*     This routine writes the values associated with a vector primitive
*     component of a structure. There is a routine for each access type,
*
*        cmpPut1D    double precision
*        cmpPut1R    real
*        cmpPut1I    integer
*        cmpPut1L    logical
*        cmpPut1C    character[*N]

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

void cmpPut1C( HDSLoc *struc, const char *comp,
               size_t nval, const char *values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPut1C(loc, nval, values, status);
   datAnnul(&loc, status);
}

void cmpPut1D( HDSLoc *struc, const char *comp,
               size_t nval, const double values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPut1D(loc, nval, values, status);
   datAnnul(&loc, status);
}

void cmpPut1I( HDSLoc *struc, const char *comp,
               size_t nval, const int values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPut1I(loc, nval, values, status);
   datAnnul(&loc, status);
}

void cmpPut1L( HDSLoc *struc, const char *comp,
               size_t nval, const hdsbool_t values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPut1L(loc, nval, values, status);
   datAnnul(&loc, status);
}

void cmpPut1R( HDSLoc *struc, const char *comp,
               size_t nval, const float values[], int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datPut1R(loc, nval, values, status);
   datAnnul(&loc, status);
}
