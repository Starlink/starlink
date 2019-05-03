#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpGet1C

*  Purpose:
*     Read vector structure component values.

*  Synopsis:
*     void cmpGet1C( HDSLoc *struc, const char *comp, size_t maxval, size_t bufsize,
*                    char *buffer, char *pntrs[], size_t *actval, int *status )

*  Description:
*     This routine reads the values associated with a vector primitive
*     component of a structure. There is a routine for each access type,
*
*        cmpGet1D    double precision
*        cmpGet1R    real
*        cmpGet1I    integer
*        cmpGet1L    logical
*        cmpGet1C    character[*N]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Parameters:
*     struc
*        Locator associated with a structured data object.
*     comp
*        The component name of a primitive object contained in the structure.
*     maxval
*        The size of pntrs.
*     bufsize
*        The size of buffer.
*     buffer
*        An array to receive the values.
*     pntrs
*        An array to be filled with pointers to the start of each string.
*     actval
*        The number of array elements read.
*     status
*        Global status value.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     02-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

void cmpGet1C( HDSLoc *struc, const char *comp, size_t maxval, size_t bufsize,
               char *buffer, char *pntrs[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGet1C(loc, maxval, bufsize, buffer, pntrs, actval, status);
   datAnnul(&loc, status);
}

void cmpGet1D( HDSLoc *struc, const char *comp, size_t maxval,
               double values[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGet1D(loc, maxval, values, actval, status);
   datAnnul(&loc, status);
}

void cmpGet1I( HDSLoc *struc, const char *comp, size_t maxval,
               int values[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGet1I(loc, maxval, values, actval, status);
   datAnnul(&loc, status);
}

void cmpGet1L( HDSLoc *struc, const char *comp, size_t maxval,
               int values[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGet1L(loc, maxval, values, actval, status);
   datAnnul(&loc, status);
}

void cmpGet1R( HDSLoc *struc, const char *comp, size_t maxval,
               float values[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGet1R(loc, maxval, values, actval, status);
   datAnnul(&loc, status);
}
