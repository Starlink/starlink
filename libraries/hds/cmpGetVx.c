#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"

/*
*+
*  Name:
*     cmpGetVC

*  Purpose:
*     Read component values as if it were a vector.

*  Synopsis:
*     void cmpGetVC( HDSLoc *struc, const char *comp, size_t maxval, size_t bufsize,
*                    char *buffer, char *pntrs[], size_t *actval, int *status )

*  Description:
*     This routine reads the values from a primitive component of a
*     structure as if it were vectorized (i.e. regardless of its
*     actual dimensionality).
*     There is a routine for each access type,

*        cmpGetVD    double precision
*        cmpGetVR    real
*        cmpGetVI    integer
*        cmpGetVL    logical
*        cmpGetVC    character[*N]

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

void cmpGetVC( HDSLoc *struc, const char *comp, size_t maxval, size_t bufsize,
               char *buffer, char *pntrs[], size_t *actval, int *status) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGetVC(loc, maxval, bufsize, buffer, pntrs, actval, status);
   datAnnul(&loc, status);
}

void cmpGetVD( HDSLoc *struc, const char *comp, size_t maxval,
               double values[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGetVD(loc, maxval, values, actval, status);
   datAnnul(&loc, status);
}

void cmpGetVI( HDSLoc *struc, const char *comp, size_t maxval,
               int values[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGetVI(loc, maxval, values, actval, status);
   datAnnul(&loc, status);
}

void cmpGetVL( HDSLoc *struc, const char *comp, size_t maxval,
               int values[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGetVL(loc, maxval, values, actval, status);
   datAnnul(&loc, status);
}

void cmpGetVR( HDSLoc *struc, const char *comp, size_t maxval,
               float values[], size_t *actval, int *status ) {
   HDSLoc *loc = NULL;
   if (*status != SAI__OK) {
      return;
   }
   datFind(struc, comp, &loc, status);
   datGetVR(loc, maxval, values, actval, status);
   datAnnul(&loc, status);
}
