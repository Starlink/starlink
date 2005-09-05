#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATCCOPY.C-*/

/* Include files */
#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "hds.h"                 /* HDS public C interface                  */

/*=====================================*/
/* DatCcopy - Copy one structure level */
/*=====================================*/

int
datCcopy(char locator1[DAT__SZLOC],
         char locator2[DAT__SZLOC],
         char *name,
         char locator3[DAT__SZLOC],
         int  *status )
{
/*
 * This is a direct translation into C of the HDS source file dat_ccopy.f
 * See that file for all comments about the function of this routine!
 * C version - BKM 20020821 - NOT YET TESTED!
 */
 
/* Local variables */
   int structure;                             /* Structure flag */
   int state;                                 /* State flag     */
   int ndim;                                  /* Number of axes */
   HDS_PTYPE dims[DAT__MXDIM];                  /* Axes sizes     */
   char type[DAT__SZTYP+1];

/* Routine entry */
   if( !_ok(*status) )
      return *status;
     
   datStruc( locator1, &structure, status );
   if( !_ok(*status) )
      return *status;
   if( !structure ) {
      datState( locator1, &state, status );
      if ( state )
         datCopy( locator1, locator2, name, status );
      else {
         datType( locator1, type, status );
         datShape( locator1, DAT__MXDIM, dims, &ndim, status );
         datNew( locator2, name, type, ndim, dims, status );
      }
   } else {
      datType( locator1, type, status );
      datShape( locator1, DAT__MXDIM, dims, &ndim, status );
      datNew( locator2, name, type, ndim, dims, status );
   }
   datFind( locator2, name, locator3, status );

   return *status;
}
       
