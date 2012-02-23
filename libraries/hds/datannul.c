#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "hds.h"

/*===========================*/
/* DAT_ANNUL - Annul locator */
/*===========================*/
int
datAnnul(HDSLoc **locator,
          int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_ANNUL_ERR"
#define context_message\
        "DAT_ANNUL: Error annulling an HDS locator."

   struct LCP   *lcp;

/*
   Begin a new error context.
*/
   hds_gl_status = *status;
   emsBegin( &hds_gl_status );

/* If the locator is valid, then annul the control packet.                  */

/* Set a new error context                                                  */
   emsMark();

   dat1_import_loc(*locator, &lcp );

   if( hds_gl_status == DAT__LOCER )
      emsAnnul(&hds_gl_status);
   emsRlse();

   if ( lcp != NULL )
   {
      dat1_annul_lcp( &lcp );
   }
/*
   Nullify the locator value.
*/

   dat1_free_hdsloc(locator );

/*
   If an error occurred, then report a contextual error message.
*/
   if ( !_ok( hds_gl_status ) )
   {
      emsRep( context_name, context_message, &hds_gl_status );
   }
/*
   End the error context and return the final status.
*/
   emsEnd( &hds_gl_status );
   *status = hds_gl_status;
   return *status;
}
