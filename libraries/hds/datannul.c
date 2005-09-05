#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/*===========================*/
/* DAT_ANNUL - Annul locator */
/*===========================*/
int
datAnnul(char locator_str[DAT__SZLOC],
          int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_ANNUL_ERR"
#define context_message\
        "DAT_ANNUL: Error annulling an HDS locator."

   struct DSC locator;
   struct LCP   *lcp;

/*
   Begin a new error context.
*/
   hds_gl_status = *status;
   ems_begin_c( &hds_gl_status );
/*
   Import the locator string.
*/
   _strflcsimp( &locator, locator_str, DAT__SZLOC );

/* If the locator is valid, then annul the control packet.                  */
   if ( _ok( dau_import_loc( &locator, &lcp ) ) )
   {
      dat1_annul_lcp( &lcp );
   }
/*
   Nullify the locator value.
*/

/*   cnf_expn( DAT__NOLOC, DAT__SZLOC, (char *) locator.body,
 *             (int) locator.length );
 */
     strncpy( (char *) locator.body, DAT__NOLOC,
                DAT__SZLOC );
 
/*
   If an error occurred, then report a contextual error message.
*/
   if ( !_ok( hds_gl_status ) )
   {
      ems_rep_c( context_name, context_message, &hds_gl_status );
   }
/*
   End the error context and return the final status.
*/
   ems_end_c( &hds_gl_status );
   *status = hds_gl_status;
   return *status;
}
