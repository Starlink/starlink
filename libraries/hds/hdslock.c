#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+HDSLOCK.C-*/

/* Include files */
#include "f77.h"                 /* F77 <-> C interface macros              */
#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/*================================*/
/* HDS_LOCK - Lock container file */
/*================================*/

int
hdsLock(HDSLoc *locator,
        int *status)

{
#undef context_name
#undef context_message
#define context_name "HDS_LOCK_ERR"
#define context_message\
        "HDS_LOCK: Error locking an HDS container file."

   struct LCP      *lcp;
   struct LCP_DATA *data;

/* Enter routine.       */

   if(!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the locator.       */

   dat1_import_loc(locator, &lcp );
   data = &lcp->data;

/* Return if the locator is not associated with a top-level object.     */

   if (data->level != 0)
      _call(DAT__OBJIN)

/* Lock the container file for exclusive write-access.  */

   _call( rec_lock( &data->han ))

   return hds_gl_status;
}

/*================================*/
/* HDS_FREE - Free container file */
/*================================*/

int
hdsFree(HDSLoc *locator,
        int *status)

{
#undef context_name
#undef context_message
#define context_name "HDS_FREE_ERR"
#define context_message\
        "HDS_FREE: Error releasing locks on an HDS container file."

   struct LCP      *lcp;
   struct LCP_DATA *data;

/* Enter routine.        */

   if(!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the locator.       */

   dat1_import_loc(locator, &lcp );
   data = &lcp->data;

/* Return if the locator is not associated with a top-level object.     */

   if (data->level != 0)
      _call(DAT__OBJIN)

/* Unlock the container file    */

   _call( rec_unlock( &data->han ))

   return hds_gl_status;
}
