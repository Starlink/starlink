#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+HDSERASE.C-*/

/* Include files */
#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/*==================================*/
/* HDS_ERASE - Erase container file */
/*==================================*/

int
hdsErase(HDSLoc **locator,
         int *status)

{
#undef context_name
#undef context_message
#define context_name "HDS_ERASE_ERR"
#define context_message\
        "HDS_ERASE: Error marking an HDS container file for deletion."

   struct LCP      *lcp;
   struct LCP_DATA *data;

/* Enter routine.        */

   if(!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the locator.       */

   _call(dat1_import_loc(*locator, &lcp ));
   data = &lcp->data;

/* Return if the locator is not associated with a top-level object.         */
   if (data->level != 0)
      _call(DAT__OBJIN)

/* Otherwise, mark the associated container file for deletion and annul the */
/* Locator Control Packet (this will close the file if its reference count  */
/* falls to zero).                                                          */
   rec_mark_delete( &data->han, &hds_gl_status );
   dat1_annul_lcp( &lcp );

/* Nullify the locator value.                                               */
   dat1_free_hdsloc(locator );

/* Exit the routine.                                                        */
   return hds_gl_status;
}
