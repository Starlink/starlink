#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+HDSERASE.C-*/

/* Include files */

#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "cnf.h"		 /* F77 <-> C string handling functions	    */
#include "ems.h"		 /* EMS error reporting routines	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Control Blocks */


   F77_INTEGER_FUNCTION(hds_erase)
                       (locator_str,status,locator_lenarg)

/*==================================*/
/* HDS_ERASE - Erase container file */
/*==================================*/

struct STR		*locator_str;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "HDS_ERASE_ERR"
#define context_message\
        "HDS_ERASE: Error marking an HDS container file for deletion."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP	   	*lcp;
struct LCP_DATA		*data;

/* Enter routine.	 */

if(!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import the locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the locator is not associated with a top-level object.	    */
if (data->level != 0)
	_call(DAT__OBJIN)

/* Otherwise, mark the associated container file for deletion and annul the */
/* Locator Control Packet (this will close the file if its reference count  */
/* falls to zero).							    */
      rec_mark_delete( &data->han, &hds_gl_status );
      dat1_annul_lcp( &lcp );

/* Nullify the locator value.						    */
      cnf_expn( DAT__NOLOC, DAT__SZLOC, (char *) locator.body,
                (int) locator.length );

/* Exit the routine.							    */
      return hds_gl_status;
   }
