#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */

#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "cnf.h"		 /* F77 <-> C string handling functions	    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   F77_INTEGER_FUNCTION(dat_annul)
                       (locator_str,status,locator_lenarg)

/*===========================*/
/* DAT_ANNUL - Annul locator */
/*===========================*/

   struct STR	 	 *locator_str;
   int	  		 *status;
   int			  locator_lenarg;

   {
#undef context_name
#undef context_message
#define context_name "DAT_ANNUL_ERR"
#define context_message\
        "DAT_ANNUL: Error annulling an HDS locator."

      struct DSC		  locator;
      int			  locator_len = locator_lenarg;
      struct LCP		 *lcp;

/*
   Begin a new error context.
*/
      hds_gl_status = *status;
      ems_begin_c( &hds_gl_status );
/*
   Import the locator string.
*/
      _strimp( &locator, locator_str, &locator_len );

/* If the locator is valid, then annul the control packet.		    */
      if ( _ok( dau_import_loc( &locator, &lcp ) ) )
      {
         dat1_annul_lcp( &lcp );
      }
/*
   Nullify the locator value.
*/
      cnf_expn( DAT__NOLOC, DAT__SZLOC, (char *) locator.body,
                (int) locator.length );
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
