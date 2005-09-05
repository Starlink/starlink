#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_locate_name( unsigned char *pcrv, int i, char **name )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_locate_name                                                      */

/* Purpose:                                                                 */
/*    Locate a component name in a Component Record Vector.                 */

/* Invocation:                                                              */
/*    dat1_locate_name( pcrv, i, name )                                     */
/*                                                                          */
/* Description:                                                             */
/*    This function returns a pointer to a component name within a          */
/*    specified element of a packed Component Record Vector.                */

/* Parameters:                                                              */
/*    unsigned char *pcrv                                                   */
/*       Pointer to the start of a packed Component Record Vector.          */
/*    int i                                                                 */
/*       Number of the Component Record Vector element whose name is        */
/*       required (zero-based).                                             */
/*    char **name                                                           */
/*       Pointer to a char pointer which will be set to point at the start  */
/*       of the required name field within the CRV. A null pointer value    */
/*       will be returned under error conditions.                           */

/* Returned Value:                                                          */
/*    int dat1_locate_name                                                  */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STRALINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    17-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    19-APR-2004 (BKM):                                                    */
/*       Revised for 64-bit HDS files.                                      */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Set an initial null value for the returned pointer.                      */
      *name = NULL;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Return a pointer to the start of the name field in the required          */
/* Component Record Vector element.                                         */
      *name = (char *) ( pcrv + ( i * SZCRV ) ); 

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
