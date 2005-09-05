#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec_locate_fns( const struct HAN *han, const char **fns )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_locate_fns                                                        */

/* Purpose:                                                                 */
/*    Obtain a pointer to the file name string for a container file.        */

/* Invocation:                                                              */
/*    rec_locate_fns( han, fns )                                            */

/* Description:                                                             */
/*    This function returns a pointer to a file name string for an HDS      */
/*    container file. A handle to a record in the file must be supplied.    */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for any record in   */
/*       the container file.                                                */
/*    const char **fns                                                      */
/*       Pointer to a char pointer which will be set to point at a          */
/*       null-terminated file name string for the container file. This      */
/*       string must not be altered by the calling routine. A null pointer  */
/*       value will be returned under error conditions.                     */

/* Returned Value:                                                          */
/*    int rec_locate_fns                                                    */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    24-APR-1991 (RFWS):                                                   */
/*       Added prologue and error handling and tidied.                      */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Set an initial null value for the returned pointer.                      */
      *fns = NULL;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Return a pointer to the File Name String in the File Control Vector.     */
      *fns = rec_ga_fcv[ han->slot ].name;

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
