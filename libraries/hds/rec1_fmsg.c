#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */

   void rec1_fmsg( const char *token, int slot )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_fmsg                                                             */

/* Purpose:                                                                 */
/*    Assign the name of a container file to a message token.               */

/* Invocation:                                                              */
/*    rec1_fmsg( token, slot )                                              */

/* Description:                                                             */
/*    This function assigns the name of a container file to a message       */
/*    token, for use in constructing error messages.                        */

/* Parameters:                                                              */
/*    const char *token                                                     */
/*       Pointer to a null-terminated character string giving the message   */
/*       token name.                                                        */
/*    int slot                                                              */
/*       The container file slot number in the File Control Vector.         */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    25-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    26-APR-1991 (RFWS):                                                   */
/*       Changed to cater for a null-terminated file name string.           */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Assign the name to the message token.                                    */
      emsSetnc( token, rec_ga_fcv[ slot ].name, EMS__SZTOK );
   }
