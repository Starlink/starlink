#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>

#include "hds1.h"
#include "rec.h"
#include "dat1.h"

   void dat1_getenv( const char *varname, int def, int *val )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_getenv                                                           */

/* Purpose:                                                                 */
/*    Obtain an integer value from an environment variable.                 */

/* Invocation:                                                              */
/*    dat1_getenv( varname, def, val )                                      */
/*                                                                          */
/* Description:                                                             */
/*    This function obtains the value of a specified environment variable   */
/*    as an integer. If the variable is not defined, or its value does not  */
/*    make sense as an integer, or any other error occurs, then a default   */
/*    value is returned instead.                                            */

/* Parameters:                                                              */
/*    const char *varname                                                   */
/*       Pointer to a null-terminated character string giving the name of   */
/*       the environment variable. The use of upper case is recommended.    */
/*    int def                                                               */
/*       The default value to be used if an integer value cannot be         */
/*       obtained from the environment variable.                            */
/*    int *val                                                              */
/*       Pointer to an integer in which the result will be returned.        */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    This routine does not perform error checking.                         */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    25-FEB-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      char *txt;                 /* Pointer to translation value            */

/*.                                                                         */

/* Obtain a pointer to the environment variable's value.                    */
      txt = getenv( varname );

/* If no value was obtained, then use the default.                          */
      if ( txt == NULL )
      {
         *val = def;
      }

/* Otherwise, try to read an integer value from it. If unsuccessful, then   */
/* use the default.                                                         */
      else if ( sscanf( txt, "%d", val ) != 1 )
      {
         *val = def;
      }

/* Exit the routine.                                                        */
      return;
   }
