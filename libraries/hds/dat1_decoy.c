#if HAVE_CONFIG_H
#  include <config.h>
#endif

   void dat1_decoy( long int arg1, void *arg2 )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_decoy                                                            */

/* Purpose:                                                                 */
/*    Provide a decoy to inhibit compiler optimisation.                     */

/* Invocation:                                                              */
/*    dat1_decoy( arg1, arg2 )                                              */

/* Description:                                                             */
/*    This function accepts a value and a pointer and does nothing with     */
/*    either. It exists to prevent compiler optimisation occurring in       */
/*    circumstances where it is not wanted. Both arguments are provided and */
/*    will generally refer to the same variable so that the compiler        */
/*    generating a call to this function (1) is forced to explicitly        */
/*    generate a value to pass and (2) cannot know what value has been      */
/*    returned.                                                             */

/* Parameters:                                                              */
/*    long int arg1                                                         */
/*       The value.                                                         */
/*    void *arg2                                                            */
/*       The pointer.                                                       */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    22-DEC-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Return without doing anything.                                           */
      return;
   }
