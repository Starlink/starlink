/*
 * Numerical error handling functions for POSIX/C99 systems.
 *
 * Declare functions to establish and revert error handling functions,
 * which are not platform-specific, but use the interfaces described
 * in the Single Unix Specification (aligned with POSIX; see
 * <http://www.unix.org/single_unix_specification/> and the C99
 * Standard, ISO/IEC 9899:1999.
 *
 * Having said that, there's essentially nothing we can do portably.
 * Although there's a standardised interface to floating-point exceptions,
 * there's no portable way to tell the processor to translate these into
 * SIGFPE signals, which is what the functions in this module are
 * intended to support.  There's probably a little more we could do
 * than is implemented below, but not much.
 *
 * Useful other material: Sun's `Numerical Computation Guide' has
 * information about floating-point operations, and contextualises the
 * material specified in the Single Unix spec, though most of the
 * details are specific to Sun.  See <http://docs.sun.com/db/doc/806-3568>
 *
 * It would probably be good to use CNF to declare the functions, but
 * since none of the other modules in this component do so, we
 * shouldn't stand out.
 *
 * Authors:
 *    NG: Norman Gray (Starlink, Glasgow)
 *
 * History:
 *    15-MAR-2004 (NG)
 *       Original version, based rather loosely on the num_han.c_*
 *       platform-dependent originals.
 *    22-FEB-2022 (DSB):
 *       This file has been superceded and is retained only to provide
 *       no-op stubs for functions it defines. Testing for numerical
 *       errors is now done by file num_test.c
 */

/* Configured flags */
#include <config.h>

#if 0
#if HAVE_FENV_H
#include <fenv.h>
#endif
#endif

/* SAE and PRM error codes */
#include <sae_par.h>
#include <prm_err.h>



void num_handl_ ( void(*routin)() )
{
/*
*+
*  Name:
*     NUM_HANDL

*  Purpose:
*     Establish a handler for numerical errors.

*  Language:
*     C

*  Invocation:
*     CALL NUM_HANDL( ROUTIN )

*  Description:
*     This routine declares a signal handler to handle numerical errors
*     such as overflow or divide by zero.

*  Arguments:
*     ROUTIN = SUBROUTINE (Given)
*        The numerical error handler.  It should be declared as
*        EXTERNAL in the routine which calls NUM_HANDL. The routine
*        NUM_TRAP is provided for this purpose.

*  Machine-specific features used:
*     None

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     NG: Norman Gray (Starlink, Glasgow)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1991 (RFWS):
*        Original version.
*     15-MAR-2004 (NG):
*        Portable version
*     {enter_changes_here}
*/

/* Do nothing, in this version (that's nice and portable) */
    return;
}


void num_revrt_( void )
{
    /*
*+
*  Name:
*     NUM_REVRT

*  Purpose:
*     Revert to original numerical error behaviour.

*  Language:
*     C

*  Invocation:
*     CALL NUM_REVRT

*  Description:
*     This routine causes the handling of numerical errors to revert to
*     its original behaviour after a previous call to NUM_HANDL. Note
*     that although matching calls to NUM_HANDL and NUM_REVRT cancel,
*     they may not be nested.

*  Machine-specific features used:
*     None

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Do nothing, in this version (that's nice and portable) */
    return;
}


int matherr()
{
/*+                                                                         */
/* Name:                                                                    */
/*    matherr                                                               */

/* Purpose:                                                                 */
/*    Replace standard maths library matherr routine.                       */

/* Invocation:                                                              */
/*    matherr( );                                                           */
/*                                                                          */
/* Description:                                                             */
/*    This function replaces the standard matherr routine in the maths      */
/*    library. It does nothing and is simply intended to supress the        */
/*    printing of warning messages when argument errors occur in calls to   */
/*    maths library functions.                                              */

/* Parameters:                                                              */
/*    void                                                                  */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    4-NOV-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Exit the routine.                                                        */
  return 1; /* Must return something */
}


/* No need for this more elaborate, and less portable, declaration */
/* void num_trap_( int sig, int code, struct sigcontext *scp, char *addr ) */

void num_trap_( int sig )
{
/*
*+
*  Name:
*     NUM_TRAP

*  Purpose:
*     Handle numerical errors.

*  Language:
*     C

*  Invocation:
*     CALL NUM_HANDL( NUM_TRAP )

*  Description:
*     This routine is intended to be passed to NUM_HANDL as a handler
*     for numerical errors. Once established, any numerical error will
*     be handled by this routine until NUM_REVRT is called. If the
*     global variable NUM_ERROR is set to SAI__OK, NUM_TRAP will set it
*     to a suitable error status value in response to the numerical
*     error, otherwise it will leave its value unchanged.

*  Arguments:
*     There is no need to refer to this routine's arguments in order to
*     use it.

*  Machine-specific features used:
*     None

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Do nothing, in this version (that's nice and portable) */
    return;
}
