/*+
 *  Name:
 *     ems_par.h

 *  Purpose:
 *     Define the EMS_ public constants.

 *  Language:
 *     Starlink ANSI C

 *  Type of module:
 *     Global constants header file.

 *  Description:
 *     This file contains the definitions of the public global constants
 *     used by the EMS_ system.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     24-JAN-1990 (PCTR):
 *        Original version adapted from MSG_SYS.
 *     10-AUG-1990 (PCTR):
 *        C version.
 *     5-OCT-1993 (PCTR):
 *        Updated for Vn. 1.2-3
 *     6-JAN-1994 (PCTR):
 *        Updated for Vn. 1.3-0
 *     22-MAR-2001 (AJC):
 *        Add EMS__BASE
 *     {enter_further_changes_here}

 *- */

#ifndef EMS_PAR_DEFINED
#define EMS_PAR_DEFINED

/* Global Constants: */
# define EMS__MXMSG 32        /*   Maximum number of messages         */

# define EMS__MXOUT 133       /*   Maximum length of output text      */

# define EMS__SZMSG 200       /*   Maximum length of error message    */
                              /*   text                               */

# define EMS__SZPAR 15        /*   Maximum length of error message    */
                              /*   name                               */

# define EMS__SZTOK 200       /*   Maximum length of message token    */

# define EMS__BASE 1          /*   Base error reporting level         */

# define EMS__TOKEC "^"       /*   Message token escape character     */

#endif /* EMS_PAR_DEFINED */
