/*+
 *  Name:
 *     ems_sys.h

 *  Purpose:
 *     EMS C Interface private macro definitions.

 *  Language:
 *     Starlink ANSI C

 *  Type of module:
 *     Macro definitions header file.

 *  Description:
 *     This include file contains the machine-specific definitions
 *     used in the compilation of the EMS C TRAIL(erface routines.

 *  System-specific:
 *     This header file is the "unknown" system version of ems.h.
 *     It is actually good for sun4, sun4_Solaris, mips and alpha_OSF1.

 *  Authors:
 *     R.T.Platon (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *      2-MAR-2001 (RTP):
 *        Original version
 *     14-MAR-2001 (AJC):
 *        Add OP_STREAM
 *        Remove EMS__BASE
 *     14-AUG-2001 (AJC):
 *        Remove EMS__VERSN (to ems.h)
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#ifndef EMS_SYS_DEFINED
#define EMS_SYS_DEFINED
#include <stdio.h>
#include <string.h>

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

typedef short Logical;

#define MIN(a,b) ( a > b ? b : a )
#define MAX(a,b) ( a > b ? a : b )

#define  DEBUG           0
#define  TRACE           0
#define  PRINT_ERRMESS   0
#define  OP_STREAM       stdout

#if DEBUG
#   undef DEBUG
#   define DEBUG(x,y,z)  (void) fprintf(OP_STREAM, "Debug (%s): ", x );\
    (void) fprintf(OP_STREAM, y, z ); (void) fprintf(OP_STREAM, "\n" ); \
    fflush(OP_STREAM);
#else
#   undef DEBUG
#   define DEBUG(x,y,z)
#endif

#if TRACE
#   undef TRACE
#   define TRACE(x)(void) fprintf(OP_STREAM,"Entering Function '%s'...\n", x );\
    fflush(OP_STREAM);
#else
#   undef TRACE
#   define TRACE(x)
#endif

#define EMS__MXLEV 256            /* Maximum context level */

#define EMS__MXTOK 64             /* Maximum number of tokens */

#define EMS__SZFMT 200            /* Size of FORMAT text */

#define EMS__SZNAM 15             /* Size of message token name */

#define EMS__SZOUT 79             /* Size of an output string */

#define EMS__SZBUF 512            /* Size of static buffer */

#endif
