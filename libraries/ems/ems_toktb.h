/*+
*  Name:
*     EMSTK_CMN

*  Purpose:
*     Define the message token table.

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     Global variables include file.

*  Description:
*     Define the common blocks holding the message token table for the 
*     EMS_ subroutine library.

*  Authors:
*     SLW: Sid Wright (UCL)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1983 (SLW):
*        Original version.
*     13-SEP-1989 (PCTR):
*        Added prologue and improved layout.
*     7-DEC-1989 (PCTR):
*        EMS_ version taken from MSGTK_CMN.
*     23-OCT-1990 (PCTR):
*        Added TOKLEV pointer.
*     19-JUN-1991 (PCTR):
*        Added token persistence flag.
*     {enter_further_changes_here}


*-
*/
#ifndef EMS_TOKTB_DEFINED
#define EMS_TOKTB_DEFINED
#include "ems_sys.h"
        
extern int toklev;                   /* Token context level */
extern int tokmrk;                   /* Number of markers */
extern int tokcnt[EMS__MXLEV+1];       /* Number of messages in table */
extern int tokhiw[EMS__MXLEV+1];       /* Token table high water mark */
extern int toklen[EMS__MXTOK+1];       /* Token string lengths */

extern char toknam[EMS__MXTOK+1][EMS__SZNAM+1];   /* Token names */
extern char tokstr[EMS__MXTOK+1][EMS__SZTOK+1];   /* Token strings */

#endif /* EMS_TOKTB_DEFINED */
