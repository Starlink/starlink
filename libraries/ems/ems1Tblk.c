/*
*+
*  Name:
*     ems1Tblk

*  Purpose:
*     Initial contents of the message token table.

*  Language:
*     Starlink ANSI C

*  Type of module:
*     External data initialisation

*  Description:
*     This routine initialises the EMSTK_CMN common blocks to perform 
*     the initialisation of the EMS_ message token table.

*  Authors:
*     BDK: B.D. Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1987 (BDK):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_MBLK
*     {enter_further_changes_here}

*-
*/

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

int toklev = EMS__BASE;
int tokmrk = EMS__BASE;
int tokcnt[ EMS__MXLEV + 1] = { 0 };
int tokhiw[ EMS__MXLEV + 1] = { 0 };
int toklen[ EMS__MXTOK + 1] = { 0 };
char toknam[ EMS__MXTOK + 1][ EMS__SZNAM + 1 ];
char tokstr[ EMS__MXTOK + 1][ EMS__SZTOK + 1 ];

void ems1Tblk() {
}
