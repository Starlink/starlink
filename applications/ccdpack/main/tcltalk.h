/*
*+
*  Name:
*     tcltalk.h

*  Purpose:
*     Include file for communication with ccdwish.

*  Language:
*     {routine_language}

*  Type of Module:
*     C header file.

*  Description:
*     This include file declares the routines which can be used by
*     calling C code to communicate with a ccdwish Tcl interpreter.
*     These routines should be used rather than directly using the Tcl
*     library routines declared in tcl.h and the Tcl documentation.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-OCT-2000 (MBT):
*        Original version.
*     2010-05-06 (TIMJ):
*        Add some consting
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#ifndef CCD_TCLTALK_DEFINED
#define CCD_TCLTALK_DEFINED

/* The following constants must be distinct from legitimate TCL return codes,
   TCL_OK, TCL_ERROR, TCL_RETURN, TCL_BREAK and TCL_CONTINUE. */
#define CCD_CCDMSG 2112
#define CCD_CCDLOG 2113
#define CCD_CCDERR 2114

   typedef struct {
      int downfd[ 2 ];
      int upfd[ 2 ];
   } ccdTcl_Interp;


   ccdTcl_Interp *ccdTclStart( int *status );
   void ccdTclStop( ccdTcl_Interp *cinterp, int *status );
   void ccdTclRun( ccdTcl_Interp *cinterp, const char *filename, int *status );
   void ccdTclDo( ccdTcl_Interp *cinterp, const char *script, int *status );
   void ccdTclAppC( ccdTcl_Interp *cinterp, const char *name, const char *value,
                    int *status );
   void ccdTclSetI( ccdTcl_Interp *cinterp, const char *name, int value,
                    int *status );
   void ccdTclSetD( ccdTcl_Interp *cinterp, const char *name, double value,
                    int *status );
   void ccdTclSetC( ccdTcl_Interp *cinterp, const char *name, const char *value,
                    int *status );
   void ccdTclGetI( ccdTcl_Interp *cinterp, const char *script, int *value,
                    int *status );
   void ccdTclGetD( ccdTcl_Interp *cinterp, const char *script, double *value,
                    int *status );
   char *ccdTclGetC( ccdTcl_Interp *cinterp, const char *script, int *status );

#endif  /* CCD_TCLTALK_DEFINED */

/* $Id$ */
