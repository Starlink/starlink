/*
*+
*  Name:
*     ccdaux.h

*  Purpose:
*     Include file for general purpose CCDPACK C functions.

*  Language:
*     {routine_language}

*  Type of Module:
*     C header file.

*  Description:
*     This include file declares some miscellaneous routines used by
*     CCDPACK C code, and the STARCALL and ASTCALL macros which can
*     be used to call routines with a Starlink-like status argument
*     from a Tcl object function.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     5-APR-2001 (MBT):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#ifndef CCD_CCDAUX_DEFINED
#define CCD_CCDAUX_DEFINED

#include "sae_par.h"
#include "mers.h"
#include "tcl.h"

/* Declare function prototypes.
*/
   void *ccdMall( const char *type, int size, int *status );

/* General purpose buffer length.
*/
#define CCDAUX_BUFLENG 4096

/* Macro for calling a routine with a Starlink-like STATUS argument.
   The code which forms the macro argument is executed, and if the
   contents of a lexically scoped variable called status is set,
   the macro will bail out with a TCL error, and the text of the
   Starlink error will be returned in the result string.
   This macro should be used only from Tcl-type routines which are
   expected to return a TCL integer error code and leave a result in
   the result of a Tcl_Interp called interp.  It should be used to
   call all routines which use a Starlink-like status argument.

   This macro and the similar ASTCALL macro below may be used to invoke
   large sections of code; however note that multiple-line comments
   within a macro argument will cause cpp(1) to get confused about
   which line is which, so this may cause difficulty when running
   under the debugger.
*/
#define STARCALL(code) \
   errMark(); \
   { \
      int status_val = SAI__OK; \
      int *status = &status_val; \
      code \
      if ( *status != SAI__OK ) { \
         char errmsg[ CCDAUX_BUFLENG ]; \
         char errname[ ERR__SZPAR ]; \
         int errmsg_leng; \
         int errname_leng; \
         Tcl_Obj *_tcl_result; \
         _tcl_result = Tcl_NewStringObj( "", 0 ); \
         while ( *status != SAI__OK ) { \
            errLoad( errname, ERR__SZPAR, &errname_leng, errmsg, \
                     CCDAUX_BUFLENG, &errmsg_leng, status ); \
            Tcl_AppendStringsToObj( _tcl_result, "\n", errmsg, (char *) NULL ); \
         } \
         Tcl_SetObjResult( interp, _tcl_result ); \
         errRlse(); \
         return TCL_ERROR; \
      } \
      else { \
         errRlse(); \
      } \
   }

/* Macro for calling AST routines.  These effectively behave like normal
   Starlink-type functions, but the handling of the status argument is
   a bit different (see SUN/211)
*/
#define ASTCALL(code) \
   errMark(); \
   { \
      int status_val = SAI__OK; \
      int *status = &status_val; \
      int *old_status = astWatch( status ); \
      code \
      astWatch( old_status ); \
      if ( *status != SAI__OK ) { \
         char errmsg[ CCDAUX_BUFLENG ]; \
         char errname[ ERR__SZPAR ]; \
         int errmsg_leng; \
         int errname_leng; \
         Tcl_Obj *_tcl_result; \
         _tcl_result = Tcl_NewStringObj( "", 0 ); \
         while ( *status != SAI__OK ) { \
            errLoad( errname, ERR__SZPAR, &errname_leng, errmsg, \
                     CCDAUX_BUFLENG, &errmsg_leng, status ); \
            Tcl_AppendStringsToObj( _tcl_result, "\n", errmsg, (char *) NULL ); \
         } \
         Tcl_SetObjResult( interp, _tcl_result ); \
         errRlse(); \
         return TCL_ERROR; \
      } \
      else { \
         errRlse(); \
      } \
   }


#endif  /* CCD_CCDAUX_DEFINED */

/* $Id$ */
