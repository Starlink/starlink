#if !defined( C2F77_INCLUDED )   /* Include this file only once */
#define C2F77_INCLUDED
/*
*+
*  Name:
*     c2f77.h

*  Purpose:
*     Define the interface to the c2f77 module.

*  Description:
*     This file defines language-specific functions which support the
*     FORTRAN 77 interface to the AST library.
*
*     Note that this module is not a class implementation, although it
*     resembles one.

*  Functions Defined:
*     Public:
*        None.
*
*     Protected:
*        astStringExport
*           Export a C string to a FORTRAN string.

*  Macros Defined:
*     Public:
*        None.
*
*     Protected:
*        astWatchSTATUS
*           Execute C code while watching a FORTRAN STATUS variable.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     15-NOV-1996 (RFWS):
*        Original version.
*     16-JUL-1997 (RFWS):
*        Added astWatchSTATUS.
*     13-JUN-2001 (DSB):
*        Make astStringExport available to F77 interface modules as well
*        as AST classes.
*-
*/

/* Macros. */
/* ======= */
/*
*+
*  Name:
*     astWatchSTATUS

*  Type:
*     Protected macro.

*  Purpose:
*     Execute C code while watching a FORTRAN STATUS variable.

*  Synopsis:
*     #include "c2f77.h"
*     astWatchSTATUS(code)

*  Description:
*     This macro expands to code which executes the C code supplied
*     via the "code" argument in a new C scope (delimited by
*     {...}). The code supplied executes while the AST error status is
*     equated to a variable called STATUS, which is an error status
*     argument passed from a FORTRAN routine using the macros defined
*     in the "f77.h" include file.
*
*     The effect of this is roughly as if the astWatch function had
*     been used to locally declare the FORTRAN STATUS argument as a
*     new AST error status variable, except that this macro also works
*     if STATUS is not an int.

*  Parameters:
*     code
*        The C code to be executed.

*  Examples:
*     F77_SUBROUTINE(ast_doit)( INTEGER(STATUS) ) {
*        astWatchSTATUS(
*           astDoit();
*        )
*     }
*        Causes the astDoit function to be invoked as if the AST error
*        status were equated to the STATUS argument passed from
*        FORTRAN.  Typically, if STATUS is set to an error value,
*        astDoit would detect this by means of the astOK macro and
*        would not then execute.  If an error occurs in astDoit,
*        causing the AST error status to be set, then that value is
*        transferred to STATUS after the C code has executed (i.e. at
*        the end of the astWatchSTATUS macro).

*  Notes:
*     - The FORTRAN argument must be called STATUS and must appear in
*     the C function's parameter list as an argument of the INTEGER()
*     macro defined in the "f77.h" include file.
*     - The C code supplied executes in a new scope, in which
*     automatic variables may be declared. However, such variables
*     will not exist after the macro's expansion has been executed.
*     - The AST error status variable and its value remain unchanged
*     after the expansion of this macro has executed.
*-
*/

/* Define the macro. */
#define astWatchSTATUS(code) \
\
/* Begin a new C scope. */ \
{ \
\
/* Ensure that a pointer to the STATUS argument exists. */ \
   GENPTR_INTEGER(STATUS) \
\
/* Store the STATUS value in a local int. */ \
   int ast_local_status = *STATUS; \
   int *status = &ast_local_status; \
\
/* Make this int the AST error status variable, saving the address of \
   the previous variable. */ \
   int *ast_previous_status = astWatch( &ast_local_status ); \
\
/* Execute the code supplied using the new error status variable. */ \
   code \
\
/* Restore the original error status variable. */ \
   (void) astWatch( ast_previous_status ); \
\
/* Return the final error status to STATUS. */ \
   *STATUS = ast_local_status; \
}

/* Function prototypes. */
/* ==================== */
void astStringExport_( const char *, char *, int );

/* Function interfaces. */
/* ==================== */
/* These wrap up the functions defined by this module to make them
   easier to use. */
#define astStringExport astStringExport_
#endif
