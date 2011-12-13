/*
 *+
 *  Name:
 *     adamtest.c

 *  Purpose:
 *    Simple test program for ADAM

 *  Notes:
 *    Build as:
 *       link adamtest.c -L. `./err_link_adam`

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-SEP-2008 (TIMJ):
*        First version
*     {enter_further_changes_here}

*-
 */

#include "sae_par.h"
#include "mers.h"
#include "msg_err.h"

#define STRING "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

void adamtest ( int * status ) {

  if (*status != SAI__OK) return;

  msgOut("MSG1", "This text should not appear for msgOut", status );
  msgSeti("TEST", 5);
  msgOut(" ", "     Testing ^^ %% % $ $$ %ET - $TESTOBJ ^TEST",status);
  msgOut( " ", "  Testing $ %ET Again %%ET ^^$", status );
  msgOut(" ", "$$DOLLAR ^^CARET %%PERCENT at start of string", status);

  /* Make up a bad status */
  *status = MSG__SYNER;
  errRep( " ", "This is the error message ^STATUS embedded",
          status );

  errRep( "MSG1", "This text should not appear", status );

  errRep( " ", "Should be expanded: %ET as 'EXPOSURE_TIME'", status);

  errRep( " ", "Object $TESTOBJ %s %XX should be somewhere", status );

  errRep( " ", "Multiple %ET and ^STATUS and %TESTOBJ and %BLAH", status );

  errRep( " ", "Double up %% escape $$ characters", status );

  msgSetc( "X1", STRING );
  msgSetc( "X2", STRING);
  msgSetc( "X3", STRING);
  msgSetc( "X4", STRING);
  msgSetc( "X5", STRING);
  msgSetc( "X6", STRING);
  errRep( " ","Overflow: ^X1:^X2:^X3:^X4:^X5:^X6", status );

}
