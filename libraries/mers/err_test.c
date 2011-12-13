/*
*+
*  Name:
*     err_test

*  Purpose:
*     A C/ADAM version of the MERS test program

*  Description:
*     To test the installation of the C interface and ADAM link scripts

*  Copyright:
*     Copyright (C) 2003 Central Laboratory of the Research Councils.
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
*     NXG: Norman Gray (Starlink)
*     {enter_authors_here}

*  History:
*     15-DEC-2003 (NXG):
*         Autoconf.

*  Notes:
*     Note that <> is used to ensure that the installed include files are used

*-
*/
#include <sae_par.h>
#include <mers.h>

void err_test(int *status) {

const char MSG[]="MSG1";

/* Call msgOut */
msgBell( status );
msgOut( MSG, "MSG C Interface is installed and working.", status );
msgOut(" ", "STATUS: token check: ^STATUS- hello ^STUB", status);
msgOut(" ", "^STATUS: at start ^STATUS hello ^STUB", status);

/* Call ERR_REP and ERR_FLUSH. */
*status = SAI__ERROR;
errMark();
errRep( " ", "ERR C Interface is installed and working.", status );
errFlbel( status );
errRlse();
}
