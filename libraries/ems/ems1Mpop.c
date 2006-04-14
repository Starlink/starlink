/*
*+
*  Name:
*     EMS1MPOP

*  Purpose:
*     Pop the current token table context level.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Mpop()

*  Description:
*     The current message context is dropped one level.

*  Copyright:
*     Copyright (C) 1984 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     SLW: Sid Wright  (UCL)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-APR-1984 (SLW):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_MPOP
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Global Constants: */
#include "ems1.h"                    /* EMS_ Internal functions */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

/*  Global Variables: */
#include "ems_toktb.h"               /* Message token table */

void ems1Mpop( void ) {

   TRACE("ems1Mpop");

/*  If the context level is marked above EMS__MXLEV, lower the level mark
 *  only. If the context level within the allowed range: if there is more 
 *  than one context marker, remove the top marker. */
   if ( toklev > EMS__MXLEV ) {

/*     Context stack overflow. */
      toklev--;

   } else if ( toklev > 1 ) {

/*     All marked context levels. */
      tokcnt[ tokmrk ] = tokcnt[ tokmrk - 1 ];
      tokhiw[ tokmrk ] = tokhiw[ tokmrk - 1 ];
      toklev--;
      tokmrk--;
   } else {

/*     Otherwise, do nothing. */
      toklev = 1;
      tokmrk = 1;
   }
   return;
}
