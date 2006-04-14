/*
*+
*  Name:
*     EMS1EMARK

*  Purpose:
*     Mark a new context in the error message table.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Emark()

*  Description:
*     This sets a new context in the error table so that subsequent 
*     EMSFLUSH or EMSANNUL calls only flush or annul table entries 
*     in this context.

*  Copyright:
*     Copyright (C) 1983 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councls.
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
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-APR-1983 (SLW):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_EMARK
*      6-MAR-2001 (AJC);
*        Correctly declare mstr and pstr
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems1.h"                    /* EMS1_ prototypes */
#include "ems_err.h"                 /* EMS_ error codes */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

/*  Global Variables: */
#include "ems_msgtb.h"             /* Error message table */

void ems1Emark( void ) {
   int istat;                   /* Local status */
   int mlen;                    /* Length of MSTR */
   int plen;                    /* Length of PSTR */
   char mstr[] = "Context stack overflow (EMS fault)."; /* Local error message text*/
   char pstr[] = "EMS_EMARK_CXOVF";  /* Local message name text */

   TRACE("ems1Emark");
   DEBUG("ems1Estor","BEFORE msglev = %d", msglev );

/*  Check for maximum number of error context levels. */
   if ( msglev < EMS__MXLEV ) {

/*     Open a new error message context and set it to contain no messages. */
      msglev++;
      msgmrk++;
      msgcnt[ msgmrk ] = msgcnt[ msgmrk - 1 ];
   } else {

/*     Context stack full, so increment MSGLEV and stack an error message. */
      msglev++;

      mlen = strlen( mstr );
      plen = strlen( pstr );
      istat = EMS__CXOVF;

/*     Call EMS1ESTOR to stack the error message. */
      ems1Estor( pstr, plen, mstr, mlen, &istat );
   }
 
   DEBUG("ems1Estor","AFTER msglev = %d", msglev );
   return;
}
  
