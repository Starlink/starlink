      SUBROUTINE sgs_1ICHMO (NCH, MODE, IESW, JSTAT)
*+
*  Name:
*     ICHMO

*  Purpose:
*     Enquire mode of choice device on the current SGS workstation.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     If an error occurs "request" and "echo on" are returned.

*  Arguments:
*     NCH = INTEGER (Given)
*         Choice device number
*     MODE = INTEGER (Returned)
*         Operating mode of choice device
*     IESW = INTEGER (Returned)
*         Echo switch setting of choice device
*     JSTAT = INTEGER (Returned)
*         Status (0=OK)

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GREQU   i     operating mode - request
*     GECHO   i     echo on

*  Errors:
*     Error returned by GKS enquiry

*  Externals:
*     GQCHS, sgs_1ERR

*  Read From Commom:
*     IZTW    i()   zone table - SGS workstation ID
*     IWTID   i()   workstation table - GKS workstation ID
*     ISZID   i     current zone ID

*-

      IMPLICIT NONE

      INTEGER NCH,MODE,IESW,JSTAT

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'

      INCLUDE 'sgscom'


      INTEGER ICHNR,IPET,LDR,IERR,JISTAT
      REAL EAREA(4)
      CHARACTER*80 DATREC,RNAME*5
      PARAMETER (RNAME='ICHMO')



      JSTAT=0
      CALL GQCHS(IWTID(ABS(IZTW(ISZID))),NCH,1,IERR,MODE,IESW,
     :                               JISTAT,ICHNR,IPET,EAREA,LDR,DATREC)
      IF (IERR.NE.0)
     :         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQCHS',
     :                                                            JSTAT)

      END
