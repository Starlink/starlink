      SUBROUTINE snx_AGOP

*+
*  Name:
*     AGOP

*  Purpose:
*     Open NCAR AUTOGRAPH plotting via SGS/GKS

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1986 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     PTW: P. T. Wallace (Starlink)
*     BLY: M J Bly (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-APR-1986 (PTW):
*        Original.
*     01-MAR-1995 (BLY):
*        Adden null calls to AGPWRT, AGCHNL to force linking with
*        SNX versions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     I1MACH, sgs_OPEN, snx_AGUVW, AGPRWT, AGCHNL

*-

      IMPLICIT NONE

      INTEGER I1MACH,LUIN,LUOUT,J,IZONID
      CHARACTER WKSTN*20

*  Get standard I/O units
      LUIN=I1MACH(1)
      LUOUT=I1MACH(2)

*  Ask for SGS workstation name
 100  CONTINUE
      WRITE (LUOUT,'(1X,''Workstation?'')')
      READ (LUIN,'(A)') WKSTN

*  Display list if so requested
      IF (WKSTN(:1).EQ.' '.OR.
     :    WKSTN(:1).EQ.'?') GO TO 200

*  Attempt to open SGS and GKS
      CALL sgs_OPEN(WKSTN,IZONID,J)

*  Skip if OK
      IF (J.EQ.0) GO TO 300

*  Display list of workstation names and try again
 200  CONTINUE
      CALL sgs_WLIST(LUOUT)
      GO TO 100

*  Match the AUTOGRAPH coordinate system to the current zone
 300  CONTINUE
      CALL snx_AGWV

*  Make null calls to AGPWRT and AGCNHL to force linking with
*  SNX library versions.
      CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
      CALL AGCHNL( 0, 0.0, ' ', 0, 0, 0, ' ', 0, 0 )

      END
