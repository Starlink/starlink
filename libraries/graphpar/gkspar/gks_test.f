      SUBROUTINE GKS_TEST(STATUS)
*+
*  Name:
*    gks_test

*  Purpose:
*    Test GKS in ADAM

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

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Authors:
*    {enter_authors_here}

*-
      IMPLICIT NONE
      INTEGER STATUS
      INTEGER WKID
      INCLUDE 'SAE_PAR'

*                      The following variable(s) are defined in the
*                      included file
*                      GACENT
      INCLUDE 'GKS_PAR'
      INTEGER N
      PARAMETER (N = 5)
      REAL X(N), Y(N)
      DATA X/0.0,0.0,1.0,1.0,0.0/,
     :     Y/0.0,1.0,1.0,0.0,0.0/

*                      Open GKS, open and activate workstation. The
*                      parameter for open GKS is system-dependent;
*                      a typical value has been given here.

      CALL GKS_ASSOC('DEVICE','CREATE',WKID,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
        CALL MSG_SETI('WKID',WKID)
        CALL MSG_OUT(' ', 'GKS_ASSOC associates wkid ^wkid', STATUS)
*                      End of standard opening sequence
*---------------------------------------------------------------------
      CALL GPL(N,X,Y)
      CALL GSTXAL(GACENT,GACENT)
      CALL GSCHH(0.03)
      CALL GTX(0.5,0.5,'Successful test of GKS in ADAM')
*---------------------------------------------------------------------

      CALL GKS_CANCL('DEVICE',STATUS)
      CALL GKS_DEACT(STATUS)

      ENDIF

      END

