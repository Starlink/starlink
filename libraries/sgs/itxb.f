      SUBROUTINE sgs_ITXB (X,Y, N, DX,DY)
*+
*  Name:
*     ITXB

*  Purpose:
*     Inquire status of text string buffer.

*  Language:
*     Starlink Fortran 77

*  Description:
*     If the current horizontal text justification is "LEFT" and a new
*     text string is begun whose coordinates are X+DX,Y+DY, the new string
*     will append to the old.
*
*     If no text string is present, X,Y,DX,DY are not returned.

*  Arguments:
*     X = REAL (Returned)
*         String coordinate (x)
*     Y = REAL (Returned)
*         "        "      (y)
*     N = INTEGER (Returned)
*         String length
*     DX = REAL (Returned)
*         Displacement to end of string (x)
*     DY = REAL (Returned)
*         "       "   "   "   "    (y)

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

*  Externals:
*     GQTXX

*  Read From Common:
*     NTEXT    i      length of current string
*     XTEXT    r      coordinate of current string (x)
*     YTEXT    r           "      "    "       "   (y)
*     IZTW     i()    zone table - SGS workstation ID
*     ISZID    i      current zone ID
*     ITWID    i()    workstation table - GKS workstation ID

*-

      IMPLICIT NONE

      REAL X,Y
      INTEGER N
      REAL DX,DY

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      INTEGER IERR,JSTAT,IWKID
      REAL TXEXPX(4),TXEXPY(4)
      CHARACTER RNAME*4
      PARAMETER (RNAME='ITXB')



      N=NTEXT
      IF (N.GT.0) THEN
         X=XTEXT
         Y=YTEXT

         IWKID = IWTID(ABS(IZTW(ISZID)))
         CALL GQTXX(IWKID,XTEXT,YTEXT,CTEXT(:NTEXT),IERR,DX,DY,
     :                                                    TXEXPX,TXEXPY)
         IF (IERR.NE.0) THEN
            CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQTXX',
     :                                                            JSTAT)
            GO TO 9999
         END IF

         DX = DX - XTEXT
         DY = DY - YTEXT
      END IF

 9999 CONTINUE

      END
