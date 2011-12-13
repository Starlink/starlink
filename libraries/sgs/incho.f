      SUBROUTINE sgs_INCHO (NCHOIC, N)
*+
*  Name:
*     INCHO

*  Purpose:
*     Inquire number of choices on choice device on current SGS
*     workstation.

*  Language:
*     Starlink Fortran 77

*  Description:
*     If the specified choice device does not exist N is set to zero.

*  Arguments:
*     NCHOIC = INTEGER (Given)
*         SGS choice device
*     N = INTEGER (Returned)
*         Number of choices

*  Notes:
*     This routine assumes that if a workstation has a choice device 2
*     then this is a keyboard.

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
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GINPUT      i      workstation category - input
*     GOUTIN      i           "         "     - input/output

*  Constants From Sgscom:
*     MAXCHO     i      maximum number of keyboard choices

*  Errors:
*     Error returned by GKS inquiry

*  Externals:
*     GQLI, GQCHD, sgs_1ERR

*  Read From Common:
*     IZTW       i()    zone table - SGS workstation ID
*     ISZID      i      current zone ID
*     IWTTY      i()    workstation table - workstation type
*     IWTCA      i()    workstation table - category

*-

      IMPLICIT NONE

      INTEGER NCHOIC,N

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IERR,ISWKID,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD
      INTEGER IOL,IPET,LDR,JSTAT,NCH
      CHARACTER*64 DATREC(1)*80,RNAME*5
      PARAMETER (RNAME='INCHO')
      REAL EAREA(4)



      IF (NCHOIC.EQ.0) THEN

*   Keyboard
         NCH = 2
      ELSEIF (NCHOIC.GT.0) THEN

*   GKS choice device
         NCH = NCHOIC
      ELSE

*   Not a valid SGS choice device number.
         N = 0
      END IF

*   Inquire availability of input primitives.
      ISWKID = ABS(IZTW(ISZID))
      IF (IWTCA(ISWKID).EQ.GINPUT .OR. IWTCA(ISWKID).EQ.GOUTIN) THEN
         CALL GQLI(IWTTY(ISWKID),IERR,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD)
         IF (IERR.NE.0) THEN
            CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQLI',
     :      JSTAT)
            NCHD = 0
            GO TO 9999
         END IF

         IF (NCHD.GE.NCH) THEN

*       Choice device exists so inquire number of choices
            CALL GQDCH(IWTTY(ISWKID),NCH,1,1,IERR,N,IOL,IPET,EAREA,LDR,
     :                                                           DATREC)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                   'Error returned by GQLI',JSTAT)
               N = 0
               GO TO 9999
            END IF
         END IF
      END IF

 9999 CONTINUE

      END
