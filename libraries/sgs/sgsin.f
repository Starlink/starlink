      SUBROUTINE sgs_1SGSIN (JSTAT)
*+
*  Name:
*     SGSIN

*  Purpose:
*     Initialise SGS.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     internal routine

*  Description:
*     Set common block variables to suitable initial states.

*  Arguments:
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status: 0=OK (if non-inherited)

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
*     GBUNDL    i      aspect source flag - bundled
*     GINDIV    i        "       "     "  - individual
*     GSTRKP    i      text precision - stroke

*  Constants From Sgscom:
*     MXWK      i      maximum number of workstations allowed
*     MXZ       i         "      "    "      zones       "

*  Externals:
*     sgs_1HSTAT, sgs_1GKERR, sgs_1ERR, GSELNT, GQASF

*  Written To Common:
*     IWTID     i()    workstation table - workstation ID
*     IWTTY     i()    workstation table - type
*     IWTCO     i()    workstation table - connection ID
*     IWTCA     i()    workstation table - category
*     IZTW      i()    zone table - workstation ID
*     ZTV       r()     "     "   - viewport
*     ZTW       r()     "     "   - window
*     IPEN      i      current pen
*     NPOLY     i      length of current polyline
*     NTEXT     i        "    "     "    text string
*     HMK       r      current marker height (obsolete)
*     IFONT     i         "    font
*     IPREC     i         "    text precision
*     HTX       r         "      "  height
*     ARTX      r         "      "  aspect ratio
*     XUPTX     r         "      "  up vector (x)
*     YUPTX     r         "      "   "   "    (y)
*     STX       r         "      "  spacing
*     CTXJ      c*2       "      "  justification
*     NCHODV    i         "    choice device
*     LCHOST    i      number of valid choice keys
*     ISZID     i      current zone ID
*     XRES      r()    WDT - x resolution
*     YRES      r()    WDT - y resolution
*     IBLKCL    i()    WDT - block clear mechanism
*     ISPOOL    i()    WDT - workstation spooled
*     NCLORQ    i      no clear open requested

*-

      IMPLICIT NONE

      INTEGER JSTAT

      INTEGER I,IWKID,IZONID,IERR

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER LASF(13)
      CHARACTER*5 RNAME
      PARAMETER (RNAME='SGSIN')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Initialise SGS parameters
      DO 20 IWKID=1,MXWK
         IWTID(IWKID) = 0
         IWTTY(IWKID) = 0
         IWTCO(IWKID) = 0
         IWTCA(IWKID) = 0
   20 CONTINUE
      DO 30 IZONID=1,MXZ
         IZTW(IZONID)=0
   30 CONTINUE
      IPEN=1
      NPOLY=0
      NTEXT=-1
      HMK=0.02
      IFONT=1
      IPREC=GSTRKP
      HTX=0.02
      ARTX=2.0/3.0
      XUPTX=0.0
      YUPTX=1.0
      STX=0.0
      CTXJ='BL'
      NCHODV=0
      CHOIST = '123456789'
      LCHOST=9

*  Set current zone to 0 and fill in tables with dummy values
      ISZID=0
      IWTID(0) = 0
      IWTTY(0) = 0
      IWTCO(0) = 0
      IWTCA(0) = 0
      IZTW(0) = 0
      DO 40 I = 1,4,2
         ZTV(I,0) = 0.0
         ZTW(I,0) = 0.0
         ZTV(I+1,0) = 1.0
         ZTW(I+1,0) = 1.0
   40 CONTINUE

*  Select transformation number 1
      CALL GSELNT(1)

*  Set attribute flags except for polygon fill
      CALL GQASF(IERR,LASF)
      IF (IERR.EQ.0) THEN
         DO 50 I = 1,3
            LASF(I) = GBUNDL
   50    CONTINUE
         DO 60 I = 4,10
            LASF(I) = GINDIV
   60    CONTINUE
         CALL GSASF(LASF)

*     Populate dummy entry in workstation description table
         XRES(0) = 1.0
         YRES(0) = 1.0
         IBLKCL(0) = 0
         ISPOOL(0) = 0

*     No clear open requested flag
         NCLORQ = .FALSE.

*     Check for GKS errors
         CALL sgs_1GKERR(RNAME, JSTAT)
       ELSE
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQASF',JSTAT)
       END IF

*  Exit
 9999 CONTINUE

      END
