      SUBROUTINE sgs_1SETTX
*+
*  Name:
*     SETTX

*  Purpose:
*     Set text size spacing & justification from SGS parameters.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

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
*     GAHALF    i      vertical text alignment - Halfline
*     GACAP     i         "      "       "     - Capline
*     GABASE    i         "      "       "     - Baseline
*     GACENT    i      horizontal text alignment - Centre
*     GALEFT    i          "        "      "     - Left
*     GARITE    i          "        "      "     - Right

*  Externals:
*     GSCHH, GSCHUP, GSCHSP, GSTXAL, GSCHXP

*  Read From Common:
*     HTX       r      text height
*     XUPTX     r      character up vector (x)
*     YUPTX     r         "      "    "    (y)
*     STX       r      character spacing
*     CTXJ      c*2    text justification
*     ARTX      r      character aspect ratio

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      INTEGER TXALV,TXALH

      CHARACTER*5 RNAME
      PARAMETER (RNAME='SETTX')

*  Nominal aspect ratio of "normal" fonts
      REAL DEFASP
      PARAMETER (DEFASP=2.0/3.0)



*  Set height
      CALL GSCHH(HTX)

*  Set up-vector
      CALL GSCHUP(XUPTX,YUPTX)

*  Set spacing
      CALL GSCHSP(STX*DEFASP)

*  Convert justifications to GKS equivalents
      IF (CTXJ(1:1).EQ.'C') TXALV = GAHALF
      IF (CTXJ(1:1).EQ.'T') TXALV = GACAP
      IF (CTXJ(1:1).EQ.'B') TXALV = GABASE

      IF (CTXJ(2:2).EQ.'C') TXALH = GACENT
      IF (CTXJ(2:2).EQ.'L') TXALH = GALEFT
      IF (CTXJ(2:2).EQ.'R') TXALH = GARITE

      CALL GSTXAL(TXALH,TXALV)

*  Set expansion factor
      CALL GSCHXP(ARTX/DEFASP)

      END
