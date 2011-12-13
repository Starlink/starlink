      SUBROUTINE sgs_IMTX(HM,IF,IPR,HT,AR,XU,YU,SP,TXJ)
*+
*  Name:
*     IMTX

*  Purpose:
*     Inquire marker & text parameters

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     HM = REAL (Returned)
*         Marker height (=width)
*     IF = INTEGER (Returned)
*         Text font number
*     IPR = INTEGER (Returned)
*         Text precision
*     HT = REAL (Returned)
*         Text height
*     AR = REAL (Returned)
*         Text aspect ratio (H/W)
*     XU,YU = REAL (Returned)
*         Text orientation direction cosines
*     SP = REAL (Returned)
*         Text spacing
*     TXJ = CHAR (Returned)
*         Text justification code

*  Notes:
*     OBSOLETE ROUTINE - RETAINED FOR GKS 6.2 VERSION COMPATABILITY

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Read From Common:
*     HMK, IFONT, IPREC, HTX, ARTX, XUPTX, YUPTX, STX, CTXJ

*-
      IMPLICIT NONE

      REAL HM
      INTEGER IF,IPR
      REAL HT,AR,XU,YU,SP
      CHARACTER*2 TXJ

      INCLUDE 'sgscom'




      HM=HMK
      IF=IFONT
      IPR=IPREC
      HT=HTX
      AR=ARTX
      XU=XUPTX
      YU=YUPTX
      SP=STX
      TXJ=CTXJ

      END
