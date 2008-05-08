      SUBROUTINE AGCHNL (IAXS, VILS, CHRM, MCIM, NCIM,
     :                   IPXM, CHRE, MCIE, NCIE)

*+
*  Name:
*     AGCHNL

*  Purpose:
*     Inserts leading zeros into NCAR numeric axis labels

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     IAXS = INTEGER (Given)
*         Axis number
*     VILS = REAL (Given)
*         Value to be represented by the label
*     CHRM = CHAR (Given & Returned)
*         Mantissa string
*     MCIM = INTEGER (Given)
*         Maximum length of mantissa string
*     NCIM = INTEGER (Given & Returned)
*         Actual length of mantissa string
*     IPXM = INTEGER (Given & Returned)
*         Pointer to "times" symbol if any
*     CHRE = CHAR (Given & Returned)
*         Exponent string
*     MCIE = INTEGER (Given)
*         Maximum length of exponent string
*     NCIE = INTEGER (Given & Returned)
*         Actual length of exponent string

*  Notes:
*     For detailed explanations see AUTOGRAPH write-up, section 3.26.

*  Copyright:
*     Copyright (C) 1986 Science & Engineering Research Council. All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-OCT-1986 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

      INTEGER IAXS
      REAL VILS
      CHARACTER*(*) CHRM
      INTEGER MCIM,NCIM,IPXM
      CHARACTER*(*) CHRE
      INTEGER MCIE,NCIE

      INTEGER IDP,I
      CHARACTER K



*  Check mantissa string not full
      IF (NCIM.LT.MCIM) THEN

*     Look for decimal point in mantissa
         IDP = INDEX(CHRM(:NCIM),'.')

         IF (IDP.NE.0) THEN

*        Pick up preceding character if any
            IF (IDP.GT.1) THEN
               K = CHRM(IDP-1:IDP-1)
            ELSE
               K = ' '
            END IF

*        Make sure point not preceded by numerics
            IF (K.LT.'0'.OR.K.GT.'9') THEN

*           Move the mantissa along one space
               DO I=NCIM,IDP,-1
                  CHRM(I+1:I+1) = CHRM(I:I)
               END DO

*           Insert the zero
               CHRM(IDP:IDP) = '0'

*           Increment the mantissa length
               NCIM = NCIM+1

*           Increment the "times" index if present
               IF (IPXM.NE.0) IPXM = IPXM+1

            END IF

         END IF

      END IF

      END
