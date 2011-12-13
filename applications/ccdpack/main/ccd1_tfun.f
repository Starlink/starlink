*+
*  Name:
*     CCD1_TFUN

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RES = CCD1_TFUN( RADSQ )

*  Description:
*     Associated routine ccd1_tfun, all parameters passed through common.
*     This routine basically returns the value which when multiplied by the
*     integrated intensity of the object gives the intensity at specified
*     radius squared

*  Arguments:
*     RADSQ = INTEGER (Givne)
*        Radius squared at which the calculation should be performed.

*  Returned Value:
*     CCD1_TFUN = REAL
*        Result.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     PWD: Peter Draper (Starlink)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1997 (PWD):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      REAL FUNCTION CCD1_TFUN( RADSQ )
      IMPLICIT NONE              ! no implicit typing

*  Global variables
      REAL C1, C2, CN, PARSQ, CHANGE, PARRAD
      REAL PARMN1, PARMN2, PARMNN, COMIX
      COMMON /PM    / C1, C2, CN, PARSQ, CHANGE, PARRAD
      COMMON /PMN   / PARMN1, PARMN2, PARMNN, COMIX

*  Arguments given:
      REAL RADSQ

*  Local variables
      REAL RAD, ARG

*  FUNC calculate Gauss/exp profile or Gauss/Lor model
      CCD1_TFUN = 0.0
      RAD = SQRT( RADSQ )
      IF ( RAD .LT. PARRAD ) THEN
         ARG = C1 * RADSQ
         CCD1_TFUN = COMIX * CN / ( 1.0 + PARMN2 * RADSQ )
         CCD1_TFUN = CCD1_TFUN + ( 1.0 - COMIX ) * CN * EXP( ARG )
      ELSE
         ARG = CHANGE + ( PARRAD - RAD ) * C2
         CCD1_TFUN = COMIX * CN / ( 1.0 + PARMN2 * RADSQ )
         CCD1_TFUN = CCD1_TFUN + ( 1.0 - COMIX ) * CN * EXP( ARG )
      END IF

      END
* $Id$
