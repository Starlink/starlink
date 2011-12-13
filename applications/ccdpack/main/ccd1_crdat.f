      SUBROUTINE CCD1_CRDAT( TYPE, BAD, DOVAR, EL, SCALE, DSCALE, ZERO,
     :                       DZERO, ORIG, DAT, VAR, NBADD, NBADV,
     :                       STATUS )
*+
*  Name:
*     CCD1_CRDAT

*  Purpose:
*     Apply corrections to a data and (optional) variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CRDAT( TYPE, BAD, DOVAR, EL, SCALE, DSCALE, ZERO, DZERO,
*                      ORIG, DAT, VAR, NBADD, NBADV, STATUS )

*  Description:
*     The routine applies scale factor and zero point corrections to a
*     data array and (optionally) an associated variance array using
*     the formula:
*
*        DAT = ( DAT - ORIG ) * SCALE + ZERO
*
*     and the corresponding error propagation formula for the variance
*     (if required).

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        Data type of the array(s) to be corrected: '_REAL' or
*        '_DOUBLE' (in upper case). The routine will return without
*        action unless one of these two strings is supplied.
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for the presence of bad
*        values in the data (and variance, if given).
*     DOVAR = LOGICAL (Given)
*        Whether a variance array is also to be processed.
*     EL = INTEGER (Given)
*        Number of data elements to be corrected.
*     SCALE = DOUBLE PRECISION (Given)
*        Scale factor correction to be applied.
*     DSCALE = DOUBLE PRECISION (Given)
*        Standard error on the scale factor correction.
*     ZERO = DOUBLE PRECISION (Given)
*        Zero point correction to be applied.
*     DZERO = DOUBLE PRECISION (Given)
*        Standard error on the zero point correction.
*     ORIG = DOUBLE PRECISION (Given)
*        Origin value to be used in making the correction. This should
*        be chosen so that the errors on the scale factor and zero
*        point corrections are expected to be un-correlated.
*     DAT = INTEGER (Given)
*        Pointer to the data array to be corrected.
*     VAR = INTEGER (Given)
*        Pointer to the variance array to be corrected. This is only
*        referenced if DOVAR is .TRUE..
*     NBADD = INTEGER (Returned)
*        Number of bad values generated amongst the corrected data
*        values.
*     NBADV = INTEGER (Returned)
*        Number of bad values generated amongst the corrected variance
*        values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      LOGICAL BAD
      LOGICAL DOVAR
      INTEGER EL
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION DSCALE
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION ORIG
      INTEGER DAT
      INTEGER VAR

*  Arguments Returned:
      INTEGER NBADD
      INTEGER NBADV

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test for each supported type in turn and call the appropriate
*  version of the routine to perform the work.
      IF ( TYPE .EQ. '_REAL' ) THEN
         CALL CCD1_CRDTR( BAD, DOVAR, EL, SCALE, DSCALE, ZERO, DZERO,
     :                    ORIG, %VAL( CNF_PVAL( DAT ) ),
     :                    %VAL( CNF_PVAL( VAR ) ), NBADD, NBADV,
     :                    STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL CCD1_CRDTD( BAD, DOVAR, EL, SCALE, DSCALE, ZERO, DZERO,
     :                    ORIG, %VAL( CNF_PVAL( DAT ) ),
     :                    %VAL( CNF_PVAL( VAR ) ), NBADD, NBADV,
     :                    STATUS )
      END IF

      END
* $Id$
