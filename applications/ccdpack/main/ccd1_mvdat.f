      SUBROUTINE CCD1_MVDAT( TYPE, ADJUST, BAD, ISVAR, LBIN, UBIN, NY,
     :                       DATIN, VARIN, I1, I2, IZ, SCALE, DSCALE,
     :                       ZERO, DZERO, ORIG, LBOUT, UBOUT, OUT, NBAD,
     :                       STATUS )
*+
*  Name:
*     CCD1_MVDAT

*  Purpose:
*     Move data from an image plane into a stack, applying corrections.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MVDAT( TYPE, ADJUST, BAD, ISVAR, LBIN, UBIN, NY, DATIN,
*                      VARIN, I1, I2, IZ, SCALE, DSCALE, ZERO, DZERO,
*                      ORIG, LBOUT, UBOUT, OUT, NBAD, STATUS )

*  Description:
*     The routine moves values from a range of columns in an array
*     (regarded as a 2-dimensional image) into a corresponding area in
*     a selected plane of a 3-dimensional "stack".  At the same time,
*     it optionally applies scale factor and zero point corrections to
*     data values according to the formula:
*
*        OUT = ( DATIN - ORIG ) * SCALE + ZERO
*
*     The routine may also be used to generate corrected variance
*     values as output, in which case the appropriate statistical error
*     propagation formula is used.

*  Arguments:
*     TYPE = CHARA* ( * ) (Given)
*        The numeric type of the values to be processed; '_REAL' or
*        '_DOUBLE' (in upper case). The routine will return without
*        action unless one of these two strings is supplied.
*     ADJUST = LOGICAL (Given)
*        Whether scale factor and zero point corrections are to be
*        applied; if .FALSE. is specified, then the output values will
*        not have these corrections applied.
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for the presence of bad
*        values in the input data (and variance, if given).
*     ISVAR = LOGICAL (Given)
*        Whether the output values should be variances (as opposed to
*        data values).
*     LBIN = INTEGER (Given)
*        Lower bound on the first dimension of the input image.
*     UBIN = INTEGER (Given)
*        Upper bound on the first dimension of the input image.
*     NY = INTEGER (Given)
*        Second dimension size of the input image.
*     DATIN = INTEGER (Given)
*        Pointer to the input image, which should have dimensions
*        (LBIN:UBIN,NY). Its data type should be as specified via the
*        TYPE argument.
*     VARIN = INTEGER (Given)
*        Pointer to an input image containing the variance values,
*        which should have dimensions (LBIN:UBIN,NY). Its data type
*        should be as specified via the TYPE argument.
*     I1 = INTEGER (Given)
*        Lower bound on the range of columns to be moved (must lie in
*        the range LBIN to UBIN inclusive).
*     I2 = INTEGER (Given)
*        Upper bound on the range of columns to be moved (must lie in
*        the range LBIN to UBIN inclusive and be at least equal to I1).
*     IZ = INTEGER (Given)
*        Number of the plane in the output image stack into which the
*        results are to be placed.
*     SCALE = DOUBLE PRECISION (Given)
*        Scale factor correction to be applied. This is only used if
*        ADJUST is .TRUE..
*     DSCALE = DOUBLE PRECISION (Given)
*        Standard error on the scale factor correction.  This is only
*        used if (ADJUST.AND.ISVAR) is .TRUE..
*     ZERO = DOUBLE PRECISION (Given)
*        Zero point correction to be applied. This is only used if
*        ADJUST is .TRUE..
*     DZERO = DOUBLE PRECISION (Given)
*        Standard error on the zero point correction.  This is only
*        used if (ADJUST.AND.ISVAR) is .TRUE..
*     ORIG = DOUBLE PRECISION (Given)
*        Origin value to be used in making the correction. This should
*        be chosen so that the errors on the scale factor and zero
*        point corrections are expected to be un-correlated. This is
*        only used if ADJUST is .TRUE..
*     LBOUT = INTEGER (Given)
*        Lower bound on the first dimension of the output stack (must
*        not exceed I1).
*     UBOUT = INTEGER (Given)
*        Upper bound on the first dimension of the output stack (must
*        not be less than I2).
*     OUT = INTEGER (Given)
*        Pointer to the stack into which the results are to be
*        inserted. This should have dimensions (LBOUT:UBOUT,NY,*). The
*        stack itself will be modified (although the pointer will not
*        be), the region affected being (I1:I2,1:NY,IZ:IZ) - other
*        parts of this stack are returned unchanged. The final
*        dimension size of the stack must be at least equal to IZ and
*        its data type should be as specified via the TYPE argument.
*     NBAD = INTEGER (Returned)
*        Number of bad values generated amongst the output values.
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
*     2-JUN-1992 (RFWS):
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
      LOGICAL ADJUST
      LOGICAL BAD
      LOGICAL ISVAR
      INTEGER LBIN
      INTEGER UBIN
      INTEGER NY
      INTEGER DATIN
      INTEGER VARIN
      INTEGER I1
      INTEGER I2
      INTEGER IZ
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION DSCALE
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION ORIG
      INTEGER LBOUT
      INTEGER UBOUT
      INTEGER OUT

*  Arguments Returned:
      INTEGER NBAD

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test for each recognised type in turn and call the appropriate
*  version of the routine to perform the work.
      IF ( TYPE .EQ. '_REAL' ) THEN
         CALL CCD1_MVDTR( ADJUST, BAD, ISVAR, LBIN, UBIN, NY,
     :                    %VAL( CNF_PVAL( DATIN ) ),
     :                    %VAL( CNF_PVAL( VARIN ) ), I1, I2, IZ,
     :                    SCALE, DSCALE, ZERO, DZERO, ORIG,
     :                    LBOUT, UBOUT, %VAL( CNF_PVAL( OUT ) ),
     :                    NBAD, STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL CCD1_MVDTD( ADJUST, BAD, ISVAR, LBIN, UBIN, NY,
     :                    %VAL( CNF_PVAL( DATIN ) ),
     :                    %VAL( CNF_PVAL( VARIN ) ), I1, I2, IZ,
     :                    SCALE, DSCALE, ZERO, DZERO, ORIG,
     :                    LBOUT, UBOUT, %VAL( CNF_PVAL( OUT ) ),
     :                    NBAD, STATUS )
      END IF

      END
* $Id$
