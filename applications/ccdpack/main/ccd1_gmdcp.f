      SUBROUTINE CCD1_GMDCP( USEVAR, MAXWT, METH, IMETH, USEWT, WEIGHT,
     :                       ALPHA, NSIGMA, NITER, RMIN, RMAX, STATUS )
*+
*  Name:
*     CCD1_GMDCP

*  Purpose:
*     Get MAKEMOS data combination parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GMDCP( USEVAR, MAXWT, METH, IMETH, USEWT, WEIGHT, ALPHA,
*                      NSIGMA, NITER, RMIN, RMAX, STATUS )

*  Description:
*     The routine returns a character string and an integer code
*     representing the data combination method to be used when
*     combining input data in the MAKEMOS application. Depending on the
*     combination method, any necessary ancillary parameter values are
*     also returned. All values are obtained via the parameter system
*     using the standard CCDPACK parameter names.

*  Arguments:
*     USEVAR = LOGICAL (Given)
*        This should indicate whether there are variance values
*        available in the input data which will be used for weighting.
*        If it is .TRUE., no attempt will be made to obtain additional
*        weighting factors.
*     MAXWT = INTEGER (Given)
*        If user-supplied weights are to be used instead of variances
*        (i.e. if USEVAR is .FALSE.), then this specifies the number of
*        weights which are required.
*     METH = CHARACTER * ( * ) (Returned)
*        A character string (in upper case) representing the data
*        combination method chosen (the first word of each entry taken
*        from the list below). This is intended for use in messages.
*        The length of the variable supplied for this argument should
*        be at least 9 characters.
*     IMETH = INTEGER (Returned)
*        An integer representing the required combination method:
*           2  = MEAN
*           3  = WEIGHTED MEDIAN
*           4  = TRIMMED MEAN
*           5  = MODE
*           6  = SIGMA CLIPPED MEAN
*           7  = THRESHOLD EXCLUSION MEAN
*           8  = MINMAX MEAN
*           9  = BROADENED MEDIAN
*           10 = SIGMA CLIPPED MEDIAN
*           11 = UNWEIGHTED MEDIAN
*     USEWT = LOGICAL (Returned)
*        Set to .TRUE. if user-supplied weights were obtained. If set
*        to .FALSE, then no weighting is to be used. This value is only
*        significant if USEVAR is .FALSE..
*     WEIGHT( MAXWT ) = REAL (Returned)
*        Weighting factors to be used. These will only be returned if
*        USEVAR is .FALSE. and a non-null set of weights is returned by
*        the parameter system. If a null set is given, then USEWT will
*        be set .FALSE..
*     ALPHA = REAL (Returned)
*        Fraction of values to remove from data (appropriate to METH =
*        'TRIMMED').
*     NSIGMA = REAL (Returned)
*        Number of standard deviations at which to clip the data
*        (appropriate to METH = 'MODE' and 'SIGMA').
*     NITER = INTEGER (Returned)
*        Maximum number of iterations (appropriate to METH = 'MODE' and
*        'SIGMA').
*     RMIN = REAL (Returned)
*        Minimum allowed data value (appropriate to METH =
*        'THRESHOLD').
*     RMAX = REAL (Returned)
*        Maximum allowed data value (appropriate to METH =
*        'THRESHOLD').
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
*     PDRAPER: Peter Draper (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     3-APR-1991 (PDRAPER):
*        Original version.
*     28-JUL-1992 (RFWS):
*        Renamed and adapted for use with MAKEMOS.
*     3-AUG-1992 (RFWS):
*        Added validity check on weighting factors.
*     4-AUG-1992 (RFWS):
*        Changed to return a character string for the chosen method.
*     19-JUL-1995 (PDRAPER):
*        Removed AIF_ routines and replaced with PAR_
*     30-JAN-1998 (PDRAPER):
*        Added sigma clipped median.
*     18-NOV-1998 (PDRAPER):
*        Added unweighted median (fastmed).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Arguments Given:
      LOGICAL USEVAR
      INTEGER MAXWT

*  Arguments Returned:
      CHARACTER * ( * ) METH
      INTEGER IMETH
      INTEGER NITER
      LOGICAL USEWT
      REAL WEIGHT( MAXWT )
      REAL NSIGMA
      REAL ALPHA
      REAL RMIN
      REAL RMAX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for weights

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the data combination method to be used, using MEDIAN as the
*  default.
      CALL PAR_CHOIC( 'METHOD', ' ', 'MEDIAN,MEAN,TRIMMED,MODE,'//
     :                'SIGMA,THRESHOLD,MINMAX,BROADENED,CLIPMED,'//
     :                'FASTMED',
     :                .FALSE., METH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Set the IMETH value appropriately.
      IF ( METH .EQ. 'MEAN' ) THEN
         IMETH = 2
      ELSE IF ( METH .EQ. 'MEDIAN' ) THEN
         IMETH = 3
      ELSE IF ( METH .EQ. 'TRIMMED' ) THEN
         IMETH = 4
      ELSE IF ( METH .EQ. 'MODE' ) THEN
         IMETH = 5
      ELSE IF ( METH .EQ. 'SIGMA' ) THEN
         IMETH = 6
      ELSE IF ( METH .EQ. 'THRESHOLD' ) THEN
         IMETH = 7
      ELSE IF ( METH .EQ. 'MINMAX' ) THEN
         IMETH = 8
      ELSE IF ( METH .EQ. 'BROADENED' ) THEN
         IMETH = 9
      ELSE IF ( METH .EQ. 'CLIPMED' ) THEN
         IMETH = 10
      ELSE IF ( METH .EQ. 'FASTMED' ) THEN
         IMETH = 11
      END IF

*  If variance values are not being used and the data combination
*  method requires weights, then get a set of external weighting
*  factors.
      USEWT = .FALSE.
      IF ( ( .NOT. USEVAR ) .AND.
     :     ( ( METH .EQ. 'MEAN' ) .OR.
     :       ( METH .EQ. 'MEDIAN' ) .OR.
     :       ( METH .EQ. 'MODE' ) .OR.
     :       ( METH .EQ. 'SIGMA' ) .OR.
     :       ( METH .EQ. 'THRESHOLD' ) .OR.
     :       ( METH .EQ. 'MINMAX' )  .OR.
     :       ( METH .EQ. 'CLIPMED' ) ) ) THEN

*  Loop until an acceptable set of weights has been obtained.
         CALL ERR_MARK
 1       CONTINUE
         CALL PAR_EXACR( 'WEIGHTS', MAXWT, WEIGHT, STATUS )

*  Interpret a null value as indicating that no external weights are to
*  be used.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  Test the weights supplied to ensure they are positive.
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            DO 2 I = 1, MAXWT

*  If an invalid weight is given, output an error and return to get a
*  new set.
               IF ( WEIGHT( I ) .LE. 0.0 ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETR( 'WEIGHT', WEIGHT( I ) )
                  CALL MSG_SETI( 'I', I )
                  CALL ERR_REP( 'CCD1_GMDCP_WT',
     :                          'Invalid value of ^WEIGHT given for ' //
     :                          'weighting factor ^I (parameter ' //
     :                          '%WEIGHTS); weighting factors must ' //
     :                          'be positive. Please respecify.',
     :                          STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( 'WEIGHTS', STATUS )
                  GO TO 1
               END IF
 2          CONTINUE

*  Note if a valid set of weights was obtained.
            USEWT = .TRUE.
         ENDIF
         CALL ERR_RLSE
      END IF

*  Trimmed mean: get the trimming fraction.
      IF ( METH .EQ. 'TRIMMED' ) THEN
         CALL PAR_GDR0R( 'ALPHA', 0.2, 0.001, 0.5, .FALSE., ALPHA,
     :                   STATUS )

*  Sigma clipping: get the number of standard deviations to clip at.
      ELSE IF ( ( METH .EQ. 'MODE' ) .OR.
     :          ( METH .EQ. 'SIGMA' ) .OR.
     :          ( METH .EQ. 'CLIPMED' ) ) THEN
         CALL PAR_GDR0R( 'SIGMAS', 4.0, 0.1, 100.0, .FALSE., NSIGMA,
     :                   STATUS )

*  Iterative sigma clipping: get the maximum number of iterations.
         IF ( METH .EQ. 'MODE' ) THEN
            CALL PAR_GDR0I( 'NITER', 10, 1, 100, .FALSE., NITER,
     :                      STATUS )
         ELSE
            NITER = 1
         END IF

*  Applying threshold cuts: get the range of values.
      ELSE IF ( METH .EQ. 'THRESHOLD' ) THEN
         CALL PAR_GDR0R( 'MIN', NUM__MINR, NUM__MINR, NUM__MAXR,
     :                   .FALSE., RMIN, STATUS )
         CALL PAR_GDR0R( 'MAX', NUM__MAXR, RMIN, NUM__MAXR,
     :                   .FALSE., RMAX, STATUS )
      END IF

*  Arrive here if an error occurs.
 99   CONTINUE
      END
* $Id$
