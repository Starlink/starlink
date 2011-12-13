      SUBROUTINE KPS1_MEMIN( SUMP, DSUMP, MASK, NPIX, NLIN, ALPHA, DEF,
     :                       SIGMA, ILEVEL, FILE2, STATUS )
*+
*  Name:
*     KPS1_MEMIN

*  Purpose:
*     Calls the MEMSYS3 inference routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMIN( SUMP, DSUMP, MASK, NPIX, NLIN, ALPHA, DEF,
*                      SIGMA, ILEVEL, FILE2, STATUS )

*  Description:
*     Call the MEMSYS3 inference routine MEMASK to find the total data
*     sum in the product of the mask and reconstruction, together with
*     the standard deviation of the total data sum.  The resulting
*     values are written to a pair of output ADAM parameters.  The
*     supplied mask is blurred with the ICF before being used in
*     MEMASK.

*  Arguments:
*     SUMP = CHARACTER (Given)
*        The name of the output ADAM parameter to receive the
*        sum of the product of the mask and the deconvolution.
*     DSUMP = CHARACTER (Given)
*        The name of the output ADAM parameter to receive the standard
*        deviation on the sum of the product of the mask and the
*        deconvolution.
*     MASK( NPIX, NLIN ) = REAL (Given)
*        The mask supplied by the user.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in the mask.  This must be the
*        same as that of the original image which was given as input to
*        the deconvolution process.
*     NLIN = INTEGER (Given)
*        The number of lines in the mask.  This must be the same as
*        that of the original image which was given as input to the
*        deconvolution process.
*     ALPHA = REAL (Given)
*        This is the value of the MEM3 argument alpha, returned on the
*        last call to MEM3 (i.e. the call which produced the final
*        deconvolved image).
*     DEF = REAL (Given)
*        The value supplied for MEM3 argument DEF during the
*        deconvolution.
*     SIGMA = REAL (Given)
*        This is the value of the MEM3 argument sigma, returned on the
*        last call to MEM3 (i.e. the call which produced the final
*        deconvolved image).
*     ILEVEl = INTEGER (Given)
*        User information level.  If set to one, the calculated sum and
*        standard deviation are still written to the output ADAM
*        parameters, but are not written to the screen.
*     FILE2( C1_NPX, C1_NLN ) = REAL (Returned)
*        MEMSYS area <2>, containing the mask after replacement of
*        bad pixels, and padding.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1990 (DSB):
*        Original version.
*     28-FEB-1991 (DSB):
*        Name changed from MEMINF to KPS1_MEMIN
*     20-MAR-1995 (DSB):
*        Modified to allow use of external work arrays.
*     1995 April 7 (MJC):
*        Minor stylistic changes.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Global Variables:
      INCLUDE 'ME_COM'           ! MEMSYS3 common blocks
      INCLUDE 'C1_COM'           ! MEM2D internal communication.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER SUMP*(*)
      CHARACTER DSUMP*(*)
      INTEGER NPIX
      INTEGER NLIN
      REAL    MASK( NPIX, NLIN )
      REAL    ALPHA
      REAL    DEF
      REAL    SIGMA
      INTEGER ILEVEL
      REAL    FILE2( C1_NPX, C1_NLN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL     DSUM              ! Standard deviation on SUM
      INTEGER  ISTAT             ! Status value from MEMASK
      INTEGER  LIN               ! Line counter
      REAL     MSKVAL            ! Current mask value
      INTEGER  NGOOD             ! No. of good pixels in the mask
      INTEGER  PIX               ! Pixel counter
      REAL     SUM               ! Sum of product of mask and
                                 ! reconstruction
      REAL     SUM2              ! Ditto squared

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the mask to file <2> incorporating the blank margins.
      NGOOD = 0
      SUM2 = 0.0

      DO LIN  = 1, NLIN

         DO PIX = 1, NPIX
            MSKVAL = MASK( PIX, LIN )

*  If the mask value is bad, set file <2> to zero.
            IF ( MSKVAL .EQ. VAL__BADR ) THEN
               FILE2( C1_XMG + PIX, C1_YMG + LIN ) = 0.0

*  If the data value is good, store it and increment statistics.
            ELSE
               FILE2( C1_XMG + PIX, C1_YMG + LIN ) = MSKVAL
               NGOOD = NGOOD + 1
               SUM2 = SUM2 + MSKVAL * MSKVAL
            ENDIF

         END DO

      END DO

*  If the mask has no good pixels, abort.
      IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MEMIN_ERR1', 'No good pixels found in '/
     :                 /'mask image.', STATUS )
         GO TO 999
      END IF

      IF ( SUM2 .EQ. 0.0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MEMIN_ERR2', 'Mask contains all zeros.',
     :                  STATUS )
         GO TO 999
      END IF

*  Smooth the mask with the ICF. NB, files <4> and <5> are used as
*  work space.
      IF ( C1_WEXT ) THEN
         CALL KPS1_ICBLU( C1_ICF, .FALSE.,
     :                    %VAL( CNF_PVAL( C1_IP( 4 ) ) ),
     :                    %VAL( CNF_PVAL( C1_IP( 5 ) ) ),
     :                    FILE2, STATUS )
      ELSE
         CALL KPS1_ICBLU( C1_ICF, .FALSE., ME_ST( ME_KB( 4 ) ),
     :                    ME_ST( ME_KB( 5 ) ), FILE2, STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Call the MEMSYS3 inference routine to find the total data sum in the
*  product of the mask and reconstruction, together with the standard
*  deviation of the total data sum.
      IF ( ILEVEL .GE. 1 ) THEN
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_OUT( 'REPORT', '  Calling the MEMSYS3 inference '/
     :                 /'routine', STATUS )
      END IF

      CALL MEMASK( DEF, 0.0, 0.1, ALPHA, SIGMA, SUM, DSUM, ISTAT )

*  Give a warning message based on the value of ISTAT
      CALL MSG_OUT( 'REPORT', ' ', STATUS )

      IF ( ISTAT .EQ. 1 ) THEN
         CALL MSG_OUT( 'REPORT', 'WARNING: Results may be inaccurate',
     :                 STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

      ELSE IF ( ISTAT .GT. 1 ) THEN
         CALL MSG_OUT( 'REPORT', 'WARNING: Results are inaccurate',
     :                 STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

      END IF

*  Display the results.
      IF ( ILEVEL .GE. 1 ) THEN

         CALL MSG_SETR( 'SUM', SUM )
         CALL MSG_SETR( 'DSUM', DSUM )

         CALL MSG_OUT( 'REPORT', '  Sum of (mask * deconvolved image)'/
     :                 /' is ^SUM +/- ^DSUM.', STATUS )

         CALL MSG_OUT( 'REPORT', ' ', STATUS )

      END IF

*  Write the results to the environment.
      CALL PAR_DEF0R( SUMP, SUM, STATUS )
      CALL PAR_PUT0R( SUMP, SUM, STATUS )
      CALL PAR_DEF0R( DSUMP, DSUM, STATUS )
      CALL PAR_PUT0R( DSUMP, DSUM, STATUS )

 999  CONTINUE

      END
