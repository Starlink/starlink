      SUBROUTINE KPS1_MEMCS( DATA, VAR, NPIX, NLIN, USEVAR, ILEVEL,
     :                       XMARG, YMARG, FILE21, FILE22, SIGMA, MEAN,
     :                       STATUS )
*+
*  Name:
*     KPS1_MEMCS

*  Purpose:
*     Sets up files <21> and <22> in common block /MECOMS/ for MEMSYS3.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMCS( DATA, VAR, NPIX, NLIN, USEVAR, ILEVEL, XMARG,
*                      YMARG, FILE21, FILE22, SIGMA, MEAN, STATUS )

*  Description:
*     File 21 holds the input data and file 22 holds the accuracies of
*     the input data (=1/sigma). The data is copied from the input,
*     replacing bad values with the value zero. Bad values always have a
*     corresponding accuracy of zero. If the input data have associated
*     variance values, then these are used to create the accuracies in
*     file <22>. If there are no variance values available, then a rough
*     estimate of the noise is formed from the RMS difference between
*     adjacent pixels.

*  Arguments:
*     DATA( NPIX, NLIN ) = REAL (Given)
*        The input data.
*     VAR( NPIX, NLIN ) = REAL (Given)
*        Variance values for the input data.
*     NPIX = INTEGER (Given)
*        No. of pixels per line in the input data.
*     NLIN = INTEGER (Given)
*        No. of pixels per line in the input data.
*     USEVAR = LOGICAL (Given)
*        If true, then there are variance values in the VAR array. If
*        false, then the VAR array will be ignored.
*     ILEVEL = INTEGER (Given)
*        The user information level. If ILEVEL is 1 or more then the
*        user is told the mean data value in the input, and the initial
*        estimate of the noise in the input.
*     XMARG = INTEGER (Given)
*        The width of the left-hand X axis margin, in pixels.
*     YMARG = INTEGER (Given)
*        The width of the bottom Y axis margin, in pixels.
*     FILE21( C1_NPX, C1_NLN ) = REAL (Returned)
*        The input data values with a blank margin around them.
*     FILE22( C1_NPX, C1_NLN ) = REAL (Returned)
*        The accuracy (1.0/sigma) associated with each value in file 21.
*     SIGMA = REAL (Returned)
*        A rough estimate of the mean noise level in the data.
*     MEAN = REAL (Returned)
*        The mean value in the input data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-SEP-1990 (DSB):
*        Original version.
*      4-MAR-1991 (DSB):
*        Name changed from SETCMS to KPS1_SETCS.
*     1991 July 18 (MJC):
*        Name changed from from KPS1_SETCS to KPS1_MEMCS.
*     {enter_further_changes_here}

*-


*  Type Definitions:

      IMPLICIT NONE              ! No implicit typing


*  Global Constants:


      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Data constants.


*  Global Variables:

      INCLUDE 'C1_COM'           ! Used to communicate with OPUS and
                                 ! TROPUS.
*        C1_NPX = INTEGER (Read)
*           The X dimension of all internal images (including margin).
*        C1_NLN = INTEGER (Read)
*           The Y dimension of all internal images (including margin).



*  Arguments Given:

      INTEGER  NPIX
      INTEGER  NLIN
      REAL     DATA( NPIX, NLIN )
      REAL     VAR( NPIX, NLIN )
      LOGICAL  USEVAR
      INTEGER  ILEVEL
      INTEGER  XMARG
      INTEGER  YMARG


*  Arguments Returned:

      REAL     FILE21( C1_NPX, C1_NLN )
      REAL     FILE22( C1_NPX, C1_NLN )
      REAL     SIGMA
      REAL     MEAN


*  Status:

      INTEGER STATUS             ! Global status


*  Local Variables:

      REAL     ACC               ! Accuracy value
      REAL     DATVAL            ! Data value
      REAL     DIFF              ! Difference between adjacent data
                                 ! values.
      REAL     LIMIT             ! Max. difference allowed between
                                 ! adjacent data values.
      INTEGER  LIN               ! Line counter.
      REAL     LSTVAL            ! The previous data value.
      INTEGER  NSUM1             ! No. of good data values in the
                                 ! input.
      INTEGER  NSUM2             ! No. of variances summed in SUM2.
      INTEGER  PIX               ! Pixel counter.
      REAL     SUM1              ! The sum of the good data values.
      REAL     SUM2              ! Sum of the variance estimates.
      REAL     VARVAL            ! Current variance value.

*.


*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN


*  Initialize both files to all zero.

      DO LIN = 1, C1_NLN
         DO PIX = 1, C1_NPX
            FILE21( PIX, LIN ) = 0.0
            FILE22( PIX, LIN ) = 0.0
         END DO
      END DO


*  Iinitialise the sums used to find the statistics.

      NSUM1 = 0
      SUM1 = 0.0
      NSUM2 = 0
      SUM2 = 0.0


*  Copy the input data to file <21>.

      DO LIN  = 1, NLIN
         LSTVAL = VAL__BADR

         DO PIX = 1, NPIX
            DATVAL = DATA( PIX, LIN )


*  If the data value is bad, set file <21> to zero.

            IF( DATVAL .EQ. VAL__BADR ) THEN
               FILE21( XMARG + PIX, YMARG + LIN ) = 0.0


*  If the data value is good, store it and increment statistics.

            ELSE
               FILE21( XMARG + PIX, YMARG + LIN ) = DATVAL
               SUM1 = SUM1 + DATVAL
               NSUM1 = NSUM1 + 1

               IF( LSTVAL .NE. VAL__BADR ) THEN
                  SUM2 = SUM2 + ( DATVAL - LSTVAL )**2
                  NSUM2 = NSUM2 + 1
               END IF

            ENDIF

            LSTVAL = DATVAL

         END DO

      END DO


*  If there are no good data values, abort.

      IF( NSUM1 .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MEMCS_ERR1',
     :                 'No good data found in input image.',
     :                  STATUS )
         GOTO 999
      END IF


*  Find mean data value.

      MEAN = SUM1 / NSUM1


*  If variance values were supplied with the data, use them to set up
*  file <22>.

      IF( USEVAR ) THEN

         SUM2 = 0.0
         NSUM2 = 0

         DO LIN  = 1, NLIN
            DO PIX = 1, NPIX

               VARVAL = VAR( PIX, LIN )

               IF( DATA( PIX, LIN ) .NE. VAL__BADR .AND.
     :             VARVAL .NE. VAL__BADR .AND.
     :             VARVAL .GT. 0.0 ) THEN

                  FILE22( XMARG + PIX, YMARG + LIN ) =
     :                                            1.0 / SQRT( VARVAL )

                  SUM2 = SUM2 + VARVAL
                  NSUM2 = NSUM2 + 1

               END IF

            END DO

         END DO


*  If no variance values were supplied with the data, find the RMS
*  difference between adjacent data values.

      ELSE

         IF( NSUM2 .GT. 0 ) THEN
            SIGMA = SQRT( SUM2 / NSUM2 )

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_MEMCS_ERR2',
     :                    'Unable to calculate noise level in input.',
     :                     STATUS )
            GOTO 999
         END IF


*  Go through the data again, excluding data points which differ from
*  their neighbour by more than 2 sigma.

         LIMIT = 2.0 * SIGMA

         NSUM2 = 0
         SUM2 = 0.0

         DO LIN = 1, NLIN
            LSTVAL = VAL__BADR

            DO PIX = 1, NPIX
               DATVAL = DATA( PIX, LIN )

               IF( DATVAL .NE. VAL__BADR ) THEN

                  IF( LSTVAL .NE. VAL__BADR ) THEN
                     DIFF = DATVAL - LSTVAL

                     IF( ABS( DIFF ) .LE. LIMIT ) THEN
                        SUM2 = SUM2 + DIFF*DIFF
                        NSUM2 = NSUM2 + 1
                     END IF

                  END IF

               END IF

               LSTVAL = DATVAL

            END DO

         END DO

      END IF


*  If possible, calculate a typical sigma value.

      IF( NSUM2 .GT. 0 ) THEN
         SIGMA = SQRT( SUM2 / NSUM2 )

      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MEMCS_ERR2',
     :                 'No usable variance values found in input.',
     :                  STATUS )
         GOTO 999
      END IF


*  If no variances were supplied with the data, set file 22 to hold
*  the value of 1/sigma were there is valid data. Bad data values and
*  the blank margin retain the initial accuracy value of zero. This
*  causes  MEMSYS3 to give them zero weight in the deconvolution.

      IF( .NOT. USEVAR ) THEN

         IF( SIGMA .GT. 0.0 ) THEN
            ACC = 1.0 / SIGMA

            DO LIN = 1, NLIN
               DO PIX = 1, NPIX
                  IF( DATA( PIX, LIN ) .NE. VAL__BADR ) THEN
                     FILE22( XMARG + PIX, YMARG + LIN ) = ACC
                  END IF
               END DO
            END DO

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_MEMCS_ERR3',
     :                    'No usable accuracy value found.', STATUS )
            GOTO 999

         END IF

      END IF


*  If required tell the user what the mean data value and noise estimate
*  are.

      IF( ILEVEL .GE. 1 ) THEN
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

         CALL MSG_SETR( 'MEAN', MEAN )
         CALL MSG_OUT( 'REPORT',
     :                 '  Mean value in the input data is ^MEAN',
     :                  STATUS )

         CALL MSG_SETR( 'SIGMA', SIGMA )
         CALL MSG_OUT( 'REPORT', '  Initial estimate of the mean '//
     :                 'noise in the input data is ^SIGMA', STATUS )

         CALL MSG_OUT( 'REPORT', ' ', STATUS )

      END IF


*  Finish.
 999  CONTINUE

      END
