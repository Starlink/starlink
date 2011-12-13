      SUBROUTINE KPS1_MEMCO( PARAM, UPDATE, EXTEND, INDF, RATE, DEF,
     :                       NITER, NOISE, STDEV, ILEVEL, METHOD,
     :                       ALPHA, BETA, ITER, STATUS )

*+
*  Name:
*     KPS1_MEMCO

*  Purpose:
*     Continues the main MEMSYS3 loop.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMCO( PARAM, UPDATE, EXTEND, INDF, RATE, DEF, NITER,
*                      NOISE, STDEV, ILEVEL, METHOD, ALPHA, BETA, ITER,
*                      STATUS )

*  Description:
*     The MEMSYS3 routine MEM3 is called iteratively until either the
*     termination criterion is reached, or the maximum number of
*     iterations is reached.  Various diagnostics are displayed
*     depending on the value of ILEVEL.
*
*     The values of the input arguments are used to initialize
*     MEMSYS3 so that the deconvolution continues from where it left
*     off.
*
*     The CLASSIC algorithm is used.  If Gaussian noise is specified
*     by the user then automatic noise scaling is applied.  A uniform
*     model equal to the value of DEF is used.
*
*     A tolerance of 10% and an aim of 1.0 are used in MEM3.

*  Arguments:
*     PARAM = CHARACTER (Given)
*        The ADAM parameter to use for accessing the output NDF.
*     UPDATE = LOGICAL (Given)
*        If .TRUE. then an output NDF is created afetr each iteration,
*        otherwise no NDF is created until the routine finishes.
*     EXTEND = LOGICAL (Given)
*        If .TRUE. then the output NDF is to have analysis information
*        appended to it in the form of an extension called MEM2D.
*     INDF = INTEGER (Given)
*        The NDF identifier for the input image.
*     RATE = REAL (Given)
*        The dimensionless distance limit between succesive MEMSYS3
*        iterations.  Should be of order 1.0.
*     DEF = REAL (Given)
*        The value of the constant default model to use.  In the absence
*        of data, the reconstruction will tend towards this value.
*     NITER = INTEGER (Given)
*        The maximum number of calls to the main MEMSYS3 routine, MEM3,
*        to make.
*     NOISE = CHARACTER (Given)
*        Either GAUSSIAN or POISSON.  Indicates the type of noise
*        statistics to be used in MEMSYS3.
*     STDEV = REAL (Given)
*        Initial estimate of the mean noise level in the input image.
*     ILEVEL = INTEGER (Given)
*        The user information level.  If equal to zero, then no
*        diagnostics are displayed.  As ILEVEL increases up to a
*        maximum of 3, more diagnostics are displayed.
*     METHOD = INTEGER (Given)
*        The value used for MEM3 argument METHOD during the previous
*        calls to MEM3.
*     ALPHA = REAL (Given and Returned)
*        On entry, the value of MEM3 argument ALPHA, produced by the
*        previous call to MEM3. On exit, the most recent value of
*        ALPHA.
*     BETA = REAL (Given and Returned)
*        On entry, the value of MEM3 argument BETA, produced by the
*        previous call to MEM3. On exit, the most recent value of
*        BETA.
*     ITER = INTEGER (Given and Returned)
*        The number of MEMSYS3 iterations completed so far.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     28-SEP-1990 (DSB):
*        Original version.
*     27-FEB-1991 (DSB):
*        Name changed from MEMCON to KPS1_MEMCO
*     1995 April 10 (MJC):
*        Used modern style of commenting.  Changed style of description.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'ME_COM'           ! Common blocks needed by MEMSYS3.
*        ME_NTR = INTEGER (Read)
*           The number of calls made so far to the transform routines
*           (OPUS and TROPUS).

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      LOGICAL  UPDATE
      LOGICAL  EXTEND
      INTEGER  INDF
      REAL     RATE
      REAL     DEF
      INTEGER  NITER
      CHARACTER * ( * ) NOISE
      REAL     STDEV
      INTEGER  ILEVEL
      INTEGER  METHOD

*  Arguments Given and Returned:
      REAL     ALPHA
      REAL     BETA
      INTEGER  ITER

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL     CHISQ             ! Chi squared (incorporating any noise
                                 ! scaling)
      INTEGER  COMPIT            ! No. of completed iterations
      LOGICAL  DONE              ! True if MEMSYS3 termination criterion
                                 ! has been reached
      REAL     GOOD              ! The number of `good' measurements
      INTEGER  ISTAT             ! MEMSYS3 status value
      INTEGER  MEMRUN            ! MEMSYS3 continuation/initialisation
                                 ! flag
      CHARACTER MODE*6           ! Mode of access to output NDF
      REAL     OMEGA             ! Rescaled termination criterion
                                 ! Should rise to 1.0 at termination
      REAL     PROB              ! The evidence for the data, which
                                 ! should rise to a maximum at
                                 ! termination
      REAL     S                 ! Entropy of the current reconstruction
                                 ! relative to a uniform model of value
                                 ! DEF
      REAL     SIGMA             ! Current scaling factor for the noise
      INTEGER  START             ! First iteration number
      REAL     TEST              ! The accuracy of the entropy
                                 ! maximisation and should always be
                                 ! less than 1 for safety.

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Set up the rest of common block /MECOMP/.
      ME_NK = 1
      ME_NJ = 1
      ME_OUT = -999999

*  Indicate that MEMSYS3 is to continue a previous run.
      MEMRUN = 1

*  Indicate that the output NDF is to be created, not updated.
      MODE = 'WRITE'

*  Loop round, doing a maximum of niter iterations of the MEMSYS3
*  algorithm starting at iteration ITER+1.
      DONE = .FALSE.

      START = ITER+1

      DO ITER = START, NITER
         IF ( .NOT. DONE ) THEN

*  If required, identify this iteration to the user.
            IF ( ILEVEL. GE. 1 ) THEN
               CALL MSG_OUT( 'REPORT', ' ', STATUS )
               CALL MSG_SETI( 'ITER', ITER )
               CALL MSG_OUT( 'REPORT', '  MEMSYS3 iteration ^ITER',
     :                        STATUS )
            END IF


*  Call the main MEMSYS3 routine.
            CALL MEM3( METHOD, 0, MEMRUN, 1, 1, 1.0, RATE, DEF, 0.0,
     :                 0.1, ALPHA, BETA, S, TEST, CHISQ, SIGMA, PROB,
     :                 GOOD, OMEGA, ISTAT )


*  Store the number of completed iterations.
            COMPIT = ITER

*  See if termination criterion has been reached.  N.B., the conjugate
*  gradient status codes can be ignored.
            IF ( MOD( ISTAT, 10000 ) .EQ. 0 ) DONE = .TRUE.


*  Give the required diagnostics.
            IF ( ILEVEL .GE. 1 ) THEN
               CALL MSG_SETR( 'OMEGA', OMEGA )
               CALL MSG_OUT( 'REPORT', '    OMEGA       : ^OMEGA',
     :                        STATUS )
            END IF

            IF ( ILEVEL .GE. 2 ) THEN
               CALL MSG_SETR( 'S', S )
               CALL MSG_OUT( 'REPORT', '    ENTROPY     : ^S', STATUS )

               CALL MSG_SETR( 'PROB', PROB )
               CALL MSG_OUT( 'REPORT', '    LOG(PROB)   : ^PROB',
     :                        STATUS )

               CALL MSG_SETR( 'TEST', TEST )
               CALL MSG_OUT( 'REPORT', '    TEST        : ^TEST',
     :                        STATUS )

               CALL MSG_SETI( 'NTR', ME_NTR )
               CALL MSG_OUT( 'REPORT', '    NTRANS      : ^NTR',
     :                        STATUS )
            END IF

            IF ( ILEVEL .GE. 3 ) THEN
               CALL MSG_SETR( 'ALPHA', ALPHA )
               CALL MSG_OUT( 'REPORT', '    ALPHA       : ^ALPHA',
     :                        STATUS )

               CALL MSG_SETR( 'CHISQ', CHISQ )
               CALL MSG_OUT( 'REPORT', '    CHI SQUARED : ^CHISQ',
     :                        STATUS )

               CALL MSG_SETR( 'GOOD', GOOD )
               CALL MSG_OUT( 'REPORT', '    GOOD        : ^GOOD',
     :                        STATUS )

               CALL MSG_SETR( 'SIGMA', SIGMA )
               CALL MSG_OUT( 'REPORT', '    SIGMA       : ^SIGMA',
     :                        STATUS )

               CALL MSG_SETI( 'ISTAT', ISTAT )
               CALL MSG_OUT( 'REPORT', '    ISTAT       : ^ISTAT',
     :                        STATUS )
            END IF


*  If required, write out the current reconstruction to disk.
            IF ( UPDATE ) THEN
               CALL KPS1_MEMSA( PARAM, MODE, EXTEND, INDF, RATE, ALPHA,
     :                          BETA, DEF, SIGMA, NITER, COMPIT,
     :                          ILEVEL, METHOD, STDEV, ISTAT, STATUS )
               IF ( STATUS. NE. SAI__OK ) GOTO 999
               MODE = 'UPDATE'
            END IF

*  Do the next MEMSYS3 iteration.
         END IF
      END DO

*  If the termination criterion was not reached, give a warning message.
      IF ( .NOT. DONE ) THEN
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_OUT( 'REPORT', '  Finished.', STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_OUT( 'REPORT',
     :     '  WARNING: MEMSYS3 termination criterion not yet reached',
     :        STATUS )

*  If termination was reached, tell user what the corrected noise level
*  was ( if automatic noise scaling was used.)
      ELSE
         IF ( ILEVEL. GE. 1 ) THEN

            CALL MSG_OUT( 'REPORT', ' ', STATUS )
            CALL MSG_OUT( 'REPORT', '  Finished.', STATUS )
            CALL MSG_OUT( 'REPORT', ' ', STATUS )

            IF ( METHOD .EQ. 2 ) THEN
               CALL MSG_SETR( 'STDEV', STDEV*SIGMA )
               CALL MSG_OUT( 'REPORT', '  Improved estimate of the '/
     :           /'mean noise level in the input image is ^STDEV',
     :           STATUS )
               CALL MSG_OUT( 'REPORT', ' ', STATUS )
            END IF

         END IF

      END IF

*  Write out the final reconstruction to disk, unless it has already
*  been done.
      IF ( .NOT. UPDATE ) THEN
         CALL KPS1_MEMSA( PARAM, MODE, EXTEND, INDF, RATE, ALPHA, BETA,
     :                    DEF, SIGMA, NITER, COMPIT, ILEVEL, METHOD,
     :                    STDEV, ISTAT, STATUS )
      END IF

*  Finish.
 999  CONTINUE

      END
