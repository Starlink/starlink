      SUBROUTINE KPS1_CRETS( DIM1, DIM2, TYPED, MEAN, HIGH, LOW,
     :                       DIRN, SIGMA, VARS, ARRAY, VARNCE, STATUS )
*+
*  Name:
*     KPS1_CRETS

*  Purpose:
*     Generates artificial data for CREFRAME

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CRETS( DIM1, DIM2, TYPED, MEAN, HIGH, LOW, DIRN, SIGMA,
*                      VARS, ARRAY, VARNCE, STATUS )

*  Description:
*     This routine generates artificial data in a two-dimensional
*     array.  Several types of data are available, including random,
*     Poissonian- and Gaussian-noise, flat, and ramping-type data.

*  Arguments:
*     DIM1 = INTEGER (Given)
*         The first dimension of the generated two-dimensional array.
*     DIM2 = INTEGER (Given)
*         The second dimension of the generated two-dimensional array.
*     TYPED = CHARACTER*2 (Given)
*         Type of data generated: RR -- random between 0 and 1;
*         RL -- random between limits; RP -- Poissonian noise about
*         mean; RA -- ramp between limits; FL -- flat; BL -- zeroes;
*         GN -- Gaussian noise about mean.
*     MEAN = REAL (Given)
*         Mean pixel value in data.
*     HIGH = REAL (Given)
*         High value in data to define limits.
*     LOW = REAL (Given)
*         Low value in data to define limits.
*     DIRN = INTEGER (Given)
*         Direction of ramping: 1 -- left to right; 2 -- right to left;
*         3 -- bottom to top; 4 -- top to bottom.
*     SIGMA = REAL (Given)
*         Standard deviation to be used in normal distribution.
*     VARS = LOGICAL (Given)
*         Whether the variance component should be populated.
*     ARRAY( DIM1, DIM2 ) = REAL (Returned)
*         Generated array of test data
*     VARNCE( IDIM1, IDIM2 ) = REAL (Returned)
*         Created variances.
*     STATUS = INTEGER (Given)
*         Global status value.

*  Implementation Deficiencies:
*     Poissonian noise is artificial. No checks are made that the ramps
*     or random limits will not generate bad pixels.

*  Copyright:
*     Copyright (C) 1985-1986, 1988-1989, 1992 Science & Engineering
*     Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJM: Mark McCaughrean
*     MJC: Malcolm Currie (Starlink, RAL)
*     AALLAN: Alasdair Allan (Starlink, University of Exeter)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUL-1985 (MJM):
*        First implemenation as aid in debugging A-tasks
*     25-OCT-1985 (MJM):
*        Modified to take blank frame option (REVA::MJM)
*     10-DEC-1985 (MJM):
*        Modified to use POISSON subroutine (UKTH::MARK)
*     13-AUG-1986 (MJC):
*        Renamed from CREFRAMESUB, completed the prologue, POISSON call
*        revised and nearly conformed to Starlink programming standards.
*     04-SEP-1986 (MJC):
*        Renamed parameters section in prologue to arguments, completed
*        prologue and tidied.
*     23-MAY-1988 (MJC):
*        Added normal-noise option.
*     27-JUL-1988 (MJC):
*        Reordered do loops for RA mode L-R and R-L.
*     07-AUG-1989 (MJC):
*        Passed array dimensions as separate variables.
*     17-MAR-1992 (MJC):
*        Used portable random-number generation.
*     26-AUG-2001 (AALLAN):
*        Renamed subroutine to confirm to KAPPA standard.
*     01-SEP-2001 (AALLAN):
*        Changed BADCOL to an integer, added BADROW and handling of
*        variances.
*     02-SEP-2001 (AALLAN):
*        Changed prologue to conform to Starlink standards.
*     11-SEP-2001 (DSB):
*        Changed layout of code comments, and variable declarations.
*        Re-wrote variances estimations.
*     2010 August 27 (MJC):
*        Rename ERROR to VARNCE to indicate meaning better and avoid
*        misdirection.  Tidied.  Substituted call to defunct NORMAL 
*        with KPG1_NORVR.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitionsffssdaw
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'PRM_PAR'          ! Magic-value constants

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      CHARACTER*2 TYPED
      REAL MEAN
      REAL HIGH
      REAL LOW
      INTEGER DIRN
      REAL SIGMA
      LOGICAL VARS

*  Arguments Returned:
      REAL ARRAY( DIM1, DIM2 )
      REAL VARNCE( DIM1, DIM2 )

*  Global Status:
      INTEGER  STATUS

*  External References:
      REAL KPG1_SEED             ! Random-number seed initialisation
      REAL SLA_RANDOM            ! Random-number generator

*  Local Variables:
      REAL DATA                  ! Dummy variable for temporary data
      INTEGER I                  ! General array counter variable
      REAL INTER                 ! Intensity interval per line/col in 
                                 ! ramp
      INTEGER J                  ! General array counter variable
      REAL SEED                  ! Random number generator seed
      REAL VALUE                 ! Value given by random subroutine
      REAL VAR                   ! Constant variance

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the random-number generator seed.  It is taken as input
*  by SLA_RANDOM, which returns a pseudo-random number between 0 and
*  1, and is updated on return.  Use SLA_RANDOM once here to fully
*  randomize numbers generated for arrays below.
      SEED = KPG1_SEED( STATUS )
      VALUE = SLA_RANDOM( SEED )

*  Check for TYPED and fill array accordingly...

*  Random between 0 and 1
*  ======================
      IF ( TYPED .EQ. 'RR' ) THEN

         VAR = 1.0/12.0

         DO  J = 1, DIM2
            DO  I = 1, DIM1
               VALUE = SLA_RANDOM( SEED )
               ARRAY( I, J ) = VALUE

*  Populate the variance component if required.
               IF ( VARS ) VARNCE( I, J ) = VAR

            END DO
         END DO

*  Random between limits
*  =====================
      ELSE IF ( TYPED .EQ. 'RL' ) THEN

         VAR = ( HIGH - LOW )**2/12.0

         DO  J = 1, DIM2
            DO  I = 1, DIM1
               VALUE = SLA_RANDOM( SEED )
               VALUE = ( VALUE * ( HIGH - LOW ) ) + LOW
               ARRAY( I, J ) = VALUE

*  Populate the variance component if required.
               IF ( VARS ) VARNCE( I, J ) = VAR

            END DO
         END DO

*  Poissonian noise on mean
*  ========================

*  This is artificial at present--to be revised.
      ELSE IF ( TYPED .EQ. 'RP' ) THEN

         DO  J = 1, DIM2
            DO  I = 1, DIM1
               DATA = MEAN
               CALL KPG1_POISR( 1, DATA, SEED, STATUS )
               ARRAY( I, J ) = DATA

*  Populate the variance component if required.
               IF ( VARS ) VARNCE( I, J ) = MEAN

            END DO
         END DO

*  Ramp between limits
*  ===================
      ELSE IF ( TYPED .EQ. 'RA' ) THEN

*  Ramp L - R
         IF ( DIRN .EQ. 1 ) THEN
            INTER  = ( HIGH - LOW ) / ( DIM1 - 1 )
            DO  J = 1, DIM2
               DO  I = 1, DIM1
                  ARRAY( I, J ) = LOW + ( INTER * ( I - 1 ) )

*  Populate the variance component if required.
                  IF ( VARS ) VARNCE( I, J ) = 0.0

               END DO
            END DO

*  Ramp R - L
         ELSE IF ( DIRN .EQ. 2 ) THEN
            INTER = ( HIGH - LOW ) / ( DIM1 - 1 )
            DO  J = 1, DIM2
               DO  I = 1, DIM1
                  ARRAY( I, J ) = HIGH - ( INTER * ( I - 1 ) )

*  Populate the variance component if required.
                  IF ( VARS ) VARNCE( I, J ) = 0.0

               END DO
            END DO

*  Ramp B - T
         ELSE IF ( DIRN .EQ. 3 ) THEN
            INTER = ( HIGH - LOW ) / ( DIM2 - 1 )
            DO  J = 1, DIM2
               DO  I = 1, DIM1
                  ARRAY( I, J ) = LOW + ( INTER * ( J - 1 ) )

*  Populate the variance component if required.
                  IF ( VARS ) VARNCE( I, J ) = 0.0

               END DO
            END DO

*  Ramp T - B
         ELSE IF ( DIRN .EQ. 4 ) THEN
            INTER = ( HIGH - LOW ) / ( DIM2 - 1 )
            DO  J = 1, DIM2
               DO  I = 1, DIM1
                  ARRAY( I, J ) = HIGH - ( INTER * ( J - 1 ) )

*  Populate the variance component if required.
                  IF ( VARS ) VARNCE( I, J ) = 0.0

               END DO
            END DO

         END IF

*  Flat over whole array
*  =====================
      ELSE IF ( TYPED .EQ. 'FL' ) THEN

         DO  J = 1, DIM2
            DO  I = 1, DIM1
               ARRAY( I, J ) = MEAN

*  Populate the variance component if required.
               IF ( VARS ) VARNCE( I, J ) = 0.0

            END DO
         END DO

*  Blank array---all zeroes
*  ========================
      ELSE IF ( TYPED .EQ. 'BL' ) THEN

         DO  J = 1, DIM2
            DO  I = 1, DIM1
               ARRAY( I, J ) = 0.0

*  Populate the variance component if required.
               IF ( VARS ) VARNCE( I, J ) = 0.0

            END DO
         END DO

*  Gaussian noise around a fixed mean
*  ==================================
      ELSE IF ( TYPED .EQ. 'GN' ) THEN

         VAR = SIGMA * SIGMA
         DO  J = 1, DIM2
            DO  I = 1, DIM1
               CALL KPG_NORVR( .FALSE., 1, MEAN, SIGMA, SEED, DATA,
     :                         STATUS )
               ARRAY( I, J ) = DATA

*  Populate the variance component if required.
               IF ( VARS ) VARNCE( I, J ) = VAR

            END DO
         END DO

      END IF

      END
