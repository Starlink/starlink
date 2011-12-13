      SUBROUTINE KPS1_MDSET( MODE, STEP, SAMSIZ, CENTRE, CORNER, SIDE,
     :                       MEDIAN, NUMSAM, SAMINF, STATUS )
*+
*  Name:
*     KPS1_MDSET

*  Purpose:
*     Sets up the weighting function for the weighted median.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL KPS1_MDSET( MODE, STEP, SAMSIZ, CENTRE, CORNER, SIDE,
*                      MEDIAN, NUMSAM, SAMINF, STATUS )

*  Description:
*     The weighting function for the weighted median filter is set up
*     according to the value of MODE.  If MODE is -1 then the input
*     values of CENTRE, CORNER and SIDE are used to set up the
*     weighting function otherwise one of the pre-defined weighting
*     functions will be used.  If an unexpected value of MODE is
*     encountered then STATUS will be set to an error value and an
*     error reported.  The SAMINF array is filled with the offsets
*     (calculated using the value of STEP ) for the elements to be
*     sampled and their corresponding weights.  MEDIAN will contain the
*     position of the median in the sorted weighted list and NUMSAM
*     will contain the number of samples to be sorted.  If STATUS had
*     an error value on entry then an immediate return occurs.
*
*     The predefined modes have the following weighting functions:
*
*     0:  1 1 1   1:  0 1 0   2:  1 0 1   3:  1 1 1   4:  0 1 0
*         1 1 1       1 1 1       0 1 0       1 3 1       1 3 1
*         1 1 1       0 1 0       1 0 1       1 1 1       0 1 0
*
*     5:  1 0 1   6:  1 2 1   7:  1 3 1
*         0 3 0       2 3 2       3 3 3
*         1 0 1       1 2 1       1 3 1

*  Arguments:
*     MODE = INTEGER (Given)
*        Determines which of the predefined weighting functions is to
*        be used if it is the range 0 to 7.  If it has the value of
*        -1 then the weighting function is user defined and the
*        information concerning the weighting function will be passed
*        to this routine.
*     STEP = INTEGER (Given)
*        The separation in pixels between the elements of the
*        weighting function.  This is used in calculating the offsets.
*     SAMSIZ = INTEGER (Given)
*        Number of elements in the sample.
*     CENTRE = INTEGER (Given and Returned)
*        Centre value for the weighting function, passed to the
*        routine if MODE = -1 or set if MODE is in the range 0 to 7.
*     CORNER = INTEGER (Given and Returned)
*        Corner value for the weighting function, passed to the
*        routine if MODE = -1 or set if MODE is in the range 0 to 7.
*     SIDE = INTEGER (Given and Returned)
*        Side value for the weighting function, passed to the
*        routine if MODE = -1 or set if MODE is in the range 0 to 7.
*     MEDIAN = INTEGER (Returned)
*        Gives the position of the median in the sorted weighted
*        sample.
*     NUMSAM = INTEGER (Returned)
*        Number of elements in the sample with non-zero weights.
*     SAMINF( SAMSIZ, 3 ) = INTEGER (Returned)
*        Will contain the offsets to the elements to form the
*        sample and the weights corresponding to each element.
*     STATUS = INTEGER (Given and Returned)
*        This is the global status, if this variable has an error
*        value on entry then an immediate return will occur.  If MODE
*        is outside the range -1 to 7 then STATUS will be set to
*        SAI__ERROR and an error reported.

*  Algorithm:
*     If no error on entry then
*        If MODE = -1 then
*           User defined weighting function, calculate median position
*        Elseif 0 <= MODE <= 7 then
*           One of the pre-defined weighting functions
*           Set up CORNER, SIDE and CENTRE and position of median
*        Else
*           Value of MODE not allowed, set STATUS and report error
*        Endif
*        If no error then
*           Insert CORNER into corner elements of WEIGHT array
*           Insert SIDE into side elements of WEIGHT array
*           Insert CENTRE into centre element of WEIGHT array
*           Initialise number of elements in SAMINF array to zero
*           For all elements of the weight array
*              Calculate X, Y offsets from position in WEIGHT array and
*                STEP
*              If weight is non zero then
*                 Increment number of elements
*                 Insert X, Y offsets and weight into SAMINF
*              Endif
*           Endfor
*        Endif
*     Endif

*  Implementation Deficiencies:
*     Uses exclamation marks to show weighting functions.

*  Copyright:
*     Copyright (C) 1983-1984, 1986, 1988 Science & Engineering
*     Research Council. Copyright (C) 1995 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     DB: Dave Baines (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20/10/1983 (DB):
*        Original version.
*     17/02/1984 (DB):
*        Documentation brought up to standard.
*     1986 September 10 (MJC):
*        Renamed parameters section to arguments and tidied.
*     1988 June 22 (MJC):
*        Added identification to error reporting.
*     1995 July 27 (MJC):
*        Used modern style and prologue.  Renamed from MEDSET.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE standard constants

*  Arguments Given:
      INTEGER SAMSIZ
      INTEGER MODE
      INTEGER STEP

*  Arguments Given and Returned:
      INTEGER CORNER
      INTEGER SIDE
      INTEGER CENTRE

*  Arguments Returned:
      INTEGER SAMINF( SAMSIZ, 3 )
      INTEGER MEDIAN
      INTEGER NUMSAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER OFSETX             ! 1st dimension offset to sample
                                 ! elements
      INTEGER OFSETY             ! 2nd dimension offset to sample
                                 ! elements
      INTEGER WEIGHT( 3, 3 )     ! Array containing weighting function
      INTEGER WINDEX             ! Index to WEIGHT array elements
      INTEGER X                  ! Index to WEIGHT array elements
      INTEGER Y                  ! Index to WEIGHT array elements

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up weighting values according to value of MODE.
      IF ( MODE .EQ. -1 ) THEN

*  Weighting function specified by user, calculate median position.
         MEDIAN = ( ( CENTRE + (4 * SIDE) + (4 * CORNER) ) / 2 ) + 1

      ELSE IF ( MODE .EQ. 0 ) THEN
         CORNER = 1              !                      1 1 1
         SIDE   = 1              ! weighting function = 1 1 1
         CENTRE = 1              !                      1 1 1
         MEDIAN = 5

      ELSE IF ( MODE .EQ. 1 ) THEN
         CORNER = 0              !                      0 1 0
         SIDE   = 1              ! weighting function = 1 1 1
         CENTRE = 1              !                      0 1 0
         MEDIAN = 3

      ELSE IF ( MODE .EQ. 2 ) THEN
         CORNER = 1              !                      1 0 1
         SIDE   = 0              ! weighting function = 0 1 0
         CENTRE = 1              !                      1 0 1
         MEDIAN = 3

      ELSE IF ( MODE .EQ. 3 ) THEN
         CORNER = 1              !                      1 1 1
         SIDE   = 1              ! weighting function = 1 3 1
         CENTRE = 3              !                      1 1 1
         MEDIAN = 6

      ELSE IF ( MODE .EQ. 4 ) THEN
         CORNER = 0              !                      0 1 0
         SIDE   = 1              ! weighting function = 1 3 1
         CENTRE = 3              !                      0 1 0
         MEDIAN = 4

      ELSE IF ( MODE .EQ. 5 ) THEN
         CORNER = 1              !                      1 0 1
         SIDE   = 0              ! weighting function = 0 3 0
         CENTRE = 3              !                      1 0 1
         MEDIAN = 4

      ELSE IF ( MODE .EQ. 6 ) THEN
         CORNER = 1              !                      1 2 1
         SIDE   = 2              ! weighting function = 2 3 2
         CENTRE = 3              !                      1 2 1
         MEDIAN = 8

      ELSE IF ( MODE .EQ. 7 ) THEN
         CORNER = 1              !                      1 3 1
         SIDE   = 3              ! weighting function = 3 3 3
         CENTRE = 3              !                      1 3 1
         MEDIAN = 10

      ELSE

*  Value of MODE not allowed.
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MODE', MODE )
         CALL ERR_REP( 'ERR_KPS1_MDSET_WMODE',
     :     'KPS1_MDSET: MODE = ^MODE not allowed.', STATUS )
      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*  Set up the corner and side weights.

         DO WINDEX = 1, 3, 2
            WEIGHT( WINDEX, 1 ) = CORNER
            WEIGHT( WINDEX, 3 ) = CORNER
            WEIGHT( WINDEX, 2 ) = SIDE
            WEIGHT( 2, WINDEX ) = SIDE
         END DO

*  Set up the centre weight.
         WEIGHT( 2, 2 ) = CENTRE
      END IF

*  Now set up the array of offsets and weights defining the sample
*  array initialise the number of elements in the sample to zero.
      NUMSAM = 0

*  For all rows of the weighting function.
      DO Y = 1, 3

*  Set up 2nd dimension offset to sample elements.
         OFSETY = STEP * ( Y - 2 )

*  For all points in weighting function row.
         DO X = 1, 3

*  Set up 1st dimension offset to sample elements.
            OFSETX = STEP * ( X - 2 )

*  Insert information into SAMINF if element has non-zero weight.
            IF ( WEIGHT( X, Y ) .NE. 0 ) THEN

* Increment number of elements in sample.
               NUMSAM = NUMSAM + 1

*  Insert offsets and weight.
               SAMINF( NUMSAM, 1 ) = OFSETX
               SAMINF( NUMSAM, 2 ) = OFSETY
               SAMINF( NUMSAM, 3 ) = WEIGHT( X, Y )
            END IF
         END DO
      END DO

      END
