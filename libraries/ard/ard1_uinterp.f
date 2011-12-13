      SUBROUTINE ARD1_UINTERP( NDIM_IN, LBND_IN, UBND_IN, IN, IN_VAR,
     :                         NPOINT, OFFSET, COORDS, PARAMS, FLAGS,
     :                         BADVAL, OUT, OUT_VAR, NBAD, STATUS )
*+
*  Name:
*     ARD1_UINTERP

*  Purpose:
*     Perform sub-pixel interpolation on a grid of data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_UINTERP( NDIM_IN, LBND_IN, UBND_IN, IN, IN_VAR,
*                        NPOINT, OFFSET, COORDS, PARAMS, FLAGS, BADVAL,
*                        OUT, OUT_VAR, NBAD, STATUS )

*  Description:
*     This routine is passed via the FINTERP argument of the
*     AST_RESAMPLEI function in order to perform sub-pixel interpolation
*     during resampling of gridded data.
*
*     For each output (mask) pixel, an interior value is returned if
*     the the corresponding position in user coords is within the current
*     ARD region. Otherwise an exterior value is returned.

*  Arguments:
*     NDIM_IN = INTEGER (Given)
*        The number of dimensions in the input grid.
*     LBND_IN( NDIM_IN ) = INTEGER (Given)
*        The lower bounds of the input grid.
*     UBND_IN( NDIM_IN ) = INTEGER (Given)
*        The upper bounds of the input grid.
*     IN( * ) = INTEGER (Given)
*        The input grid. This array is not actually used since the output
*        value depends on its position alone. The bounds of this array
*        can be supplied all equal to 1.
*     IN_VAR( * ) = INTEGER (Given)
*        Not used.
*     NPOINT = INTEGER (Given)
*        The number of points at which the input grid is to be
*        interpolated. This will be at least one.
*     OFFSET( NPOINT ) = INTEGER (Given)
*        For each interpolation point, this array will contain the
*        offset from the start of the OUT array at which the interpolated
*        value should be stored. For example, the interpolated value for
*        point number POINT should be stored in OUT(1+OFFSET(POINT)).
*     COORDS( NPOINT, NDIM_IN ) = DOUBLE PRECISION (Given)
*        A 2-dimensional array containing the coordinates of the
*        points at which interpolation should be performed. These will
*        be stored so that coordinate number COORD for interpolation
*        point number POINT is found in element COORDS(POINT,COORD).
*
*        If any interpolation point has any of its coordinates equal
*        to the value AST__BAD (as defined in the AST_PAR include
*        file), then the corresponding output data (and variance)
*        should be set to the value given by BADVAL (see below).
*     PARAMS( * ) = DOUBLE PRECISION (Given)
*        The parameters defining the region.
*     FLAGS = INTEGER (Given)
*        This will be the same value as was given via the FLAGS
*        argument of AST_RESAMPLE<X>. You may test this value to
*        provide additional control over the operation of your
*        resampling algorithm. Note that the special flag values
*        AST__URESAMP1, 2, 3 & 4 are reserved for you to use for your
*        own purposes and will not clash with other pre-defined flag
*        values (see AST_RESAMPLE<X>).
*     BADVAL = INTEGER (Given)
*        This will be the same value as was given for the BADVAL
*        argument of AST_RESAMPLE<X>. It should be used for identifying
*        bad output values in the OUT array.
*     OUT( * ) = INTEGER (Returned)
*        An array into which the interpolated data values should be
*        returned.  Note that details of the storage order and number
*        of dimensions of this array are not required, since the
*        OFFSET array contains all necessary information about where
*        each returned value should be stored.
*
*        In general, not all elements of this array may be used in any
*        particular invocation of the routine. Those which are not used
*        should be returned unchanged.
*     OUT_VAR( * ) = INTEGER (Returned)
*        Not used.
*     NBAD = INTEGER (Returned)
*        This should return the number of interpolation points at
*        which an output data value equal to BADVAL has been assigned
*        because no valid interpolated value could be obtained.  The maximum
*        value that should be returned is NPOINT, and the minimum is
*        zero (indicating that all output values were successfully
*        obtained).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine will typically be invoked more than once for each
*     invocation of AST_RESAMPLEI.
*     - If an error occurs within this routine, it should set the
*     STATUS argument to an error value before returning. This will
*     cause an immediate return from AST_RESAMPLEI. The error value
*     AST__UINER is available for this purpose, but other values may also
*     be used (e.g. if you wish to distinguish different types of error).
*     The AST__UINER error value is defined in the AST_ERR include file.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JUN-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'ARD_CONST'        ! ARD_ private constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_RNDXC = INTEGER (Read)
*           The RINDEX value passed to the ARD "drawing" routines.
*        CMN_TYPEC = INTEGER (Read)
*           The region type.
*        CMN_FRMC = INTEGER (Read)
*           An AST Frame representing user coords.
*        CMN_NPARC = INTEGER (Read)
*           Number of region parameters.
*        CMN_INIT = Logical (Read and Write)
*           Supplied .TRUE. if a new region is being processed.
*           Set .FALSE. this routine.

*  Arguments Given:
      INTEGER NDIM_IN
      INTEGER LBND_IN( NDIM_IN )
      INTEGER UBND_IN( NDIM_IN )
      INTEGER IN( * )
      INTEGER IN_VAR( * )
      INTEGER NPOINT
      INTEGER OFFSET( NPOINT )
      DOUBLE PRECISION COORDS( NPOINT, NDIM_IN )
      DOUBLE PRECISION PARAMS( * )
      INTEGER FLAGS
      INTEGER BADVAL

*  Arguments Given and Returned:
      INTEGER OUT( * )

*  Arguments Returned:
      INTEGER OUT_VAR( * )
      INTEGER NBAD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL ARD1_INTR          ! Is the point an interior point?

*  Local Variables:
      DOUBLE PRECISION
     :        UC( ARD__MXDIM )   ! User coords of current point

      INTEGER
     :        OFF,
     :        I,                 ! Point index
     :        J                  ! Axis index

      integer k

      LOGICAL
     :        BAD                ! Is the user position good?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Indiate we have no bad positions as yet.
      NBAD = 0

*  Do each of the required user positions.
      DO I = 1, NPOINT

*  Copy this position to a local array, checking that the axis values
*  are usable.
         BAD = .FALSE.
         DO J = 1, NDIM_IN
            UC( J ) = COORDS( I, J )
            IF( UC( J ) .EQ. AST__BAD ) BAD = .TRUE.
         END DO

         OFF = OFFSET( I )

*  If this position is unusable, increment the number of bad values.
         IF( BAD ) THEN
            NBAD = NBAD + 1
            OUT( 1 + OFF ) = BADVAL

*  Otherwise, check to see if the point is within the region. If so,
*  assign an interior value to the output value. If not, assign the
*  exterior value (zero).
         ELSE
            CALL AST_NORM( CMN_FRMC, UC, STATUS )

            IF( ARD1_INTR( CMN_FRMC, CMN_TYPEC, NDIM_IN, CMN_NPARC,
     :                     PARAMS, UC, CMN_INIT, STATUS ) ) THEN
               OUT( 1 + OFF ) = CMN_RNDXC
            ELSE
               OUT( 1 + OFF ) = 0
            END IF

         END IF

*  Indicate that any require initialization has now been done, and so
*  should not be done again.
         CMN_INIT = .FALSE.

      END DO

      END
