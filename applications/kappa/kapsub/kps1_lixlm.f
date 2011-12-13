      SUBROUTINE KPS1_LIXLM( LBND, UBND, AXIS, DATACO, XLOG, BOUND,
     :                       XLMT, STATUS )
*+
*  Name:
*     KPS1_LIXLM

*  Purpose:
*     Get the limits of the horizontal axis of a line plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LIXLM( LBND, UBND, AXIS, DATACO, XLOG, BOUND, XLMT,
*                      STATUS )

*  Description:
*     This routine gets the lower and upper horizontal axis limits for
*     a linear or logarithmic line plot.
*
*     If the limits are specified to be in data co-ordinates the
*     minimum and maximum axis values given in the input NDF are used.
*     The routine assumes that the axis is increasing or decreasing
*     monotonically. If the axis is logarithmic, only the positive
*     section of the axis is returned in the limits.
*
*     If the limits are to be specified in pixel indices the axis
*     bounds are retuned as the limits.  If the plot is logarithmic,
*     only the positive section of the axis bounds are returned.
*
*     There is a check that there are at least two pixels selected.

*  Arguments:
*     LBND = INTEGER (Given)
*        Lower bound (index) of the axis.
*     UBND = INTEGER (Given)
*        Upper bound (index) of the axis.
*     XAXIS( LBND:UBND ) = DOUBLE PRECISION (Given)
*        The axis value of each sample defined in the input NDF.
*     DATACO = LOGICAL (Given)
*        If true the limits should be obtained in data co-ordinates from
*        the axis ADIM in the NDF; otherwise the limits will be in pixel
*        indices.
*     XLOG = LOGICAL (Given)
*        If true, the axis will be plotted logarithmically.
*     BOUND( 2 ) = INTEGER (Returned)
*        The index bounds of the limits.  The first element is the lower
*        bound and the secind is the upper bound.
*     XLMT( 2 ) = DOUBLE PRECISION (Returned)
*        The limits of the data to plot in world (DATACO = FALSE) or
*        data (DATACO = TRUE) co-ordinates.  XLMT( 1 ) corresponds to
*        BOUND( 1 ) and likewise for XLMT( 2 ).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
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
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAY-1991 (WG):
*        Original version.
*     1991 June 18 (MJC):
*        Renamed from GTXMLT, revised the error reporting, added KPG
*        calls, does not reorder data co-ordinates, supports pixel
*        indices to be input controlled via the DATACO parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 14 (MJC):
*        Removed obtaining and validating bounds from a parameter, and
*        therefore PARAM argument is removed.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT definitions

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION AXIS( LBND:UBND )
      LOGICAL DATACO
      LOGICAL XLOG

*  Arguments Returned:
      DOUBLE PRECISION XLMT( 2 )
      INTEGER BOUND( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  EL,                      ! Number of elements in the axis array
     :  FSTPOS                   ! Index of the first positive axis
                                 ! value or pixel index

      DOUBLE PRECISION
     :  FSTVAL                   ! Value of first positive axis element

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the number of elements in the axis.

      EL = UBND - LBND + 1
      IF ( EL .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'LB', LBND )
         CALL MSG_SETI( 'UB', UBND )
         CALL ERR_REP( 'KPS1_LIXLM_BOUNDS',
     :     'Zero or negative axis length.  Bounds supplied were '/
     :     /'lower: ^LB to upper: ^UB. (Probable programming error.)',
     :     STATUS )
         GOTO 999
      END IF

*    This part will be redundant once a sub-region can be specified via
*    bounds after the NDF's name.

*    Define the range of the bounds in data co-ordinates.
*    ====================================================

*    Set the default bounds.

      BOUND( 1 ) = LBND
      BOUND( 2 ) = UBND

      IF ( DATACO ) THEN

*       The following code assumes that the axis is increasing or
*       decreasing monotonically.  Find the range of the axis
*       co-ordinates.

         XLMT( 1 ) = AXIS( LBND )
         XLMT( 2 ) = AXIS( UBND )

*       If the axis is logarithmic and there is a negative axis section,
*       exclude the negative axis section.

         IF ( XLOG ) THEN
            IF ( XLMT( 1 ) .LT. VAL__SMLD ) THEN

*             If no positive axis value exists at all, set status and
*             report the error.

               IF ( XLMT( 2 ) .LT. VAL__SMLD ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'KPS1_LIXLM_NEGAXS',
     :              'None of the axis co-ordinates are positive '/
     :              /'as required by a logarithmic axis.', STATUS )
                  GOTO 999
               END IF

*             Find the first positive axis element and its value.

               CALL KPG1_AXGVD( EL, AXIS, VAL__SMLD, FSTPOS, FSTVAL,
     :                          STATUS )

*             Set this value as smallest selectable value and default
*             lower value of the horizontal axis.  Assign the lower
*             bound, allowing for the change of origin from 1, that
*             the above call uses, to LBND.

               XLMT( 1 ) = FSTVAL
               BOUND( 1 ) = ABS( FSTPOS ) + LBND - 1

            ELSE IF ( XLMT( 2 ) .LT. VAL__SMLD ) THEN

*             Find the first negative axis element and its value.

               CALL KPG1_AXLVD( EL, AXIS, VAL__SMLD, FSTPOS, FSTVAL,
     :                          STATUS )

*             Set the previous value as the smallest selectable value
*             and default upper value of the horizontal axis.  The
*             pixel index returned is has a lower bound of 1 so allow
*             for the axis's actual lower bound.

               BOUND( 2 ) = ABS( FSTPOS - 1 ) + LBND - 1
               XLMT( 2 ) = AXIS( BOUND( 2 ) )
            END IF
         END IF

*    Define the range of the bounds in pixel indices.
*    ================================================

      ELSE
         IF ( XLOG ) THEN

*          Cannot plot any data if the upper bound is not positive.

            IF ( UBND .LT. 1 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'LINPLOT_NEGAXS',
     :           'LINPLOT: None of the pixel co-ordinates are '/
     :           /'positive as required by a logarithmic axis.',
     :           STATUS )
               GOTO 999

            ELSE IF ( LBND .LT. 1 ) THEN

*             Make a shorter section.
*             =======================

*             Extract the maximum possible section by making the lower
*             bound set to 1.

               XLMT( 1 ) = 1.0D0
               BOUND( 1 ) = 1
            ELSE
               XLMT( 1 ) = DBLE( LBND )
            END IF
         ELSE
            XLMT( 1 ) = DBLE( LBND )
         END IF

         XLMT( 2 ) = DBLE( UBND )
      END IF

*    Check whether there are enough samples to plot. If more then
*    2 samples to plot, the given limits are valid.

      IF ( BOUND( 2 ) - BOUND( 1 ) .GE. 2 ) THEN

*    If there are no more than 2 samples to plot, the limits are
*    invalid.  Report and flush the error and annul the parameter and
*    go back to get new limits.

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NS', BOUND( 2 ) - BOUND( 1 ) + 1 )
         CALL ERR_REP( 'KPS1_LIXLM_NOSMP',
     :     '^NS is not enough samples to plot.', STATUS )
      END IF

999   CONTINUE

      END
