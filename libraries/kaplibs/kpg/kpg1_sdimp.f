      SUBROUTINE KPG1_SDIMP( NDF, NDIM, DIMV, STATUS )
*+
*  Name:
*     KPG1_SDIMP

*  Purpose:
*     Obtains up to a number of significant dimensions of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SDIMP( NDF, NDIM, DIMV, STATUS )

*  Description:
*     This routine finds the dimensions which are significant in an
*     NDF, i.e. those with greater than one element.  The significant
*     dimensions are recorded and returned.  If the number of
*     significant dimensions found is less than a specified value, the
*     insignificant dimensions pad out the array of dimension indices
*     returned; and all the dimensions are returned in order of
*     increasing dimensionality.  However, should the number of
*     significant dimensions exceed the required number a bad status,
*     SAI_ERROR, is returned.  Likewise there is an error when there
*     are no significant dimensions.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     NDIM = INTEGER (Given)
*        The desired number of dimensions.
*     DIMV( NDIM ) = INTEGER (Returned)
*        The significant dimensions i.e. the ones that are greater than
*        one.  There is an execption when there are fewer than NDIM
*        present in the NDF, whereupon this array includes in dimension
*        order those that are insignificant too.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The NDF must exist.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 December 21 (MJC):
*        Original version.
*     15-JUN-1998 (DSB):
*        Avoid addressing DIMV outside range [1-NDIM]
*     4-OCT-2019 (DSB):
*        CHanged to use the 8-byte NDF interface.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER
     :  NDF,
     :  NDIM

*  Arguments Returned:
      INTEGER
     :  DIMV( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  ACTDIM,                  ! Actual number of dimensions in the
                                 ! NDF
     :  DEFICT,                  ! The deficit between the required
                                 ! number of dimensions and the number
                                 ! of significant ones.
     :  I,                       ! Loop counter
     :  PADDIM,                  ! Number of insignificant dimensions
     :  SIGDIM                   ! Number of significant dimensions

      INTEGER*8
     :  DIM( NDF__MXDIM )        ! The NDF dimensions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of dimensions.
      CALL NDF_DIM8( NDF, NDF__MXDIM, DIM, ACTDIM, STATUS )

*  Deal with one special case---the NDF has a dimensionality that is
*  less than or equal to qwhat is expected---by just copying the
*  dimensions to the "significant dimensions".
      IF ( ACTDIM .LE. NDIM ) THEN
         DO I = 1, NDIM
            DIMV( I ) = I
         END DO
      ELSE

* Initialise the counter.
         SIGDIM = 0

* Initialise the significant dimensions.
         DO I = 1, NDIM
            DIMV( I ) = 0
         END DO

*  Loop for each dimension.
         DO I = 1, ACTDIM

*  Is the dimension significant?
            IF ( DIM( I ) .GT. 1 ) THEN

*  Yes, so add it to the total.
               SIGDIM = SIGDIM + 1

*  Record the dimension.
               IF( SIGDIM .LE. NDIM ) DIMV( SIGDIM ) = I
            END IF
         END DO

*  Pad the returned array with insignificant dimensions, where
*  necessary.
         IF ( SIGDIM .LT. NDIM ) THEN

*  Find the deficit.
            DEFICT = NDIM - SIGDIM

*  Repeat the search but this time inserting the insignificant
*  dimensions in order until the deficiency is made up.

*  Initialise the counters of significant and insignificant dimensions.
            SIGDIM = 0
            PADDIM = 0

*  Loop for each dimension up to the required number.
            I = 0
            DO WHILE ( SIGDIM .LT. NDIM )
               I = I + 1

*  Is the dimension significant?
               IF ( DIM( I ) .GT. 1 ) THEN

*  Yes, so add it to the total.
                  SIGDIM = SIGDIM + 1

*  Record the dimension.
                  DIMV( SIGDIM ) = I

*  Is the dimension insignificant and padding dimensions are still
*  required.
               ELSE IF ( PADDIM .LT. DEFICT ) THEN

*  Count the number of padded dimensions.
                  PADDIM = PADDIM + 1

*  Yes, so add it to the total.
                  SIGDIM = SIGDIM + 1

*  Record the dimension.
                  DIMV( SIGDIM ) = I

               END IF
            END DO

         END IF

*  Look for error conditions.
*  ==========================

*  Must have at least one significant dimension.
         IF ( SIGDIM .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF )
            CALL ERR_REP( 'KPG1_SDIMP_NOSIG',
     :        'All dimensions are one in the NDF ^NDF.', STATUS )

*    The effective dimensionality of the NDF must equal the prescribed
*    number.

         ELSE IF ( SIGDIM .GT. NDIM ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF )
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL ERR_REP( 'KPG1_SDIMP_NODIM',
     :        'The NDF ^NDF is not ^NDIM-dimensional.', STATUS )
         END IF
      END IF

      END
