      SUBROUTINE KPS1_MFGNS( PARNAM, AXIS, NDIM, DIMS,
     :                       NCSECT, SECT, STATUS )
*+
*  Name:
*     KPS1_MFGNS

*  Purpose:
*     Obtains the NDF section to average for automatic range
*     determination for MFITTREND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MFGNS( PARNAM, AXIS, NDIM, DIMS, NCSECT, SECT, STATUS )

*  Description:
*     This routine serves MFITTREND.  It returns an NDF section
*     describing the lines that are to be averaged and analysed
*     to specfify regions to exclude from the detrending.  This section
*     is supplied through an ADAM parameter (argument PARNAM) unless the
*     NDF dimensionality is one, whereupon a null section is returned.
*     When a null value is supplied a representative section is created.

*     The supplied section is filtered such that all elements along the
*     detrend axis (argument AXIS) are included, and elements for
*     dimensions higher than exist in the NDF are removed.

*  Arguments:
*     PARNAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to obtain the NDF section string.
*     AXIS = INTEGER (Given)
*        The axis index of the dimension that is being detrended.
*     NDIM = INTEGER (Given)
*        The number of NDF dimensions.
*     DIMS( NDIM ) = INTEGER*8 (Given)
*        The dimensions of the NDF.
*     NCSECT = INTEGER (Given)
*        The number of characters in the returned section.
*     SECT = CHARACTER * ( * ) (Returned)
*        The NDF section.  Allow at least forty characters for this.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  The default section is about the NDF centre with extents set to
*     the square root of the number of elements along each axis, with
*     a minimum of ten or the axis dimension.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council
*     Copyright (C) 2008 Science & Technology Facilities Council
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
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 June 1 (MJC):
*        Original version.
*     2008 December 18 (MJC):
*        Allow for degenerate axes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors

*  Arguments Given:
      CHARACTER * ( * ) PARNAM
      INTEGER AXIS
      INTEGER NDIM
      INTEGER*8 DIMS( NDIM )

*  Arguments Returned:
      INTEGER NCSECT
      CHARACTER * ( * ) SECT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      LOGICAL DEFSEC             ! Define NDF section automatically?
      INTEGER ENWORD( NDF__MXDIM ) ! End positions of section elements
      INTEGER I                  ! Loop variable
      INTEGER NWORD              ! Number of words in section
      INTEGER SDIM               ! Automatic-masking sample dimension
      INTEGER STWORD( NDF__MXDIM ) ! Start positions of section elements
      CHARACTER * ( 40 ) WORDS( NDF__MXDIM ) ! NDF sections by axis
      INTEGER UDIM               ! Number of useful dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for degenerate axes.
      UDIM = 0
      DO I = 1, NDF__MXDIM
         IF ( DIMS( I ) .GT. 1 ) UDIM = UDIM + 1
      END DO

*  If the NDF is one-dimensional, there's no section to access, and we
*  set the null default, meaning use all the data.
      NCSECT = 0
      SECT = ''

      IF ( UDIM .GT. 1 ) THEN
         DEFSEC = .FALSE.

*  Obtain the NDF section.
         CALL PAR_GET0C( PARNAM, SECT, STATUS )
         NCSECT = CHR_LEN( SECT )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            DEFSEC = .TRUE.

*  Along the axis whose lines are to be detrended, the section should
*  be the full pixel axis.  Thus split the section into its
*  comma-separated elements, remove any subsections along the detrend
*  axis, and reform the section string.  This step also removes surplus
*  dimensions.
         ELSE
            CALL CHR_TRCHR( ',', ' ', SECT, STATUS )
            CALL CHR_DCWRD( SECT, NDF__MXDIM, NWORD, STWORD, ENWORD,
     :                      WORDS, STATUS )

            SECT = ' '
            NCSECT = 0
            DO I = 1, NDIM
               IF ( I .NE. AXIS ) THEN
                  CALL CHR_APPND( WORDS( I ), SECT, NCSECT )
               END IF
               IF ( I .LT. NDIM ) CALL CHR_APPND( ',', SECT, NCSECT )
            END DO
         END IF

*  Form a default section.  It is arbitrary, but call a representative
*  region the square root of the dimension along each pixel axis
*  located about its centre.  This does not apply to the axis whose
*  lines are to be fitted.
         IF ( DEFSEC ) THEN
            SECT = ' '
            NCSECT = 0
            DO I = 1, NDIM
               IF ( I .NE. AXIS ) THEN
                  SDIM = MAX( MIN( 10, DIMS( I ) ),
     :                        NINT( SQRT( REAL( DIMS( I ) ) ) ) )
                  CALL CHR_APPND( '~', SECT, NCSECT )
                  CALL CHR_PUTI( SDIM, SECT, NCSECT )
               END IF
               IF ( I .LT. NDIM ) CALL CHR_APPND( ',', SECT, NCSECT )
            END DO

         END IF
      END IF

      END
