      SUBROUTINE KPS1_DTPCL( INDF, LBND, UBND, SDIM, TYPDAT, STATUS )
*+
*  Name:
*     KPS1_DTPCL

*  Purpose:
*     Finds a typical data value in a 2-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_DTPCL( INDF, LBND, UBND, SDIM, TYPDAT, STATUS )

*  Description:
*     This routine finds a data value which is used to define the
*     default vector scaling in VECPLOT.  It first estimates the mean
*     and standard deviation of the data in th sub-array, and then
*     rejects values further than 4 sigma from the mean.  It then
*     recomputes the mean and standard deviation.  This is done four
*     times, and the returned "typical data value" is then the sum of
*     the final mean value and standard deviation.

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF.
*     LBND( 2 ) = INTEGER (Given)
*        Upper bounds of the two significant dimensions.
*     UBND( 2 ) = INTEGER (Given)
*        Upper bounds of the two significant dimensions.
*     SDIM( 2 ) = INTEGER (Given)
*        The indices of the two significant dimensions.
*     TYPDAT = REAL (Returned)
*        A typical data value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     24-SEP-1993 (DSB):
*        Original version.
*     1995 April 12 (MJC):
*        Used modern-style variable declaration.  Transposed the LBND
*        and UBND arguments.  Minor stylistic changes.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER SDIM( 2 )

*  Arguments Returned:
      REAL TYPDAT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BAD                ! May bad pixels be present?
      REAL CLIP( 4 )             ! Clipping levels (multiples of sigma)
      DOUBLE PRECISION DMAX      ! Max. data value before clipping
      DOUBLE PRECISION DMIN      ! Min. data value before clipping
      DOUBLE PRECISION DMAXC     ! Max. data value after clipping
      DOUBLE PRECISION DMINC     ! Min. data value after clipping
      INTEGER EL                 ! No. of mapped elements
      INTEGER I                  ! Loop count
      INTEGER IMAX               ! Position of max value before clipping
      INTEGER IMIN               ! Position of min value before clipping
      INTEGER IMAXC              ! Position of max value after clipping
      INTEGER IMINC              ! Position of min value after clipping
      INTEGER INDFS              ! NDF section identifier
      DOUBLE PRECISION MEAN      ! Mean before clipping
      DOUBLE PRECISION MEANC     ! Mean after clipping
      INTEGER NGOOD              ! No. of valid values before clipping
      INTEGER NGOODC             ! No. of valid values after clipping
      INTEGER PNTR               ! Pointer to mapped data array
      INTEGER SLBND( NDF__MXDIM ) ! Low bounds for all NDF dimensions
      DOUBLE PRECISION STDEV     ! Standard deviation before clipping
      DOUBLE PRECISION STDEVC    ! Standard deviation after clipping
      INTEGER SUBND( NDF__MXDIM ) ! High bounds for all NDF dimensions
      DOUBLE PRECISION SUM       ! Sum before clipping
      DOUBLE PRECISION SUMC      ! Sum after clipping

*  Local Data:
      DATA CLIP/ 4.0, 4.0, 4.0, 4.0 /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the bounds of the NDF section to be plotted.
      DO I = 1, NDF__MXDIM
         SLBND( I ) = 1
         SUBND( I ) = 1
      END DO

      SLBND( SDIM( 1 ) ) = LBND( 1 )
      SUBND( SDIM( 1 ) ) = UBND( 1 )
      SLBND( SDIM( 2 ) ) = LBND( 2 )
      SUBND( SDIM( 2 ) ) = UBND( 2 )

*  Obtain a section of the input NDF covering the region to be plotted.
      CALL NDF_SECT( INDF, NDF__MXDIM, SLBND, SUBND, INDFS, STATUS )

*  Map the data array of the NDF section.
      CALL KPG1_MAP( INDFS, 'DATA', '_REAL', 'READ', PNTR, EL, STATUS )

*  See if the section may contain bad values.
      CALL NDF_BAD( INDFS, 'DATA', .FALSE., BAD, STATUS )

*  Compute the statistics.
      CALL KPG1_STATR( BAD, EL, %VAL( CNF_PVAL( PNTR ) ),
     :                 4, CLIP, NGOOD, IMIN,
     :                 DMIN, IMAX, DMAX, SUM, MEAN, STDEV, NGOODC,
     :                 IMINC, DMINC, IMAXC, DMAXC, SUMC, MEANC, STDEVC,
     :                 STATUS )

*  Report an error if there are no good pixel values in the first
*  NDF.
      IF ( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDFS )
         CALL ERR_REP( 'KPS1_DTPCL_NOGOOD', 'There are no good '/
     :                 /'values in ^NDF', STATUS )
         GO TO 999
      END IF

*  Report an error if all data values are effectively zero.
      IF ( ABS( DMAX ) .LE. VAL__SMLR .AND.
     :     ABS( DMIN ) .LE. VAL__SMLR .AND.
     :     STATUS .EQ. SAI__OK ) THEN
         STATUS  = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDFS )
         CALL ERR_REP( 'KPS1_DTPCL_ALLZERO', 'There are no '/
     :                 /'non-zero values in ^NDF', STATUS )
         GO TO 999
      END IF

*  Calculate the "typical value".
      IF ( MEANC .NE. VAL__BADD .AND. STDEVC .NE. VAL__BADD ) THEN
         TYPDAT = MEANC + STDEVC
      ELSE
         TYPDAT = MEAN
      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Annul the identifier to the section.
      CALL NDF_ANNUL( INDFS, STATUS )

      END
