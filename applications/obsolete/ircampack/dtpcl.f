      SUBROUTINE DTPCL( INDF, UBND, LBND, SDIM, TYPDAT, STATUS )
*+
*  Name:
*     DTPCL

*  Purpose:
*     Find a typical data value in a 2D NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTPCL( INDF, UBND, LBND, SDIM, TYPDAT, STATUS )

*  Description:
*     This routine finds a data value which is used to define the
*     default vector scaling in VECPLOT. It first estimates the mean and
*     standard deviation of the data in th sub-array, and then rejects
*     values further than 4 sigma from the mean. It then recomputes the
*     mean and standard deviation. This is done 4 times, and the
*     returned "typical data value" is then the sum of the final mean
*     value and standard deviation.

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF.
*     UBND( 2 ) = INTEGER (Given)
*        Upper bounds of the two significant dimensions.
*     LBND( 2 ) = INTEGER (Given)
*        Upper bounds of the two significant dimensions.
*     SDIM( 2 ) = INTEGER (Given)
*        The indices of the two significant dimensions.
*     TYPDAT = REAL (Returned)
*        A typical data value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

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
      DOUBLE PRECISION
     :        DMAX,              ! Max. data value before clipping
     :        DMIN,              ! Min. data value before clipping
     :        DMAXC,             ! Max. data value after clipping
     :        DMINC,             ! Min. data value after clipping
     :        MEAN,              ! Mean before clipping
     :        MEANC,             ! Mean after clipping
     :        STDEV,             ! Standard deviation before clipping
     :        STDEVC,            ! Standard deviation after clipping
     :        SUM,               ! Sum before clipping
     :        SUMC               ! Sum after clipping

      INTEGER
     :        EL,                ! No. of mapped elements
     :        I,                 ! Loop count
     :        IMAX,              ! Position of max value before clipping
     :        IMIN,              ! Position of min value before clipping
     :        IMAXC,             ! Position of max value after clipping
     :        IMINC,             ! Position of min value after clipping
     :        INDFS,             ! NDF section identifier
     :        NGOOD,             ! No. of valid values before clipping
     :        NGOODC,            ! No. of valid values after clipping
     :        PNTR,              ! Pointer to mapped data array
     :        SLBND( NDF__MXDIM),! Low bounds for all NDF dimensions
     :        SUBND( NDF__MXDIM) ! High bounds for all NDF dimensions

      LOGICAL
     :        BAD                ! May bad pixels be present?

      REAL
     :        CLIP( 4 )          ! Clipping levels (multiples of sigma)

*  Local Data Initialisation:
      DATA CLIP/ 4.0, 4.0, 4.0, 4.0 /

*.

*  Check inherited global status.
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

*  Obtain a section of the input NDF covering the region to be
*  plotted.
      CALL NDF_SECT( INDF, NDF__MXDIM, SLBND, SUBND, INDFS, STATUS )

*  Map the data array of the NDF section.
      CALL NDF_MAP( INDFS, 'DATA', '_REAL', 'READ', PNTR, EL, STATUS )

*  See if the section may contain bad values.
      CALL NDF_BAD( INDFS, 'DATA', .FALSE., BAD, STATUS )

*  Compute the statistics.
      CALL KPG1_STATR( BAD, EL, %VAL( PNTR ), 4, CLIP, NGOOD, IMIN,
     :                 DMIN, IMAX, DMAX, SUM, MEAN, STDEV, NGOODC,
     :                 IMINC, DMINC, IMAXC, DMAXC, SUMC, MEANC, STDEVC,
     :                 STATUS )

*  Report an error if there are no good pixel values in the first
*  NDF.
      IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDFS )
         CALL ERR_REP( 'DTPCL_NOGOOD', 'There are no good values in '//
     :                 '^NDF', STATUS )
         GO TO 999
      END IF

*  Report an error if all data values are effectively zero.
      IF( ABS( DMAX ) .LE. VAL__SMLR .AND.
     :    ABS( DMIN ) .LE. VAL__SMLR .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS  = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDFS )
         CALL ERR_REP( 'DTPCL_ALLZERO', 'There are no '//
     :                 'non-zero values in ^NDF', STATUS )
         GO TO 999
      END IF

*  Calculate the "typical value".
      IF( MEANC .NE. VAL__BADD .AND. STDEVC .NE. VAL__BADD ) THEN
         TYPDAT = MEANC + STDEVC
      ELSE
         TYPDAT = MEAN
      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Annul the identifier to the section.
      CALL NDF_ANNUL( INDFS, STATUS )

      END
