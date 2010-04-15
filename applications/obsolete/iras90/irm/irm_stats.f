      SUBROUTINE IRM_STATS( BSMP, ESMP, BROW, EROW, IN, NCLIP, CLIP,
     :                      MXMNCL, MEANCL, SGMACL, MX, MN, MEAN, SGMA,
     :                      NVAL, STATUS )
*+
*  Name:
*     IRM_STATS

*  Purpose:
*     Statistically analyse each row of a 2-D array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STATS( BSMP, ESMP, BROW, EROW, IN, NCLIP, CLIP,
*                     MXMNCL, MEANCL, SGMACL, MX, MN, MEAN, SGMA,
*                     NVAL, STATUS )

*  Description:
*     This routine returns the statistical information about each row
*     of a 2-D array, comprising the mean, the maximum and minimum,
*     the standard deviation, and the number of valid samples of each
*     row. The statistics can be computed either with or without
*     clipping. If a row contains no valid sample, the returned max.
*     min. and standard deviation of the row will be the Starlink bad
*     values, VAL__BADR.


*  Arguments:
*     BSMP = INTEGER (Given)
*        The begin index of the samples in each row of the input array.
*     ESMP = INTEGER (Given)
*        The end index of the samples in each row of the input array.
*     BROW = INTEGER (Given)
*        The begin index of the rows in each row of the input array.
*     EROW = INTEGER (Given)
*        The end index of the rows in each row of the input array.
*     IN( BSMP: ESMP, BROW: EROW ) = REAL (Given)
*        Input array whose rows are to be analysed.
*     NCLIP = INTEGER (Given)
*        The number of clipping cycles.
*     CLIP( NCLIP ) = REAL (Given)
*        The array of standard deviation thresholds. The analysis will
*        only take into account those samples whose values are in tha
*        range MEAN-CLIP*SIGMA to MEAN+CLIP*SIGMA.
*     MXMNCL = LOGICAL (Given)
*        MXMNCL=TRUE means that the returned maximum and minimum values
*        should be those after clipping; MXMNCL=FALSE causes the routine
*        to return unclipped extreme values.
*     MEANCL = LOGICAL (Given)
*        MEANCL=TRUE causes the returned mean values to be those after
*        clipping; MEANCL=FALSE causes the routine to return unclipped
*        averages.
*     SGMACL = LOGICAL (Given)
*        SGMACL=TRUE means that the returned standard deviations should
*        be those after clipping; SGMACL=FALSE causes the routine
*        to return unclipped standard deviations.
*     MX( BROW: EROW ) = REAL (Returned)
*        Maximum value of each row with or without clipping, depending
*        on the value of MXMNCL.
*     MN( BROW: EROW ) = REAL (Returned)
*        Minimum value of each row with or without clipping, depending
*        on the value of MXMNCL.
*     MEAN( BROW: EROW ) = REAL (Returned)
*        Mean value of each row with or without clipping, depending
*        on the value of MEANCL.
*     SGMA( BROW: EROW ) = REAL (Returned)
*        Standard deviations of each row with or without clipping,
*        depending on the value of SGMACL.
*     NVAL( BROW: EROW ) = INTEGER (Returned)
*        Number of valid samples in each row.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-JAN-1991 (WG):
*        Original version.
*     16-MAR-1992 (MJC):
*        Corrected a bug in passing the number of clip operations.
*        Called up-to-date KAPPA application to perform the
*        calculations.  Corrected typo's and improved the parameter
*        descriptions.
*     22-MAY-1992 (DSB):
*        Dependancy on KAPPA internal routines removed. (KPG1_STATR
*        included in IRM library as IRM1_STATR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BROW
      INTEGER EROW
      REAL IN( BSMP: ESMP, BROW: EROW )
      INTEGER NCLIP
      REAL CLIP( NCLIP )

      LOGICAL MXMNCL
      LOGICAL MEANCL
      LOGICAL SGMACL

*  Arguments Returned:
      INTEGER NVAL( BROW: EROW )
      REAL MX( BROW: EROW )
      REAL MN( BROW: EROW )
      REAL MEAN( BROW: EROW )
      REAL SGMA( BROW: EROW )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      INTEGER MAXPOS, MINPOS     ! Positions of the first max. and min.
                                 ! values along the sample index.
      INTEGER MXPSCL, MNPSCL     ! Postitions of the first max. and min.
                                 ! along sample index after clipping.
      INTEGER NUMCLP             ! Number of clipping cycles.
      INTEGER NGOOD              ! Number of valid samples in a row.
      INTEGER NGODCL             ! Number of valid samples in a row
                                 ! after clipping

      DOUBLE PRECISION AVG       ! Mean value of a row.
      DOUBLE PRECISION AVGCL     ! Mean value of a row after clipping.
      DOUBLE PRECISION DMAX      ! Maximum value of a row.
      DOUBLE PRECISION DMAXCL    ! Maximum value of a row after clipping
      DOUBLE PRECISION DMIN      ! Minimum value of a row.
      DOUBLE PRECISION DMINCL    ! Minimum value of a row after clipping
      DOUBLE PRECISION STDDEV    ! Standard deviation of a row
      DOUBLE PRECISION STDVCL    ! Standard deviation of a row after
                                 ! clipping.
      DOUBLE PRECISION TOTAL     ! Total value of a row.
      DOUBLE PRECISION TOTLCL    ! Total value of a row after clipping.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If none of max. and min., mean, and standard deviation should be the
*  one after clipping, set clipping number as 0.
      IF ( (.NOT.MXMNCL) .AND. (.NOT.MEANCL) .AND. (.NOT.SGMACL) ) THEN
         NUMCLP = 0

*  Otherwise use the specified number of clipping.
      ELSE
         NUMCLP = NCLIP
      END IF

*  Enter a do loop to process the rows of the input array one by one.
      DO I = BROW, EROW

*  Call subroutine IRM1_STATR to get the statistical information about
*  this row.
         CALL IRM1_STATR( .TRUE., ESMP - BSMP + 1, IN( 1, I ), NUMCLP,
     :                    CLIP, NGOOD, MINPOS, DMIN, MAXPOS, DMAX,
     :                    TOTAL, AVG, STDDEV, NGODCL, MNPSCL, DMINCL,
     :                    MXPSCL, DMAXCL, TOTLCL, AVGCL, STDVCL,
     :                    STATUS )

*  Pass the result of IRM1_STATR to the returned argument according to
*  the clipping requirement. Get the max. value of the row.
         IF ( MXMNCL ) THEN
            IF ( NGODCL .GT. 0 ) THEN
               MX( I ) = REAL( DMAXCL )
               MN( I ) = REAL( DMINCL )
            ELSE
               MX( I ) = VAL__BADR
               MN( I ) = VAL__BADR
            END IF
         ELSE
            IF ( NGOOD .GT. 0 ) THEN
               MX( I ) = REAL( DMAX )
               MN( I ) = REAL( DMIN )
            ELSE
               MX( I ) = VAL__BADR
               MN( I ) = VAL__BADR
            END IF
         END IF

*  Get the mean value of the row.
         IF ( MEANCL ) THEN
            IF ( NGODCL .GT. 0 ) THEN
               MEAN( I ) = REAL( AVGCL )
            ELSE
               MEAN( I ) = VAL__BADR
            END IF
         ELSE
            IF ( NGOOD .GT. 0 ) THEN
               MEAN( I ) = REAL( AVG )
            ELSE
               MEAN( I ) = VAL__BADR
            END IF
         END IF

*  Get the standard deviation of the row.
         IF ( SGMACL ) THEN
            IF ( NGODCL .GT. 0 ) THEN
               SGMA( I ) = REAL( STDVCL )
            ELSE
               SGMA( I ) = VAL__BADR
            END IF
         ELSE
            IF ( NGODCL .GT. 0 ) THEN
               SGMA( I ) = REAL( STDDEV )
            ELSE
               SGMA( I ) = VAL__BADR
            END IF
         END IF

*  Get the number of valid samples in the row.
         IF ( NUMCLP .EQ. 0 ) THEN
            NVAL( I ) = NGOOD
         ELSE
            NVAL( I ) = NGODCL
         END IF

      END DO

*  End of the routine.
      END
