      SUBROUTINE CDCRA6( NCRDD, NDFID, IRCID, SCNDIR, INTER, WEIGHT,
     :                   SCNLEN, NCROS, CRSFLX, CRSDTX, CRSSMP, CRSFLG,
     :                   DATARY, WGTARY, STATUS )
*+
*  Name:
*     CDCRA6

*  Purpose:
*     Put crossing trace sections into temporary arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  CDCRA6( NCRDD, NDFID, IRCID, SCNDIR, INTER, WEIGHT,
*                   SCNLEN, NCROS, CRSFLX, CRSDTX, CRSSMP, CRSFLG,
*                   DATARY, WGTARY, STATUS )

*  Description:
*     This subroutine is used to extract the crossing sections from
*     their NDF files and put into a temporary array. The crossing
*     traces are aligned with the first crossing which is closest to the
*     expected source. And if a crossing trace is in the direction other
*     than that of the first crossing, the trace will be reversed when
*     put into the array.

*  Arguments:
*     NCRDD = INTEGER (Given)
*        Number of input CRDD NDF file.
*     NDFID( NCRDD ) = INTEGER (Given)
*        NDF id of the input CRDD NDFs.
*     IRCID( NCRDD ) = INTEGER (Given)
*        IRC id of the input CRDD NDFs
*     SCNDIR( NCRDD ) = LOGICAL
*        The direction of each scan. True means from north to south.
*     INTER = CHARACTER*( * ) (Given)
*        Specify the method used while alignning traces. It can be
*        either 'LINEAR' or 'NEAREST'.
*     WEIGHT = CHARACTER*( * ) (Given)
*        The method to weight the traces while coadding. If VARIANCE
*        are to be used the variance corresponding to the crossing trace
*        sections are to be extract together with the crossing data.
*     SCNLEN = INTEGER (Given)
*        The length of the crossing trace section to be extracted.
*     NCROS = INTEGER (Given)
*        Number of crossings.
*     CRSFLX( NCROS ) = INTEGER (Given)
*        File indices of the NDF the crossing traces belong to.
*     CRSDTX( NCROS ) = INTEGER (Given)
*        Detector indices of the crossing traces.
*     CRSSMP( NCROS ) = INTEGER (Given)
*        Crossing sample of  the crossing traces.
*     CRSFLG( NCROS ) = INTEGER (Returned)
*        The flag of each crossing. 0 - the crossing has not been
*        extracted (this is for the internal use by this subroutine ).
*        1 - the original crossing trace section contains more than
*        half length of bad samples and hence should be discarded.
*        2 - the crossing trace section is valid and extracted,
*        3 - the crossing will be extracted (this is for the internal
*        use by this subroutine ).
*     DATARY( SCNLEN, NCROS ) = REAL (Returned)
*        Alignned crossing traces.
*     WGTARY( SCNLEN, NCROS ) = REAL (Returned)
*        Weight of the crossing trace sections.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     25-NOV-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants
      INCLUDE 'I90_DAT'          ! IRAS90 package constants

*  Arguments Given:
      INTEGER NCRDD
      INTEGER NDFID( NCRDD )
      INTEGER IRCID( NCRDD )
      LOGICAL SCNDIR( NCRDD )
      INTEGER SCNLEN, NCROS
      CHARACTER*( * ) INTER
      CHARACTER*( * ) WEIGHT
      INTEGER CRSFLX( NCROS ), CRSDTX( NCROS )
      REAL CRSSMP( NCROS )

*  Arguments Returned:
      INTEGER CRSFLG( NCROS )
      REAL DATARY( SCNLEN, NCROS )
      REAL WGTARY( SCNLEN, NCROS )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Det. Number from its index

*  Local Variables:
      CHARACTER*( 10 ) COMP      ! List of components to be mapped
      INTEGER COMPLN             ! Used length of COMP
      CHARACTER*( 25 ) CRDUNT    ! Units of current CRDD NDF
      INTEGER DET                ! Detect number
      INTEGER DETNO( I90__MAXDT ) ! Detect number of each trace in NDF
      INTEGER DETX               ! Do loop index of the detector index
      INTEGER EL                 ! Number of element of a mapped array
      INTEGER FLX                ! File index
      CHARACTER*( 25 ) FSTUNT    ! Units of the first crossing CRDD NDF
      INTEGER I, J               ! Do loop indices
      INTEGER LBND( 2 ), UBND( 2 ) ! Bounds of an NDF data array
      INTEGER NBAD               ! Number of bad samples in a section
      INTEGER NDIM               ! Number of dimention of the data array
      INTEGER PNTR( 2 )          ! Pointer to mapped data array
      LOGICAL REVS               ! Reversing trace flag
      REAL SCALE( I90__MAXDT )   ! Scale required to convert units
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the crossing flag.
      DO I = 1, NCROS
         CRSFLG( I )  = 0
      END DO

*  Processing the crossings one by one.
      DO I = 1, NCROS

*  Process this crossing only if it hasn't been extracted,
         IF ( CRSFLG( I ) .EQ. 0 ) THEN
            CRSFLG( I ) = 3

*  Flag out the remaining crossings which are from the same NDF as
*  this one.
            FLX = CRSFLX( I )
            DO J = I + 1, NCROS
               IF ( CRSFLX( J ) .EQ. FLX ) CRSFLG( J ) = 3
            END DO

*  Find whether the reversing is required.
            IF ( SCNDIR( FLX ) .EQV. SCNDIR( CRSFLX( 1 ) ) ) THEN
               REVS = .FALSE.
            ELSE
               REVS = .TRUE.
            END IF

*  Get the shap of the CRDD NDF these crossing belong to and map the
*  data-array (and variance-array if required ) of the NDF.
            CALL NDF_BOUND( NDFID( FLX ), 2, LBND, UBND, NDIM, STATUS )

*  If the variance of the CRDD data is to be used to weight the data,
*  map it together with the data array.
            IF ( WEIGHT( : 8 ) .EQ. 'VARIANCE' ) THEN
               COMP = 'Data,Variance'
               COMPLN = 13

*  Otherwise only map the data array.
            ELSE
               COMP = 'Data'
               COMPLN = 4
            END IF
            CALL NDF_MAP( NDFID( FLX ), COMP( : COMPLN ), '_REAL',
     :                   'READ', PNTR, EL, STATUS )

*  If this NDF contains the first crossing, no units conversion is
*  needed since the output NDF will be in the same unit as this one.
            IF ( FLX .EQ. CRSFLX( 1 ) ) THEN
               DO DETX = LBND( 2 ), UBND( 2 )
                  SCALE( DETX - LBND( 2 ) + 1 ) = 1.0
               END DO

*  Otherwise, find the units of the two NDFs.
            ELSE
               CALL NDF_CGET( NDFID( FLX ), 'Units', CRDUNT, STATUS )
               CALL NDF_CGET( NDFID( CRSFLX( 1 ) ), 'Units', FSTUNT,
     :                        STATUS )

*  Find the detector number of the traces contained in prsent CRDD NDF.
               DO DETX = LBND( 2 ), UBND( 2 )
                  DETNO( DETX - LBND( 2 ) + 1 ) =
     :                          IRC_DETNO( IRCID( FLX ), DETX, STATUS )
               END DO

*  Get the conversion scales for the detector traces contained in the
*  present CRDD NDF.
               CALL IRM_UNTCV( CRDUNT, FSTUNT,
     :                         UBND( 2 ) - LBND( 2 ) + 1, DETNO, SCALE,
     :                         STATUS )
            END IF

*  Extract the data of the crossing section from the CRDD NDF.
            CALL CDCRB0( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ),
     :                  %VAL( PNTR( 1 ) ), SCALE, REVS, INTER, SCNLEN,
     :                   NCROS, CRSDTX, CRSSMP, CRSFLG, DATARY, STATUS )

*  If variance is to be used, extract the variance of the crossing
*  section from the CRDD NDF.
            IF ( WEIGHT( : 8 ) .EQ. 'VARIANCE' ) THEN
               CALL CDCRB0( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ),
     :                     %VAL( PNTR( 2 ) ), SCALE, REVS, INTER,
     :                     SCNLEN, NCROS, CRSDTX, CRSSMP, CRSFLG,
     :                     WGTARY, STATUS )
            END IF

*  Unmap the data array.
            CALL NDF_UNMAP( NDFID( FLX ), COMP( : COMPLN ), STATUS )
         END IF
      END DO

*  If variance is extracted, get its reciprocal, leave bad unchanged and
*  set 0 variance as bad.
      IF ( WEIGHT( : 8 ) .EQ. 'VARIANCE' ) THEN
         DO I = 1, NCROS
            DO J = 1, SCNLEN
               IF ( WGTARY( J, I ) .NE. VAL__BADR  .AND.
     :              ABS( WGTARY( J, I ) ) .GT. VAL__SMLR ) THEN
                  WGTARY( J,  I ) = 1.0 / WGTARY( J, I )
               ELSE
                  WGTARY( J, I ) = VAL__BADR

*  When variance is bad, set corresponding samples bad as well.
                  DATARY( J, I ) = VAL__BADR
               END IF
            END DO
         END DO

*  If equal weighting is to be used, set all element of weight array
*  as unit.
      ELSE IF ( WEIGHT( : 5 ) .EQ. 'EQUAL' ) THEN
         DO I = 1, NCROS
            DO J = 1, SCNLEN
               WGTARY( J, I ) = 1.0
            END DO
         END DO

*  If NEFD weight is to be used, set the value of the weight array
*  according to its corresponding detector number.
      ELSE IF ( WEIGHT( : 4 ) .EQ. 'NEFD' ) THEN
         DO I = 1, NCROS
            DET = IRC_DETNO( IRCID( CRSFLX( I ) ), CRSDTX( I ), STATUS )
            DO J = 1, SCNLEN
               IF ( I90__DNEFD( DET ) .GT. VAL__SMLR ) THEN
                  WGTARY( J, I ) = 1.0 / I90__DNEFD( DET )
               ELSE
                  WGTARY( J, I ) = VAL__BADR
               END IF
            END DO
         END DO
      END IF

*  Count the number of bad sample in each crossing trace section.
      DO I = 1, NCROS
         NBAD = 0
         DO J = 1, SCNLEN
            IF ( DATARY( J, I ) .EQ. VAL__BADR ) NBAD = NBAD + 1
         END DO

*  If more than half of samples are bad, set the flag of this crossing
*  as 1.
         IF ( 2 * NBAD .GE. SCNLEN ) CRSFLG( I ) = 1
      END DO

      END
