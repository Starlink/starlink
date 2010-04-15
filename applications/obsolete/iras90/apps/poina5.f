	SUBROUTINE POINA5( DETLBD, DETUBD, IDET, MAXDET, SMPLBD, SMPUBD,
     :                     INDATA, DETSMP, OUDATA, OUTSMP, STATUS )
*+
*  Name:
*     POINA5

*  Purpose:
*     To copy the data for a single detector from a two dimensional
*     array to a single dimension array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA5( DETLBD, DETUBD, IDET, MAXDET, SMPLBD, SMPUBD,
*                  INDATA, DETSMP, OUDATA, OUTSMP, STATUS )

*  Description:
*     To copy the data for a single detector from a two dimensional
*     array to a single dimension array. The subroutine also copies the
*     bounds of the length of the output array to be used to a one
*     dimensional array.

*  Arguments:
*     DETLBD = INTEGER (Given)
*        Lower limit of the detector index in the current NDF
*     DETUBD = INTEGER (Given)
*        Upper limit of the detector index in the current NDF
*     IDET = INTEGER (Given)
*        Detector index for the detector whose noise is to be calculated
*     MAXDET = INTEGER (Given)
*        Maximum number of detectors ( used as an array size only )
*     SMPLBD = INTEGER (Given)
*        Lower limit of the sample index in the current NDF
*     SMPUBD = INTEGER (Given)
*        Upper limit of the sample index in the current NDF
*     INDATA( SMPLBD : SMPUBD , DETLBD : DETUBD ) = REAL
*        The input two dimensioned CRDD data array
*     DETSMP( MAXDET, 2 )  = INTEGER (Given)
*        The lower and upper sample numbers that should be considered in
*        subsequent analysis for all detectors.
*     OUDATA( SMPLBD : SMPUBD ) = REAL (Returned)
*        The output single dimension CRDD detector data array
*     OUTSMP( 2 )  = INTEGER (Given)
*        The lower and upper sample numbers that should be considered in
*        subsequent analysis for the current detector as a single dimensioned
*	 array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEPT-1994 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitve constants inc. VAL__BAD

*  Arguments Given:
      INTEGER DETLBD
      INTEGER DETUBD
      INTEGER IDET
      INTEGER MAXDET
      INTEGER SMPLBD
      INTEGER SMPUBD
      REAL INDATA( SMPLBD : SMPUBD , DETLBD : DETUBD )
      INTEGER DETSMP( MAXDET, 2 )

*  Arguments Returned:
      REAL OUDATA( SMPLBD : SMPUBD )
      INTEGER OUTSMP( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISAMP              ! Sample index for loops
      INTEGER OUTLEN             ! Index for output data array
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the detector sample numbers marking the ends of the range to be
*  considered into a single dimension array
      OUTSMP( 1 ) = DETSMP( IDET, 1)
      OUTSMP( 2 ) = DETSMP( IDET, 2)


*  Copy the detector data for the current detector from the two dimensional
*  CRDD data array to a single dimensioned array starting with the sample
*  associated with SMPLBD in first position of output array
      OUTLEN = 0
      DO ISAMP = SMPLBD, SMPUBD
         OUTLEN = OUTLEN + 1
         OUDATA( OUTLEN ) = INDATA( ISAMP, IDET )
      END DO



      END
