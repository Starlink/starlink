      SUBROUTINE ENMAP( STATUS )
*+
*  Name:
*     ENMAP

*  Purpose:
*     Computes the mean channel in X,Y plane of spectral image

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL ENMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Takes a spectral image as input and finds the mean corrected
*     amplitude channel of all the photons recorded in each image pixel.
*     It then writes an output image containing these values

*  Usage:
*     enmap {parameter_usage}

*  Environment Parameters:
*      INP = CHAR (read)
*         Name of input spectral image
*      OUT = CHAR (read)
*         Name of output image
*      ENAXIS = INTEGER (read)
*         The energy axis. Only used if the axis cannot be derived
*         from the axis labels

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     enmap, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RDS: Richard Saxton (Starlink, University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Sep 1990 V1.3-0 (RDS):
*        Original version.
*     30 Jul 1991 V1.3-2 (RDS):
*        Fixed bug causing error if axes weren't ordered X,Y,E
*      7 Jan 1993 V1.6-0 (RDS):
*        Allow a thresholding requirement
*     28 Jun 1993 V1.7-0 (DJA):
*        Handle quality and writes full history
*     28 Feb 1994 V1.7-1 (DJA):
*        Use BIT_ routines to do bit manipulations
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      8 Dec 1995 V2.0-0 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'ENMAP Version 2.1-0' )

*  Local Variables:
      CHARACTER*80              TEXT                    ! History text

      REAL                      THRESH                  ! Threshold below which to zero

      INTEGER                   AXPTR                   ! Input energy axis data
      INTEGER                   ENAX                    ! Energy axis number
      INTEGER                   IDPTR                   ! Input data
      INTEGER			IFID			! Input dataset id
      INTEGER			IFILES			! USI input file info
      INTEGER                   IQPTR                   ! Input quality
      INTEGER                   NDIM, DIMS(3)           ! Data dimensions
      INTEGER                   ODIM(2)                 ! O/p dimension
      INTEGER                   ODPTR                   ! Output data
      INTEGER			OFID			! Output dataset id
      INTEGER                   OQPTR                   ! Output quality
      INTEGER                   ORDER(3)                ! Order of input axes
                                                        ! energy is order(3)
      INTEGER                   TLEN                    ! Length of text used
      INTEGER                   TPTR                    ! Workspace

      LOGICAL                   OK
      LOGICAL                   QOK                     ! Input quality present
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get name of input file and output file
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )

*  Is data array there ?
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, 3, DIMS, NDIM, STATUS )
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT('Error accessing data array')
        GOTO 99
      END IF

*  Check that input data is 3 dimensional
      IF ( NDIM .NE. 3 ) THEN
        CALL MSG_PRNT('** Input data must be 3-dimensional **')
        GOTO 99
      END IF

*  Get thresholding value from the environment.
      CALL USI_GET0R( 'THRESH', THRESH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Find which axis is the energy axis
      CALL BDI0_FNDAXC( IFID, 'E', ENAX, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT( 'Which of these axes is the energy axis?' )
        CALL AXIS_TLIST( IFID, NDIM, STATUS )
        CALL USI_GET0I( 'ENAXIS', ENAX, STATUS )
        IF ( (ENAX.LT.1) .OR. (ENAX.GT.NDIM) ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Illegal axis number', STATUS )
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Create order array to show where energy axis is
      ORDER(3) = ENAX
      IF ( ENAX .EQ. 1 ) THEN
        ORDER(1) = 2
        ORDER(2) = 3
      ELSE IF ( ENAX .EQ. 2 ) THEN
        ORDER(1) = 1
        ORDER(2) = 3
      ELSE
        ORDER(1) = 1
        ORDER(2) = 2
      END IF

*  Set dimensions of output array and link to output file
      ODIM(1) = DIMS(ORDER(1))
      ODIM(2) = DIMS(ORDER(2))
      CALL BDI_LINK( 'BinDS', 2, ODIM, 'REAL', OFID, STATUS )

*  Copy the relevant parts of the input file to the output file
      CALL BDI_COPY( IFID, 'Title,Label,Units', OFID, ' ', STATUS )
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Warning: Error copying auxilliary '/
     :             /'information from the input to the output file')
        CALL ERR_ANNUL( STATUS )
      END IF

*  Map the input data array
      CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error mapping data array')
        GOTO 99
      END IF

*  Is quality present?
      CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
      IF ( QOK ) THEN
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', IQPTR,
     :                 STATUS )
      END IF

*  Attempt to map the energy axis. NB: this routine fills the array with
*  values 1-N if the axis isn't present in the input file
      CALL BDI_AXMAPR( IFID, ENAX, 'Data', 'READ', AXPTR, STATUS )

*  Create output array
      CALL BDI_MAPR( OFID, 'Data', 'WRITE/ZERO', ODPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Create output quality
      IF ( QOK ) THEN
        CALL BDI_MAPUB( OFID, 'Quality', 'WRITE/QBAD', OQPTR, STATUS )
        CALL BDI_PUT0UB( OFID, 'QualityMask', QUAL__MASK, STATUS )
      END IF

*  Map some work space and zero it
      CALL DYN_MAPR( 2, ODIM, TPTR, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error mapping temporary space')
        GOTO 99
      END IF
      CALL ARR_INIT1R( 0.0, ODIM(1)*ODIM(2), %VAL(TPTR), STATUS )

*  Calculate output array
      CALL ENMAP_CREATE( DIMS, DIMS(1), DIMS(2), DIMS(3), %VAL(IDPTR),
     :                   QOK, %VAL(IQPTR), ORDER, DIMS(ENAX),
     :                   %VAL(AXPTR), ODIM(1), ODIM(2), THRESH,
     :                   %VAL(TPTR), %VAL(ODPTR), %VAL(OQPTR), STATUS )

*  Create output axes
      CALL BDI_AXCOPY( IFID, ORDER(1), ' ', OFID, 1, STATUS )
      CALL BDI_AXCOPY( IFID, ORDER(2), ' ', OFID, 2, STATUS )

*  Copy history from the original file and add our record
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Create text strings for history
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_PTXTI( OFID, IFILES, .TRUE., STATUS )

*  Write threshold value
      CALL MSG_SETR( 'THRESH', THRESH )
      CALL MSG_MAKE( 'Threshold value ^THRESH', TEXT, TLEN )
      CALL HSI_PTXT( OFID, 1, TEXT(:TLEN), STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  ENMAP_CREATE - Fill output array
      SUBROUTINE ENMAP_CREATE( DIMS, DIM1, DIM2, DIM3, INDAT, QOK,
     :                         INQUAL, ORDER, NEN, ENCHAN,
     :                         ODIM1, ODIM2, THRESH, NORM, OUTDAT,
     :                         OUTQUAL, STATUS )
*
*    Description :
*     <description of what the subroutine does>
*    History :
*     17-SEP-1990    original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER DIMS(3)              ! Dimensions of input array
      INTEGER DIM1,DIM2,DIM3       ! Dims of input array
      REAL INDAT(DIM1,DIM2,DIM3)   ! Input data
      LOGICAL INQUAL(DIM1,DIM2,DIM3)
      LOGICAL                  QOK              ! Input quality present

      INTEGER ORDER(3)             ! Order of the input data - energy is
*                                  ! axis(ORDER(3))
      INTEGER NEN                  ! Number of energy bins in input file
      REAL ENCHAN(NEN)             ! PH channel of the centre of each EN. bin
      INTEGER ODIM1,ODIM2          ! Output dimensions
      REAL THRESH                  ! Value below which to set output to zero
*
*    Import-Export :
*
      REAL NORM(ODIM1,ODIM2)       ! Work array
      REAL OUTDAT(ODIM1,ODIM2)     ! Output data
      BYTE OUTQUAL(ODIM1,ODIM2)    ! Output quality
*
*    Local variables :
*
      INTEGER ALP(3),LP1,LP2,LP3
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over outer 2 dimensions in cube
      DO LP2=1,DIM2
        ALP(2)=LP2
        DO LP1=1,DIM1
          ALP(1)=LP1

*        Switch on presence of input quality
          IF ( QOK ) THEN

*          Loop over 3rd dimension
            DO LP3=1,DIM3
              ALP(3)=LP3

*            Good input pixel?
              IF ( INQUAL(LP1,LP2,LP3) ) THEN

*              Sum the energy contributions in this pixel multiplying by
*              the energy bin number
                OUTDAT(ALP(ORDER(1)),ALP(ORDER(2))) =
     :             OUTDAT(ALP(ORDER(1)),ALP(ORDER(2))) +
     :               INDAT(LP1,LP2,LP3) *
     :                 ENCHAN(ALP(ORDER(3)))

*              Calculate the normalising factor which is the sum of the
*              counts in this image pixel
                NORM(ALP(ORDER(1)),ALP(ORDER(2))) =
     :             NORM(ALP(ORDER(1)),ALP(ORDER(2))) +
     :               INDAT(LP1,LP2,LP3)

*              This output pixel is now ok
                OUTQUAL(ALP(ORDER(1)),ALP(ORDER(2))) = QUAL__GOOD

              END IF

            END DO

          ELSE

*          Loop over 3rd dimension
            DO LP3=1,DIM3
              ALP(3)=LP3

*            Sum the energy contributions in this pixel multiplying by
*            the energy bin number
              OUTDAT(ALP(ORDER(1)),ALP(ORDER(2))) =
     :           OUTDAT(ALP(ORDER(1)),ALP(ORDER(2))) +
     :             INDAT(LP1,LP2,LP3) *
     :               ENCHAN(ALP(ORDER(3)))

*            Calculate the normalising factor which is the sum of the counts
*            in this  image pixel
              NORM(ALP(ORDER(1)),ALP(ORDER(2))) =
     :           NORM(ALP(ORDER(1)),ALP(ORDER(2))) +
     :             INDAT(LP1,LP2,LP3)

            END DO

          END IF

        END DO
      END DO

*    Normalise the array to give the mean energy of each output pixel
      IF ( QOK ) THEN
        DO LP2 = 1, DIMS(ORDER(2))
          DO LP1 = 1, DIMS(ORDER(1))
            IF ( (OUTQUAL(LP1,LP2) .EQ. QUAL__GOOD) .AND.
     :           (NORM(LP1,LP2) .GE. THRESH) .AND.
     :           (NORM(LP1,LP2) .NE. 0.0)  ) THEN
              OUTDAT(LP1,LP2) = OUTDAT(LP1,LP2) / NORM(LP1,LP2)
            ELSE
              OUTDAT(LP1,LP2) = 0.0
            END IF
          END DO
        END DO
      ELSE
        DO LP2 = 1,DIMS(ORDER(2))
          DO LP1 = 1,DIMS(ORDER(1))
            IF ( (NORM(LP1,LP2) .GE. THRESH) .AND.
     :           (NORM(LP1,LP2) .NE. 0.0)  ) THEN
              OUTDAT(LP1,LP2) = OUTDAT(LP1,LP2) / NORM(LP1,LP2)
            ELSE
              OUTDAT(LP1,LP2) = 0.0
            END IF
          END DO
        END DO
      END IF

      END
