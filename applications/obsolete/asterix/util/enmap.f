*+  ENMAP - Displays image as a function of mean photon energy
      SUBROUTINE ENMAP( STATUS )
*
*    Description :
*
*      Takes a spectral_image as input and finds the mean corrected
*      amplitude channel of all the photons recorded in each image pixel.
*      It then writes an output image containing these values
*
*    Environment parameters :
*
*      INPUT     UNIV        Name of 3-d input file
*      OUTPUT    UNIV        Name of output 2-d file
*      ENAXIS    INTEGER     The energy axis. Only if the energy label
*                            is not present in the axis structure
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Richard Saxton (LTVAD::RDS)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     15 Sep 90 : V1.3-0  Original (RDS)
*     30 Jul 91 : V1.3-2  Fixed bug causing error if
*                         axes weren't ordered X,Y,E   (RDS)
*      7 Jan 93 : V1.6-0  Allow a thresholding requirement (RDS)
*     28 Jun 93 : V1.7-0  Handle quality and writes full history (DJA)
*     28 Feb 94 : V1.7-1  Use BIT_ routines to do bit manipulations (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   ILOC		! Locator to input file
      CHARACTER*(DAT__SZLOC)   OLOC		! Locator to output file
      CHARACTER*80             LABEL(3)         ! Axis labels
      CHARACTER*80             PATH(4)          ! Input dataset path for History
      CHARACTER*80             TEXT             ! History text

      REAL                     THRESH           ! Threshold below which to zero

      INTEGER                  AXLP
      INTEGER                  AXPTR            ! Input energy axis data
      INTEGER                  DIMS(DAT__MXDIM) ! Data dimensions
      INTEGER                  ENAX             ! Axis number containing energy
      INTEGER                  IDPTR            ! Input data
      INTEGER                  IQPTR            ! Input quality
      INTEGER                  NDIM             ! Data dimensionality
      INTEGER                  NLINES           ! No. of lines of history text
      INTEGER                  ODIM(2)          ! Dimensionas of output array
      INTEGER                  ODPTR            ! Output data
      INTEGER                  OQPTR            ! Output quality
      INTEGER                  ORDER(3)         ! Order of input axes
                                                ! energy is order(3)
      INTEGER                  QNDIM            ! Quality dimensions
      INTEGER                  QDIMS(DAT__MXDIM)
      INTEGER                  TLEN             ! Length of text used
      INTEGER                  TPTR             ! Workspace

      BYTE                     IQMASK           ! input quality mask

      LOGICAL                  INPRIM,OK
      LOGICAL                  QOK              ! Input quality present
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER  (VERSION = 'ENMAP Version 1.8-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix common blocks
      CALL AST_INIT

*    Get name of input file and output file
      CALL USI_ASSOC2( 'INP', 'OUT', 'READ', ILOC, OLOC,
     :                                       INPRIM, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Is data array there ?
      CALL BDA_CHKDATA( ILOC, OK, NDIM, DIMS, STATUS )
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT('Error accessing data_array')
        GOTO 999
      END IF

*    Check that input data is 3 dimensional
      IF (NDIM .NE. 3) THEN
        CALL MSG_PRNT('** Input data must be 3-dimensional **')
        GOTO 999
      END IF

*    Get thresholding value from the environment.
      CALL USI_GET0R('THRESH', THRESH, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

*   Copy the relevant parts of the input file to the output file
*    Title, label and units:
        CALL BDA_COPTEXT(ILOC, OLOC, STATUS)
*    More box:
        CALL BDA_COPMORE(ILOC, OLOC, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Warning: Error copying auxilliary '/
     :             /'information from the input to the output file')
           CALL ERR_ANNUL(STATUS)
        ENDIF

*    Map the input data array
      CALL BDA_MAPDATA( ILOC, 'READ', IDPTR, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error mapping data array')
        GOTO 999
      END IF

*    Is quality present?
      CALL BDA_CHKQUAL( ILOC, QOK, QNDIM, QDIMS, STATUS )
      IF ( QOK ) THEN
        CALL BDA_MAPQUAL( ILOC, 'READ', IQPTR, STATUS )
        CALL BDA_GETMASK( ILOC, IQMASK, STATUS )
      END IF

*    Find which axis is the energy axis
      ENAX = 0
      DO AXLP=1,3
        CALL BDA_GETAXLABEL(ILOC, AXLP, LABEL(AXLP), STATUS)
        IF ( INDEX(LABEL(AXLP), 'ENERGY') .NE. 0 .OR.
     :       INDEX(LABEL(AXLP), 'PH') .NE. 0 ) THEN
          ENAX=AXLP
        END IF
      END DO

*    Ask the user which axis is energy if the label hasn't been found
      IF ( (STATUS .NE. SAI__OK) .OR. (ENAX .EQ. 0) ) THEN

*      Annul bad status
        IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL(STATUS)

*      Get axis choice from user
        CALL AXIS_GET(ILOC, 'AMP', 'ENAXIS', 3, ENAX, STATUS)
        IF (STATUS .NE. SAI__OK) GOTO 999

      END IF

*    Attempt to map the energy axis. NB: this routine fills the array with
*    values 1-N if the axis isn't present in the input file
      CALL BDA_MAPAXVAL( ILOC, 'READ', ENAX, AXPTR, STATUS )

*    Create order array to show where energy axis is
      ORDER(3)=ENAX
      IF (ENAX.EQ.1) THEN
        ORDER(1) = 2
        ORDER(2) = 3
      ELSE IF (ENAX.EQ.2) THEN
        ORDER(1) = 1
        ORDER(2) = 3
      ELSE
        ORDER(1) = 1
        ORDER(2) = 2
      END IF

*    Set dimensions of output array
      ODIM(1) = DIMS(ORDER(1))
      ODIM(2) = DIMS(ORDER(2))

*    Create output array
      CALL BDA_CREDATA( OLOC, 2, ODIM, STATUS )
      CALL BDA_MAPDATA( OLOC, 'WRITE', ODPTR, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error creating and mapping output array')
        GOTO 999
      END IF

*    Create output quality
      IF ( QOK ) THEN
        CALL BDA_CREQUAL( OLOC, 2, ODIM, STATUS )
        CALL BDA_MAPQUAL( OLOC, 'WRITE', OQPTR, STATUS )
        CALL BDA_PUTMASK( OLOC, QUAL__MASK, STATUS )
      END IF

*    Zero output arrays
      CALL ARR_INIT1R( 0.0, ODIM(1)*ODIM(2), %VAL(ODPTR), STATUS )
      IF ( QOK ) THEN
        CALL ARR_INIT1B( QUAL__BAD, ODIM(1)*ODIM(2), %VAL(OQPTR),
     :                                                   STATUS )
      END IF

*    Map some work space
      CALL DYN_MAPR( 2, ODIM, TPTR, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error mapping temporary space')
        GOTO 999
      END IF

*    Zero temp. array
      CALL ARR_INIT1R( 0.0, ODIM(1)*ODIM(2), %VAL(TPTR), STATUS )

*    Calculate ouput array
      CALL ENMAP_CREATE( DIMS, DIMS(1), DIMS(2), DIMS(3), %VAL(IDPTR),
     :                   QOK, %VAL(IQPTR), IQMASK, ORDER, DIMS(ENAX),
     :                   %VAL(AXPTR), ODIM(1), ODIM(2), THRESH,
     :                   %VAL(TPTR), %VAL(ODPTR), %VAL(OQPTR), STATUS )

*    Create axis structure in output file and copy relevant axes from input file
      CALL BDA_CREAXES( OLOC, 2, STATUS )
      CALL BDA_COPAXIS( ILOC, OLOC, ORDER(1), 1, STATUS )
      CALL BDA_COPAXIS( ILOC, OLOC, ORDER(2), 2, STATUS )

*    Delete axis structure if there was an error creating it
      IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_ANNUL( STATUS )
        CALL DAT_ERASE( OLOC, 'AXIS', STATUS )
      ENDIF

*    Copy history from the original file
      CALL HIST_COPY( ILOC, OLOC, STATUS )

*    Add history record
      CALL HIST_ADD( OLOC, VERSION, STATUS )

*    Create text strings for history
      CALL USI_NAMEI( NLINES, PATH, STATUS)
      CALL HIST_PTXT( OLOC, NLINES, PATH, STATUS )

*    Write threshold value
      CALL MSG_MAKE( 'Threshold value ^THRESH', TEXT, TLEN )
      CALL HIST_PTXT( OLOC, 1, TEXT(:TLEN), STATUS )

*    Tidy up
 999  CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  ENMAP_CREATE - Fill output array
      SUBROUTINE ENMAP_CREATE( DIMS, DIM1, DIM2, DIM3, INDAT, QOK,
     :                         INQUAL, IQMASK, ORDER, NEN, ENCHAN,
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
      BYTE INQUAL(DIM1,DIM2,DIM3)
      BYTE                     IQMASK           ! Input quality mask
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
*    Functions :
*
      BYTE 			BIT_ANDUB
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
              IF ( BIT_ANDUB(INQUAL(LP1,LP2,LP3),IQMASK)
     :                    .EQ.QUAL__GOOD ) THEN

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
