*+  AXIS_TGETORD - Return axis transformation array given requested order
      SUBROUTINE AXIS_TGETORD( FID, AXORD, NEED_MOVES, MAXOR,
     :                                 NDIM, MDIMS, STATUS )
*
*    Description :
*
*     The AXORD string is broken up into tokens which are used to identify
*     an axis in the input dataset - these may be standard tokens such as
*     X,Y,T or E, or numeric axis numbers. The number of the axis is stored
*     in the MAXOR array. Unused axes are then added, and the MDIMS found and
*     padded to 7D.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     29 Apr 91 : Original (DJA)
*     30 Nov 92 : Supports numeric axis tokens (DJA)
*     28 Mar 95 : ADI version (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER			FID			! Input dataset
      CHARACTER*(*)             AXORD                	! Axis order string
*
*    Export :
*
      LOGICAL                   NEED_MOVES           	! Axes needing moved?
      INTEGER                   MAXOR(ADI__MXDIM)    	! New axis order
      INTEGER                   NDIM                 	! Dimensionality
      INTEGER                   MDIMS(ADI__MXDIM)    	! Dimensions
*
*    Status :
*
      INTEGER 			STATUS
*
*    Externals:
*
      EXTERNAL			CHR_ISDIG
        LOGICAL			CHR_ISDIG
*
*    Local variables :
*
      INTEGER                         IAX                  ! Loop over axes
      INTEGER                         IDIMS(ADI__MXDIM)    ! Input dimensions
      INTEGER                         MIAX                 ! Moved axis index

      LOGICAL                         DOMOVES              ! Move axes?
      LOGICAL                         USED(ADI__MXDIM)     ! Dimension used?
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*    Get input dimensions
        CALL BDI_GETSHP( FID, ADI__MXDIM, IDIMS, NDIM, STATUS )

*    Abort if bad, otherwise...
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Set axes unused
          DO IAX = 1, NDIM
            USED(IAX) = .FALSE.
          END DO

*      Locate each axis whose order is specified
          IF ( DOMOVES ) THEN
            IAX = 1
            DO IAX = 1, LEN(AXORD)

*          Try to locate next axis quantity
              IF ( CHR_ISDIG( AXORD(IAX:IAX) ) ) THEN
                CALL CHR_CTOI( AXORD(IAX:IAX), MIAX, STATUS )
              ELSE
                CALL BDI0_FNDAXC( FID, AXORD(IAX:IAX), MIAX, STATUS )
              END IF
              IF ( STATUS .NE. SAI__OK ) GOTO 99

*          Store axis no
              MAXOR(IAX) = MIAX
              MDIMS(IAX) = IDIMS(MIAX)
              USED(MIAX) = .TRUE.

            END DO

*        Any dimensions not covered by specification
            IF ( IAX .LT. NDIM ) THEN
              MIAX = IAX + 1
              DO IAX = 1, NDIM
                IF ( .NOT. USED(IAX) ) THEN
                  MAXOR(MIAX) = IAX
                  MDIMS(MIAX) = IDIMS(IAX)
                  MIAX = MIAX + 1
                END IF
              END DO
            END IF

          ELSE
            DO IAX = 1, NDIM
              MAXOR(IAX) = IAX
              MDIMS(IAX) = IDIMS(IAX)
            END DO

          END IF

*      Pad output dimensions to 7D
          IF ( NDIM .LT. ADI__MXDIM ) THEN
            DO IAX = NDIM + 1, ADI__MXDIM
              MAXOR(IAX) = IAX
              MDIMS(IAX) = 1
            END DO
          END IF

*      Any axis moves required?
          NEED_MOVES = .FALSE.
          IF ( DOMOVES ) THEN
            DO IAX = 1, NDIM
              NEED_MOVES = ( NEED_MOVES .OR. ( MAXOR(IAX) .NE. IAX ) )
            END DO
          END IF

        END IF

*    Report error
  99    IF ( STATUS .NE. SAI__OK ) THEN
          CALL AST_REXIT( 'AXIS_TGETORD', STATUS )
        END IF

      END IF

      END
