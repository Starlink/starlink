*+  PSS_BGND_CLOSE - Close bgnd files down
      SUBROUTINE PSS_BGND_CLOSE( LAST, STATUS )
*
*    Description :
*
*     Reads the prime input dataset supplied to PSS by the user.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     05 Jul 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      LOGICAL                  LAST                    ! Last input file?
*-

*    New error context
      CALL ERR_BEGIN( STATUS )

*    A dynamic array?
      IF ( BG_DYNAMIC ) THEN

*      Release workspace
        CALL DYN_UNMAP( BG_DATA_PTR, STATUS )

      ELSE

*      Unmap everything
        CALL BDI_UNMAP( BG_ID, STATUS )

*      Free BDI resources
        CALL BDI_RELEASE( BG_ID, STATUS )

      END IF

*    Free log(bgnd) array
      IF ( CP_CASH .AND. (CP_MODE.NE.PSS__M_SENMAP) ) THEN
        CALL DYN_UNMAP( BDS_LBGND_PTR, STATUS )
      END IF

*    Close the file
      IF ( BG_DYNAMIC ) THEN
        IF ( .NOT. CP_MULTI ) THEN
          CALL USI_TANNUL( BG_ID, STATUS )
        END IF
      ELSE
        IF ( CP_MULTI ) THEN
          CALL ADI_FCLOSE( BG_ID, STATUS )
        ELSE
          CALL USI_TANNUL( BG_ID, STATUS )
        END IF
      END IF

*    Restore error context
      CALL ERR_END( STATUS )

      END
*+  PSS_BGND_LOAD - Get information about background model
      SUBROUTINE PSS_BGND_LOAD( STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     18 Sep 90 : Original (DJA)
*     14 Sep 92 : Only processes area defined by BDS_EXTREMA (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  I                       ! Loop over dimensions
      INTEGER                  DIMS(ADI__MXDIM)        ! Image dimensions
      INTEGER                  NDIM
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    First the data array
      CALL BDI_CHKDATA( BG_ID, BG_OK, NDIM, DIMS, STATUS )
      IF ( .NOT. BG_OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid background data', STATUS )

      ELSE

*      Check it matches the input image
        DO I = 1, NDIM
          IF ( BDS_DIMS(I) .NE. DIMS(I) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Image and background datasets'/
     :                       /' have different sizes', STATUS )
            GOTO 99
          END IF
        END DO

*      Map the data
        CALL BDI_MAPDATA( BG_ID, 'READ', BG_DATA_PTR, STATUS )

*      Map array for background variance & divide by data variance squared
        IF ( .NOT. CP_CASH ) THEN

          CALL DYN_MAPR( NDIM, BDS_DIMS, BG_VAR_PTR, STATUS )
          BG_VAR_OK = .TRUE.
          BG_VAR_DYNAMIC = .TRUE.
          CALL PSS_BGND_PROC( BDS_DIMS(1), BDS_DIMS(2),
     :                        %VAL(IM_VAR_PTR),
     :                        %VAL(BG_DATA_PTR),
     :                        %VAL(BG_VAR_PTR), STATUS )

        END IF

      END IF

*    Report errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_BGND_LOAD', STATUS )
      END IF

      END
*+  PSS_BGND_LOG - Take Log of bgnd array
      SUBROUTINE PSS_BGND_LOG( NX, NY, BGND, LBGND, STATUS )
*
*    Description :
*
*     Set value in bgnd array
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Feb 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  NX, NY                  ! Array dimensions
      REAL                     BGND(NX,NY)             ! Bgnd array
*
*    Export :
*
      REAL                     LBGND(NX,NY)            ! Log(bgnd) array
*
*    Local variables :
*
      INTEGER                  I,J                     ! Loops over array
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO J = BDS_EXTREMA(1,2), BDS_EXTREMA(2,2)
        DO I = BDS_EXTREMA(1,1), BDS_EXTREMA(2,1)
          IF ( BGND(I,J) .GT. 0.0 ) THEN
            LBGND(I,J) = LOG(BGND(I,J))
          ELSE
            LBGND(I,J) = -10.0
          END IF
        END DO
      END DO

      END
*+  PSS_BGND_PROC - Perform background processing
      SUBROUTINE PSS_BGND_PROC( NX, NY, IMBV, BGND, BGNV, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Apr 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NX, NY                  ! Number of data values
      REAL                     IMBV(NX,NY)             ! B'subed variance
      REAL                     BGND(NX,NY)             ! Background data
*
*    Export :
*
      REAL                     BGNV(NX,NY)             ! Background variance
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                  I,J                     ! Array loops
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Signif data
      DO J = BDS_EXTREMA(1,2), BDS_EXTREMA(2,2)
        DO I = BDS_EXTREMA(1,1), BDS_EXTREMA(2,1)
          IF ( IMBV(I,J) .LE. 1.0E-8 ) THEN
            BGNV(I,J) = 0.1
          ELSE
            BGNV(I,J) = MAX(0.1,BGND(I,J)/(IMBV(I,J)**2))
          END IF
        END DO
      END DO

      END
*+  PSS_BGND_SET - Set value in bgnd array
      SUBROUTINE PSS_BGND_SET( NX, NY, VAL, BGND, STATUS )
*
*    Description :
*
*     Set value in bgnd array
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Feb 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  NX, NY                  ! Array dimensions
      REAL                     VAL                     ! Bgnd array value
*
*    Export :
*
      REAL                     BGND(NX,NY)             ! Log(bgnd) array
*
*    Local variables :
*
      INTEGER                  I,J                     ! Loops over array
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO J = BDS_EXTREMA(1,2), BDS_EXTREMA(2,2)
        DO I = BDS_EXTREMA(1,1), BDS_EXTREMA(2,1)
          BGND(I,J) = VAL
        END DO
      END DO

      END
