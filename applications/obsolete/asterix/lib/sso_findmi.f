*+  SSO_FINDMI - Introduce a mapped item
      SUBROUTINE SSO_FINDMI( LOC, FLD, TYPE, ADDIFNEW, N, STATUS )
*
*    Description :
*
*     Look up table of existing datasets. If present, return the entry
*     number. If not present, add to table if the ADDIFNEW flag is true.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jul 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Global variables :
*
      INCLUDE 'SSO_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)     LOC                ! The dataset
      CHARACTER*(*)              FLD                ! The field name
      INTEGER                    TYPE               ! The type of the slot
      LOGICAL                    ADDIFNEW           ! Add if not found?
*
*    Export :
*
      INTEGER                    N                  ! The mapped item slot
*
*    Local variables :
*
      INTEGER                    I                  ! Loop over SSO mapped items
      INTEGER                    NDS                ! Dataset id
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*      Find slot for dataset
        CALL SSO_FINDDS( LOC, .FALSE., NDS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Search for the mapped item. The dataset, the field name and TYPE
*      must all match, and the slot must be in use.
        N = 0
        I = 1
        DO WHILE ( ( I .LE. SSO__MXMI ) .AND. ( N .EQ. 0 ) )
          IF ( ( NDS .EQ. SSO.MI(I).DS ) .AND.
     :         ( FLD .EQ. SSO.MI(I).FLD ) .AND.
     :         ( TYPE .EQ. SSO.MI(I).TYPE ) .AND.
     :         SSO.MI(I).USED ) THEN
            N = I
          ELSE
            I = I + 1
          END IF
        END DO

*      Add to table if not present?
        IF ( ( N .EQ. 0 ) .AND. ADDIFNEW ) THEN

*        Search for unused slot
          I = 1
          DO WHILE ( ( I .LE. SSO__MXMI ) .AND. ( N .EQ. 0 ) )
            IF ( .NOT. SSO.MI(I).USED ) THEN
              N = I
            ELSE
              I = I + 1
            END IF
          END DO

*        No more slots?
          IF ( N .EQ. 0 ) THEN
            CALL MSG_PRNT( '! Maximum number of mapped items exceeded' )
            STATUS = SAI__ERROR

          ELSE

*          Use slot
            SSO.MI(N).USED = .TRUE.
            SSO.MI(N).DS = NDS
            SSO.MI(N).TYPE = TYPE
            SSO.MI(N).FLD = FLD
            SSO.MI(N).PTR = 0
            SSO.MI(N).MAPPED = .FALSE.

          END IF

        ELSE IF ( N .EQ. 0 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Mapped item not found', STATUS )

        END IF

*      Tidy up
 99     IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_FINDMI', STATUS )
        END IF

      END IF

      END
