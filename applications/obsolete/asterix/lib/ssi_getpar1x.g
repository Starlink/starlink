*+  SSI_GETPAR1<T> - Read a vector parameter of type <COMM>[]
      SUBROUTINE SSI_GETPAR1<T>( ID, FILE, PAR, MAXVAL, VALUE,
     :                                         ACTVAL, STATUS )
*    Description :
*
*     Read a vector parameter from an SSDS.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Mar 92 : Original (DJA)
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
*    Import :
*
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
      INTEGER                      FILE          ! BOOK structure slot
      CHARACTER*(*)                PAR           ! Parameter to read
      INTEGER                      MAXVAL        ! Maximum number of values
*
*    Export :
*
      <TYPE>                       VALUE(*)      ! Field item values
      INTEGER                      ACTVAL        ! Actual number of values
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER ID
*-

      IF(STATUS.EQ.SAI__OK) THEN
        CALL ADI1_GETLOC(LOC,ID,STATUS)
        CALL SSO_GETPAR1<T>( ID, FILE, PAR, MAXVAL, VALUE,
     :                                         ACTVAL, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_GETPAR1<T>', STATUS )
        ENDIF
      ENDIF
      END
