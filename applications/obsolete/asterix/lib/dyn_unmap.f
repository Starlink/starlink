*+  DYN_UNMAP - unmaps dynamic memory
      SUBROUTINE DYN_UNMAP(PTR)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*     16 Jun 92 : Uses PSX call for memory deallocation (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER PTR			! pointer to dynamic memory
*    Import/Export :
*    Export :
*    External references :
*    Global variables :
*    Local Constants :
      INTEGER NULL
      PARAMETER (NULL=0)
*    Local variables :
      INTEGER NPAGE			! number of pages
      LOGICAL FOUND			! whether pointer in list
      LOGICAL SECTION
      CHARACTER*(DAT__SZLOC) LOC
*-
      FOUND=.FALSE.
      SECTION=.FALSE.
      NPAGE=0
      CALL DYN_UNMAP_REM(PTR,NPAGE,FOUND,SECTION,LOC)
      IF (FOUND) THEN
        IF (SECTION) THEN
          STATUS=SAI__OK
          CALL HDS_ERASE(LOC,STATUS)
        ELSE
          CALL PSX_FREE( PTR, STATUS )
        ENDIF
      ENDIF
      END


*+
      SUBROUTINE DYN_UNMAP_REM(PTR,NPAGE,FOUND,SECTION,LOC)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
*    Import :
      INTEGER PTR			! pointer to dynamic memory
*    Import/Export :
*    Export :
      INTEGER NPAGE			! number of pages
      LOGICAL FOUND			! whether pointer in list
      LOGICAL SECTION			! whether disk section
      CHARACTER*(DAT__SZLOC) LOC	! locator to disk section
*    External references :
*    Global variables :
      INCLUDE 'ASTLIB(DYN_CMN)'
*    Local Constants :
*    Local variables :
      INTEGER IPTR,JPTR
*-

      FOUND=.FALSE.
      SECTION=.FALSE.
      IPTR=0

*  find pointer in list
      DO WHILE (.NOT.FOUND.AND.IPTR.LT.NPTR)
        IPTR=IPTR+1
        FOUND=(PTR.EQ.LIST(IPTR).PTR)
      ENDDO

      IF (FOUND) THEN
*  return number of pages to release
        NPAGE=LIST(IPTR).NPAGE
        IF (LIST(IPTR).SECTION) THEN
          SECTION=.TRUE.
          LOC=LIST(IPTR).LOC
        ENDIF

*  remove from list
        DO JPTR=IPTR,NPTR-1
          LIST(JPTR) = LIST(JPTR+1)
        ENDDO
        LIST(NPTR).PTR=0
        LIST(NPTR).NITEM=0
        LIST(NPTR).NPAGE=0
        LIST(NPTR).NBYTE=0
        LIST(NPTR).LOC=' '
        LIST(NPTR).SECTION=.FALSE.
        NPTR=NPTR-1
      ENDIF
      END
