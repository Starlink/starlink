      SUBROUTINE PAGE(IP,CATRUN,NONS,IDENTS,EQUOUT, STATUS )

*+
*
*   PAGE Writes out the Headings for the Star List
*   Columns. It throws to a New Page if its' not the First
*
*   Gets
*   ----
*       IP     - PAGE NUMBER
*       CATRUN - AFFECTS HEADING FORMAT
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      LOGICAL CATRUN,IDENTS,NONS
      CHARACTER*20 ID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (IP.GE.1) WRITE (7,999) IP
999   FORMAT('1',100X,'PAGE ',I3)
      IF (CATRUN) THEN
        WRITE (7,990)
990     FORMAT(/' ',71X,2(' Offsets',10X),/,' ',70X,'From centre',
     :   7X,'From Centre',6X,'Proper Motions',/,
     :   8X,'Star',12X,'Mag.',1X,'Spec',10X,
     :   'R.A.',10X,'Dec.',10X,'X',8X,'Y',9X,'X',7X,'Y',
     :   7X,'R.A.',6X,'Dec.')
        WRITE(7,992) EQUOUT
992     FORMAT(' ',47X,'(',F7.2,')',14X,'"',8X,'"',8X,'mm.',5X,
     :   'mm.',6x,'s/yr',6X,'"/yr')
      ELSE IF (.NOT.NONS) THEN
        IF (IDENTS) THEN
          ID ='    Identifications'
        ELSE
          ID = ' '
        ENDIF
        WRITE (7,994) EQUOUT,ID
994     FORMAT(//8X,'HD',10X,'DM',7X,'Spec   V      B  ',7X,'R.A.',
     :  7X,'Dec.',8X,'Offsets'/35X,'Mag   Mag',11X,'(',F7.2,')',11X,
     :  'From Centre'/76X,'"',7X,'"',3X,A20)
      ELSE
        WRITE (7,996) EQUOUT
996   FORMAT(//'  No.  Name of Object',4X,'R.A.',8X,'Dec.',4X,
     : 'Mag',3X,'Diam',4X,'Description',15X,'Offsets',/,28X,'(',F6.1,
     : ')',14X,'(Secs)',28X,'From Centre'/85X,'"',6X,'"')
      ENDIF
      END

