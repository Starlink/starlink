      SUBROUTINE PRCOL(IP, STATUS )
*+
*   This subroutine prints suitable column headings
*   as required by the RGO guide star subroutine GSOUT
*   This is a slight variation on the subroutine PAGE
*   from the ICL 1903T version of Chart.
*
*   History:

*   It was modified by K F Hartley at RGO on 2-2-83
*
*   Modified by R W Argyle at RGO on 1-2-84 to replace
*   26 inch by LPO 1 metre camera
*
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*
*
*   It throws a page except for the first page.
*
*   Gets
*   ----
*      IP  - Indicator as to which page number.
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (IP.GT.0) THEN
         WRITE (7,900)
  900    FORMAT ('1')
      END IF
      WRITE (7,910)
  910 FORMAT (/' ',58X,'OFFSETS',/,' ',57X,'FROM CENTRE',/,
     :       ' ',8X,'STAR',9X,'MAG.',5X,'R.A.',8X,'DEC.',12X,
     :        'X',5X,'Y')
      WRITE (7,920)
  920 FORMAT (' ' ,57X,'"',7X,'"')
      WRITE (7,930)
  930 FORMAT ('+',72X,'1 METRE',16X,'26"',12X,'13"',/
     :        ' ',72X,'CAMERA',14X,'MERZ GUIDER',
     :        5X,'GUIDER'/,
     :        73X,'X',5X,'Y',1X,'ORIENTATION',2X,'X',6X,'Y',7X,
     :        'X',6X,'Y')
      END
