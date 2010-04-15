      SUBROUTINE STARP(J,NCAT,RMAG,RAO,DECO, STATUS )
*+
*   This subroutine is used to format and print the
*   Number, magnitude, RA and DEC of a star.
*
*   Later the guide star information is added.
*
*   Gets
*   ----
*      J    - The Number of the Star in the Output List
*      NCAT - Number of the Star in the ASTROMETCAT Catalogue
*      RMAG - Magnitude of the Star
*      RAO  - RA of the Star (in Double Precision Radians)
*      DECO - Dec of the Star (in Double Precision Radians)
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   This is a minor variation on the ICL 1903T subroutine
*   adapted by K F Hartley at RGO on 2-2-83

*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONV calls
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*     12-MAR-1993 (AJJB):
*        Changed I and JSIGN (used as 4th argument in calls to
*        CONV) to type Character, as CONV has been changed.

*
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER ISIGN, MINUS, IPLUS, ISPACE, I, JSIGN
      DOUBLE PRECISION RAO,DECO
      DATA MINUS,IPLUS,ISPACE/'-','+',' '/

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Convert the precessed positions
*
      CALL CONV(2,RAO,0,I,MHAO,MINSAO,M,SECSAO, STATUS )
      CALL CONV(1,DECO,0,JSIGN,MDEGD,MINSD,M,SECSD, STATUS )
*
*   Different action depending on whether it is an SAO or AGK3 number
*
      IF (NCAT.LT.2000000) THEN
*
*   It is an AGK3 number
*
*   So separate band number and number within band
*
         NCAT=NCAT-1000000
         NS=MOD(IABS(NCAT),10000)
         NBAND=NCAT/10000
C
C      Modified by K.F.Hartley at RGO on 29-4-85
C      to handle the correct sign of the star number near the
C      equator. This version is consistent with TTYOUT.
C
         IF ( NCAT.LT.0) THEN
            ISIGN=MINUS
         ELSE
            ISIGN=IPLUS
         END IF
C
C     End of modification.
C
         WRITE (7,900) J,ISIGN,NBAND,NS,RMAG,MHAO,MINSAO,SECSAO,JSIGN,
     :                 MDEGD,MINSD,SECSD
  900    FORMAT (' ',I3,' AGK3 ',1X,A1,I2,1X,I4,F6.1,I5,I3,F5.1,2X,
     :           A1,I2,I3,F5.1)
*
*   End of AGK3 option
*
      ELSE
*
*   It is an SAO star
*
         NCAT=NCAT-2000000
         WRITE (7,910) J,NCAT,RMAG,MHAO,MINSAO,SECSAO,JSIGN,MDEGD,
     :                 MINSD,SECSD

  910    FORMAT (' ',I3,' SAO ',I7,2X,F7.1,I5,I3,F5.1,2X,A1,I2,I3,F5.1)
*
*   End of SAO option
*
      END IF
      END
