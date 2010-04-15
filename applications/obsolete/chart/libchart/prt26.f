      SUBROUTINE PRT26( STATUS )
*+
*   This subroutine prints a heading relevant to the RGO
*   26 and 13 inch refractors.
*
*   It was adapted from the ICL 1903T version of Chart.
*   by K F Hartley at RGo on 2-1-83

*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONV calls
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     12-MAR-1993 (AJJB):
*        Changed I, ISIGN1, ISIGN2 and ISIGN3 (used as 4th argument in
*        calls to CONV) to be of type Character as CONV has been
*        changed.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER I, ISIGN1, ISIGN2, ISIGN3   ! Holds sign from CONV call
      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      REAL EQ(3)
      DOUBLE PRECISION AJ,DJ,AJE,DJE,AJW,DJW

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   First write a heading
*
      WRITE (7,900)
  900 FORMAT(///,44X,'*********************************',
     :         /,44X,'* 26" & 13" OFFSET CO-ORDINATES *',
     :         /,44X,'*********************************')
      WRITE (7,910)
  910 FORMAT(///,' ',22X,'TRUE POSITION',33X,'26 INCH EAST',19X,
     :       '26 INCH WEST',/,' ',8X,'EQUINOX',7X,'RA',9X,'DEC',34X,
     :       'RA',8X,'DEC',19X,'RA',8X,'DEC')
*
*   Now calculate corrected field centres for 26" E/W orientations
*   and for three equinoxes
*
      EQ(1)=AINT((EQUOUT-2.5)/5.0) * 5.0
      EQ(2)=EQUOUT
      EQ(3)=AINT((EQUOUT+7.5)/5.0) * 5.0
*
*   Now print the field centres for three dates
*   past and future (rounded to 5-year dates)
*   and output equinox
*
      DO K=1,3
         CALL PRECES(AP,DP,AJ,DJ,1950.0,EQ(K), STATUS )
         CALL CONV(2,AJ,0,I,MHAJ,MINAJ,MSAJ,X, STATUS )
         CALL CONV(1,DJ,0,ISIGN1,MHDJ,MINDJ,MSDJ,X, STATUS )
*
*   Now apply the telescope correction
*
         CALL TELCOR(AJ,DJ,AJE,DJE,AJW,DJW, STATUS )
*
*   and convert back into hms,dms
*
         CALL CONV(2,AJE,0,I,MHAJE,MINAJE,MSAJE,X, STATUS )
         CALL CONV(1,DJE,0,ISIGN2,MHDJE,MINDJE,M,X, STATUS )
         CALL CONV(2,AJW,0,I,MHAJW,MINAJW,MSAJW,X, STATUS )
         CALL CONV(1,DJW,0,ISIGN3,MHDJW,MINDJW,M,X, STATUS )
*
*   Now do the Write
*
         WRITE (7,920) EQ(K),MHAJ,MINAJ,MSAJ,ISIGN1,MHDJ,MINDJ,MSDJ,
     :                 MHAJE,MINAJE,MSAJE,ISIGN2,MHDJE,MINDJE,
     :                 MHAJW,MINAJW,MSAJW,ISIGN3,MHDJW,MINDJW
  920    FORMAT (' ',8X,'(',F6.1,')',2X,3I3,3X,A1,I2,2I3,27X,
     :           3I3,3X,A1,I2,I3,14X,3I3,3X,A1,I2,I3)
*
*   That ends the loop.
*
      END DO
      WRITE (7,'(///)')
      END
