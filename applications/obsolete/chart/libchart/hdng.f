      SUBROUTINE HDNG( STATUS )
*+
*   This routine Prints a Heading giving the Basic Field Details
*   i.e. Input RA & Dec and Equinox
*   RA & Dec on Equinox 1950.00
*   Epoch of Positions,Scale & Size of Field

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  History:
*     Sometime (UNK):
*        Original version.
*     9-DEC-1991: PMA
*       Changed calls to CTOR to CHR_CTOR
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONV call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to COORD call
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     11-MAR-1993 (AJJB):
*        Changed ISIGN1 and ISIGN2 (used as 4th argument in calls to
*        CONV) to type Character, as CONV has been changed.
*     22-MAR-1993 (AJJB):
*        Commented out declaration of local variable which is never
*        used.
*
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'MAIN'

*  Status:
      INTEGER STATUS             ! Global status

*     LOGICAL A06E
      CHARACTER ISIGN1, ISIGN2
      CHARACTER*70 PARAMS(25),VALUE*50

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Pick up Scale from Parameter File (MJV)
*
      CALL GETPARAMS (PARAMS,NPAR, STATUS )
      CALL GETDEFLT (PARAMS,NPAR,'SCALE',VALUE,NPOS, STATUS )
      IF (NPOS.EQ.0) THEN
         SCALE=0.0
      ELSE
         CALL CHR_CTOR(VALUE,SCALE,ISTAT)
         IF (ISTAT.NE.0) SCALE=0.0
      ENDIF

      WIDE = 2.0 * SIZE
      IF (ABS(SCALE).LT.1E-5) THEN
         WRITE(7,949) IDFLD,WIDE
      ELSE
         WRITE(7,949) IDFLD,WIDE,SCALE
      ENDIF
 949  FORMAT('1'/,' ','Field ',A10,'    Search Width = ',F6.2
     : ,' Degrees     Scale =',F7.2,' Secs/mm. ')
      CALL CONV(2,A,3,ISIGN1,IHA,IMA,ISA,SECA, STATUS )
      CALL CONV(1,D,2,ISIGN2,IDD,IMD,ISD,SECD, STATUS )
      WRITE (7,919) IHA,IMA,SECA,ISIGN2,IDD,IMD,SECD,EQUIN
 919  FORMAT('0','Input Field Centre ',2I3,F7.3,4X,A1,2I3,F6.2,
     : '  Equinox',F8.2)
      IF (EQUIN.EQ.1950.00) GO TO 101
      CALL CONV(2,AP,3,ISIGN1,IHA,IMA,ISA,SECA, STATUS )
      CALL CONV(1,DP,2,ISIGN2,IDD,IMD,ISD,SECD, STATUS )
      IF(EQUOUT.NE.1950.0)WRITE (7,929) IHA,IMA,SECA,ISIGN2,IDD,IMD,SECD
 929  FORMAT(' ',5X,'Equivalent to ',2I3,F7.3,4X,A1,2I3,F6.2,
     : '  Equinox 1950.00')
101   CONTINUE
      IF (EQUOUT.EQ.EQUIN) GOTO 103
      CALL CONV(2,AO,3,ISIGN1,IHA,IMA,ISA,SECA, STATUS )
      CALL CONV(1,DO,2,ISIGN2,IDD,IMD,ISD,SECD, STATUS )
      WRITE (7,925) IHA,IMA,SECA,ISIGN2,IDD,IMD,SECD,EQUOUT
 925  FORMAT(' ',5X,'Equivalent to ',2I3,F7.3,4X,A1,2I3,F6.2,
     : '  Equinox',F8.2)
*
*   Also Galactic & Ecliptic Coords
*
103   CONTINUE
      CALL COORD(AP,DP,AO,DO,EQUOUT,GLAT,GLONG,ELAT,ELONG, STATUS )
      WRITE (7,932) GLAT,GLONG,EQUOUT,ELAT,ELONG
932   FORMAT(//,6X,'Galactic Coordinates:',12X,'b  = ',F6.2,6X,
     : 'l  = ',F6.2/6X,'Ecliptic Coordinates (',F7.2,'):',
     : 'Lat. = ',F6.2,3X,'Long. = ',F6.2/)
      WRITE(7,930) EQUOUT,EPOCH
930   FORMAT('0','Output positions for Equinox',F8.2,4X,'Epoch',F8.2)
*
*   Print out the supplied object postions (if any)
*
      IF (SUPP) THEN
         WRITE (7,934) EQUOUT
934      FORMAT(//7X,31('*'),/7X,'*  Supplied Object Positions  *',
     :   /7X,31('*')//3X,'No.',8X,'R.A.',11X,'Dec'/21X,'(',F6.1,')')
         DO K=1,NUMSUPP
            CALL CONV(2,OWNOBJ(1,K),3,ISIGN1,IHA,IMA,ISA,SECA, STATUS )
            CALL CONV(1,OWNOBJ(2,K),2,ISIGN2,IDD,IMD,ISD,SECD, STATUS )
            WRITE (7,936) K,IHA,IMA,SECA,ISIGN2,IDD,IMD,SECD
936         FORMAT(2X,I3,3X,2I3,F7.3,3X,A1,I2,I3,F6.2)
         ENDDO
         WRITE (7,938)
938      FORMAT(//' (These Objects Plotted With Negative Numbers'
     :   ,' On The Chart')
      ENDIF
*
*   And Print Survey Plate No. and Approx.
*   Co-ords of Centre on that Plate
*
      CALL PALP(AP,DP, STATUS )
      END
