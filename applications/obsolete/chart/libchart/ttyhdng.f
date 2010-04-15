      SUBROUTINE TTYHDNG( RESPONSE, STATUS )
*+
*  Name:
*     TTYHDNG

*  Purpose:
*   This Routine Prints a Heading giving the Basic Field Details
*   i.e. Input RA & Dec and Equinox
*   Epoch of positions, Scale & Size of Field

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TTYHDNG( RESPONSE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     RESPONSE = LOGICAL (Returned)
*        Logical value obtained from call to TTYHOLD. Comes back set
*        to .FALSE. is user responded 'NO'. If that is the case, causes
*        this routine to immediately return execution to calling
*        routine.
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     ANO: Someone (Somewhere)
*     {enter_new_authors_here}

*  History:
*     {date} ({author_identifier}):
*        Original version.
*     23-FEB-1993 (AJJB):
*        Conversion to ADAM and proper commenting
*     1-MAR-1993 (AJJB):
*        STATUS arg added to TTYPALP call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to COORD call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     5-MAR-1993 (AJJB):
*        RESPONSE argument added, which returns execution to calling
*        routine if user enters 'NO' during TTYHOLD call.
*     12-MAR-1993 (AJJB):
*        Changed ISIGN1 and ISIGN2 (used as 4th argument in calls to
*        CONV) to type Character, as CONV has been changed.
*     22-MAR-1993 (AJJB):
*        Commented out declarations of local variables which are never
*        used.
*     28-APR-1993 (AJJB):
*        Set RESPONSE, the argument for TTYHDNG and TTYOUT, to .TRUE. at
*        the beginning of the routine, as TTYHDNG doesn't alter it's
*        value if there are no user supplied objects to output, so that
*        this routine does not exit because it thinks it's getting a
*        false RESPONSE value back from TTYHDNG (which usually means the
*        user doesn't want any more output).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'MAIN'             ! CHART control common blocks
*
*  Globals used from MAIN.FOR:
*
*        NUMSUPP = INTEGER (Read)
*           NUMBER OF USER SUPPLIED OBJECTS
*        IDFLD = CHARACTER * ( 10 ) (Read)
*           RUN IDENTIFIER
*        SCALE = REAL (Read)
*           PLOTTING SCALE IN ARC. SECS./MM.
*        SIZE = REAL (Read)
*           HALF-WIDTH OF FIELD IN DEGREES
*        EQUIN = REAL (Read)
*           EQUINOX OF INPUT FIELD CENTRE POSITIONS
*        EQUOUT = REAL (Read and Write)
*           REQUIRED EQUINOX OF FIELD STAR POSITIONS
*        EPOCH = REAL (Read)
*           REQUIRED EPOCH OF FIELD STAR POSITIONS IF 'CATRUN' TRUE
*        A = DOUBLE PRECISION (Read and Write)
*           INPUT FIELD CENTRE RA
*        D = DOUBLE PRECISION (Read and Write)
*           INPUT FIELD CENTRE DEC
*        AO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE RA AT EQUINOX 'EQUOUT'
*        DO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE DEC AT EQUINOX 'EQUOUT'
*        AP = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE RA AT EQUINOX 1950.0
*        DP = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE DEC AT EQUINOX 1950.0
*        OWNOBJ( 2, 1000 ) = DOUBLE PRECISION (Read and Write)
*           ARRAY OF USER SUPPLIED OBJECT POSNS
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*        {descriptions_of_global_variables_referenced}...

*  Status:
      INTEGER STATUS             ! Global status

*  Argument returned:
      LOGICAL RESPONSE

*  Local Variables:
*     LOGICAL A06E
      INTEGER IHA, IMA, ISA, IDD, IMD, ISD, K
      REAL WIDE, SECA, SECD, GLAT, GLONG, ELAT, ELONG
      CHARACTER*80 TEXT
      CHARACTER ISIGN1, ISIGN2


*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      RESPONSE = .TRUE.  ! Default value

      CALL MSG_OUT(' ', ' ', STATUS)
      WIDE = 2.0 * SIZE
      IF (ABS(SCALE).LT.1E-5) THEN
         WRITE(TEXT,949) IDFLD,WIDE
      ELSE
         WRITE(TEXT,949) IDFLD,WIDE,SCALE
      ENDIF
      CALL MSG_OUT( ' ', TEXT, STATUS)
 949  FORMAT(' ','Field ',A10,'    Field Width = ',F6.2,
     : ' Degrees':
     : '     Scale =',F7.2,' Secs/mm. ')
      CALL CONV(2,A,3,ISIGN1,IHA,IMA,ISA,SECA, STATUS )
      CALL CONV(1,D,2,ISIGN2,IDD,IMD,ISD,SECD, STATUS )
      WRITE (TEXT,919) IHA,IMA,SECA,ISIGN2,IDD,IMD,SECD,EQUIN
 919  FORMAT('0','Input Field Centre ',2I3,F7.3,4X,A1,2I3,F6.2,
     : '  Equinox',F8.2)
      CALL MSG_OUT( ' ', TEXT, STATUS)
      IF (EQUIN.EQ.1950.00) GO TO 101
      CALL CONV(2,AP,3,ISIGN1,IHA,IMA,ISA,SECA, STATUS )
      CALL CONV(1,DP,2,ISIGN2,IDD,IMD,ISD,SECD, STATUS )
      IF(EQUOUT.NE.1950.0) THEN
         WRITE (TEXT,929) IHA,IMA,SECA,ISIGN2,IDD,IMD,SECD
         CALL MSG_OUT( ' ', TEXT, STATUS )
      END IF
 929  FORMAT(' ',5X,'Equivalent to ',2I3,F7.3,4X,A1,2I3,F6.2,
     : '  Equinox 1950.00')
101   IF (EQUOUT.EQ.EQUIN) GOTO 103
      CALL CONV(2,AO,3,ISIGN1,IHA,IMA,ISA,SECA, STATUS )
      CALL CONV(1,DO,2,ISIGN2,IDD,IMD,ISD,SECD, STATUS )
      WRITE (TEXT,925) IHA,IMA,SECA,ISIGN2,IDD,IMD,SECD,EQUOUT
      CALL MSG_OUT( ' ', TEXT, STATUS )
 925  FORMAT(' ',5X,'Equivalent to ',2I3,F7.3,4X,A1,2I3,F6.2
     :,'  Equinox',F8.2)
*
*   Also Galactic & Ecliptic Coords *
103   CALL COORD(AP,DP,AO,DO,EQUOUT,GLAT,GLONG,ELAT,ELONG, STATUS )
      WRITE (TEXT,932) GLAT,GLONG
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE (TEXT,933) EQUOUT,ELAT,ELONG
      CALL MSG_OUT( ' ', TEXT, STATUS )
932   FORMAT(6X,'Galactic Coordinates:',12X,'b  = ',F6.2,6X,
     : 'l  = ',F6.2)
933   FORMAT (6X,'Ecliptic Coordinates (',F7.2,'):',
     : 'Lat. = ',F6.2,3X,'Long. = ',F6.2)
      WRITE(TEXT,930) EQUOUT,EPOCH
      CALL MSG_OUT( ' ', TEXT, STATUS )
930   FORMAT(' ',5X,'Output positions for Equinox',F8.2,4X,
     :       'Epoch',F8.2)
*
*   Print out the supplied object positions (if any)
*
      IF (SUPP) THEN
         CALL MSG_OUT(' ', ' ', STATUS )
         CALL TTYHOLD( RESPONSE, STATUS )
         IF ( .NOT. RESPONSE) GOTO 500    ! Exit routine if NO
         CALL MSG_OUT( ' ', ' ', STATUS )
         WRITE (TEXT,934) EQUOUT
         CALL MSG_OUT( ' ', '*** Supplied Objects ****', STATUS )
         CALL MSG_OUT( ' ', TEXT, STATUS )
934      FORMAT(3X,'No.',8X,'R.A.',11X,'Dec',21X,'(',F6.1,')')
         DO K=1,NUMSUPP
            CALL CONV(2,OWNOBJ(1,K),3,ISIGN1,IHA,IMA,ISA,SECA, STATUS )
            CALL CONV(1,OWNOBJ(2,K),2,ISIGN2,IDD,IMD,ISD,SECD, STATUS )
            WRITE (TEXT,936) K,IHA,IMA,SECA,ISIGN2,IDD,IMD,SECD
936         FORMAT(2X,I3,3X,2I3,F7.3,3X,A1,I2,I3,F6.2)
            CALL MSG_OUT(' ', TEXT, STATUS)
         ENDDO
         CALL MSG_OUT(' ', ' ', STATUS)
         TEXT='They will be plotted on the Chart with negative numbers'
         CALL MSG_OUT( ' ', TEXT, STATUS )
      ENDIF
C
C   Abbreviated form of Sky Survey Heading
C
      CALL TTYPALP( AP, DP, STATUS )

 500  CONTINUE

      END


C
