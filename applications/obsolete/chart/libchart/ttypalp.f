      SUBROUTINE TTYPALP( AP, DP, STATUS )
*+
*  Name:
*     TTYPALP

*  Purpose:
*   This Routine finds which Sky Survey
*   Plate a given Field Centre
*   is on & its' Approx. Co-ordinates Relative
*   to the Bottom Left-hand Corner.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TTYPALP( AP, DP, STATUS )

*  Description:
*   This Routine finds which Sky Survey
*   Plate a given Field Centre
*   is on & its' Approx. Co-ordinates Relative
*   to the Bottom Left-hand Corner.
*   Centres South of -20 Deg. Use
*   ESO/SRC Survey otherwise Palomar.
*   Note : Palomar /SRC Survey Schmidt Plates are 14 ins.
*   Square with Scale 67.14 Arc. sec./mm.
*   ESO Survey Plates are 30 cms. Square
*   with Scale 67.6 Arc. secs. per mm.
*
*   This variation writes output directly to a terminal
*     {routine_description}

*  Arguments:
*     AP = DOUBLE PRECISION (Given)
*     DP = DOUBLE PRECISION (Given)
*        AP,DP   - Given Field Centre
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     ANO: A.Nonymous (Some University Or Something)
*     {enter_new_authors_here}

*  History:
*     {date} ({author_identifier}):
*        Original version.
*     24-FEB-1993 (AJJB):
*        Conversion to ADAM.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     16-MAR-1993 (AJJB):
*        Removed the VAL function from the end of this file, as it is
*        redundant - a newer, simpler version of the function is in
*        VAL.F (VAL.FOR).
*
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'CONVF'            ! /CONVF/ common block
*        RDSA = DOUBLE PRECISION (Read)
*        RDST = DOUBLE PRECISION (Read)
*        RDDG = DOUBLE PRECISION (Read)
*           [global_variable_purpose]
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*        {descriptions_of_global_variables_referenced}...

*  Arguments Given:
      DOUBLE PRECISION AP, DP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AL, DL, PCA, PCR
      LOGICAL SURVEY(3)
      CHARACTER*10 NAMES(3)
      INTEGER NPW, SPW, FNUM, FNST, K, NPS, MRA, NBAND, NPWIDE, NPL,
     :        NTOT, NRAC, NCDEC, NCRAH, NCRAM
      REAL DEC, RAMINS, X, Y, SCALE, PALSC, FIELD, PALF, ESOSC, ESOF,
     :        CORNER, NXC, NYC
      DIMENSION NPW(23), SPW(22), FNST(22)
      CHARACTER*80 TEXT



*  Local Data:

      DATA NPW / 2880, 150, 92, 68, 52, 44, 38, 34, 30, 28,
     : 26, 26, 24, 24, 24, 24, 24, 24, 24, 26, 26, 28, 30 /
      DATA SPW / 20, 20, 20, 20, 20, 20, 20, 21, 22, 23, 24,
     : 26, 28, 30, 33, 38, 44, 52, 66, 90, 144, 2880 /
      DATA FNST / 1039, 967, 895, 823, 751, 679, 607, 538, 472,
     : 409, 349, 293, 241, 193, 149, 111, 78, 50, 28, 12, 2, 1 /
      DATA NAMES / 'Palomar', 'UK Schmidt', 'ESO' /
      DATA PALF, ESOF / 355.6, 300.0 /
      DATA PALSC, ESOSC / 67.14, 67.6 /

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   PALF,ESOF = Plate Sizes in mm.
*

      CALL MSG_OUT( ' ', ' ', STATUS )
      CALL MSG_OUT( ' ', 'Position on Sky Survey plates', STATUS )
      CALL MSG_OUT( ' ', ' ', STATUS )
      WRITE (TEXT,997)
997   FORMAT(' ','Survey',5X,' Field Centre ',9X,'X',8X,'Y')
*
*   Precess Field Centre to 1855.0 (Equinox of Poss Field Centres)
*
      CALL PRECES(AP,DP,AL,DL,1950.00,1855.00, STATUS )
*
*   Find which Surveys Field Centre is on
*   1 = POSS ; 2=UKST ; 3 = ESO
*
      SURVEY(1) = DL.GT.(-45.0)*RDDG
      SURVEY(2) = DP.LT.(18.0*RDDG)
      SURVEY(3) = DP.LT.(-17.5*RDDG)

      DO 100 K=1,3
         IF (SURVEY(K)) THEN
*
*   Set Plate Spacing (6 Degs. for Palomar,5 for ESO,SRC)
*
            IF (K.GT.1) NPS = 5
            IF (K.EQ.1) NPS = 6
*
*   Precess Centres to Survey Equinox
*
            IF (K.EQ.1.AND.DL/RDDG.LT.-21.0) THEN
               CALL PRECES(AP,DP,AL,DL,1950.00,1875.00, STATUS )
            ENDIF
            IF (K.GT.1) THEN
               AL = AP
               DL = DP
            ENDIF
            DEC = REAL( DL/RDDG )
            RAMINS = REAL( AL * 4.0/RDDG )
            MRA = NINT(RAMINS)
*
*   Now find which Band the Centre is in
*   +90 =1,.....,-42=23 at 6 Deg. Spacing ( Palomar)
*   +15 = 1......,-90 = 22 at 5 Deg. Spacing (SRC/ESO Southern)
*
            IF (K.EQ.1) THEN
               NBAND = 16 - (IFIX(DEC+SIGN(FLOAT(NPS)/2.0,DEC))/NPS)
            ELSE
               NBAND = 4 - (IFIX(DEC+SIGN(FLOAT(NPS)/2.0,DEC))/NPS)
*
*   Deal with Case at Northern Limit of UKST Survey
*
               IF (K.EQ.2.AND.NBAND.EQ.0) NBAND = 1
            ENDIF
*
*   And how Wide the Plates are for the Band in mins. of RA
*
            IF (K.EQ.1) THEN
               NPWIDE = NPW(NBAND)
            ELSE
               NPWIDE = SPW(NBAND)
            ENDIF
*
*   NPL is Plate No. Within Band (RA 0H. = 0)
*   NRAC is Survey Plate Centre in mins. of Time
*
            NPL = (MRA+NPWIDE/2)/NPWIDE
*
*   For UK Schmidt can Compute Field Number
*
            IF (K.EQ.2) THEN
               NTOT = FNST(NBAND)
               FNUM = NTOT + NPL
C
C             If the RA is close to 24 hrs the field may actually
C             be found on the first rather than the last plate
C             in the band. This is tested by checking the plate
C             number found against the first number in the next band.
C
C             Modified by K.F.Hartley at RGO on 4-11-83
C             Corrected to make it work by J.V.Carey ON 11-5-84
C
               IF (FNUM.GE.FNST(NBAND-1)) THEN
                  FNUM = FNST(NBAND)
               END IF
            ENDIF
            NRAC = NPL * NPWIDE
*
*   Inserted by KFH on 17/12/82 to get round irregular
*   interval between last plate in band and first in band
*
            IF (NRAC.GE.24*60) NRAC=0
*
*   NCDEC:Plate Centre Dec in Degs.
*   NCRAH,NCRAM : P.C. RA hrs. and mins.
*   CDEC = Corner Dec. in Degs.
*   CRA = Corner RA in Mins.
*
            IF (K.EQ.1) THEN
               NCDEC = 90 -((NBAND - 1) * 6)
            ELSE
               NCDEC = 15 -((NBAND-1)*5)
            ENDIF
            PCR = NCDEC * RDDG
            NCRAH = NRAC/60
            NCRAM = NRAC - (NCRAH * 60)
*
*   Correct for fact that last plate in band overlaps
*   first in band (hopefully)!
*   Inserted by K F Hartley 1982 December 17
*
            IF (NCRAH.GE.24) THEN
               NCRAH=0
               NCRAM=0
            END IF
            PCA = NRAC * 60.0 * RDST

            CALL CONST(PCA,PCR, STATUS )
            CALL PROJ(1,AL,DL,X,Y, STATUS )
*
*   Scale is 67.14 for Poss & UKST ; 67.6 for ESO
*   Field Widths are PALF,ESOF in mm.
*
            IF (K.LE.2) THEN
               SCALE = PALSC
               FIELD = PALF
            ELSE
               SCALE = ESOSC
               FIELD = ESOF
            ENDIF
            CORNER = REAL( FIELD/2.0 * SCALE * RDSA )
*
*   XC,YC are Co-ords of Centre in mm.
*
            NXC = NINT((CORNER - X)/(RDSA * SCALE))
            NYC = NINT((CORNER + Y)/(RDSA * SCALE))
            IF (K.EQ.2) THEN
               WRITE (TEXT,900) NAMES(K),NCDEC,NCRAH,NCRAM,NXC,NYC,FNUM
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ELSE
               WRITE (TEXT,901) NAMES(K),NCDEC,NCRAH,NCRAM,NXC,NYC
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ENDIF
900         FORMAT(' ',A10,3X,SP,I3,S,2X,I2,'H',I3,'M',6X,F6.2,3X,F6.2,
     :          '  (Field Number: ',I4,')')
901         FORMAT(' ',A10,3X,SP,I3,S,2X,I2,'H',I3,'M',6X,F6.2,3X,F6.2)
         ENDIF
100   CONTINUE
      CALL MSG_OUT( ' ', 'Co-ordinates are relative to South East '//
     :      'Corner', STATUS )
      CALL MSG_OUT( ' ', 'of the actual plate, not the emulsion',
     :       STATUS )
      END

