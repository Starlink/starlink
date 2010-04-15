      SUBROUTINE TTYOUT( RESPONSE, STATUS )

*+
*  Name:
*     TTYOUT

*  Purpose:
*   This subroutine is a minor variation on the original routine
*   OUTPUT, but adapted to write the star list directly to the
*   terminal.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TTYOUT( RESPONSE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     RESPONSE = LOGICAL (Returned)
*        Obtained from TTYHOLD call. Set to .FALSE. if user responds NO,
*        or .TRUE. if YES.
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
*     13-6-83 ( K F Hartley):
*        Adapted to write the star list directly to the
*        terminal.
*     23-FEB-1993 (AJJB):
*        Conversion to ADAM.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONV calls
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     5-MAR-1993 (AJJB):
*        RESPONSE argument added, which is passed to TTYHOLD and if it
*        comes back .FALSE. (because user entered NO), execution is
*        returned to calling routine.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     12-MAR-1993 (AJJB):
*        Changed I and JSIGN (used as 4th argument in calls to
*        CONV) to type Character, as CONV has been changed.
*     19-MAY-1993 (AJJB):
*        Put another exit point, a CONTINUE statement, right before the
*        RETURN at the end of the routine (label 201), and changed a
*        line in the main loop which used to GOTO 200 if the user
*        answers NO to a 'Continue ?' prompt so that it now jumps to the
*        new exit, 201. Label 200 is the end of the loop, you see, and
*        so it never exited from this subroutine if the user did'nt want
*        to continue - it would just go round the loop again.
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
*        CATRUN = LOGICAL (Read)
*           IF '.TRUE.' THEN SELECT STARS FROM PARTICULAR CATALOGUES
*        CHOOSE = LOGICAL (Read)
*           IF 'TRUE' SELECT SPECIFIED CSI ENTRIES ONLY
*        IDENTS = LOGICAL (Read)
*           IF '.TRUE.' WANT ALL CSI IDENTIFICATIONS
*           DEFAULT
*        NONS = LOGICAL (Read)
*           TRUE IF NONSTELLAR OBJECTS TO BE SELECTED
*        NUM = INTEGER (Read)
*           NUMBER OF FIELD STARS FOUND IN CATALOGUE
*        NUMSUPP = INTEGER (Read)
*           NUMBER OF USER SUPPLIED OBJECTS
*        YESCAT( 50 ) = INTEGER (Read)
*           ARRAY OF CATALOGUE NUMBERS TO BE INCLUDED
*        NOCAT( 50 ) = INTEGER (Read)
*           ARRAY OF CATALOGUE NUMBERS TO BE AVOIDED
*        DIAM( IDIM ) = INTEGER (Read)
*           ARRAY OF NONSTELLAR OBJ. DIAMETERS
*        NSTAR( 4, IDIM ) = INTEGER (Read)
*           INTEGER DATA ARRAY FOR THE FIELD STARS
*        IP = INTEGER (Read)
*           INDEX ARRAY TO POINT TO THE 'MAXNUM' BRIGHTEST STARS
*        NCH = INTEGER (Read)
*           NO OF CATALOGUES SELECTED IF 'CHOOSE' TRUE
*        SCALE = REAL (Read)
*           PLOTTING SCALE IN ARC. SECS./MM.
*        STAR( 2, IDIM ) = DOUBLE PRECISION (Read)
*           REAL DATA ARRAY FOR THE FIELD STARS
*        NAME( IDIM ) = CHARACTER * ( 14 ) (Read)
*           CHARACTER VAR. HOLDING NONS. OBJ. NAMES
*        CSTAR( IDIM ) = CHARACTER * ( 2 ) (Read)
*           CHARACTER*2 ARRAY OF SPECTRAL TYPES
*        DESCR( IDIM ) = CHARACTER * ( 24 ) (Read)
*           CHAR. VAR. HOLDING NONS. OBJ. DESCRIPTIONS
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'CONVF'            ! /CONVF/ common
*
*  Globals used from CONVF.FOR:
*        RDSA = DOUBLE PRECISION (Read)
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*        {descriptions_of_global_variables_referenced}...

*  Argument returned:
      LOGICAL RESPONSE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NBITS              ! [constant_description]
      PARAMETER ( NBITS = 8 * IDBYTE )

*  Local Variables:
      DOUBLE PRECISION RAO, DECO
      INTEGER DMNO, DMZ, DMN, ARR(NBITS), NVAL, NPOS,
     :        LNUM, K, LENG, J, L, IXC, IYC, MHAO, MINSAO, N, MDEGD,
     :        MINSD, NCAT, NS, NBAND, MRS, MDS, MAGV, MAGP, NHD, IDMNO,
     :        NUMID, N1SPEC, N2SPEC
      REAL X, Y, XC, YC, XM, YM, SECSAO, SECSD, RMAG, RSECS, RMAGV,
     :        RMAGP
      LOGICAL*1 IDLIST(IDBYTE), PRIVATE
      LOGICAL SET, NOSCAL
      CHARACTER*1 ISIGN, SUPPCH(20), DMSIGN, DMSUPP, HDSUPP, I, JSIGN
      CHARACTER*2 SPEC
      CHARACTER*3 DMC(4), DM
      CHARACTER*5 CATNAM
      CHARACTER*6 HDCHAR, MVCHAR, MPCHAR, MGCHAR, DIAMCH
      CHARACTER*4 IDNAM(80), CHTEMP
      CHARACTER*80 TEXT
      CHARACTER*70 PARAMS(25), VALUE*50

*  Local Data:
      DATA IDNAM / 'HD', 'AGK', 'HZ', 'CPC', 'YZ', 'CCFS', 'Boss', ' ',
     : 'SAO', 'ADS', 'IDS', 'GCRV', 'YBS', 'N30', 'FK4', 'JSK', 'A+B',
     : 'KDY', 'uvby', 'Bay', 'USNP', 'GCTP', 'GCVS', 'UBV', 'NGC', 'IC'
     : ,'CLA', 'CLB', 'LS', 'IRC', 'Cel', 'GEN', 'U+F', 'SB', 41*' ',
     : 'DM2', 'YZO', 'MMAG', ' ', ' ' /

      DATA DMC / ' BD', 'CoD', 'CPD', ' **' /

      DATA SUPPCH / ' ', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
     : 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's' /

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check for presence of PRIVATE astrometric catalogue
*
      CALL GETPARAMS(PARAMS,NVAL, STATUS )
      CALL GETDEFLT(PARAMS,NVAL,'MODE',VALUE,NPOS, STATUS )
      PRIVATE = .FALSE.
      IF(VALUE(1:3).EQ.'PRI') PRIVATE = .TRUE.
*
*   Now Print the Co-ords
*   They are Derived from the Projected
*   X,Y of the Stars
*   For Position and Field Centre Precessed Date
*   Co-ordinates are in mins. of Arc
*
*   Write out Column Headings
*
*   Initialise Line Count
*
      LNUM = 24
      IF (SUPP) LNUM = LNUM+9+NUMSUPP
      IF (CATRUN) THEN
         WRITE (TEXT,906) NUM
         CALL MSG_OUT( ' ', TEXT, STATUS )
      ELSE IF (.NOT.NONS) THEN
         WRITE (TEXT,905) NUM
         CALL MSG_OUT( ' ', TEXT, STATUS )
         IF (CHOOSE) THEN
            IF (YESCAT(1).NE.0) THEN
               WRITE (TEXT,902)
     :           (IDNAM(YESCAT(K))(:LENG(IDNAM(YESCAT(K)))),K=1,NCH)
            ELSE
               WRITE (TEXT,903)
     :             (IDNAM(NOCAT(K))(:LENG(IDNAM(NOCAT(K)))),K=1,NCH)
            ENDIF
            CALL MSG_OUT( ' ', TEXT, STATUS )
         ENDIF
      ELSE
         WRITE (TEXT,907) NUM
         CALL MSG_OUT( ' ', TEXT, STATUS )
      ENDIF
902   FORMAT(' ','Stars Selected are ONLY those in ',40(A:' and '))
903   FORMAT(' ','Stars Selected are ONLY those not in ',40(A:' or '))
905   FORMAT(' ','Number of CSI Catalogue stars found =',I4)
906   FORMAT(' ','Number of Astrometric catalogue stars found =',I4)
907   FORMAT(' ','Number of Nonstellar Objects Found =',I4)
      NOSCAL = SCALE.LT.1E-6
      DO 200 J = 1,NUM
         LNUM=LNUM+1
         IF (MOD((LNUM),45).EQ.0) THEN
            L=((LNUM)/45) + 1
         ENDIF
         IF (MOD(J-1,5).EQ.0) THEN
            CALL MSG_OUT(' ', ' ', STATUS )
         END IF
         IF (MOD(J-1,15).EQ.0) THEN
            CALL TTYHOLD( RESPONSE, STATUS )
            IF (.NOT. RESPONSE) GOTO 201  ! Fall out of routine if NO
         END IF
         RAO = STAR(1,IP(J))
         DECO = STAR(2,IP(J))
         SPEC = CSTAR(IP(J))
*
*   Compute Projected X,Y
*
         CALL PROJ(1,RAO,DECO,X,Y, STATUS )
         XC = REAL( X/RDSA )
         YC = REAL( Y/RDSA )
         IXC = NINT(XC)
         IYC = NINT(YC)
         IF (.NOT.NOSCAL) THEN
            XM = -(XC/SCALE)
            YM = YC/SCALE
         ENDIF
*
*   'CATRUN' Type Output
*   Print the Catalogue
*   Number and Magnitude of a Star
*   and its' RA & Dec.
*
         IF (CATRUN) THEN
*
*   Convert Precessed Positions
*
            CALL CONV(2,RAO,4,I,MHAO,MINSAO,N,SECSAO, STATUS )
            CALL CONV(1,DECO,3,JSIGN,MDEGD,MINSD,N,SECSD, STATUS )
            RMAG = FLOAT(NSTAR(1,IP(J)))/10.0
            NCAT = NSTAR(3,IP(J))
*
*   Check for PRIVATE astrometric catalogue
*
            IF (PRIVATE) THEN
                CATNAM = 'PRI  '
*
*   Check if SAO or AGK3 Cat. No.
*
            ELSEIF (NCAT.LT.2000000) THEN
*
*   Is AGK3, So Separate Band No. and No. within
*   that Band
*
                NCAT = NCAT - 1000000
                NS = MOD(IABS(NCAT),10000)
                NBAND = ABS(NCAT/10000)
                IF (NCAT.LT.0) THEN
                   ISIGN = '-'
                ELSE
                   ISIGN = '+'
                ENDIF
                IF (NOSCAL) THEN
               WRITE (TEXT,920) J,ISIGN,NBAND,NS,RMAG,SPEC,MHAO,MINSAO,
     :               SECSAO,JSIGN,MDEGD,MINSD,SECSD
                ELSE
               WRITE (TEXT,920) J,ISIGN,NBAND,NS,RMAG,SPEC,MHAO,MINSAO,
     :               SECSAO,JSIGN,MDEGD,MINSD,SECSD
                ENDIF
                CALL MSG_OUT( ' ', TEXT, STATUS )
920         FORMAT(' ',I3,' AGK3',1X,A1,I2,1X,I4,F8.1,3X,A2,3X,I5,I3,
     :            F7.3,2X,A1,I2,I3,F6.2,3X)
            ELSEIF (NCAT.GE.3000000) THEN
               CATNAM = 'PERTH'
               NCAT = MOD(NCAT,1000000)
               IF (NOSCAL) THEN
               WRITE(TEXT,901) J,CATNAM,NCAT,RMAG,SPEC,MHAO,MINSAO,
     :           SECSAO,JSIGN,MDEGD,MINSD,SECSD
               ELSE
               WRITE(TEXT,901) J,CATNAM,NCAT,RMAG,SPEC,MHAO,MINSAO,
     :           SECSAO,JSIGN,MDEGD,MINSD,SECSD
               ENDIF
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ELSE
               CATNAM = 'SAO  '
*
*   Modified KFH 12/5/82 to correct no-magnitude in SAO catalogue
*
               IF (ABS(RMAG-15.7).LE.0.05) RMAG=999.9
               NCAT = MOD(NCAT,1000000)
               IF (NOSCAL) THEN
               WRITE(TEXT,901) J,CATNAM,NCAT,RMAG,SPEC,MHAO,MINSAO,
     :           SECSAO,JSIGN,MDEGD,MINSD,SECSD
               ELSE
               WRITE(TEXT,901) J,CATNAM,NCAT,RMAG,SPEC,MHAO,MINSAO,
     :           SECSAO,JSIGN,MDEGD,MINSD,SECSD
               ENDIF
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ENDIF
901         FORMAT(' ',I3,1X,A5,I7,2X,F7.1,3X,A2,3X,I5,I3,F7.3,
     :              2X,A1,I2,I3,F6.2,3X)
         ELSE IF (.NOT.NONS) THEN
*
*   Normal Type Output
*
*   Convert Precessed Positions
*
            CALL CONV(2,RAO,2,I,MHAO,MINSAO,MRS,RSECS, STATUS )
            CALL CONV(1,DECO,0,JSIGN,MDEGD,MINSD,MDS,X, STATUS )
            MAGV = NSTAR(1,IP(J))
            MAGP = NSTAR(2,IP(J))
            NHD = NSTAR(3,IP(J))
            DMNO = NSTAR(4,IP(J))
            HDSUPP = SUPPCH(MOD(NHD,10) + 1)
            NHD = NHD/10
*
*   Decode the Durchmusterung Zone,Number Etc.
*
            IDMNO = ABS(DMNO)
            K = IDMNO/100000000
            DM = DMC(K)
            DMZ = (IDMNO - (K*100000000))/1000000
            DMN = (IDMNO - (K*100000000) - (DMZ*1000000))/20
            DMSUPP = SUPPCH(MOD(IDMNO,20) + 1)
            IF (DMNO.LT.0) THEN
               DMSIGN = '-'
            ELSE
               DMSIGN = '+'
            ENDIF
*
*   Convert to Chars.,Blank if Zero
*   Bracket to Indicate Unhomogenized to UBV (B.79-80 SET)
*
            WRITE (HDCHAR,930) NHD
930         FORMAT(I6)
            IF (NHD.EQ.0) HDCHAR = ' '
            RMAGV=MAGV/10.0
            RMAGP=MAGP/10.0
            IF (MAGV.EQ.9999)  THEN
               MVCHAR = ' '
            ELSE
               WRITE (MVCHAR,932) RMAGV
               IF (SET(ID(IDBYTE,IP(J)),8)) THEN
                  CHTEMP = MVCHAR(2:5)
                  MVCHAR='('//CHTEMP//')'
               ENDIF
            ENDIF
            IF (MAGP.EQ.9999) THEN
               MPCHAR = ' '
            ELSE
               WRITE (MPCHAR,932) RMAGP
               IF (SET(ID(IDBYTE,IP(J)),7)) THEN
                 CHTEMP = MPCHAR(2:5)
                 MPCHAR='('// CHTEMP //')'
               ENDIF
            ENDIF
932         FORMAT(1X,F4.1,1X)
*
*   Get the Star IDS
*
            IF (IDENTS) THEN
               DO K=1,IDBYTE
                  IDLIST(K)=ID(K,IP(J))
               ENDDO
               CALL IDENT(IDLIST,ARR,NUMID, STATUS )
            ELSE
               NUMID = 0
            ENDIF
*
*   Fix for Probable Catalogue Bug : ASCII 0'S Occuring
*
            N1SPEC=ICHAR(SPEC(1:1))
            N2SPEC=ICHAR(SPEC(2:2))
            IF (N1SPEC.EQ.0.OR.N2SPEC.EQ.0) SPEC = '  '
*
*   Now Write out the Record
*
            WRITE (TEXT,990) J,HDCHAR,HDSUPP,DM,DMSIGN,DMZ,DMN,
     :       DMSUPP,SPEC,MVCHAR,MPCHAR,MHAO,MINSAO,RSECS,JSIGN,
     :       MDEGD,MINSD,MDS
            CALL MSG_OUT( ' ', TEXT, STATUS )
990         FORMAT(1X,I4,1X,A6,A1,1X,A3,1X,A1,I2,I6,A1,2X,A2,1X,2A6,
     :       4X,I2,I3,1X,F4.1,3X,A1,I2,2I3)
         ELSE
*
*   Nonstellar Objects
*
            CALL CONV(2,RAO,2,I,MHAO,MINSAO,MRS,RSECS, STATUS )
            CALL CONV(1,DECO,0,JSIGN,MDEGD,MINSD,MDS,X, STATUS )
            RMAG = NSTAR(1,IP(J))/10.0
            IF (NSTAR(1,IP(J)).EQ.0) THEN
               MGCHAR ='      '
            ELSE
               WRITE (MGCHAR,934) RMAG
            ENDIF
            IF (DIAM(IP(J)).EQ.0) THEN
               DIAMCH = '      '
            ELSE
               WRITE (DIAMCH,936) DIAM(IP(J))
            ENDIF
934         FORMAT(F6.1)
936         FORMAT(I6)
            WRITE (TEXT,995) J,NAME(IP(J)),MHAO,MINSAO,RSECS,JSIGN,
     :       MDEGD,MINSD,MDS,MGCHAR,DIAMCH,DESCR(IP(J))
            CALL MSG_OUT( ' ', TEXT, STATUS )
995         FORMAT(1X,I4,1X,A15,2I3,F5.1,2X,A1,I2,2I3,2A6,1X,A24)
         ENDIF
200   CONTINUE     ! End of main loop
      CALL TTYHOLD( RESPONSE, STATUS )
201   CONTINUE     ! Exit point
      END

