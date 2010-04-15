      SUBROUTINE SELECT( STYPE, STATUS )
*+
*  Name:
*     SELECT

*  Purpose:
*     This is the main star selection routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SELECT( STYPE, STATUS)

*  Description:
*   This is the main star selection routine.
*   Catalogue files are as follows:
*
*   Divided into 180 zones,each
*   covering 1 degree of Dec, in north polar distance order
*
*   The first 180 records each consist of 2 integers:
*   NAVSTART,NUMBER, Where NAVSTART is the starting
*   associated variable for that zone and number the
*   number of stars in the zone.
*
*   The routine selects the stars from the correct catalogue
*   and applies proper motions as necessary.
*   The results are two arrays, STAR, and NSTAR, in the /CONTROL/
*   common block:
*
*   STAR(1,N) = RA of star in radians
*   STAR(2,N) = Dec of star in radians
*
*   NSTAR(1,N) = VIS. Magnitude of star N in 1/10ths. (9999 if unknown)
*   NSTAR(2,N) = Phot. Magnitude (ditto)
*   NSTAR(3,N) = Catalogue Number
*   NSTAR(4,N) = DM Number (=0 if CATRUN true )
*
*  Arguments:
*    STYPE = CHARACTER *(*) (Given)
*        {argument_description}
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     ANO: Someone (Somewhere)
*

*  History:
*     15-FEB-1993 (AJJB):
*        Conversion to ADAM, and proper commenting
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST calls
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     15-MAR-1993 (AJJB):
*        Function SET removed from the end if this file as it's in it's
*        own file, SET.
*     22-MAR-1993 (AJJB):
*        Replaced OPEN statement, with call to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the READONLY specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     8-FEB-1994 (PMA):
*        Increase the length of the character variable FILE to 63
*        characters.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'             ! CHART control common blocks
      INCLUDE 'CHT_ERR'          ! CHART error constants
      INCLUDE 'CONVF'            ! CONVF common block
      INCLUDE 'CATINF_CMN'       ! CATINF common
      INCLUDE 'SPT_CMN'          ! SPT common
      INCLUDE 'FILENAMES'        ! Assign filenames to string constants

*  Global variables used:
*  Globals used from /CONTROL/ common in file 'MAIN.FOR':
*
*        CATRUN = LOGICAL (Read)
*           IF '.TRUE.' THEN SELECT STARS FROM PARTICULAR CATALOGUES
*        SUPP = LOGICAL (Read)
*           TRUE IF USERS SUPPLIED OBJECTS INPUT
*        ICAT = INTEGER (Read)
*        CHOOSE = LOGICAL (Read)
*           IF 'TRUE' SELECT SPECIFIED CSI ENTRIES ONLY
*        IDENTS = LOGICAL (Read)
*           IF '.TRUE.' WANT ALL CSI IDENTIFICATIONS
*        NONS = LOGICAL (Read)
*           TRUE IF NONSTELLAR OBJECTS TO BE SELECTED
*        ID = LOGICAL*1 (Write)
*           ARRAY OF 8*IDBYTE BIT IDENT. STRINGS FROM CSI
*        NUM = INTEGER (Write)
*           NUMBER OF FIELD STARS FOUND IN CATALOGUE
*        DIAM(IDIM) = INTEGER (Write)
*           ARRAY OF NONSTELLAR OBJ. DIAMETERS
*        NSTAR(4,IDIM) = INTEGER (Write)
*           INTEGER DATA ARRAY FOR THE FIELD STARS
*        ICAT1(0:10) = INTEGER (Write)
*           ARRAY CONTAINING CATALOGUE IDENTIFICATION
*        SIZE = REAL (Read)
*           HALF-WIDTH OF FIELD IN DEGREES
*        FAINT = REAL (Read)
*           MAG. OF FAINTEST STAR REQUIRED
*        EQUOUT = REAL (Read)
*           REQUIRED EQUINOX OF FIELD STAR POSITIONS
*        EPOCH = REAL (Read)
*           REQUIRED EPOCH OF FIELD STAR POSITIONS IF 'CATRUN' TRUE
*        D = DOUBLE PRECISION (Read)
*           INPUT FIELD CENTRE DEC
*        AP = DOUBLE PRECISION (Read)
*           FIELD CENTRE RA AT EQUINOX 1950.0
*        DP = DOUBLE PRECISION (Read)
*           FIELD CENTRE DEC AT EQUINOX 1950.0
*        STAR(2, IDIM) = DOUBLE PRECISION (Read and Write)
*           REAL DATA ARRAY FOR THE FIELD STARS
*        PM(2,IDIM) = REAL (Write)
*           REAL ARRAY FOR PROPER MOTIONS
*        NAME(IDIM) = CHARACTER*14 (Write)
*           CHARACTER VAR. HOLDING NONS. OBJ. NAMES

*  Globals used from /SPECS/ common in file MAIN.FOR:

*        CSTAR(IDIM) = CHARACTER*2 (Write)
*           CHARACTER*2 ARRAY OF SPECTRAL TYPES
*        DESCR(IDIM) = CHARACTER*24 (Write)
*           CHAR. VAR. HOLDING NONS. OBJ. DESCRIPTIONS

* Globals used from /CONVF/ common in CONVF.FOR:
*
*        TWOPI = DOUBLE PRECISION (Read)
*        HALFPI = DOUBLE PRECISION (Read)
*        RDSA = DOUBLE PRECISION (Read)
*        RDST = DOUBLE PRECISION (Read)
*        RDDG = DOUBLE PRECISION (Read)
*
*     Globals used from /CATINF/ common in CATINF.FOR:
*
*        RA = DOUBLE PRECISION (Read and Write)
*        DEC = DOUBLE PRECISION (Read and Write)
*        MAGP = INTEGER (Read)
*        NHD = INTEGER (Read)
*           [global_variable_purpose]
*        [descriptions_of_global_variables_referenced]...

*        MAGV = INTEGER (Read)
*        DMNO = INTEGER (Write)
*        IEPS = INTEGER (Read)
*        IPA = INTEGER (Read)
*        IPD = INTEGER (Read)
*        ICAT = INTEGER (Read)
*        NCAT = INTEGER (Read)
*        IDLIST(IDBYTE) = LOGICAL*1 (Read and Write)
*        DIAMET = INTEGER (Read)
*           [global_variable_purpose]
*        [descriptions_of_global_variables_referenced]...
*
*     Globals used from /SPT/ common in SPT.FOR:
*
*        SPEC = CHARACTER * ( 2 ) (Read)
*        NAM = CHARACTER * ( 14 ) (Read)
*        DES = CHARACTER * ( 24 ) (Read)

*  Arguments Given:
      CHARACTER * ( * ) STYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

      DOUBLE PRECISION PMA, PMD
      LOGICAL FOUND, FOUNDX, SET
      CHARACTER FILE*63
      CHARACTER*4  CHARNUM
      CHARACTER*70 TEXT, PARAMS( 25 ), VALUE*50
      INTEGER UNIT, RECL, NVAL, NPOS, MINB, MAXB, N, M, NAV, ISTA,
     : NUMBER, ISTB, MIN, MAX, MINAV, MAXAV, NTEM, LASTAV, I, MAG, K
      REAL DG, RSIZE, TEMP, REAL, AMIN, AMAX, CATEP, TOL, X, Y, ADIF

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Test if a private astrometric catalogue is to be opened
*
      CALL GETPARAMS(PARAMS,NVAL, STATUS )
      CALL GETDEFLT(PARAMS,NVAL,'MODE',VALUE,NPOS, STATUS )
      IF (VALUE(1:3).EQ.'PRI') THEN
         CALL GETDEFLT(PARAMS,NVAL,'INPUT',FILE,NPOS, STATUS)
         UNIT = 12
         RECL = 6
      ELSEIF (CATRUN) THEN
         FILE = AST
         UNIT = 12
         RECL = 6
      ELSE IF (.NOT.NONS) THEN
         FILE = CSI
         UNIT = 11
         RECL = 9
      ELSE
         FILE = NSO
         UNIT = 13
         RECL = 13
      ENDIF

* This statement :
*
*     OPEN(UNIT=UNIT,FILE=FILE,ACCESS='DIRECT',
*    :  STATUS='OLD',ERR=700,RECL=RECL,READONLY)
*
* is now replaced by this call (see History):

      CALL FILEOPEN( UNIT, FILE, 'OLD', 'DIRECT', ' ', .FALSE., RECL,
     :              .TRUE., STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 700

      CALL CONST(AP,DP, STATUS )
      DG = REAL( DP/RDDG )
      MINB = 90 - INT(DG+SIZE)
      MAXB = 90 - INT(DG-SIZE)
*
*   Determine limits of RA and Dec
*   If upper limit is in region of one of the poles
*   Examine all stars north of the lower limit(N. Pole)
*   or south of upper limit(S. Pole)
*
      IF((DG+SIZE).LT.0.0) MINB = MINB + 1
      IF((DG-SIZE).LT.0.0) MAXB = MAXB + 1
      IF(MINB.LT.1)   MINB = 1
      IF (MAXB.GT.180) MAXB = 180
      IF(DG.GT. 90.0-SIZE) MINB = 1
      IF (DG.GT.90.0-SIZE) MAXB = MAXB + 1
      IF(DG.LT.SIZE-90.0) MAXB = 180
      IF(DG.LT. SIZE - 90.0) MINB = MINB - 1
      RSIZE = REAL( SIZE*RDDG )
      TEMP = REAL( DP + RSIZE )
      IF(DP.LT.0.0) TEMP = REAL( DP - RSIZE )
      IF (TEMP.GE.HALFPI) GO TO 110
      ADIF = RSIZE/COS(TEMP)
*
* Setup AMIN and AMAX according to search criterion
*
      IF (STYPE.EQ.'NORMAL') THEN
          AMIN = REAL( AP - ADIF )
          AMAX = REAL( AP + ADIF )
          IF(AMIN.LT.0.0)   AMIN = REAL( AMIN + TWOPI )
          IF(AMAX.GT.TWOPI) AMAX = REAL( AMAX - TWOPI )
      ELSEIF (STYPE.EQ.'BAND') THEN
          AMIN = 0.0
          AMAX = REAL( TWOPI )
      ENDIF
*
*   Initialise Star Count
*
110   CONTINUE
      N = 0
*
*   This is the Main Catalogue Search Loop
*   Each zone in the Catalogue which may be on the field
*   is scanned.
*   Firstly, the area in the zone which contains
*   the limits of R.A. is found by a Binary Search.
*   Then each star in this part of the zone is examined
*   to see if it is in the field.
*   Proper Motions are applied if the star is in.
*
      DO 400 M = MINB,MAXB
*
*   Look at Index to find Range of A.V.'S
*
         NAV = M
         READ (UNIT,REC=NAV) ISTA,NUMBER
         IF (CATRUN) THEN
            ISTB = ISTA + NUMBER
         ELSE
            ISTB = NUMBER +1
         ENDIF
*
*   ISTA = A.V. for first star of zone with NPD = M
*   ISTB = A.V. for first star of zone with NPD = M+1
*
*   Find MINAV,the A.V. of the star with the R.A.
*   on the left of the field
*
         MIN = ISTA
         MAX = ISTB - 1
         IF (MINB.NE.1.AND.MAXB.NE.180) GO TO 120
         MINAV = MIN
         MAXAV = MAX
         GO TO 138
120      CONTINUE
         IF((MAX-MIN).LE.5) GO TO 124
         NTEM = (MIN+MAX)/2
         NAV  = NTEM
121      CONTINUE
         CALL CATREC(UNIT,NAV, STATUS )
         IF(NAV.GE.MAX) GO TO 123
         IF(RA.GE.AMIN) GO TO 122
         MIN = NAV - 1
         GO TO 120
122      CONTINUE
         MAX = NAV - 1
         GO TO 120
123      CONTINUE
         MAX = NTEM
         GO TO 120
124      CONTINUE
         MINAV = MIN
*
*   And MAXAV,The AV of the right hand side
*
         MIN = ISTA
         MAX = ISTB - 1
130      CONTINUE
         IF((MAX-MIN).LE.5) GO TO 134
         NTEM = (MIN+MAX)/2
         NAV  = NTEM
         IF(NAV.GE.MAX) GO TO 133
131      CONTINUE
         CALL CATREC(UNIT,NAV, STATUS )
         IF(RA.GE.AMAX) GO TO 132
         MIN = NAV - 1
         GO TO 130
132      CONTINUE
         MAX = NAV - 1
         GO TO 130
133      CONTINUE
         MAX = NTEM
         GO TO 130
134      CONTINUE
         MAXAV = MAX
*
*   Make selection of stars
*   MINAV = A.V. of min RA in zone
*   MAXAV = A.V. of max RA in zone
*   but across (RA=0) we will have MAXAV.LT.MINAV
*   LASTAV is the current stopping point,reading serially
*
138         CONTINUE
            NAV = MINAV
            LASTAV = MAXAV
            IF (MINAV.GT.MAXAV) LASTAV = ISTB
144         CONTINUE
            IF(NAV.LT.ISTB)       GO TO 145
*
*   Have read off end of zone.
*   If search area straddles 0h. R.A. then go back to beginning
*   otherwise we are finished (i.e. LASTAV = ISTB - 1)
*
            IF (MINAV.LT.MAXAV) GO TO 400
            NAV = ISTA
            LASTAV = MAXAV

145         CONTINUE
            IF (NAV.GT.LASTAV) GO TO 400
            CALL CATREC(UNIT,NAV, STATUS )
            NAV = NAV+1
*
*   Catalogue Selection
*   If 'CATRUN' type run use ICAT1(0:10)
*   otherwise  use YESCAT,NOCAT arrays checked by 'FOUND'
*
         IF (CATRUN) THEN
             DO I = 0,10
                 IF(ICAT.EQ.ICAT1(I)) GO TO 146
             ENDDO
             GO TO 144
         ENDIF
146      CONTINUE
         IF (CHOOSE.AND.(.NOT.FOUND(IDLIST))) GOTO 144

         IF(ABS(DEC-D).GT.SIZE) GO TO 144
         IF (MAGV.EQ.9999) THEN
            MAG = MAGP
         ELSE
            MAG=MAGV
         ENDIF
         FOUNDX = SET(IDLIST(3),7)
         IF (FLOAT(MAG)/10.0.GT.FAINT.AND..NOT.FOUNDX) GO TO 144
*
*   Apply Proper Motions (if 'CATRUN' true)
*   to the required Epoch
*
         IF (CATRUN) THEN
            CATEP = FLOAT(IEPS)/100.0 + 1900.0
190         CONTINUE
            PMD  = FLOAT(IPD)*RDSA/10000.0
            PMA  = FLOAT(IPA)*RDST/100000.0
            RA   = RA  + PMA*(EPOCH-CATEP)
            DEC  = DEC + PMD*(EPOCH-CATEP)
         ENDIF
*
*   Compute Projected Plate X,Y
*
195      CONTINUE
         TOL = TAN(RSIZE)
         CALL PROJ(1,RA,DEC,X,Y, STATUS )
*
*  Check that it is in Range (TOL is Projected Plate Size)
*
         IF (STYPE.EQ.'NORMAL') THEN
             IF ((ABS(X).GT.TOL).OR.(ABS(Y).GT.TOL)) GO TO 144
         ELSEIF (STYPE.EQ.'BAND') THEN
             CALL CONST(RA,DP, STATUS )
             IF ((ABS(Y).GT.TOL)) GO TO 144
         ENDIF
*
*   Check that limit has not been exceeded
*
147      CONTINUE
         IF (N.LT.IDIM) GO TO 151
         IF (.NOT.NONS) THEN
            TEXT='          SEARCH TERMINATED:  2000 STARS FOUND'
         ELSE
            TEXT='          SEARCH TERMINATED:  2000 OBJECTS FOUND'
         ENDIF
         CALL MSG_OUT (' ', TEXT, STATUS)
         CALL MSG_OUT (' ', ' ', STATUS)
         CALL MSG_OUT (' ', ' ', STATUS)
         GOTO 160
*
*   Found a star in field
*   Increment count,N.
*
151      CONTINUE
         N = N + 1
*
*   Set up array of stars in this field: STAR(2,IDIM)
*   (IDIM is max. no. of stars).
*   STAR(1,N) = RA of star in radians
*   STAR(2,N) = Dec of star in radians
*   NSTAR(1,N) = VIS. Magnitude of star N in 1/10ths. (9999 if unknown)
*   If 'CATRUN' this is the magnitude in the catalogue used
*      NSTAR(2,N) = Phot. Magnitude (ditto)
*   If 'CATRUN' is = 9999
*      NSTAR(3,N) = Catalogue Number:
*   If CATRUN : is the Catalogue Number as found
*     + 1000000 for AGK3
*     + 2000000 for SAO
*     + 3000000 for PERTH70
*   If not    : is the Henry Draper Number (0 if not in HD)
*      NSTAR(4,N) = DM Number (=0 if CATRUN true )
*      DM is coded as :
*      DMNO= DM*100000000 + ABS(ZONE)*1000000 + NO.*20 + SUPP
*      where DM = (1,2,3,4)(BD,CD,CP,UNKNOWN)
*      The posns. and the Field Centre are Equinox 1950.0
*      in the Catalogue. These are precessed to 'EQUOUT'
*
153      CONTINUE
         CALL PRECES(RA,DEC,STAR(1,N),STAR(2,N),1950.0,EQUOUT, STATUS )

         IF (CATRUN) THEN
            NSTAR(1,N) = MAG
            NSTAR(2,N) = 9999
            NSTAR(3,N) = NCAT + (1000000 * ICAT)
            NSTAR(4,N) = 0
            PM(1,N)=REAL( PMD/RDSA )
            PM(2,N)=REAL( PMA/RDST )
         ELSE IF (.NOT.NONS) THEN
            NSTAR(1,N) = MAGV
            NSTAR(2,N) = MAGP
            NSTAR(3,N) = NHD
            NSTAR(4,N) = DMNO
            PM(1,N)=0.0
            PM(2,N)=0.0
         ELSE
            NSTAR(1,N) = MAG
            DESCR(N) = DES
            NAME(N)  = NAM
            DIAM(N)  = DIAMET
            PM(1,N)=0.0
            PM(2,N)=0.0
         ENDIF
         CSTAR(N) = SPEC
         IF (IDENTS) THEN
            DO K=1,IDBYTE
               ID(K,N)=IDLIST(K)
            ENDDO
         ENDIF

         GO TO 144

400   CONTINUE
*
*   End of Main Catalogue Search Loop
*
      CALL MSG_OUT (' ', ' ', STATUS)
      CALL MSG_OUT (' ', ' ', STATUS)
      CALL MSG_OUT (' ', ' ', STATUS)

      WRITE (CHARNUM,'(I4)') N
      IF (.NOT.NONS) THEN
         CALL MSG_OUT (' ', '        NUMBER OF CATALOGUE STARS IN AREA'
     :   // ' = '//CHARNUM, STATUS)
      ELSE
         CALL MSG_OUT (' ', '        NUMBER OF NONSTELLAR OBJECTS FOUND'
     :   // ' = '//CHARNUM, STATUS)
      ENDIF
      CALL MSG_OUT (' ', ' ', STATUS)
      CALL MSG_OUT (' ', ' ', STATUS)

160   CONTINUE
      NUM = N
      CLOSE(UNIT=UNIT)
      RETURN
*
*     ERROR DURING OPENING CATALOGUE FILES
*
700   CONTINUE
      IF (UNIT.EQ.12.AND.VALUE(1:3).EQ.'PRI') THEN
         TEXT=' ERROR - Unable to open Private catalogue file'
         STATUS = CHT__NOCAT
         CALL ERR_REP (' ', TEXT, STATUS)
         CALL ERR_FLUSH( STATUS )
      ELSE IF (UNIT.EQ.12.AND.VALUE(1:3).NE.'PRI') THEN
         TEXT=' ERROR - Unable to open Astrometric catalogue file'
         STATUS = CHT__NOCAT
         CALL ERR_REP (' ', TEXT, STATUS)
         CALL ERR_FLUSH( STATUS )
      ELSE IF (UNIT.EQ.11) THEN
         TEXT=' ERROR - Unable to open CSI catalogue file'
         STATUS = CHT__NOCAT
         CALL ERR_REP (' ', TEXT, STATUS)
         CALL ERR_FLUSH( STATUS )
      ELSE
         TEXT=' ERROR - Unable to open Nonstellar object file'
         STATUS = CHT__NOCAT
         CALL ERR_REP (' ', TEXT, STATUS)
         CALL ERR_FLUSH( STATUS )
      ENDIF
      END



