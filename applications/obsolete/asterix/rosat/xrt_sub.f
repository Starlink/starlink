*+  XRT_SUB - general subroutines for XRT interface
*-
*+ Swaps BYTE array about specified axes
      SUBROUTINE XRT_AXSWAP_B( DIMS, IN, SAX, ODIMS, OUT, STATUS )
*    Description :
*    Method :
*
*    Authors :
*
*     Richard Saxton, David J. Allan ( BHVAD::DJA )
*
*
*    History :
*
*     7-Jun-90    original
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER              DIMS(DAT__MXDIM)
      BYTE                 IN(*)
      INTEGER              SAX(DAT__MXDIM)
      INTEGER              ODIMS(DAT__MXDIM)

*    Export :
      BYTE                 OUT(*)
*    Status :
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Call internal routine
      CALL XRT_AXSWAP_B_INT( DIMS, DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                      DIMS(5), DIMS(6), DIMS(7), IN, SAX,
     :                      ODIMS(1), ODIMS(2), ODIMS(3), ODIMS(4),
     :                      ODIMS(5), ODIMS(6), ODIMS(7), OUT )

      END

*+  XRT_AXSWAP_B_INT - Swap BYTE array about specified axes
      SUBROUTINE XRT_AXSWAP_B_INT( DIMS, L1, L2, L3, L4, L5, L6, L7, IN,
     :                            SAX, O1, O2, O3, O4, O5, O6, O7, OUT )
*    Description :
*    Method :
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*     13 Dec 89 : Original (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER              DIMS(DAT__MXDIM)
      INTEGER              L1,L2,L3,L4,L5,L6,L7
      INTEGER              SAX(DAT__MXDIM)
      BYTE                 IN(L1,L2,L3,L4,L5,L6,L7)
      INTEGER              O1,O2,O3,O4,O5,O6,O7
*    Export :
      BYTE                 OUT(O1,O2,O3,O4,O5,O6,O7)
*    Local variables :
*
      INTEGER              II(DAT__MXDIM)
      INTEGER              A,B,C,D,E,F,G
*-

*    Perform data transfer
      DO G = 1, L7
        II(7)=G
       DO F = 1, L6
        II(6) = F
        DO E = 1, L5
         II(5) = E
         DO D = 1, L4
          II(4) = D
          DO C = 1, L3
           II(3) = C
           DO B = 1, L2
            II(2) = B
            DO A = 1, L1
             II(1)=A
*
             OUT (II(SAX(1)),II(SAX(2)),II(SAX(3)),II(SAX(4)),II(SAX(5))
     :           ,II(SAX(6)),II(SAX(7))) = IN (A,B,C,D,E,F,G)
*
            END DO
           END DO
          END DO
         END DO
        END DO
       END DO
      END DO
*
      END

*+XRT_AXSWAP_R Swap REAL array about specified axes
      SUBROUTINE XRT_AXSWAP_R( DIMS, IN, SAX, ODIMS, OUT, STATUS )
*    Description :
*    Method :
*
*    Authors :
*
*     Richard Saxton, David J. Allan ( BHVAD::DJA )
*
*
*    History :
*
*     7-Jun-90    original
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER              DIMS(DAT__MXDIM)
      REAL                 IN(*)
      INTEGER              SAX(DAT__MXDIM)
      INTEGER              ODIMS(DAT__MXDIM)

*    Export :
      REAL                 OUT(*)
*    Status :
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Call internal routine
      CALL XRT_AXSWAP_R_INT( DIMS, DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                      DIMS(5), DIMS(6), DIMS(7), IN, SAX,
     :                      ODIMS(1), ODIMS(2), ODIMS(3), ODIMS(4),
     :                      ODIMS(5), ODIMS(6), ODIMS(7), OUT )

      END
*+  XRT_AXSWAP_R_INT - Swap REAL array about specified axes
      SUBROUTINE XRT_AXSWAP_R_INT( DIMS, L1, L2, L3, L4, L5, L6, L7, IN,
     :                            SAX, O1, O2, O3, O4, O5, O6, O7, OUT )
*    Description :
*    Method :
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*     13 Dec 89 : Original (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER              DIMS(DAT__MXDIM)
      INTEGER              L1,L2,L3,L4,L5,L6,L7
      INTEGER              SAX(DAT__MXDIM)
      REAL                 IN(L1,L2,L3,L4,L5,L6,L7)
      INTEGER              O1,O2,O3,O4,O5,O6,O7
*    Export :
      REAL                 OUT(O1,O2,O3,O4,O5,O6,O7)
*    Local variables :
*
      INTEGER              II(DAT__MXDIM)
      INTEGER              A,B,C,D,E,F,G
*-

*    Perform data transfer
      DO G = 1, L7
        II(7)=G
       DO F = 1, L6
        II(6) = F
        DO E = 1, L5
         II(5) = E
         DO D = 1, L4
          II(4) = D
          DO C = 1, L3
           II(3) = C
           DO B = 1, L2
            II(2) = B
            DO A = 1, L1
             II(1)=A
*
             OUT (II(SAX(1)),II(SAX(2)),II(SAX(3)),II(SAX(4)),II(SAX(5))
     :           ,II(SAX(6)),II(SAX(7))) = IN (A,B,C,D,E,F,G)
*
            END DO
           END DO
          END DO
         END DO
        END DO
       END DO
      END DO
*
      END

*+XRT_CALCAREA - Calculate area of a box
      SUBROUTINE XRT_CALCAREA(HEAD, LQUAL, NX, NY, NT, NE,
     &                                    NR, QUAL, MASK, AREA)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global variables :
      INCLUDE 'QUAL_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ HEAD                ! Header structure
      LOGICAL LQUAL                     ! Is quality available ?
      INTEGER NX,NY,NT,NE,NR            ! Dimensions of quality array
      BYTE QUAL(NX,NY,NT,NE,NR)         ! Quality
      BYTE MASK                         ! Quality mask
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      REAL AREA                         ! Box area in square arcmins
*    Local constants :
      REAL PI
        PARAMETER (PI=3.14159265)
*    Local variables :
      INTEGER PCOUNT,LPX,LPY
      REAL XOFF,YOFF                    ! X and Y off axis angle (degs)
      REAL AXOFF                        ! Mean off-axis angle (arcmins)
*-
* Either find the area by counting pixels if possible and wanted or
* calculate the geometric area
      IF (.NOT. LQUAL .OR. (NX.EQ.1 .AND. NY.EQ.1) ) THEN
*
         IF (HEAD.SHAPE(1:1) .EQ. 'R') THEN
*
*   Square box:
            AREA = 4.0 * ( HEAD.XOUTER * HEAD.YOUTER -
     &                     HEAD.XINNER * HEAD.YINNER )
*
         ELSEIF (INDEX ('CAE', HEAD.SHAPE(1:1)) .NE. 0) THEN
*
*   Circle, ellipse or annulus
            AREA = PI * ( HEAD.XOUTER * HEAD.YOUTER -
     &                             HEAD.XINNER * HEAD.YINNER )
*
         ENDIF
*
      ELSE
*
*   Calculate the area by finding the number of good pixels
*   Just take one slice in time and energy.
         PCOUNT = 0
         AXOFF = 0.0
*
         DO LPY=1,NY
            DO LPX=1,NX
*
               IF ( (QUAL(LPX,LPY,1,1,1) .AND. MASK)
     &                                         .EQ. QUAL_GOOD ) THEN
                  PCOUNT = PCOUNT + 1
*
*       Sum the offset angles of each GOOD pixel. (Relies on the X axis
*       increasing from right to lef and Y increasing from bottom to top !)
                  XOFF = HEAD.XCENT - (NX/2.0 - LPX) * HEAD.XSCALE
                  YOFF = HEAD.YCENT + (NY/2.0 - LPY) * HEAD.YSCALE
                  AXOFF = AXOFF + SQRT(XOFF*XOFF + YOFF*YOFF)
*
               ENDIF
*
            ENDDO
         ENDDO
*
*   Check some good pixels were found
         IF (PCOUNT .EQ. 0) THEN
            CALL MSG_PRNT('** Error: No good pixels found **')
            GOTO 999
         ENDIF
*
*   Modify the off-axis value
         HEAD.OFFAX = AXOFF / REAL(PCOUNT) * 60.
*
*   Multiply by the area of a pixel in square degrees.
         AREA = PCOUNT * HEAD.XSCALE * HEAD.YSCALE
*
      ENDIF
*
* Convert area in square degrees into arcmin^2
      AREA = AREA * 3600.
*
999   CONTINUE

      END

*+XRT_CALDEF - returns the directory name for XRT cal files depending on machine
      SUBROUTINE XRT_CALDEF(CALDIR, STATUS)
*    Description :
*     Decodes the environment variable or logical which points to
*     the XRT cal directory and returns the path name as a string
*
*      Output:
*
*         VAX:    AST_ROOT:[DATA.ROSAT]
*        UNIX:    /star/asterix/data/rosat/
*
*    Method :
*     Uses PSX_GETENV to translate the directory name
*    History :
*     4-Mar-1993 - original  (RDS)
*    Type definitions :
      IMPLICIT NONE
*    Status:
      INTEGER STATUS
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_SYS_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
      CHARACTER*(*) CALDIR           ! Name of the directory holding the
*                                    ! XRT cal. files.
*    Functions :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*    Local variables :
*    Local data :
*-
* Translate the dierctory specifier XRTCAL
      CALL PSX_GETENV('XRTCAL', CALDIR, STATUS)
*
* Add the standard character on to the directory spec for this machine
      CALDIR = CALDIR(1:CHR_LEN(CALDIR)) //
     &                   FIL_SEP_CH
*
      END

*+ XRT_CIRCLE
	SUBROUTINE XRT_CIRCLE(XC,YC,RAD,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
*  Import :
        REAL XC,YC,RAD
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
	REAL PI, DTOR
	PARAMETER (PI = 3.1415927, DTOR = PI/180.0)
*  Local variables :
      REAL X1,X2,X3,X4,X
      REAL Y1,Y2,Y3,Y4,Y
      REAL A
      INTEGER IA
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL PGUPDT(0)

        X1=XC+RAD
        Y1=YC
        X2=X1
        Y2=Y1
        X3=XC-RAD
        Y3=YC
        X4=X3
        Y4=Y3

        DO IA=5,90,5

          A=REAL(IA)*DTOR
          X=RAD*COS(A)
          Y=RAD*SIN(A)

          CALL PGMOVE(X1,Y1)
          X1=XC+X
          Y1=YC+Y
          CALL PGDRAW(X1,Y1)
          CALL PGMOVE(X2,Y2)
          X2=XC+X
          Y2=YC-Y
          CALL PGDRAW(X2,Y2)
          CALL PGMOVE(X3,Y3)
          X3=XC-X
          Y3=YC+Y
          CALL PGDRAW(X3,Y3)
          CALL PGMOVE(X4,Y4)
          X4=XC-X
          Y4=YC-Y
          CALL PGDRAW(X4,Y4)

        ENDDO

        CALL PGUPDT(2)
        CALL PGUPDT(1)

      ENDIF

      END



*+XRT_CREPROC     Writes processing box into binned dataset
      SUBROUTINE XRT_CREPROC(LOC, VIGCOR, DTCOR, BGND, STATUS)
*    Description :
*      Creates a processing box in a datafile.
*    Bugs :
*    Authors :
*     Richard Saxton     (LTVAD::RDS)
*    History :
*     14-JUN-1989   ORIGINAL     (LTVAD::RDS)
*     6-Mar-1990    Now sets background flag false whatever
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
      CHARACTER*(DAT__SZLOC) LOC                  ! Locator to binned datafile
      LOGICAL VIGCOR                              ! Vignetting correction flag
      LOGICAL DTCOR                               ! Dead time correction flag
      LOGICAL BGND                                ! Background subtraction flag
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ALOC                 ! Locator to ASTERIX box
      CHARACTER*(DAT__SZLOC) PLOC                 ! Locator to processing box
      CHARACTER*(DAT__SZLOC) CLOC                 ! Locator to corrected box
*
      IF (STATUS .NE. SAI__OK) RETURN
*
* Get locator to asterix box
      CALL BDA_LOCAST(LOC, ALOC, STATUS)
*
* Create processing box.
      CALL DAT_NEW(ALOC, 'PROCESSING', 'EXTENSION', 0, 0, STATUS)
      CALL DAT_FIND(ALOC, 'PROCESSING', PLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error creating processing box',STATUS)
         GOTO 999
      ENDIF
*
* Test if background subtraction has been performed
      CALL HDX_PUTL(PLOC, 'BGND_SUBTRACTED', 1, BGND, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing background subtracted flag')
      ENDIF
*
* Create corrections box
      CALL DAT_NEW(PLOC, 'CORRECTED', 'EXTENSION', 0, 0, STATUS)
      CALL DAT_FIND(PLOC, 'CORRECTED', CLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error creating corrections box',STATUS)
         GOTO 999
      ENDIF
*
* Write dead time object
      CALL HDX_PUTL(CLOC, 'Dead_time', 1, DTCOR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing dead_time corrected object')
         GOTO 999
      ENDIF
*
* Write vignetting object
      CALL HDX_PUTL(CLOC, 'VIGNETTING', 1, VIGCOR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing vignetting corrected object')
         GOTO 999
      ENDIF
*
* Annul locators
      CALL DAT_ANNUL(CLOC, STATUS)
      CALL DAT_ANNUL(PLOC, STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRT_CREPROC',STATUS)
      ENDIF
*
      END

*+  XRT_DIROPEN - Open a direct access file on a logical unit
      SUBROUTINE XRT_DIROPEN( FILENAME, ACCESS, BYTES_PER_REC,
     :                        UNIT, STATUS )
*
*  Purpose:
*     Open a direct access file for unformatted read or write with a specified
*     record length in bytes.
*
*  Authors:
*     DJA: David J. Allan (ROSAT)
*     {enter_new_authors_here}
*
*  History:
*     20 Dec 93 : Original (DJA)
*
*  Bugs:
*     None known.
*     {note_new_bugs_here}
*
*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIO_ERR'


*  Arguments Given:
      CHARACTER*(*) FILENAME,ACCESS
      INTEGER BYTES_PER_REC

*  Arguments Returned:
      INTEGER UNIT

*  Status:
      INTEGER STATUS

*  External references :
      LOGICAL CHR_SIMLR

*  Local Variables:
      INTEGER  IERR

      CHARACTER
     :  MACHIN * ( 24 ),       ! Machine name
     :  NODE * ( 20 ),         ! Node name
     :  RELEAS * ( 10 ),       ! Release of operating system
     :  SYSNAM * ( 10 ),       ! Operating system
     :  VERSIO * ( 10 )        ! Sub-version of operating system

      CHARACTER*3 FSTAT
	LOGICAL DECST,VMS,FIRST
        SAVE DECST
        SAVE VMS
         SAVE FIRST

      DATA FIRST/.TRUE./
*-
*.

*    Check for an error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    First time through
      IF ( FIRST ) THEN
        FIRST = .FALSE.

*      Enquire which operating system is being used.
        CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN, STATUS )
        CALL CHR_UCASE( SYSNAM )
        CALL CHR_UCASE( MACHIN )

*      Is it VAX/VMS?
        VMS = INDEX( SYSNAM, 'VMS' ) .NE. 0 .OR.
     :        INDEX( SYSNAM, 'RSX' ) .NE. 0

*      Find if the machine is a DECstation.
        IF ( VMS ) THEN
          DECST = .FALSE.
        ELSE
          DECST = SYSNAM( 1:6 ) .EQ. 'ULTRIX' .OR.
     :            ( SYSNAM( 1:3 ) .EQ. 'OSF' .AND.
     :              MACHIN( 1:4 ) .EQ. 'MIPS' ) .OR.
     :            ( SYSNAM( 1:3 ) .EQ. 'OSF' .AND.
     :              MACHIN( 1:5 ) .EQ. 'ALPHA' )
        END IF

      END IF

*    Get logical unit
      CALL FIO_GUNIT( UNIT, STATUS )

*    Determine STATUS keyword
      IF ( CHR_SIMLR(ACCESS,'READ') ) THEN
        FSTAT = 'OLD'
      ELSE IF ( CHR_SIMLR(ACCESS,'WRITE') ) THEN
        FSTAT = 'NEW'
      ELSE
        CALL MSG_SETC( 'ACC', ACCESS )
        CALL ERR_REP( ' ', 'Unknown file access mode /^ACC', STATUS )
        GOTO 99
      END IF

*    Open the file. On VMS and DECstations the record length is in longwords
      IF ( DECST .OR. VMS ) THEN

*      Open old files on VMS without a record length check.
        IF ( VMS .AND. (FSTAT.EQ.'OLD') ) THEN
          OPEN( UNIT, FILE=FILENAME,ACCESS='DIRECT',FORM='UNFORMATTED',
     :          STATUS=FSTAT, IOSTAT=IERR )
        ELSE
          OPEN( UNIT, FILE=FILENAME,ACCESS='DIRECT',FORM='UNFORMATTED',
     :          RECL=(BYTES_PER_REC+3)/4, STATUS=FSTAT, IOSTAT=IERR )
        END IF

*    Otherwise in bytes
      ELSE
        OPEN( UNIT, FILE=FILENAME, ACCESS='DIRECT', FORM='UNFORMATTED',
     :        RECL=BYTES_PER_REC, STATUS=FSTAT, IOSTAT=IERR )
      END IF

*    Trap error
      IF ( IERR .NE. 0 ) THEN
        CALL FIO_SERR( IERR, STATUS )
      END IF

*    Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'XRT_DIROPEN', STATUS )
      END IF

      END

*+XRT_FWHM  calculates the full width half max of the XRT point spread fn.
      SUBROUTINE XRT_FWHM(FOVR, ENERGY, FWHM)
*    Description :
*      Calculates the fwhm of the point spread function.
*
*   calculates the preliminary gaussian blur-circle as a function of
*   the off-axis distance which was calibrated with simulations.
*   point-spread function is based on a simple gaussian fit, where in
*   in the outer regions of the field of view the hole in the point
*   spread function has been filtered out by a gradient filter.
*   the fwhm of the detector was assumed to be
*   fwhm(d) = 23.3 / sqrt(e) [arcsec]       (e is energy in kev)
*   !!!!  NEW after PSF simulations  !!!
*   fwhm(d) = sqrt(409.65/e + 69.28*e**2.88 + 66.29)
*   the fwhm of the telescope was determined to
*   fwhm(t) = 0.29 * (eps**1.74) [arcsec]   (eps is off-axis angle ')
*
*    History :
*      author : GRH               date: 1987     original
*      update : RDS               date: 10-Dec-1990   Asterix version
*    Type definitions :
      IMPLICIT NONE
*    Import :
      REAL FOVR                ! Off axis angle in arcmins
      REAL ENERGY              ! Energy in keV
*    Import-Export :
*    Export :
      REAL FWHM                ! Full width half maximum of gaussian
*                              ! point spread in arcsecs
*    Local constants :
*    Local variables :
*-
      REAL DETECT              ! fwhm of the detector
      REAL TELESC              ! fwhm of the telescope
*
      REAL TELES
*    Local data :
      DATA TELES/0.29/
*
*  Calculate the telescope contribution
      TELESC = TELES * TELES * (FOVR)**3.48
*
*  Calculate the detector contribution
      DETECT = 409.65/ENERGY + 69.28*ENERGY**2.88 + 66.29
*
*  Calculate the total FWHM of the gaussian approximation to the point
*  spread function
      FWHM   = SQRT(DETECT+TELESC)
*
      END

*+XRT_GETMVR - Gets Master Veto rate data from the EVENTRATES file
      SUBROUTINE XRT_GETMVR(ORIGIN,ROOTNAME, ERLOC, NTIMES,
     &                       TPNTR, MVPNTR, AVMVR, STATUS)
*    Description :
*     Returns an array of Master Veto rates from the eventrate file
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     18 NOV 1991  ORIGINAL
*     23 FEB 1994  Modified to use file and field names:INC_RDF (LTVAD::JKA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*     <global variables held in named COMMON>
*    Import :
      CHARACTER*(*) ORIGIN                  ! Origin of datafiles
      CHARACTER*(*) ROOTNAME                ! Rootname for data files
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      CHARACTER*(DAT__SZLOC) ERLOC          ! Locator to eventrate file
      INTEGER NTIMES                        ! Number of elements in EVR file
      INTEGER TPNTR                         ! Pointer to time array
      INTEGER MVPNTR                        ! Pointer to Master Veto Rate data
      REAL AVMVR                            ! Average MVR in the array
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
      CHARACTER*20 RAT_LOOKUP
        EXTERNAL RAT_LOOKUP
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*20 EXT                      ! filename extension
      CHARACTER*20 COL                      ! HDS column name
      CHARACTER*80 ERFILE                   ! Eventrate filename
      CHARACTER*(DAT__SZLOC) TLOC           ! Locator to times
      CHARACTER*(DAT__SZLOC) ALOC1          ! Locator to MVR data
      REAL SD
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Open eventrate file (subroutine only call for PSPC)
      EXT = RAT_LOOKUP(ORIGIN,'PSPC','EVRATE','EXTNAME')
      ERFILE = ROOTNAME(1:CHR_LEN(ROOTNAME))//EXT
*
      CALL HDS_OPEN(ERFILE, 'READ', ERLOC, STATUS)
*
* If file coundn't be opened return
      IF (STATUS .NE. SAI__OK) THEN
*
         CALL MSG_SETC('ERFILE', ERFILE)
         CALL MSG_PRNT('** Error opening eventrate file ^ERFILE **')
*
      ELSE
*
*   Read time values
         COL = RAT_LOOKUP(ORIGIN,'PSPC','EVRATE','TIME')
         CALL DAT_FIND(ERLOC, COL, TLOC, STATUS)
         CALL DAT_SIZE(TLOC, NTIMES, STATUS)
*
*   Map the time array
         CALL DAT_MAPR(TLOC, 'READ', 1, NTIMES, TPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping eventrate TIME array')
            GOTO 999
         ENDIF
*
*   Map the MVR
         COL = RAT_LOOKUP(ORIGIN,'PSPC','EVRATE','MVRATE')
         CALL DAT_FIND(ERLOC, COL, ALOC1, STATUS)
         CALL DAT_MAPR(ALOC1, 'READ', 1, NTIMES, MVPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping MASTER_VETO_RATE array')
         ELSE
*
*      Find the average MVR value
            CALL ARR_MEANR(NTIMES, %val(MVPNTR), AVMVR, SD, STATUS)
         ENDIF
      ENDIF
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRT_GETMVR',STATUS)
      ENDIF
*
      END

*+ XRT_GETPSF - Gets the radius of the PSF in the PSPC
      SUBROUTINE XRT_GETPSF(PFRAC, ENERGY, OFFAX, RADIUS, STATUS)
*    Description :
*     Calculates the radius in arcminutes of the PSPC PSF. The
*     inputs specify the fraction of enclosed counts which are
*     wanted at a given energy and given off-axis angle.
*    Method :
*     This routine calculates the total radius by adding in
*     quadrature the PSF due to the telescope and that due to
*     the detector. The detector contribution is a function
*     of energy only and has been taken directly from the
*     George and Turner paper. The telescope contribution
*     was calculated by subtracting the detector contribution
*     from a set of measurements of tht total PSF obtained from
*     the US.
*    Authors :
*     Richard Saxton and Ioannis Georgantopolous
*    History :
*     18-Sep-1992
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'           ! STATUS values
*    Global variables :
*    Structure definitions :
*    Status :
      INTEGER STATUS
*    Import :
      REAL PFRAC                  ! The fraction of enclosed cnts wanted
      REAL ENERGY                 ! Photon energy (keV)
      REAL OFFAX                  ! Off-axis angle (arcmins)
*    Export :
      REAL RADIUS                 ! PSF radius (arcmins)
*    Local constants :
*    Local variables :
      INTEGER IOFFAX              ! Closest integer off-axis angle
      LOGICAL INTERP,EINTRP       ! Interpolate the fraction or the energy
      REAL TELPSF                 ! The PSF radius due to the telescope
      REAL DETPSF                 ! The PSF radius due to the detector
      REAL TP1,TP2                ! Telescope radii to interpolate between
      REAL DP1,DP2                ! Detector radii to interpolate between
      REAL RP1,RP2                ! Radii to interpolate between
      INTEGER ELP,FLP             ! Fraction loop and Energy loop variables
*    Local data :
      INCLUDE 'XRTLIB(INC_GETPSF_DATA)'   ! Includes the data values for
*                                 ! ENERGY,FRAC,TELRAD and DETRAD
*                                 ! and the constants MAXENG and MAXFRAC
*-
* Initialise
      INTERP = .FALSE.
      EINTRP = .FALSE.
*
* Calculate the closest integer off-axis angle, but insist that it
* is between 0 and 60
      IF (OFFAX .LT. 0.0) THEN
         CALL MSG_PRNT('XRT_GETPSF: Off axis angle is < 0: using '/
     &                /'angle = 0')
      ENDIF
*
      IOFFAX = MAX( 0, MIN( NINT(OFFAX), 60 ) )
*
* Calculate the telescope PSF radius for this percentage at this off-axis
* angle
      IF (PFRAC .LE. 0.1) THEN
         IF (PFRAC .LT. 0.1) THEN
            CALL MSG_PRNT('XRT_GETPSF: fraction required < 0.1 : '/
     &                /'using fraction = 0.1')
         ENDIF
*
         TELPSF = TELRAD(1,IOFFAX)
*
         FLP = 1
*
      ELSEIF(PFRAC . GE. 0.95) THEN
C         IF (PFRAC .GT. 0.95) THEN
C            CALL MSG_PRNT('XRT_GETPSF: fraction required > 0.95 : '/
C     &                /'using fraction = 0.95')
C         ENDIF
*
         TELPSF = TELRAD(MAXFRAC,IOFFAX)
*
         FLP = MAXFRAC
*
* Interpolate the telescope fraction
      ELSE
*
         INTERP = .TRUE.
*
         DO FLP=1,MAXFRAC
            IF (PFRAC .LT. FRAC(FLP)) GOTO 10
         ENDDO
*
10       CONTINUE
*
         TP1 = TELRAD(FLP-1,IOFFAX)
         TP2 = TELRAD(FLP,IOFFAX)
*
* Linearly interpolate
         TELPSF = TP1 + (TP2-TP1) * (PFRAC-FRAC(FLP-1)) /
     &            (FRAC(FLP) - FRAC(FLP-1))
*
      ENDIF
*
* Calculate the detector contribution
*   We have no information outside the energy range 0.188-1.7 keV
      IF ( ENERGY .LE. DEFENG(1)) THEN
         ELP = 1
      ELSEIF (ENERGY .GT. DEFENG(MAXENG)) THEN
         ELP = MAXENG
      ELSE
*
*   Find the two energies to interpolate between
         EINTRP = .TRUE.
*
         DO ELP=1,MAXENG
            IF (ENERGY .LT. DEFENG(ELP)) GOTO 20
         ENDDO
*
20       CONTINUE
*
      ENDIF
*
*   Interpolate between the two fractions at this energy if necc.
      IF (INTERP) THEN
         RP1 = DETRAD(FLP-1,ELP)
         RP2 = DETRAD(FLP,ELP)
*
*      Linearly interpolate over the fractions
         DP2 = RP1 + (RP2-RP1) * (PFRAC-FRAC(FLP-1)) /
     &            (FRAC(FLP) - FRAC(FLP-1))
      ELSE
         DP2 = DETRAD(FLP,ELP)
      ENDIF
*
*   Interpolate energies if neccessary
      IF (EINTRP) THEN
         IF (INTERP) THEN
            RP1 = DETRAD(FLP-1,ELP-1)
            RP2 = DETRAD(FLP,ELP-1)
*
*         Linearly interpolate over the fractions
            DP1 = RP1 + (RP2-RP1) * (PFRAC-FRAC(FLP-1)) /
     &            (FRAC(FLP) - FRAC(FLP-1))
         ELSE
            DP1 = DETRAD(FLP,ELP-1)
         ENDIF
*
         DETPSF = DP1 + (DP2-DP1) * (ENERGY-DEFENG(ELP-1)) /
     &            (DEFENG(ELP) - DEFENG(ELP-1))
      ELSE
         DETPSF = DP2
      ENDIF
*
* Combine the two fractions
      RADIUS = SQRT ( TELPSF * TELPSF + DETPSF * DETPSF )
*
      END

*+XRT_GETSORT  - Gets sort parameters from a file
      SUBROUTINE XRT_GETSORT(LOCIN, HEAD, STATUS)
*    Description :
*      Gets sorting ranges from an XRT SORT box
*    Environment parameters :
*    Method :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     7-Jun-1990   original
*    16-May-1991   revised to use new spatial sort structure
*    27-Apr-1992   finds if detector is PSPCB or C if not in file
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      CHARACTER*(DAT__SZLOC) LOCIN               ! Locator to input file
*    Import-Export :
      RECORD /CORR/ HEAD
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC,HLOC,SLOC
      CHARACTER*(DAT__SZLOC) TLOC,PLOC,CLOC
      LOGICAL NEW
      INTEGER NTIM                        ! Number of sets of time windows
      INTEGER LP
      INTEGER MDATE                       ! MJD of the observation
      INTEGER MSWITCH                     ! MJD of 26th Jan 1991

*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Get locator to the instrument box
      CALL BDA_LOCINSTR(LOCIN, ILOC, STATUS)
*
* Get detector type i.e. PSPC or HRI
      CALL CMP_GET0C(ILOC, 'DETECTOR', HEAD.DET, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('*Error reading instrument type*')
         GOTO 999
      ENDIF
*
* Sometimes it doesn't say if it is PSPC-B or C, but just says PSPC. In
* this case the only way to tell is from the observation date.
* Before Jan 26th 1991 is 'C' and after is 'B'.
*   Test if the detector string is ok.
      IF ( (INDEX(HEAD.DET, 'HRI') .EQ. 0) .AND.
     &       (INDEX(HEAD.DET, 'PSPCB') .EQ. 0) .AND.
     &           (INDEX(HEAD.DET, 'PSPCC') .EQ. 0) ) THEN
*
*      Get the observation date as an MJD (use MJDs to compare dates)
         CALL BDA_LOCHEAD(LOCIN, HLOC, STATUS)
*
         CALL CMP_GET0I(HLOC, 'BASE_MJD', MDATE, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('** Error reading BASE_MJD **')
            CALL MSG_PRNT('** Can not determine detector type **')
            GOTO 999
         ENDIF
*
*      Convert 26th Jan 1991 to an MJD
         CALL CONV_YMDMJD(1991, 1, 26, MSWITCH)
*
*      Compare the MJD of the observation to the switch over point
         IF (MDATE .LE. MSWITCH) THEN
            HEAD.DET = 'PSPCC'
         ELSE
            HEAD.DET = 'PSPCB'
         ENDIF
*
      ENDIF
*
* Get locator to sort box
      CALL DAT_FIND(ILOC, 'SORT', SLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error getting locator to the SORT box')
         GOTO 999
      ENDIF
*
*   Check if this is the new or old sort box
      CALL DAT_THERE(SLOC, 'SPACE', NEW, STATUS)
*
      IF (NEW) THEN
*
*      Read from the new definition of the SORT box
         CALL XRT_GETSORT_NEWSPACE(SLOC, HEAD, STATUS)
*
      ELSE
*
*      Read from the old definition of the SORT box
         CALL XRT_GETSORT_OLDSPACE(SLOC, HEAD, STATUS)
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Calculate the off axis angle of the collection box
      CALL XRT_OFFAX(1, 1, HEAD)
*
*   Time range
      CALL DAT_FIND(SLOC, 'TIME', TLOC, STATUS)
*
*   Find the number of sets of time ranges
      CALL DAT_SIZE(TLOC, NTIM, STATUS)
*
*   Can't handle more than one set of time ranges at present
      IF (NTIM .GT. 1) THEN
         CALL MSG_PRNT('Can only handle the first set of time ranges '/
     &                /'at present ')
         NTIM = 1
      ENDIF
*
      DO LP=1,NTIM
*
         CALL DAT_CELL(TLOC, 1, LP, CLOC, STATUS)
*
*   Find size of START array - if single element, need to do a GET0R.
         CALL CMP_SIZE(CLOC, 'START', HEAD.NTRANGE, STATUS)
*
         IF (HEAD.NTRANGE .GT. 1) THEN
            CALL CMP_GET1R(CLOC, 'START', MAXRNG, HEAD.TMIN,
     &                                     HEAD.NTRANGE, STATUS)
            CALL CMP_GET1R(CLOC, 'STOP', MAXRNG, HEAD.TMAX,
     &                                     HEAD.NTRANGE, STATUS)
         ELSE
            CALL CMP_GET0R(CLOC, 'START', HEAD.TMIN(1), STATUS)
            CALL CMP_GET0R(CLOC, 'STOP', HEAD.TMAX(1), STATUS)
         ENDIF
*
         CALL DAT_ANNUL(CLOC, STATUS)
      ENDDO
*
      CALL DAT_ANNUL(TLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing time range information from '/
     &                /' the SORT box in the file')
         GOTO 999
      ENDIF
*
* Get energy channel information if this is a PSPC file
      IF (INDEX(HEAD.DET, 'PSPC') .NE. 0) THEN
*
*   Corrected PH channel
         CALL DAT_FIND(SLOC, 'ENERGY', PLOC, STATUS)
         CALL DAT_CELL(PLOC, 1, 1, CLOC, STATUS)
*
         CALL CMP_GET0R(CLOC, 'START', HEAD.PMIN, STATUS)
         CALL CMP_GET0R(CLOC, 'STOP', HEAD.PMAX, STATUS)
*
         CALL DAT_ANNUL(CLOC, STATUS)
         CALL DAT_ANNUL(PLOC, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error accessing corrected PH range '/
     &                /' information from the SORT box in the file')
            GOTO 999
         ENDIF
*
      ELSE
*
*   Read raw pulse height info if HRI
         CALL DAT_FIND(SLOC, 'PH_CHANNEL', PLOC, STATUS)
         CALL DAT_CELL(PLOC, 1, 1, CLOC, STATUS)
*
         CALL CMP_GET0R(CLOC, 'START', HEAD.PMIN, STATUS)
         CALL CMP_GET0R(CLOC, 'STOP', HEAD.PMAX, STATUS)
*
         CALL DAT_ANNUL(CLOC, STATUS)
         CALL DAT_ANNUL(PLOC, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error accessing PH range '/
     &                /' information from the SORT box in the file')
            GOTO 999
         ENDIF
*
      ENDIF
*
* Calculate scale value for pulse heights
C      HEAD.PSCALE = (HEAD.PMAX - HEAD.PMIN + 1) / NBIN
*
      CALL DAT_ANNUL(SLOC, STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error finding ranges from SORT box')
      ENDIF
*
      END

*
******************************************************************
*+XRT_GETSORT_NEWSPACE - reads new style spatial sort info.
      SUBROUTINE XRT_GETSORT_NEWSPACE(SLOC, HEAD, STATUS)
*    Description :
*      Reads spatial sort infotmation from the SORT box. Uses the
*      old style SORT box
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     16-5-1991         original    (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      CHARACTER*(DAT__SZLOC) SLOC       ! Locator to SORT box
*    Import-Export :
      RECORD /CORR/ HEAD
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*(DAT__SZLOC) CLOC                !Locator to spatial cell
      CHARACTER*(DAT__SZLOC) SSLOC               !Locator to spatial array
      INTEGER NSPACE                             !Number of spatial selections
*    Local data :
*
*   Get locator to spatial array
      CALL DAT_FIND(SLOC, 'SPACE', SSLOC, STATUS)
*
*   Find the number of spatial regions
      CALL DAT_SIZE(SSLOC, NSPACE, STATUS)
*
      IF (NSPACE .GT. 1) THEN
         CALL MSG_PRNT('Sorry can only handle one spatial region at '/
     &                /'present - just using the first')
      ENDIF
*
*   Get locator to the first spatial region
      CALL DAT_CELL(SSLOC, 1, 1, CLOC, STATUS)

*   Get the shape of the box
      CALL CMP_GET0C(CLOC, 'SHAPE', HEAD.SHAPE, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining box shape')
         GOTO 999
      ENDIF
*
*   Get the orientation of the box - if an ellipse
      IF (HEAD.SHAPE .EQ. 'E') THEN
         CALL CMP_GET0C(CLOC, 'PHI', HEAD.PHI, STATUS)
*
*      Assume an orientation of zero if there is a problem
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            CALL MSG_PRNT('Assuming ellipse orientation of zero')
            HEAD.PHI=0.0
         ENDIF
      ENDIF
*
*   Get the box centre
      CALL CMP_GET0R(CLOC, 'XCENT', HEAD.XCENT, STATUS)
      CALL CMP_GET0R(CLOC, 'YCENT', HEAD.YCENT, STATUS)
*
*   Get the inner radii
      CALL CMP_GET0R(CLOC, 'XINNER', HEAD.XINNER, STATUS)
      CALL CMP_GET0R(CLOC, 'YINNER', HEAD.YINNER, STATUS)
*
*   Get the outer radii
      CALL CMP_GET0R(CLOC, 'XOUTER', HEAD.XOUTER, STATUS)
      CALL CMP_GET0R(CLOC, 'YOUTER', HEAD.YOUTER, STATUS)
*
*   Annul cell locator
      CALL DAT_ANNUL(CLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading spatial region in SORT box')
         GOTO 999
      ENDIF
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRT_GETSORT_NEWSPACE',STATUS)
      ENDIF
*
      END

*
******************************************************************
*+XRT_GETSORT_OLDSPACE - reads old style spatial sort info.
      SUBROUTINE XRT_GETSORT_OLDSPACE(SLOC, HEAD, STATUS)
*    Description :
*      Reads spatial sort infotmation from the SORT box. Uses the
*      old style SORT box
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     16-5-1991        original   (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      CHARACTER*(DAT__SZLOC)SLOC       ! Locator to SORT box
*    Import-Export :
      RECORD /CORR/ HEAD
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*(DAT__SZLOC) XLOC,YLOC,CLOC
      REAL XMIN(10),XMAX(10),YMIN(10),YMAX(10)
      INTEGER NRANGE,LP
*    Local data :
*
*   Get the shape of the box
      CALL CMP_GET0C(SLOC, 'SHAPE', HEAD.SHAPE, STATUS)
*
*   Get the X axis range
      CALL DAT_FIND(SLOC, 'X', XLOC, STATUS)
*
*   Find the number of X ranges
      CALL DAT_SIZE(XLOC, NRANGE, STATUS)
*
      DO LP=1,NRANGE
         CALL DAT_CELL(XLOC, 1, LP, CLOC, STATUS)
*
         CALL CMP_GET0R(CLOC, 'START', XMIN(LP), STATUS)
         CALL CMP_GET0R(CLOC, 'STOP', XMAX(LP), STATUS)
*
         CALL DAT_ANNUL(CLOC, STATUS)
      ENDDO
*
      CALL DAT_ANNUL(XLOC, STATUS)
*
*   Set X centre value
      HEAD.XCENT = (XMIN(1) + XMAX(NRANGE)) / 2.0
*
*   Y range
      CALL DAT_FIND(SLOC, 'Y', YLOC, STATUS)
*
*   Find the number of X ranges
      CALL DAT_SIZE(YLOC, NRANGE, STATUS)
*
      DO LP=1,NRANGE
         CALL DAT_CELL(YLOC, 1, LP, CLOC, STATUS)
*
         CALL CMP_GET0R(CLOC, 'START', YMIN(LP), STATUS)
         CALL CMP_GET0R(CLOC, 'STOP', YMAX(LP), STATUS)
*
         CALL DAT_ANNUL(CLOC, STATUS)
      ENDDO
*
      CALL DAT_ANNUL(YLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading spatial region in SORT box')
         GOTO 999
      ENDIF
*
*   Set Y centre value
      HEAD.YCENT = (YMIN(1) + YMAX(NRANGE)) / 2.0
*
*   Calculate inner radii
      IF (NRANGE .EQ. 1) THEN
*
         HEAD.XINNER=0.0
         HEAD.YINNER=0.0
*
      ELSEIF (NRANGE .EQ. 2) THEN
*
         HEAD.XINNER = ABS(HEAD.XCENT - XMAX(1))
         HEAD.YINNER = ABS(HEAD.YCENT - YMAX(1))
*
      ELSE
         CALL MSG_PRNT(' Cant understand spatial region in sort box')
         GOTO 999
      ENDIF
*
*   Calculate outer radii
      HEAD.XOUTER = ABS(HEAD.XCENT - XMIN(1))
      HEAD.YOUTER = ABS(HEAD.YCENT - YMIN(1))
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRT_GETSORT_OLDSPACE',STATUS)
      ENDIF
*
      END

*+XRT_GETTYPE	Works out the type of file produced from the axes.
	SUBROUTINE XRT_GETTYPE (NAXES,AXES,TYPE)
* Description :
*       Takes the axes used in the data array of the binned dataset and
*      works out the type of file being produced.
*      This routine expects the type of axis to be referred to by
*      an integer code.
* Authors :
*     Richard Saxton
* History :
*     5 Nov 1988 Original (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Import :
      INTEGER NAXES                  !No of dimensions of output array
      INTEGER AXES(7)                !Code for type of axis in output
                                     !     1: XPIX
                                     !     2: YPIX
                                     !     3: XDET
                                     !     4: YDET
                                     !     5: Time
                                     !     6: Sumsig
                                     !     7: Energy
* Import-Export :
* Export :
      CHARACTER*(*) TYPE             !Type of dataset created
* Local constants :
* Local variables :
*-
* Write dataset title
*
        TYPE='                              '
*
	IF (NAXES.EQ.1) THEN
*
           IF (AXES(1).EQ.5) THEN
               TYPE='TIME_SERIES'
           ELSE IF (AXES(1).EQ.6 .OR. AXES(1).EQ.7) THEN
               TYPE='SPECTRUM'
           ELSE
               TYPE='UNKNOWN'
           ENDIF
*
	ELSE IF (NAXES.EQ.2) THEN
*
           IF (AXES(1).EQ.1 .AND. AXES(2).EQ.2) THEN
               TYPE='IMAGE'
           ELSEIF (AXES(1).EQ.3 .AND. AXES(2).EQ.4) THEN
               TYPE='DETECTOR_IMAGE'
           ELSEIF (AXES(1).EQ.5 .AND. (AXES(2).EQ.6 .OR. AXES(2).EQ.7)
     &                                                       ) THEN
               TYPE='SPECTRUM_SERIES'
           ELSE
               TYPE='UNKNOWN'
           ENDIF
*
	ELSE IF (NAXES.EQ.3) THEN
*
           IF (AXES(1).EQ.1 .AND. AXES(2).EQ.2 .AND. AXES(3).EQ.3) THEN
               TYPE='IMAGE_SERIES'
           ELSEIF( AXES(1).EQ.1 .AND. AXES(2).EQ.2 .AND.
     &            (AXES(3).EQ.4 .OR. AXES(3).EQ.5) )THEN
               TYPE='SPECTRAL_IMAGE'
           ELSE
               TYPE='UNKNOWN'
           ENDIF
*
	ELSE IF (NAXES.EQ.4) THEN
*
           TYPE='SPECIMAGESERIES'
*
        ELSE
*
           TYPE='UNKNOWN'
*
        ENDIF
*
	END

*+  XRT_HSPOT - Checks if photon comes from a hotspot/deadspot
	LOGICAL FUNCTION XRT_HSPOT(HEAD, XEV, YEV)
*    Description :
*     Compares event position with a list of hotspot positions.
*     Currently only works with HRI hotspots/deadspots
*     XRT_HSPOT is true if the pixel is not within a hotspot and false if
*     it is.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     11-Nov-1991  original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)' ! Header variables
*    Import :
      RECORD /XRT_HEAD/ HEAD        ! Header info
      INTEGER XEV,YEV               ! Detector coordinates of event
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LP
      REAL DIST
*    Local data :
*     <any DATA initialisations for local variables>
*-
      XRT_HSPOT = .TRUE.

* Check if this event is within a hotspot radius
      DO LP=1,HEAD.NSPOT
*
         DIST = SQRT( REAL(HEAD.XSPOT(LP) - XEV)**2 +
     &                     REAL(HEAD.YSPOT(LP) - YEV)**2 )
*
         IF (DIST .LE. REAL(HEAD.SPOTRAD(LP))) THEN
            XRT_HSPOT = .FALSE.
            GOTO 999
         ENDIF
*
      ENDDO
*
999   CONTINUE
*
      END



*+XRT_IMVIG  - Finds vignetting correction for XRT images
      SUBROUTINE XRT_IMVIG(HEAD, ELOC, NENERGY, ENERGY, NX, NY, NP,
     &                        EPHBIN, DISPLAY, VCORR, VFLAG, STATUS)
*    Description :
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton   (LTVAD::RDS)
*    History :
*     18-OCT-1992 original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Status :
      INTEGER STATUS
*    Import :
      RECORD /CORR/ HEAD
      CHARACTER*(DAT__SZLOC) ELOC      ! Locator to effective area file
      INTEGER NENERGY
      REAL ENERGY(NENERGY)
      INTEGER NP                       !Number of pulse height bins
      INTEGER NX,NY                    !Image dimensions
      REAL EPHBIN(NP)                  !Energy of each PHA channel
      LOGICAL DISPLAY                  !Display the result on the screen ?
*    Export :
      REAL VCORR(NX,NY,NP)             !Vignetting corrections
      LOGICAL VFLAG                    !Have vignetting corrections been calc'd?
*    Functions :
*    Local constants :
      INTEGER MAXANG
         PARAMETER(MAXANG=60)
*    Local variables :
      CHARACTER*(DAT__SZLOC) OALOC     !Locator to off axis array
*
      INTEGER NEFF                     !No. of eff. area arrays
      REAL ANGLE(MAXANG)               !Off axis angles
      INTEGER EFBPTR                   !Pointer to eff area array
      INTEGER EFDIM(2)                 !Dims of EFBPTR
      LOGICAL HRI,PSPC
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
      VFLAG = .FALSE.

* Determine which detector
      PSPC=((HEAD.DET(:4)).EQ.'PSPC')
      HRI=((HEAD.DET(:3)).EQ.'HRI')
*

      IF (PSPC) THEN

* Read in the array of off-axis angles from the effective areas file
        CALL DAT_FIND(ELOC, 'OFF_ANGLE', OALOC, STATUS)
        CALL DAT_SIZE(OALOC, NEFF, STATUS)
*
        CALL DAT_GET1R(OALOC, MAXANG, ANGLE, NEFF, STATUS)
*
        CALL DAT_ANNUL(OALOC, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT(
     :             'Error reading off axis angles from EFFAR file')
           GOTO 999
        ENDIF
*
* Map an array to hold the relevant effective areas for the energies wanted
        EFDIM(1)=NEFF
        EFDIM(2)=NP
        CALL DYN_MAPR(2,EFDIM,EFBPTR,STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error obtaining dynamic memory')
           GOTO 999
        ENDIF
*
* Produce an array of NEFF*NP eff area values
        CALL XRT_IMVIG_EFF(ELOC, NEFF, NP, EPHBIN, NENERGY,
     &                           ENERGY, %val(EFBPTR), STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
*  Calculate the interpolated effective areas
        CALL XRT_IMVIG_PSPC(HEAD, NEFF, NX, NY, NP, MAXANG, ANGLE,
     &                       %val(EFBPTR), DISPLAY, VCORR, STATUS)
*
        VFLAG=.TRUE.

      ELSEIF (HRI) THEN

        CALL XRT_IMVIG_HRI(HEAD, NX, NY, DISPLAY, VCORR, STATUS)

        VFLAG=.TRUE.

      ENDIF
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRT_IMVIG', STATUS)
      ENDIF
*
      END


*+XRT_IMVIG_PSPC    -   Calculates the vignetting correction for PSPC
      SUBROUTINE XRT_IMVIG_PSPC(HEAD, NEFF, NX, NY, NP, MAXANG,
     &                            ANGLE, BIGEFF, DISPLAY, VCORR, STATUS)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ HEAD
      INTEGER NEFF                        ! No. of off-axis angle arrays
      INTEGER NX,NY                       ! Image dimensions
      INTEGER NP                          ! Number of pulse height bins in file
      INTEGER MAXANG                      ! Max no of eff. area arrays
      REAL ANGLE(MAXANG)                  ! Off axis angles
      REAL BIGEFF(NEFF,NP)                ! Eff areas.
      LOGICAL DISPLAY                     ! Display result on screen ?
*    Import-Export :
      REAL VCORR(NX,NY,NP)                ! Vignetting correction array
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER LP,TOP,BOT
      INTEGER XLP,YLP,PLP
      REAL VTOT
      REAL XCENT,YCENT,XSCALE,YSCALE
      REAL XOFF,YOFF
      REAL FRAC,VSING
*-

      IF (STATUS.NE.SAI__OK) RETURN

* Initialise :
      XCENT = HEAD.XCENT
      YCENT = HEAD.YCENT
      XSCALE = HEAD.XSCALE
      YSCALE = HEAD.YSCALE
*
* Loop over each image pixel
      DO XLP=1,NX
       DO YLP=1,NY
*
*    Calculate off axis angle in arcmins
          XOFF = HEAD.XCENT + (XLP-NX/2.0) * HEAD.XSCALE
          YOFF = HEAD.YCENT + (YLP-NY/2.0) * HEAD.YSCALE
          HEAD.OFFAX = SQRT(XOFF*XOFF + YOFF*YOFF) * 60.0
*
D          WRITE(1,*)HEAD.OFFAX
*
*    Calculate which two effective area off axis angles are closest to
*    the off-axis angle of the data file
          TOP = 0
          DO LP=1,NEFF
*
            IF (HEAD.OFFAX .LT. ANGLE(LP)) THEN
               TOP = LP
               FRAC = (HEAD.OFFAX-ANGLE(LP-1)) / (ANGLE(LP)-ANGLE(LP-1))
               GOTO 100
            ENDIF
*
          ENDDO
*
100       CONTINUE
*
*    Check that the offset was within the effective area calibrations
          IF (TOP .EQ. 0) THEN
             TOP = 14
             FRAC = 0
          ENDIF
*
          IF (TOP .GT. 1) THEN
             BOT = TOP - 1
          ELSE
             BOT = 0
          ENDIF
*
*  Interpolate the vignetting correction for this pixel for each energy
          DO PLP=1,NP
*
             IF (BOT .NE. 0) THEN
                VSING = BIGEFF(TOP,PLP) -
     &               ( BIGEFF(BOT,PLP) - BIGEFF(TOP,PLP) ) * FRAC
             ELSE
                VSING = BIGEFF(TOP,PLP)
             ENDIF
*
*       Calculate the ratio of this effective area to the eff area.
*       which would be seen at the centre
             VCORR(XLP,YLP,PLP) = BIGEFF(1,PLP) / VSING
*
             VTOT = VTOT + VCORR(XLP,YLP,PLP)
*
          ENDDO
*
       ENDDO
      ENDDO
*
      IF (DISPLAY) THEN
         CALL MSG_SETR('VMEAN', VTOT/(NX*NY*NP))
         CALL MSG_PRNT('Mean vignetting correction : ^VMEAN')
      ENDIF
*
      END


*+XRT_IMVIG_HRI    -   Calculates the vignetting correction for HRI
      SUBROUTINE XRT_IMVIG_HRI(HEAD, NX, NY, DISPLAY, VCORR, STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      RECORD /CORR/ HEAD
      INTEGER NX,NY                       ! Image dimensions
      LOGICAL DISPLAY                     ! Display result on screen ?
*    Import-Export :
      REAL VCORR(NX,NY)                   ! Vignetting correction array
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      INTEGER XLP,YLP
      REAL VTOT
      REAL XCENT,YCENT,XSCALE,YSCALE,OFFAX
      REAL XOFF,YOFF
      REAL VIG,QE
*-
      IF (STATUS.NE.SAI__OK) RETURN

* Initialise :
      XCENT = HEAD.XCENT
      YCENT = HEAD.YCENT
      XSCALE = HEAD.XSCALE
      YSCALE = HEAD.YSCALE
*
      VTOT=0.0


* Loop over each image pixel
      DO XLP=1,NX
       DO YLP=1,NY
*
*    Calculate off axis angle in arcmins
          XOFF = XCENT + (REAL(XLP)-REAL(NX)/2.0) * XSCALE
          YOFF = YCENT + (REAL(YLP)-REAL(NY)/2.0) * YSCALE
          OFFAX = SQRT(XOFF*XOFF + YOFF*YOFF) * 60.0

*    get vignetting factor
          CALL XRT_HRIVIG(OFFAX,VIG)

*    Get quantum efficiency factor
          CALL XRT_HRIQE(OFFAX,QE)
*
          VCORR(XLP,YLP)=1.0/(VIG*QE)

          VTOT = VTOT + VCORR(XLP,YLP)
*
*
       ENDDO
      ENDDO
*
      IF (DISPLAY) THEN
         CALL MSG_SETR('VMEAN', VTOT/(NX*NY))
         CALL MSG_PRNT('Mean vignetting correction : ^VMEAN')
      ENDIF

 999  CONTINUE

      END

*
*+ XRT_IMVIG_EFF - sets the effective area big array
      SUBROUTINE XRT_IMVIG_EFF(ELOC, NEFF, NP, EPHBIN,
     &                           NENERGY, ENERGY, BIGEFF, STATUS)
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*     <specification of FORTRAN structures>
*    Local constants :
      INTEGER MAXANG                   !Max. no. of off-axis angles
        PARAMETER (MAXANG=60)
*    Import :
      CHARACTER*(DAT__SZLOC) ELOC      ! Locator to effective area file
      INTEGER NEFF                     !Number of off-axis angles
      INTEGER NP                       !Number of pulse height bins
      REAL EPHBIN(NP)                  !Energy of each PHA channel
      INTEGER NENERGY                  !Number of trial energies
      REAL ENERGY(NENERGY)             !TRIAL ENERGIES
*    Import-Export :
*    Export :
      REAL BIGEFF(NEFF,NP)
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local variables :
      CHARACTER*40 ARRTOP
      CHARACTER*(DAT__SZLOC) TLOC(MAXANG) !Locator to effective area arrays
      INTEGER TPNTR(MAXANG)            !Pointer to effective area arrays
      INTEGER OLP,K,ESTART,PLP,ELP
      CHARACTER*3 CSTR
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Map each effective area array
      DO OLP=1,NEFF
*
         CALL CHR_ITOC(OLP, CSTR, K)
         ARRTOP = 'EFFAR_' // CSTR(1:K)
*
         CALL DAT_FIND(ELOC, ARRTOP, TLOC(OLP), STATUS)
         CALL DAT_MAPR(TLOC(OLP),'READ', 1, NENERGY, TPNTR(OLP), STATUS)
*
      ENDDO
*
* Check status
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading EFFAR array from '/
     &                /'effective area file')
         GOTO 999
      ENDIF
*
      ESTART=1
*
* Loop over each PHA energy
      DO PLP=1,NP
*
*    Which energy is this
         IF (EPHBIN(PLP) .LE. ENERGY(1)) THEN
            ELP=1
         ELSEIF (EPHBIN(PLP) .GE. ENERGY(NENERGY)) THEN
            ELP=NENERGY
         ELSE
*
            DO ELP=ESTART,NENERGY-1
*
               IF ( EPHBIN(PLP) .GE. ENERGY(ELP) .AND.
     &              EPHBIN(PLP) .LE. ENERGY(ELP+1) ) THEN
*
                  ESTART=ELP
                  GOTO 10
               ENDIF
*
            ENDDO
*
10          CONTINUE
*
         ENDIF
*
* Get each effective area value at this energy
         DO OLP=1,NEFF
            CALL ARR_COP1R(1,%val(TPNTR(OLP)+VAL__NBR*(ELP-1)),
     :                     BIGEFF(OLP,PLP),STATUS)
         ENDDO
*
      ENDDO
*
* Unmap each effective area array and unmap
      DO OLP=1,NEFF
         CALL DAT_UNMAP(TLOC(OLP), STATUS)
         CALL DAT_ANNUL(TLOC(OLP), STATUS)
      ENDDO
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRT_IMVIG_EFF',STATUS)
      ENDIF
*
      END


*+XRT_HRIQE  -   gets quantum efficiency factor for HRI
      SUBROUTINE XRT_HRIQE(OFFAX,QE)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
*    Import :
      REAL OFFAX		! off axis angle (arc min)
*    Import-Export :
*    Export :
      REAL QE			! QE factor
*    Status :
      INTEGER STATUS
*    Functions :
*    Local constants :
      REAL OFFMAX
      PARAMETER (OFFMAX=22.0)
      INTEGER NQE
      PARAMETER (NQE=9)
*    Local variables :
      REAL ANGLE(NQE),FACTOR(NQE)
      REAL OFF
      INTEGER I
*    Data :
      DATA ANGLE/1.066666,3.200001,5.333333,7.466668,9.600001,
     :           11.73333,13.86666,16.00000,18.13334/
      DATA FACTOR/1.027776,1.026845,1.013261,1.001316,0.9795799,
     :            0.9516422,0.9307627,0.8949695,0.6786818/
*-

*  limit radial extent to prevent silly values
      OFF=MIN(OFFAX,OFFMAX)
*

*  on-axis
      IF (OFF.LE.ANGLE(1)) THEN
        QE=FACTOR(1)
*  off-axis
      ELSEIF (OFF.LT.ANGLE(NQE)) THEN
        I=1
        DO WHILE (OFF.GT.ANGLE(I))
          I=I+1
        ENDDO
        QE=FACTOR(I-1)+(OFF-ANGLE(I-1))/(ANGLE(I)-ANGLE(I-1))*
     :                                          (FACTOR(I)-FACTOR(I-1))
*  right on edge
      ELSE
        QE=FACTOR(NQE)-(OFF-ANGLE(NQE))*
     :                           (FACTOR(NQE-1)-FACTOR(NQE))/
     :                              (ANGLE(NQE)-ANGLE(NQE-1))

      ENDIF
*

      END


*+XRT_HRIVIG  -   gets vignetting factor for HRI
      SUBROUTINE XRT_HRIVIG(OFFAX,VIG)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
*    Import :
      REAL OFFAX		! off axis angle (arc min)
*    Import-Export :
*    Export :
      REAL VIG			! VIG factor
*    Status :
      INTEGER STATUS
*    Functions :
*    Local constants :
      REAL OFFMAX
      PARAMETER (OFFMAX=22.0)
*    Local variables :
      REAL OFF
*-

*  limit radial extent to prevent silly values
      OFF=MIN(OFFAX,OFFMAX)
*
*    Set vignetting correction according to formula in HRI calibration
*    report  (December 93)
      VIG=1.0 - 1.49E-3*OFF -3.07E-4*OFF**2


      END



*+XRT_LIVEWIND - puts live time data into a single array
      SUBROUTINE XRT_LIVEWIND(NLIVE, ON, OFF, LIV_TIM)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NLIVE                   ! No of live time ranges
      REAL ON(NLIVE),OFF(NLIVE)       ! On and OFF live times
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      DOUBLE PRECISION LIV_TIM(NLIVE*2) ! Single output array
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LP
*-
      DO LP=1,NLIVE
         LIV_TIM(1+(LP-1)*2) = ON(LP)
         LIV_TIM(LP*2) = OFF(LP)
      ENDDO
*
      END


*+XRT_OFFAX - Calculates distance in arcmins from centre of field
      SUBROUTINE XRT_OFFAX(RDIM, RLP, HEAD)
*    Description :
*     <description of what the subroutine does>
*    History :
*     original 22-5-1991  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Import :
      INTEGER RDIM                        ! Number of radial bins in file
      INTEGER RLP                         ! Radial bin wanted
*    Import-Export :
      RECORD /CORR/ HEAD
*    Export :
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      REAL XINC,RXINN,RXOUT               ! Used in calculating off axis
      REAL YINC,RYINN,RYOUT               !    angle.
      REAL OFF1,OFF2,OFF3,OFF4            ! Offset angles at each "corner"
      REAL XPOS1,XPOS2,YPOS1,YPOS2        ! Used in calc of off-axis pos.
*-
*   Calculate the off axis angle of this radial bin
      XINC = (HEAD.XOUTER - HEAD.XINNER) / REAL(RDIM)
      RXINN = HEAD.XINNER + XINC * REAL(RLP-1.0)
      RXOUT = RXINN + XINC
*
      YINC = (HEAD.YOUTER - HEAD.YINNER) / REAL(RDIM)
      RYINN = HEAD.YINNER + YINC * REAL(RLP-1.0)
      RYOUT = RYINN + YINC
*
*   Offset is the average of four offsets
      XPOS1 = HEAD.XCENT + ( RXINN + (RXOUT - RXINN) / 2.0 )
      XPOS2 = HEAD.XCENT - ( RXINN + (RXOUT - RXINN) / 2.0 )
      YPOS1 = HEAD.YCENT + ( RYINN + (RYOUT - RYINN) / 2.0 )
      YPOS2 = HEAD.YCENT - ( RYINN + (RYOUT - RYINN) / 2.0 )
*
      OFF1 = SQRT(XPOS1**2 + HEAD.YCENT**2)
      OFF2 = SQRT(XPOS2**2 + HEAD.YCENT**2)
      OFF3 = SQRT(YPOS1**2 + HEAD.XCENT**2)
      OFF4 = SQRT(YPOS2**2 + HEAD.XCENT**2)
*
*   Off-axis angle is the average offset in arcmins
      HEAD.OFFAX = (OFF1 + OFF2 + OFF3 + OFF4) * 60.0 / 4.0
*
      END

*+XRT_PARTS - Calculates particle countrate at given off axis position
      SUBROUTINE XRT_PARTS(DETB, OLDOFF, NEWOFF, P1, P2, P3, PTOT)
*    Description :
*     Calculates the particle countrate at a given off-axis angle
*    Method :
*     Particle rates are initally calculated at a particular position
*     To calculate the rate at another position in the field of view,
*     the initial rate is multiplied by the ratios of the position
*     dependent factor for the internally produced component.
*     This varies between detectors.
*     The externally produced particles are assumed to have no positional
*     dependence.
*    History :
*     15-Nov-1991   original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Structure definitions :
*    Import :
      LOGICAL DETB                     ! Is the detector PSPCB ?
      REAL OLDOFF                      ! Off-axis angle, that its defined at
      REAL NEWOFF                      ! Off-axis angle wanted
      REAL P1,P2,P3                    ! Countrate in each particle
*                                      ! component at initial position
*    Import-Export :
*    Export :
      REAL PTOT                        ! Total particle count at new position
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      REAL CFACT                       ! Ratios of position dep. factors
*-
* Find difference in positional correction
*   Detector B spatial variation :
      IF (DETB) THEN
*
         CFACT =(1.02E-4 + 3.3E-5 * exp(- (NEWOFF - 20.6)**2 / 12.8 )) /
     &        (1.02E-4 + 3.3E-5 * exp(- (OLDOFF - 20.6)**2 / 12.8))
*
*   Detector C spatial variation :
      ELSE
*
         CFACT = (8.42E-5 + 3.95E-7 * NEWOFF) /
     &                 (8.42E-5 + 3.95E-7 * OLDOFF)
*
      ENDIF
*
      PTOT = P1 * CFACT + P2 + P3
*
      END


*+XRT_PFRAC   -  Calculates the fraction of counts contained in a given radius
      SUBROUTINE XRT_PFRAC(ENERGY, AZ, EL, RAD, FRAC)
*    Description :
*       Calculates the fraction of counts within a source box of
*       radius RAD centred on AZ,EL for an XRT field.
*    History :
*     2-Jan-1991   original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      REAL ENERGY                ! Mean photon energy in keV
      REAL AZ,EL                 ! Position of source box centre (arcmin)
      REAL RAD                   ! Radius of source box (arcmin)
*    Import-Export :
*    Export :
      REAL FRAC                  ! Fraction of counts within this source box
*    Local constants :
*    Local variables :
      REAL FWHM                  ! Full width half max of the PSF (arcsec)
      REAL ALPHA
      REAL OFFAX
*-
*   Calculate off-axis angle
      OFFAX = SQRT(AZ*AZ + EL*EL)
*
*   Calculate PSF FWHM
      CALL XRT_FWHM(OFFAX, ENERGY, FWHM)
*
      ALPHA = 4.0 * LOG(2.0) / ( FWHM * FWHM / 3600.0 )
*
      FRAC = (1.0 - EXP( - ALPHA * RAD * RAD ))

      END

*+XRT_RAWTIM  Reads the SASS selection times from the header file
	SUBROUTINE XRT_RAWTIM(RTNAME, NRAW, RAWTIM, STATUS)
*
* Description :
*        This routine finds the SASS selection times of the observation
*       from the .HDR file.
* Environment parameters :
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     Richard Saxton
* History :
*     23-Apr-1992  Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'   ! Gets the max time ranges constant
* Import :
      CHARACTER*(*) RTNAME            ! Rootname for cal files
* Import-Export :
* Export :
      INTEGER NRAW                    ! Number of time values
      DOUBLE PRECISION RAWTIM(MAXRAN*2) ! SASS selection times
* Status :
      INTEGER STATUS
* Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
* Local constants :
      INTEGER MAXRAW
         PARAMETER (MAXRAW=100)
* Local variables :
      CHARACTER*132 STRING            ! Record from file
      CHARACTER*72 STRING72           ! Character string
      CHARACTER*80 HFILE              ! Header filename
      CHARACTER*80 TIMSTRING          ! Time string
      CHARACTER*8 CH8                 ! Dummy character variable
      INTEGER IERR                    ! OPEN status
      INTEGER LP,I
      INTEGER LEFT
      INTEGER HUNIT                   ! Logical unit for header file
*
      DOUBLE PRECISION BASESC         ! Base s/c clock time
      LOGICAL JUMPOUT
      LOGICAL TFOUND                  ! Were raw times found in file ?
      LOGICAL CLOCK                   ! Was s/c base time found in file ?
*
*-
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN
*
*   Create the .HDR filename
      HFILE = RTNAME(1:CHR_LEN(RTNAME)) // '.hdr'
*
*   Define a logical unit for the header file
      CALL FIO_GUNIT(HUNIT, STATUS)
*
*   Get header file from user
      JUMPOUT = .FALSE.
      DO WHILE (.NOT. JUMPOUT)
*
*      Open header file
         OPEN(HUNIT, FILE=HFILE, STATUS='OLD', IOSTAT=IERR)
*
         IF (IERR .NE. 0) THEN
            CALL MSG_SETC('HFILE', HFILE)
            CALL MSG_PRNT('Error opening header file ^HFILE')
*
*      Ask user for another .HDR file
            CALL USI_CANCL('HDRFIL', STATUS)
*
            CALL USI_DEF0C('HDRFIL', HFILE, STATUS)
*
            CALL USI_GET0C('HDRFIL', HFILE, STATUS)
*
            IF (STATUS .NE. SAI__OK) GOTO 999
*
         ELSE
            JUMPOUT = .TRUE.
         ENDIF
*
      ENDDO
*
* Initialise found flag
      TFOUND = .FALSE.
      CLOCK = .FALSE.
*
* Read header info
      JUMPOUT = .FALSE.
      DO WHILE (.NOT. JUMPOUT)
*
         READ(HUNIT, FMT='(A)', END=100, ERR=100)STRING
*
         IF (INDEX(STRING(1:30), 'TIM_SEL') .NE. 0) THEN
*
*   Set found flag
            TFOUND = .TRUE.
*
            READ(STRING(46:48), FMT='(I3)', END=100)NRAW
*
            IF (NRAW .EQ. 0) THEN
               CALL MSG_PRNT('Error deciphering time range')
               STATUS = SAI__ERROR
               GOTO 999
*
            ELSEIF (NRAW .GT. MAXRAN*2) THEN
               CALL MSG_PRNT('Maximum number of time ranges in'/
     &                        /' header has been exceeded')
               STATUS = SAI__ERROR
               GOTO 999
            ELSE
*
*      Read in times used to select data. Note this assumes that there are
*      three times per line which may not always be true
               DO LP=1,INT(NRAW/3.0)
                  READ(HUNIT, FMT='(A8,A72)', END=100)CH8,STRING72
                  READ(STRING72,*) (RAWTIM((LP-1)*3+I), I=1,3)
               ENDDO
*
               LEFT=NRAW-INT(NRAW/3.0)*3
*
               IF (LEFT .GT. 0) THEN
                  READ(HUNIT, FMT='(A8,A72)', END=100)CH8,STRING72
                  READ(STRING72,*) (RAWTIM((LP-1)*3 + I), I=1,LEFT)
               ENDIF
*
            ENDIF
*
* Get the base clock time
         ELSEIF (INDEX(STRING(1:30), 'OBS_CLOCK') .NE. 0) THEN
*
            READ(HUNIT, FMT='(A8, A)', END=100)CH8,TIMSTRING
            READ(TIMSTRING, *)BASESC
            CLOCK=.TRUE.
*
         ENDIF
*
*  Set early jumpout if all info. has been found
         IF (CLOCK .AND. TFOUND) JUMPOUT = .TRUE.
*
      ENDDO
*
100   CONTINUE
*
*   Have the times been found ?
      IF (.NOT. TFOUND .OR. .NOT. CLOCK) THEN
         CALL MSG_PRNT('** Error reading raw selection times from '/
     &                  /'.hdr file **')
         STATUS = SAI__ERROR
      ELSE
*
*      Convert select times from s/c clock units into offset time
         DO LP=1,NRAW
            RAWTIM(LP) = RAWTIM(LP) - BASESC
         ENDDO

      ENDIF
*
999   CONTINUE
*
      CLOSE(HUNIT)
      CALL FIO_PUNIT(HUNIT, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRT_RAWTIM',STATUS)
      ENDIF
*
      END

*+XRT_RDHEAD   Reads a text header file associated with an XRT observation
	SUBROUTINE XRT_RDHEAD(DISP, SRT, HEAD, VERS, STATUS)
*
* Description :
*        This routine finds the times and status of each observation
*       these are output to the calling routine along with auxilliary
*       information, which will be written into the header variable.
* Environment parameters :
* Method :
* Deficiencies :
*    The initial simulated tape had the components AMP_SEL and CAM_SEL
*    in the header giving the ranges of raw and corrected amplitude
*    channels present in the data. The tape of real data uses
*    RAW_SEL and AMP_SEL. Because of the ambiguity in the AMP_SEL record
*    this routine will no longer work on the simulated data
* Bugs :
* Authors :
*     Richard Saxton
* History :
*     5-Jul-1990   Original
*    29-Feb-1994 Adjusted to eliminate unwanted spaces before details (JKA)
*    22-Apr-1994 Opens the header file as READONLY (jka)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
* Import :
      LOGICAL DISP                    ! Display observation info ?
      RECORD /XRT_SCFDEF/ SRT         ! Sort control structure
* Import-Export :
      RECORD /XRT_HEAD/ HEAD   ! Header structure for subsequent use
*                                     ! and dummy header structure
* Export :
      CHARACTER*(*) VERS              ! SASS version date
* Status :
      INTEGER STATUS
* Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
* Local constants :
* Local variables :
      CHARACTER*100 HFILE             ! HEADER File name
      CHARACTER*132 STRING            ! Record from file
      CHARACTER*8 CH8                 ! Dummy character variable
      CHARACTER*30 RASTRING           ! String for RA
      CHARACTER*30 DECSTRING          ! String for DEC
      CHARACTER*30 ROLLSTRING         ! String for ROLL
      CHARACTER*80 DATE               ! String for date
      CHARACTER*50 PIXSTRING          ! String for pixel size
      CHARACTER*3 MONTH               ! Month as a character string
      CHARACTER*80 LABSTRING          ! String for labels
      CHARACTER*80 TIME               ! Time string
      CHARACTER*80 TIMSTRING          ! Time string
      CHARACTER*80 AMPSTRING
      CHARACTER*72 STRING72
      CHARACTER*100 STRING100
      CHARACTER*1 SIGN
      REAL XSIZE,YSIZE
      INTEGER IERR                    ! IOSTAT variable
      INTEGER HRS,MIN,DEG,ISEC
      DOUBLE PRECISION SC_END         ! Final spacecraft time
      REAL SEC
      DOUBLE PRECISION ONOFF(MAXRAN*2)! Time ranges used to select data
      DOUBLE PRECISION  END_MJD       ! MJD of end time
      INTEGER LP,K,K2,I
      INTEGER LEFT,NT
      INTEGER HUNIT                   ! Logical unit for header file
      INTEGER YEAR,IMONTH,DAY         ! Start date
      INTEGER IMJD1                   ! MJD of Start date
      INTEGER IMJD2                   ! MJD of end date
      INTEGER KR

      DOUBLE PRECISION MJDBIT         ! Fraction of day
      REAL R1,R2                      ! Real variables
*
      LOGICAL ISMNUX,ISMNUY,DET,TARGET
      LOGICAL OBSERV,LRA,LDEC,LROLL,LDATE
      LOGICAL UT,TLEN,ARANGE,CRANGE,FILT,CLOCK
      LOGICAL PIXRAN,DETRAN,EVMAP,PIXSIZ
      LOGICAL JUMPOUT
*
*-
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN
*
* Open header file
      HFILE=SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))//'.hdr'
*
      CALL FIO_GUNIT(HUNIT, STATUS)
*
      OPEN(HUNIT, READONLY, FILE=HFILE, STATUS='OLD', IOSTAT=IERR)
*
      IF (IERR .NE. 0) THEN
         CALL MSG_SETC('HFILE', HFILE)
         CALL MSG_PRNT('Error opening header file ^HFILE')
         STATUS=SAI__ERROR
         GOTO 999
      ENDIF
*
* Read header info
      JUMPOUT = .FALSE.
      DO WHILE (.NOT. JUMPOUT)
*
         READ(HUNIT, FMT='(A)', END=100, ERR=100)STRING
*
         IF (INDEX(STRING(1:30), 'NO_SM_X') .NE. 0) THEN
            READ(HUNIT, FMT='(A8, I)', END=100)CH8, HEAD.ISMNUX
            ISMNUX=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'NO_SM_Y') .NE. 0) THEN
            READ(HUNIT, FMT='(A8, I)', END=100)CH8, HEAD.ISMNUY
            ISMNUY=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'NAXIS2') .NE. 0) THEN
            READ(STRING(15:50), *)HEAD.IEVTNU
C This part added for completeness
         ELSEIF (INDEX(STRING(1:30), 'MISSION_ID') .NE. 0) THEN
            READ(HUNIT, FMT='(A8,1X, A)', END=100)CH8, HEAD.OBSERVATORY
         ELSEIF (INDEX(STRING(1:30), 'TELESCOPE_ID') .NE. 0) THEN
            READ(HUNIT, FMT='(A8,1X, A)', END=100)CH8, HEAD.INSTRUMENT
C ---
         ELSEIF (INDEX(STRING(1:30), 'DETECTOR_ID') .NE. 0) THEN
            READ(HUNIT, FMT='(A8,1X, A)', END=100)CH8, HEAD.DETECTOR
            DET=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'FILTER_ID') .NE. 0) THEN
            READ(HUNIT, FMT='(A8,1X, A)', END=100)CH8, HEAD.FILTER
            FILT=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'TARGET_ID') .NE. 0) THEN
            READ(HUNIT, FMT='(A8,1X, A)', END=100)CH8, HEAD.TARGET
            TARGET=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'SKY_PIX_SIZE') .NE. 0) THEN
            READ(HUNIT, FMT='(A)', END=100)PIXSTRING
            READ(PIXSTRING(8:50), *)HEAD.PIXEL
            PIXSIZ=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'OBS_ID') .NE. 0) THEN
            READ(HUNIT, FMT='(A8, A)', END=100)CH8, HEAD.OBSERVER
            OBSERV=.TRUE.
*
*    REM: Files from AO-1 onwards use LAT and LONG for DEC and RA
         ELSEIF (INDEX(STRING(1:30), 'POINT_RA') .NE. 0 .OR.
     &           INDEX(STRING(1:30), 'POINT_LONG') .NE. 0 ) THEN
            READ(HUNIT, FMT='(A)', END=100)RASTRING
            K=INDEX(RASTRING(8:30), 'H') + 7
            READ(RASTRING(K-2:K+7),FMT='(I2,X,I2,X,F4.1)')HRS,MIN,SEC
            HEAD.AXIS_RA = HRS*15.0 + MIN*15.0/60.0 + SEC*15.0/3600.
            LRA=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'POINT_DEC') .NE. 0 .OR.
     &           INDEX(STRING(1:30), 'POINT_LAT') .NE. 0) THEN
            READ(HUNIT, FMT='(A)', END=100)DECSTRING
            K=INDEX(DECSTRING, 'D')
            READ(DECSTRING(K-3:K+5),FMT='(A1,I2,X,I2,X,I2)')
     &                                            SIGN,DEG,MIN,ISEC
            HEAD.AXIS_DEC = DEG + MIN/60.0 + ISEC/3600.
            IF (SIGN .EQ. '-') HEAD.AXIS_DEC = -HEAD.AXIS_DEC
            LDEC=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'XPIX_TO_NORTH') .NE. 0) THEN
            READ(HUNIT, FMT='(A8, A)', END=100)CH8, ROLLSTRING
            KR=INDEX(ROLLSTRING, 'D')
*
* Read roll angle in degrees
            IF (KR . EQ. 0) THEN
               READ(ROLLSTRING, *)HEAD.ROLLCI
            ELSE
               READ(ROLLSTRING(1:KR-1), *)HEAD.ROLLCI
            ENDIF
*
*      Convert roll angle to Asterx standard
            HEAD.ROLLCI = 90.0 - HEAD.ROLLCI
            LROLL=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'OBS_TITLE') .NE. 0) THEN
            READ(HUNIT, FMT='(A8, A80)', END=100)CH8, HEAD.TITLE
            OBSERV=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'OBS_DATE') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)DATE
*
*       Convert first date in date string to an MJD
            K=INDEX(DATE, '-')
            READ(DATE(K+5:K+8), FMT='(I4)')YEAR
*
*       The year must be in the form 1990 - so add 1900 if 90.
            IF (YEAR .GT. 3000) YEAR = YEAR/100. + 1900
*
            MONTH=DATE(K+1:K+3)
            CALL CONV_MONTH(MONTH,IMONTH,STATUS)
*
            IF (STATUS .EQ. SAI__OK) THEN
               READ(DATE(K-2:K-1), FMT='(I2)')DAY
               CALL CONV_YMDMJD(YEAR, IMONTH, DAY, IMJD1)
               LDATE=.TRUE.
            ELSE
               CALL MSG_PRNT('*Couldnt understand month string*')
               LDATE=.FALSE.
            ENDIF
*
*       Convert second date in date string to an MJD
            K2=INDEX(DATE(K+8:80), '-') + K+7
            READ(DATE(K2+5:K2+8), FMT='(I4)')YEAR

*       The year must be in the form 1990 - so add 1900 if 90.
            IF (YEAR .GT. 3000) YEAR = YEAR/100. + 1900
*
            MONTH=DATE(K2+1:K2+3)
            CALL CONV_MONTH(MONTH,IMONTH,STATUS)
*
            IF (STATUS .EQ. SAI__OK) THEN
               READ(DATE(K2-2:K2-1), FMT='(I2)')DAY
               CALL CONV_YMDMJD(YEAR, IMONTH, DAY, IMJD2)
               LDATE=.TRUE.
            ELSE
               CALL MSG_PRNT('*Couldnt understand month string*')
               LDATE=.FALSE.
            ENDIF
*
         ELSEIF (INDEX(STRING(1:30), 'OBS_UT') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)TIME
            K=INDEX(TIME, ':')
            READ(TIME(K-2:K-1), FMT='(I2)')HRS
            READ(TIME(K+1:K+2), FMT='(I2)')MIN
            READ(TIME(K+4:K+12), *)SEC
            MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
            HEAD.BASE_MJD=DBLE(IMJD1) + MJDBIT/86400.0
            UT=.TRUE.
*
*      Calculate the end UT time
            K2=INDEX(TIME(K+12:80), ':') + K+11
            READ(TIME(K2-2:K2-1), FMT='(I2)')HRS
            READ(TIME(K2+1:K2+2), FMT='(I2)')MIN
            READ(TIME(K2+4:K2+12), *)SEC
            MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
            END_MJD=DBLE(IMJD2) + MJDBIT/86400.0
            HEAD.END_MJD = END_MJD
*
         ELSEIF (INDEX(STRING(1:30), 'OBS_CLOCK') .NE. 0) THEN
            READ(HUNIT, FMT='(A8, A)', END=100)CH8,TIMSTRING
            READ(TIMSTRING, *)HEAD.BASE_SCTIME,SC_END
            CLOCK=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'TLABL') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)LABSTRING
            IF (INDEX(LABSTRING, 'XPIX') .NE. 0) THEN
               HEAD.XPUNITS=LABSTRING(26:36)
            ELSEIF (INDEX(LABSTRING, 'YPIX') .NE. 0) THEN
               HEAD.YPUNITS=LABSTRING(26:36)
            ENDIF
         ELSEIF (INDEX(STRING(1:30), 'RAW_SEL') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)AMPSTRING
            READ(AMPSTRING(8:80), *)R1,R2
            HEAD.ASTART=INT(R1)
            HEAD.AEND=INT(R2)
            ARANGE=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'AMP_SEL') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)AMPSTRING
            READ(AMPSTRING(8:80), *)R1,R2
            HEAD.CSTART=INT(R1)
            HEAD.CEND=INT(R2)
            CRANGE=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'TIM_SEL') .NE. 0) THEN
            READ(STRING(46:48), FMT='(I3)', END=100)NT
*
            IF (NT .EQ. 0) THEN
               CALL MSG_PRNT('Error deciphering time range')
               TLEN=.FALSE.
*
            ELSEIF (NT .GT. MAXRAN*2) THEN
               CALL MSG_PRNT('Maximum number of time ranges in'/
     &                        /' header has been exceeded')
               TLEN=.FALSE.
            ELSE
*
*      Read in times used to select data. Note this assumes that there are
*      three times per line which may not always be true
              DO LP=1,INT(NT/3.0)
                 READ(HUNIT, FMT='(A8,A72)', END=100)CH8,STRING72
                 READ(STRING72,*) (ONOFF((LP-1)*3+I), I=1,3)
              ENDDO
*
              LEFT=NT-INT(NT/3.0)*3
*
              IF (LEFT .GT. 0) THEN
                 READ(HUNIT, FMT='(A8,A72)', END=100)CH8,STRING72
                 READ(STRING72,*) (ONOFF((LP-1)*3 + I), I=1,LEFT)
              ENDIF
*
              HEAD.NTRANGE = NT / 2
*
              DO LP=1,HEAD.NTRANGE
                 HEAD.TSTART(LP)=ONOFF((LP-1)*2+1) - HEAD.BASE_SCTIME
                 HEAD.TEND(LP)=ONOFF(LP*2) - HEAD.BASE_SCTIME
              ENDDO
              TLEN=.TRUE.

            ENDIF
*
         ELSEIF (INDEX(STRING(1:30), 'SKY_FIELD') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)AMPSTRING
            READ(AMPSTRING(8:80), *)HEAD.XSTART,HEAD.YSTART,HEAD.XEND,
     &                                                      HEAD.YEND
            HEAD.IFDSZX = HEAD.XEND - HEAD.XSTART
            HEAD.IFDSZY = HEAD.YEND - HEAD.YSTART

            PIXRAN=.TRUE.
*
*   Get sky pixel centre
         ELSEIF (INDEX(STRING(1:30), 'SKY_CEN_X') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)AMPSTRING
            READ(AMPSTRING(8:80), *)HEAD.SKYCX
*
         ELSEIF (INDEX(STRING(1:30), 'SKY_CEN_Y') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)AMPSTRING
            READ(AMPSTRING(8:80), *)HEAD.SKYCY
*
         ELSEIF (INDEX(STRING(1:30), 'DET_FIELD') .NE. 0) THEN
            READ(HUNIT, FMT='(A80)', END=100)AMPSTRING
            READ(AMPSTRING(8:80), *)HEAD.XDSTART,HEAD.YDSTART,
     &                                          HEAD.XDEND,HEAD.YDEND
            DETRAN=.TRUE.
         ELSEIF (INDEX(STRING(1:30), 'OBS_PROC_DATIM') .NE. 0) THEN
            READ(HUNIT, FMT='(7X,A30)', END=100)VERS
            HEAD.SASS_DATE = VERS(1:CHR_LEN(VERS))
         ELSEIF (INDEX(STRING(1:30), 'START_SM') .NE. 0) THEN
*
*  Read in small map pointers
            HEAD.ISMTNU=HEAD.ISMNUX*HEAD.ISMNUY
            R1 = HEAD.ISMTNU / 7.0
*
            DO LP=1,INT(R1)
               READ(HUNIT, FMT='(A100)', END=100)STRING100
               READ(STRING100(8:100), *)( HEAD.EVSTART(I),
     &                  I = (1+(LP-1)*7), (LP*7) )
            ENDDO

            IF (MOD(R1,1.0) .NE. 0.0) THEN
               READ(HUNIT, FMT='(A100)', END=100)STRING100
               READ(STRING100(8:100), *)( HEAD.EVSTART(I),
     &                  I = (1+(LP-1)*7), HEAD.ISMTNU )
            ENDIF

            EVMAP=.TRUE.
*
         ENDIF
*
        ENDDO
*
100     CONTINUE
*
* Check all info was obtained from header
        IF (.NOT. ISMNUX .OR. .NOT. ISMNUY) THEN
           CALL MSG_PRNT('Couldnt find number of small maps in header')
           STATUS=SAI__ERROR
        ELSEIF (.NOT. DET) THEN
           CALL MSG_PRNT('Couldnt find detector name in header')
           STATUS=SAI__ERROR
        ELSEIF (.NOT. LRA .OR. .NOT. LDEC .OR. .NOT. LROLL) THEN
           CALL MSG_PRNT('Couldnt find all attitude info. in header')
           STATUS=SAI__ERROR
        ELSEIF (.NOT. LDATE .OR. .NOT. UT ) THEN
           CALL MSG_PRNT('Couldnt find start time info. in header')
           STATUS=SAI__ERROR
        ELSEIF (.NOT. TLEN .OR. .NOT. CLOCK ) THEN
           CALL MSG_PRNT('Couldnt find time range in header')
           STATUS=SAI__ERROR
        ELSEIF (.NOT. ARANGE .OR. .NOT. CRANGE) THEN
           CALL MSG_PRNT('Couldnt find amp channel range in header')
           STATUS=SAI__ERROR
        ELSEIF (.NOT. PIXRAN .OR. .NOT. DETRAN) THEN
           CALL MSG_PRNT('Couldnt find X, Y range in header')
           STATUS=SAI__ERROR
        ELSEIF (.NOT. PIXSIZ) THEN
           CALL MSG_PRNT('Couldnt find pixel size in header')
           CALL MSG_PRNT('assuming raw pixel width is 1 arcsec')
           HEAD.PIXEL=1.0
        ELSEIF (.NOT. EVMAP) THEN
           CALL MSG_PRNT('Couldnt find event map index in header')
           STATUS=SAI__ERROR
        ENDIF
*
* Calculate the conversion factor between spacecraft time and UT
        HEAD.SCCONV = (END_MJD - HEAD.BASE_MJD) * 86400. /
     &                            (SC_END - HEAD.BASE_SCTIME)
* Create array of small map start positions
        XSIZE = HEAD.IFDSZX / REAL(HEAD.ISMNUX)
        YSIZE = HEAD.IFDSZY / REAL(HEAD.ISMNUY)

        DO LP=1,HEAD.ISMTNU
           HEAD.XSMAP(LP) = HEAD.XSTART + MOD((LP-1), HEAD.ISMNUX)
     &                                                      * XSIZE
           HEAD.YSMAP(LP) = HEAD.YSTART + INT((LP-1) / HEAD.ISMNUY)
     &                                                      * YSIZE
        ENDDO
*
C fill in target if blank
        IF (HEAD.TARGET.EQ.' '.AND.OBSERV) THEN
           HEAD.TARGET = HEAD.TITLE
        ENDIF

* Display info about this observation
        IF (DISP) THEN

           CALL MSG_PRNT(' ')
           CALL MSG_PRNT(' OBSERVATION    DATA-STARTS     DATA-ENDS  '/
     &        /' EVENTS   FILT_STAT')
*
           WRITE(*,1000)HEAD.TARGET(1:10),HEAD.TSTART(1),
     &        HEAD.TEND(HEAD.NTRANGE),HEAD.IEVTNU,HEAD.FILTER(1:10)

1000       FORMAT(X,A10,4X,F10.1,6X,F10.1,3X,I7,4X,A10)
*
        ENDIF
*
999     CONTINUE
*
        CALL FIO_PUNIT(HUNIT, STATUS)
        CLOSE(HUNIT)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ','from XRT_RDHEAD',STATUS)
        ENDIF
*
        END

*+ XRT_RDHOTSPOT - Reads HOTSPOT file
      SUBROUTINE XRT_RDHOTSPOT(RTNAME, HEAD, STATUS)
*    Description :
*     Reads hotspot file into a header structure
*    Method :
*    Authors :
*     author (institution::username)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Import :
      CHARACTER*(*) RTNAME                     ! Rootname of data files
*    Import-Export :
      RECORD /XRT_HEAD/ HEAD
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*80 FNAME
      CHARACTER*(DAT__SZLOC) HLOC
      CHARACTER*20 EXT,COL
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Just return if PSPC data
      IF (INDEX(HEAD.DETECTOR, 'HRI') .EQ. 0) GOTO 999
*
* Attempt to open the hotmap file
      CALL RAT_HDLOOKUP(HEAD,'HOTSPOT','EXTNAME',EXT,STATUS)
      FNAME = RTNAME(1:CHR_LEN(RTNAME)) // EXT
*
      CALL HDS_OPEN(FNAME, 'READ', HLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC('HOT', FNAME)
         CALL MSG_PRNT('Warning: cannot open hotspot file ^HOT')
         CALL MSG_PRNT('         will assume no hotspots')
         HEAD.NSPOT=0
         CALL ERR_ANNUL(STATUS)
         GOTO 999
      ENDIF
*
* Read in arrays
      CALL RAT_HDLOOKUP(HEAD,'HOTSPOT','XSPOT',COL,STATUS)
      CALL CMP_GET1I(HLOC,COL,MAXSPOT,HEAD.XSPOT,HEAD.NSPOT, STATUS)

      CALL RAT_HDLOOKUP(HEAD,'HOTSPOT','YSPOT',COL,STATUS)
      CALL CMP_GET1I(HLOC,COL,MAXSPOT,HEAD.YSPOT,HEAD.NSPOT, STATUS)

      CALL RAT_HDLOOKUP(HEAD,'HOTSPOT','SPOTRAD',COL,STATUS)
      CALL CMP_GET1I(HLOC,COL,MAXSPOT,HEAD.SPOTRAD,HEAD.NSPOT, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Cannot find arrays in HOTSPOT file')
         CALL MSG_PRNT('Will assume there are no hotspots')
         HEAD.NSPOT=0
         CALL ERR_ANNUL(STATUS)
      ENDIF
*
999   CONTINUE
*
      END

*+XRT_RDRESPHEAD    Translates FITS header into energy arrays
      SUBROUTINE XRT_RDRESPHEAD(MAXHEAD, FHEAD, NHEAD, NPHA,
     &           NENERGY, EBINW, ENERGY_BOUNDS, EPHA_BOUNDS, STATUS)
*    Description :
*    Bugs :
*    Authors :
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Import :
      INTEGER MAXHEAD                           !Max allowed no. of head recs
      CHARACTER*90 FHEAD(MAXHEAD)               !Array of header records
      INTEGER NHEAD                             !Number of header records
      INTEGER NPHA,NENERGY                      !PH and energy dimensions
*    Export :
      REAL EBINW(NENERGY)                       !Bin widths of energy
      REAL ENERGY_BOUNDS(NENERGY+1)             !Boundaries of energy bins
      REAL EPHA_BOUNDS(NPHA)                  !Equivalent energies of the
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER COUNT                             !Count variable
      LOGICAL JUMPOUT
      INTEGER ECLP,I,LEFT
      LOGICAL LEC,LEN,LEBIN                     !Were values found in header ?
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Loop over each header record
      COUNT=0
      JUMPOUT=.FALSE.
*
      DO WHILE (.NOT. JUMPOUT)
*
         COUNT=COUNT+1
*
*   Check for energy of channel record
         IF (INDEX (FHEAD(COUNT), 'ENERGY_OF_CHAN') .NE. 0) THEN
*
*     Read in the various energies - note this relies on their being 5
*     records per column.
            DO ECLP=1,INT(NPHA/5.0)

               COUNT=COUNT+1

               READ(FHEAD(COUNT)(10:90), *)
     &                        (EPHA_BOUNDS((ECLP-1)*5+I), I=1,5)

            ENDDO
*
            LEFT=NPHA-INT(NPHA/5.0)*5
*
            IF (LEFT .GT. 0) THEN

               COUNT=COUNT+1

               READ(FHEAD(COUNT)(10:90), *)
     &                        (EPHA_BOUNDS((ECLP-1)*5+I), I=1,LEFT)

            ENDIF
*
            LEC=.TRUE.
*
*   Check for energy value record
         ELSEIF (INDEX (FHEAD(COUNT), 'ENERGY') .NE. 0) THEN
*
*     Read in the various energies - note this relies on their being 5
*     records per column.
            DO ECLP=1,INT(NENERGY/5.0)

               COUNT=COUNT+1

               READ(FHEAD(COUNT)(10:90), *)
     &                        (ENERGY_BOUNDS((ECLP-1)*5+I), I=1,5)

            ENDDO
*
            LEFT=NENERGY-INT(NENERGY/5.0)*5
*
            IF (LEFT .GT. 0) THEN

               COUNT=COUNT+1

               READ(FHEAD(COUNT)(10:90), *)
     &                        (ENERGY_BOUNDS((ECLP-1)*5+I), I=1,LEFT)

            ENDIF
*
            LEN=.TRUE.

*     Read in the various energy bin widths - note this relies on their being 5
*     records per column.
         ELSEIF (INDEX (FHEAD(COUNT), 'E_BIN_WIDTH') .NE. 0) THEN

            DO ECLP=1,INT(NENERGY/5.0)

               COUNT=COUNT+1

               READ(FHEAD(COUNT)(10:90), *)
     &                        (EBINW((ECLP-1)*5+I), I=1,5)

            ENDDO
*
            LEFT=NENERGY-INT(NENERGY/5.0)*5
*
            IF (LEFT .GT. 0) THEN

               COUNT=COUNT+1

               READ(FHEAD(COUNT)(10:90), *)
     &                        (EBINW((ECLP-1)*5+I), I=1,LEFT)

            ENDIF
*
            LEBIN=.TRUE.

         ENDIF
*
         IF (COUNT .GE. NHEAD) JUMPOUT=.TRUE.
*
      ENDDO
*
* Check all items were present
      IF (.NOT. LEC .OR. .NOT. LEN .OR. .NOT. LEBIN) THEN
         CALL MSG_PRNT('Error reading FITS header records from '/
     &                /'detector response file')
         STATUS=SAI__ERROR
*
      ELSE
*
*   Calculate the energy boundaries of each PH chan
c         EPHA_BOUNDS(NPHA+1) = EPHA_BOUNDS(NPHA) +
c     &              (EPHA_BOUNDS(NPHA) - EPHA_BOUNDS(NPHA-1)) / 2.0
*
c         DO ECLP=NPHA,2,-1
c            EPHA_BOUNDS(ECLP) = (EPHA_BOUNDS(ECLP)+EPHA_BOUNDS(ECLP-1))
c     &                             / 2.0
c         ENDDO
*
c         EPHA_BOUNDS(1) = EPHA_BOUNDS(2) - 2.0 *
c     &              (EPHA_BOUNDS(2) - EPHA_BOUNDS(1))
*
*   Calculate the boundaries of the energy values
         ENERGY_BOUNDS(NENERGY+1) = ENERGY_BOUNDS(NENERGY) +
     &                                         EBINW(NENERGY) / 2.0
*
*     Discontinuities in the spectrum are represented by a bin width
*     of zero in the raw data, so need to ensure that the energy bins
*     we produce have a finite width. NB: there should never be a case
*     where two side by side bins have zero width (hopefully).

         DO ECLP=NENERGY-1,1,-1
            IF (EBINW(ECLP) .LT. 1.0E-10) THEN
*
*     Temporary fiddle !
               ENERGY_BOUNDS(ECLP+1) = ENERGY_BOUNDS(ECLP) + 1.0E-6
            ELSE
               ENERGY_BOUNDS(ECLP+1)=ENERGY_BOUNDS(ECLP)+EBINW(ECLP)/2.0
            ENDIF
         ENDDO
*
         ENERGY_BOUNDS(1)=ENERGY_BOUNDS(1)-EBINW(1)/2.0

      ENDIF
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRT_RDRESPHEAD', STATUS)
      ENDIF
*
      END


*+XRT_TIMSET - Returns a series of times which overlap between 2 series
      SUBROUTINE XRT_TIMSET(N1DIM,N1,INT1,N2DIM,N2,INT2,
     &                                MAXOUT,NOUT,INTOUT,OVRLAP)
*    Description :
*     Two series of disjoint intervals are described by the increasing
*     DOUBLE PRECISION arrays INT1(N1) and INT2(N2).
*     N1 and N2 are even INTEGERS.
*     This subroutine produces a third series, containing the overlapping
*     times.
*     This is a modification of UTIL_TIMSET which allows a section of
*     two arrays to be merged.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*     Richard Saxton (LTVAD::RDS)
*    History :
*     1 Nov 84:  original - util_ovrlap (BHVAD::JCMP)
*    15 Nov 91:  modified.
*    25 Feb 92:  changed UTIL_TIMSET to allow sections of arrays to be merged
*    26 Apr 92:  changed from XRTHK_TIMSET to XRT_TIMSET
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	INTEGER N1DIM			! size of interval array 1
        INTEGER N1                      ! elements to merge
	DOUBLE PRECISION INT1(N1DIM)   	! interval array 1
	INTEGER N2DIM			! size of interval array 2
	INTEGER N2			! elements to merge
	DOUBLE PRECISION INT2(N2DIM)	! interval array 2
        INTEGER MAXOUT                  ! Max no. of output vals - total
*    Import-Export :
*    Export :
        INTEGER NOUT                    ! No of output pairs
        DOUBLE PRECISION INTOUT(MAXOUT) ! Intersecting series
	DOUBLE PRECISION OVRLAP			! total overlap
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
	INTEGER I,J

	DOUBLE PRECISION TI,TIP1			! ith interval
	DOUBLE PRECISION TJ,TJP1			! jth interval
*    Internal References :
*    Local data :
*-

        NOUT = 0
	I=1
	J=1
	OVRLAP=0.
10	TI=INT1(I)
	TIP1=INT1(I+1)
	TJ=INT2(J)
	TJP1=INT2(J+1)
	IF(TIP1.LE.TJP1) THEN
	   IF(TI.GE.TJ) THEN
* INT1 < INT2
              NOUT = NOUT + 1
              INTOUT(1+(NOUT-1)*2) = TI
              INTOUT(NOUT*2) = TIP1
*
	      OVRLAP=OVRLAP+TIP1-TI
	   ELSEIF(TIP1.GT.TJ) THEN
*
              NOUT = NOUT + 1
              INTOUT(1+(NOUT-1)*2) = TJ
              INTOUT(NOUT*2) = TIP1
*
* non-empty intersection
	      OVRLAP=OVRLAP+TIP1-TJ
	   ENDIF
*	  Next I
	   I=I+2
	   IF(I.LE.N1) THEN
	      GOTO 10
	   ELSE
	      GOTO 90
	   ENDIF
	ELSE
	   IF(TI.LT.TJ) THEN
*	     INT2 < INT1
*
              NOUT = NOUT + 1
              INTOUT(1+(NOUT-1)*2) = TJ
              INTOUT(NOUT*2) = TJP1
*
	      OVRLAP=OVRLAP+TJP1-TJ
	   ELSEIF(TI.LT.TJP1) THEN
*	     non-empty intersection
*
              NOUT = NOUT + 1
              INTOUT(1+(NOUT-1)*2) = TI
              INTOUT(NOUT*2) = TJP1
*
	      OVRLAP=OVRLAP+TJP1-TI
	   ENDIF
*	  Next J
	   J=J+2
	   IF(J.LE.N2) THEN
	      GOTO 10
	   ELSE
	      GOTO 90
	   ENDIF
	ENDIF

* End of one or other interval array
90	CONTINUE

	END

*+XRT_VIGNET    Finds vignetting correction for XRT datafiles
      SUBROUTINE XRT_VIGNET(HEAD, ELOC, NENERGY, ENERGY, MEAN_EN,
     &                     DISPLAY, NP, VCORR, VSING, VFLAG, STATUS)
*    Description :
*     <description of what the application does - for user info>
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Richard Saxton   (LTVAD::RDS)
*    History :
*     2-Dec-1990   original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_CORR)'
*    Status :
      INTEGER STATUS
*    Import :
      RECORD /CORR/ HEAD
      CHARACTER*(DAT__SZLOC) ELOC      ! Locator to effective area file
      INTEGER NENERGY
      REAL ENERGY(NENERGY)
      REAL MEAN_EN                     !Mean photon energy
      LOGICAL DISPLAY                  !Display the result on the screen ?
      INTEGER NP                       !Number of pulse height bins
*    Export :
      REAL VCORR(NENERGY)              !Vignetting corrections
      REAL VSING                       !Single vignetting correction to apply
*                                      !to data array. This is 1.0 for files
*                                      !containing spectral info or the value
*                                      !of the correction for a mean photon en.
      LOGICAL VFLAG                    !Have vignetting corrections been calc'd?
*    Functions :
      INTEGER CHR_LEN
          EXTERNAL CHR_LEN
*    Local constants :
      INTEGER MAXANG
         PARAMETER(MAXANG=60)
*    Local variables :
      CHARACTER*(DAT__SZLOC) OALOC     !Locator to off axis array
      CHARACTER*(DAT__SZLOC) BLOC      !Locator to effective area array
      CHARACTER*(DAT__SZLOC) TLOC      !Locator to effective area array
      CHARACTER*(DAT__SZLOC) FLOC      !Locator to first effective area array
      CHARACTER*(DAT__SZLOC) FTLOC     !Locator to filter array
*
      CHARACTER*40 ARRTOP,ARRBOT
      INTEGER BPNTR,TPNTR              !Pointers to eff. area arrays
      INTEGER PNTR1                    !Pointer to first eff. area array
      INTEGER FTPNTR                   !Pointer to filter response array
      INTEGER BOT,TOP                  !Eff area. array no's to interpolate over
      INTEGER LP,K
      INTEGER NEFF                     !No. of eff. area arrays
      REAL ANGLE(MAXANG)               !Off axis angles
      REAL FRAC                        !Interpolation fraction
      CHARACTER*3 CSTR                 !String
      LOGICAL PSPC,HRI
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
      VFLAG = .FALSE.

* Determine which detector
      PSPC=((HEAD.DET(:4)).EQ.'PSPC')
      HRI=((HEAD.DET(:3)).EQ.'HRI')
*
*
      IF (PSPC) THEN

* Read in the array of off-axis angles from the effective areas file
        CALL DAT_FIND(ELOC, 'OFF_ANGLE', OALOC, STATUS)
        CALL DAT_SIZE(OALOC, NEFF, STATUS)
*
        CALL DAT_GET1R(OALOC, MAXANG, ANGLE, NEFF, STATUS)
*
        CALL DAT_ANNUL(OALOC, STATUS)
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT(
     :              'Error reading off axis angles from EFFAR file')
           GOTO 999
        ENDIF

* Calculate which two effective area off axis angles are closest to
* the off-axis angle of the data file
        TOP = 0
        DO LP=1,NEFF
*
           IF (HEAD.OFFAX .LT. ANGLE(LP)) THEN
              TOP = LP
              FRAC = (HEAD.OFFAX-ANGLE(LP-1)) / (ANGLE(LP)-ANGLE(LP-1))
              GOTO 100
           ENDIF
*
        ENDDO
*
100     CONTINUE
*
* Check that the offset was within the effective area calibrations
        IF (TOP .EQ. 0) THEN
           TOP = 14
           FRAC = 0
        ENDIF
*
        IF (TOP .GT. 1) THEN
           BOT = TOP - 1
        ELSE
           BOT = 0
        ENDIF
*
*  Map relevant arrays
        CALL CHR_ITOC(TOP, CSTR, K)
        ARRTOP = 'EFFAR_' // CSTR(1:K)
*
        CALL DAT_FIND(ELOC, ARRTOP, TLOC, STATUS)
        CALL DAT_MAPR(TLOC, 'READ', 1, NENERGY, TPNTR, STATUS)
*
* Check status
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('Error reading EFFAR array from '/
     &                /'effective area file')
           GOTO 999
        ENDIF
*
*  Lower eff. area array to interpolate over
        IF (BOT .NE. 0) THEN
*
           CALL CHR_ITOC(BOT, CSTR, K)
           ARRBOT = 'EFFAR_' // CSTR(1:K)
*
           CALL DAT_FIND(ELOC, ARRBOT, BLOC, STATUS)
           CALL DAT_MAPR(BLOC, 'READ', 1, NENERGY, BPNTR, STATUS)
*
*    Check status
           IF (STATUS .NE. SAI__OK) THEN
              CALL MSG_PRNT('Error reading EFFAR array from '/
     &                   /'effective area file')
              GOTO 999
           ENDIF
*
        ELSE
*
*   Map a dynamic array
           CALL DYN_MAPR(1, NENERGY, BPNTR, STATUS)
*
           IF (STATUS .NE. SAI__OK) THEN
              CALL MSG_PRNT('Error mapping temporary space')
              GOTO 999
           ENDIF

        ENDIF
*
*  Map the first array for normalisation purposes or if it is
*  already mapped as the bottom array make the pointers equivalent
        IF (BOT .EQ. 1) THEN
           PNTR1 = BPNTR
        ELSE
           ARRBOT = 'EFFAR_1'
*
           CALL DAT_FIND(ELOC, ARRBOT, FLOC, STATUS)
           CALL DAT_MAPR(FLOC, 'READ', 1, NENERGY, PNTR1, STATUS)
*
*    Check status
           IF (STATUS .NE. SAI__OK) THEN
              CALL MSG_PRNT('Error reading EFFAR_1 array from '/
     &                   /'effective area file')
              GOTO 999
           ENDIF
*
        ENDIF
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT(
     :            'Error reading eff. area arrays from EFFAR file')
           GOTO 999
        ENDIF
*
*  Calculate the interpolated effective areas
        CALL XRT_VIGNET_PSPC(BOT, NENERGY, ENERGY, %val(BPNTR),
     &                     %val(TPNTR), %val(PNTR1), FRAC, MEAN_EN,
     &                              DISPLAY, NP, VCORR, VSING, STATUS)
*
        VFLAG=.TRUE.
*
*  Are filter corrections required. For now, no correction for anything
*  other than spectra.
        IF (NP .GT. 1 .AND. INDEX(HEAD.FILTER, 'BORON') .GT. 0) THEN
*
*    Get filter correction array
           CALL DAT_FIND(ELOC, 'FILTER_1', FTLOC, STATUS)
           CALL DAT_MAPR(FTLOC, 'READ', 1, NENERGY, FTPNTR, STATUS)
*
           IF (STATUS .NE. SAI__OK) THEN
              CALL MSG_PRNT('Error mapping filter array')
              GOTO 999
           ENDIF
*
*    Multiply the vignetting correction by the filter correction
           CALL XRT_VIGNET_FILT(NENERGY, %val(FTPNTR), DISPLAY, VCORR)
*
*    Tidy the filter array locators
           CALL DAT_UNMAP(FTLOC, STATUS)
           CALL DAT_ANNUL(FTLOC, STATUS)
        ENDIF
*
*  Annul and unmap
        CALL DAT_UNMAP(TLOC, STATUS)
        CALL DAT_ANNUL(TLOC, STATUS)
*
        IF (BOT .NE. 0) THEN
           CALL DAT_UNMAP(BLOC, STATUS)
           CALL DAT_ANNUL(BLOC, STATUS)
        ENDIF
        IF (BOT .NE. 1) THEN
           CALL DAT_UNMAP(FLOC, STATUS)
           CALL DAT_ANNUL(FLOC, STATUS)
        ENDIF
*
      ELSEIF (HRI) THEN

        CALL XRT_VIGNET_HRI(HEAD.OFFAX,VCORR,VSING,STATUS)

        VFLAG=.TRUE.

      ENDIF

      CALL MSG_SETR('VIG',VSING)
      CALL MSG_PRNT('Vignetting correction : ^VIG')

999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRT_VIGNET', STATUS)
      ENDIF
*
      END


*+XRT_VIGNET_PSPC    -   Calculates the vignetting correction for PSPC
      SUBROUTINE XRT_VIGNET_PSPC(BOT, NENERGY, ENERGY, BARR, TARR,
     &       FARR, FRAC, MEAN_EN, DISPLAY, NP, VCORR, VSING, STATUS)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER BOT                         ! 0 if only one array usedto interp
      INTEGER NENERGY                     ! Number of energies
      REAL ENERGY(NENERGY)                ! Trial energies
      REAL BARR(NENERGY)                  ! First interp. effective area array
      REAL TARR(NENERGY)                  ! Second interp. effective area array
      REAL FARR(NENERGY)                  ! The first effective area array
      REAL FRAC                           ! Interpolation fraction
      REAL MEAN_EN                        ! Mean photon energy (keV)
      LOGICAL DISPLAY                     ! Display result on screen ?
      INTEGER NP                          ! Number of pulse height bins in file
*    Import-Export :
      REAL VCORR(NENERGY)                 ! Vignetting correction array
      REAL VSING                          ! Single correction value
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER ELP
      REAL VMEAN,VTOT
*-
      IF (STATUS.NE.SAI__OK) RETURN

*  If not a pulse height file interpolate using the mean photon energy
*  to calculate the sing;e correction
      IF (NP .EQ. 1) THEN
*
         DO ELP=1,NENERGY-1
*
            IF (ENERGY(ELP) .LE. MEAN_EN .AND.
     &          ENERGY(ELP+1) .GE. MEAN_EN) THEN
*
*     Interpolate over the two eff. area arrays
               IF (BOT .NE. 0) THEN
                  VSING = BARR(ELP) - (BARR(ELP)- TARR(ELP)) * FRAC
               ELSE
                  VSING = TARR(ELP)
               ENDIF
*
*       Calculate the ratio of this effective area to the eff area.
*       which would be seen at the centre
               VSING = FARR(ELP) / VSING
*
            ENDIF
*
            VMEAN = VSING
*
         ENDDO
*
         IF (DISPLAY) THEN
            CALL MSG_SETR('VMEAN', VMEAN)
            CALL MSG_PRNT('Mean vignetting correction : ^VMEAN')
         ENDIF
*
*  if this is a pulse height file calculate the effective area over all
*  energies
      ELSE
*
         VTOT=0.0
*
         DO ELP=1,NENERGY
*
*     Interpolate over the two eff. area arrays
            IF (BOT .NE. 0) THEN
               VCORR(ELP) = BARR(ELP) - (BARR(ELP)- TARR(ELP)) * FRAC
            ELSE
               VCORR(ELP) = TARR(ELP)
            ENDIF
*
            VTOT = VTOT + VCORR(ELP)
*
         ENDDO
*
         VSING = 1.0
         VMEAN = VTOT / REAL(NENERGY)
*
         IF (DISPLAY) THEN
            CALL MSG_SETR('VMEAN', VMEAN)
            CALL MSG_PRNT('Mean vignetting correction : ^VMEAN cm**2')
         ENDIF
*
      ENDIF
*
      END


*+XRT_VIGNET_HRI    -   Calculates the vignetting correction for HRI
      SUBROUTINE XRT_VIGNET_HRI(OFFAX, VCORR, VSING, STATUS)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      REAL OFFAX			  ! off-axis angle (arcmin)
*    Import-Export :
      REAL VCORR			  ! Vignetting correction
      REAL VSING                          ! Single correction value
*                                           both the same - for now
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Local constants :
*    Functions :
      INTEGER CHR_LEN
*    Local variables :
      REAL VIG,QE
*-
      IF (STATUS.NE.SAI__OK) RETURN

*  get vignetting factor
      CALL XRT_HRIVIG(OFFAX,VIG)

*  Get quantum efficiency factor
      CALL XRT_HRIQE(OFFAX,QE)
*
      VCORR=1.0/(VIG*QE)
      VSING=VCORR


      END


*+XRT_VIGNET_FILT  -  Multiplies vignetting array by filter array
      SUBROUTINE XRT_VIGNET_FILT(NENERGY, FILTER, DISPLAY, VCORR)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NENERGY                     ! Number of energies
      REAL FILTER(NENERGY)                ! Filter response at each energy
      LOGICAL DISPLAY                     ! Display filter correction value ?
*    Import-Export :
      REAL VCORR(NENERGY)                 ! Vignetting correction array
*    Export :
*     <declarations and descriptions for exported arguments>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER ELP
      REAL FMEAN,FTOT
*-
      FTOT = 0.0
*
* Loop over each energy
      DO ELP=1,NENERGY-1
*
*   Multiply each element
         VCORR(ELP) = VCORR(ELP) * FILTER(ELP)
*
         FTOT = FTOT + FILTER(ELP)

      ENDDO
*
      FMEAN = FTOT / REAL(NENERGY)
*
      IF (DISPLAY) THEN
         CALL MSG_SETR('FMEAN', FMEAN)
         CALL MSG_PRNT('Mean filter correction : ^FMEAN')
      ENDIF
*
      END
