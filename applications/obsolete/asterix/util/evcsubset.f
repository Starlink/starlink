*+  EVCSUBSET - Interactively select an annular subset of an event dataset
      SUBROUTINE EVCSUBSET( STATUS )
*
*    Description :
*
*     The user may select a circular or annular subset of the event dataset
*     according to X and Y coordinate list values.
*
*    Parameters :
*
*     INP=UNIV(U)
*           Dataset from which selection will be made
*     OUT=UNIV(W)
*           Dataset into which selected items will be put
*     ANNULUS=LOGICAL(R)
*           Annular subset?
*     XCENT=REAL(R)
*     YCENT=REAL(R)
*           Coords. of centre of region
*     OUTER=REAL(R)
*           Outer radius of region
*     INNER=REAL(R)
*           Inner radius of region
*
*    Method :
*
*     The new values of FIELD_MIN and FIELD_MAX are derived from the
*     minimum and maximum values of the events which pass the annular
*     criterion.
*
*    Deficiencies :
*
*     Currently, EVCSUBSET does not update either OBS_LENGTH or EXPOSURE_TIME
*     components, the reason being that data are selected subject to spatial
*     cconstraints and thus the pbservation length and exposur time of the
*     selected data are not changed by this process. Eg if the original
*     dataset had an OBS_LENGTH of 2000 secs and an annulur subset was selected\
*     then the data in this region is all that was collected in 2000 secs in
*     that region- the observation length is unchanged.
*
*    Bugs :
*    Authors :
*     Alan McFadzean (BHVAD::ADM)
*    History :
*
*     16 Jan 89 : V1.1-1  Original (BHVAD::ADM)
*     11 Jan 90 : V1.1-2  DATA_MIN and DATA_MAX references removed (BHVAD::DJA)
*     28 Mar 90 : V1.1-3  x and y coords made into separate parameters
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'LIST_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZLOC) CLOC               ! component locator
      CHARACTER*(DAT__SZLOC) ILLOC(LIST__MXNL)  ! input list locators
      CHARACTER*(DAT__SZLOC) ILOC               ! input dataset locator
      CHARACTER*(DAT__SZLOC) OLLOC(LIST__MXNL)  ! output list locators
      CHARACTER*(DAT__SZLOC) OLOC               ! output dataset locator
      CHARACTER*(DAT__SZNAM) CNAM               ! current quantity
      CHARACTER*(DAT__SZNAM) XNAME              ! X List
      CHARACTER*(DAT__SZNAM) YNAME              ! Y List
      CHARACTER*(DAT__SZNAM) LNAMES(LIST__MXNL) ! list names
      CHARACTER*(DAT__SZTYP) TYPE               ! component type
      CHARACTER*80           C                  ! Character for input number
      CHARACTER*80           TEXTI(2)           ! History text
      CHARACTER*6            SIRAD              ! History text
      CHARACTER*6            SORAD              ! History text
      CHARACTER*6            SXCEN              ! History text
      CHARACTER*6            SYCEN              ! History text
      CHARACTER*80           TEXT(2)            ! History text

      INTEGER                XPTR               ! X axis pointer
      INTEGER                YPTR               ! Y axis pointer
      INTEGER                DIMS(DAT__MXDIM)   ! item dimensions
      INTEGER                I            	! Loop counters
      INTEGER                IDATA              ! Pointer to input list DATA_ARRAY
      INTEGER                IQUAN              ! Pointer to input list QUANTUM
      INTEGER                INLEN              ! Length of input lists
      INTEGER                N                  !
      INTEGER                NCOMP              ! # components in input object
      INTEGER                NDIM               ! # item dimensions
      INTEGER                INLINES            ! Number of lines of TEXTI
      INTEGER                NLIST              ! # lists
      INTEGER                ODATA              ! Pointer to output list DATA_ARRAY
      INTEGER                OUTLEN             ! Length of output lists
      INTEGER                OQUAN              ! Pointer to output list QUANTUM
      INTEGER                PTR                ! pointer to selection logicals
      INTEGER                XN                 ! X List number
      INTEGER                YN                 ! Y List number
      INTEGER                L1,L2,L3,L4        ! lengths of char strings

      LOGICAL                INPRIM             ! Input is primitive?
      LOGICAL                OK
      LOGICAL                SCALAR_QUAN        ! Is QUANTUM scalar?
      LOGICAL                ANNTYPE            ! annulur or circular aperture?

      REAL                   MIN, MAX           ! range of output data
      REAL                   XMIN, XMAX         ! range of X list
      REAL                   YMIN, YMAX           ! range of Y list
      REAL                   VALUE              ! Used in copying values between input & output
      REAL                   XCEN               ! X value of annulus centre
      REAL                   YCEN               ! Y value of annulus centre
      REAL                   IRAD               ! Inner annulus radius
      REAL                   ORAD               ! Outer annulus radius
*
*    Local Constants :
*
      CHARACTER*25           VERSION            ! version ID
        PARAMETER           (VERSION = ' EVCSUBSET Version 1.8-0')
*-

*    Version announcement
      CALL MSG_PRNT (VERSION)

*    Initialise
      CALL AST_INIT

*   Obtain object name and associate locator and display to term.
      CALL USI_ASSOC2 ('INP', 'OUT', 'READ',ILOC, OLOC, INPRIM, STATUS)

      IF (INPRIM) THEN
        CALL MSG_PRNT ('FATAL ERROR: This is not an event dataset!')
        STATUS = SAI__ERROR
        GOTO 9000

      END IF

*    Locate & display all lists in the input data object
      CALL MSG_PRNT ('The LIST''s present are:')
      CALL LIST_FINDALLOK (ILOC, .TRUE., ILLOC, LNAMES, NLIST, INLEN,
     :                                                           STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 9000

*    Check there are some
      IF (NLIST .EQ.0) THEN
        CALL MSG_PRNT ('FATAL ERROR: there are no lists!')
        STATUS = SAI__ERROR
        GOTO 9000

      END IF

      DO I=1,NLIST
* Find X list
        IF(LNAMES(I).EQ.'X_CORR') THEN
	   XNAME='X_CORR'
           XN=I
	ELSEIF(LNAMES(I).EQ.'X_RAW') THEN
           XNAME='X_RAW'
           XN=I
	ELSEIF(LNAMES(I).EQ.'Y_CORR') THEN
	   YNAME='Y_CORR'
           YN=I
	ELSEIF(LNAMES(I).EQ.'Y_RAW') THEN
           YNAME='Y_RAW'
           YN=I
        ENDIF
      ENDDO
      IF(XN.EQ.0) THEN
        CALL MSG_PRNT ('No legal Y-Coordinate')
        GOTO 9000
      ELSEIF(YN.EQ.0) THEN
        CALL MSG_PRNT ('No legal Y-Coordinate')
        GOTO 9000
      ENDIF

* Map X and Y lists using common type
        CALL BDA_CHKDATA(ILLOC(XN),OK,NDIM,DIMS,STATUS)
        IF(OK.AND.NDIM.EQ.1) THEN
          CALL BDA_MAPDATA(ILLOC(XN),'READ',XPTR,STATUS)
        ELSE
          CALL MSG_PRNT ('No legal X-Coordinate')
          GOTO 9000
        ENDIF
	IF(STATUS.NE.SAI__OK) GOTO 9000

        CALL BDA_CHKDATA(ILLOC(YN),OK,NDIM,DIMS,STATUS)
        IF(OK.AND.NDIM.EQ.1) THEN
          CALL BDA_MAPDATA(ILLOC(YN),'READ',YPTR,STATUS)
        ELSE
          CALL MSG_PRNT ('No legal Y-Coordinate')
          GOTO 9000
        ENDIF
	IF(STATUS.NE.SAI__OK) GOTO 9000
        INLEN=DIMS(1)

* Obtain field ranges
	CALL LIST_GFLDR(ILLOC(XN),XMIN,XMAX,STATUS)
	CALL LIST_GFLDR(ILLOC(YN),YMIN,YMAX,STATUS)

* Tell user what data range is
* First X axis..
       CALL MSG_SETR('XMIN',XMIN)
       CALL MSG_SETR('XMAX',XMAX)
       CALL MSG_SETC('XNAME',XNAME)
       CALL MSG_PRNT ('The ^XNAME data range is ^XMIN to ^XMAX')

* now Y axis..
       CALL MSG_SETR('YMIN',YMIN)
       CALL MSG_SETR('YMAX',YMAX)
       CALL MSG_SETC('YNAME',YNAME)
       CALL MSG_PRNT ('The ^YNAME data range is ^YMIN to ^YMAX')

* annular or circular subset?
       CALL USI_GET0L('ANNULUS',ANNTYPE,STATUS)
* Central point?
       CALL USI_GET0R('XCENT',XCEN,STATUS)
       CALL USI_GET0R('YCENT',YCEN,STATUS)
* Get radius/radii
       CALL USI_GET0R('OUTER',ORAD,STATUS)
       IF(ANNTYPE) THEN
         CALL USI_GET0R('INNER',IRAD,STATUS)
       ELSE
         IRAD=0.0
       ENDIF

*    Set up and map temp 1D logical array
      CALL DYN_MAPL (1, INLEN, PTR, STATUS)

*    Find elements to copy to output
      CALL EVCSUB_SETSEL(INLEN,%VAL(XPTR),%VAL(YPTR),XCEN,YCEN,ORAD,
     :IRAD,%VAL(PTR),OUTLEN)

*    Tell user how many items will remain
      CALL MSG_SETI ('N1', INLEN)
      CALL MSG_SETI ('N2',  OUTLEN)
      CALL MSG_PRNT ('^N2 items remain out of ^N1 in original  dataset')

      IF (OUTLEN .EQ. 0) THEN
        CALL MSG_PRNT ('FATAL ERROR: All data excluded')
        STATUS = SAI__ERROR
      ELSE IF (OUTLEN .EQ. INLEN) THEN
        CALL MSG_PRNT ('FATAL ERROR: All data included')
        STATUS = SAI__ERROR
      END IF

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 9000

*    Copy non list components ie TITLE, HISTORY, & MORE.
      CALL DAT_NCOMP (ILOC, NCOMP, STATUS)

      DO I = 1, NCOMP
        CALL DAT_INDEX (ILOC, I, CLOC, STATUS)
        CALL DAT_TYPE  (CLOC, TYPE,    STATUS)

        IF (TYPE .NE. 'LIST') THEN
          CALL DAT_NAME (CLOC, CNAM,       STATUS)
          CALL DAT_COPY (CLOC, OLOC, CNAM, STATUS)

        END IF
      END DO

*    Do the subset operation
      DO I = 1, NLIST
        CALL MSG_PRNT ('Processing '//LNAMES(I))
*      Find out if QUANTUM is scalar
        IF(LNAMES(I).NE.'QUALITY') THEN
          CALL CMP_SHAPE (ILLOC(I), 'QUANTUM', 7, DIMS, NDIM, STATUS)
        ELSE
          NDIM=0
        ENDIF

        IF (NDIM .EQ. 0) THEN
          SCALAR_QUAN = .TRUE.

        ELSE
          SCALAR_QUAN = .FALSE.

        END IF

*      Get data type
        CALL CMP_TYPE (ILLOC(I), 'DATA_ARRAY', TYPE, STATUS)

*      Create new list
        CALL LIST_NEW (OLOC, LNAMES(I), TYPE, OUTLEN, SCALAR_QUAN,
     :                                                 OLLOC(I), STATUS)

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 9000

*      Map input & output list vectors
        IF (I.EQ.XN) THEN
          IDATA = XPTR
        ELSEIF(I.EQ.YN) THEN
          IDATA = YPTR
        ELSE
          CALL BDA_MAPDATA(ILLOC(I),'READ',IDATA,STATUS)
        END IF

        CALL BDA_MAPDATA(OLLOC(I),'WRITE',ODATA,STATUS)

        IF (.NOT. SCALAR_QUAN) THEN
          CALL CMP_MAPV (ILLOC(I), 'QUANTUM', '_REAL', 'READ', IQUAN,
     :                                                        N, STATUS)
          CALL CMP_MAPV (OLLOC(I), 'QUANTUM', '_REAL', 'WRITE', OQUAN,
     :                                                        N, STATUS)

        END IF

*      Perform the selection
        CALL EVCSUB_SEL (INLEN, SCALAR_QUAN, %VAL(PTR), %VAL(IDATA),
     :          %VAL(IQUAN), %VAL(ODATA), %VAL(OQUAN), STATUS)
        CALL ARR_RANG1R(OUTLEN,%VAL(ODATA),MIN,MAX,STATUS)
* Update field range
        CALL LIST_PFLDR(OLLOC(I),MIN,MAX,STATUS)

*      Unmap
        CALL BDA_UNMAPDATA(ILLOC(I),STATUS)
        CALL BDA_UNMAPDATA(OLLOC(I),STATUS)


        IF (SCALAR_QUAN.AND.LNAMES(I).NE.'QUALITY') THEN
          CALL HDX_OK (ILLOC(I), 'QUANTUM', OK, STATUS)

          IF (OK) THEN
            CALL CMP_GET0R (ILLOC(I), 'QUANTUM', VALUE, STATUS)
            CALL CMP_PUT0R (OLLOC(I), 'QUANTUM', VALUE, STATUS)

          END IF
        ENDIF


*      Update exposure time if TIMETAG list
c        IF (LNAMES(I) .EQ. 'TIMETAG' .OR.
c     :                                LNAMES(I) .EQ. 'RAW_TIMETAG') THEN
*      Update exposure_time, etc.
c          IF(SCALAR_QUAN) THEN
c            EXPOS=VALUE*FLOAT(OUTLEN)
c          ELSE
c            CALL EVCSUB_EXPOS(OUTLEN,%VAL(OQUAN),EXPOS)
c          ENDIF
c          OBSLEN=MAX-MIN
c
c          CALL BDA_LOCHEAD (OLOC, TLOC,                STATUS)

c          CALL DAT_THERE   (TLOC, 'EXPOSURE_TIME', THERE, STATUS)
c          IF (.NOT. THERE) THEN
c            CALL DAT_NEW0R (TLOC, 'EXPOSURE_TIME', STATUS)
c          END IF
c          CALL CMP_PUT0R (TLOC, 'EXPOSURE_TIME', EXPOS, STATUS)

c          CALL DAT_THERE   (TLOC, 'OBS_LENGTH', THERE, STATUS)
c          IF (.NOT. THERE) THEN
c            CALL DAT_NEW0R (TLOC, 'OBS_LENGTH', STATUS)
c          END IF
c          CALL CMP_PUT0R (TLOC, 'OBS_LENGTH', OBSLEN, STATUS)

c        END IF


        IF(.NOT.SCALAR_QUAN) THEN
          CALL CMP_UNMAP (ILLOC(I), 'QUANTUM', STATUS)
          CALL CMP_UNMAP (OLLOC(I), 'QUANTUM', STATUS)
        END IF

*      Write FIELD_MIN & FIELD_MAX values
        CALL CMP_PUT0R (OLLOC(I), 'FIELD_MAX', MAX, STATUS)
        CALL CMP_PUT0R (OLLOC(I), 'FIELD_MIN', MIN, STATUS)

*      Copy units
        CALL HDX_OK (ILLOC(I), 'UNITS', OK, STATUS)

        IF (OK) THEN
          CALL CMP_GET0C (ILLOC(I), 'UNITS', C, STATUS)
          CALL CMP_PUT0C (OLLOC(I), 'UNITS', C, STATUS)

        END IF
      END DO

*    Unmap temporary booleans
      CALL DYN_UNMAP (PTR)

*    Update history
      CALL HIST_ADD    (OLOC, VERSION,      STATUS)
      CALL USI_NAMEI (INLINES,TEXTI, STATUS)
      CALL HIST_PTXT   (OLOC, INLINES, TEXTI, STATUS)
      CALL CHR_RTOC(XCEN,SXCEN,L1)
      CALL CHR_RTOC(YCEN,SYCEN,L2)
      ORAD=SQRT(ORAD)
      CALL CHR_RTOC(ORAD,SORAD,L3)
      IF(ANNTYPE) THEN
        IRAD=SQRT(IRAD)
        CALL CHR_RTOC(IRAD,SIRAD,L4)
        TEXT(1)='Annulus, centre: ('//SXCEN(1:L1)//','//SYCEN(1:L2)
     ://')'
        TEXT(2)='Inner radius= '//SIRAD(1:L4)//',Outer radius='
     ://SORAD(1:L3)
      ELSE
        TEXT(1)='Circle, centre: ('//SXCEN(1:L1)//','//SYCEN(1:L2)
     ://')'
        TEXT(2)='Radius= '//SORAD(1:L3)
      ENDIF
      CALL HIST_PTXT   (OLOC, 2, TEXT, STATUS)

*    Tidy up and exit
9000  CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END

*+  EVCSUB_SETSEL - Set up selection logicals according to values
      SUBROUTINE EVCSUB_SETSEL(INLEN,XPTR,YPTR,XCEN,YCEN,ORAD,
     : IRAD,TPTR,OUTLEN)
*    Description :
*     The selection criteria are applied, and if met TPTR = .TRUE.
*     else TPTR = .FALSE.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Alan McFadzean (BHVAD::ADM)
*    History :
*     16/1/89:  Original (ADM)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                INLEN            ! length of lists
      REAL                   XPTR(*)          ! X COORDS
      REAL                   YPTR(*)          ! Y COORDS
      REAL                   XCEN             ! X CENTRE POSITION
      REAL                   YCEN             ! Y CENTRE POSITION
      REAL                   ORAD             ! Annulus outer radius
      REAL                   IRAD             ! Annulus inner radius

*    Export :
      LOGICAL                TPTR(*)          ! Copy this element to output?
      INTEGER                OUTLEN           ! # outputs
* Internal
      REAL                   TEST             ! Test values
      INTEGER                I                ! Loop counter
*-

      IF(IRAD.EQ.0) THEN
* circular
        ORAD=ORAD**2
        OUTLEN=0
        DO I=1,INLEN
          TEST=((XCEN-XPTR(I))**2)+((YCEN-YPTR(I))**2)
          IF(TEST.LE.ORAD) THEN
            TPTR(I)=.TRUE.
            OUTLEN=OUTLEN+1
          ELSE
            TPTR(I)=.FALSE.
          ENDIF
        ENDDO
      ELSE
* annular
        ORAD=ORAD**2
        IRAD=IRAD**2
        OUTLEN=0
        DO I=1,INLEN
          TEST=((XCEN-XPTR(I))**2)+((YCEN-YPTR(I))**2)
          IF(TEST.LE.ORAD.AND.TEST.GT.IRAD) THEN
            TPTR(I)=.TRUE.
            OUTLEN=OUTLEN+1
          ELSE
            TPTR(I)=.FALSE.
          ENDIF
        ENDDO
      ENDIF

      END

*+  EVCSUB_SEL - Perform the subsetting
      SUBROUTINE EVCSUB_SEL (INLEN, SCALAR_QUAN, COPY, IDATA, IQUAN,
     :                                 ODATA, OQUAN, STATUS)
*    Description :
*     Loops over list, copying selected input values to output.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews (PLA_AST88@uk.ac.bham.sr.star)
*    History :
*     30/11/88:  Original (PLA)
*     17/11/89:  Borrowed from EVSUBSET (BHVAD::ADM)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                INLEN            ! Length of input lists

      LOGICAL                SCALAR_QUAN        ! Is QUANTUM scalar?
      LOGICAL                COPY(INLEN)      ! Copy this element to output?

      REAL                   IDATA(INLEN)     ! Input list DATA_ARRAY
      REAL                   IQUAN(INLEN)     ! Input list QUANTUM
*    Export :
      REAL                   ODATA(*)           ! Output list DATA_ARRAY
      REAL                   OQUAN(*)           ! Output list QUANTUM
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER                I, J               ! loop variables
*-
*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

      J = 1

      IF (SCALAR_QUAN) THEN
        DO I = 1, INLEN
          IF (COPY(I)) THEN
            ODATA (J) = IDATA (I)
            J = J + 1

          END IF
        END DO
      ELSE
        DO I = 1, INLEN
          IF (COPY(I)) THEN
            ODATA (J) = IDATA (I)
            OQUAN (J) = IQUAN (I)
            J = J + 1

          END IF
        END DO
      END IF
      END


*+  EVCSUB_EXPOS - determine exposure time from QUANTUM info.
      SUBROUTINE EVCSUB_EXPOS(OUTLEN,OQUAN,VALUE)
*    Description :
*     Loops over output QUANTUM values and sums to give total integration time.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Alan McFadzean (BHVAD::ADM)
*    History :
*     17/1/89:  Original (ADM)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                OUTLEN            ! Length of QUANTUM list

      REAL                   OQUAN(OUTLEN)     ! Output list QUANTUM
*    Export :
      REAL                   VALUE             ! Integration time
*    Local variables :
      INTEGER                I                 ! loop variables
*-
      VALUE=0.0

      DO I=1,OUTLEN
        VALUE=VALUE+OQUAN(I)
      ENDDO
      END
