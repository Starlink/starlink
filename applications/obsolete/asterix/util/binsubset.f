*+  BINSUBSET - Subsets a DATA_ARRAY subclass object
      SUBROUTINE BINSUBSET(STATUS)
*
*    Description :
*
*     A binned dataset is reduced according to user specified
*     ranges on its axis and/or data values.
*       If KEEP = .TRUE. then data .GE. lower axis bound and .LT. upper axis bound are included.
*        EXCEPTION - if upper axis bound = highest axis value; .LE. upper axis bound are included.
*       If KEEP = .FALSE. then data .LT. lower axis bound .OR. .GT. opper axis bound are included.
*
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*
*    History :
*
*     29 Jun 84 : Original (BHVAD::JCMP)
*     21 Nov 85 : V0.4-1 Rewrite with more facilities (BHVAD::JCMP)
*     28 Nov 85 : V0.4-2 Some minor mods (BHVAD::JCMP)
*     17 Dec 85 : V0.4-3 ADAM version (BHVAD::JCMP)
*     26 Sep 86 : V0.5-1 DTA__MXRANG changed (BHVAD::JCMP)
*      1 Oct 86 : V0.5-2 Bug fix? now handles 2D datasets! (BHVAD::JCMP)
*     29 Oct 87 : V0.6-1 Improved use of HISTORY etc (BHVAD::ADM)
*     24 Nov 87 : V0.6-2 Modified to ROSAT BINSUBSET specs.
*     28 Jun 88 : V1.0-1 ASTERIX88 [BHVAD::ADM]
*      1 Dec 88 : ASTERIX88 conversion done properly (PLA)
*     25 Sep 89 : V1.0-2 Now handles decreasing axis values (RJV)
*     10 Dec 89 : V1.0-3 Primitive now spelt correctly! No longer
*                        prompts for axis numbers if 1D (DJA)
*     19 Jan 90 : V1.0-4 Bug fix and efficiency improvements
*                        remove selection on data value (RJV)
*                        add slicing as an option
*      6 Jun 90 : V1.0-5 Option to select ranges by index number (RJV)
*      4 Jul 90 : V1.2-0 Range selection now works on bin centres, and not
*                        bin bounds (DJA)
*     27 Mar 91 : V1.4-0 Handles primitive inputs (DJA)
*      7 Jun 91 : V1.5-1 History improved. Bug dealing with structured
*                        objects without axes fixed. (DJA)
*     20 Oct 92 : V1.7-0 Proofed against failure to map output (DJA)
*     19 Nov 92 : V1.7-1 Changed arguments to AXIS_VAL2PIX (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     25 Apr 95 : V1.8-1 New data interfaces (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local parameters :
*
      INTEGER                DTA__MXRANG        ! max no. permissible ranges
        PARAMETER           (DTA__MXRANG = 20)
      INTEGER                MX__HTEXT
        PARAMETER           (MX__HTEXT = ADI__MXDIM)
*
*    Local variables :
*
      CHARACTER*80           TEXTI(4)           ! Input file spec
      CHARACTER*132          HTEXT(MX__HTEXT)   ! History text
      CHARACTER*80           AXUNT(ADI__MXDIM)  ! Units for each axis
      CHARACTER*6            PARNAM
      CHARACTER*40           TEM                ! Dummy string
      CHARACTER*7 AXID

      REAL                   AXLO(ADI__MXDIM)   ! axis low
      REAL                   AXHI(ADI__MXDIM)   ! axis high
      REAL                   DIR(ADI__MXDIM)	! axis direction indicator
      REAL                   BASE               ! Base value of regular axis
      REAL                   RANGES(2,DTA__MXRANG,ADI__MXDIM)  ! item ranges
      REAL                   SCALE              ! Used in copying values from input to output axes
      REAL                   WID                ! WIDTH of uniform axis

      INTEGER                DIMS(ADI__MXDIM)   ! Input DATA_ARRAY dimensions
      INTEGER                HU                 ! History lines used
      INTEGER                IDPTR              ! Pointer to input data
      INTEGER			IFID			! Input dataset id
      INTEGER                IVPTR              ! Pointer to input VARIANCE
      INTEGER                IWPTR(ADI__MXDIM)  ! Pointer to input axis widths
      INTEGER                NDIM               ! Number of input dimensions
      INTEGER                NRANGE(ADI__MXDIM) ! # item ranges
      INTEGER                ODIMS(ADI__MXDIM)  ! Output DATA_ARRAY dimensions
      INTEGER                ODPTR              ! Pointer to output data
      INTEGER                OVPTR              ! Pointer to output VARIANCE
      INTEGER                OWPTR(ADI__MXDIM)  ! Pointer to output axis widths
      INTEGER                ONDIM              ! Number of output dimensions
      INTEGER                PARENT(ADI__MXDIM) ! parent axis of output
      INTEGER                SIZ                ! size of this & that
      INTEGER                TPTR               ! pointer to temp mapped array of logicals
      INTEGER                I, J, K            ! Loop counters
      INTEGER                NELM               ! Total length of input data
      INTEGER                IQPTR              ! Pointer to input QUALITY
      INTEGER			OFID			! Output dataset id
      INTEGER                OQPTR              ! Pointer to output QUALITY
      INTEGER                INLINES            ! Number of TEXTI lines
      INTEGER                AXRANGE(2,DTA__MXRANG,ADI__MXDIM)
						! Pixel equivalent of RANGES
      INTEGER                IAXPTR(ADI__MXDIM) ! Pointers to input axes
      INTEGER                NAX                ! Number of dataset axes
      INTEGER                NSEL,ISEL
      INTEGER                OAXPTR(ADI__MXDIM) ! Pointers to output axes
      INTEGER                SELAX(ADI__MXDIM)
      INTEGER                TLEN               ! Text length

      BYTE                   MASK               ! Quality mask

      LOGICAL                INPRIM             ! Input object is primitive
      LOGICAL                WIDOK(ADI__MXDIM)  ! Width component od axes ok?
      LOGICAL                IUNIF(ADI__MXDIM)  ! Uniform widths? - input
      LOGICAL                OUNIF(ADI__MXDIM)  ! Uniform widths? - output
      LOGICAL                OK                 ! object is ok
      LOGICAL                KEEP(ADI__MXDIM)   ! Are ranges those to keep?
      LOGICAL                KEEPDATA
      LOGICAL 		     SLICE
      LOGICAL                INDEX              ! select by index
      LOGICAL                IREG(ADI__MXDIM)   ! input axis regularly spaced?
      LOGICAL                OREG(ADI__MXDIM)   ! output axis regularly spaced?
      LOGICAL                QUALOK             ! Input QUALITY OK?
      LOGICAL                VAROK              ! Input VARIANCE OK?
      LOGICAL                NORM               ! Axes normalised?
      LOGICAL                SEL(ADI__MXDIM)    ! Has axis been selected on?
*
*    Version :
*
      CHARACTER*(25)         VERSION
        PARAMETER            ( VERSION = 'BINSUBSET Version 1.8-1' )
*-

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT()
      HU = 0

*    Obtain identifiers to input and output objects
      CALL USI_TASSOC2( 'INP', 'OUT', 'READ',IFID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get DATA_ARRAY
      CALL BDI_PRIM( IFID, INPRIM, STATUS )
      CALL BDI_CHKDATA( IFID, OK, NDIM, DIMS, STATUS )
      IF (.NOT. OK) THEN
        CALL MSG_PRNT('AST_ERR: Invalid data')
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( INPRIM ) THEN
        CALL MSG_PRNT( 'Primitive input - use pixel numbers in ranges' )
        INDEX = .TRUE.
      END IF

*    Define axis ranges
      NELM = 1
      DO I = 1, ADI__MXDIM
        SEL(I) = .FALSE.
        IF ( I .GT. NDIM ) THEN
          DIMS(I) = 1
        END IF
        NRANGE(I) = 1
        AXRANGE(1,1,I) = 1
        AXRANGE(2,1,I) = DIMS(I)
        KEEP(I) = .TRUE.
        NELM = NELM * DIMS(I)
      END DO

* see if selected ranges to be kept or discarded
      CALL USI_GET0L( 'KEEP', KEEPDATA, STATUS )

* see if slicing required
      CALL USI_GET0L( 'SLICE', SLICE, STATUS )

* see if selecting by index number
      IF ( .NOT. INPRIM ) CALL USI_GET0L('INDEX',INDEX,STATUS)

*    Display axis labels, and get min & max values
      CALL BINSUBSET_DISPAX( IFID, INPRIM, NDIM, DIMS, AXUNT, AXLO,
     :                               AXHI, IREG, DIR, NAX, STATUS )
      INDEX = ( INDEX .OR. ( NAX .EQ. 0 ) )

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Choose axes to select on unless 1D
      IF ( NDIM .EQ. 1 ) THEN
        SELAX(1) = 1
        NSEL = 1
      ELSE
        NSEL=0
        CALL PRS_GETLIST( 'AXES', NDIM, SELAX, NSEL, STATUS )
      END IF
      AXID = '1234567'

*  go through selected axes
      DO ISEL=1,NSEL
        I=SELAX(ISEL)
        SEL(I) = .TRUE.
        KEEP(I)=KEEPDATA

*  get required ranges for this axis
        PARNAM='AXIS'//AXID(I:I)
        CALL MSG_SETI( 'AX', I )

        IF ( INDEX ) THEN
          CALL CHR_ITOC( DIMS(I), TEM, TLEN )
          CALL MSG_PRNT( 'Axis ^AX has '//TEM(:TLEN)//' elements' )
          CALL USI_DEF0C( PARNAM, '1:'//TEM(:TLEN), STATUS )
          CALL PRS_GETRANGES (PARNAM, DTA__MXRANG, 1,1.0,
     :               REAL(DIMS(I)),RANGES(1,1,I),NRANGE(I), STATUS )
          IF ( RANGES(1,1,I) .LT. 1.0 ) THEN
            RANGES(1,1,I) = 1.0
          END IF
          IF (RANGES(2,NRANGE(I),I).GT.REAL(DIMS(I))) THEN
            RANGES(2,NRANGE(I),I)=REAL(DIMS(I))
          END IF

        ELSE
          CALL MSG_SETR( 'LO', AXLO(I) )
          CALL MSG_SETR( 'HI', AXHI(I) )
          CALL MSG_SETC( 'UNT', AXUNT(I) )
          CALL MSG_PRNT( 'The range for axis ^AX is ^LO to ^HI ^UNT')
          CALL MSG_SETR( 'LO', AXLO(I) )
          CALL MSG_SETR( 'HI', AXHI(I) )
          CALL MSG_MAKE( '^LO:^HI', TEM, TLEN )
          CALL USI_DEF0C( PARNAM, TEM(:TLEN), STATUS )
          CALL PRS_GETRANGES( PARNAM, DTA__MXRANG, 1, AXLO(I),
     :               AXHI(I),RANGES(1,1,I),NRANGE(I), STATUS )

*  if ranges given are outside limits then set equal to limits
          IF ((RANGES(1,1,I)-AXLO(I))*DIR(I).LT.0.0) THEN
            RANGES(1,1,I) = AXLO(I)
          END IF
          IF ((AXHI(I)-RANGES(2,NRANGE(I),I))*DIR(I).LT.0.0) THEN
            RANGES(2,NRANGE(I),I) = AXHI(I)
          END IF

        END IF

*      Write history text
        HU = HU + 1
        CALL MSG_SETI( 'AX', I )
        CALL MSG_MAKE( 'Axis ^AX subset', HTEXT(HU), TLEN )
        TLEN = TLEN + 1
        DO K = 1, NRANGE(I)
          CALL MSG_SETR( 'LO', RANGES(1,K,I) )
          CALL MSG_SETR( 'HI', RANGES(2,K,I) )
          CALL MSG_SETC( 'BIT', HTEXT(HU)(:TLEN) )
          CALL MSG_MAKE( '^BIT, ^LO:^HI', HTEXT(HU), TLEN )
        END DO
        IF ( INDEX ) THEN
          HTEXT(HU) = HTEXT(HU)(:TLEN)//' pixels'
        ELSE
          HTEXT(HU) = HTEXT(HU)(:TLEN)//' '//AXUNT(I)
        END IF

      END DO

*    Convert ranges into integer pixel ranges
      IF ( INDEX ) THEN
        DO I = 1,ADI__MXDIM
          IF (SEL(I)) THEN
            DO J=1,NRANGE(I)
              AXRANGE(1,J,I)=INT(RANGES(1,J,I))
              AXRANGE(2,J,I)=INT(RANGES(2,J,I))
            END DO
          END IF
        END DO
      ELSE
        CALL BINSUBSET_AXRAN( IFID,DIMS,NRANGE,RANGES,SEL,DIR,AXRANGE,
     :                                                         STATUS)
      END IF

*    Set up and map temp. logical array
      CALL DYN_MAPL( NDIM, DIMS, TPTR, STATUS )
      CALL ARR_INIT1L( (.NOT. KEEPDATA), NELM, %VAL(TPTR), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Find out which elements are to be written to the output
      CALL BINSUBSET_SETSEL (DIMS(1),DIMS(2),DIMS(3),DIMS(4),
     :                       DIMS(5),DIMS(6),DIMS(7),NRANGE,AXRANGE,
     :                                     KEEPDATA,%VAL(TPTR),STATUS)

*    Find dimensions of output dataset
      ONDIM = 0
      DO I = 1, NDIM

*  axis wasn't changed
        IF (.NOT.SEL(I)) THEN

          ONDIM=ONDIM+1
          ODIMS(ONDIM)=DIMS(I)
          PARENT(ONDIM)=I

*  was changed
        ELSE

*  case of axis reduced to one bin
          IF (NRANGE(I).EQ.1.AND.AXRANGE(2,1,I).EQ.AXRANGE(1,1,I)) THEN

*  if slicing skip this axis
            IF (SLICE) THEN
              CONTINUE
*  otherwise set output accordingly
            ELSE
              ONDIM=ONDIM+1
              ODIMS(ONDIM)=1
              PARENT(ONDIM)=I
            END IF

*  case of data being kept
          ELSEIF (KEEP(I)) THEN
            ONDIM        = ONDIM + 1
            ODIMS(ONDIM) = 0

*  scan through ranges incrementing size
            DO J = 1, NRANGE(I)
              DO K = AXRANGE(1,J,I), AXRANGE(2,J,I)
                ODIMS(ONDIM) = ODIMS(ONDIM) + 1
              END DO
            END DO
            PARENT(ONDIM) = I

*  case of data being discarded
          ELSE
            ONDIM        = ONDIM + 1
            ODIMS(ONDIM) = DIMS(I)

*  scan through ranges decrementing size
            DO J = 1, NRANGE(I)
              DO K = AXRANGE(1,J,I), AXRANGE(2,J,I)
                ODIMS(ONDIM) = ODIMS(ONDIM) - 1
              END DO
            END DO
            PARENT(ONDIM) = I
          END IF

        END IF

      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  case of total reduction to one bin
      IF (ONDIM .EQ. 0) THEN
        ONDIM    = 1
        ODIMS(1) = 1
        PARENT(1) = 1
      END IF

*    Create output dataset
      CALL BDI_CREDATA (OFID, ONDIM, ODIMS, STATUS)
      CALL BDI_MAPDATA (IFID, 'READ', IDPTR, STATUS)
      CALL BDI_MAPDATA (OFID, 'WRITE', ODPTR, STATUS)
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy other stuff
      CALL BDI_COPTEXT(IFID,OFID,STATUS)

      CALL BDI_CHKVAR (IFID, VAROK, NDIM, DIMS, STATUS)

*  variance
      IF ( VAROK ) THEN
        CALL BDI_MAPVAR (IFID, 'READ', IVPTR,  STATUS)
        CALL BDI_CREVAR (OFID, ONDIM, ODIMS,   STATUS)
        CALL BDI_MAPVAR (OFID, 'WRITE', OVPTR, STATUS)
      END IF

* quality
      CALL BDI_CHKQUAL (IFID, QUALOK, NDIM, DIMS, STATUS)
      IF ( QUALOK ) THEN
        CALL BDI_MAPQUAL( IFID, 'READ', IQPTR, STATUS )
        CALL BDI_CREQUAL( OFID, ONDIM, ODIMS, STATUS )
        CALL BDI_MAPQUAL( OFID, 'WRITE', OQPTR, STATUS )
        CALL BDI_GETMASK( IFID, MASK, STATUS )
        CALL BDI_PUTMASK( OFID, MASK, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write output DATA, VARIANCE, & QUALITY
      CALL BINSUBSET_SEL (NELM, %VAL(IDPTR),%VAL(IVPTR),%VAL(IQPTR),
     :              %VAL(TPTR),VAROK,QUALOK,%VAL(ODPTR),%VAL(OVPTR),
     :                                          %VAL(OQPTR),STATUS )

*    Finished with logical array
      CALL DYN_UNMAP( TPTR, STATUS )

*    Do rest of tidying up
      CALL BDI_UNMAPDATA( IFID, STATUS )
      CALL BDI_UNMAPDATA( OFID, STATUS )
      IF ( VAROK ) THEN
        CALL BDI_UNMAPVAR( IFID, STATUS )
        CALL BDI_UNMAPVAR( OFID, STATUS )
      END IF
      IF ( QUALOK ) THEN
        CALL BDI_UNMAPQUAL( IFID, STATUS )
        CALL BDI_UNMAPQUAL( OFID, STATUS )
      END IF

*  now deal with axes
      IF ( ( NAX .GT. 0 ) .AND. .NOT. INPRIM ) THEN

        CALL BDI_CREAXES( OFID, ONDIM, STATUS )

        DO I=1,ONDIM
*    get parent axis
          J = PARENT(I)

*    if unchanged then just copy
          IF (.NOT.SEL(J)) THEN

            CALL BDI_COPAXIS(IFID,OFID,J,I,STATUS)

          ELSE

*    see if change needed to regularity of axis

*    if parent axis is fragmented then must become irregular
            IF (NRANGE(J).GT.1.AND.KEEP(J)) THEN
              OREG(I) = .FALSE.
*    if specified range discarded - can only stay regular if at end
            ELSEIF (IREG(J).AND..NOT.KEEP(J)) THEN
              IF (NRANGE(J).EQ.1.AND.AXRANGE(1,1,J).NE.1.AND.
     :                           AXRANGE(2,1,J).NE.DIMS(J)) THEN
                OREG(I)=.FALSE.
              ELSEIF(NRANGE(J).EQ.2.AND.AXRANGE(1,1,J).NE.1.AND.
     :                              AXRANGE(2,2,J).NE.DIMS(J)) THEN
                OREG(I)=.FALSE.
              END IF

            ELSE
              OREG(I)=IREG(J)
            END IF

*   create axis value component
            CALL BDI_CREAXVAL (OFID, I, OREG(I), ODIMS(I), STATUS)

*    see if width component needed
            CALL BDI_CHKAXWID (IFID, J, WIDOK(J), IUNIF(J), SIZ, STATUS)
            OUNIF(I)=(IUNIF(J).AND.OREG(I))
            IF (WIDOK(J)) THEN
              CALL BDI_CREAXWID (OFID, I, OUNIF(I), ODIMS(I), STATUS)
            END IF

            IF (OREG(I)) THEN

*    calculated new base value and write to output
              CALL BDI_GETAXVAL (IFID, J, BASE, SCALE, SIZ, STATUS)
*    case of single chunk being taken out
              IF (KEEP(J)) THEN
                BASE=BASE+(AXRANGE(1,1,J)-1)*SCALE
*    case of chunk being discarded from beginning
              ELSEIF (.NOT.KEEP(J).AND.AXRANGE(1,1,J).EQ.1) THEN
                  BASE=BASE+AXRANGE(2,1,J)*SCALE
              END IF

              CALL BDI_PUTAXVAL (OFID, I, BASE, SCALE, ODIMS(I), STATUS)

*    write width component if required
              IF (WIDOK(J).AND.OUNIF(I)) THEN
                CALL BDI_GETAXWID (IFID, J, WID, STATUS)
                CALL BDI_PUTAXWID (OFID, I, WID, STATUS)
              END IF

*    irregular axis
            ELSE
*          Map axis values
              CALL BDI_MAPAXVAL (IFID, 'READ', J, IAXPTR(J), STATUS)
              CALL BDI_MAPAXVAL (OFID, 'WRITE', I, OAXPTR(I), STATUS)
              IF (WIDOK(J)) THEN
                CALL BDI_MAPAXWID(IFID,'READ',J,IWPTR(J),STATUS)
                CALL BDI_MAPAXWID(OFID,'WRITE',I,OWPTR(I),STATUS)
              END IF

               CALL BINSUBSET_AXCOP(DIMS(J),NRANGE(J),AXRANGE(1,1,J),
     :                         KEEP(J),%VAL(IAXPTR(J)),%VAL(IWPTR(J)),
     :                         WIDOK(J),%VAL(OAXPTR(I)), %VAL(OWPTR(I)),
     :                                                           STATUS)

            END IF

*        Copy labels etc.
            CALL BDI_COPAXTEXT(IFID,OFID,J,I,STATUS)

            CALL BDI_GETAXNORM( IFID, J, NORM, STATUS )
            CALL BDI_PUTAXNORM( OFID, I, NORM, STATUS )

          END IF

        END DO

      END IF

*    History component
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMEI( INLINES, TEXTI, STATUS )
      CALL HSI_PTXT( OFID, INLINES, TEXTI, STATUS )
      CALL HSI_PTXT( OFID, HU, HTEXT, STATUS )

*    Copy all ancilliary stuff
      CALL BDI_COPMORE(IFID,OFID,STATUS)

*    Clean up
 99   CALL BDI_RELEASE(OFID,STATUS)
      CALL BDI_RELEASE(IFID,STATUS)
      CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END



*+  BINSUBSET_DISPAX - Display axes
      SUBROUTINE BINSUBSET_DISPAX( FID, PRIM, NDIM, DIMS, AXUNT, AXLO,
     :                                   AXHI, REG, DIR, NAX, STATUS )
*
*    Description :
*
*     Returns the range of axis data in AXMIN,AXMAX (REAL); the axis
*     label in AXLAB; an the axis units in AXUN for the DATA_ARRAY
*     subclass object located by FID.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER			FID			! Dataset identifier
      LOGICAL                PRIM               ! Input primitive?
      INTEGER                NDIM               ! Number of dimensions
      INTEGER                DIMS(*)            ! Length of each axis
*    Export :
      CHARACTER*80           AXUNT(*)           ! Units for each axis
      REAL                   AXLO(*)            ! low value  for each axis
      REAL                   AXHI(*)            ! high value for each axis
      REAL                   DIR(*)             ! axis direction indicator
      LOGICAL                REG(*)             ! Is axis regularly spaced?
      INTEGER                NAX                ! Number of axes
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(80)         AXLAB              ! axis labels

      REAL                   BASE, SCALE

      INTEGER			AXPTR			! Ptr to axis values
      INTEGER                	I                  	! loop variable
      INTEGER                	SIZ                	! dummy

      LOGICAL                	OK
*-

*    Status check
      IF (STATUS .NE. SAI__OK) RETURN

*    Get number of axes
      IF ( .NOT. PRIM ) THEN
        CALL BDI_CHKAXES( FID, NAX, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_FLUSH( STATUS )
          STATUS = SAI__OK
          NAX = 0
        END IF
      ELSE
        NAX = 0
      END IF

      IF ( NAX .GT. 0 ) THEN
        CALL MSG_PRNT ('The axes present are:')

        DO I = 1, NDIM
          CALL BDI_GETAXLABEL( FID, I, AXLAB,    STATUS )
          CALL BDI_GETAXUNITS( FID, I, AXUNT(I), STATUS )
          CALL MSG_SETI ('I', I)
          CALL MSG_SETC ('NAME', AXLAB)
          CALL MSG_PRNT (' ^I ^NAME')
          CALL BDI_CHKAXVAL ( FID, I, OK, REG(I), SIZ, STATUS)

          IF (REG(I)) THEN
            CALL BDI_GETAXVAL ( FID, I, BASE, SCALE, SIZ, STATUS)
            AXLO(I) = BASE
            AXHI(I) = BASE + (SIZ - 1) * SCALE

          ELSE
            CALL BDI_MAPAXVAL( FID, 'READ', I, AXPTR, STATUS )
            CALL ARR_ELEM1R( AXPTR, DIMS(I), 1, AXLO(I), STATUS )
            CALL ARR_ELEM1R( AXPTR, DIMS(I), DIMS(I), AXHI(I), STATUS )
          END IF

*    set direction indicator for axis
          IF (AXHI(I).GT.AXLO(I)) THEN
            DIR(I)=1.0
          ELSEIF (AXHI(I).LT.AXLO(I)) THEN
            DIR(I)=-1.0
          ELSE
            DIR(I)=0.0
          END IF

        END DO
      ELSE

        CALL MSG_SETI( 'ND', NDIM )
        CALL MSG_PRNT( 'Object has ^ND dimensions' )
        DO I = 1, NDIM
          AXLO(I) = 1
          AXLO(I) = DIMS(I)
          DIR(I) = 1.0
          CALL MSG_SETI( 'NA', I )
          CALL MSG_SETI( 'NP', DIMS(I) )
          CALL MSG_PRNT( 'The range for axis ^NA is 1 to ^NP pixels' )
        END DO

      END IF

      IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'BINSUBSET_DISPAX', STATUS)
      END IF

      END




*+  BINSUBSET_AXRAN - Converts selected axis range to pixel values
      SUBROUTINE BINSUBSET_AXRAN(FID,DIMS,NRANGE,RANGES,SEL,DIR,
     :                                             AXRANGE,STATUS)
*    Description :
*     Converts the NRANGE RANGES of the selected axes into pixel values.
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'

      INTEGER                DTA__MXRANG        ! max no. permissible ranges
        PARAMETER           (DTA__MXRANG = 20)
*    Import :
      INTEGER			FID			! Input dataset id
      INTEGER                NRANGE(*)          ! Nuber of ranges for each axis
      INTEGER                DIMS(*)            ! Length of each axis
      REAL                   DIR(*)		! direction indicator
      LOGICAL                SEL(*)             ! Axis selected on?
*    Import-Export :
      REAL                   RANGES(2,DTA__MXRANG,ADI__MXDIM)  ! AXIS ranges
*    Export :
      INTEGER                AXRANGE(2,DTA__MXRANG,ADI__MXDIM) ! AXIS pixel ranges
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                IAX                ! Loop counters
      INTEGER APTR,WPTR

*-
      IF (STATUS.EQ.SAI__OK) THEN


*    Loop over all possible axes
        DO IAX = 1, ADI__MXDIM
          IF (SEL(IAX)) THEN
            CALL BDI_MAPAXVAL(FID,'R',IAX,APTR,STATUS)
            CALL BDI_MAPAXWID(FID,'R',IAX,WPTR,STATUS)
            CALL BINSUBSET_AXRAN_AXISN(%VAL(APTR),%VAL(WPTR),DIMS(IAX),
     :           DIR(IAX),NRANGE(IAX),RANGES(1,1,IAX),AXRANGE(1,1,IAX),
     :                                                          STATUS)
          END IF
        END DO

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'BINSUBSET_AXRAN', STATUS )
        END IF

      END IF

      END




*+  BINSUBSET_AXRAN_AXISN
      SUBROUTINE BINSUBSET_AXRAN_AXISN (AXVAL,AXWID,DIM,DIR,
     :                           NRANGE,RANGES,AXRANGE,STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'

      INTEGER                DTA__MXRANG          ! max no. permissible ranges
        PARAMETER           (DTA__MXRANG = 20)
*    Import :
      INTEGER                NRANGE               ! Nuber of ranges for each axis
      INTEGER                DIM                  ! Length of each axis

      REAL                   AXVAL(*)	          ! Axis values
      REAL                   AXWID(*)		  ! Axis widths
      REAL                   DIR
*    Import-Export :
      REAL                   RANGES(2,DTA__MXRANG)  ! AXIS ranges
*    Export :
      INTEGER                AXRANGE(2,DTA__MXRANG) ! AXIS pixel ranges
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER JR
*-

      IF (STATUS.EQ.SAI__OK) THEN

*      Range selection should work on bin centres
        DO JR = 1,NRANGE
          CALL AXIS_VAL2PIX( DIM, AXVAL, .FALSE., RANGES(1,JR),
     :              RANGES(2,JR), AXRANGE(1,JR), AXRANGE(2,JR),
     :                                                 STATUS )
        END DO

c        IDIM=1
c
c*  loop over selected ranges matching to axis values
c        DO JR = 1, NRANGE
c
c*  match lower bound
c          MATCHLO=.FALSE.
c          DO WHILE (.NOT.MATCHLO.AND.IDIM.LE.DIM)
c*  calculate upper bin boundary
c            UPBOUND=AXVAL(IDIM)+0.5*DIR*AXWID(IDIM)
c*  calculate offset from this
c            OFFSET=(UPBOUND-RANGES(1,JR))*DIR
c*  match found
c            IF (OFFSET.LE.AXWID(IDIM).AND.OFFSET.GT.0.0) THEN
c              AXRANGE(1,JR)=IDIM
c              MATCHLO=.TRUE.
c
c*  match upper bound
c              MATCHUP=.FALSE.
c              JDIM=IDIM
c              DO WHILE (.NOT.MATCHUP.AND.JDIM.LE.DIM)
c                UPBOUND=AXVAL(JDIM)+0.5*DIR*AXWID(JDIM)
c                OFFSET=(UPBOUND-RANGES(2,JR))*DIR
c*  match found
c                IF (OFFSET.LE.AXWID(JDIM).AND.OFFSET.GT.0.0) THEN
c                  AXRANGE(2,JR)=JDIM
c                  MATCHUP=.TRUE.
c* no match found
c                ELSE
c                  JDIM=JDIM+1
c                END IF
c              END DO
c
c* no match yet for lower bound
c            ELSE
c              IDIM=IDIM+1
c            END IF
c
c          END DO
c
c          IDIM=IDIM+1
c
c        END DO

c* trap case of final range value = upper bin boundary
c        IF (MATCHLO.AND..NOT.MATCHUP) THEN
c          AXRANGE(2,NRANGE)=DIM
c        END IF

      END IF
      END





*+  BINSUBSET_SETSEL - Set values to copy to output
      SUBROUTINE BINSUBSET_SETSEL (D1,D2,D3,D4,D5,D6,D7,NRANGE,AXRANGE,
     :                                               KEEP,COPY,STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INTEGER                DTA__MXRANG        ! max no. permissible ranges
        PARAMETER           (DTA__MXRANG = 20)
*    Import :
      INTEGER D1,D2,D3,D4,D5,D6,D7
      INTEGER                NRANGE(ADI__MXDIM)          ! # item ranges
      INTEGER                AXRANGE(2,DTA__MXRANG,ADI__MXDIM)


      LOGICAL                KEEP               ! Keep axis ranges selected?
*    Export :
      LOGICAL                COPY(D1,D2,D3,D4,D5,D6,D7)
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                A,B,C,D,E,F,G,H,I,J,K,L,M,N !Loop counters

*-
      IF (STATUS.EQ.SAI__OK) THEN
        DO A = 1, NRANGE(7)
          DO B = AXRANGE(1,A,7), AXRANGE(2,A,7)
            DO C = 1, NRANGE(6)
              DO D = AXRANGE(1,C,6), AXRANGE(2,C,6)
                DO E = 1, NRANGE(5)
                  DO F = AXRANGE(1,E,5), AXRANGE(2,E,5)
                    DO G = 1, NRANGE(4)
                      DO H = AXRANGE(1,G,4), AXRANGE(2,G,4)
                        DO I = 1, NRANGE(3)
                          DO J = AXRANGE(1,I,3), AXRANGE(2,I,3)
                            DO K = 1, NRANGE(2)
                              DO L = AXRANGE(1,K,2), AXRANGE(2,K,2)
                                DO M = 1, NRANGE(1)
                                  DO N = AXRANGE(1,M,1), AXRANGE(2,M,1)

                                    COPY(N,L,J,H,F,D,B)= KEEP

                                  END DO
                                END DO
                              END DO
                            END DO
                          END DO
                        END DO
                      END DO
                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO

      END IF
      END




*+  BINSUBSET_SEL - Write output DATA, QUALITY & VARIANCE
      SUBROUTINE BINSUBSET_SEL(LEN,IDATA,IVAR,IQUAL,COPY,VAROK,QUALOK,
     :                                          ODATA,OVAR,OQUAL,STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER                DTA__MXRANG        ! max no. permissible ranges
        PARAMETER           (DTA__MXRANG = 20)

*    Import :
      INTEGER                LEN                ! Length of input dataset
      REAL                   IDATA(*)           ! Input data array
      REAL                   IVAR(*)            ! Input variance
      BYTE                   IQUAL(*)           ! Input quality

      LOGICAL                COPY(*)            ! Elements to copy to output

      LOGICAL                VAROK              ! Variance OK
      LOGICAL                QUALOK             ! Quality OK

*    Export :
      REAL                   ODATA(*)           ! Output data
      REAL                   OVAR(*)            ! Output variance
      BYTE                   OQUAL(*)           ! Output quality
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                I, J            	! Counters
*-
      IF (STATUS.EQ.SAI__OK) THEN

* copy data
        J=0
        DO I=1,LEN
          IF (COPY(I)) THEN
            J=J+1
            ODATA(J)=IDATA(I)
          END IF
        END DO

* copy variance if present
        IF (VAROK) THEN
          J=0
          DO I=1,LEN
            IF (COPY(I)) THEN
              J=J+1
              OVAR(J)=IVAR(I)
            END IF
          END DO
        END IF

* copy QUALITY if present
        IF (QUALOK) THEN
          J=0
          DO I=1,LEN
            IF (COPY(I)) THEN
              J=J+1
              OQUAL(J)=IQUAL(I)
            END IF
          END DO
        END IF

      END IF

      END





*+  BINSUBSET_AXCOP - Write output axis values & widths
      SUBROUTINE BINSUBSET_AXCOP (NPTS, NR, R, KEEP, IAXIS, IWIDTH,
     :                                 WDCOP, OAXIS, OWIDTH, STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER                NPTS               ! Length of input axis
      INTEGER                NR                 ! Number of ranges
      INTEGER                R(2,NR)            ! Axis pixel values of ranges

      LOGICAL                KEEP               ! Are ranges those to keep?
      LOGICAL                WDCOP              ! Copy width data?

      REAL                   IAXIS(*)           ! Input axis
      REAL                   IWIDTH(*)          ! Input width

*    Export :
      REAL                   OAXIS(*)           ! Output axis
      REAL                   OWIDTH(*)          ! Output width
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                I, J, K            ! Counters
*-
      IF (STATUS.NE.SAI__OK) RETURN

* data to be kept
      IF (KEEP) THEN
        K = 1
        DO I = 1, NR
          DO J = R(1,I), R(2,I)
            OAXIS(K)=IAXIS(J)
            IF (WDCOP) THEN
              OWIDTH(K) = IWIDTH(J)
            END IF

            K= K + 1

          END DO
        END DO

* data to be discarded
      ELSE
        K=1
        J = 1
        DO I = 1, NPTS
          IF (J .LE. NR) THEN
            IF (I .LT. R(1,J)) THEN
              OAXIS(K)  = IAXIS(I)
              IF (WDCOP) THEN
                OWIDTH(K) = IWIDTH(I)
              END IF
              K = K + 1

            ELSE IF (I .EQ. R(2,J)) THEN
              J  = J + 1

            END IF
          ELSE
            OAXIS(K)  = IAXIS(I)
            IF (WDCOP) THEN
              OWIDTH(K) = IWIDTH(I)
            END IF
            K = K + 1

          END IF
        END DO

      END IF

      END
