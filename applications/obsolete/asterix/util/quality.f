*+  QUALITY - Manipulate quality values
      SUBROUTINE QUALITY( STATUS )
*
*    Description :
*
*     Allows manipulation of QUALITY values. Availible options are:
*
*       IGNORE  - Set temporary bad quality bit
*       RESTORE - Unset temporary bad quality bit
*       SET     - Set quality to specified value
*       AND     - AND existing QUALITY with specified QUALITY value
*       OR      - OR existing QUALITY with specified QUALITY value
*       EOR     - EOR existing QUALITY with specified QUALITY value
*       NOT     - NOT existing QUALITY with specified QUALITY value
*
*     Changes are made within specified axis ranges, and or data value
*     ranges.
*
*    Environment Parameters :
*
*     IGNORE    - Set temp bad qual bit?                 (Logical(F), read)
*     RESTORE   - Unset temp bad qual bit?               (Logical(F), read)
*     SET       - Set quality to specified value?        (Logical(F), read)
*     AND       - AND qualiity with specified value?     (Logical(F), read)
*     OR        - OR qualiity with specified value?      (Logical(F), read)
*     EOR       - EOR qualiity with specified value?     (Logical(F), read)
*     NOT       - NOT existing quality value?            (Logical(F), read)
*     OVERWRITE - Overwrite input file?                  (Logical(T), read)
*     AXSEL     - Specify axis ranges for change?        (Logical(T), read)
*     DATSEL    - Specify data ranges for change?        (Logical(T), read)
*     QSEL      - Specify a quality value to alter?      (Logical(F), read)
*     MAGIC     - Select on magic values                 (Logical(F), read)
*     INP       - Input file name.                       (Univ, read)
*     AUXIN     - Input file name.                       (Univ, read)
*     OUT       - Output filename                        (Univ, write)
*     QVAL      - Specified quality value                (Byte, read)
*     SELAX     - Select axes to have ranges applied     (Character, read)
*     AXRNG1..7 - Specified axis ranges                  (Character, read)
*     DATRNG    - Specified data value ranges            (Character, read)
*     MODQUAL   - Quality value to modify                (Character, read)
*     QVAL      - Specified quality value                (Character, read)
*
*    Method :
*
*     Map output quality & pretend is 7 dimensional.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Phil Andrews
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     19 Jun 89 : Original  (PLA_AST88@uk.ac.bham.sr.star)
*     29 Jan 90 : Slight change to avoid rounding at extremes of ranges (RJV)
*      2 Feb 90 : V1.0-2  Magic value handling plus general improvements
*                           in behaviour and code (RJV)
*      4 Jul 90 : V1.2-0  Axis ranges bug fix (DJA)
*     10 Apr 91 : V1.4-0  _GETQV routine added. # points modified is now
*                         reported (DJA)
*     20 May 91 : V1.4-1  Bug in magic handling fixed (DJA)
*     23 May 91 : V1.4-2  Adjustment to upper bound trap tolerance in
*                         AXIS_VAL2PIX (DJA)
*     20 May 92 : V1.6-1  Includes handling of spatial files (RDS)
*     19 Nov 92 : V1.7-0  Axis bound fix in AXIS_VAL2PIX (DJA)
*      3 Jun 93 : V1.7-1  Use VAL__ constants for magic value (DJA)
*     20 Sep 93 : V1.7-2  Auxilliary array handling added (DJA)
*     24 Feb 94 : V1.7-3  Bug fix in _GETQV causing crash on UNIX (DJA)
*     25 Feb 94 : V1.7-4  Use BIT_ routines to do bit manipulations (DJA)
*     29 Jun 94 : V1.8-0  Fixed bug affecting N-d > 2 datasets in ARD
*                         file mode when X,Y not 1st 2 dimensions (DJA)
*      7 Aug 94 : V1.8-1  Fixed astonishing bug in DATSEL mode, where 1.0e-4
*                         was used as a guard value regardless of the size
*                         of the input data. Use VAL__EPSR instead (DJA)
*     31 Aug 94 : V1.8-2  Fixed another missing initialisation in ARD mode,
*                         which stopped it working on UNIX (DJA)
*     24 Nov 94 : V1.8-3  Now use USI for user interface (DJA)
*      5 Dec 94 : V1.8-4  ARD and other modes no longer exclusive (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                CHR_LEN
      BYTE		     BIT_NOTUB
*
*    Local Constants :
*
      INTEGER                MX_BNDS                ! max no. permissible bounds
        PARAMETER           (MX_BNDS = 100)
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) AUXLOC                 ! Auxilliary data object
      CHARACTER*(DAT__SZLOC) DLOC                   ! Locator to data array
      CHARACTER*(DAT__SZLOC) ILOC                   ! Locator to input file
      CHARACTER*(DAT__SZLOC) OLOC                   ! Locator to output file
      CHARACTER*80           AXLABEL(DAT__MXDIM)    ! Axis labels
      CHARACTER*(PAR__SZNAM) PAR                    ! Parameter name
      CHARACTER*8            MODQUAL                ! Quality to modify
      CHARACTER*80           TEXT(2)                ! History text
      CHARACTER*8            QVAL                   ! Quality value
      CHARACTER*40           DEF                    ! Default value string

      REAL                   AXLO(DAT__MXDIM)       ! lo axis value
      REAL                   AXHI(DAT__MXDIM)       ! hi axis value
      REAL                   DIR(DAT__MXDIM)        ! axis direction indicator
      REAL                   AXRANGES(2,MX_BNDS,DAT__MXDIM)  ! Axis ranges
      REAL                   DMIN, DMAX             ! Data min, max
      REAL                   DRANGES(2,MX_BNDS)     ! Data ranges

      INTEGER                AXPTR(DAT__MXDIM)      ! Mapped axes
      INTEGER                CPTR                   ! Pointer to dynamic
                                                    ! array
      INTEGER                DIMS(DAT__MXDIM)       ! Length of each dimension
      INTEGER                DPTR                   ! Pointer to mapped
                                                    ! data array
      INTEGER                I, J                   ! Loop counters
      INTEGER                JUNK
      INTEGER                NAXES                  ! Number of selected
                                                    ! axes
      INTEGER                NAXRANGES(DAT__MXDIM)  ! Number of ranges
                                                    ! for each axis
      INTEGER                NCH                    ! # changed pixels
      INTEGER                NDIM                   ! Number of dimensions
      INTEGER                NDRANGES               ! Number of data ranges
      INTEGER                NELM                   ! Number of data points
      INTEGER                NFROM                  ! # user selected points
      INTEGER                NLINES                 ! Number of TEXT lines
      INTEGER                NMOD                   ! # modified points
      INTEGER                PIXRNG(2,MX_BNDS,DAT__MXDIM)    ! Pixel ranges
      INTEGER                QDIMS(DAT__MXDIM)      ! Length of each dimension
      INTEGER                QNDIM                  ! Number of dimensions
      INTEGER                QPTR                   ! Pointer to mapped
                                                    ! QUALITY
      INTEGER                AXES(DAT__MXDIM)       ! Selected axes

      BYTE                   BQVAL		    ! Quality value

      LOGICAL                AXSEL                  ! Select on axis ranges?
      LOGICAL                DATSEL                 ! Select on data ranges?
      LOGICAL                FSEL                   ! Select on spatial file ?
      LOGICAL                GOTAUX                 ! Got auxilliary file ?
      LOGICAL 		     MAGIC		    ! Select on magic values?
      LOGICAL                MODESET                ! Mode selected
      LOGICAL                INPRIM                 ! Is input primitive?
      LOGICAL                OK                     ! OK?
      LOGICAL                OVERWRITE              ! Overwrite input dataset
      LOGICAL        	     Q_IGNORE		    ! Mode switches
      LOGICAL        	     Q_RESTORE
      LOGICAL        	     Q_SET
      LOGICAL        	     Q_AND
      LOGICAL        	     Q_OR
      LOGICAL        	     Q_EOR
      LOGICAL        	     Q_NOT
      LOGICAL                QSEL                   ! Select QUALITY
                                                    ! Value to modify?
      LOGICAL                SEL(DAT__MXDIM)        ! Select this axis?
*
*    Version id :
*
      CHARACTER*80           VERSION
        PARAMETER            ( VERSION = 'QUALITY Version 1.8-4' )
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialize
      CALL AST_INIT()

*    Overwrite?
      CALL USI_GET0L( 'OVERWRITE', OVERWRITE, STATUS )

*    Get input
      IF ( OVERWRITE ) THEN
        CALL USI_ASSOCI( 'INP', 'UPDATE', ILOC, INPRIM, STATUS )
        OLOC = ILOC
      ELSE
        CALL USI_ASSOC2( 'INP', 'OUT', 'R', ILOC, OLOC, INPRIM, STATUS )
        CALL HDX_COPY( ILOC, OLOC, STATUS )
      END IF

*    Check DATA
      CALL BDA_CHKDATA( OLOC, OK, NDIM, DIMS, STATUS )
      IF (.NOT. OK) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid dataset', STATUS )
        GOTO 99
      END IF

*    Number of modified values
      NMOD = 0

*    Obtain mode from user
      MODESET=.FALSE.
      CALL USI_GET0L( 'SET', Q_SET, STATUS )
      MODESET = Q_SET
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'IGNORE', Q_IGNORE, STATUS )
        MODESET = Q_IGNORE
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'RESTORE', Q_RESTORE, STATUS )
        MODESET = Q_RESTORE
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'AND', Q_AND, STATUS )
        MODESET = Q_AND
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'OR', Q_OR, STATUS )
        MODESET = Q_OR
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'EOR', Q_EOR, STATUS )
        MODESET = Q_EOR
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'NOT', Q_NOT, STATUS )
        MODESET = Q_NOT
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check that a mode has been selected
      IF ( .NOT. MODESET ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No QUALITY operation selected', STATUS )
        GOTO 99
      END IF

*    If in SET mode see if selecting on magic values
      IF ( Q_SET ) THEN
        CALL USI_GET0L( 'MAGIC', MAGIC, STATUS )
      ELSE
        MAGIC = .FALSE.
      END IF

*    Not selecting on magic values, so see how selection to be made
      IF ( .NOT. MAGIC ) THEN

*      Select pixels using a spatial descriptor file ?
        CALL USI_GET0L( 'FSEL', FSEL, STATUS )

*      Only allow axis selection if more than 2 dimensions
        IF ( (NDIM.GT.2) .OR. .NOT. FSEL ) THEN
          CALL USI_GET0L( 'AXSEL', AXSEL, STATUS )
        ELSE
          AXSEL = .FALSE.
        ENDIF

        CALL USI_GET0L( 'DATSEL', DATSEL, STATUS )
        CALL USI_GET0L( 'QSEL', QSEL, STATUS )

      ELSE
        AXSEL = .FALSE.
        DATSEL = .FALSE.
        QSEL = .FALSE.

      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check inprim
      IF ( INPRIM ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input not a dataset!', STATUS )
        GOTO 99
      END IF

*    Find length of array
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Check QUALITY
      CALL BDA_CHKQUAL( OLOC, OK, QNDIM, QDIMS, STATUS )

      IF ( .NOT. OK ) THEN
        IF ( Q_SET .OR. Q_IGNORE ) THEN
          CALL BDA_CREQUAL( OLOC, NDIM, DIMS, STATUS )
          CALL BDA_MAPQUAL( OLOC, 'WRITE', QPTR, STATUS )
          CALL ARR_INIT1B( QUAL__GOOD, NELM, %VAL(QPTR), STATUS )
          CALL BDA_PUTMASK( OLOC, QUAL__MASK, STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'No input quality !', STATUS )
          GOTO 99

        END IF
      ELSE
        CALL BDA_MAPQUAL( OLOC, 'UPDATE', QPTR, STATUS )
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Adjust size of array to 7 D
      DO I = NDIM + 1, DAT__MXDIM
        DIMS(I) = 1
      END DO

*    Set up temporary array for flagging selections
      CALL DYN_MAPL( 1, NELM, CPTR, STATUS )

*    Auxilliary data object?
      IF ( DATSEL .OR. MAGIC ) THEN
        CALL USI_ASSOCI( 'AUXIN', 'READ', AUXLOC, INPRIM, STATUS )
        IF ( STATUS .EQ. PAR__NULL ) THEN
          CALL ERR_ANNUL( STATUS )
          GOTAUX = .FALSE.
          CALL DAT_CLONE( ILOC, DLOC, STATUS )
        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99
        ELSE
          GOTAUX = .TRUE.
          CALL DAT_CLONE( AUXLOC, DLOC, STATUS )
        END IF
      END IF

*    Initialise selection array
      CALL ARR_INIT1L( ( .NOT. MAGIC), NELM, %VAL(CPTR), STATUS )
      IF ( MAGIC ) THEN
        NMOD = 0
      ELSE
        NMOD = NELM
      END IF

*    Select on axis values?
      IF ( AXSEL ) THEN

*      Display axes
        CALL QUALITY_DISPAX( OLOC, NDIM, DIMS, AXLABEL, AXLO, AXHI,
     :                                                DIR, STATUS )

*      Construct allowed ranges
        DO I = 1, DAT__MXDIM
          NAXRANGES(I)  = 1
          AXRANGES(1,1,I) = AXLO(I)
          AXRANGES(2,1,I) = AXHI(I)
          PIXRNG(1,1,I)=1
          PIXRNG(2,1,I)=DIMS(I)
          SEL(I) = .FALSE.
        END DO

*      Get axes to select on
        IF ( NDIM .GT. 1 ) THEN
          CALL MSG_PRNT( ' ' )
          CALL PRS_GETLIST( 'SELAX', NDIM, AXES, NAXES, STATUS )

*        Check status
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        ELSE
          NAXES   = 1
          AXES(1) = 1
        END IF

*  get ranges for each selected axis
        DO I = 1, NAXES
          J=AXES(I)
          SEL(J) = .TRUE.

          CALL MSG_PRNT( ' ' )
          CALL MSG_SETC('LABEL', AXLABEL(J))
          CALL MSG_PRNT('     ^LABEL Axis:')
          CALL MSG_SETR( 'MIN', AXLO(J) )
          CALL MSG_SETR( 'MAX', AXHI(J) )
          CALL MSG_PRNT( 'Ranges from ^MIN to ^MAX')
          WRITE( PAR, '(A5,I1)') 'AXRNG', I
          CALL MSG_SETR('MIN', AXLO(J) )
          CALL MSG_SETR('MAX', AXHI(J) )
          CALL MSG_MAKE( '^MIN:^MAX', DEF, JUNK )
          CALL USI_DEF0C( PAR, DEF(1:CHR_LEN(DEF)), STATUS )
          CALL PRS_GETRANGES( PAR, MX_BNDS, 1, AXLO(J), AXHI(J),
     :                   AXRANGES(1,1,J), NAXRANGES(J), STATUS )

*      Convert axis value ranges to pixel ranges
          CALL BDA_MAPAXVAL (OLOC, 'R', J, AXPTR(J), STATUS)
          CALL QUALITY_AXRAN (DIMS(J),NAXRANGES(J),AXRANGES(1,1,J),
     :                     %VAL(AXPTR(J)),DIR(J),PIXRNG(1,1,J),STATUS)
          CALL BDA_UNMAPAXVAL( OLOC, J, STATUS )

        END DO

*      Set up selection array
        CALL QUALITY_SETAXSEL(NDIM, DIMS, NAXRANGES, PIXRNG, %VAL(CPTR),
     :                                                    NMOD, STATUS )

*    End of axis selection option
      END IF

*    Use a spatial file (ARD file) to select pixels wanted?
      IF ( FSEL ) THEN

*      Read some axis values
        CALL QUALITY_DISPAX( ILOC, NDIM, DIMS, AXLABEL, AXLO, AXHI,
     :                                                DIR, STATUS )

*      Read ARD file and set pixel values wanted
        CALL QUALITY_SETFSEL( ILOC, NDIM, DIMS, AXLO, AXHI,
     :                                 NMOD, CPTR, STATUS )

*    End of ARD file selection option
      END IF

*    No axis selection - assume all wanted unless MAGIC mode
      IF ( .NOT. (FSEL.OR.AXSEL) ) THEN
        CALL ARR_INIT1L( ( .NOT. MAGIC), NELM, %VAL(CPTR), STATUS )
        IF ( MAGIC ) THEN
          NMOD = 0
        ELSE
          NMOD = NELM
        END IF
      END IF

*    Get data value selection
      IF ( DATSEL ) THEN
        CALL BDA_MAPDATA( DLOC, 'READ', DPTR, STATUS )
        CALL ARR_RANG1R( NELM, %VAL(DPTR), DMIN, DMAX, STATUS )

        CALL MSG_PRNT( ' ' )
        CALL MSG_SETR( 'MIN', DMIN )
        CALL MSG_SETR( 'MAX', DMAX )
        CALL MSG_PRNT( 'Data values range from ^MIN to ^MAX' )
        CALL MSG_SETR( 'MIN', DMIN )
        CALL MSG_SETR( 'MAX', DMAX )
        CALL MSG_MAKE(' ^MIN:^MAX', DEF, JUNK)
        CALL USI_DEF0C( 'DATRNG', DEF, STATUS )
        CALL PRS_GETRANGES( 'DATRNG', MX_BNDS, 1, DMIN, DMAX, DRANGES,
     :                                              NDRANGES, STATUS )

*      Check status
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      if ranges go to limits adjust slightly to ensure inclusion
        IF ( DRANGES(1,1)*(1.0-VAL__EPSR) .LE. DMIN ) THEN
          DRANGES(1,1) = MIN(DRANGES(1,1),DMIN * (1.0 - VAL__EPSR))
        END IF
        IF ( DRANGES(2,NDRANGES)*(1.0+VAL__EPSR) .GE. DMAX) THEN
          DRANGES(2,NDRANGES) = MAX(DRANGES(2,NDRANGES),
     :                              DMAX* (1.0 + VAL__EPSR))
        END IF

        CALL QUALITY_SETDSEL( NELM, NDRANGES, DRANGES, %VAL(DPTR),
     :                                  %VAL(CPTR), NMOD, STATUS )

      END IF

*  selecting on magic values
      IF ( MAGIC ) THEN
        CALL BDA_MAPDATA( DLOC, 'READ', DPTR, STATUS )
        CALL QUALITY_SETMSEL( NELM, %VAL(DPTR), %VAL(CPTR), NMOD,
     :                                                   STATUS )
      END IF

*    Get quality value to modify
      IF ( QSEL ) THEN
        CALL QUALITY_GETQV( 'MODQUAL', ' ', MODQUAL, BQVAL, STATUS )
        CALL QUALITY_SETQSEL( NELM, BQVAL, %VAL(QPTR), %VAL(CPTR),
     :                                              NMOD, STATUS )
      END IF

*    Write the output quality
      IF ( Q_SET ) THEN
        IF ( MAGIC ) THEN
          CALL USI_DEF0C( 'QVAL','00000001', STATUS )
          CALL QUALITY_GETQV( 'QVAL', 'Quality value corresponding to'/
     :                          /' magic values', QVAL, BQVAL, STATUS )
          TEXT(1)='QUALITY '//QVAL//' set for points with magic values'
        ELSE
          CALL QUALITY_GETQV( 'QVAL', 'New quality value', QVAL,
     :                                           BQVAL, STATUS )
          TEXT(1) = '     QUALITY set to '//QVAL
        END IF
        CALL QUALITY_SET( NELM, BQVAL, %VAL(CPTR), %VAL(QPTR), NCH,
     :                                                     STATUS )

      ELSE IF ( Q_IGNORE ) THEN
        BQVAL = QUAL__IGNORE
        CALL QUALITY_OR( NELM, BQVAL, %VAL(CPTR), %VAL(QPTR), NCH,
     :                                                    STATUS )
        TEXT(1) = '     IGNORE mode'

      ELSE IF ( Q_RESTORE ) THEN
        BQVAL = BIT_NOTUB(QUAL__IGNORE)
        CALL QUALITY_AND( NELM, BQVAL, %VAL(CPTR), %VAL(QPTR), NCH,
     :                                                     STATUS )
        TEXT(1) = '     RESTORE mode'

      ELSE IF ( Q_AND ) THEN
        CALL QUALITY_GETQV( 'QVAL', 'Value for AND operation', QVAL,
     :                                               BQVAL, STATUS )
        CALL QUALITY_AND( NELM, BQVAL, %VAL(CPTR), %VAL(QPTR), NCH,
     :                                                     STATUS )
        TEXT(1) = '     QUALITY ANDed with '//QVAL

      ELSE IF ( Q_OR ) THEN
        CALL QUALITY_GETQV( 'QVAL', 'Value for OR operation', QVAL,
     :                                              BQVAL, STATUS )
        CALL QUALITY_OR( NELM, BQVAL, %VAL(CPTR), %VAL(QPTR), NCH,
     :                                                    STATUS )
        TEXT(1) = '     QUALITY ORed with '//QVAL

      ELSE IF ( Q_EOR ) THEN
        CALL QUALITY_GETQV( 'QVAL', 'Value for EOR operation', QVAL,
     :                                               BQVAL, STATUS )
        CALL QUALITY_EOR( NELM, BQVAL, %VAL(CPTR), %VAL(QPTR), NCH,
     :                                                     STATUS )
        TEXT(1) = '     QUALITY EORed with '//QVAL

      ELSE IF ( Q_NOT ) THEN
        CALL QUALITY_NOT( NELM, %VAL(CPTR), %VAL(QPTR), NCH, STATUS )
        TEXT(1) = '     QUALITY complemented'

      END IF

      CALL BDA_UNMAPQUAL( OLOC, STATUS )

*    Inform user of altered points
      NFROM = NMOD
      IF ( AXSEL .AND. DATSEL ) THEN
        CALL MSG_SETC( 'FROM', 'axis/data range' )
      ELSE IF ( AXSEL ) THEN
        CALL MSG_SETC( 'FROM', 'axis range' )
      ELSE IF ( FSEL ) THEN
        CALL MSG_SETC( 'FROM', 'area specified by ARD file' )
      ELSE
        CALL MSG_SETC( 'FROM', 'data range' )
      END IF
      CALL MSG_SETI( 'NCH', NCH )
      CALL MSG_SETI( 'NMOD', NFROM )
      CALL MSG_PRNT( '^NCH points had quality altered out of'/
     :                               /' ^NMOD points in ^FROM' )

*    Write history
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      NLINES = 1
      IF ( QSEL ) THEN
        TEXT(2) = ' Only points having QUALITY = '//MODQUAL//' altered'
        NLINES  = 2
      END IF

      CALL HIST_PTXT( OLOC, NLINES, TEXT, STATUS )

*    Auxilliary array used?
      IF ( GOTAUX ) THEN
        CALL BDA_RELEASE( DLOC, STATUS )
        CALL DAT_ANNUL( DLOC, STATUS )
      END IF

*    Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  QUALITY_DISPAX - Display axes
      SUBROUTINE QUALITY_DISPAX (LOC,NDIM,DIMS,AXLABEL,
     :                                  AXLO,AXHI,DIR, STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Jim Peden (BHVAD::JCMP)
*
*    History :
*
*     21 Nov 85 : Original (BHVAD::JCMP)
*     18 Sep 86 : ERR_FLUSH -> ERR_ANNUL (BHVAD::JCMP)
*     28 Sep 88 : ASTERIX88 (BHVAD::ADM)
*      2 Feb 90 : Now returns direction indicator (RJV)
*
*    Type Definitions :
*
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) LOC                ! locator to container object

      INTEGER                NDIM               ! Number of dimensions
      INTEGER                DIMS(*)            ! Length of each axis
*
*    Export :
*
      CHARACTER*80           AXLABEL(*)         ! Units for each axis

      REAL                   AXLO(*)            ! lo value for each axis
      REAL                   AXHI(*)            ! hi value for each axis
      REAL                   DIR(*)             ! direction indicator
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) CELL               ! Locator to AXIS DATA_ARRAY cell
      CHARACTER*(DAT__SZLOC) AXLOC              ! Locator to AXIS structure

      INTEGER                I                  ! loop variable
      INTEGER                SIZ                ! dummy

      LOGICAL                REG                ! Is axis regularly spaced?
      LOGICAL                OK

      REAL                   BASE, SCALE
*-

*    Status check
      IF (STATUS .NE. SAI__OK) RETURN

      CALL MSG_PRNT ('The axes present are:')

      DO I = 1, NDIM
*  get label
        CALL BDA_GETAXLABEL (LOC, I, AXLABEL(I), STATUS)
        CALL MSG_SETI ('I', I)
        CALL MSG_SETC ('NAME', AXLABEL(I))
        CALL MSG_PRNT (' ^I ^NAME')

*  check values
        CALL BDA_CHKAXVAL (LOC, I, OK, REG, SIZ, STATUS)

* find lo and hi values
        IF ( REG ) THEN
          CALL BDA_GETAXVAL (LOC, I, BASE, SCALE, SIZ, STATUS)
          AXLO(I) = BASE
          AXHI(I) = BASE + (SIZ - 1) * SCALE

         ELSE
           CALL BDA_LOCAXVAL( LOC, I, AXLOC, STATUS )
           CALL DAT_CELL( AXLOC, 1, 1, CELL, STATUS )
           CALL DAT_GET0R( CELL, AXLO(I), STATUS )
           CALL DAT_ANNUL( CELL, STATUS )
           CALL DAT_CELL( AXLOC, 1, DIMS(I), CELL, STATUS )
           CALL DAT_GET0R( CELL, AXHI(I), STATUS )
           CALL DAT_ANNUL( CELL, STATUS )
         END IF

*  set direction indicator
         DIR(I) = SIGN(1.0,AXHI(I)-AXLO(I))

       END DO

       IF ( STATUS .NE. SAI__OK ) THEN
         CALL AST_REXIT( 'QUALITY_DISPAX', STATUS )
       END IF

       END




*+  QUALITY_AXRAN - Converts selected axis range to pixel values
      SUBROUTINE QUALITY_AXRAN (DIM,NRANGE,RANGES,AXIS,DIR,PIXRNG,
     :                                                    STATUS )
*    Description :
*     Converts the axis value ranges into pixel ranges
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

      INTEGER                MX_BNDS            ! max no. permissible bounds
        PARAMETER           (MX_BNDS = 100)
*    Import :
      INTEGER                NRANGE              ! Number of ranges
      INTEGER                DIM                 ! Length of each axis

      REAL                   AXIS(DIM)        	 ! Axis values
      REAL                   DIR                 ! direction indicator

*    Import-Export :
      REAL                   RANGES(2,MX_BNDS)   ! AXIS ranges

*    Export :
      INTEGER                PIXRNG(2,MX_BNDS)   ! AXIS pixel ranges
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                I                   ! Loop counters
*-
      IF (STATUS.EQ.SAI__OK) THEN

        DO I = 1,NRANGE
          CALL AXIS_VAL2PIX( DIM, AXIS, .FALSE., RANGES(1,I),
     :           RANGES(2,I), PIXRNG(1,I), PIXRNG(2,I), STATUS )
        END DO

      END IF

      END



*+  QUALITY_SETAXSEL - Use axis ranges to select valid output pixels
      SUBROUTINE QUALITY_SETAXSEL(NDIM,DIMS,NRANGE,AXRANGE,SELECT,
     :                                              NMOD, STATUS )
*    Description :
*    History :
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                NDIM               ! # dimensions
      INTEGER                DIMS(*)            ! DATA_ARRAY dimensions
      INTEGER                NRANGE(*)          ! # item ranges
      INTEGER                AXRANGE(*)         ! axis pixel ranges
*
*    Export :
*
      LOGICAL                SELECT(*)
      INTEGER                NMOD               ! # points modified
*
*    Status :
*
      INTEGER STATUS
*-
      IF (STATUS.NE.SAI__OK) RETURN

      CALL QUALITY_SETAXSEL_INT(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5),
     :                       DIMS(6),DIMS(7),NRANGE,AXRANGE,SELECT,NMOD)

      END



      SUBROUTINE QUALITY_SETAXSEL_INT (D1,D2,D3,D4,D5,D6,D7,
     :                        NRANGE, AXRANGE, SELECT, NMOD)
*    Description :
*     Loops over input data array setting SELECT = .TRUE. if values lie
*     within selected ranges.
*    History :
*    Type definitions :
      IMPLICIT NONE
      INTEGER                MX_BNDS            ! max no. permissible bounds
        PARAMETER           (MX_BNDS = 100)

*    Import :
      INTEGER                D1,D2,D3,D4,D5,D6,D7 ! DATA_ARRAY dimensions
      INTEGER                NRANGE(*)            ! # item ranges
      INTEGER                AXRANGE(2,MX_BNDS,7) ! axis pixel ranges

*    Export :
      LOGICAL                SELECT(D1,D2,D3,D4,D5,D6,D7)
      INTEGER                NMOD                 ! # points modified

*    Local variables :
      INTEGER                A,B,C,D,E,F,G,H,I,J,K,L,M,N !Loop counters

*-
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
                                  IF ( .NOT. SELECT(N,L,J,H,F,D,B) )THEN
                                    SELECT(N,L,J,H,F,D,B) = .TRUE.

                                    NMOD = NMOD + 1
                                  END IF
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
      END




*+  QUALITY_SETDSEL - Alter SELECT according to data ranges
      SUBROUTINE QUALITY_SETDSEL(LEN,NDRANGES,DRANGES,DATA,SELECT,
     :                                              NMOD, STATUS )
*    Description :
*     Sets the SELECT array to FALSE if data values are outside specified
*     ranges
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                LEN                    ! Number of data points
      INTEGER                NDRANGES               ! Number of data ranges

      REAL                   DRANGES(2,*)           ! data ranges bounds
      REAL                   DATA(*)                ! Data values

*    Import-Export :
      LOGICAL                SELECT(*)              ! Copy to output?
      INTEGER                NMOD                   ! # points modified

*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                I, J                   ! Loop counters
      LOGICAL                FOUND
*-

      IF (STATUS.NE.SAI__OK) RETURN

      DO I = 1, LEN

*  only take points already selected
        IF (SELECT(I)) THEN

*  see if data point lies within one of specified ranges
          FOUND = .FALSE.
          J     = 1
          DO WHILE (.NOT.FOUND.AND.J.LE.NDRANGES)
            IF (DATA(I).GE.DRANGES(1,J) .AND.
     :          DATA(I).LE.DRANGES(2,J))     THEN
              FOUND = .TRUE.
            ELSE
              J=J+1
            END IF
          END DO

*  if not within specified ranges then deselect it
          IF (.NOT. FOUND) THEN
            SELECT(I) = .FALSE.
            NMOD = NMOD - 1
          END IF

        END IF

      END DO

      END



*+  QUALITY_SETQSEL - Alter SELECT according to QUALITY value
      SUBROUTINE QUALITY_SETQSEL( LEN, QVAL, QUAL, SELECT, STATUS )
*    Description :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :
      INTEGER                LEN                    ! Number of data points
      BYTE                   QVAL
      BYTE                   QUAL(LEN)
*    Import-Export :
      LOGICAL                SELECT(LEN)
      INTEGER                NMOD                   ! # points modified

*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER I
*-

      IF (STATUS.NE.SAI__OK) RETURN

      DO I = 1, LEN
        IF ( SELECT(I) ) THEN
          IF ( QVAL .NE. QUAL(I) ) THEN
            SELECT(I) = .FALSE.
            NMOD = NMOD - 1
          END IF
        END IF
      END DO

      END



*+  QUALITY_SETMSEL - Alter SELECT according to magic values in data
      SUBROUTINE QUALITY_SETMSEL (LEN,D,SELECT,NMOD,STATUS)
*    Description :
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*
*    Import :
*
      INTEGER                LEN                    ! Number of data points
      REAL                   D(*)
*
*    Import-Export :
*
      LOGICAL                SELECT(*)
      INTEGER                NMOD                   ! # points modified
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER I
*-

      IF (STATUS.NE.SAI__OK) RETURN

      DO I = 1, LEN
        IF ( D(I) .EQ. VAL__BADR ) THEN
          SELECT(I) = .TRUE.
          NMOD = NMOD + 1
        END IF
      END DO

      END



*+  QUALITY_SET - Set quality to specified value
      SUBROUTINE QUALITY_SET(LEN,QVAL,SELECT,QUAL,NCH,STATUS)
*    Description :
*     Sets quality to QVAL for selected points
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :
      INTEGER                LEN                    ! Number of data points
      BYTE                   QVAL
      LOGICAL                SELECT(*)              ! Alter this quality?

*    Import-Export :
      BYTE                   QUAL(*)                ! Quality values
*    Export :
      INTEGER                NCH                    ! # changed pixels
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                I                      ! Loop counter
*-

      IF (STATUS.EQ.SAI__OK) THEN
        NCH = 0
        DO I = 1, LEN
          IF (SELECT(I)) THEN
            IF ( QUAL(I) .NE. QVAL ) THEN
              QUAL(I)=QVAL
              NCH = NCH + 1
            END IF
          END IF
        END DO
      END IF
      END



*+  QUALITY_AND - AND quality with specified value
      SUBROUTINE QUALITY_AND (LEN,QVAL,SELECT,QUAL,NCH,STATUS)
*    Description :
*     AND quality with specified value
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                LEN                    ! Number of data points
      BYTE                   QVAL
      LOGICAL                SELECT(*)              ! Alter this quality?

*    Import-Export :
      BYTE                   QUAL(*)                ! Quality values
*    Export :
      INTEGER                NCH                    ! # changed pixels
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local variables :
      INTEGER                I                      ! Loop counter
      BYTE                   QOLD
*-

      IF (STATUS.EQ.SAI__OK) THEN
        NCH = 0
        DO I = 1, LEN
          IF (SELECT(I)) THEN
            QOLD = QUAL(I)
            QUAL(I)= BIT_ANDUB(QUAL(I),QVAL)
            IF ( QUAL(I) .NE. QOLD ) NCH = NCH + 1
          END IF
        END DO
      END IF
      END



*+  QUALITY_OR - OR quality with specified value
      SUBROUTINE QUALITY_OR(LEN,QVAL,SELECT,QUAL,NCH,STATUS)
*    Description :
*     OR quality with specified value
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                LEN                    ! Number of data points
      BYTE                   QVAL
      LOGICAL                SELECT(*)              ! Alter this quality?

*    Import-Export :
      BYTE                   QUAL(*)                ! Quality values
*    Export :
      INTEGER                NCH                    ! # changed pixels
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ORUB
*    Local variables :
      INTEGER                I                      ! Loop counter
      BYTE                   QOLD
*-

      IF (STATUS.EQ.SAI__OK) THEN
        NCH = 0
        DO I = 1, LEN
          IF (SELECT(I)) THEN
            QOLD = QUAL(I)
            QUAL(I) = BIT_ORUB(QUAL(I),QVAL)
            IF ( QUAL(I) .NE. QOLD ) NCH = NCH + 1
          END IF
        END DO
      END IF
      END



*+  QUALITY_EOR - EOR quality with specified value
      SUBROUTINE QUALITY_EOR (LEN,QVAL,SELECT,QUAL,NCH,STATUS)
*    Description :
*     EOR quality with specified value
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                LEN                    ! Number of data points
      BYTE                   QVAL
      LOGICAL                SELECT(*)              ! Alter this quality?

*    Import-Export :
      BYTE                   QUAL(*)                ! Quality values
*    Export :
      INTEGER                NCH                    ! # changed pixels
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_EORUB
*    Local variables :
      INTEGER                I                      ! Loop counter
      BYTE                   QOLD
*-

      IF (STATUS.EQ.SAI__OK) THEN
        NCH = 0
        DO I = 1, LEN
          IF (SELECT(I)) THEN
            QOLD = QUAL(I)
            QUAL(I)=BIT_EORUB(QUAL(I),QVAL)
            IF ( QUAL(I) .NE. QOLD ) NCH = NCH + 1
          END IF
        END DO
      END IF
      END



*+  QUALITY_NOT - Complement existing quality bits
      SUBROUTINE QUALITY_NOT( LEN, SELECT, QUAL, NCH, STATUS )
*    Description :
*     Compelment existing quality bits
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                LEN                    ! Number of data points
      LOGICAL                SELECT(*)              ! Alter this quality?
*    Import-Export :
      BYTE                   QUAL(*)                ! Quality values
*    Export :
      INTEGER                NCH                    ! # changed pixels
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_NOTUB
*    Local variables :
      INTEGER                I                      ! Loop counter
      BYTE                   QOLD
*-

      IF (STATUS.EQ.SAI__OK) THEN
        NCH = 0
        DO I = 1, LEN
          IF (SELECT(I)) THEN
            QOLD = QUAL(I)
            QUAL(I)=BIT_NOTUB(QUAL(I))
            IF ( QUAL(I) .NE. QOLD ) NCH = NCH + 1
          END IF
        END DO
      END IF
      END



*+  QUALITY_GETQV - Read quality value from user
      SUBROUTINE QUALITY_GETQV( PAR, PRMPT, QSTR, QVAL, STATUS )
*
*    Description :
*
*     Reads character string supplied by user an returns a quality value.
*     The string should be either a sequence of binary digits, or one of
*     the strings below.
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Apr 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      CHARACTER*(*)                 PAR                     ! Parameter name
      CHARACTER*(*)                 PRMPT                   ! Parameter prompt
*
*    Export :
*
      CHARACTER*(*)                 QSTR                    ! User response
      BYTE                          QVAL                    ! Quality value
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER                       CHR_LEN
*
*    Local constants :
*
      INTEGER                       NSPEC                   ! # special values
        PARAMETER                   ( NSPEC = 6 )
*
*    Local variables :
*
      INTEGER                       ISPEC                   ! Loop over SNAMEs
      INTEGER                       QLEN                    ! Length of QSTR

      LOGICAL                       GOT_VAL                 ! Valid value?
*
*    Local data :
*
      CHARACTER*8                   SNAME(NSPEC)
      BYTE                          SVAL(NSPEC)
      DATA                          SVAL/QUAL__GOOD, QUAL__BAD,
     :                                   QUAL__MISSING,QUAL__PATCHED,
     :                                   QUAL__ARITH, QUAL__IGNORE/
      DATA                          SNAME/'GOOD     ','BAD     ',
     :                                    'MISSING  ','PATCHED ',
     :                                    'ARITH    ','IGNORE  '/
*-

*    Set prompt
      IF ( PAR .EQ. 'QVAL' ) CALL USI_PROMT( PAR, PRMPT, STATUS )

      GOT_VAL = .FALSE.
      DO WHILE ( ( STATUS .EQ. SAI__OK ) .AND. .NOT. GOT_VAL )

*      Get user response
        CALL USI_GET0C( PAR, QSTR, STATUS )

*      Check against alternatives
        IF ( STATUS .EQ. SAI__OK ) THEN

          CALL CHR_UCASE( QSTR )
          QLEN = CHR_LEN( QSTR )

*        First the special values
          ISPEC = 1
          DO WHILE ( (ISPEC.LE.NSPEC ) .AND. .NOT. GOT_VAL )
            IF ( QSTR(:QLEN) .EQ. SNAME(ISPEC)(:QLEN) ) THEN
              GOT_VAL = .TRUE.
              QVAL = SVAL(ISPEC)
            ELSE
              ISPEC = ISPEC + 1
            END IF
          END DO

*        If not a special try for a binary number
          IF ( .NOT. GOT_VAL ) THEN
            CALL STR_CTOB( QSTR(:QLEN), QVAL, STATUS )
          END IF

*        Try again if duff
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            STATUS = SAI__OK
            CALL MSG_SETC( 'RESP', QSTR(:QLEN) )
            CALL MSG_PRNT( 'Invalid quality specification /^RESP/'/
     :                                            /' - try again' )
            CALL USI_CANCL( PAR, STATUS )
          ELSE
            GOT_VAL = .TRUE.
          END IF

        END IF

      END DO

      END


*+  QUALITY_SETFSEL - Reads a spatial descriptor file and sets a mask
	SUBROUTINE QUALITY_SETFSEL( LOC, NDIM, DIM, AXLO, AXHI, NAREA,
     :                              CPTR, STATUS )
*    Description :
*     Reads a description of a spatial region from a spatial file
*     and sets a mask with the pixels defined in the file good and
*     the others set bad.
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     16-Mar-1992     original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) LOC              ! Locator to datafile
      INTEGER NDIM                            ! No. of data dimensions
      INTEGER DIM(DAT__MXDIM)                 ! Data dimensions
      REAL AXLO(NDIM),AXHI(NDIM)              ! Axis ranges
      INTEGER CPTR                            ! Pointer to mask array
*    Import-Export :
*    Export :
      INTEGER NAREA
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER AUNIT                           ! logical unit of ARD file
      INTEGER EPNTR,WPNTR                     ! workspace pointers
      CHARACTER*20 UNITS(2)                   ! units of each data axis
      CHARACTER*60 AFILE                      ! name of ARD file
      REAL BASE(2),SCALE(2)                   ! base and scale of data axes
      INTEGER IMAX(2)                         ! Axis numbers of the X,Y axis
      INTEGER MPNTR                           ! Pointer to image mask
      INTEGER MDIM(2)                         ! Dimensions of image mask
      INTEGER LP
*-

*    Get name of ARD file and open it
      CALL USI_GET0C('ARDFILE',AFILE,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Open the ARD file
      CALL FIO_OPEN( AFILE, 'READ', 'LIST' , 0, AUNIT, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Find the X and Y axis dimensions
      CALL AXIS_GET(LOC, 'X pos', 'XDIM', NDIM, IMAX(1), STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999
      CALL AXIS_GET(LOC, 'Y pos', 'YDIM', NDIM, IMAX(2), STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Create a dynamic array to hold the mask for the image region
      MDIM(1) = DIM(IMAX(1))
      MDIM(2) = DIM(IMAX(2))
      CALL DYN_MAPL(2,MDIM,MPNTR,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Insufficient dynamic memory')
         GOTO 999
      ENDIF
*
*  read the image axis units from the datafile
      CALL BDA_GETAXUNITS(LOC, IMAX(1), UNITS(1), STATUS)
      CALL BDA_GETAXUNITS(LOC, IMAX(2), UNITS(2), STATUS)
*
*  set to blank if not found
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_ANNUL(STATUS)
         UNITS(1) = '   '
         UNITS(2) = '   '
      ENDIF
*
*  calculate the scale values of each axis
      DO LP=1,2
        BASE(LP) = AXLO(IMAX(LP))
        SCALE(LP) = (AXHI(IMAX(LP)) - AXLO(IMAX(LP))) /
     :                 REAL(DIM(IMAX(LP))-1)
      ENDDO

* Map some dynamic memory
*  EPNTR is the exclude array - not used at present but could be enabled
      CALL DYN_MAPL(2,MDIM,EPNTR,STATUS)
      CALL DYN_MAPL(2,MDIM,WPNTR,STATUS)
      CALL ARR_INIT1L(.FALSE., MDIM(1)*MDIM(2), %val(WPNTR), STATUS )
      CALL ARR_INIT1L(.FALSE., MDIM(1)*MDIM(2), %val(MPNTR), STATUS )

*    Read the ARD file and modify the image mask accordingly
      CALL QUALITY_ARD_DRIVE(BASE, SCALE, UNITS, AUNIT, EPNTR, MPNTR,
     :                                MDIM(1), MDIM(2), WPNTR, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Modify the full quality mask with the 2-d one just produced
      CALL QUALITY_SETFSEL_ADDMASK( MDIM(1), MDIM(2), %val(MPNTR),
     :                              IMAX(1),
     :         IMAX(2), DIM(1), DIM(2), DIM(3), DIM(4), DIM(5), DIM(6),
     :         DIM(7), %val(CPTR), NAREA )

*    Close the ARD file
      CALL FIO_CLOSE(AUNIT, STATUS)

*    Abort point
 999  CONTINUE

      END


*+ Drives the ARD language interpreter
      SUBROUTINE QUALITY_ARD_DRIVE( BASE, SCALE, UNITS, ID, IPEXC,
     :           IPINC, NCOLS, NLINES, IPWRK, STATUS )
*
*  Name:
*     QUALITY_ARD_DRIVE

*  Purpose:
*     Drives the ARD language interpreter for the Asterix Quality program

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL QUALITY_ARD_DRIVE( ID, STATUS )

*  Description:
*     ARD is a package of routines which supports the interpretation
*     of files containing ARD language descriptions. This routine
*     is user a modifiable template for processing the ARD description
*     into a form which the calling application requires.

*  Arguments:
*     UNITS(2) = CHARACTER (Given)
*        units of each data axis
*     BASE(2) = REAL (Given)
*        value of the centre of the first bin in the axis
*     SCALE(2) = REAL (Given)
*        width of each axis bin
*     ID = INTEGER (Given)
*        The FIO system descriptor of the file containing the ARD
*        description.
*     ..... user defined arguments (arrays etc.)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-OCT-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'ARD_PAR'          ! ARD system buffer sizes
      INCLUDE 'FIO_PAR'          ! FIO system buffer sizes

*  Arguments Given:
      CHARACTER*(*) UNITS(2)
      REAL BASE(2)
      REAL SCALE(2)
      INTEGER ID
      INTEGER IPEXC
      INTEGER IPINC
      INTEGER IPWRK
      INTEGER NCOLS
      INTEGER NLINES

*  Status:
      INTEGER STATUS             ! Global status

*  Functions:
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*  Local Variables:
      CHARACTER * ( ARD__MAXLEN ) LINE ! Buffer to contain processed
                                       ! lines extracted from input file
      CHARACTER BINOP * ( 5 )    ! Binary operator
      CHARACTER COMNAM * ( 15 )  ! Composite block name
      CHARACTER KEYWRD * ( 15 )  ! Current ARD keyword
      CHARACTER UNIOP * ( 5 )    ! Unary operator
      CHARACTER FNAME * ( FIO__SZFNM ) ! Input file name
      INTEGER IPVAL              ! Pointer to list of values
      INTEGER ISTART             ! Start of string excluding keyword and
                                 ! initial operators.
      INTEGER LINNUM             ! Current line number
      INTEGER NCHAR              ! Number of characters in extracted
                                 ! line
      INTEGER NITEM              ! Number of words after keyword
      INTEGER NREQD              ! Number of words which may be
                                 ! required when mapping w/s
      INTEGER NUMDIM             ! Current dimensionality
      INTEGER PRODIM             ! The PROJECT dimension
      INTEGER PRORAN( 2 )        ! The PROJECT range
      INTEGER LSTAT              ! Local status
      INTEGER IPAT               ! Pointer to current mask array
      INTEGER NCOORD             ! Number of coordinate values extracted
                                 ! from trailing numerics
      INTEGER NPIX               ! Number of pixels
      LOGICAL ALLINT             ! Whether extracted coordinates were
                                 ! all integers or not
      LOGICAL DIMEN              ! Whether statement DIMENSION
      LOGICAL EOF                ! True when end of file
      LOGICAL EXCLUD             ! Whether current region is exclude
                                 ! or include
      LOGICAL COMEX              ! Whether to composite region is an
                                 ! exlude or include region
      LOGICAL SPARE              ! Spare logical flag for storage
      LOGICAL FIRST              ! Signifies that this is the first
                                 ! line after a COMPOSITE statement,
                                 ! used to check operators.
      LOGICAL INCOM              ! Signifies whether we're within a
                                 ! COMPOSITE block or not.
      LOGICAL BGNCOM             ! Signifies that a COMPOSITE
                                 ! statement has been encountered.
      LOGICAL ENDCOM             ! Signifies that a END COMPOSITE
                                 ! statement has been encountered.
      LOGICAL PROJEC             ! True if current statement is PROJECT
      LOGICAL STATE              ! True if the current line contains a
                                 ! statement
*
      CHARACTER*20 ARDUNITS      ! Units of ARD file ! RDS
      DOUBLE PRECISION CFACT(2)  ! Conversion between axis units and ARD file
      INTEGER K                  ! Length variable   ! RDS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up an error context for deferring all error messages.

*  Initialise ARD.

*  Initialise the ARDUNITS
      ARDUNITS = '                    '

*  Not in COMPOSITE block.
      INCOM = .FALSE.

*  Not first none statement after COMPOSITE.
      FIRST = .FALSE.

*  Not End_Of_file.
      EOF = .FALSE.

*  No lines read yet.
      LINNUM = 0

*  Initialise the ARD keyword description tables.
      CALL ARD_INIT( STATUS )

*  Initialise the number of dimensions. (Only allowing 2 in this
*  routine.)
      NUMDIM = 2

*  Loop while not End_Of_File and no error.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .NOT.EOF .AND. STATUS .EQ. SAI__OK ) THEN

*  Read in a line of useful information.
         CALL ARD_RDLIN( ID, ARD__MAXLEN, LINE, NCHAR, LINNUM, EOF,
     :                      STATUS )
         IF ( EOF .OR. STATUS .NE. SAI__OK ) GO TO 1

*  Write it out.
c         WRITE(*,*)LINNUM,':',LINE(:NCHAR)

*  Find out if the line is a valid statement.
*  Look for a DIMENSION statement, PROJECT or COMPOSITE-END COMPOSITE.
*  DIMENSION must be 2, PROJECT and COMPOSITE are disabled for now.
         CALL ARD_STATE( LINE, NCHAR, LINNUM, STATE, DIMEN, NUMDIM,
     :                   PROJEC, PRODIM, PRORAN, BGNCOM, INCOM, ENDCOM,
     :                   SPARE, COMNAM, STATUS )
*
* !RDS - test if UNITS string - would be incorporated into ARD_STATE
         IF ( INDEX ( LINE, 'UNITS' ) .NE. 0 ) THEN
            K = INDEX( LINE, '=' )
            ARDUNITS = LINE(K+1:K+20)
            STATE = .TRUE.
         ENDIF
* !RDS

         IF ( STATE ) THEN

*  Have intercepted (and interpreted) a statement, no further work to do
*  this time, just inform user if present capabilities have been
*  exceeded.
            IF ( DIMEN .AND. NUMDIM .NE. 2 ) THEN
               NUMDIM = 2
               CALL MSG_OUT( ' ',
     :         '  Dimensionality of regions must be 2', STATUS )

*  Look for other unsupported items.
            ELSE IF ( PROJEC ) THEN
               CALL MSG_OUT( ' ',
     :         '  PROJECT statement not supported - ignored', STATUS )
            ELSE IF ( BGNCOM ) THEN

*  Must have been a COMPOSITE statement, set FIRST to signify this.
*  FIRST will not be reset within this block until begin COMPOSITE is
*  re-encountered.
               FIRST = .TRUE.

*  Record whether the composite is include or exlude
               COMEX = SPARE

*  On entering a composite region, set the array pointer to some clean
*  workspace initialised all true.
               IPAT = IPWRK
*               CALL FIL_SETL( %VAL( IPAT ) , NPIX, STATUS )

            ELSE IF ( ENDCOM ) THEN

*  End composite statement, apply the current region to the appropriate
*  main mask.
               IF ( COMEX ) THEN
                  IPAT = IPEXC
               ELSE
                  IPAT = IPINC
               END IF
               NPIX = NCOLS * NLINES
               CALL ARR_COP1L( NPIX, %VAL(IPWRK) , %VAL(IPAT), STATUS )

*  Set flag to out of composite block
               INCOM = .FALSE.
            ELSE

*  Don't know how it got here, just pass through.

            END IF

         ELSE
*  Separate out the keyword, and any other operators. The operators
*  allowed on regions are to be excluded from consideration or
*  included.
            CALL ARD_KEYW( LINE, LINNUM, INCOM, FIRST, KEYWRD, EXCLUD,
     :                     UNIOP, BINOP, NITEM, ISTART, STATUS )
c            WRITE(*,*)KEYWRD,'-',EXCLUD,'-',UNIOP,'-',BINOP,'-',NITEM

*  Set FIRST to FALSE. If FIRST was true then next line is not the first
*  in the composite block, otherwise no effect.
            FIRST = .FALSE.

*  Time to extract the numeric items, round the required space up to
*  a multiple of the current dimensionality. This should allow a safe
*  vectorisation of the stored items. Get all trailing value as double
*  precision. But find out if the values are really integers.
            NREQD = ( ( NITEM / NUMDIM ) + 1 ) * NITEM
            CALL ARD_MALLOC( NREQD, '_DOUBLE', IPVAL, STATUS )

*  Extract the values.
            CALL ARD_EXVAL( KEYWRD, LINE, LINNUM, ISTART, NITEM,
     :                       NUMDIM, %VAL( IPVAL ), ALLINT, NCOORD,
     :                       STATUS )
c            WRITE(*,*)'All integers =',ALLINT
*
* !RDS - convert ARD values into pixels if none_integer trailing values
            IF (.NOT. ALLINT) THEN
*
*      - are ARDUNITS set ?
               IF (CHR_LEN(ARDUNITS) .GT. 0) THEN
*
*      -    Calculate the conversion factor between the units of the ARD
*           file and the units of the datafile
                  CALL UNT_CONV(ARDUNITS, UNITS(1), CFACT(1), STATUS)
                  CALL UNT_CONV(ARDUNITS, UNITS(2), CFACT(2), STATUS)
*
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL MSG_OUT(' ','Error converting units',STATUS)
                     GOTO 999
                  ENDIF
               ELSE
*
*        assume units are axis units (perhaps should assume pixels ??)
                  CFACT(1) = 1.0
                  CFACT(2) = 1.0
               ENDIF
*
*      - convert the ARD values into pixels
               CALL ARD_PCONV(CFACT, BASE, SCALE, KEYWRD,
     &                             NITEM, %val(IPVAL), STATUS)
*
               IF (STATUS .NE. SAI__OK) GOTO 999
*
            ENDIF
* !RDS
*
*  Set pointer to appropriate main mask, Include or Exclude.
            IF ( INCOM ) THEN

*  In composite block, stick to the workspace pointer.
               IPAT = IPWRK

            ELSE IF ( EXCLUD ) THEN

*  Point at exclude mask.
               IPAT = IPEXC
            ELSE

*  Point at include mask.
               IPAT = IPINC
            END IF

*
*  Convert input coordinates to array coordinates.



*  End of coordinate conversion.
*
*  Process regions block.

*  Put current region into array. Using the appropriate operators
            IF ( KEYWRD .EQ. 'POLYGON' ) THEN
               CALL FIL_POLYL( %VAL( IPAT ), NCOLS, NLINES, UNIOP,
     :                         BINOP, %VAL( IPVAL), NITEM, STATUS )
            ELSE IF ( KEYWRD .EQ. 'ELLIPSE' ) THEN
               CALL FIL_ELLPS( %VAL( IPAT ), NCOLS, NLINES, UNIOP,
     :                         BINOP, %VAL( IPVAL ), STATUS )
            ELSE IF ( KEYWRD .EQ. 'COLUMN' ) THEN
               CALL FIL_COL( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), NITEM, STATUS )
            ELSE IF ( KEYWRD .EQ. 'ROW' ) THEN
               CALL FIL_ROW( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), NITEM, STATUS )
            ELSE IF ( KEYWRD .EQ. 'CIRCLE' ) THEN
               CALL FIL_CIR( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), STATUS )
            ELSE IF ( KEYWRD .EQ. 'LINE' ) THEN
               CALL FIL_LINE( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), STATUS )
            ELSE IF ( KEYWRD .EQ. 'PIXEL' ) THEN
               CALL FIL_PIXS( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), NITEM, STATUS )
            ELSE IF ( KEYWRD .EQ. 'BOX' ) THEN
               CALL FIL_BOX( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       %VAL( IPVAL ), STATUS )
            ELSE IF ( KEYWRD .EQ. 'NDF' ) THEN
               CALL FIL_NDF( %VAL( IPAT ), NCOLS, NLINES, UNIOP, BINOP,
     :                       STATUS )
            ELSE

*  Not supported.
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'KEYWRD', KEYWRD )
               CALL MSG_SETI( 'LINNUM', LINNUM )
               CALL ERR_REP( 'ARD_DRIVE1',
     :         '  region type ^KEYWRD not supported within this'//
     :         ' application - line ^LINNUM'  , STATUS )
            END IF

*  Release memory.
            CALL ARD_FREE( IPVAL, STATUS )

*  End of processing block
*--------------------

*  End of looking for region keyword block
         END IF
*  Next line of input file.
         GO TO 1
      END IF

999   CONTINUE

*  If exiting with an error status, write out name of file.

      IF ( STATUS .NE. SAI__OK ) THEN

*  Set up a new error context, issue error and flush it.
         CALL ERR_MARK
         LSTAT = SAI__OK
         CALL FIO_FNAME( ID, FNAME, LSTAT )
         CALL MSG_SETC( 'FNAME', FNAME )
         LSTAT = SAI__ERROR
         CALL ERR_REP( 'ARD_DRIVE2',
     :   '  ARD - Error translating file ^FNAME', LSTAT )
         CALL ERR_FLUSH( LSTAT )
         CALL ERR_RLSE

      END IF

*  Now release the main error context.
      CALL ERR_RLSE

      END


*+QUALITY_SETFSEL_ADDMASK - includes a 2-d mask in an n-d mask
      SUBROUTINE QUALITY_SETFSEL_ADDMASK(MDIM1, MDIM2, IMASK, IMAX1,
     &               IMAX2, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7,
     &               MASK,NAREA)
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
*
*    Import :
*
      INTEGER MDIM1,MDIM2                 ! Image mask dimensions
      LOGICAL IMASK(MDIM1,MDIM2)          ! Image mask
      INTEGER IMAX1,IMAX2                 ! Dim. of X and Y axis in big array
      INTEGER DIM1,DIM2,DIM3,DIM4,DIM5,DIM6,DIM7 ! Dims of big mask
*
*    Import-Export :
*
      LOGICAL MASK(DIM1,DIM2,DIM3,DIM4,DIM5,DIM6,DIM7) ! The full mask
      INTEGER NAREA
*
*    Local variables :
*
      INTEGER LP1,LP2,LP3,LP4,LP5,LP6,LP7,LPVAL(DAT__MXDIM)
*-

      NAREA = 0

      DO LP7=1,DIM7
         LPVAL(7) = LP7
       DO LP6=1,DIM6
          LPVAL(6) = LP6
        DO LP5=1,DIM5
           LPVAL(5) = LP5
         DO LP4=1,DIM4
            LPVAL(4) = LP4
          DO LP3=1,DIM3
             LPVAL(3) = LP3
           DO LP2=1,DIM2
              LPVAL(2) = LP2
            DO LP1=1,DIM1
               LPVAL(1) = LP1
*
               IF ( .NOT. MASK(LP1,LP2,LP3,LP4,LP5,LP6,LP7) ) THEN
                 MASK(LP1,LP2,LP3,LP4,LP5,LP6,LP7) =
     :                   IMASK(LPVAL(IMAX1),LPVAL(IMAX2))
                 IF ( IMASK(LPVAL(IMAX1),LPVAL(IMAX2)) ) THEN
                   NAREA = NAREA + 1
                 END IF
               END IF
*
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
*
      END
