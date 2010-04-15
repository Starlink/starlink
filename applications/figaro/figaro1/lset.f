C+
      SUBROUTINE LSET
C
C     L S E T
C
C     This routine is the main body of LXSET,LYSET
C
C     LXSET is a fudging routine that sets the range of the first axis
C     associated with a spectrum or image, given a start and end
C     wavelength and whether or not the wavelengths are to be scaled
C     logarithmically.
C
C     LYSET performs an analgous set for the second axis structure
C     (for images).
C
C     Command parameters -
C
C     IMAGE      (Character) The name of the structure containing the
C                image.
C
C     WSTART or  (Numeric) The value of the CENTER of the first
C     VSTART     bin of the resulting data. WSTART and VSTART are used
C                by  LXSET and LYSET respectively)
C
C     WEND  or   (Numeric) The value of the CENTER of the final
C     VEND       bin of the resulting data. WEND and VEND are used
C                by  LXSET and LYSET respectively)
C
C     OUTPUT     (Character) The name of the result of the operation.
C                This can be the same as for IMAGE.  If not, a new
C                structure is created, with everything but the axis
C                data a direct copy of the input.
C
C     Command keywords -
C
C     LOG    Axis values are to increase logarithmically.
C
C                                      KS / AAO 28th MArch 1985
C     Modified
C
C     17th Aug 1988.  JM/AAO Modication of existing LXSET code
C                     to permit addition of either .X or .Y axis
C                     structure
C     18th Aug 1988.  JM/AAO Converted to use DSA routines for data
C                     access.
C     29th Sep 1992.  HME / UoE, Starlink.  INCLUDE changed.
C     17th Sep 1993.  HME / UoE, Starlink.  Changed call to FIG_WFILLD,
C                     it needs start/end as double precision numbers.
C                     In order to use the DBLE function, the local
C                     logical had to become LDBLE.
C     26th Jul 1996.  MJCL / Starlink, UCL.  Added PAR_ABORT checking.
C     2005 June 10    MJC / Starlink  Use CNF_PVAL for pointers to
C                     mapped data.  Tidy variable declarations.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      REAL AXCENT                ! Midpoint axis centre to evaluate DBLE
      REAL AXEND                 ! Last pixel's axis centre
      CHARACTER*10 AXFIRST       ! 'WSTART' OR 'VSTART' as appropriate
      CHARACTER*14 AXLAB         ! 'Angstroms' or ' ' (blank) currently
      CHARACTER*10 AXLAST        ! 'WEND' OR 'VEND' as appropriate
      REAL AXSTART               ! First pixel's axis centre
      CHARACTER*64 COMMAND       ! Command value
      REAL DELTAX                ! Mean axis increment to evaluate DBLE
      INTEGER DIMS(10)           ! Dimensions of data
      LOGICAL EXIST              ! True if axis array already exists
      LOGICAL LDBLE              ! True if double precision required for
*                                ! new axis array
      LOGICAL LOGWR              ! Axis centres increae logarthmically?
      INTEGER NDATA              ! Number of data elemets
      INTEGER NDIM               ! Number of dimensions
      INTEGER NELM               ! Total number of elements in existing
*                                ! axis array (if any)
      INTEGER NAXIS              ! Axis number (X-axis =1, Y-Axis=2)
      INTEGER PTR                ! Pointer mapped axis centres
      REAL RESET
      INTEGER SLOT               ! Slot for mapping axis data
      INTEGER STATUS             ! Status
      CHARACTER TYPE*16          ! Data type for processing
      CHARACTER*1 XORY           ! 'X' or 'Y' whichever is appropriate
      LOGICAL XSET               ! XSET true for LXSET command
      LOGICAL YSET               ! YSET true for LYSET command

C
C     Numeric parameter limits - close to VAX real limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)

C     Open DSA

      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GOTO 500

C     Get command name
C
      CALL PAR_COMMAND(COMMAND)
      XSET=COMMAND(2:2).EQ.'X'
      YSET=COMMAND(2:2).EQ.'Y'

C
C     Get input name

      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GOTO 500

C   Set up parameter names etc. for LXSET or LYSET as appropriate

      IF (XSET)THEN
         AXLAB='Angstroms'
         XORY='X'
         AXFIRST='WSTART'
         AXLAST='WEND'
         NAXIS=1
      ELSEIF(YSET)THEN
         AXLAB='    '
         XORY='Y'
         AXFIRST='VSTART'
         AXLAST='VEND'
         NAXIS=2
      END IF
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      NDATA=DIMS(NAXIS)
C
C     Get values of the parameters W(or V)START & W(or V)END, & LOG
C     keyword.
C
      RESET=1.
      CALL PAR_RDVAL(AXFIRST,FMIN,FMAX,RESET,AXLAB,AXSTART)
      RESET=FLOAT(NDATA)
      CALL PAR_RDVAL(AXLAST,FMIN,FMAX,RESET,AXLAB,AXEND)
      CALL PAR_RDKEY('LOG',.FALSE.,LOGWR)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get output structure name and copy input to output file.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C
C     See if there is a compatible axis array in the output file.

      CALL DSA_SEEK_AXIS('IMAGE',NAXIS,EXIST,STATUS)


C     If a compatible array exists, check its dimensionality and issue
C     an informational message if it was not one-dimensional.

      IF (EXIST)THEN
         CALL DSA_AXIS_SIZE ('IMAGE',NAXIS,10,NDIM,DIMS,NELM,STATUS)
         IF (NDIM.NE.1) THEN
            CALL PAR_WRUSER(
     :  'Existing multi-dimensional '//XORY//'-array has been replaced.'
     :  ,STATUS)
         END IF
      END IF

C     The new axis bounds are checked to decide whether a type of
C     'DOUBLE' (precision) or only 'FLOAT' is necessary.

      AXCENT=(AXSTART+AXEND)/2.0
      DELTAX=(AXEND-AXSTART)/NDATA
      LDBLE=((AXCENT/ABS(DELTAX)).GT.50000.)
      IF (LDBLE) THEN
         TYPE='DOUBLE'
      ELSE
         TYPE='FLOAT'
      END IF
C
C     Create new axis-array.

      CALL DSA_COERCE_AXIS_DATA('OUTPUT',NAXIS,TYPE,1,NDATA,
     :                          STATUS)

C     Map data
      CALL DSA_MAP_AXIS_DATA('OUTPUT',NAXIS,'WRITE',TYPE,PTR,
     :                       SLOT,STATUS)

      IF(STATUS.NE.0) GOTO 500

C     Fill up the output axis array
      IF (LDBLE)THEN
         CALL FIG_WFILLD(DBLE(AXSTART),DBLE(AXEND),
     :                   LOGWR,NDATA,%VAL(CNF_PVAL(PTR)))
      ELSE
         CALL FIG_WFILL(AXSTART,AXEND,LOGWR,NDATA,%VAL(CNF_PVAL(PTR)))
      END IF
C
C     Set the .X (or Y).LOG flag as required. (If it already exists, set
C     it accordingly, if it doesn't, create it and set it only if
C     log values used.)   (NOT IMPLEMENTED)
C
  500 CONTINUE

      CALL DSA_CLOSE(STATUS)
      END
