C+
      SUBROUTINE LSET
C
C     L S E T
C
C     This routine is the main body of LXSET,LYSET
C
C     LXSET is a fudging routine that sets the range of the first axis
C     associated with a spectrum or image, given a start and end wavelength and
C     whether or not the wavelengths are to be scaled logarithmically.
C
C     LYSET performs an analgous set for the second axis structure
C     (for images).
C
C     Command parameters -
C
C     IMAGE      (Character) The name of the structure containing the image.
C
C     WSTART or  (Numeric) The value of the CENTER of the first
C     VSTART      bin of the resulting data. WSTART and VSTART are used
C                 by  LXSET and LYSET respectively)
C
C     WEND  or   (Numeric) The value of the CENTER of the final
C     VEND        bin of the resulting data. WEND and VEND are used
C                 by  LXSET and LYSET respectively)
C
C     OUTPUT     (Character) The name of the result of the operation.
C                This can be the same as for IMAGE.  If not, a new structure
C                is created, with everything but the axis data a direct
C                copy of the input.
C
C     Command keywords -
C
C     LOG    Axis values are to increase logarithmically.
C
C                                      KS / AAO 28th MArch 1985
C     Modified
C
C     17th Aug 1988.  JM/AAO Modication of existing LXSET code
C                     to permit addition of either .X or .Y axis structure
C     18th Aug 1988.  JM/AAO Converted to use DSA routines for data access
C     29th Sep 1992.  HME / UoE, Starlink.  INCLUDE changed.
C     17th Sep 1993.  HME / UoE, Starlink.  Changed call to FIG_WFILLD,
C                     it needs start/end as double precision numbers.
C                     In order to use the DBLE function, the local
C                     logical had to become LDBLE.
C     26th Jul 1996.  MJCL / Starlink, UCL.  Added PAR_ABORT checking.
C+
      IMPLICIT NONE
C
C     Functions used
C
      LOGICAL PAR_ABORT            ! (F)PAR abort flag
C
C     Local variables
C
      LOGICAL  LOGWR
      INTEGER DIMS(10), NDIM, NDATA, STATUS
      REAL RESET
      CHARACTER*64 COMMAND, IMAGE, OUTPUT
      CHARACTER TYPE*16

      LOGICAL XSET,YSET            ! XSET true for LXSET command etc.
      CHARACTER*1 XORY             ! 'X' or 'Y' whichever is appropriate
      CHARACTER*14 AXLAB           ! 'Angstroms' or ' ' (blank) currently
      CHARACTER*10 AXFIRST         ! 'WSTART' OR 'VSTART' as appropriate
      CHARACTER*10 AXLAST          ! 'WEND' OR 'VEND' as appropriate
      REAL AXSTART,AXEND           ! Values assigned to (centres of) first

      LOGICAL EXIST                ! True if axis array already exists
      INTEGER NELM                 ! Total number of elements in existing
*                                  ! axis array (if any)
      INTEGER NAXIS                ! Axis number (X-axis =1, Y-Axis=2)
      LOGICAL LDBLE                ! True if double precision required for
*                                  ! new axis array
      REAL DELTAX,AXCENT           ! Used in evaluating DBLE
      INTEGER SLOT,PTR,ADDRESS     ! Used in mapping axis data

C
C     Numeric parameter limits - close to VAX real limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)

C     Dynamic memory support - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
      INTEGER DYN_ELEMENT

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

      IF(XSET)THEN
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
      ENDIF
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      NDATA=DIMS(NAXIS)
C
C     Get values of the parameters W(or V)START & W(or V)END, & LOG keyword.
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
C     an informational message if it was not one-dimensional

       IF(EXIST)THEN
         CALL DSA_AXIS_SIZE ('IMAGE',NAXIS,10,NDIM,DIMS,NELM,STATUS)
         IF (NDIM.NE.1) THEN
            CALL PAR_WRUSER(
     :  'Existing multi-dimensional '//XORY//'-array has been replaced.'
     :  ,STATUS)
         ENDIF
      ENDIF

C     The new axis bounds are checked to decide whether a type of
C     'DOUBLE' (precision) or only 'FLOAT' is necessary.

      AXCENT=(AXSTART+AXEND)/2.0
      DELTAX=(AXEND-AXSTART)/NDATA
      LDBLE=((AXCENT/ABS(DELTAX)).GT.50000.)
      IF (LDBLE) THEN
         TYPE='DOUBLE'
      ELSE
         TYPE='FLOAT'
      ENDIF
C
C     Create new axis-array.

      CALL DSA_COERCE_AXIS_DATA('OUTPUT',NAXIS,TYPE,1,NDATA,
     :                                                STATUS)

C     Map data
      CALL DSA_MAP_AXIS_DATA('OUTPUT',NAXIS,'WRITE',TYPE,ADDRESS,
     :                                         SLOT,STATUS)
      PTR=DYN_ELEMENT(ADDRESS)

      IF(STATUS.NE.0)GOTO 500
C     Fill up the output axis array
      IF(LDBLE)THEN
         CALL FIG_WFILLD(DBLE(AXSTART),DBLE(AXEND),
     :      LOGWR,NDATA,DYNAMIC_MEM(PTR))
      ELSE
         CALL FIG_WFILL(AXSTART,AXEND,LOGWR,NDATA,DYNAMIC_MEM(PTR))
      ENDIF
C
C     Set the .X (or Y).LOG flag as required. (If it already exists, set
C     it accordingly, if it doesn't, create it and set it only if
C     log values used.)   (NOT IMPLEMENTED)
C
  500 CONTINUE

      CALL DSA_CLOSE(STATUS)
      END
