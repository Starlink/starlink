C+
      SUBROUTINE INTERP
C
C     I N T E R P    /     S P I F I T    /   L I N T E R P
C
C     Interpolates between the points of a 'spiketrum' to 
C     generate a spectrum.   The INTERP command does this by
C     spline interpolation, the SPIFIT command uses global polynomial
C     fitting, and the LINTERP command uses linear interpolation.
C
C     Command parameters -
C
C     SPIKETRUM  The name of the structure containing the spiketrum
C
C     ORDER      The order for the global polynomial to be fitted
C                (SPIFIT only).
C
C     SPECTRUM   The name of the result of the operation.  This can
C                be the same as for SPIKE.  If not, a new structure
C                is created, with everything but the data a direct
C                copy of the input.
C
C     Command keywords  - 
C
C     LOG        (INTERP & SPIFIT only) Perform the interpolation on 
C                the log of the data
C
C     LINEND     (INTERP only) Use a linear interpolation for the
C                ends of the data - spline fits can go wild outside the
C                range of the defined points.
C
C     User variables used - None
C                                      KS / CIT 6th July 1984
C     Modified:
C
C     26th Mar 1985  KS / AAO.  Modified for use with new, NAG,
C                    version of FIG_ISPIKE.  Workspace usage and
C                    FIG_ISPIKE call changed.
C     12th Jan 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     12th Feb 1991  JMS / AAO. Added STATUS check to abort.
C     25th Sep 1992  HME / UoE, Starlink. INCLUDE changed.
C                    Lowercase file name spiketrum (.def).
C     18th May 1995  HME / UoE, Starlink. FIG_ISPIKE now needs a longer
C                    work space. Make DWPTR 11*NX+24.
C+
      IMPLICIT NONE
C
C     Functions
C
      INTEGER DYN_ELEMENT
 
C
C     Local variables
C
      INTEGER   ADDRESS    ! Virtual address for data array
      INTEGER   BFLOAT     ! Number of bytes per item of type 'FLOAT'
      INTEGER   BYTES      ! Bytes required for an array
      CHARACTER COMMAND*8  ! Figaro command name
      INTEGER   DIMS(5)    ! Spiketrum dimensions
      CHARACTER DNAME*72   ! DTA name of data component
      INTEGER   DSTAT      ! Status for DTA routines
      INTEGER   DWPTR      ! Dynamic memory element for workspace
      REAL      ENDS(4)    ! End values
      LOGICAL   LINEND     ! True if linear interp. used at ends of data
      LOGICAL   LOGFIT     ! True if interpolation performed on log of data
      INTEGER   NDIM       ! Number of dimensions
      INTEGER   NELM       ! Number of elements in Spiketrum
      INTEGER   NP         ! No. points used for fitting in Spiketrum
      INTEGER   NX         ! No. of elements in Spiketrum axis(1) array
      INTEGER   ORDER      ! Polynomial order (SPIFIT only).
      INTEGER   SLOT       ! Slot number for mapped data - ignored
      INTEGER   SPTR       ! Dynamic memory element for data
      INTEGER   STATUS     ! Running status for DSA routines
      REAL      VALUE      ! Polynomial order (SPIFIT only).
      INTEGER   XPTR       ! Dynamic memory element for axis(1) data
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Dynamic memory common - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C     
C     Initial values
C
      STATUS=0
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Open SPIKETRUM file
C
      CALL DSA_INPUT ('SPIKE','SPIKETRUM',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('SPIKE',1,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
      NX=DIMS(1)
C
C     Get command name
C
      CALL PAR_COMMAND(COMMAND)
C
C     Interpolate on the log of the data?
C
      IF (COMMAND.NE.'LINTERP') CALL PAR_RDKEY('LOG',.FALSE.,LOGFIT)
C
C     If SPIFIT, get the value of ORDER.  If
C     INTERP, get the value of LINEND.
C
      IF (COMMAND.EQ.'SPIFIT') THEN
         CALL PAR_RDVAL('ORDER',1.,10.,10.,' ',VALUE)
         ORDER=VALUE
         LINEND=.FALSE.
      ELSE IF (COMMAND.EQ.'INTERP') THEN
         ORDER=-1
         CALL PAR_RDKEY('LINEND',.FALSE.,LINEND)
      ELSE
         ORDER=-2
         LINEND=.TRUE.
      END IF
      
C
C     Open output file and map data for update
C
      CALL DSA_OUTPUT('SPECT','SPECTRUM','SPIKE',0,0,STATUS)
      CALL DSA_MAP_DATA('SPECT','UPDATE','FLOAT',ADDRESS,SLOT,STATUS)
      SPTR=DYN_ELEMENT(ADDRESS)
C
C     Map SPIKETRUM axis data
C
      CALL DSA_MAP_AXIS_DATA('SPIKE',1,'READ','FLOAT',ADDRESS,SLOT,
     :                        STATUS)
      XPTR=DYN_ELEMENT(ADDRESS)
C
C     Look for any 'end' values. These are located with the aid of the
C     structure definition file SPIKETRUM.DEF. Failure to find these
C     end values in the input spiketrum file simply results in 
C     default values being used.
C
      CALL DSA_READ_STRUCT_DEF('spiketrum',STATUS)

      CALL DSA_ELEMENT_NAME('SPIKE','LAMBDA_LEFT',DNAME,STATUS)
      IF(STATUS.NE.0)GOTO 500
      CALL DTA_RDVARF(DNAME,1,ENDS(1),DSTAT)
      IF (DSTAT.EQ.0) THEN
         CALL DSA_ELEMENT_NAME('SPIKE','DATA_LEFT',DNAME,STATUS)
         IF(STATUS.NE.0)GOTO 500
         CALL DTA_RDVARF(DNAME,1,ENDS(2),DSTAT)
      END IF
      IF (DSTAT.NE.0) THEN
         ENDS(1)=0.
         ENDS(2)=0.
      END IF

      CALL DSA_ELEMENT_NAME('SPIKE','LAMBDA_RIGHT',DNAME,STATUS)
      IF(STATUS.NE.0)GOTO 500
      CALL DTA_RDVARF(DNAME,1,ENDS(3),DSTAT)
      IF (DSTAT.EQ.0) THEN
         CALL DSA_ELEMENT_NAME('SPIKE','DATA_RIGHT',DNAME,STATUS)
         IF(STATUS.NE.0)GOTO 500
         CALL DTA_RDVARF(DNAME,1,ENDS(4),DSTAT)
      END IF
      IF (DSTAT.NE.0) THEN
         ENDS(3)=0.
         ENDS(4)=0.
      END IF
C
C     Get workspace needed by the fitting routines.  LINTERP doesn't
C     need any workspace.   See FIG_ISPIKE for explanation of
C     workspace requirements for SPIFIT and INTERP.  (The NP=NP+2
C     is because the NAG version of FIG_ISPIKE may add up to 2 more
C     dummy points to the spiketrum.)
C
      IF (COMMAND.NE.'LINTERP') THEN
         CALL FIG_NPSPIK(NX,DYNAMIC_MEM(SPTR),ENDS,NP)
         NP=NP+2
         BYTES=(NP*11+24)*8
         CALL DSA_GET_WORKSPACE(BYTES,ADDRESS,SLOT,STATUS)
         DWPTR=DYN_ELEMENT(ADDRESS)
         IF(STATUS.NE.0)GOTO 500
C
C        Operate on the data.
C
         CALL FIG_ISPIKE (NX,DYNAMIC_MEM(XPTR),ENDS,LOGFIT,LINEND,
     :                    ORDER,NP,DYNAMIC_MEM(DWPTR),
     :                    DYNAMIC_MEM(SPTR),STATUS)
C
      ELSE
C
C        Linear interpolation is much simpler..
C
         CALL FIG_LSPIKE (NX,DYNAMIC_MEM(XPTR),ENDS,DYNAMIC_MEM(SPTR))
      END IF

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
