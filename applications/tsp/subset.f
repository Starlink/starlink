C+
      SUBROUTINE SUBSET(STATUS)
C
C            S U B S E T
C
C     Command name:
C        SUBSET
C
C     Function:
C        Take a subset of a dataset in wavelength or time axes.
C
C     Description:
C        A subset of the input file is taken covering a specified
C        range in wavelength and time. The command works on
C        either 1D or 2D data.
C
C     Parameters:
C    (1) INPUT      (TSP, nD)  The input dataset.
C    (2) LSTART     (Real)     Starting wavelength for subset.
C    (3) LEND       (Real)     End wavelength for subset.
C    (C) TSTART     (Double)   Starting Time for subset.
C    (C) TEND       (Double)   End Time for subset.
C    (4) OUTPUT     (TSP, nD)  The output dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         30/4/1988
C
C-
C
C  History:
C    30/4/1988   Original Version.   JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER LPTR,TPTR,LSIZE,TSIZE,DPTR,VPTR,OPTR

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,LLOC,TLOC,DLOC,ODLOC,VLOC
      CHARACTER*(DAT__SZLOC) SLOC,OSLOC

*  Range for subset in X and Y
      INTEGER XSTART,XFIN,YSTART,YFIN

*  Dimensions of data
      INTEGER ACTDIM,DIMS(7)

*  Number of Stokes parameters
      INTEGER NUM

*  Stokes parameter present flags
      LOGICAL QZ,UZ,VZ
      LOGICAL VARIANCE
      CHARACTER*64 LABEL,UNITS
      INTEGER UPPER(2),LOWER(2)
      INTEGER NLSIZE,NTSIZE

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get dimensions of data
      CALL TSP_SIZE(LOC,7,DIMS,ACTDIM,STATUS)
      IF (ACTDIM .GT. 2) THEN
          CALL MSG_OUT(' ','More Than 2 Dimensions',STATUS)
          CALL DAT_ANNUL(LOC,STATUS)
          RETURN
      ENDIF
      LSIZE = DIMS(1)

*  Map the wavelength array
      CALL TSP_MAP_LAMBDA(LOC,'READ',LPTR,LLOC,STATUS)

*  Select range for subset in Lambda
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_SUBLRANGE(LSIZE,%VAL(LPTR),XSTART,XFIN,STATUS)
      ENDIF

*  Calculate new size in the wavelength axis
      NLSIZE = XFIN-XSTART+1

*  Set bounds for slice
      LOWER(1)=XSTART
      UPPER(1)=XFIN

*  If the data is 2 dimensional we have to subset in the time axis as well
      IF (ACTDIM .EQ. 2) THEN
          TSIZE = DIMS(2)

*  Map the time axis
          CALL TSP_MAP_TIME(LOC,'READ',TPTR,TLOC,STATUS)

*  Get limits for subset in time axis
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_SUBTRANGE(TSIZE,%VAL(TPTR),YSTART,YFIN,STATUS)
          ENDIF

*  Calculate new size for the time axis
          NTSIZE = YFIN-YSTART+1

*  Set limits for time slice
          LOWER(2)=YSTART
          UPPER(2)=YFIN
      ELSE
          NTSIZE=1
      ENDIF

*  Map the required slice of the data array
      CALL TSP_MAP_SLICE(LOC,ACTDIM,LOWER,UPPER,'READ',DPTR,
     :        DLOC,STATUS)

*  And of the variance array if present
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_MAP_VSLICE(LOC,ACTDIM,LOWER,UPPER,'READ',VPTR,
     :        VLOC,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              VARIANCE = .FALSE.
              STATUS = SAI__OK
          ELSE
              VARIANCE = .TRUE.
          ENDIF
      ENDIF

*  Create the output dataset
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  The output structure is first created with no Stokes Parameters
*  These are added later

      IF (ACTDIM .EQ. 1) THEN
          CALL TSP_CREATE_1D(OLOC,NLSIZE,' ',VARIANCE,.FALSE.,STATUS)
      ELSE
          CALL TSP_CREATE_2D(OLOC,NLSIZE,NTSIZE,' ',VARIANCE,.FALSE.,
     :          STATUS)
      ENDIF

*  Copy the main data array

      CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,ODLOC,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_GEN_MOVE(NLSIZE*NTSIZE,%VAL(DPTR),%VAL(OPTR))
      ENDIF
      CALL TSP_UNMAP(ODLOC,STATUS)
      CALL TSP_UNMAP(DLOC,STATUS)

*  Copy the Variance array if present

      IF (VARIANCE) THEN
          CALL TSP_MAP_VAR(OLOC,'WRITE',OPTR,ODLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_GEN_MOVE(NLSIZE*NTSIZE,%VAL(VPTR),%VAL(OPTR))
          ENDIF
          CALL TSP_UNMAP(ODLOC,STATUS)
          CALL TSP_UNMAP(VLOC,STATUS)
      ENDIF

*  Copy the Stokes Parameters

      CALL TSP_STOKES(LOC,NUM,QZ,UZ,VZ,STATUS)
      IF (QZ) THEN

*  Copy the Q Stokes parameter

         CALL TSP_GET_STOKES(LOC,'Q',SLOC,STATUS)

*  Map the required slice of the Q array

         CALL TSP_MAP_SLICE(SLOC,ACTDIM,LOWER,UPPER,'READ',DPTR,
     :        DLOC,STATUS)

*  And of the variance array if present

          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_MAP_VSLICE(SLOC,ACTDIM,LOWER,UPPER,'READ',VPTR,
     :        VLOC,STATUS)
              IF (STATUS .NE. SAI__OK) THEN
                  VARIANCE = .FALSE.
                  STATUS = SAI__OK
              ELSE
                  VARIANCE = .TRUE.
              ENDIF
          ENDIF

*  Create Stokes component on output file and copy to it

          CALL TSP_ADD_STOKES(OLOC,'Q',VARIANCE,STATUS)
          CALL TSP_GET_STOKES(OLOC,'Q',OSLOC,STATUS)

*  Map the Q stokes data of the output file
          CALL TSP_MAP_DATA(OSLOC,'WRITE',OPTR,ODLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN

*  Copy Q stokes data
              CALL TSP_GEN_MOVE(NLSIZE*NTSIZE,%VAL(DPTR),%VAL(OPTR))
          ENDIF

*  Unmap arrays
          CALL TSP_UNMAP(ODLOC,STATUS)
          CALL TSP_UNMAP(DLOC,STATUS)
          IF (VARIANCE) THEN

*  Map the Q variance array in the output dataset
              CALL TSP_MAP_VAR(OSLOC,'WRITE',OPTR,ODLOC,STATUS)
              IF (STATUS .EQ. SAI__OK) THEN

*  Copy the Q stokes variance
                  CALL TSP_GEN_MOVE(NLSIZE*NTSIZE,%VAL(VPTR),%VAL(OPTR))
              ENDIF

*  Unmap arrays
              CALL TSP_UNMAP(ODLOC,STATUS)
              CALL TSP_UNMAP(VLOC,STATUS)
          ENDIF
          CALL DAT_ANNUL(OSLOC,STATUS)
          CALL DAT_ANNUL(SLOC,STATUS)
      ENDIF

      IF (UZ) THEN

*  Copy the U Stokes parameter

         CALL TSP_GET_STOKES(LOC,'U',SLOC,STATUS)

*  Map the required slice of the U array

         CALL TSP_MAP_SLICE(SLOC,ACTDIM,LOWER,UPPER,'READ',DPTR,
     :        DLOC,STATUS)

*  And of the variance array if present

          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_MAP_VSLICE(SLOC,ACTDIM,LOWER,UPPER,'READ',VPTR,
     :        VLOC,STATUS)
              IF (STATUS .NE. SAI__OK) THEN
                  VARIANCE = .FALSE.
                  STATUS = SAI__OK
              ELSE
                  VARIANCE = .TRUE.
              ENDIF
          ENDIF

*  Create Stokes component on output file and copy to it

          CALL TSP_ADD_STOKES(OLOC,'U',VARIANCE,STATUS)
          CALL TSP_GET_STOKES(OLOC,'U',OSLOC,STATUS)

*  Map the U Stokes data
          CALL TSP_MAP_DATA(OSLOC,'WRITE',OPTR,ODLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN

*  Copy the U Stokes data
              CALL TSP_GEN_MOVE(NLSIZE*NTSIZE,%VAL(DPTR),%VAL(OPTR))
          ENDIF

*  Unmap arrays
          CALL TSP_UNMAP(ODLOC,STATUS)
          CALL TSP_UNMAP(DLOC,STATUS)
          IF (VARIANCE) THEN

*  Map the U Stokes variance
              CALL TSP_MAP_VAR(OSLOC,'WRITE',OPTR,ODLOC,STATUS)
              IF (STATUS .EQ. SAI__OK) THEN

*  Copy the U Stokes variance
                  CALL TSP_GEN_MOVE(NLSIZE*NTSIZE,%VAL(VPTR),%VAL(OPTR))
              ENDIF

*  Unmap the arrays
              CALL TSP_UNMAP(ODLOC,STATUS)
              CALL TSP_UNMAP(VLOC,STATUS)
          ENDIF
          CALL DAT_ANNUL(OSLOC,STATUS)
          CALL DAT_ANNUL(SLOC,STATUS)
      ENDIF

      IF (VZ) THEN

*  Copy the V Stokes parameter

         CALL TSP_GET_STOKES(LOC,'V',SLOC,STATUS)

*  Map the required slice of the V array

         CALL TSP_MAP_SLICE(SLOC,ACTDIM,LOWER,UPPER,'READ',DPTR,
     :        DLOC,STATUS)

*  And of the variance array if present

          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_MAP_VSLICE(SLOC,ACTDIM,LOWER,UPPER,'READ',VPTR,
     :        VLOC,STATUS)
              IF (STATUS .NE. SAI__OK) THEN
                  VARIANCE = .FALSE.
                  STATUS = SAI__OK
              ELSE
                  VARIANCE = .TRUE.
              ENDIF
          ENDIF

*  Create Stokes component on output file and copy to it

          CALL TSP_ADD_STOKES(OLOC,'V',VARIANCE,STATUS)
          CALL TSP_GET_STOKES(OLOC,'V',OSLOC,STATUS)

*  Map the V Stokes data
          CALL TSP_MAP_DATA(OSLOC,'WRITE',OPTR,ODLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN

*  Copy the V stokes data
              CALL TSP_GEN_MOVE(NLSIZE*NTSIZE,%VAL(DPTR),%VAL(OPTR))
          ENDIF

*  Unmap the arrays
          CALL TSP_UNMAP(ODLOC,STATUS)
          CALL TSP_UNMAP(DLOC,STATUS)
          IF (VARIANCE) THEN

*  Map the V stokes variance
              CALL TSP_MAP_VAR(OSLOC,'WRITE',OPTR,ODLOC,STATUS)
              IF (STATUS .EQ. SAI__OK) THEN

*  Copy the V stokes variance
                  CALL TSP_GEN_MOVE(NLSIZE*NTSIZE,%VAL(VPTR),%VAL(OPTR))
              ENDIF

*  Unmap the arrays
              CALL TSP_UNMAP(ODLOC,STATUS)
              CALL TSP_UNMAP(VLOC,STATUS)
          ENDIF
          CALL DAT_ANNUL(OSLOC,STATUS)
          CALL DAT_ANNUL(SLOC,STATUS)
      ENDIF

*  Copy Label and Units

      CALL TSP_RLU(LOC,LABEL,UNITS,STATUS)
      CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)

*  Copy Wavelength Axis Data

      CALL TSP_MAP_LAMBDA(OLOC,'WRITE',OPTR,ODLOC,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_GEN_MOVE(NLSIZE,%VAL(LPTR+4*(XSTART-1)),%VAL(OPTR))
      ENDIF

*  Unmap the arrays
      CALL TSP_UNMAP(ODLOC,STATUS)
      CALL TSP_UNMAP(LLOC,STATUS)

*  Copy wavelength label and units
      CALL TSP_RLU_LAMBDA(LOC,LABEL,UNITS,STATUS)
      CALL TSP_WLU_LAMBDA(OLOC,LABEL,UNITS,STATUS)

*  Copy Time Axis Data

      IF (ACTDIM .GT. 1) THEN
          CALL TSP_MAP_TIME(OLOC,'WRITE',OPTR,ODLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN
            CALL TSP_GEN_MOVE(2*NTSIZE,%VAL(TPTR+8*(YSTART-1)),
     :             %VAL(OPTR))
          ENDIF

*  Unmap arrays
          CALL TSP_UNMAP(ODLOC,STATUS)
          CALL TSP_UNMAP(TLOC,STATUS)

*  Copy the time axis label and units
          CALL TSP_RLU_TIME(LOC,LABEL,UNITS,STATUS)
          CALL TSP_WLU_TIME(OLOC,LABEL,UNITS,STATUS)
      ENDIF

      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



      SUBROUTINE TSP_SUBLRANGE(SIZE,L,XSTART,XEND,STATUS)
*+
*
*  T S P _ S U B L R A N G E
*
*  SUBSET command
*
*  Get range of data for subset in the X (wavelength) direction.
*  The required range of wavelength is prompted for, and the wavelength
*  array is searched to find the limits in X which this corresponds to.
*
*  Parameters:
*
*  (>)  SIZE   (Integer)            Size of wavelength array
*  (>)  L      (Real array(SIZE))   Array of wavelength axis data
*  (<)  XSTART (Integer)            First X index to use
*  (<)  XEND   (Integer)            Last X index to use
*  (!)  STATUS (Integer)            Status value
*
*  Jeremy Bailey   30/4/1988
*
*  Modified:
*      17/12/1991
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE,STATUS,XSTART,XEND
      REAL L(SIZE)

*  Local variables
      REAL LSTART,LEND,LL

*  Get LSTART parameter
      CALL PAR_GET0R('LSTART',LSTART,STATUS)

*  Get LEND parameter
      CALL PAR_GET0R('LEND',LEND,STATUS)

      IF (L(SIZE) .GT. L(1)) THEN

*  Search for XSTART index corresponding to LSTART
          XSTART = 1
          DO WHILE (L(XSTART) .LT. LSTART .AND. XSTART .LT. SIZE)
              XSTART = XSTART+1
          ENDDO

*  Search for XEND index corresponding to LEND
          XEND = SIZE
          DO WHILE (L(XEND) .GT. LEND .AND. XEND .GT. XSTART)
              XEND = XEND-1
          ENDDO
      ELSE

*  Do the searches the other way round for the case where the wavelength
*  axis is reversed
          XSTART = 1

*  Search for XSTART index corresponding to LEND
          DO WHILE (L(XSTART) .GT. LEND .AND. XSTART .LT. SIZE)
              XSTART = XSTART+1
          ENDDO

*  Search for XEND index corresponding to LSTART
          XEND = SIZE
          DO WHILE (L(XEND) .LT. LSTART .AND. XEND .GT. XSTART)
              XEND = XEND-1
          ENDDO
       ENDIF
      END



      SUBROUTINE TSP_SUBTRANGE(SIZE,T,YSTART,YEND,STATUS)
*+
*
*  T S P _ S U B T R A N G E
*
*  SUBSET command
*
*  Get range of data for subset in the Y (time) direction.
*  The required range of MJD is prompted for, and the time
*  array is searched to find the limits in Y which this corresponds to.
*
*  Parameters:
*
*  (>)  SIZE   (Integer)            Size of time axis array
*  (>)  T      (Double array(SIZE)) Array of time axis data (MJD)
*  (<)  YSTART (Integer)            First Y index to use
*  (<)  YEND   (Integer)            Last Y index to use
*  (!)  STATUS (Integer)            Status value
*
*  Jeremy Bailey   30/4/1988
*
*  Modified:
*      17/12/1991
*      20/03/2000   Use PAR_GET0D to obtain TSTART,TEND.  BLY/RAL
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE,STATUS,YSTART,YEND
      DOUBLE PRECISION T(SIZE)

*  Local variables
      DOUBLE PRECISION TSTART,TEND

*  Get Starting time value
      CALL PAR_GET0D('TSTART',TSTART,STATUS)

*  Get ending time value
      CALL PAR_GET0D('TEND',TEND,STATUS)

*  Search time axis array for YSTART corresponding to TSTART
      YSTART = 1
      DO WHILE (T(YSTART) .LT. TSTART .AND. YSTART .LT. SIZE)
          YSTART = YSTART+1
      ENDDO

*  Search time axis array for YEND corresponding to TEND
      YEND = SIZE
      DO WHILE (T(YEND) .GT. TEND .AND. YEND .GT. YSTART)
          YEND = YEND-1
      ENDDO
      END

