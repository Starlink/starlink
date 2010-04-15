C+
      SUBROUTINE TLIST(STATUS)
C
C            T L I S T
C
C     Command name:
C        TLIST
C
C     Function:
C        List time series data to a file.
C
C     Description:
C        List the time, intensity and percentage stokes parameters
C        for a specified channel of a time series data set to an
C        ASCII file
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C    (2) CHAN       (Integer)  Channel to plot.
C        WHOLE      (Logical)  If TRUE, All time points are used.
C    (C) XSTART     (Double)   First time value (MJD) to use.
C    (C) XEND       (Double)   Last time value (MJD) to use.
C        FILE       (File)     Name of file for output
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         27/2/1990
C
C-
C
C  History:
C    27/2/1990   Original Version.   JAB/JAC
C


      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS
      INTEGER IPTR,QPTR,UPTR,VPTR          ! Pointers to Stokes arrays
      INTEGER XPTR                         ! Pointer to X (time) array
      INTEGER WPTR                         ! Pointer to wavelength axis
      INTEGER STRT,FIN                     ! Start and finish channels
      INTEGER SIZE                         ! Original size of data in X
      INTEGER FSIZE                        ! Size after X limits selection
      CHARACTER*(DAT__SZLOC) LOC           ! Top level locator
      INTEGER CHAN                         ! Current channel to be listed
      INTEGER NCHANS                       ! Number of channels in data
      CHARACTER*(DAT__SZLOC) XLOC,WLOC     ! Axis locators
      INTEGER FD                           ! File descriptor
      INTEGER ACTDIM,DIMS(7)

*   Temporary array pointers and locators

      CHARACTER*(DAT__SZLOC) XXLOC,T1LOC
      INTEGER XXPTR,T1PTR,YRPTR

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*  Get Axis data

*  Map time axis

          CALL TSP_MAP_TIME(LOC,'READ',XPTR,XLOC,STATUS)

*  Map wavelength axis

          CALL TSP_MAP_LAMBDA(LOC,'READ',WPTR,WLOC,STATUS)

*  Get size of data

          CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
          STRT=1
          FIN=DIMS(2)
          NCHANS=DIMS(1)
          SIZE = FIN-STRT+1

*  Get temporary arrays

          CALL TSP_TEMP(SIZE,'_REAL',T1PTR,T1LOC,STATUS)
          CALL TSP_TEMP(SIZE,'_DOUBLE',XXPTR,XXLOC,STATUS)

*   Find X plotting limits

          CALL TSP_PHSXLIMITS(%VAL(XPTR),STRT,FIN,%VAL(XXPTR),STATUS)
          SIZE = FIN-STRT+1
          FSIZE = SIZE

*  Get channel number to list

          CALL PAR_GET0I('CHAN',CHAN,STATUS)
          IF ((CHAN .LE. 0) .OR. (CHAN .GT. NCHANS)) THEN
             CALL MSG_OUT('MSG','Invalid Channel',STATUS)
             CHAN=1
          ENDIF

*   Map required data arrays

          CALL TSP_PHSGETITEM(LOC,CHAN,STRT,FIN,IPTR,QPTR,UPTR,VPTR,
     :       STATUS)

*   Substitute temporary array for non existent stokes parameters

          IF (QPTR .EQ. 0) QPTR = T1PTR
          IF (UPTR .EQ. 0) UPTR = T1PTR
          IF (VPTR .EQ. 0) VPTR = T1PTR

*   Get file name

          CALL FIO_ASSOC('FILE','WRITE','LIST',0,FD,STATUS)

*   List data

          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_TLIST(SIZE,%VAL(XXPTR),%VAL(IPTR),%VAL(QPTR),
     :          %VAL(UPTR),%VAL(VPTR),FD,STATUS)
          ENDIF

*  Close file

          CALL FIO_CANCL('FILE',STATUS)

*  Unmap data

          CALL TSP_PHSUNMAPITEM(LOC,STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(XXLOC,STATUS)
      CALL TSP_UNMAP(T1LOC,STATUS)
      CALL TSP_UNMAP(XLOC,STATUS)
      CALL TSP_UNMAP(WLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END







      SUBROUTINE TSP_TLIST(NX,X,I,Q,U,V,FD,STATUS)
*+
*
*  T S P _ T L I S T
*
*  TLIST command
*
*  Output a listing of the selected data
*
*  (>)  NX     (Integer)            Number of data points in the arrays
*  (>)  X      (Double array(SIZE)) X axis (time) array
*  (>)  I      (Real array(SIZE))   Intensity array
*  (>)  Q      (Real array(SIZE))   Q Stokes parameter array
*  (>)  U      (Real array(SIZE))   U Stokes parameter array
*  (>)  V      (Real array(SIZE))   V Stokes parameter array
*  (>)  FD     (Integer)            File descriptor for output file
*  (!)  STATUS (Integer)            Status value
*
*   Jeremy Bailey    27/2/1990
*
*+

*  Parameters
      IMPLICIT NONE
      INTEGER NX,FD,STATUS
      DOUBLE PRECISION X(NX)
      REAL I(NX),Q(NX),U(NX),V(NX)

*  Local variables
      REAL QP,UP,VP
      INTEGER IX
      INTEGER IOS
      CHARACTER*80 BUFFER

*  Loop over points
      DO IX=1,NX

*  Calculate percentage stokes parameters
          QP = 100.0*Q(IX)/I(IX)
          UP = 100.0*U(IX)/I(IX)
          VP = 100.0*V(IX)/I(IX)

*  Write results to buffer
          WRITE(BUFFER,'(I5,F16.7,G14.4,3F12.3)',IOSTAT=IOS)
     :        IX,X(IX),I(IX),QP,UP,VP

*  Output buffer to file
          CALL FIO_WRITE(FD,BUFFER,STATUS)
      ENDDO

      END
