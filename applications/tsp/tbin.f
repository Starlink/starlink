C+
      SUBROUTINE TBIN(STATUS)
C
C            T B I N
C
C     Command name:
C        TBIN
C
C     Function:
C        Bin a time series
C
C     Description:
C        TBIN creates a new time series by binning an input time series
C        into bins of a specified size. All the points falling within
C        one bin are averaged, and their time value is averaged to create
C        the new bin time. Note that this means that the output is not
C        necessarily equally spaced in time. It depends where the points
C        fall within the bin.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C        BIN        (Double)   The bin size (days).
C        WHOLE      (Logical)  If TRUE, All time points are used.
C    (C) XSTART     (Double)   First time value (MJD) to use.
C    (C) XEND       (Double)   Last time value (MJD) to use.
C        OUTPUT     (TSP, 2D)  The output binned dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         26/2/1988
C
C-
C
C  History:
C    Aug/1987   Original Version.   JAB/AAO
C    26/2/1988   TSP Monolith version.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS
      INTEGER IPTR,QPTR,UPTR,VPTR          ! Pointers to Stokes arrays
      INTEGER XPTR                         ! Pointer to X (time) array
      INTEGER WPTR                         ! Pointer to wavelength axis
      INTEGER STRT,FIN                     ! Start and finish channels
      INTEGER SIZE                         ! Original size of data in X
      INTEGER FSIZE                        ! Size after X limits selection
      INTEGER NEWSIZE                      ! Size of binned data
      INTEGER ACTSIZE                      ! Size after removing points
                                           ! with no data
      CHARACTER*(DAT__SZLOC) LOC           ! Top level locator
      INTEGER CHAN                         ! Current channel
      INTEGER NCHANS                       ! Number of channels in data
      DOUBLE PRECISION BINSIZE             ! Binsize
      CHARACTER*64 LABEL,UNITS
      INTEGER DIMS(7),ACTDIM
      CHARACTER*3 STOKES

*   Temporary array pointers and locators

      CHARACTER*(DAT__SZLOC) YRLOC,YELOC,ALOC,OLOC,OILOC,OIVLOC
      CHARACTER*(DAT__SZLOC) XXLOC,XDLOC,TILOC,TDLOC,QLOC,QILOC,QIVLOC
      CHARACTER*(DAT__SZLOC) ULOC,UILOC,UIVLOC,VLOC,VILOC,VIVLOC
      CHARACTER*(DAT__SZLOC) XLOC,WLOC
      INTEGER XXPTR,XDPTR,TIPTR,TDPTR,OIPTR,OIVPTR,APTR
      INTEGER YRPTR,YEPTR,QIPTR,QIVPTR,UIPTR,UIVPTR,VIPTR,VIVPTR

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*  Get X Axis data


*  Find size of array
        CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
        STRT=1

*  If the array is three dimensional, treat it as 2D collapsing the
*  first two dimensions into one
        IF (ACTDIM .EQ. 3) THEN
            FIN=DIMS(3)
            NCHANS = DIMS(1)*DIMS(2)
        ELSE IF (ACTDIM .EQ. 2) THEN
            FIN=DIMS(2)
            NCHANS=DIMS(1)
        ELSE
            CALL MSG_OUT(' ','Not a time series dataset',STATUS)
            CALL DAT_ANNUL(LOC,STATUS)
            RETURN
        ENDIF
        SIZE = FIN-STRT+1

*  Map the time axis data
        CALL TSP_MAP_TIME(LOC,'READ',XPTR,XLOC,STATUS)

*  Map the wavelength axis data
        CALL TSP_MAP_LAMBDA(LOC,'READ',WPTR,WLOC,STATUS)

*  Get temporary arrays

        CALL TSP_TEMP(SIZE,'_REAL',YRPTR,YRLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',YEPTR,YELOC,STATUS)
        CALL TSP_TEMP(SIZE,'_INTEGER',TIPTR,TILOC,STATUS)
        CALL TSP_TEMP(SIZE,'_DOUBLE',TDPTR,TDLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_DOUBLE',XXPTR,XXLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_DOUBLE',XDPTR,XDLOC,STATUS)

*   Find X limits

        CALL TSP_PHSXLIMITS(%VAL(XPTR),STRT,FIN,%VAL(XXPTR),STATUS)
        SIZE = FIN-STRT+1
        FSIZE = SIZE

*  Loop over channels

        CHAN = 1
        DO WHILE (CHAN .LE. NCHANS)
          SIZE = FSIZE

*   Map required data arrays

          CALL TSP_PHSGETITEM(LOC,CHAN,STRT,FIN,IPTR,QPTR,UPTR,
     :          VPTR,STATUS)

*  Bin Size?

          CALL PAR_GET0D('BIN',BINSIZE,STATUS)

*  Bin data

          CALL TSP_TBBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(IPTR),%VAL(YRPTR),%VAL(YEPTR),BINSIZE,
     :             %VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)

*  Create the output file

          IF (CHAN .EQ. 1) THEN
              CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
              CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Get new size

              CALL TSP_TBSIZE(NEWSIZE,%VAL(XDPTR),ACTSIZE,%VAL(XXPTR))

*  Build the string describing the Stokes parameters needed in the
*  output dataset

              IF (QPTR .NE. 0 .AND. UPTR.NE.0 .AND. VPTR.NE.0) THEN
                  STOKES = 'QUV'
              ELSE IF (QPTR.NE.0 .AND. UPTR.NE.0) THEN
                  STOKES = 'QU'
              ELSE IF (QPTR.NE.0 .AND. VPTR.NE.0) THEN
                  STOKES = 'QV'
              ELSE IF (UPTR.NE.0 .AND. VPTR.NE.0) THEN
                  STOKES = 'UV'
              ELSE IF (QPTR .NE. 0) THEN
                  STOKES = 'Q'
              ELSE IF (UPTR .NE. 0) THEN
                  STOKES = 'U'
              ELSE IF (VPTR .NE. 0) THEN
                  STOKES = 'V'
              ELSE
                  STOKES = ' '
              ENDIF

*  Create the output dataset
              IF (ACTDIM .EQ. 2) THEN
                  CALL TSP_CREATE_2D(OLOC,NCHANS,ACTSIZE,STOKES,.TRUE.,
     :               .TRUE.,STATUS)
              ELSE
                  CALL TSP_CREATE_3D(OLOC,DIMS(1),DIMS(2),ACTSIZE,
     :               STOKES,.TRUE.,.TRUE.,STATUS)
              ENDIF

*  Write Wavelength axis (X-axis) data

              CALL TSP_MAP_LAMBDA(OLOC,'WRITE',APTR,ALOC,STATUS)
              CALL TSP_GEN_MOVE(DIMS(1),%VAL(WPTR),%VAL(APTR))
              CALL TSP_UNMAP(ALOC,STATUS)

*  Copy the wavelength axis label and units
              CALL TSP_RLU_LAMBDA(LOC,LABEL,UNITS,STATUS)
              CALL TSP_WLU_LAMBDA(OLOC,LABEL,UNITS,STATUS)

*  If 3D write Y-axis data

              IF (ACTDIM .EQ. 3) THEN
                  CALL TSP_MAP_Y(OLOC,'WRITE',APTR,ALOC,STATUS)
                  CALL TSP_GEN_MOVE(DIMS(2),%VAL(WPTR),%VAL(APTR))
                  CALL TSP_UNMAP(ALOC,STATUS)
                  CALL TSP_RLU_Y(LOC,LABEL,UNITS,STATUS)
                  CALL TSP_WLU_Y(OLOC,LABEL,UNITS,STATUS)
              ENDIF

*  Write Time axis data

              CALL TSP_MAP_TIME(OLOC,'WRITE',APTR,ALOC,STATUS)
              CALL TSP_GEN_MOVE(2*ACTSIZE,%VAL(XXPTR),%VAL(APTR))
              CALL TSP_UNMAP(ALOC,STATUS)

*  Copy label and units of time axis
              CALL TSP_RLU_TIME(LOC,LABEL,UNITS,STATUS)
              CALL TSP_WLU_TIME(OLOC,LABEL,UNITS,STATUS)


              DIMS(1)=NCHANS
              DIMS(2)=ACTSIZE

*  Map the output intensity data ...
              CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,OILOC,STATUS)

*  ... and the output variance
              CALL TSP_MAP_VAR(OLOC,'WRITE',OIVPTR,OIVLOC,STATUS)

              IF (QPTR .NE. 0) THEN

*  Q Stokes parameter
                CALL TSP_GET_STOKES(OLOC,'Q',QLOC,STATUS)

*  Map the Q Stokes data
                CALL TSP_MAP_DATA(QLOC,'WRITE',QIPTR,QILOC,STATUS)

*  Map the Q Stokes variance
                CALL TSP_MAP_VAR(QLOC,'WRITE',QIVPTR,QIVLOC,STATUS)
                CALL DAT_ANNUL(QLOC,STATUS)
              ENDIF

              IF (UPTR .NE. 0) THEN

*  U Stokes parameter
                CALL TSP_GET_STOKES(OLOC,'U',ULOC,STATUS)

*  Map the U Stokes data
                CALL TSP_MAP_DATA(ULOC,'WRITE',UIPTR,UILOC,STATUS)

*  Map the U Stokes variance
                CALL TSP_MAP_VAR(ULOC,'WRITE',UIVPTR,UIVLOC,STATUS)
                CALL DAT_ANNUL(ULOC,STATUS)
              ENDIF

              IF (VPTR .NE. 0) THEN

*  V Stokes parameter
                CALL TSP_GET_STOKES(OLOC,'V',VLOC,STATUS)

*  Map the V Stokes data
                CALL TSP_MAP_DATA(VLOC,'WRITE',VIPTR,VILOC,STATUS)

*  Map the V Stokes variance
                CALL TSP_MAP_VAR(VLOC,'WRITE',VIVPTR,VIVLOC,STATUS)
                CALL DAT_ANNUL(VLOC,STATUS)
              ENDIF
          ENDIF

*  Copy the points containing good data to the output array
          IF (STATUS .EQ. SAI__OK) THEN
            CALL TSP_TBCPY(NEWSIZE,ACTSIZE,CHAN,NCHANS,%VAL(YRPTR),
     :        %VAL(YEPTR),%VAL(OIPTR),%VAL(OIVPTR),%VAL(XDPTR))
          ENDIF

          IF (QPTR .NE. 0) THEN

*  Bin the Q Stokes parameter
              CALL TSP_TBBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(QPTR),%VAL(YRPTR),%VAL(YEPTR),BINSIZE,
     :             %VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              IF (STATUS .EQ. SAI__OK) THEN

*  Copy the points containing good data to the output array
                CALL TSP_TBCPY(NEWSIZE,ACTSIZE,CHAN,NCHANS,%VAL(YRPTR),
     :            %VAL(YEPTR),%VAL(QIPTR),%VAL(QIVPTR),%VAL(XDPTR))
             ENDIF
          ENDIF
          IF (UPTR .NE. 0) THEN

*  Bin the U Stokes parameter
              CALL TSP_TBBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(UPTR),%VAL(YRPTR),%VAL(YEPTR),BINSIZE,
     :             %VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              IF (STATUS .EQ. SAI__OK) THEN

*  Copy the points containing good data to the output
                CALL TSP_TBCPY(NEWSIZE,ACTSIZE,CHAN,NCHANS,%VAL(YRPTR),
     :            %VAL(YEPTR),%VAL(UIPTR),%VAL(UIVPTR),%VAL(XDPTR))
              ENDIF
          ENDIF
          IF (VPTR .NE. 0) THEN

*  Bin the V Stokes parameter
              CALL TSP_TBBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(VPTR),%VAL(YRPTR),%VAL(YEPTR),BINSIZE,
     :             %VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              IF (STATUS .EQ. SAI__OK) THEN

*  Copy the points containing good data to the output
                 CALL TSP_TBCPY(NEWSIZE,ACTSIZE,CHAN,NCHANS,%VAL(YRPTR),
     :             %VAL(YEPTR),%VAL(VIPTR),%VAL(VIVPTR),%VAL(XDPTR))
              ENDIF
          ENDIF


*  Unmap the main arrays
          CALL TSP_PHSUNMAPITEM(LOC,STATUS)
          CHAN = CHAN+1
        ENDDO
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(WLOC,STATUS)
      CALL TSP_UNMAP(XLOC,STATUS)

*  Unmap the intensity arrays
      CALL TSP_UNMAP(OILOC,STATUS)
      CALL TSP_UNMAP(OIVLOC,STATUS)

*  Unmap the Stokes arrays
      IF (QPTR .NE. 0) THEN
          CALL TSP_UNMAP(QILOC,STATUS)
          CALL TSP_UNMAP(QIVLOC,STATUS)
      ENDIF
      IF (UPTR .NE. 0) THEN
          CALL TSP_UNMAP(UILOC,STATUS)
          CALL TSP_UNMAP(UIVLOC,STATUS)
      ENDIF
      IF (VPTR .NE. 0) THEN
          CALL TSP_UNMAP(VILOC,STATUS)
          CALL TSP_UNMAP(VIVLOC,STATUS)
      ENDIF


*  Unmap the temporary arrays
      CALL TSP_UNMAP(YRLOC,STATUS)
      CALL TSP_UNMAP(YELOC,STATUS)
      CALL TSP_UNMAP(XXLOC,STATUS)
      CALL TSP_UNMAP(XDLOC,STATUS)
      CALL TSP_UNMAP(TILOC,STATUS)
      CALL TSP_UNMAP(TDLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END



      SUBROUTINE TSP_TBCPY(SIZE,ACTSIZE,CHAN,NCHANS,X,XE,Y,YE,XD)
*+
*
*   T S P _ T B C P Y
*
*   TBIN command
*
*   Copy the good points from the input arrays to the appropriate channel
*   of the output array
*
*   Parameters:
*
*   (>)   SIZE     (Integer)             Size of input array
*   (>)   ACTSIZE  (Integer)             Number of  points in output array
*   (>)   CHAN     (Integer)             Channel of output array to write to
*   (>)   NCHANS   (Integer)             Number of channels in output array
*   (>)   X        (Real array(SIZE))    First input array
*   (>)   XE       (Real array(SIZE))    Second input array
*   (<)   Y        (Real array(NCHANS,ACTSIZE))   First output array
*   (<)   YE       (Real array(NCHANS,ACTSIZE))   Second output array
*   (>)   XD       (Double array(SIZE))    X array containing bad values
*                                          for each bin with no data
*
*   Jeremy Bailey    26/2/1988
*
*   Modified:
*       17/12/1991   -   Use VAL__BADD for bad points
*
*+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE,CHAN,NCHANS,ACTSIZE
      REAL X(SIZE),XE(SIZE)
      REAL Y(NCHANS,ACTSIZE),YE(NCHANS,ACTSIZE)
      DOUBLE PRECISION XD(SIZE)

*  Local variables
      INTEGER I,J

*  Loop over points
      J=1
      DO I=1,SIZE
          IF (XD(I) .NE. VAL__BADD) THEN

*  If data is good copy to output
              Y(CHAN,J) = X(I)
              YE(CHAN,J) = XE(I)
              J = J+1
          ENDIF
      ENDDO
      END


      SUBROUTINE TSP_TBSIZE(SIZE,X,ACTSIZE,XD)
*+
*
*  T S P _ T B S I Z E
*
*  TBIN command
*
*  Count the number of good points in the dataset, and return a
*  new X array containing just the good points
*
*  Parameters:
*
*  (>)   SIZE    (Integer)              Size of the array
*  (>)   X       (Double array(SIZE))   Array of X values
*  (<)   ACTSIZE (Integer)              Number of good points
*  (<)   XD      (Double array(SIZE))   New X array containing good points
*
*  Jeremy Bailey    26/2/1988
*
*  Modified:
*      17/12/1991   Use VAL__BADD for bad points
*
*+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE,ACTSIZE
      DOUBLE PRECISION X(SIZE),XD(SIZE)

*  Local variables
      INTEGER I,J

*  Loop over data points
      I=1
      DO J=1,SIZE
         IF (X(J) .NE. VAL__BADD) THEN
             XD(I)=X(J)
             I=I+1
         ENDIF
      ENDDO

*  Return actual number of good points
      ACTSIZE = I-1
      END







      SUBROUTINE TSP_TBBIN(SIZE,X,XB,Y,YB,YE,BINSIZE,NP,DS,
     :    NEWSIZE,STATUS)
*+
*
*  T S P _ T B B I N
*
*  TBIN command
*
*  Bin data with a specified binsize in the X axis. Bins containing
*  no data are returned with both the bin value and the new X array
*  containing a bad value.
*
*  (>)  SIZE     (Integer)   The number of data points before binning
*  (>)  X        (Double)    The X array before binning
*  (<)  XB       (Double)    The X array after binning
*  (>)  Y        (Real)      The Y array before binning
*  (<)  YB       (Real)      The Y array after binning
*  (<)  YE       (Real)      The array of Y errors (from bin statistics)
*  (>)  BINSIZE  (Double)    The size of the bins in X. A negative
*                              value means no binning will be done
*  (W)  NP       (Integer)   Workspace integer array
*  (W)  DS       (Double)    Workspace Double precision array
*  (<)  NEWSIZE  (Integer)   Number of data points after binning
*  (!)  STATUS   (Integer)   Status value
*
*  Jeremy Bailey    26/2/1988
*
*  Modified:
*      17/12/1991  -  Handle bad values
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'USER_ERR'

*  Parameters
      INTEGER SIZE,NEWSIZE
      REAL Y(SIZE), YB(SIZE), YE(SIZE)
      INTEGER STATUS
      INTEGER I
      DOUBLE PRECISION BINSIZE
      DOUBLE PRECISION X(*),XB(*)
      INTEGER NP(*)
      DOUBLE PRECISION DS(*)

*  Local variables
      DOUBLE PRECISION D
      INTEGER BIN
      REAL YX


      IF (STATUS .EQ. SAI__OK) THEN

*  determine size of binned array and check that it is not too large

         IF (BINSIZE .GT. 0.0) THEN
             NEWSIZE = (X(SIZE)-X(1))/BINSIZE + 1
         ELSE

*  If the binsize is negative we leave the data unchanged

             NEWSIZE = SIZE
         ENDIF
         IF (NEWSIZE .GT. SIZE) THEN
             CALL MSG_OUT('MSG','Too Many Bins',STATUS)
             STATUS = USER__001
             RETURN
         ENDIF

*  Zero initial contents of bins

         DO I=1,NEWSIZE
             YB(I) = 0.0
             DS(I) = 0.0
             NP(I) = 0
             XB(I) = 0.0
         ENDDO

*  Loop over points of unbinned data, adding into appropriate bin

         DO I=1,SIZE

*  Determine bin which this point lies in

             IF (BINSIZE .GT. 0.0) THEN
                 BIN = (X(I)-X(1))/BINSIZE + 1
             ELSE
                 BIN = I
             ENDIF

*  Add data into bin

             IF (Y(I) .NE. VAL__BADR) THEN
                 YB(BIN) = YB(BIN) + Y(I)
                 DS(BIN) = DS(BIN) + Y(I)*Y(I)
                 NP(BIN) = NP(BIN) + 1
                 XB(BIN) = XB(BIN) + X(I)
             ENDIF
         ENDDO

*  Calculate X,Y and errors for bin

         DO I=1,NEWSIZE
             IF (NP(I) .EQ. 0) THEN

*  No data in bin - return bad value
                 YB(I)=VAL__BADR
                 YE(I)=VAL__BADR
                 XB(I)=VAL__BADD             ! Signals no data in bin
             ELSE IF (NP(I) .EQ. 1) THEN
                 YE(I)=0.0
             ELSE IF (NP(I) .GE. 2) THEN

*  Calculate mean and standard error for bin
                 YB(I) = YB(I)/NP(I)
                 XB(I) = XB(I)/NP(I)
                 D = YB(I)
                 YE(I) = REAL((DS(I)-D*D*NP(I))/
     :                      (NP(I)*(NP(I)-1)))
             ENDIF
         ENDDO

      ENDIF
      END


