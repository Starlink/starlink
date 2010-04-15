C+
      SUBROUTINE LTCORR(STATUS)
C
C            L T C O R R
C
C     Command Name:
C        LTCORR
C
C     Function:
C        Apply Light Time corrections to the time axis of a data set.
C
C     Description:
C        LTCORR applies light time corrections to the time axis of a data
C        set, converting observed times to heliocentric or barycentric
C        times. If the parameter SINGLE is true a single correction is
C        calculated for the mid point time of the dataset, and applied to
C        all points in the dataset. If SINGLE is false the correction is
C        recalculated for each data point.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset with
C                               observed times.
C    (2) OUTPUT     (TSP, 2D)  The output corrected dataset with heliocentric
C                               or barycentric times.
C    (3) RA         (Char)     The B1950 mean Right Ascension of the
C                               observed source.
C    (4) DEC        (Char)     The B1950 mean declination of the observed
C                               source.
C        BARY       (Logical)  If True, correction is to the solar system
C                               Barycentre. If False, to the heliocentre.
C        SINGLE     (Logical)  If True, a single correction is calculated
C                               for the mid point time of the dataset. If
C                               False, the correction is recalculated for
C                               each point.
C        REVERSE    (Logical)  If True, a reverse correction is performed.
C                               e.g. heliocentric times are converted to
C                               observed times.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 27/2/1988
C
C-
C
C  History:
C    30/11/1987   Original Version.   JAB/AAO
C    27/2/1988   TSP Monolith version.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS


*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,LOC

*  Array sizes
      INTEGER SIZE
      INTEGER DIMS(7),NDIMS

*  Pointer to time axis
      INTEGER PTR
      CHARACTER*80 RASTRING,DECSTRING
      INTEGER IH,ID,IM
      DOUBLE PRECISION SEC,RM,DM
      INTEGER J,SIGN
      INTEGER NSTRT
      LOGICAL SINGLE,BARY,REVERSE
      CHARACTER*64 LABEL,UNITS

*  Get Locators to the input and output datasets

      CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get size of data

      CALL TSP_SIZE(OLOC,7,DIMS,NDIMS,STATUS)
      SIZE = DIMS(NDIMS)

*  Map the time axis of the output data set

      CALL TSP_MAP_TIME(OLOC,'UPDATE',PTR,LOC,STATUS)

*  Read label and units

      CALL TSP_RLU_TIME(OLOC,LABEL,UNITS,STATUS)

*  Check for a valid time axis label

      IF (LABEL .EQ. 'MJD(UTC)' .OR.
     :    LABEL .EQ. 'MJD(TAI)' .OR.
     :    LABEL .EQ. 'MJD(TDT)') THEN
          CONTINUE

*  Complain if data has already been light time corrected

      ELSE IF (LABEL(10:) .EQ. 'Heliocentric' .OR.
     :         LABEL(10:) .EQ. 'Barycentric') THEN
          CALL MSG_OUT('MSG','Data is already Corrected',STATUS)
          STATUS = USER__001
      ELSE

*  or if the label is not valid

          CALL MSG_OUT('MSG','Unrecognized Axis Label',STATUS)
          CALL MSG_OUT('MSG',LABEL,STATUS)
      ENDIF

*  Get RA and Dec

      CALL PAR_GET0C('RA',RASTRING,STATUS)
      CALL PAR_GET0C('DEC',DECSTRING,STATUS)

*  Decode RA

      IF (STATUS .EQ. SAI__OK) THEN
         NSTRT = 1
         CALL SLA_INTIN(RASTRING,NSTRT,IH,J)
         CALL SLA_INTIN(RASTRING,NSTRT,IM,J)
         CALL SLA_DFLTIN(RASTRING,NSTRT,SEC,J)
         CALL SLA_DTF2R(IH,IM,SEC,RM,J)
         IF (J .NE. 0) THEN
             CALL MSG_OUT('MSG','Invalid RA',STATUS)
             RETURN
         ENDIF

*  Decode Dec

         NSTRT = 1

*  Degrees and sign

         CALL SLA_INTIN(DECSTRING,NSTRT,ID,J)
         IF (J .EQ. -1) THEN
             ID = -ID
             SIGN = -1
         ELSE
             SIGN = 1
         ENDIF

*  Minutes and seconds

         CALL SLA_INTIN(DECSTRING,NSTRT,IM,J)
         CALL SLA_DFLTIN(DECSTRING,NSTRT,SEC,J)
         CALL SLA_DAF2R(ID,IM,SEC,DM,J)
         IF (SIGN .EQ. -1) THEN
             DM = -DM
         ENDIF
         IF (J .NE. 0) THEN
             CALL MSG_OUT('MSG','Invalid DEC',STATUS)
             RETURN
         ENDIF
         PRINT *,RM,DM

*  Get SINGLE flag

         CALL PAR_GET0L('SINGLE',SINGLE,STATUS)

*  Get Barycentric flag

         CALL PAR_GET0L('BARY',BARY,STATUS)

*  Get Reverse Flag

         CALL PAR_GET0L('REVERSE',REVERSE,STATUS)

*  Perform the correction

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_LTCORR(RM,DM,SINGLE,BARY,REVERSE,SIZE,
     :           %VAL(PTR),STATUS)
         ENDIF

*  Update Axis label by appending 'Barycentric' or 'Heliocentric'

         IF (LABEL .EQ. 'MJD(UTC)') THEN
             IF (BARY) THEN
                 LABEL = 'MJD(UTC) Barycentric'
             ELSE
                 LABEL = 'MJD(UTC) Heliocentric'
             ENDIF
         ELSE IF (LABEL .EQ. 'MJD(TDT)') THEN
             IF (BARY) THEN
                 LABEL = 'MJD(TDT) Barycentric'
             ELSE
                 LABEL = 'MJD(TDT) Heliocentric'
             ENDIF
         ELSE IF (LABEL .EQ. 'MJD(TAI)') THEN
             IF (BARY) THEN
                 LABEL = 'MJD(TAI) Barycentric'
             ELSE
                 LABEL = 'MJD(TAI) Heliocentric'
             ENDIF
         ENDIF

*  If correction is reverse just take first eight characters of label

         IF (REVERSE) THEN
             LABEL = LABEL(1:8)
         ENDIF
         CALL TSP_WLU_TIME(OLOC,LABEL,UNITS,STATUS)
      ENDIF

*  Unmap data and annul locators

      CALL TSP_UNMAP(LOC,STATUS)
      CALL DAT_ANNUL(ILOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      IF (STATUS .EQ. USER__001) STATUS = SAI__OK
      END



      SUBROUTINE TSP_LTCORR(RM,DM,SINGLE,BARY,REVERSE,SIZE,TIMES,
     :     STATUS)
*+
*
*   T S P _ L T C O R R
*
*   TSP_LTCORR applies light time corrections to the data in the TIMES
*   array (A double precision array of MJDs). The corrections are those
*   needed to convert an observed MJD of an object at B1950 FK4 position
*   (RM,DM) to a heliocentric or barycentric MJD for the same
*   observation.
*
*   The SINGLE flag, if specified causes a single correction to be calculated
*   for the mid point of the observation. Otherwise the correction is
*   recalculated for every point (This can be very slow if the array is
*   large).
*
*   (>)  RM       (Double)          B1950 RA of object
*   (>)  DM       (Double)          B1950 Dec of object
*   (>)  SINGLE   (Logical)         TRUE if a single correction is to be
*                                     calculated.
*   (>)  BARY     (Logical)         TRUE if correction is to solar system
*                                     barycentre, FALSE for heliocentric.
*   (>)  REVERSE  (Logical)         TRUE for a reverse correction.
*   (>)  SIZE     (Integer)         Size of time axis array.
*   (!)  TIMES    (Double array(SIZE)) Array of MJD times.
*   (!)  STATUS   (Integer)         Status value.
*
*   Jeremy Bailey    27/2/1988
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE,STATUS
      DOUBLE PRECISION RM,DM,R2000,D2000,TIMES(SIZE)

*  Local variables
      DOUBLE PRECISION TL
      INTEGER I
      DOUBLE PRECISION V(3),DVB(3),DPB(3),DVH(3),DPH(3),MJD
      LOGICAL SINGLE,BARY,REVERSE
      DOUBLE PRECISION SLA_DVDV

*   Transform star position to J2000 FK5

      CALL SLA_FK45Z(RM,DM,1990.0D0,R2000,D2000)

*  Convert to cartesian vector

      CALL SLA_DCS2C(R2000,D2000,V)

*   Light Time corrections - single correction for mean time

      IF (SINGLE) THEN

*   Calculate mid point MJD

          MJD = 0.5D0 * (TIMES(1)+TIMES(SIZE))

*   Calculate Earth position

          CALL SLA_EVP(MJD,2000.0D0,DVB,DPB,DVH,DPH)

*   Light time correction is scalar product of star position and
*   earth position times light time for one a.u.

          IF (BARY) THEN
              TL = 499.0047837D0 * SLA_DVDV(DPB,V)
          ELSE
              TL = 499.0047837D0 * SLA_DVDV(DPH,V)
          ENDIF

*  Output correction

          CALL MSG_SETD('TL',TL)
          CALL MSG_OUT('MSG','Correction = ^TL seconds',STATUS)

*  Apply as correction to MJD array

          IF (REVERSE) THEN
            DO I=1,SIZE
              TIMES(I) = TIMES(I) - TL/86400.0D0
            ENDDO
          ELSE
            DO I=1,SIZE
              TIMES(I) = TIMES(I) + TL/86400.0D0
            ENDDO
          ENDIF
      ELSE

*   Correct each point individually

          DO I=1,SIZE
              MJD = TIMES(I)

*  Get earth position

              CALL SLA_EVP(MJD,2000.0D0,DVB,DPB,DVH,DPH)

*   Light time correction is scalar product of star position and
*   earth position times light time for one a.u.

              IF (BARY) THEN
                  TL = 499.0047837D0 * SLA_DVDV(DPB,V)
              ELSE
                  TL = 499.0047837D0 * SLA_DVDV(DPH,V)
              ENDIF

*  Output correction for first point

              IF (I .EQ. 1) THEN
                  CALL MSG_SETD('TL',TL)
                  CALL MSG_OUT('MSG','Correction = ^TL seconds',STATUS)
              ENDIF

*  Correct MJD in array

              IF (REVERSE) THEN
                  TIMES(I) = TIMES(I) - TL/86400.0D0
              ELSE
                  TIMES(I) = TIMES(I) + TL/86400.0D0
              ENDIF
          ENDDO
      ENDIF
      END

