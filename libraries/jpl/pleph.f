      SUBROUTINE PLEPH (TDB, NP, NC, R, OK)
*+
*
*  - - - - - -
*   P L E P H
*  - - - - - -
*
*  Read and interpolate one item from a JPL ephemeris file.
*
*  Given (as arguments):
*     TDB      d        TDB as either JD or MJD (see note 1, below)
*     NP       i        body whose coordinates are required
*     NC       i        body defining origin of the coordinate system
*
*  Given (in /STCOMM/ common):
*     KM       l        units:
*                        .TRUE.  = km, sec
*                        .FALSE. = AU, days
*
*  Returned (argument):
*     R        d(6)     x,y,z,dx/dt,dy/dt,dz/dt
*     OK       l        status:  .TRUE. = TDB within range of ephemeris
*
*  Explanation:
*     This routine reads and interpolates a JPL ephemeris file to
*     give one item - most commonly the position and velocity of a
*     body relative to a given origin.  Though slightly less efficient
*     and flexible than the STATE subroutine (qv), it is simpler to
*     use.
*
*     It can also be used to obtain the nutation components, though the
*     SLALIB routine sla_NUTC is the more direct method under normal
*     circumstances.
*
*     The arguments NP and NC, which represent respectively the body
*     or reference point whose position and velocity is required and
*     the origin of the Cartesian reference frame, can each take the
*     following values:
*
*         NP or NC        meaning
*
*            1            Mercury
*            2            Venus
*            3            Earth
*            4            Mars
*            5            Jupiter
*            6            Saturn
*            7            Uranus
*            8            Neptune
*            9            Pluto
*           10            Moon
*           11            Sun
*           12            SSB
*           13            EMB
*
*     In each case, the position and velocity of the point specified
*     by NP relative to the point specified by NC is put into R(1-6).
*     Alternatively, for NP=14 and NC=0, the nutation (dPsi, dEps,
*     ddPsi, ddEps) is put into R(1-4).  The units are determined by
*     the /STCOMM/ location KM, which is .TRUE. for km and sec, and
*     .FALSE. for AU and days (default AU and days).
*
*     For example, the following call will obtain the position and
*     velocity of the Earth relative to the Solar System barycentre
*     at 0 hours TDB on 30 April 1986:
*
*        CALL PLEPH (46550D0, 3, 12, R, OK)
*
*     Errors - TDB out of range or illegal NP, NC value - are indicated
*     by all elements of the result array R being set to zero.
*
*     PLEPH is a coroutine - values determined on the first call are
*     saved and used again on subsequent calls.
*
*  Called:
*     CONST, STATE
*
*  Notes:
*
*  1  The argument TDB can either be Julian Date or Modified Julian
*     Date (JD-2400000.5).  A TDB value of more than 2400000.5 is
*     assumed to be a JD, while a TDB smaller than this value is
*     taken to be an MJD.  The preferred form is MJD as this better
*     conserves precision.  The option of using JD is provided
*     only for compatibility with the JPL-supplied routine of the
*     same name (see note 2).
*
*  2  This routine was suggested by E Myles Standish Jr (private
*     communication, 1986).  Subsequent releases from JPL included
*     an "official" PLEPH routine, which unfortunately differs from
*     Starlink's version in using JD rather than MJD for the first
*     argument.  A revised form of Starlink's version has been adopted,
*     supporting both forms of date (see note 1);  the JPL PLEPH is not
*     used in the Starlink release.
*
*  P T Wallace    Starlink    25 April 1994
*-

      IMPLICIT NONE

      DOUBLE PRECISION TDB
      INTEGER NP,NC
      DOUBLE PRECISION R(6)
      LOGICAL OK

*  Common
      LOGICAL KM,BARY
      DOUBLE PRECISION PVSUN(3,2)
      COMMON /STCOMM/ KM,BARY,PVSUN
      EXTERNAL COMDAT

*  Saved value of BARY
      LOGICAL SBARY

*  First time flag
      LOGICAL JFTF

*
*  For CONST call:
*
*  Array for 6-character names of constants (with 2 trailing spaces)
      CHARACTER*6 NAM(400)
*  Arrays for data values of constants
      DOUBLE PRECISION VAL(400)
      DOUBLE PRECISION SS(3)
*  Number of data values and counter
      INTEGER N
*  Count of CONST values recognised
      INTEGER NFOUND
*  AU to km
      DOUBLE PRECISION AU
*  Earth/Moon mass ratio, M/(E+M), E/(E+M)
      DOUBLE PRECISION EMRAT,RM,RE

*
*  For STATE call:
*
*  Julian Date (not MJD)
      DOUBLE PRECISION DJ(2)
*  Option flags
      INTEGER LIST(12)
*  6-vectors (matching NP/NC meanings given above)
      DOUBLE PRECISION PV(6,13)
*  Nutation:  in longitude, in obliquity, derivatives
      DOUBLE PRECISION ANUT(4)
*  Status
      LOGICAL GOOD


*  Work vector
      DOUBLE PRECISION RES(6)

*  Multipurpose index
      INTEGER I

*  Flag for when final result obtained
      LOGICAL DONE

*  Variables set up first time through
      SAVE JFTF,EMRAT,RM,RE,AU

      DATA JFTF /.TRUE./
      DATA PV /78*0D0/



*  First time through, obtain AU to km factor and Earth/Moon mass ratio
      IF (JFTF) THEN
         JFTF=.FALSE.
         CALL CONST(NAM,VAL,SS,N)
         NFOUND=0
         DO I=1,N
            IF (NAM(I) .EQ. 'AU    ') THEN
               NFOUND=NFOUND+1
               AU=VAL(I)
            ENDIF
            IF (NAM(I) .EQ. 'EMRAT ') THEN
               NFOUND=NFOUND+1
               EMRAT=VAL(I)
            ENDIF
         END DO
         IF (NFOUND.NE.2) GO TO 9000
         RM=1D0/(EMRAT+1D0)
         RE=EMRAT*RM
      END IF

*  Convert MJD to DJ in form required by STATE
      DJ(2)=MOD(TDB,1D0)
      DJ(1)=TDB-DJ(2)
      IF (TDB.LE.2400000.5D0) DJ(1)=DJ(1)+2400000.5D0

*  Reset all the interpolation flags
      DO I=1,12
         LIST(I)=0
      END DO

*  Examine body and origin arguments
      IF ( NP.GE.1 .AND. NP.LE.13 .AND.
     :     NC.GE.1 .AND. NC.LE.13 ) THEN

*
*     Position/velocity
*     -----------------

*     Set all the required interpolation flags
         IF (NP.LE.10) LIST(NP)=2
         IF (NP.EQ.3) LIST(10)=2
         IF (NP.EQ.10.OR.NP.EQ.13) LIST(3)=2
         IF (NC.LE.10) LIST(NC)=2
         IF (NC.EQ.3) LIST(10)=2
         IF (NC.EQ.10.OR.NC.EQ.13) LIST(3)=2

*     Read and interpolate the ephemeris file
         SBARY=BARY
         BARY=.TRUE.
         CALL STATE(DJ,LIST,PV,ANUT,GOOD)
         BARY=SBARY
         IF (.NOT.GOOD) GO TO 9000

*     Move the EMB to SSB vector
         DO I=1,6
            PV(I,13)=PV(I,3)
         END DO

*     Get Sun to SSB vector if required in calculation
         IF (NP.EQ.11.OR.NC.EQ.11) THEN
            DO I=1,6
               PV(I,11)=PVSUN(I,1)
            END DO
         END IF

*     The statements below implement a CASE-type construct which, for
*     any case of
*
*          target = Earth, Moon or EMB and
*          origin = Earth, Moon or EMB
*
*     picks the best method of calculation for each individual case,
*     so as to preserve numerical accuracy, and also calculates Earth
*     or Moon barycentric vectors if needed for other cases.

*     Logical flag DONE is set if any of the 6 Earth, Moon or EMB cases
*     is done, to avoid the main vector subtraction bit afterwards
*     messing up the result.

         DONE=.FALSE.

*     Case for target = EMB and centre = Moon
         IF (NP.EQ.13.AND.NC.EQ.10) THEN
            DO I=1,6
               RES(I)=-PV(I,10)*RE
            END DO
            DONE=.TRUE.

*     Case for target = EMB and centre = Earth
         ELSE IF (NP.EQ.13 .AND. NC.EQ.3) THEN
            DO I=1,6
               RES(I)=PV(I,10)*RM
            END DO
            DONE=.TRUE.

*     Case for target = Moon and centre = EMB
         ELSE IF (NP.EQ.10 .AND. NC.EQ.13) THEN
            DO I=1,6
               RES(I)=PV(I,10)*RE
            END DO
            DONE=.TRUE.

*     Case for target = Earth and centre = Moon
         ELSE IF (NP.EQ.3 .AND. NC.EQ.10) THEN
            DO I=1,6
               RES(I)=-PV(I,10)
            END DO
            DONE=.TRUE.

*     Case for target = Moon and centre = Earth
         ELSE IF (NP.EQ.10 .AND. NC.EQ.3) THEN
            DO I=1,6
               RES(I)=PV(I,10)
            END DO
            DONE=.TRUE.

*     Case for target = Earth and centre = EMB
         ELSE IF (NP.EQ.3 .AND. NC.EQ.13) THEN
            DO I=1,6
               RES(I)=-PV(I,10)*RM
            END DO
            DONE=.TRUE.

*     Case for target or centre = Earth and other point is something
*     other than Moon or EMB, which were dealt with above
         ELSE IF (NP.EQ.3 .OR. NC.EQ.3) THEN
            DO I=1,6
               PV(I,3)=PV(I,13)-PV(I,10)*RM
            END DO

*     Case for target or centre = Moon and other point is something
*     other than Earth or EMB, which were dealt with above
         ELSE IF (NP.EQ.10 .OR. NC.EQ.10) THEN
            DO I=1,6
               PV(I,10)=PV(I,13)+PV(I,10)*RE
            END DO
         END IF

*     Subtract second 6-vector from first
         IF (.NOT.DONE) THEN
            DO I=1,6
               RES(I)=PV(I,NP)-PV(I,NC)
            END DO
         END IF

      ELSE IF (NP.EQ.14 .AND. NC.EQ.0) THEN

*
*     Nutation
*     --------

*     Set interpolation flag
         LIST(11)=2

*     Read and interpolate the ephemeris file
         CALL STATE(DJ,LIST,PV,ANUT,GOOD)
         IF (.NOT.GOOD) GO TO 9000

*     Copy values
         DO I=1,4
            RES(I)=ANUT(I)
         END DO
         DO I=5,6
            RES(I)=0D0
         END DO

      ELSE

*     Illegal NP, NC values
         GO TO 9000

      END IF

*  Return values, set the status to OK, then exit
      DO I=1,6
         R(I)=RES(I)
      END DO
      OK=.TRUE.
      GO TO 9999

*  Errors: set results to zero, set the status to fail, then exit
 9000 CONTINUE
      DO I=1,6
         R(I)=0D0
      END DO
      OK=.FALSE.

*  Exit
 9999 CONTINUE

      END
