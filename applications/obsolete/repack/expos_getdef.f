*+EXPOS_GETDEF - Gets info from uncorrected file and user
      SUBROUTINE EXPOS_GETDEF(LOC, EXP, STATUS)
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'CONSTANTS.INC'
      INCLUDE 'EXPOS_DEF.INC'
      include 'DAT_PAR'

* Input:
      CHARACTER*(DAT__SZLOC) 	LOC		! Locator to input datafile
* Output:
      RECORD /EXPOS_DEF/     EXP
      INTEGER                STATUS

*    Local variables :
      integer blksz
      integer lunit
      integer fc, lc

      DOUBLE PRECISION		SMJD,EMJD	! Start end times of user file
*
      CHARACTER*(DAT__SZLOC) 	INS 	! Loc to INSTRUMENT
      CHARACTER*(DAT__SZLOC) 	SOR     ! Loc to SORT
      CHARACTER*(DAT__SZLOC) 	HEA	! Loc to HEADER
      CHARACTER*80    re_res			! Path to reserv. files
*
      LOGICAL		BREJD	! Bgnd rejection done?
      LOGICAL		MREJD	! Moon rejection done?

* Modified to produce good slot times P.McGale Sept 92
* P McGale Apr UNIX mods
*-

*   Check status
      IF (STATUS .NE. 0) RETURN

      CALL BDA_LOCINSTR(LOC, INS, STATUS)
      CALL DAT_FIND    (INS, 'SORT', SOR, STATUS)

* Get info from the SORT structure
      CALL CMP_GET0D(SOR, 'HALF_AZ',      EXP.DAZ,      STATUS)
      CALL CMP_GET0D(SOR, 'HALF_EL',      EXP.DEL,      STATUS)
      CALL CMP_GET0D(SOR, 'ROLL',         EXP.ROLL,     STATUS)
      CALL CMP_GET0R(SOR, 'IRIS',         EXP.IRIS,     STATUS)
      CALL CMP_GET0I(SOR, 'FILTER',       EXP.FILT,     STATUS)
      CALL CMP_GET0I(SOR, 'DETECTOR',     EXP.DET, 	STATUS)
      CALL CMP_GET0C(SOR, 'ROOT',         EXP.EVE,	STATUS)
      CALL CMP_GET0L(SOR, 'BGND_REJ',     BREJD,	STATUS)
      CALL CMP_GET0L(SOR, 'MOON_REJ',     MREJD,	STATUS)

      CALL DAT_ANNUL(SOR, STATUS)

* Get info from the HEADER structure
      CALL BDA_LOCHEAD(LOC, HEA, STATUS)
      CALL CMP_GET0D  (HEA, 'AXIS_RA',    EXP.ARA,   STATUS)
      CALL CMP_GET0D  (HEA, 'AXIS_DEC',   EXP.ADEC,  STATUS)
      CALL CMP_GET0D  (HEA, 'FIELD_RA',   EXP.FRA,   STATUS)
      CALL CMP_GET0D  (HEA, 'FIELD_DEC',  EXP.FDEC,  STATUS)
      CALL CMP_GET0R  (HEA, 'OBS_LENGTH', EXP.DUR,   STATUS)
      CALL CMP_GET0I  (HEA, 'BASE_MJD',   EXP.BMJD,  STATUS)
      CALL CMP_GET0D  (HEA, 'BASE_UTC',   EXP.BUTC,  STATUS)

* Correct all values to radians
      EXP.DAZ       = EXP.DAZ*DTOR
      EXP.DEL       = EXP.DEL*DTOR
      EXP.ARA       = EXP.ARA*DTOR
      EXP.ADEC      = EXP.ADEC*DTOR
      EXP.FRA       = EXP.FRA*DTOR
      EXP.FDEC      = EXP.FDEC*DTOR
      EXP.IRIS      = EXP.IRIS*DTOR
      EXP.ROLL      = EXP.ROLL*DTOR

* Work out times of for potential exposure.
      SMJD = DBLE(EXP.BMJD) + (EXP.BUTC/86400.D0)
      EMJD = SMJD + DBLE(EXP.DUR/86400.0)
      call getenv("RECAL", re_res)
      if (re_res .eq. ' ') then
        write(*,*)'   Error in EXPOS_GETDEF'
	write(*,*)'   Can''t get RECAL environment variable.'
	status = 1
	return
      endif
      call chr_fandl(re_res, fc, lc)
      call ftgiou(lunit, status)
      call ftopen(lunit, re_res(fc:lc)//'/re_slots.fit', 0,
     &                                             blksz, status)
      if (status .ne. 0) then
        write(*,*) '   Error in EXPOS_GETDEF - opening S3_SLOTS file'
	return
      endif

      CALL GETSLOTS(lunit,SMJD,EMJD,MREJD,BREJD,EXP.FILT,EXP.SW,
     &                  EXP.EW,EXP.CONF,EXP.NPAIR,STATUS)

      call ftclos(lunit, status)
      call ftfiou(lunit, status)

      IF (STATUS .NE. 0) THEN
	 WRITE(*,*) '   Error in EXPOS_GETDEF'
      END IF

      END

