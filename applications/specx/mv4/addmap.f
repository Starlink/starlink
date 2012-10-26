*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to new STACKCOMM/PROTOTYPE, no longer use
*        TSYS as start of scan header, since it isn't any more.
*     13 Aug 1994 (hme):
*        Use MV4_PROTWR instead of discarded WRITE_PROT_HEADER.
*        Replace TOMAP with ADDMAP2. Leave writing the index to ADDMAP2.
*     20 July 2000 (ajc):
*        Missing commas in FORMAT
*        Change TYPE * to PRINT *
*        Unused REPLACE, I, NSP, IREP, STATUS, STRING
*        Initialise PUSHED to FALSE
C--------------------------------------------------------------

      SUBROUTINE ADDMAP (NQ, IFAIL)

C   Routine to take the current spectrum, modify it to map format, and
C   add it into the current map. At end of operation the
C   current spectrum is left in the stack in unaltered state

      IMPLICIT  NONE

*     Formal pararameters

      INTEGER   NQ
      INTEGER   IFAIL         ! SPECX error return

*     Include files:

      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

*     Local variables:

      LOGICAL          PUSHED

      INTEGER          IDUP

      REAL             DMISS
      REAL             DELX,     DELY
      REAL             RAOFF,    DECOFF
      REAL             TDRA,     TDDEC
      REAL             XCELL,    YCELL
      REAL             X_OFFSET, Y_OFFSET

      CHARACTER        RASTRING*12, DECSTRING*12

*  Ok, go...

      IFAIL = 0
      PUSHED = .FALSE.

C     Check that a map is open

      IF (.NOT.MAP_OPEN) THEN
        IFAIL = 52
        RETURN
      END IF

C     Extract a quadrant if that is necessary

      IF (NQUAD.NE.1)   THEN
        CALL PUSH
        PUSHED = .TRUE.
        CALL XTRCTQ (NQ,DATA)
        WRITE (6,'('' Quadrant ''I2'' extracted'')')  NQ
      END IF

C     Check that quadrant actually contains some data (to avoid
C     adjustable array dimension error in TOMAP)

      IF (NPTS(1).EQ.0) THEN
        IFAIL = 5
        GO TO 99
      END IF

C     Write spectrum header to map if no existing entries and
C     update map header to say that it now has a header
C     Note: We have to save the offsets IDRA and IDDEC if we want to
C     put zero offsets in the prototype header - then restore them
C     after writing the header to the map file

      IF (IHEAD.EQ.0)   THEN

        TDRA  = DRA
        TDDEC = DDEC
        DRA  = 0
        DDEC = 0

C       How many blocks will be required for each spectrum?
C       Work it out here so that it can be filed as part of the
C       prototype header.

        IST   = ((NPTS(1)-1)/64) + 1

        CALL MV4_PROTWR( )

C       (restore altered values)

        DRA  = TDRA
        DDEC = TDDEC

C       Calculate true map centre (get from scan header if necessary)

        CALL COPY_MAPPOS   (RA, DEC, RAM, DECM, RAM, DECM)

        CALL DEG_TO_STRING (RAM/15.D0,  RASTRING)
        CALL DEG_TO_STRING (DECM,       DECSTRING)
        WRITE  (6,'(''    R.A. ''A11)') RASTRING
        WRITE  (6,'(''    Dec. ''A12)') DECSTRING

        WRITE (6,'('' Current scan header used as prototype for map'')')
        WRITE (6,'(1X,I2,'' blocks of data per mapped spectrum'')') IST

        IHEAD = 1

      END IF

*     Find position offsets of current spectrum and
*     calculate offsets in seconds in the x and y directions

      CALL CALC_POSOFF (RAM, DECM, RA, DEC, DRA, DDEC,
     &                  RAOFF, DECOFF)
      CALL CALC_XYSOFF (RAOFF, DECOFF, POS_ANGLE,
     &                  X_OFFSET, Y_OFFSET, IFAIL)
CD    PRINT *, '(R,D) offsets (arcsec): ', RAOFF,    DECOFF
CD    PRINT *, '(X,Y) offsets (arcsec): ', X_OFFSET, Y_OFFSET
      IF (IFAIL.NE.0) GO TO 99

*     Find coordinates of nearest cell to data point

      CALL CALC_XYCOFF (X_OFFSET, Y_OFFSET, CELL_XSIZE, CELL_YSIZE,
     &                  MSTEP, NSTEP, XCELL, YCELL, IFAIL)
CD    PRINT *, 'Nearest cell to data point: ', XCELL, YCELL
      IF (IFAIL.NE.0) GO TO 99

*     Check that scan is "close enough" to a data pixel centre. DMISS
*     is the amount by which the spectrum "misses" the centre of the
*     nearest pixel (measured in pixels).

*     Evaluate actual coordinates of the pixel centre

      DELX   = X_OFFSET - XCELL*CELL_XSIZE
      DELY   = Y_OFFSET - YCELL*CELL_YSIZE
      DMISS  = SQRT ((DELX/CELL_XSIZE)**2 + (DELY/CELL_YSIZE)**2)

CD    PRINT *, 'DELX, DELY, DMISS  = ', DELX, DELY, DMISS

      IF (DMISS .GT. MAP_TOL) THEN
        IFAIL = 70
        GO TO 99
      END IF

*     Set parameter to say whether OK to replace an existing spectrum
*     (if not set, and a spectrum already exists at that position, TOMAP
*     will return with an error)

      IF (REPLACE_MAP_DATA) THEN
        IDUP = 1
      ELSE
        IDUP = 0
      END IF

*     Now write the spectrum to the map at the right place

      CALL ADDMAP2 (IDUP, XCELL, YCELL,
     &            DATA,  NPTS(1), %VAL(CNF_PVAL(INDEX_ADDRESS)),
     &            IFAIL)
      IF (IFAIL.NE.0) GO TO 99

*     Output SUCCESS messages

      WRITE (6,1025) NINT (XCELL), NINT (YCELL)
      WRITE (6,1030) DELX,         DELY

*     TOMAP has rewritten map header and updated index array;
*     Rewrite INDEX array and output message to terminal

*     CALL WRITE_MAP_INDEX (MCHAN, %VAL(CNF_PVAL(INDEX_ADDRESS)),NREDT)

*     Finish and error return

   99 IF (PUSHED) CALL POP

*     Mark rotated and/or interpolated data cube as empty so it
*     will be remade before it is used again.

      CALL RELEASE_NEW_CUBE
      RETURN

 1025 FORMAT (' Spectrum placed in cell (',I5,',',I5,') ')
 1030 FORMAT ('  -- position is ', F5.1, ' arcsec in X and ',
     &          F5.1, ' arcsec  in Y from pixel centre')

      END
