      SUBROUTINE ASK_MAP (MAP_ID, MAP_OWNER_NAME,
     &                    CELL_XSIZE, CELL_YSIZE,
     &                    POS_ANGLE, MSTEP, NSTEP,
     &                    NPTS1,     RA,    DEC,
     &                    IFAIL)

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IFAIL
      CHARACTER MAP_ID*40              ! Map identification string
      CHARACTER MAP_OWNER_NAME*12      ! Name of map 'owner' (reference only)
      REAL      CELL_XSIZE, CELL_YSIZE ! X & Y size of pixels (arcseconds)
      REAL      POS_ANGLE              ! Position angle of map y-axis
      INTEGER   MSTEP,NSTEP            ! X & Y sizes of map (pixels)
      INTEGER   NPTS1                  ! Number of spectral channels
      REAL*8    RA                     ! Map centre RA (same format as scan header)
      REAL*8    DEC                    ! Map centre Dec (")

*     Local variables:

      LOGICAL   AUTO_CENTRE
      REAL      CELL_SIZE(2)           ! X & Y size of pixels (arcseconds)
      INTEGER   NCELLS(2)              ! No of cells on X and Y axes
      INTEGER   I
      INTEGER   ISTAT
      CHARACTER RASTR*20,  DECSTR*20

*  Ok, go...

      IFAIL = 0

*     Get header items from terminal

      CALL GEN_GETSTR ('File title?', ' ', ' ', MAP_ID,         ISTAT)
      CALL GEN_GETSTR ('File owner?', ' ', ' ', MAP_OWNER_NAME, ISTAT)

      AUTO_CENTRE = .TRUE.
      RA          = 0.D0
      DEC         = 0.D0

      CALL GEN_YESNO  ('Set map centre automatically from first '//
     &                 'scan added to map? ',
     &                  AUTO_CENTRE, AUTO_CENTRE, ISTAT)

      IF (.NOT.AUTO_CENTRE) THEN
        CALL READ_DMS (RA,  'R.A. of map centre? (free format)', ISTAT)
        CALL READ_DMS (DEC, 'Dec. of map centre?',               ISTAT)
      END IF
      RA = RA * 15.D0

  101 CALL GEN_GETR4A ('x (R.A.) & y (Dec) cell sizes? (arcsec)',
     &                 CELL_SIZE, 2, 'F5.1,1X,F5.1', CELL_SIZE, ISTAT)
      IF (CELL_SIZE(1).LE.0.0 .OR. CELL_SIZE(2).LE.0.0) THEN
         TYPE *,'Cell dimensions must be positive!'
         GO TO 101
      END IF
      CELL_XSIZE = CELL_SIZE(1)
      CELL_YSIZE = CELL_SIZE(2)

      CALL GEN_GETR4  ('Position angle of map y-axis? (degrees)',
     &                 POS_ANGLE, 'F6.1', POS_ANGLE, ISTAT)

  100 CALL GEN_GETI4A ('Maximum number of cells on x & y axes?',
     &                 NCELLS, 2, 'I3,1X,I3', NCELLS, ISTAT)
      MSTEP = NCELLS(1)
      NSTEP = NCELLS(2)
      IF (2*(MSTEP/2).EQ.MSTEP .OR. 2*(NSTEP/2).EQ.NSTEP) THEN
        TYPE *,'Number of cells must be odd!'
        GO TO 100
      ELSE IF (MSTEP.LE.0 .OR. NSTEP.LE.0) THEN
        TYPE *,'Number of cells must be positive!'
        GO TO 100
      END IF

  102 CALL GEN_GETI4 ('Number of spectral channels in map?',
     &                 NPTS1, ' ', NPTS1, ISTAT)
      IF (NPTS1.LE.0) THEN
        TYPE *, 'Number of channels must be positive!'
        GO TO 102
      END IF

      IF (AUTO_CENTRE) THEN
        TYPE *
        TYPE *,'Map centre (i.e. pos''n corresponding to centre pixel)'
        TYPE *,'will be the map centre of first spectrum ADDed to the'
        TYPE *,'map after it is created. Use ED-S-H on first spectrum'
        TYPE *,'if you want to force some other map centre.'
        TYPE *
      END IF

      RETURN
      END
