      SUBROUTINE RDDISP( FD, STATUS )
*+
*  Name:
*     SUBROUTINE RDDISP

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDDISP( FD, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        Fortran I/O unit of open file to be read from.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The CMDISP contents are read.
*     No resolution discrimination is performed.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version
*     06-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     04-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     29-MAR-95 (MJC):
*       IUEDR Vn. 3.2
*       Support for new-style dispersion data files.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISP'
      INCLUDE 'CMWCOR'
      INCLUDE 'CMECOR'
      INCLUDE 'CMVEL'

*  Local Constants:
      INTEGER LAP            ! LAP index.
      INTEGER MAXAPER        ! Maximum number of apertures.
      INTEGER MAXLABEL       ! Maximum length of label string.
      INTEGER MAXNAME        ! Maximum length of name string.
      INTEGER OK             ! OK status.
      INTEGER SAP            ! SAP index.
      PARAMETER ( LAP = 2, MAXAPER = 2, MAXLABEL = 40,
     :            MAXNAME = 16, OK = 0, SAP = 1 )

*  Arguments Given:
      INTEGER FD             ! File descriptor.

*  Status:
      INTEGER STATUS         ! Global status.

*  External References:
      LOGICAL STR_SIMLR      ! Caseless string equality.

*  Local variables:
      REAL*8 DDS( MAXAPER )  ! Temporary DS values.
      REAL*8 DDL( MAXAPER )  ! Temporary DL values.

      CHARACTER*( MAXNAME ) CTYPE ! F77 type string.
      CHARACTER*( MAXLABEL ) CID  ! F77 id string.

      INTEGER I              ! Loop index.
      INTEGER IAPER          ! Aperture index.
      INTEGER NAPR           ! Loop index.
      INTEGER NCHAR          ! Character count.
      INTEGER ND             ! Loop index.
      INTEGER ISTAT          ! Local status.
*.

      NODISP = .TRUE.
      READ ( FD, *, IOSTAT = ISTAT ) CTYPE, CID
      IF ( ISTAT .NE. 0 ) THEN
         CALL ERROUT( 'Error: reading dispersion data\\', STATUS )
         GO TO 999
      END IF

      CALL GEN_CTOS( CTYPE, MAXNAME, DISPTP, NCHAR )
      CALL STR_RMBLK( DISPTP )
      CALL GEN_CTOS( CID, MAXLABEL, DISPID, NCHAR )
      CALL STR_RMBLK( DISPID )

*  Get expected number of dispersion constants.
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         NDISP = 7

      ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         NDISP = 2

      ELSE
         CALL ERRSTR( 'Error: \\' )
         CALL ERRSTR( RESOL )
         CALL ERROUT( ' is invalid dispersion type\\' , STATUS )
         GO TO 999
      END IF

*  RIPCON.
      IF ( NDISP .EQ. 7 ) THEN
         READ ( FD, *, IOSTAT = ISTAT ) RIPCON
         IF ( ISTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading dispersion data\\', STATUS )
            GO TO 999
         END IF
      END IF

*  Check dispersion description type.
      IF ( STR_SIMLR( 'IUE_DISPN\\', DISPTP ) ) THEN

*     NDISP, DISPS, DISPL.
         READ ( FD, *, IOSTAT = ISTAT ) ND,
     :          ( DISPS( I ), I = 1, MIN( ND, NDISP ) ),
     :          ( DISPL( I ), I = 1, MIN( ND, NDISP ) )
         IF ( ISTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading dispersion data\\', STATUS )
            GO TO 999

         ELSE IF ( ND .NE. NDISP ) THEN
            CALL ERROUT( 'Error: in dispersion constants\\', STATUS )
            GO TO 999
         END IF

*     DISPT0, DISPD0, DISPWS, DISPWL.
         READ ( FD, *, IOSTAT = ISTAT ) DISPT0, DISPD0,
     :          DISPWS1, DISPWS2, DISPWS3, DISPWS4,
     :          DISPWL1, DISPWL2, DISPWL3, DISPWL4
         IF ( ISTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading dispersion data\\', STATUS )
            GO TO 999
         END IF

      ELSE

*     NDISP, DISPS, DISPL, DISPT0, DISPD0, DISPST, DISPSD, DISPLT, DISPLD.
         READ ( FD, *, IOSTAT = ISTAT ) ND,
     :          ( DISPS( I ), I = 1, MIN( ND, NDISP ) ),
     :          ( DISPL( I ), I = 1, MIN( ND, NDISP ) ),
     :          DISPT0, DISPD0, DISPST, DISPSD, DISPLT, DISPLD
         IF ( ISTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading dispersion data\\', STATUS )
            GO TO 999

         ELSE IF ( ND .NE. NDISP ) THEN
            CALL ERROUT( 'Error: in dispersion constants\\', STATUS )
            GO TO 999
         END IF
      END IF

*  NAPR, DDS, DDL.
      READ ( FD, *, IOSTAT = ISTAT )
     :       NAPR, ( DDS( I ), DDL( I ), I = 1, MIN( NAPR, MAXAPER ) )
      IF ( ISTAT .NE. 0 ) THEN
         CALL ERROUT( 'Error: reading dispersion data\\', STATUS )
         GO TO 999
      END IF

*  Assign DDS, DDL to apertures.
      IF ( NAPER .GT. 0 ) THEN
         DO I = 1, NAPER
            DISPDS( I ) = 0.0
            DISPDL( I ) = 0.0
            DISPSG( I ) = 0.0
            DISPLG( I ) = 0.0
         END DO
      END IF

      DO I = 1, NAPR
         IF ( I .EQ. SAP ) THEN

            CALL FNAPER( 'SAP\\', IAPER )

         ELSE IF ( I .EQ. LAP ) THEN
            CALL FNAPER( 'LAP\\', IAPER )

         ELSE
            IAPER = 0
         END IF

         IF ( IAPER .GT. 0 ) THEN
            DISPDS( IAPER ) = DDS( I )
            DISPDL( IAPER ) = DDL( I )
            DISPSG( I ) = 0.0
            DISPLG( I ) = 0.0
         END IF
      END DO

      NODISP = .FALSE.

 999  CONTINUE

      END
