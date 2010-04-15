      SUBROUTINE CATREC( UNIT, NAV, STATUS )
*+
*  Name:
*     CATREC

*  Purpose:
*     Copy data from a catalogue into internal storage

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CATREC( UNIT, NAV, STATUS )

*  Description:
*     This subroutine reads a record from the star catalogue. The info
*     from the catalogue is put in a common block.

*  Arguments:
*     UNIT = INTEGER (Given)
*        The I/O unit number.
*     NAV = INTEGER (Given)
*        The record number in the catalogue to be read.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*   Notes:
*      -  Catalogue File 'Astrometcats' is as follows:
*
*         It is divided into 180 zones,each
*         covering 1 degree of Dec, in north polar distance order
*         the first 180 records each consist of 2 integers:
*         NAVSTART,NUMBER, where NAVSTART is the starting
*         associated variable for that zone and NUMBER the
*         number of stars in the zone.
*
*         The main part of the catalogue has 1 record per star
*         in all approx. 340000 records of 22 bytes each
*
*      Type     Data   Description
*      ----     ----   -----------
*      INTEGER  NCAT = Catalogue number & name :
*               1000000 * cat. no. + star number
*      BYTE     MAG  = Magnitude in units of 0.1 mag.
*      CHAR*2   SPEC = Spectral type
*      INTEGER  IPA  = Annual p.m. in ra.  units 0.00001s.
*      INTEGER  IPD  = Annual p.m. in dec.  units 0.0001".
*      INTEGER  IRA  = Right ascension in units of 0.001s.
*      INTEGER  IDEC = Declination in units of 0.01"
*
*      Catalogue CSI (catalogue of stellar identifications)
*      is as follows:
*
*      Index records as above except 2nd number is the a.v.
*      of the last star in the zone
*
*      Star records each 36 bytes long:  434927 records in all
*
*      Type   Data   Description
*      ----   ----   -----------
*      INTEGER     NHD  = 10 * Henry Draper number + supplementary no.
*                         (usually 0)
*      CHAR*2      SPEC = Spectral type, usually from H.D.
*      INTEGER*2   MB   = Photographic magnitude
*      INTEGER*2   MV   = Visual magnitude (both mb, mv homogenized
*                         values,in 1/10ths. mag.)
*      INTEGER     IRA  = Right ascension in units of 0.001s.
*      INTEGER     IDEC = Declination in units of 0.01"
*      CHARACTER*2 DM   = Which durchmusterung (bd,cd,cp or **(non dm) )
*      CHARACTER*1 DMSIGN = DM zone sign
*      INTEGER*2   DMZ  = DM zone number(unsigned)
*      INTEGER*2   DMN  = Number in dm zone
*      LOGICAL*1   DMSUPP = Number (1-20) denoting multiple DM stars
*      LOGICAL*1   IDLIST(8) = 8*idbyte bit supplementary identification
*                           flags 434,927 records of length 36 bytes
*
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     UNK: Someone (Somewhere)
*     {enter_new_authors_here}

*  History:
*     Sometime (UNK):
*        Original version.
*     16-NOV-1992 (PMA):
*        Tidied up code.
*        Added STATUS argument.
*     25-JAN-1993 (PMA):
*        Converted prologue to ADAM style.
*        Moved common blocks out to INCLUDE files.
*     26-FEB-1993 (PMA):
*        Removed the use of DCV_BTOI in converting LMAg to MAG. Just use
*        an assignment statement.
*     8-MAR-1993 (AJJB):
*        Assigned DMSUPP value to IDMSUP after it's read in, which is an
*        INTEGER variable, and used IDMSUP in place of DMSUPP in the
*        code so that we're working with an integer rather LOGICAL*1
*        type in calculations.
*     22-MAR-1993 (AJJB):
*        Commented out declarations of unused local variables, to keep
*        Sun complier quiet.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER UNIT
      INTEGER NAV

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'MAIN'             ! CHART main common block
*        CATRUN = LOGICAL (Read)
*           If 'TRUE' then the Astrometric catalogue is being read;
*           otherwise reading the CSI catalogue.
*        NONS = LOGICAL (Read)
*           If 'TRUE' and CATRUN 'FALSE', then the non-stellar objects
*           catalogue is being read.

      INCLUDE 'CATINF_CMN'       ! [global_variables_description]
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'SPT_CMN'          ! [global_variables_description]
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'CONVF'          ! [global_variables_description]
*        {descriptions_of_global_variables_referenced}...


*  Local Variables:
      BYTE DMSUPP                ! Replaced by IDMSUP (see history)
      BYTE LMAG                  ! [local_variable_description]
*     LOGICAL*1 ISP              ! [local_variable_description]
*     INTEGER*2 NGC              ! [local_variable_description]
      INTEGER*2 DMN              ! [local_variable_description]
      INTEGER*2 MB               ! [local_variable_description]
      INTEGER*2 MV               ! [local_variable_description]
      INTEGER*2 DMZ              ! [local_variable_description]
      INTEGER*2 I2MAG            ! [local_variable_description]
      INTEGER*2 LDIAM            ! [local_variable_description]
      INTEGER IDMSUP             ! WOrking storage for 1 byte DMSUPP
      INTEGER MAG                ! [local_variable_description]
*     INTEGER NAGK               ! [local_variable_description]
*     INTEGER NSAO               ! [local_variable_description]
*     INTEGER NYZ                ! [local_variable_description]
*     INTEGER NEM                ! [local_variable_description]
*     INTEGER ISPT               ! [local_variable_description]
      REAL SRA                   ! [local_variable_description]
      REAL SDEC                  ! [local_variable_description]
      CHARACTER * ( 1 ) DMSIGN   ! [local_variable_description]
      CHARACTER * ( 2 ) DM       ! [local_variable_description]
      CHARACTER * ( 2 ) DMC( 4 ) ! [local_variable_description]
      INTEGER IRA                ! [local_variable_description]
      INTEGER IDEC               ! [local_variable_description]
      INTEGER IDMZ               ! [local_variable_description]
      INTEGER IDMN               ! [local_variable_description]
      INTEGER J                  ! Loop counter

*  Local Data:
      DATA DMC / 'BD', 'CD', 'CP', '**' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( CATRUN ) THEN
         READ( UNIT, REC=NAV ) NCAT, LMAG, SPEC, IPA, IPD, IRA, IDEC
         ICAT = NCAT / 1000000
         NCAT = MOD( NCAT, 1000000 )
         MAG  = LMAG
         IEPS = 5000
         RA   = FLOAT( IRA ) / 1000.0 * RDST
         DEC  = FLOAT( IDEC ) / 100.0 * RDSA
         IF ( ICAT .EQ. 1 ) THEN
            MAGP = MAG
            MAGV = 9999
         ELSE
            MAGV = MAG
            MAGP = 9999
         ENDIF
      ELSE IF ( .NOT. NONS ) THEN
         READ( UNIT, REC=NAV ) NHD, SPEC, MB, MV, IRA, IDEC, DM,
     :      DMSIGN, DMZ, DMN, DMSUPP, IDLIST
         IDMSUP = DMSUPP         ! Convert value to INT form to work with
         RA = FLOAT( IRA ) / 1000.0 * RDST
         DEC = FLOAT( IDEC ) / 100.0 * RDSA
         IDMZ = DMZ
         IDMN = DMN
         DO J = 1, 4
            IF ( DM .EQ. DMC( J ) ) GOTO 150
         END DO
  150    CONTINUE

*  Code to correct possible bug in value of DMSUPP reported by
*  A.N.Argue and corrected by K.F.Hartley at RGO on 28-8-85
         IF ( IDMSUP .GE. 40 ) THEN
            IDMSUP = IDMSUP - 40
         ELSE IF ( IDMSUP .GE. 20 ) THEN
            IDMSUP = IDMSUP - 20
         END IF
         DMNO = J * 100000000 + IDMZ * 1000000 + IDMN * 20 + IDMSUP
         IF ( DMSIGN .EQ. '-' ) DMNO = -DMNO
         IF ( MV .NE. 0 ) THEN
            MAGV = MV
         ELSEIF ( NHD .EQ. 1721670 .OR. NHD .EQ. 1248970 ) THEN
            MAGV = MV
         ELSE
            MAGV = 9999
         ENDIF
         IF ( MB .NE. 0 ) THEN
            MAGP = MB
         ELSE IF ( NHD .EQ. 1721670 ) THEN
            MAGP = MB
         ELSE
            MAGP = 9999
         ENDIF
      ELSE
         READ( UNIT, REC=NAV ) NAM, SRA, SDEC, LDIAM, I2MAG, DES
         MAGV = I2MAG
         DIAMET = LDIAM
         RA = DBLE( SRA )
         DEC = DBLE( SDEC )
      ENDIF
700   CONTINUE
      END
