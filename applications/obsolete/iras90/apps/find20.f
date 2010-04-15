
            SUBROUTINE FIND20( SPFNAM, STATUS )
*+
*  Name:
*     FIND20

*  Purpose:
*  The subroutine tests each observation to see whether it passes
*  within the area required for each source position. If so parameters
*  defining the required subsection of it, ie. scan, are stored for
*  possible extraction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND20( SPFNAM, STATUS )

*  Description:
*  The subroutine tests each observation to see whether it passes
*  within the area required for each source position. If so parameters
*  defining the required subsection of it, ie. scan, are stored for
*  possible extraction.
*
*  The subroutine uses the SPFARCH file which contains details of
*  scan parameters, but these differ sightly from those of the
*  Boresight file. This means that positions are sufficiently
*  accurate for selection but have to be refined in EXCRDD for
*  further processing.
*
*  Arguments:
*     SPFNAM = CHARACTER * ( * ) (Given)
*        Parameter SPFARCHFILE for name of file containing the SDF
*        version of the SPFARCH positions archive catalogue
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*
*  Notes:
*  Section 1
*     -  The problem is to check whether each scan crosses the required
*     area around a source position.
*     -  The positions traversed by a scan are described in satellite
*     angles so we must first calculate the satellite angles of the
*     source.(i.e. The satellite angles that IRAS would have had if it
*     were to have observed the source position). These are relative to
*     the sun's position at the time the SOP was taken. So we have to
*     find out where the Sun was when each observation was taken and
*     calculate the source satellite angles appropriate to each SOP.
*     -  Next we have to find the required area around the source
*     position. We have the users specified size in the crosscan and
*     inscan directions. (Inscan is measured along the scan path,
*     crosscan is the minimum distance from the scan to the source).
*     To this we add an allowance for the size of the focal plane.
*     The SOP/observation data are for the centre of the focal plane,
*     this means that if any detector crosses the users specified area
*     the observation will be selected.
*     -  Now we come to the main problem - Does the scan cross this
*     region? In fact we apply three progressively tighter criteria,
*     and those that cross are those that do not get rejected through
*     all three.
*     -  The first stage rejects a complete SOP if it does not meet the
*     constraints that IRAS should not point too far towards the Sun on
*     either a north_south or a south_north traverse.
*     -  The second stage is a loose comparison of the satellite angles
*     traversed in the observation, with a enlarged source required box.
*     Why is this a loose comparison? The problem is time. The actual
*     positions recorded in the scan are the satellite angles taken at
*     the time the observation was being made, ie relative to the
*     position of the Sun at the time the observation was made.
*     When we do our second stage comparison we calculate the
*     position of the source in 1983.5 ecliptic coords, and use the
*     position of the Sun at the beginning of the scan, to calculate
*     the source satellite angles. A bit of a mess! So when we find
*     that the scan meets this messy criterion we can also calculate
*     approximately when this crossing took place. We then recalculate
*     the source ecliptic coords, the position of the Sun, and from this
*     get a much better value of the source satellite angles. If the
*     scan still crosses the required area we can refine the crossing
*     time and repeat the process. If the scan still passes the
*     criterion on the third repeat it is accepted.
*
*  Section 2.
*  How do you calculate the Source satellite angles?
*     -  It involves using spherical triangles on a triangle whose
*     corners are the source position, the Sun position, and the north
*     ecliptic pole.
*     -  The source position relative to the Sun position is worked out
*     as the ecliptic position of the source minus that of the Sun. It
*     is assumed that the position of the Sun has a negligible solar
*     latitude, and that only the solar longitude needs to be
*     subtracted. In the first approximation the source ecliptic
*     position is taken at epoch 1983.5, and the solar longitude as
*     that of the start of the SOP. In the second and third iterations
*     we use a time calculated from the previous iteration. This
*     crossing time is the approximate time the satellite looked at the
*     source. It is calculated from the scan rate and the approximate
*     source satellite angles???????????. The source ecliptic position
*     is then calculated for this time. The Sun's position is
*     calculated from its position at the start of the scan and the
*     rate of change of its longitude. These figures are then fed into
*     the next iteration of the satellite angle calculation.
*     -  The spherical triangle then looks like this
*
*
*                          North ecliptic pole
*                                / I
*                              / A I
*                         ac /     I
*                          /       I
*                 Source / C       I ab
*                        \         I
*                          \       I
*                         bc \     I
*                              \ B I
*                                \ I
*                                Sun
*      Given
*            A = Solar longitude - Source ecliptic longitude
*           ab = Pi/2
*           ac = Pi/2 - Source ecliptic latitude
*      To be found
*           bc = Theta ( satellite Sun angle )
*            C = Psi ( satellite clock angle )
*
*
*  Section 3.
*  Let us describe in more detail the criteria the scan has to meet to
*  be accepted?
*     -  In the first stage a complete SOP is rejected if, to observe
*     the source, would mean that IRAS would be pointed too close to
*     the Sun on either the North-South or South-North traverse
*     ie IRAS has to be 90 deg from the Sun plus or minus 30 deg.
*     To determine whether this is so the Theta of the source is
*     calculated, using the solar longitude at the beginning of the SOP.
*     This is in radians. It is then tested to see whether it lies
*     between pi/3 and 2pi/3. If it is outside this range it is
*     rejected.
*     -  In the second stage a scan is accepted if it meets two
*     criteria:-
*     First that the Scan's theta is within plus or minus a crosscan
*     range of the Source's theta. This crosscan range is half the user
*     specified cross scan extent, plus 1 deg. The 1 deg is composed of
*     half the width of the focal plane ( 0.27 deg = 0.0047 rads), and
*     an allowance for timing discrepancies ( 1.0 - 0.27 deg).
*     Second that the Source psi (clock angle) falls in the range of
*     clock angle covered by the scan. A scan for which part of the
*     region the user has specified is covered by the scan, but the
*     source or what would be the closest point does not, is rejected.
*     -  In the third stage the criteria are the same as in the second
*     stage except that the crosscan range is tightened by removing the
*     allowance for timing discrepancies.
*
*  Section 4.
*  How do we access the data we need?
*     -  We need three types of data:-
*     -  The acceptance and rejection criteria described above, which
*     are determined partly from known parameters of the mission
*     geometry eg the size of the focal plane, and partly from inscan
*     and crosscan requirements specified by the user and made
*     available through the source common variables.
*     -  The ecliptic coordinates of the source position. These are
*     calculated from the source position specified by the user and
*     made available through the source common block. We also need the
*     time at which the ecliptic coordinates are to be calculated. This
*     is initially 1983.5. In subsequent iterations the crossing time
*     is calculated from data held in SPFARCH for the scan.
*     -  Details of the positions covered by each scan. The details of
*     this are held in the SPFARCH file.
*
*  Section 5.
*  What is the contents of the SPFARCH file?
*     -  The SPFARCH file is an SDF file prepared from the original
*     fortran direct access file. It consists of a set of SOP header
*     details of various types described below, written as vectors. And
*     several blocks containing observation details. These were
*     prepared by blocking together observations from consecutive SOPs
*     until a suitable number for output was reached (NOOBBL=600). As
*     the data on observations for a given SOP could start anywhere in a
*     block, the block number and position within the block for the
*     first observation associated with a SOP is also held in the SOP
*     header. Observation data for a SOP may start in one block and be
*     completed in the next.
*
*     _  The data on positions covered in the IRAS mission is given in
*     the SPFARCH file. The original file was an indexed file
*     containing the following data.
*     i) A header record for each SOP
*     ii) Followed by one Observation data record for each observation
*     in the SOP.
*
*     -  The SOP header record consists of:-
*           Number of observations,
*           A data item that is not used,
*           Time of start of SOP in yrs, days, and secs,
*           SATCAL time at the start of the SOP and the SATCAL rate,
*           Solar longitude at the start of the SOP and Solar longitude
*              rate,
*           Two data items that are not used,
*           Direction of scan given as the sign of psi dot.
*
*     -  The Observation data consists of:-
*           Observation identifier?,
*           Observation sequence number,
*           Satcal time of first measurements in the observation,
*           Satcal time of last measurements in the observation,
*           Satellite angle theta,
*           Uncertainty in satellite angle theta,
*           Satellite angle psi at the begining of the observation,
*           Uncertainty in satellite angle psi,
*           Rate of change of satellite angle psi,
*           Uncertainty in psi rate.

*     -  From this data we have the theta and range of psi covered by
*     each observation. We can derive a crossing time as
*        Crossing time =  (source psi - start of scan psi)
*                               / (scan psi rate)
*     We can use this time to calculate a more accurate solar longitude
*     at time of crossing as
*        Solar longitude = (SOP start solar longitude) +
*                          (Crossing time ) * (the solar longitude rate)
*     And we can calculate the source ecliptic longitude at the crossing
*     time.
*
*     -  In what follows the subsections of a SOP, denoted by different
*     observation numbers are called observations. The name scan is
*     reserved for the subsection of an observation which is to be
*     extracted to cover part of the required region around the source
*     position.

*  External Routines Used:
*     FINDCRDD routines:
*        FIND09, FIND17, FIND18, FIND19, FIND21, FIND32
*     FIO/RIO:
*        FIO_SERR, FIO_UNIT, RIO_OPEN
*     MSG:
*        MSG_FMTC,MSG_FMTI,MSG_OUT

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'FIO_PAR'          ! FIO constants
      INCLUDE 'FIO_ERR'          ! FIO errors
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'DAT_ERR'          ! DAT errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      CHARACTER * ( * ) SPFNAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NSOP               ! Number of SOP headers to be generated
      PARAMETER ( NSOP = 603 )
      INTEGER NOOBBL             ! Number of observations in an
                                 ! observation block
      PARAMETER ( NOOBBL = 600 )
      INTEGER MAXBL              ! Maximum number of observation blocks
      PARAMETER ( MAXBL = 30 )

*  Local Variables:
*  Note :-
*  All source variables in source common are prefixed by SO,
*  All source variables that are local are prefixed by SL,
*  All SOP variables that are local are prefixed by SP,
*  All observation variables that are local are prefixed by OB.
      INTEGER CUBLNO             ! Current observation block being used
      INTEGER CUOBNO             ! Current observation within block
      INTEGER ELS                ! Count of number of elements read in
                                 ! CMP_GET calls.
      INTEGER FD                 ! File descriptor for the SPFARCH file
      INTEGER FINDEX             ! Direct access index used in accessing
                                 ! SPFARCH
      INTEGER ICOUNT             ! Do loop variable for reading the SOP
                                 ! index form SPFARCH file
      INTEGER IOBS               ! Do loop control var for Observation
                                 ! loop
      INTEGER IOSTAT             ! IOSTAT for the file read
      INTEGER ISOP               ! Do loop control variable for SOP loop
                                 ! Takes values 11 to 603 which are the
                                 ! actual SOP numbers.
                                 ! display progress of program
      INTEGER ISOURC             ! Do loop control var for Source loop
      INTEGER J                  ! Controls the reading of the SOP index
                                 ! array
      CHARACTER * ( DAT__SZLOC) LOC0 ! Locator to top level in HDS
      CHARACTER * ( DAT__SZLOC) LOC1 ! Locator observation blocks
      CHARACTER * ( DAT__SZLOC) LOC2 ! Locator to cell for this obs
                                     ! block
      INTEGER NOBLUS             ! Number of observation blocks used in
                                 ! SPFARCH sdf
      INTEGER NOOBUS             ! Number of observations in this
                                 ! observation block in  the SPFARCH sdf
      INTEGER OBBLCT             ! Counter denoting the observation
                                 ! block which is currently read in
      INTEGER OBIRP( NOOBBL )    ! Obs data - not used - irp in POSNTIM
      INTEGER OBNSQ( NOOBBL )    ! Observation Identification number
      REAL OBPS( NOOBBL )        ! Psi at start of observation
      REAL OBPSHI                ! Highest Psi value in observation.
      REAL OBPSLO                ! Lowest Psi value in observation.
      REAL OBPSR( NOOBBL )       ! Psi rate for observation
      REAL OBPSRU( NOOBBL )      ! Uncertainty in Psi rate for obs.
      REAL OBPSU( NOOBBL )       ! Uncertainty in Psi for obs.
      DOUBLE PRECISION OBST1D    ! Satcal time of start of observation
                                 ! double precision version.
      REAL OBST1S( NOOBBL )      ! Satcal time of start of observation
                                 ! single precision version.
      DOUBLE PRECISION OBST2D    ! Satcal time of end of observation
                                 ! double precision version.
      REAL OBST2S( NOOBBL )      ! Satcal time of end of observation
                                 ! single precision version.
      REAL OBTH( NOOBBL )        ! Theta for observation
      REAL OBTHU( NOOBBL )       ! Uncertainty in Theta of obs.
      INTEGER PSINDX             ! Position in the SOP index array
      INTEGER RECLEN             ! Record length fro the SPFARCH file
      REAL SLCRHW                ! Source cross scan half width test
                                 ! size
      REAL SLELA5                ! Source ecliptic latitude (1950)
      REAL SLELAT                ! Source ecliptic latitude at time of
                                 ! observation
      REAL SLELO5                ! Source ecliptic longitude (1950)
      REAL SLELOT                ! Source ecliptic longitude at time of
                                 ! observation
      REAL SLPSI                 ! Source psi in epoch of assumed time
                                 ! of observation
      REAL SLPSI3( MAXSO )       ! Source psi in epoch 1983.5
      REAL SLTH                  ! Source theta in epoch of assumed time
                                 ! of observation
      REAL SLTH3( MAXSO )        ! Source theta in epoch 1983.5
      INTEGER SPINDX( NSOP )     ! SPFARCH index for each SOP
      INTEGER SPITQ( NSOP )      ! SOP data - not used - itq in POSNTIM
      INTEGER SPNOOB( NSOP )     ! Number of observations in SOP
      INTEGER SPOBBL( NSOP )     ! SOP - block in which 1st obs occurs
      INTEGER SPOBCT( NSOP )     ! SOP - Number of 1st obs within block
      DOUBLE PRECISION SPPAFT    ! PAF time for start of SOP
      CHARACTER * ( 8 ) SPRNDT( NSOP ) ! SOP data - not used - rndt in POSNTIM
      CHARACTER * ( 8 ) SPRNTM( NSOP ) ! SOP data - not used - rntm in POSNTIM
      REAL SPSLG( NSOP )         ! Solar longitude at start of SOP
      REAL SPSLGR( NSOP )        ! Solar longitude rate for SOP
      REAL SPSLGT                ! Solar longitude at crossing time
      REAL SPSPD( NSOP )         ! Sign of psi dot for SOP
      DOUBLE PRECISION SPSTCL( NSOP ) ! Satcal time at start of SOP
      REAL SPSTCR( NSOP )        ! Satcal rate for SOP
      INTEGER SPSTD( NSOP )      ! Days part of time of start of SOP
      REAL SPSTS( NSOP )         ! Secs part of time of start of SOP
      INTEGER SPSTY( NSOP )      ! Year part of time of start of SOP
      LOGICAL SPVAL( NSOP )      ! Flag for is SOP valid
      LOGICAL TESTOK             ! .TRUE. if test is passed
      DOUBLE PRECISION TIMCT     ! SATCAL time of crossing time.
                                 ! ie best current estimate of the time
                                 ! of closest approach of the
                                 ! observation to the source.
      REAL TIMYRS                ! Time at which ecliptic coords are
                                 ! calculated in years and decimal years
      REAL THDIFF                ! The absolute value of ( the
                                 ! difference between the theta of the
                                 ! source and the observation theta )
      INTEGER UNIT               ! Unit number allocated to SPFARCH by
                                 ! RIO
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Display message to tell user that this is the start of finding scans
*  and will take a few minutes
      CALL MSG_OUT( ' ', ' ', STATUS )
      CALL MSG_OUT( ' ', ' The program is now finding scans, this will '
     : //'take a couple of minutes', STATUS )

*  Call FIND09 to clean scan common and associated source pointers
      CALL FIND09( STATUS )

* **********************************************************************
*  Get a parameter to associate with the SDF version of the SPFARCH
*  file and read in the SOP records and the first observation data block
* **********************************************************************

*  Get a parameter of the NEWSPFARCH.SDF file and return an HDS locator
*  to it.
      CALL DAT_ASSOC( SPFNAM, 'READ', LOC0, STATUS )

*  For all the components of the SOP header data get the values into
*  corresponding arrays using CMP_GET routines
      CALL CMP_GET1L( LOC0, 'SOP_VALID', NSOP, SPVAL, ELS,
     : STATUS )
      CALL CMP_GET1I( LOC0, 'SOP_NO_OBS', NSOP, SPNOOB, ELS,
     : STATUS )
      CALL CMP_GET1I( LOC0, 'SOP_NOTUSED', NSOP, SPITQ, ELS,
     : STATUS )
      CALL CMP_GET1I( LOC0, 'SOP_STARTYR', NSOP, SPSTY, ELS,
     : STATUS )
      CALL CMP_GET1I( LOC0, 'SOP_STARTDAY', NSOP, SPSTD, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC0, 'SOP_STARTSEC', NSOP, SPSTS, ELS,
     : STATUS )
      CALL CMP_GET1D( LOC0, 'SOP_STARTSATCAL', NSOP, SPSTCL, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC0, 'SOP_SATCALRATE', NSOP, SPSTCR, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC0, 'SOP_STARTSOLONG', NSOP, SPSLG, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC0, 'SOP_SOLLONGRATE', NSOP, SPSLGR, ELS,
     : STATUS )
      CALL CMP_GET1C( LOC0, 'SOP_TNOTUSED', NSOP, SPRNDT, ELS,
     : STATUS )
      CALL CMP_GET1C( LOC0, 'SOP_MNOTUSED', NSOP, SPRNTM, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC0, 'SOP_SIGNPSIDOT', NSOP, SPSPD, ELS,
     : STATUS )
      CALL CMP_GET1I( LOC0, 'SOP_OBSBLOCK', NSOP, SPOBBL, ELS,
     : STATUS )
      CALL CMP_GET1I( LOC0, 'SOP_FIRSTOBNUM', NSOP, SPOBCT ,ELS,
     : STATUS )

*  Get the number of observation blocks used
      CALL CMP_GET0I( LOC0, 'NO_OBS_BLOCK', NOBLUS, STATUS )

*  Get a locator to the observation blocks components
      CALL DAT_FIND( LOC0, 'OBS_BLOCK', LOC1, STATUS )

*  Set the next obs block to be read to 1
      OBBLCT = 1

*  Find the cell in the OBS blocks for this particular block
      CALL DAT_CELL( LOC1, 1, OBBLCT, LOC2, STATUS )

*  And read in the correct number of obsevations for this block
      CALL CMP_GET0I( LOC2, 'OBS_IN_BLOCK', NOOBUS, STATUS )

*  Read all the components of the OBS data into vectors, using CMP
*  package routines
      CALL CMP_GET1I( LOC2, 'OBS_IDNO', NOOBUS, OBNSQ, ELS,
     : STATUS )
      CALL CMP_GET1I( LOC2, 'OBS_NOTUSED', NOOBUS, OBIRP, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC2, 'OBS_STARTSATCAL', NOOBUS, OBST1S, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC2, 'OBS_ENDSATCAL', NOOBUS, OBST2S, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC2, 'OBS_THETA', NOOBUS, OBTH, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC2, 'OBS_THETAUNC', NOOBUS, OBTHU, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC2, 'OBS_STARTPSI', NOOBUS, OBPS, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC2, 'OBS_STPSIUNC', NOOBUS, OBPSU, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC2, 'OBS_PSIRATE', NOOBUS, OBPSR, ELS,
     : STATUS )
      CALL CMP_GET1R( LOC2, 'OBS_PSIRATEUNC', NOOBUS, OBPSRU, ELS,
     : STATUS )

*  Annul locator to OBS cell
      CALL DAT_ANNUL( LOC2, STATUS )

* **********************************************************************
*  Start of for each SOP loop
* **********************************************************************
      DO 400 ISOP = 11, 603

*  Check whether the SOP is valid and has observations
         IF ( SPVAL( ISOP ) .AND. ( SPNOOB( ISOP ) .GT. 0 ) ) THEN

*  Calculate the start of SOP paf time from the start of SOP in days
*  years and secs, using FIND32.
            CALL FIND32( SPSTD( ISOP ), SPSTS( ISOP ), SPSTY( ISOP ),
     :      SPPAFT, STATUS )

* **********************************************************************
*  Start of for each Source loop
* **********************************************************************
            DO 300 ISOURC = 1,  NOFSO

*  Check that the source is not marked for deletion.
               IF ( .NOT. SOMADE( ISOURC ) ) THEN

*  Check that the source still has room for more scans
                  IF ( SONOSC( ISOURC ) .LE. MAXSS ) THEN

* **********************************************************************
* First stage of testing
* **********************************************************************
*
*  Call FIND17 to carry out the first stage of testing.
*  It determines whether the source position is sufficiently far from
*  the position of the Sun at the time the SOP was made, for it
*  to be feasible for IRAS to observe the source at that time.
                     CALL FIND17( SODEC( ISOURC ), SORA( ISOURC ),
     :               SPSLG( ISOP ), SLELA5, SLELAT, SLELO5, SLELOT,
     :               SLPSI3( ISOURC ), SLTH3( ISOURC ), TESTOK, STATUS )

*  Is first stage of testing criterion met?
                     IF ( TESTOK .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Set the current block number and observation number within block
*  to the first observation position for this SOP
                        CUBLNO = SPOBBL( ISOP )
                        CUOBNO = SPOBCT( ISOP )

* *********************************************************************
*  Start of for each Observation loop
* **********************************************************************
                        DO 200 IOBS = 1, SPNOOB( ISOP )

*  Check whether the next observation is in the current observation
*  block
                           IF ( CUBLNO .NE. OBBLCT ) THEN

*  Check whether the required block number is within the number
*  available
                              IF ( CUBLNO .LE. NOBLUS ) THEN

*  Set block number required to that specified by the current block
*  number
                                 OBBLCT = CUBLNO

*  Read in the correct block of observation data for this observation
*  Find the cell in the OBS blocks for this particular block
                                 CALL DAT_CELL( LOC1, 1, OBBLCT, LOC2,
     :                           STATUS )

*  And read in the correct number of obsevations for this block
                                 CALL CMP_GET0I( LOC2, 'OBS_IN_BLOCK',
     :                           NOOBUS, STATUS )

*  Read all the components of the OBS data into vectors, using CMP
*  package routines
                                 CALL CMP_GET1I( LOC2, 'OBS_IDNO',
     :                           NOOBUS, OBNSQ, ELS, STATUS )
                                 CALL CMP_GET1I( LOC2, 'OBS_NOTUSED',
     :                           NOOBUS, OBIRP, ELS, STATUS )
                                 CALL CMP_GET1R( LOC2,'OBS_STARTSATCAL',
     :                           NOOBUS, OBST1S, ELS, STATUS )
                                 CALL CMP_GET1R( LOC2, 'OBS_ENDSATCAL',
     :                           NOOBUS, OBST2S, ELS, STATUS )
                                 CALL CMP_GET1R( LOC2, 'OBS_THETA',
     :                           NOOBUS, OBTH, ELS, STATUS )
                                 CALL CMP_GET1R( LOC2, 'OBS_THETAUNC',
     :                           NOOBUS, OBTHU, ELS, STATUS )
                                 CALL CMP_GET1R( LOC2, 'OBS_STARTPSI',
     :                           NOOBUS, OBPS, ELS, STATUS )
                                 CALL CMP_GET1R( LOC2, 'OBS_STPSIUNC',
     :                           NOOBUS, OBPSU, ELS, STATUS )
                                 CALL CMP_GET1R( LOC2, 'OBS_PSIRATE',
     :                           NOOBUS, OBPSR, ELS, STATUS )
                                 CALL CMP_GET1R( LOC2, 'OBS_PSIRATEUNC',
     :                           NOOBUS, OBPSRU, ELS, STATUS )

*  Annul locator to OBS cell
                                 CALL DAT_ANNUL( LOC2, STATUS )

                              ELSE

*  There is no further data error message
                                 CALL ERR_REP( ' ',
     :                           'SPFARCH file error - contact'//
     :                           ' programmer' , STATUS )
                                 RETURN
                              END IF

*  End if for if incorrect observation block was required and a new one
*  had to be read
                           END IF

*  Create double precision copies of the start and end of observation
*  satcal times
                           OBST1D = DBLE( OBST1S( CUOBNO ) )
                           OBST2D = DBLE( OBST2S( CUOBNO ) )

*  Set the source Psi and Theta in epoch of assumed time of observation
*  as the values calculated for epoch 1983.5
                           SLPSI = SLPSI3( ISOURC )
                           SLTH  = SLTH3( ISOURC )

* **********************************************************************
* Second stage of testing
* **********************************************************************

*  Call FIND18 to carry out the second stage of testing.
*  It calculates the satellite angles IRAS would have had to see the
*  source, and compares them with the actual satellite angle range for
*  the observation under consideration. All these figures are
*  approximate as the source position is that at epoch 1983.5, the
*  solar longitude and the observation satellite angles are at the
*  epoch of the start of the SOP. A wider range of angles are accepted
*  to compensate.
                           CALL FIND18( OBPS( CUOBNO ), OBPSR( CUOBNO ),
     :                     OBST1D, OBST2D, OBTH( CUOBNO ),
     :                     SOCRSZ( ISOURC ), SLPSI, SLTH, TESTOK,
     :                     OBPSHI, OBPSLO, SLCRHW, THDIFF, STATUS )

*  Is second stage of testing criterion met?
                           IF ( TESTOK .AND. ( STATUS .EQ. SAI__OK ) )
     :                        THEN

* **********************************************************************
* Third stage of testing
* **********************************************************************
*
*  Call FIND203 to carry out the third stage of testing.
*  The third stage of testing is a refinement on the second.
*  Again the satellite angles IRAS would have had to see the source are
*  calculated, and compared with the actual satellite angle range for
*  the observation under consideration. But this time both the source
*  position and the solar longitude are evaluated at the epoch of the
*  best estimate of the crossing time. The calculation is repeated
*  through four iterations to refine the crossing time, and hence the
*  positions.
                              CALL FIND19( OBPS( CUOBNO ), OBPSHI,
     :                        OBPSLO, OBPSR( CUOBNO ), OBST1D, OBST2D,
     :                        OBTH( CUOBNO ), SOCRSZ( ISOURC ),SLELA5,
     :                        SLELO5, SPPAFT, SPSLG( ISOP ),
     :                        SPSLGR( ISOP ), SPSPD( ISOP ),
     :                        SPSTCL( ISOP ), SPSTCR( ISOP ),
     :                        SLPSI, SLTH, SLCRHW, SLELAT, SLELOT,
     :                        TESTOK, THDIFF, SPSLGT, TIMCT, TIMYRS,
     :                        STATUS )




*  Is third stage of testing criterion met?
                              IF ( TESTOK .AND.
     :                        ( STATUS .EQ. SAI__OK ) ) THEN

* **********************************************************************
* Observation meets positional requirement, store it
* **********************************************************************
*
*  Call FIND21 to store details of the section of the observation which
*  forms the scan in scan common, and set up cross references between
*  the scan details and the corresponding source details, and vice
*  versa.

                                 CALL FIND21( IOBS, ISOP, ISOURC,
     :                           OBNSQ( CUOBNO ), OBPSR( CUOBNO ),
     :                           OBTH( CUOBNO ), SLPSI, SLTH,
     :                           SPSLGR( ISOP ), SPSLGT, SPSPD( ISOP ),
     :                           SPSTCL( ISOP ), SPSTCR( ISOP ),
     :                           SPSTD( ISOP ), SPSTS( ISOP ), TIMCT,
     :                           STATUS )

*  Check that the return status is ok
	                        IF ( STATUS .NE. SAI__OK ) RETURN

*  End if for third stage criterion
                              END IF

*  End if for second stage criterion
                           END IF

*  Increment the pointer for the current observation to next observation
                           CUOBNO = CUOBNO + 1

*  Check whether there are this number of observations in the current
*  block
                           IF ( CUOBNO .GT. NOOBUS ) THEN

*  Update observation required to the first observation of the next
*  block
                              CUBLNO = CUBLNO + 1
                              CUOBNO = 1
                           END IF

* *********************************************************************
*  End of for each Observation loop
* **********************************************************************
 200                    CONTINUE

*  End if for first stage criterion
                     END IF

*  If there is no more room for scans for this source
                  ELSE

*  Print a message
                     CALL MSG_FMTC( 'C1', 'A8', SONAME( ISOURC ) )
                     CALL MSG_FMTC( 'C2', 'A8',
     :                   PLID( SOBPLI ( ISOURC ) ) )
                     CALL MSG_FMTI( 'I1', 'I3', MAXSS )

                     CALL MSG_OUT( ' ', 'There is no room for more '//
     :               'scans for source ^C1, you can process the '//
     :               'output file for Plate ^C2, but only ^I1 scans '//
     :               'will be produced', STATUS )

                     CALL MSG_OUT( ' ', 'If you want all the scans, '//
     :               'make two sources out of this one, with '//
     :               'narrower extents, and displaced by half the '//
     :               'new extent from each other', STATUS )

*  End if for test that source still has room for more scans
                  END IF

*  End if for test of whether source is not marked for deletion
               END IF

* **********************************************************************
*  End of for each Source loop
* **********************************************************************
 300        CONTINUE

         END IF

* **********************************************************************
*  End of for each SOP loop
* **********************************************************************
 400  CONTINUE

      END
