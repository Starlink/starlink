      SUBROUTINE FIND16( DISFIL, ILEVEL, PDATTY, PDISFI,
     :   PNEXTP, POFFIL, PRETMA, MENU, RETMAI, STATUS )
*+
*  Name:
*     FIND16

*  Purpose:
*     IRAS boresight survey data is held in plates, each plate holding
*     all data for one region of the sky. These plates are stored either
*     on UKP tapes for Pass 2 or on Exobyte for Pass 3.
*
*     The main function of the program is to calculate from the
*     position of each source the plate number required. It prepares a
*     list in common of each plate required in subsequent processing,
*     and creates pointers linking each plate to the sources to be
*     obtained from it. For each source it also determines whether the
*     area of source required may fall partly outside the region
*     covered by the plate, and gives details if this is so.
*
*     Part of the plate details consists of the identification number of
*     the tape or exobyte containing the plate and the position of the
*     plate on the tape/exobyte. The user has the option of specifying
*     pass 2 tape, pass 3 exobyte, or I don't know. ( EXCRDD has a mode
*     of operation in which these details can be entered at run time if
*     the user has previously entered "Don't Know" in FINDCRDD.)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND16( DISFIL, ILEVEL, PDATTY, PDISFI,
*     :   PNEXTP, POFFIL, PRETMA, MENU, RETMAI, STATUS )

*  Description:
*
*     IRAS boresight survey data is held in plates, each plate holding
*     all data for one region of the sky. These plates are stored either
*     on UKP tapes for Pass 2 or on Exobyte for Pass 3.
*
*     The main function of the program is to calculate from the
*     position of each source the plate number required. It prepares a
*     list in common of each plate required in subsequent processing,
*     and creates pointers linking each plate to the sources to be
*     obtained from it. For each source it also determines whether the
*     area of source required may fall partly outside the region
*     covered by the plate, and gives details if this is so.
*
*     Part of the plate details consists of the identification number of
*     the tape or exobyte containing the plate and the position of the
*     plate on the tape/exobyte. The user has the option of specifying
*     pass 2 tape, pass 3 exobyte, or I don't know. ( EXCRDD has a mode
*     of operation in which these details can be entered at run time if
*     the user has previously entered "Don't Know" in FINDCRDD.)
*
*  For versions other than IRASFIND 1.1 The program asks the user what
*  type of survey data he is intending to extract. Details of how the
*  pass 3 data is organised onto its Exobytes is not known at the time
*  version 1.1 was written, and therefore this part of the code is
*  commented out.
*
*  For each Source that is not marked for deletion (ie has valid region
*  size and at least one waveband required) the program:-
*
*     Calculates the Dec band in which the source falls
*
*     Separate calculations are carried out for each pole, and for all
*     intermediate Dec bands.
*
*     If the source is not in the polar plates the program calculates
*     the number of plates between the first plate for the Dec band
*     and that containing the source RA.
*
*     Calculates the plate number required and checks whether it has
*     already been entered in the list of plates required in common.
*
*     If plate details are not stored the program stores plate details,
*     including "tape" identification if possible, and boundaries of
*     the plate.
*
*     Sets up the cross linkages between the source record and the plate
*     record.
*
*     Determines whether the area of source required may fall partly
*     outside the region covered by the plate. This is not an accurate
*     check. The region size parameters define a box parallel to the
*     scan direction. We do not know the scan orientation which means
*     we do not know the orientation of the box. We make the worst
*     case assumption in making each boundary check. If the required region
*     could lie outside the plate the program reports the size and
*     position of the overflow.
*
*  If at the end of processing all sources, one or more are off the edge
*  of the plate the user is offered the option of returning to the main
*  menu.

*  Arguments:
*     DISFIL = CHARACTER * ( * ) (Given)
*        Value of the DISPLAYORFILE parameter
*     ILEVEL = INTEGER (Given)
*        Program interaction level
*     PDATTY = CHARACTER * ( * ) (Given)
*        Parameter DATATYPE, 0 = Don't know, 2 = pass 2 UKP tape
*        3 = pass 3  Exobyte
*     PDISFI = CHARACTER * ( * ) (Given)
*        Parameter DISPLAYORFILE, A = ask, B = both, D = display on
*        terminal, F = put to file
*     PNEXTP = CHARACTER * ( * ) (Given)
*        Parameter NEXTPAGE to trigger next page of display
*     POFFIL = CHARACTER * ( * ) (Given)
*        Parameter OFFEDGEFILE, the name of the file in which off edge
*        details are saved.
*     PRETMA = CHARACTER * ( * ) (Given)
*        Prameter RETURNMAIN, does the user want to return to main menu
*     MENU   = CHARACTER * ( 1 ) (Given and Returned)
*        Choice from FINDCRDD main menu
*     RETMAI = LOGICAL (Returned)
*        Set .TRUE. if the user wants to return to the main menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  External Routines Used:
*     FINDCRDD:
*        FIND06, FIND08, FIND45
*     ERR:
*        ERR_ANNUL, ERR_REP
*     MSG:
*        MSG_FMTC, MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_GET0I, PAR_GET0L

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     21-OCT-1991 (DCP):
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
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      CHARACTER * ( * ) DISFIL
      INTEGER ILEVEL
      CHARACTER * ( * ) PDATTY
      CHARACTER * ( * ) PDISFI
      CHARACTER * ( * ) PNEXTP
      CHARACTER * ( * ) POFFIL
      CHARACTER * ( * ) PRETMA

*  Arguments Given and Returned:
      CHARACTER * ( 1 )  MENU

*  Arguments Returned:
      LOGICAL RETMAI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL COCRDE                ! Coefficient of cross scan size used
                                 ! in determining source region Dec size
      PARAMETER ( COCRDE = 1.0 )
      REAL COCRRA                ! Coefficient of cross scan size used
                                 ! in determining source region RA size
      PARAMETER ( COCRRA = 1.0)
      REAL COINDE                ! Coefficient of In scan size used in
                                 ! determining source region Dec size
      PARAMETER ( COINDE = 1.0)
      REAL COINRA                ! Coefficient of In scan size used in
                                 ! determining source region RA size
      PARAMETER ( COINRA = 0.5)
*  These parameters are based on the two worst cases. In calculating
*  the RA and Dec sizes we assume that the scans would be between
*  vertical ( RA size = cross scan size, Dec size = in scan size) and
*  position angle 30deg (or the corresponding angle west of north.
*  Rather than carry out the latter calculation we assume that the RA
*  size is approx 1/2 in scan size, and Dec size is max( cross scan, in
*  scan). To obtain the final sizes we take the max of these two
*  alternatives.
      REAL TWOPI                 ! 2*pi in radians
      PARAMETER ( TWOPI = 6.283185307 )
      REAL PI                    ! pi in radians
      PARAMETER ( PI = 3.141592654 )
      REAL PIBY2                 ! pi/2 in radians
      PARAMETER ( PIBY2 = 1.570796327)

*  Local Variables:
      INTEGER DATATY             ! Type of data from which extraction is
                                 ! to take place, 0 = Don't know,
                                 ! 2 = Pass 2 UKP tape
                                 ! 3 = Pass 3     exobyte
      INTEGER DEC1PL( 37 )       ! Plate number of the first plate in
                                 ! the Dec band
      INTEGER DECBND             ! DEC band in which current source dec
                                 ! lies ( Dec bands are
                                 ! -90.0 to -87.50000000001 = 1 ,
                                 ! -87.5 to -82.50000000001 = 2 ,
                                 ! then every 5 deg until
                                 !  87.5 to 90.0            = 37)
      LOGICAL FIRSTP             ! TRUE if first plate in the Dec band
                                 ! this is not set if the band is either
                                 ! pole.
      REAL DECDEG                ! Dec of the current source in degrees
      REAL DECHIP                ! High Dec boundary value for plate
      REAL DECHIS                ! High Dec value for source area
      REAL DECLOP                ! Low Dec boundary value for plate
      REAL DECLOS                ! Low Dec value for source area
      REAL LAPLCE                ! Plate center of last plate in dec
                                 ! band
      REAL LAPLHW                ! Half width of last plate in dec band
      LOGICAL OFFEDG             ! Set .TRUE. if part of any source
                                 ! region could be off the edge of its
                                 ! plate
      INTEGER PLPOS              ! Position of plate record in plate
                                 ! common
      CHARACTER * ( 1 ) PLTYPE   ! Plate type N = North pole, S = South
                                 ! pole, O = other.
      REAL RAHIP                 ! High RA boundary value for plate
      REAL RAHIS                 ! High RA value for source area
      REAL RAINC( 37 )           ! Gives the RA size of a plate for each
                                 ! Dec band in degrees
      REAL RALOP                 ! Low RA boundary value for plate
      REAL RALOS                 ! Low RA value for source area
      INTEGER REQPLA             ! Plate required for this source
      REAL SODESZ                ! Source Dec size used for off edge
                                 ! determination
      REAL SORASZ                ! Source RA size used for off edge
                                 ! determination
      LOGICAL SOOFEG             ! This particular source is off edge
      INTEGER SOPOS              ! Pointer to the source currently being
                                 ! processed

*  Local Data:
      DATA DEC1PL( 1)/   1/ , RAINC( 1)/360.0/
      DATA DEC1PL( 2)/   2/ , RAINC( 2)/ 36.0/
      DATA DEC1PL( 3)/  12/ , RAINC( 3)/ 22.5/
      DATA DEC1PL( 4)/  28/ , RAINC( 4)/ 16.5/
      DATA DEC1PL( 5)/  50/ , RAINC( 5)/ 13.0/
      DATA DEC1PL( 6)/  78/ , RAINC( 6)/ 11.0/
      DATA DEC1PL( 7)/ 111/ , RAINC( 7)/  9.5/
      DATA DEC1PL( 8)/ 149/ , RAINC( 8)/  8.25/
      DATA DEC1PL( 9)/ 193/ , RAINC( 9)/  7.5/
      DATA DEC1PL(10)/ 241/ , RAINC(10)/  7.0/
      DATA DEC1PL(11)/ 293/ , RAINC(11)/  6.5/
      DATA DEC1PL(12)/ 349/ , RAINC(12)/  6.0/
      DATA DEC1PL(13)/ 409/ , RAINC(13)/  5.75/
      DATA DEC1PL(14)/ 472/ , RAINC(14)/  5.5/
      DATA DEC1PL(15)/ 538/ , RAINC(15)/  5.25/
      DATA DEC1PL(16)/ 607/ , RAINC(16)/  5.0/
      DATA DEC1PL(17)/ 679/ , RAINC(17)/  5.0/
      DATA DEC1PL(18)/ 751/ , RAINC(18)/  5.0/
      DATA DEC1PL(19)/ 823/ , RAINC(19)/  5.0/
      DATA DEC1PL(20)/ 895/ , RAINC(20)/  5.0/
      DATA DEC1PL(21)/ 967/ , RAINC(21)/  5.0/
      DATA DEC1PL(22)/1039/ , RAINC(22)/  5.0/
      DATA DEC1PL(23)/1111/ , RAINC(23)/  5.25/
      DATA DEC1PL(24)/1180/ , RAINC(24)/  5.5/
      DATA DEC1PL(25)/1246/ , RAINC(25)/  5.75/
      DATA DEC1PL(26)/1309/ , RAINC(26)/  6.0/
      DATA DEC1PL(27)/1369/ , RAINC(27)/  6.5/
      DATA DEC1PL(28)/1425/ , RAINC(28)/  7.0/
      DATA DEC1PL(29)/1477/ , RAINC(29)/  7.5/
      DATA DEC1PL(30)/1525/ , RAINC(30)/  8.25/
      DATA DEC1PL(31)/1569/ , RAINC(31)/  9.5/
      DATA DEC1PL(32)/1607/ , RAINC(32)/ 11.0/
      DATA DEC1PL(33)/1640/ , RAINC(33)/ 13.0/
      DATA DEC1PL(34)/1668/ , RAINC(34)/ 16.5/
      DATA DEC1PL(35)/1690/ , RAINC(35)/ 22.5/
      DATA DEC1PL(36)/1706/ , RAINC(36)/ 36.0/
      DATA DEC1PL(37)/1716/ , RAINC(37)/360.0/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the data type to pass 2 crdd until other types become available
      DATATY = 2
* *********************************************************************
* This section is commented out until we recieve pass 3 CRDD and can
* implement the selection of tape identifiers for it
* *********************************************************************
**  Obtain the form of data to be used ie PASS 2 UKP tape, PASS 3
**  Exobyte or Don't Know
* 100  CONTINUE
*      CALL PAR_GET0I( PDATTY, DATATY, STATUS )
*
**  Cancel the parameter so that a new value is obtained next time
**  through this section
*      CALL PAR_CANCL( PDATTY, STATUS )
*
**  Check whether the parameter was abort
*      IF ( STATUS .EQ. PAR__ABORT ) RETURN
*
**  Check whether the parameter was null and if so rerequest
*      IF ( STATUS .EQ. PAR__NULL ) THEN
*         CALL ERR_ANNUL( STATUS )
*         GO TO 100
*      END IF
* *********************************************************************
*

*  Set sources off edge flag to .FALSE.
      OFFEDG = .FALSE.

*  Call FIND08 to clean plate common and associated source pointers
      CALL FIND08( STATUS )

*  Check return status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Print a blank line
      CALL MSG_OUT( ' ', ' ', STATUS )

*  *********************************************************************
*  For each source
*  *********************************************************************

      DO 300 SOPOS = 1, NOFSO

*  Check whether the source is to be deleted flag is set FALSE
*  If the source is to be deleted flag is set TRUE it indicates
*  that the source area or waveband details were incorrect and the
*  source will not be processed further
         IF ( .NOT. SOMADE( SOPOS ) ) THEN

*  *********************************************************************
*     Calculate the plate number for source.
*  *********************************************************************

*  Calculate the DEC band from the source DEC. For clarity the source
*  dec is first translated from radians to degrees and the calculation
*  is carried out in degrees.
            DECDEG = SODEC( SOPOS ) / DEGTOR
            DECBND = NINT( ( 90 + DECDEG ) / 5 ) + 1

*  Set the required plate to the first plate number for this DEC band.
            REQPLA= DEC1PL(DECBND)

*  *********************************************************************
* CALCULATION NOW SPLITS DEPENDING ON WHETHER POSITION IS SOUTH POLE,
* NORTH POLE, OR OTHER DEC BAND.
*  *********************************************************************

*  *********************************************************************
*  Is Dec band south pole ie -90 to -87.5 degrees
*  *********************************************************************

            IF ( DECBND .EQ. 1 ) THEN

*  The plate contains the complete circle of the south pole ie all RA
*  from 0 to 24 hrs

*  Define the boundaries of the area covered by the plate. These are
*  given in degrees and converted to radians.
               DECHIP = -87.5 * DEGTOR
               DECLOP = -90.0 * DEGTOR
               RAHIP  = 359.99999 * DEGTOR
               RALOP  =   0.0 * DEGTOR

*  Call FIND06 to check whether the plate is in plate common and add it
*  if not. The pointers to cross link the source record and the plate
*  record are set up. The subroutine returns the position of the plate
*  record in common as PLPOS.

               CALL FIND06( DATATY, DECHIP, DECLOP, 'S', RAHIP, RALOP,
     :         REQPLA, SOPOS, PLPOS, STATUS )

*  If there is an error on return from subroutine go to near end of
*  subroutine where no more processing will take place
               IF ( STATUS .NE. SAI__OK ) GO TO 400

*  *********************************************************************
*  Calculate the RA and Dec boundaries of the source region using
*  the worst possible assumptions. In scan and Cross scan measurements
*  are measured along and across the scan direction, however we don't
*  know the orientation of the scan to the RA and Dec axes unless we
*  access the data. Therefore we use a general method of calculating
*  RA and Dec sizes. For example the RA size is the max of
*  In_scan_to_RA_coeff * Inscan size, and Cross_scan_to_RA_coeff * Cross
*  scan size. Notes on the coefficient values are given in the local
*  parameters above.
*  *********************************************************************
*  Calculate the RA and Dec source region size, these sizes are based on
*  assumptions of what area would be needed for the extremes of possible
*  orientations of the scans.
               SODESZ = MAX( COINDE * SOINSZ( SOPOS ),
     :                       COCRDE * SOCRSZ( SOPOS ))
               SORASZ = MAX( COINRA * SOINSZ( SOPOS ),
     :                       COCRRA * SOCRSZ( SOPOS ))

*  Calculate the raw source region RA and Dec high and low values
*  (not wrapped over if the source region goes over the pole itsself)
               DECHIS = SODEC( SOPOS ) + SODESZ/2.0
               DECLOS = SODEC( SOPOS ) - SODESZ/2.0
               RAHIS  = SORA( SOPOS ) + SORASZ/2.0
               RALOS  = SORA( SOPOS ) - SORASZ/2.0

*  Check whether the source region covers the pole ie dec low is less
*  than -90 deg (actually less than - pi/2 radians as figures are in
*  radians)
               IF ( DECLOS .LT. ( -90.0 * DEGTOR ) ) THEN

*  Normalise the DECLOS value to the range - pi/2 and upwards
                  DECLOS = - ( PI + DECLOS )

*  If so set the dec high value to the max of the dec high value and the
*  normalised dec low value. Set the dec low to the radian equivalent
*  of -90 deg. Also we need to set the RA high and low to a complete
*  circle as a square over the pole will have data from all RA's.
                  DECHIS = MAX( DECHIS, DECLOS)
                  DECLOS = -90.0 * DEGTOR
                  RAHIS  = 359.99999 * DEGTOR
                  RALOS  =   0.0 * DEGTOR
               ENDIF

*  Normalise RA values to a range 0 to 2pi
               IF ( RAHIS .LT. 0.0   ) RAHIS = RAHIS + TWOPI
               IF ( RAHIS .GT. TWOPI ) RAHIS = RAHIS - TWOPI
               IF ( RALOS .LT. 0.0   ) RALOS = RALOS + TWOPI
               IF ( RALOS .GT. TWOPI ) RALOS = RALOS - TWOPI

*  *********************************************************************
*  See whether requested area will fall outside plate boundaries
*  *********************************************************************

               IF ( DECHIS .GT. DECHIP ) THEN

*  Write a message to say data required for this source may be off edge
                  CALL MSG_FMTC( 'C1', 'A8', SONAME( SOPOS ) )
                  CALL MSG_OUT( ' ', ' Area required for source ^C1,'
     :            //' may pass outside plate boundary', STATUS )
                  CALL MSG_OUT( ' ', ' Actual scan RA values may cover'
     :            //' a different range from that reported here',
     :            STATUS )

*  Call subroutine to display or write file of off edge details
                 CALL FIND45( DECHIS, DECLOS, DISFIL, ILEVEL, PDISFI,
     :           PLPOS, PNEXTP, POFFIL, RAHIS, RALOS, SOPOS, STATUS )

*  If there is an error on return from subroutine go to near end of
*  subroutine where no more processing will take place
                 IF ( STATUS .NE. SAI__OK ) GO TO 400

*  Set off edge flag to .TRUE.
                 OFFEDG = .TRUE.

               ENDIF
*
*  *********************************************************************
*  Is Dec band not a pole ie 87.5 to -87.5 degrees
*  *********************************************************************
            ELSE IF ( ( DECBND .GT. 1 ) .AND. ( DECBND .LT. 37 ) ) THEN

*  *********************************************************************
*    Modify the plate required from the first plate in the dec band to
*  the plate containing the source RA.
*    The width of each plate in RA depends on the dec band. Thus
*  equatorial plates have a width of 5 deg while higher latitudes have
*  wider RA boundries. The width for each dec band is given by
*  RAINC( DECBND ).
*    RA of 0hr is the plate center for the first plate of the band, and
*  therefore sources with RA's close to 24hrs are in the wrap around
*  region, and require the first plate. There is a problem if the size
*  of the plate does not give an integral number of plates covering the
*  full 24hrs. The first and the last plate of the band are deemed to be
*  short and sources are allocated to each depending on which plate
*  centre thay are nearer to.
*  Therfore the program first checks whether the source is within the
*  wrap around region near 24hrs which uses the first plate of the band.
*  It also checks for sources close to 0hrs using the first plate. It
*  then checks for sources using the last plate of the dec band as the
*  boundaries of this last plate may be "short" if the Dec band is not
*  coverd by an integral number of plates.
*
*  *********************************************************************

*  Calculate the plate center of the "last" plate in the Dec band, this
*  is equal to 360deg if there is an integral number of plates covering
*  the dec band.
*  (This is worked out in degrees as RAINC is in degrees)
               LAPLCE = INT( 360.0 / RAINC( DECBND )) * RAINC( DECBND )

*  Check whether this is within a tolerance of 360 deg
               IF ( ( 360.0 - LAPLCE ) .GE. 0.02) THEN

*  If the last plate centre is not 360 deg set the last plate half width
*  to half the difference between the last plate centre and 360 degrees
*  translated to radians
                  LAPLHW = ( 360.0 - LAPLCE) / 2.0
                  LAPLHW = LAPLHW * DEGTOR
               ELSE
*  If the last plate centre is 360 deg set the last plate half width
*  to half the plate width for this dec band, translated to radians
                  LAPLHW = RAINC( DECBND ) / 2.0
                  LAPLHW = LAPLHW * DEGTOR
               END IF

*  First we check whether the source is between 360 deg minus half
*  the plate width and 360 deg. ie is in the wrap around of the first
*  plate. Work out the value of the lower boundary of the first plate
*  (in radians).
               RALOP = 360.0 * DEGTOR
               RALOP = RALOP - LAPLHW

*  Work out the value of the upper boundary of the first plate of the
*  dec band
               RAHIP  = ( RAINC( DECBND ) * DEGTOR ) / 2.0

*  Set first plate flag to TRUE
               FIRSTP = .TRUE.

*  Actually the program checks that the source RA is not in the wrap
*  around region ie Check whether the source RA is less than the
*  lower boundary of the first plate. ( The RA will not be more than
*  2*pi because is normalised to that range)
               IF ( SORA( SOPOS) .LT. RALOP ) THEN

*  If the source is not in the wrap around area of the first plate,
*  check whether that it is not in the rest of the first plate.
                  IF ( SORA( SOPOS) .GE. RAHIP  ) THEN

*  Having established that the source is not in the first plate
*  Set first plate flag to FALSE
                     FIRSTP = .FALSE.

*  We now check whether it is in the last plate of the dec band.
*  Set up the boundaries of the last plate of the dec band.
                     RAHIP = RALOP
                     RALOP = RALOP -
     :                 ( LAPLHW + ( RAINC( DECBND ) * DEGTOR ) / 2.0 )

*  Carry out check whether the source is in the last plate
                     IF ( ( SORA( SOPOS) .GE. RALOP ) .AND.
     :                    ( SORA( SOPOS) .LT. RAHIP ) ) THEN

*  If source is in the last plate calculate the plate required as the
*  first plate in the next dec band minus 1.
                        REQPLA = DEC1PL( DECBND + 1 ) - 1
                     ELSE

*  If the source is not in either the first or the last plate of the dec
*  band.
*  Set the plate boundries to those for the second plate in the dec band
                        RAHIP  = ( RAINC( DECBND ) * DEGTOR ) * 1.5
                        RALOP  = RAHIP - ( RAINC( DECBND ) * DEGTOR )

*  Increment the required plate count by 1 to give the second plate
                        REQPLA = REQPLA + 1

*  Test whether source is in each plate starting with the second and
*  stopping when the correct plate is found.
*  Test source RA against current plate boundaries and increment the
*  plate boundaries and plate required number if the source is not
*  within the plate.
 200                    CONTINUE        ! Start of 'DO WHILE' loop
                        IF ( SORA( SOPOS ) .GE. RAHIP ) THEN
                           RALOP  = RAHIP
                           RAHIP  = RAHIP + ( RAINC( DECBND ) * DEGTOR )
                           REQPLA = REQPLA + 1
                           GO TO 200
                        END IF
                     END IF
                  END IF

*  End if for modify plate required according to the RA of the source
               END IF

*  Define the Dec boundaries of the area covered by the plate. These are
*  given in degrees and converted to radians.
               DECLOP = -87.5 + ( DECBND - 2) * 5
               DECHIP = DECLOP + 5

               DECLOP = DECLOP * DEGTOR
               DECHIP = DECHIP * DEGTOR

*  Call FIND06 to check whether the plate is in plate common and add it
*  if not. The pointers to cross link the source record and the plate
*  record are set up. The subroutine returns the position of the plate
*  record in common as PLPOS.

               CALL FIND06( DATATY, DECHIP, DECLOP, 'O', RAHIP, RALOP,
     :         REQPLA, SOPOS, PLPOS, STATUS )

*  If there is an error on return from subroutine go to near end of
*  subroutine where no more processing will take place
               IF ( STATUS .NE. SAI__OK ) GO TO 400

*  *********************************************************************
*  Calculate the RA and Dec boundaries of the source region using
*  the worst possible assumptions. In scan and Cross scan measurements
*  are measured along and across the scan direction, however we don't
*  know the orientation of the scan to the RA and Dec axes unless we
*  access the data. Therefore we use a general method of calculating
*  RA and Dec sizes. For example the RA size is the max of
*  In_scan_to_RA_coeff * Inscan size, and Cross_scan_to_RA_coeff * Cross
*  scan size. Notes on the coefficient values are given in the local
*  parameters above.
*  *********************************************************************
*  Calculate the RA and Dec source region size, these sizes are based on
*  assumptions of what area would be needed for the extremes of possible
*  orientations of the scans.
               SODESZ = MAX( COINDE * SOINSZ( SOPOS ),
     :                       COCRDE * SOCRSZ( SOPOS ))
               SORASZ = MAX( COINRA * SOINSZ( SOPOS ),
     :                       COCRRA * SOCRSZ( SOPOS ))

*  Calculate the source region RA and Dec high and low values
               DECHIS = SODEC( SOPOS ) + SODESZ/2.0
               DECLOS = SODEC( SOPOS ) - SODESZ/2.0
               RAHIS  = SORA( SOPOS ) + SORASZ/2.0
               RALOS  = SORA( SOPOS ) - SORASZ/2.0

*  Normalise RA values to a range 0 to 2pi
               IF ( RAHIS .LT. 0.0   ) RAHIS = RAHIS + TWOPI
               IF ( RAHIS .GT. TWOPI ) RAHIS = RAHIS - TWOPI
               IF ( RALOS .LT. 0.0   ) RALOS = RALOS + TWOPI
               IF ( RALOS .GT. TWOPI ) RALOS = RALOS - TWOPI

*  *********************************************************************
*  See whether requested area will fall outside plate boundaries
*  *********************************************************************

*  The normal check for high RA values is to compare RA high source with
*  RA high plate and cause an off edge message to be output if the
*  source is greater than the plate. However this simple test can
*  produce a wobbler if the plate concerned is the first plate of a
*  Dec band. In this case the plate high value is a radian value of
*  just greater than 0hrs, while the source RAHIP can be just less than
*  24hrs = 0hrs, this should not produce an off edge message. But the
*  values of RAHIS ( around 0 ) and RAHIP ( around 6 ) would produce an
*  off edge message if this simple test is applied. So the test used
*  first separates the first plate and the other plates. The simple test
*  is used for the other plates. For the first plate the source RA high
*  value is first tested to see whether it is in the region of 0hrs plus
*  (the actual test being whether it is less than a radian, but we expect it
*  to be considerably less than a radian) and only if it is in this region is
*  the original test applied. A similar argument also applies for the
*  low RA of the first plate, where a simple test of source low RA less
*  than plate low RA would produce a false off edge if the source value
*  is just above 0 hrs while the plate value was around 24hrs.

*  Set the off edge flag for this particular source to .FALSE.
*  indicating not off edge.
               SOOFEG = .FALSE.

*  Simple checks of off edge for plates other than the first plate
               IF ( .NOT. FIRSTP ) THEN
                  IF      ( RAHIS  .GT. RAHIP  ) THEN
                     SOOFEG = .TRUE.
                  ELSE IF ( RALOS  .LT. RALOP  ) THEN
                     SOOFEG = .TRUE.
                  ELSE IF ( DECHIS .GT. DECHIP ) THEN
                     SOOFEG = .TRUE.
                  ELSE IF ( DECLOS .LT. DECLOP ) THEN
                     SOOFEG = .TRUE.
                  ENDIF
               ELSE

*  For first plate check first if off edge over the Dec boundaries and
*  if it is bypass further checks.
                  IF ( ( DECHIS .GT. DECHIP ) .OR.
     :                 ( DECLOS .LT. DECLOP ) ) THEN
                     SOOFEG = .TRUE.

*  For first plate check if the source RA high is just greater than 0hrs
*  (Actually in the range 0 to 1 radian) and is greater than the plate
*  RA high - this implies the source is off edge.
                  ELSE IF (  ( RAHIS .LT. 1 ) .AND.
     :                       ( RAHIS .GT. RAHIP ) ) THEN
                     SOOFEG = .TRUE.

*  For first plate check if the source RA low is just less than 24hrs
*  (Actually in the range (2pi - 1) to 2pi radians) and is less than
*  the plate RA low - this implies the source is off edge.
                  ELSE IF (  ( RALOS .GT. ( TWOPI - 1 ) ) .AND.
     :                       ( RALOS .LT. RALOP ) ) THEN
                     SOOFEG = .TRUE.
                  END IF
               ENDIF

*  If the source is off edge
               IF ( SOOFEG ) THEN

*  Write a message to say data required for this source may be off edge
                  CALL MSG_FMTC( 'C1', 'A8',
     :            SONAME( SOPOS ) )
                  CALL MSG_OUT( ' ', ' Area required for source ^C1,'
     :            //' may pass outside plate boundary', STATUS )

*  Call subroutine to display or write file of off edge details
                  CALL FIND45( DECHIS, DECLOS, DISFIL, ILEVEL, PDISFI,
     :            PLPOS, PNEXTP, POFFIL, RAHIS, RALOS, SOPOS, STATUS )

*  If there is an error on return from subroutine go to near end of
*  subroutine where no more processing will take place
                  IF ( STATUS .NE. SAI__OK ) GO TO 400

*  Set off edge flag to .TRUE.
                  OFFEDG = .TRUE.

               ENDIF
*  *********************************************************************
*  Is Dec band north pole ie 87.5 to 90 degrees
*  *********************************************************************

            ELSE IF ( DECBND .EQ. 37 ) THEN

*  The plate contains the complete circle of the south pole ie all RA
*  from 0 to 24 hrs

*  Define the boundaries of the area covered by the plate. These are
*  given in degrees and converted to radians.
               DECHIP =  90.0 * DEGTOR
               DECLOP =  87.5 * DEGTOR
               RAHIP  = 359.99999 * DEGTOR
               RALOP  =   0.0 * DEGTOR

*  Call FIND06 to check whether the plate is in plate common and add it
*  if not. The pointers to cross link the source record and the plate
*  record are set up. The subroutine returns the position of the plate
*  record in common as PLPOS.

               CALL FIND06( DATATY, DECHIP, DECLOP, 'N', RAHIP, RALOP,
     :         REQPLA, SOPOS, PLPOS, STATUS )

*  If there is an error on return from subroutine go to near end of
*  subroutine where no more processing will take place
               IF ( STATUS .NE. SAI__OK ) GO TO 400

*  *********************************************************************
*  Calculate the RA and Dec boundaries of the source region using
*  the worst possible assumptions. In scan and Cross scan measurements
*  are measured along and across the scan direction, however we don't
*  know the orientation of the scan to the RA and Dec axes unless we
*  access the data. Therefore we use a general method of calculating
*  RA and Dec sizes. For example the RA size is the max of
*  In_scan_to_RA_coeff * Inscan size, and Cross_scan_to_RA_coeff * Cross
*  scan size. Notes on the coefficient values are given in the local
*  parameters above.
*  *********************************************************************
*  Calculate the RA and Dec source region size, these sizes are based on
*  assumptions of what area would be needed for the extremes of possible
*  orientations of the scans.
               SODESZ = MAX( COINDE * SOINSZ( SOPOS ),
     :                       COCRDE * SOCRSZ( SOPOS ))
               SORASZ = MAX( COINRA * SOINSZ( SOPOS ),
     :                       COCRRA * SOCRSZ( SOPOS ))

*  Calculate the raw source region RA and Dec high and low values
*  (not wrapped over if the source region goes over the pole itsself)
               DECHIS = SODEC( SOPOS ) + SODESZ/2.0
               DECLOS = SODEC( SOPOS ) - SODESZ/2.0
               RAHIS  = SORA( SOPOS ) + SORASZ/2.0
               RALOS  = SORA( SOPOS ) - SORASZ/2.0

*  Check whether the source region covers the pole ie dec high is more
*  than 90 deg (actually more than pi/2 radians as figures are in
*  radians)
               IF ( DECHIS .GT. ( 90.0 * DEGTOR ) ) THEN

*  Normalise the DECHIS value to the range  pi/2 and downwards
                  DECHIS = ( PI - DECHIS )

*  If so set the dec low value to the min of the dec low value and the
*  normalised dec high value. Set the dec high to the radian equivalent
*  of 90 deg. Also we need to set the RA high and low to a complete
*  circle as a square over the pole will have data from all RA's.
                  DECLOS = MIN( DECHIS, DECLOS)
                  DECHIS =  90.0 * DEGTOR
                  RAHIS  = 359.99999 * DEGTOR
                  RALOS  =   0.0 * DEGTOR
               ENDIF

*  Normalise RA values to a range 0 to 2pi
               IF ( RAHIS .LT. 0.0   ) RAHIS = RAHIS + TWOPI
               IF ( RAHIS .GT. TWOPI ) RAHIS = RAHIS - TWOPI
               IF ( RALOS .LT. 0.0   ) RALOS = RALOS + TWOPI
               IF ( RALOS .GT. TWOPI ) RALOS = RALOS - TWOPI

*  *********************************************************************
*  See whether requested area will fall outside plate boundaries
*  *********************************************************************
               IF ( DECLOS .LT. DECLOP ) THEN

*  Write a message to say data required for this source may be off edge
                  CALL MSG_FMTC( 'C1', 'A8', SONAME( SOPOS ) )
                  CALL MSG_OUT( ' ', ' Area required for source ^C1,'
     :            //' may pass outside plate boundary', STATUS )
                  CALL MSG_OUT( ' ', ' Actual scan RA values may cover'
     :            //' a different range from that reported here',
     :            STATUS )

*  Call subroutine to display or write file of off edge details
                  CALL FIND45( DECHIS, DECLOS, DISFIL, ILEVEL, PDISFI,
     :            PLPOS, PNEXTP, POFFIL, RAHIS, RALOS, SOPOS, STATUS )

*  If there is an error on return from subroutine go to near end of
*  subroutine where no more processing will take place
                  IF ( STATUS .NE. SAI__OK ) GO TO 400

*  Set off edge flag to .TRUE.
                  OFFEDG = .TRUE.

               ENDIF
*  *********************************************************************
*  If Dec band is not found
*  *********************************************************************

            ELSE
*  If the dec band is not within the range of valid dec bands
*  give an error message and set STATUS to SAI__ERROR
               CALL ERR_REP( ' ', 'ERROR in identifying Dec band,'//
     :         ' consult programmer', STATUS )
               STATUS = SAI__ERROR
            ENDIF

*  End if for if source is marked to be deleted and therefore is not
*  processed
         END IF

*  End of loop of for each source
 300  CONTINUE

*  Set return to main menu as false
      RETMAI = .FALSE.

*  Check whether any source was off edge
      IF ( OFFEDG ) THEN

*  And if a source was offedge offer the user the choice of returning to
*  the main menu (he can then use input edit to add a dummy source to
*  catch the rest of his data)
 400  CONTINUE
         CALL PAR_GET0L( PRETMA, RETMAI, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PRETMA, STATUS )

*  Check whether the parameter was abort
         IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the parameter was null and if so rerequest
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            GO TO 400
         END IF

*  If a return to the FINDCRDD main menu is required, the program sets
*  the FINDCRDD main menu choice to 'M'
         IF ( RETMAI ) MENU = 'M'

      END IF

      END
