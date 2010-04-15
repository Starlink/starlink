      SUBROUTINE FIND06( DATATY, DECHIP, DECLOP, PLTYPE, RAHIP, RALOP,
     : REQPLA, SOPOS, PLPOS, STATUS )
*+
*  Name:
*     FIND06

*  Purpose:
*     Checks to see whether a plate is incuded in plate common and if
*     not adds it. Sets up cross linkage between the source record in
*     source common and the plate record in plate common.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND06( DATATY, DECHIP, DECLOP, PLTYPE, RAHIP, RALOP,
*    : REQPLA, SOPOS, PLPOS, STATUS )

*  Description:
*     Checks to see whether a plate is incuded in plate common and if
*     not adds it. Sets up cross linkage between the source record in
*     source common and the plate record in plate common.
*

*  Arguments:
*     DATATY = INTEGER (Given)
*        Type of data from which extraction is to take place,
*        0 = Don't know, 2 = Pass 2 UKP tape, 3 = Pass 3     exobyte
*     DECHIP = REAL (Given)
*        High Dec boundary value for plate
*     DECLOP = REAL (Given)
*        Low Dec boundary value for plate
*     PLTYPE = CHARACTER * ( 1 ) (Given)
*        Plate type  - N = North pole, S = South pole, O = other.
*     RAHIP = REAL (Given)
*        High RA boundary value for plate
*     RALOP = REAL (Given)
*        Low RA boundary value for plate
*     REQPLA = INTEGER (Given)
*        Plate required for this source
*     SOPOS = INTEGER (Given)
*        Pointer to the source currently being processed
*     PLPOS = INTEGER (Returned)
*        Position of plate record in plate common
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     CHR:
*        CHR_ITOC
*     ERR:
*        ERR_REP

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     22-OCT-1991 (DCP):
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
      INTEGER DATATY
      REAL DECHIP
      REAL DECLOP
      CHARACTER * ( 1 ) PLTYPE
      REAL RAHIP
      REAL RALOP
      INTEGER REQPLA
      INTEGER SOPOS

*  Arguments Returned:
      INTEGER PLPOS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NCHAR              ! Number of characters in translation
                                 ! of TAPPOS to character
      INTEGER NEXTSO             ! Next position available for a source
                                 ! pointer in the plate record of the
                                 ! required plate.
      LOGICAL PLFOUN             ! Flag to indicate plate has been found
      CHARACTER * ( 3 ) TAPENO   ! Character version of tape number
      INTEGER TAPPOS             ! Position in list of first plate on
                                 ! tape, this is also the tape number of
                                 ! the tape currently being tested.
      INTEGER UKP1ST( 295 )      ! List of the first plate on each UKP
                                 ! tape ( The last entry 295 does not
                                 ! actually exist but is put in to act
                                 ! as a stop on the processing loop )

*  Local Data:
      DATA UKP1ST / 1,4,7,10,13,16,19,23,26,30,33,35,39,43,47,52,55,57,
     :59,62,67,71,76,81,85,87,89,92,96,101,106,111,116,119,122,125,128,
     : 133,138,144,149,155,159,162,166,170,175,180,186,192,199,203,207,
     : 211,216,222,227,233,239,247,251,255,259,266,272,277,283,289,298,
     : 303,307,311,317,325,331,337,344,353,359,364,369,376,384,390,396,
     : 403,412,419,424,429,436,445,451,457,464,473,481,486,491,497,506,
     : 514,520,527,534,545,551,556,562,570,579,585,591,598,608,617,623,
     : 629,635,645,653,659,666,673,684,691,697,703,711,722,729,735,742,
     : 752,761,767,773,780,790,798,803,810,817,829,836,842,849,858,868,
     : 874,880,887,898,906,912,919,926,937,944,950,956,965,975,981,988,
     : 995,1005,1013,1019,1025,1031,1042,1050,1056,1063,1070,1081,1088,
     : 1093,1099,1107,1117,1123,1130,1137,1146,1155,1160,1165,1171,1181,
     : 1189,1195,1202,1209,1219,1225,1229,1234,1242,1251,1257,1264,1271,
     : 1280,1287,1292,1296,1301,1311,1318,1324,1331,1338,1347,1352,1355,
     : 1360,1369,1376,1382,1389,1395,1403,1408,1411,1415,1423,1430,1436,
     : 1443,1449,1457,1461,1464,1468,1475,1481,1487,1493,1499,1506,1510,
     : 1513,1516,1520,1525,1531,1536,1542,1548,1553,1557,1560,1564,1568,
     : 1573,1578,1584,1589,1593,1596,1598,1601,1605,1610,1615,1621,1626,
     : 1630,1632,1634,1638,1643,1647,1652,1657,1660,1662,1665,1669,1673,
     :1678,1682,1684,1687,1691,1695,1699,1702,1705,1708,1712,1715,1717 /
*.


*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  Check whether the plate is present in the plate list in common
*  *********************************************************************

*  Set plate found flag to .FALSE.
      PLFOUN = .FALSE.

*  Set the position examined to the first record in the plate list
      PLPOS = 1

*  Search through plate list to see if required plate is already
*  included in plate list
 100  CONTINUE             ! Start of 'DO WHILE' loop
      IF ( PLPOS .LE. NOPLAT ) THEN
         IF ( PLNUM( PLPOS ) .EQ. REQPLA  ) THEN
            PLFOUN = .TRUE.
         ELSE
            PLPOS = PLPOS + 1
            GO TO 100
         END IF
      END IF

*  *********************************************************************
*  If the plate has not been found add a new plate at the next position
*  in plate common.
*  *********************************************************************

      IF ( .NOT. PLFOUN ) THEN

*  Set PLPOS to this position
         PLPOS = NOPLAT + 1

*  Add details of the new plate
         PLNUM( PLPOS )  = REQPLA
         PLSUBI( PLPOS ) = ' '
         PLPOLE( PLPOS ) = PLTYPE
         PLLORA( PLPOS ) = RALOP
         PLHIRA( PLPOS ) = RAHIP
         PLLODE( PLPOS ) = DECLOP
         PLHIDE( PLPOS ) = DECHIP
         PLNOSO( PLPOS ) = 0

*  *********************************************************************
*  Add details of the tape number and position of plate file on tape
*  *********************************************************************

*  *********************************************************************
*  For Pass 2 UKP tape use UKP1ST list of first plate on tape
*  *********************************************************************
         IF ( DATATY .EQ. 2 ) THEN

*  Search through the list of first plate on tape to find position where
*  the required plate falls between the first plate on this tape and the
*  first plate on the next
            TAPPOS = 1
            PLID( PLPOS ) = 'UKP000'


 200        CONTINUE               ! Start of 'DO WHILE' loop
            IF ( TAPPOS .LE. 295 ) THEN
               IF (  ( REQPLA .GE. UKP1ST( TAPPOS ) ) .AND.
     :               ( REQPLA .LT. UKP1ST( TAPPOS+1 ) )  ) THEN

*  Tape number has been found. Translate the tape number to character
*  and make up a tape identifier UKP*** where *** is the tape number.
                  CALL CHR_ITOC( TAPPOS, TAPENO, NCHAR )
                  PLID( PLPOS )( 7-NCHAR : 6 ) = TAPENO

*  Calculate the position of the plate on the tape. Note this is
*  not the file number, for a UKP tape each plate has 6 files therefore
*  the number of files to skip before getting to the Boresight of the
*  required plate would be ( PLPOTA - 1 ) * 6  + 1.
                  PLPOTA( PLPOS ) = REQPLA - UKP1ST(TAPPOS) + 1
               ELSE

*  Tape number has not been found increment tape count
                  TAPPOS = TAPPOS + 1
                  GO TO 200
               END IF

            ELSE
*  If the required plate is not within the list of plates on tape
*  give an error message and set STATUS to SAI__ERROR
               CALL ERR_REP( ' ', 'ERROR  in finding tape identifier',
     :         STATUS       )
               STATUS = SAI__ERROR
            END IF
	 END IF

*  *********************************************************************
*  For Pass 3    Exobyte use
*  *********************************************************************
         IF ( DATATY .EQ. 3 ) THEN
*  The structure of the plates on exobyte is not known as yet
*            {executable_statement}...
         END IF

*  *********************************************************************
*  If the data type is unknown ( DATATY = 0 ) no tape id or position on
*  tape is calculated
*  *********************************************************************

*  *********************************************************************
*  For all newly added plates
*  *********************************************************************

*  Update number of plates in plate common list
         NOPLAT = PLPOS

*  End if for end of add new plate to plate common
      END IF

*  *********************************************************************
*  When either the plate has been found in plate common or a new plate
*  has been added, set up pointers to cross reference the plate to the
*  source and vice versa.
*  *********************************************************************

*  Set up pointer to plate in the source common
      SOBPLI( SOPOS ) = PLPOS

*  Set up pointer to source in plate common.
*  First find next empty source slot in plate record.
      NEXTSO = PLNOSO( PLPOS ) + 1

*  Put the source number in that position
      PLSOI( PLPOS,NEXTSO) = SOPOS

*  And update the number of sources in that plate
      PLNOSO( PLPOS ) = NEXTSO

      END
