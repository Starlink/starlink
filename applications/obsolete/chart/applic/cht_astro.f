      SUBROUTINE CHT_ASTRO( STATUS )
*+
*  Name:
*     CHT_ASTRO

*  Purpose:
*     Create a file to give to ASTROM

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_ASTRO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Read the CHART intermediate file and create a formatted file for
*     input to the program ASTROM.

*  Usage:
*      ASTRO

*  ADAM Parameters:
*     none

*  Examples:
*     ASTRO

*  Implementation Deficiencies:
*     -  This routine uses VMS specific Fortran OPEN options.
*        Replace these with calls to FIO.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     PMA: P.M. Allan (Starlink, RAL)
*     KFH: K.F. Hartley (RGO)
*     AJJB: A.J.J. Broderick
*     {enter_new_authors_here}

*  History:
*     14-JAN-1992 (PMA):
*        Original version - based on the KFH INTERIM version.
*        The call to CONST used to be CALL CONST(AO,DO), but variables
*        AO and DO did not exist.
*     12-MAY-1992 (PMA):
*        The call to CONST should indeed be CALL CONST( AO, DO ). The
*        error was a missing common block.
*     25-MAY-1993 (AJJB):
*        Add a status argument in the call to GETASTROM.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     4-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     19-MAR-1993 (AJJB):
*        Replaced OPEN statement, which opens ASTROM.DAT with a call to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the CARRIAGECONTROL specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     17-MAY-1993 (AJJB):
*        Changed STATUS value when file is OPENed (by FILEOPEN) from
*        'NEW' to 'UNKNOWN'.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'MAIN'           ! Main CHART common blocks
*        AO = DOUBLE PRECISION (Read and Write)
*           Field centre RA at equinox 'EQUOUT'
*        DO = DOUBLE PRECISION (Read and Write)
*           Field centre declination at equinox 'EQUOUT'

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) FLDNUM   ! Field number
      CHARACTER * ( 60 ) TEXT    ! Message
      INTEGER PARI               ! Unit number of intermediate file
      INTEGER OUT                ! Unit number of output file
      INTEGER FLD                ! Field number
      INTEGER EOD                ! Flag for end of data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the common block CONVF
      CALL CON_FACTOR( STATUS )

*  Open the intermediate file.
      PARI=4
      OPEN( UNIT=PARI, FILE='chartint.dat', FORM='UNFORMATTED',
     :      STATUS='OLD', ERR=10 )
      GOTO 20

   10 CONTINUE
*  If error, then
*  Report an error trying to open the intermediate file.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASTRO_UNOPFIL',
     :      'Unable to open the intermediate file', STATUS )
         GOTO 900
*  endif
   20 CONTINUE

*  Open a file for the formatted output.
      OUT=7

* This call is equivalent to the statement :
*
* OPEN( UNIT=OUT, FILE='astrom.dat', STATUS='UNKNOWN',
* RECL=80, CARRIAGECONTROL='LIST')
* (see History)

      CALL FILEOPEN( OUT, 'astrom.dat', 'UNKNOWN', ' ', ' ',.TRUE., 80,
     :               .FALSE., STATUS )

      IF (STATUS .NE. SAI__OK) GOTO 900

*  There may be several sets of parameters, corresponding to
*  several fields.
*  So - keep restoring until an error occurs.
      FLD = 1
  100 CONTINUE
         CALL RESTORE( EOD, PARI , STATUS )
         IF ( EOD .GT. 0 ) GOTO 800

*  If this is NOT the first field.........
*  write an "end-of-data" marker.
         IF ( FLD .NE. 1 ) THEN
            WRITE( OUT, '(A)' ) '*   End of section '
            WRITE( OUT, '(A)' ) '/'
         END IF
         WRITE( FLDNUM, '(I3)' ) FLD
         TEXT = 'Field number ' // FLDNUM // ' read in'
         CALL MSG_OUT( ' ', TEXT, STATUS )
         FLD = FLD + 1
         CALL SORT( STATUS )
         CALL CONST( AO, DO , STATUS )
         CALL GETASTROM( OUT, STATUS )
         GO TO 100
  800 CONTINUE

*  Write an "end-of-file" marker.
      WRITE (OUT,'(A)') '*   End of data marker'
      WRITE (OUT,'(A)') 'END '

  900 CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTRO_ERR',
     :   'ASTRO: Error creating file for ASTROM program',
     :   STATUS )
      END IF

      END
