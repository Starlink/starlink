      SUBROUTINE SURFLIB_GET_FOCAL_STATION( TELESCOPE, INSTRUMENT,
     :     SUB_INSTRUMENT, FOCAL_STATION, STATUS )
*+
*  Name:
*     SURFLIB_GET_FOCAL_STATION

*  Purpose:
*     Determine where the instrument is located within the focal plane

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_GET_FOCAL_STATION( TELESCOPE, INSTRUMENT,
*    :     SUB_INSTRUMENT, FOCAL_STATION, STATUS )

*  Description:

*  Arguments:
*     TELESCOPE = _CHAR (Given)
*        Name of the telescope.
*     INSTRUMENT = _CHAR (Given)
*        Name of the instrument
*     SUB_INSTRUMENT = _CHAR (Given)
*        Name of the sub-instrument (this is sometimes required
*        to break ambiguous cases where the SCUBA system is used
*        as the data acquisition computer).
*     FOCAL_STATION = _CHAR (Returned)
*        The focal station of the instrument. LNASMYTH, RNASMYTH
*        and CASS.
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Authors:
*     TIMJ: Tim Jenness (JACH)

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.1  2005/03/19 01:41:03  timj
*     Propogate focal station from app level to calc_bol_coords
*

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) TELESCOPE
      CHARACTER*(*) INSTRUMENT
      CHARACTER*(*) SUB_INSTRUMENT

*  Arguments Returned:
      CHARACTER*(*) FOCAL_STATION

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise
      FOCAL_STATION = ' '

*     Check telescope name first
      IF (TELESCOPE .EQ. 'JCMT') THEN

         IF (INSTRUMENT .EQ. 'SCUBA') THEN

*     Just assume things since THUMP is the special case
            IF (SUB_INSTRUMENT .NE. 'THUMP') THEN
               FOCAL_STATION = 'LNASMYTH'
            ELSE
               FOCAL_STATION = 'RNASMYTH'
            END IF

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'INS', INSTRUMENT )
            CALL ERR_REP( ' ', 'GET_FOCAL_STATION: Instrument ^INS '//
     :           'not currently recognized by this program.', STATUS)
         END IF

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TEL', TELESCOPE )
         CALL ERR_REP( ' ', 'GET_FOCAL_STATION: Telescope ^TEL not '//
     :        'currently recognized by this program.',STATUS)
      END IF

      END
