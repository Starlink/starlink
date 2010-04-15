      SUBROUTINE SCULIB_STANDARD_APPARENT (N, RA_APP, DEC_APP, IN_MJD,
     :  OUT_MJD, STATUS)
*+
*  Name:
*     SCULIB_STANDARD_APPARENT

*  Purpose:
*     convert apparent RA,Decs from one date to another

*  Description:
*     This routine converts a list of apparent RA,Decs for one date into
*     apparent coordinates at another. To do this it calls SLA_MAPPA and
*     SLA_AMPQK to convert the input coordinates into mean coordinates for
*     a J2000.0 equinox, then SLA_MAPPA and SLA_MAPQKZ to convert the mean
*     coordinates into apparent on the output date.

*  Invocation:
*     CALL SCULIB_STANDARD_APPARENT (N, RA_APP, DEC_APP, IN_MJD, OUT_MJD,
*    :  STATUS)

*  Arguments:
*     N                      = INTEGER (Given)
*           number of positions to be converted
*     RA_APP (N)             = DOUBLE PRECISION (Given and returned)
*           apparent RA (radians)
*     DEC_APP (N)            = DOUBLE PRECISION (Given and returned)
*           apparent Dec (radians)
*     IN_MJD                 = DOUBLE PRECISION (Given)
*           date of input coordinates (modified Julian day)
*     OUT_MJD                = DOUBLE PRECISION (Given)
*           date of output coordinates (modified Julian day)
*     STATUS                 = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     29-AUG-1995: original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION IN_MJD
      DOUBLE PRECISION OUT_MJD

*  Arguments Given & Returned:
      DOUBLE PRECISION RA_APP (N)
      DOUBLE PRECISION DEC_APP (N)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION AMPRMS (21)        ! SLA mean-to-apparent parameters
      DOUBLE PRECISION DEC_TEMP           ! scratch Dec
      INTEGER          I                  ! DO loop variable
      DOUBLE PRECISION RA_TEMP            ! scratch RA

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (N .GT. 0) THEN

*  convert apparent place on input date to mean place - J2000.0 equinox

         CALL SLA_MAPPA (2000.0D0, IN_MJD, AMPRMS)

         DO I = 1, N
            CALL SLA_AMPQK (RA_APP(I), DEC_APP(I), AMPRMS, RA_TEMP,
     :        DEC_TEMP)
            RA_APP (I) = RA_TEMP
            DEC_APP (I) = DEC_TEMP
         END DO

*  now convert mean place to apparent place on output date

         CALL SLA_MAPPA (2000.0D0, OUT_MJD, AMPRMS)

         DO I = 1, N
            CALL SLA_MAPQKZ (RA_APP(I), DEC_APP(I), AMPRMS, RA_TEMP,
     :        DEC_TEMP)
            RA_APP (I) = RA_TEMP
            DEC_APP (I) = DEC_TEMP
         END DO

      END IF

      END
