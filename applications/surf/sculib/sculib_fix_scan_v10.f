      SUBROUTINE SCULIB_FIX_SCAN_V10(CENTRE_COORDS, LAT_OBS, LONG, LAT,
     :     MJD, RA_OFF_START, DEC_OFF_START, RA_OFF_END,
     :     DEC_OFF_END, RA_NEW_START, DEC_NEW_START,
     :     RA_NEW_END, DEC_NEW_END, STATUS)
*+
*  Name:
*     SCULIB_FIX_SCAN_V10

*  Purpose:
*     Correct scan positions for version 1.0 data from SCUCD

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_FIX_SCAN_V10(CENTRE_COORDS, LAT_OBS, LONG, LAT,
*    :     MJD, RA_OFF_START, DEC_OFF_START, RA_OFF_END,
*    :     DEC_OFF_END, RA_NEW_START, DEC_NEW_START,
*    :     RA_NEW_END, DEC_NEW_END, STATUS)

*  Description:
*     For data taken with version 1.0 of SCUCD the positions of
*     the scan ends were incorrect. This was because of a bug
*     in the software that calculated the scan ends concerning
*     the use of arcseconds instead of radians.
*     This routine attempts to recreate the bug so that the
*     correct scan offsets can be calculated.

*  Arguments:
*     CENTRE_COORDS = CHAR (Given)
*        The coordinate system of the map centre
*     LAT_OBS                = DOUBLE PRECISION (Given)
*        Latitude of observatory (radians)
*     LONG = DOUBLE (Given)
*        The longitude (ra) of the map centre (radians)
*     LAT = DOUBLE (Given)
*        The latitude (dec) of the map centre (radians)
*     MJD = DOUBLE (Given)
*        The modified Julian date at which the data was taken
*     RA_OFF_START = DOUBLE (Given)
*        The RA start position of the scan as stored in the header (RD)
*     DEC_OFF_START = DOUBLE (Given)
*        The Dec start position of the scan as stored in the header (RD)
*     RA_OFF_END = DOUBLE (Given)
*        The RA end position of the scan as stored in the header (RD)
*     DEC_OFF_END = DOUBLE (Given)
*        The DEC end position of the scan as stored in the header (RD)
*     RA_NEW_START = DOUBLE (Returned)
*        The corrected RA start position of the scan  (RD)
*     DEC_NEW_START = DOUBLE (Returned)
*        The corrected Dec start position of the scan  (RD)
*     RA_NEW_END = DOUBLE (Returned)
*        The corrected RA end position of the scan  (RD)
*     RA_NEW_END = DOUBLE (Returned)
*        The corrected Dec end position of the scan  (RD)
*     STATUS = INTEGER (Given & Returned)

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T.Jenness (timj@jach.hawaii.edu)

*  Notes:
*     The scan ends are all stored as apparent RA/Decs.


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.3  1999/08/03 19:35:00  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.2  1999/07/14 20:13:27  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.1  1998/06/17 07:44:00  timj
*     Initial revision
*

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER *(*) CENTRE_COORDS
      DOUBLE PRECISION LONG
      DOUBLE PRECISION LAT
      DOUBLE PRECISION LAT_OBS
      DOUBLE PRECISION MJD
      DOUBLE PRECISION RA_OFF_START
      DOUBLE PRECISION DEC_OFF_START
      DOUBLE PRECISION RA_OFF_END
      DOUBLE PRECISION DEC_OFF_END

*  Arguments Returned:
      DOUBLE PRECISION RA_NEW_START
      DOUBLE PRECISION RA_NEW_END
      DOUBLE PRECISION DEC_NEW_START
      DOUBLE PRECISION DEC_NEW_END

*  Global Status:
      INTEGER STATUS

*  Local Constants:
      DOUBLE PRECISION RAD2SEC
      PARAMETER (RAD2SEC = 206264.806249)


*  Local Variables:
      DOUBLE PRECISION D_DEC_OFF     !
      DOUBLE PRECISION D_RA_OFF      !
      DOUBLE PRECISION DEC_END       !
      DOUBLE PRECISION DEC_OBJ       ! Apparent Dec of map centre
      DOUBLE PRECISION DEC_RJ_OBJ    !
      DOUBLE PRECISION DEC_RJ_TEMP   !
      DOUBLE PRECISION DEC_RJ_OFF_END!
      DOUBLE PRECISION DEC_RJ_OFF_START !
      DOUBLE PRECISION DEC_TEMP      !
      INTEGER          ITEMP         !
      REAL             LENGTH        !
      REAL             LENGTH2       !
      REAL             LENGTH3       !
      DOUBLE PRECISION OUT_ROTATION  !
      DOUBLE PRECISION RA_END        !
      DOUBLE PRECISION RA_OBJ        ! Apparent RA of map centre
      DOUBLE PRECISION RA_RJ_OBJ     !
      DOUBLE PRECISION RA_RJ_TEMP    !
      DOUBLE PRECISION RA_RJ_OFF_END !
      DOUBLE PRECISION RA_RJ_OFF_START !
      DOUBLE PRECISION RA_TEMP       !
      REAL             RTEMP         ! Scratch real
      DOUBLE PRECISION XOFF_END      !
      DOUBLE PRECISION XOFF_START    !
      DOUBLE PRECISION YOFF_END      !
      DOUBLE PRECISION YOFF_START    !



*.

      IF (STATUS .NE. SAI__OK) RETURN

*     The bug was only in the RJ/RB/GA system.
*     RD coordinates will have been correct all along so we should simply
*     return without action

      IF (CENTRE_COORDS .EQ. 'PLANET' .OR. CENTRE_COORDS .EQ. 'RD')
     :     RETURN



*     calculate apparent ra, dec of centre
*     Assume that we are not using an AZ centre!

      IF (CENTRE_COORDS .EQ. 'AZ') THEN

         STATUS = SAI__ERROR
         CALL ERR_REP(' ','SCULIB_FIX_SCAN: AZ coordinates ' //
     :        'not yet tested', STATUS)

      ELSE

         CALL SCULIB_CALC_APPARENT(LAT_OBS, LONG, LAT, 0.0D0, 0.0D0,
     :        0.0D0, 0.0D0, CENTRE_COORDS, 0.0, MJD, 0.0D0, 0.0D0,
     :        RA_OBJ, DEC_OBJ, OUT_ROTATION, STATUS)

      END IF


*  calculate tangent plane offset in apparent ra, dec

      ITEMP = 0
      CALL SLA_DS2TP (RA_OFF_START, DEC_OFF_START, RA_OBJ,
     :  DEC_OBJ, XOFF_START, YOFF_START, ITEMP)


      ITEMP = 0
      CALL SLA_DS2TP (RA_OFF_END, DEC_OFF_END, RA_OBJ,
     :  DEC_OBJ, XOFF_END, YOFF_END, ITEMP)


      LENGTH = SQRT ((XOFF_START-XOFF_END)**2 +
     :  (YOFF_START-YOFF_END)**2) * RAD2SEC

*  and then convert the apparent ra,dec offsets to rj (this a late modification
*  because tel cannot handle rd local coords with centre_coords other than
*  rd)

      CALL SLA_AMP (RA_OBJ, DEC_OBJ, MJD, 2000.0D0, RA_RJ_OBJ,
     :  DEC_RJ_OBJ)

      RTEMP = SNGL (XOFF_START)
      RTEMP = RTEMP * SNGL (RAD2SEC)
      D_RA_OFF = DBLE(RTEMP)

      RTEMP = SNGL (YOFF_START)
      RTEMP = RTEMP * SNGL (RAD2SEC)
      D_DEC_OFF = DBLE (RTEMP)

      CALL SLA_DTP2S (D_RA_OFF, D_DEC_OFF, RA_OBJ, DEC_OBJ,
     :  RA_TEMP, DEC_TEMP)
      CALL SLA_AMP (RA_TEMP, DEC_TEMP, MJD, 2000.0D0, RA_RJ_TEMP,
     :  DEC_RJ_TEMP)
      ITEMP=0
      CALL SLA_DS2TP (RA_RJ_TEMP, DEC_RJ_TEMP, RA_RJ_OBJ, DEC_RJ_OBJ,
     :  D_RA_OFF, D_DEC_OFF, ITEMP)
      RA_RJ_OFF_START = REAL (D_RA_OFF)
      DEC_RJ_OFF_START = REAL (D_DEC_OFF)




*  now work out the apparent ra, dec corresponding to the incorrect
*  start offsets

      CALL SLA_DTP2S (RA_RJ_OFF_START/RAD2SEC,
     :  DEC_RJ_OFF_START/RAD2SEC,
     :  RA_RJ_OBJ, DEC_RJ_OBJ, RA_TEMP, DEC_TEMP)
      CALL SLA_MAP (RA_TEMP, DEC_TEMP, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :  2000.0D0, MJD, RA_NEW_START, DEC_NEW_START)

*  work out the incorrect rj offset of the scan end

      RTEMP = SNGL (XOFF_END)
      RTEMP = RTEMP * SNGL (RAD2SEC)
      D_RA_OFF = DBLE(RTEMP)

      RTEMP = SNGL (YOFF_END)
      RTEMP = RTEMP * SNGL (RAD2SEC)
      D_DEC_OFF = DBLE (RTEMP)

      CALL SLA_DTP2S (D_RA_OFF, D_DEC_OFF, RA_OBJ, DEC_OBJ,
     :  RA_TEMP, DEC_TEMP)
      CALL SLA_AMP (RA_TEMP, DEC_TEMP, MJD, 2000.0D0, RA_RJ_TEMP,
     :  DEC_RJ_TEMP)
      ITEMP=0
      CALL SLA_DS2TP (RA_RJ_TEMP, DEC_RJ_TEMP, RA_RJ_OBJ, DEC_RJ_OBJ,
     :  D_RA_OFF, D_DEC_OFF, ITEMP)
      RA_RJ_OFF_END = REAL (D_RA_OFF)
      DEC_RJ_OFF_END = REAL (D_DEC_OFF)


*  now use the incorrect rj offsets to calculate the scan angle and work
*  out the incorrect rj offset equal to the start plus the known scan
*  speed

      LENGTH2 = SQRT ((RA_RJ_OFF_END-RA_RJ_OFF_START)**2 +
     :  (DEC_RJ_OFF_END-DEC_RJ_OFF_START)**2)
      RA_END = RA_RJ_OFF_START + LENGTH *
     :  (RA_RJ_OFF_END-RA_RJ_OFF_START) / LENGTH2
      DEC_END = DEC_RJ_OFF_START + LENGTH *
     :  (DEC_RJ_OFF_END-DEC_RJ_OFF_START) / LENGTH2


      LENGTH3 = SQRT ((RA_END-RA_RJ_OFF_START)**2 +
     :  (DEC_END-DEC_RJ_OFF_START)**2)
*      PRINT *, 'LENGTHS ', LENGTH, LENGTH2, LENGTH3

*  now work out the apparent ra, dec corresponding to the incorrect
*  end offsets

      CALL SLA_DTP2S (RA_END/RAD2SEC,
     :  DEC_END/RAD2SEC,
     :  RA_RJ_OBJ, DEC_RJ_OBJ, RA_TEMP, DEC_TEMP)
      CALL SLA_MAP (RA_TEMP, DEC_TEMP, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :  2000.0D0, MJD, RA_NEW_END, DEC_NEW_END)

      END
