      SUBROUTINE FIND21( IOBS, ISOP, ISOURC, OBNSQ, OBPSR, OBTH, SLPSI,
     : SLTH, SPSLGR, SPSLGT, SPSPD, SPSTCL, SPSTCR, SPSTD, SPSTS, TIMCT,
     : STATUS )
*+
*  Name:
*     FIND21

*  Purpose:
*     To store data for a scan that has been found to satisfy find scan
*     acceptance criteria for a source, and to set up crosslinkages
*     between the scan data and the source data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND21( IOBS, ISOP, ISOURC, OBNSQ, OBPSR, OBTH, SLPSI,
*     : SLTH, SPSLGR, SPSLGT, SPSPD, SPSTCL, SPSTCR, SPSTD, SPSTS,
*     : TIMCT, STATUS )

*  Description:
*     To store data for a scan that has been found to satisfy find scan
*     acceptance criteria for a source, and to set up crosslinkages
*     between the scan data and the source data.
*
*     - Store scan details in scan common, this includes:-
*        Updating the number of scans stored in scan common.
*        Enter a cross reference to the source details in the scan
*        record.
*        Enter a cross reference to the scan number in scan common in
*        the source record, and increment the number of scans found for
*        this source.
*        Entering SOP number and observation number in the scan record.
*        Storing the difference between source theta and scan theta as
*        the cross scan displacement.
*        Storing the scan theta.
*        Calculating and storing the satcal crossing time, psi, and
*        solar longitude, of the start and end of the scan length
*        required and the nearest point at which the scan passes the
*        source.
*        Transforming the start scan time and end scan time, from
*        satcal time to UTCS using FIND42. ( A time translation
*        allowance of TITRAL is added to either end of the scan lenght
*        to allow for errors in translation between SATCAL and UCTS
*        times).
*

*  Arguments:
*     IOBS = INTEGER (Given)
*        Do loop control var for Observation loop
*     ISOP = INTEGER (Given)
*        Do loop control variable for SOP loop takes values 11 to 603
*        which are the actual SOP numbers.
*     ISOURC = INTEGER (Given)
*        Do loop control var for Source loop
*     OBNSQ = INTEGER (Given)
*        The observation number
*     OBPSR = REAL (Given)
*        Psi rate for observation (increment in psi per satcal sec)
*     OBTH = REAL (Given)
*        Theta for observation
*     SLPSI = REAL (Given)
*        Source psi
*     SLTH = REAL (Given)
*        Source theta
*     SPSLGR = REAL (Given)
*        Solar longitude rate for SOP (incr.in sol.long. per satcal sec)
*     SPSLGT = REAL (Given)
*        Solar longitude at crossing time
*     SPSPD = REAL (Given)
*        Sign of psi dot for SOP
*     SPSTCL = DOUBLE PRECISION (Given)
*        Satcal time at start of SOP
*     SPSTCR = REAL (Given)
*        Satcal rate for SOP
*     SPSTD = INTEGER (Given)
*        Days part of time of start of SOP
*     SPSTS = REAL (Given)
*        Secs part of time of start of SOP
*     TIMCT = DOUBLE PRECISION (Given)
*        SATCAL time of crossing time. ie best current estimate of the
*        time of closest approach of the observation to the source.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD routines:
*        FIND42

*  Notes:
*  Notes on testing of subroutine on GCASS data
*  The data entered was as follows
*   Source     Coord             Position               Region size
*    Name       Sys      1st Coord       2nd Coord    Inscan  Xscan
*  GAMMACAS   Eq'50    00 55 00.00     60 40 00.00    120.0   20.0
*
*     -  CROSS SCAN SIZE recorded in scan common
*     For GCASS the x scan offset between source and observation 11 of
*     SOP 381 was -6.2104 radians, when this is translated to arcmin
*     it gives -21.3497 which compares with the SNIP_CRDD x scan value
*     for this SOP of -21.4.
*
*     -  IN SCAN size recorded in scan common
*     For observation 11 of SOP 381 the psi of the nearest scan point
*     to the source is 0.7169860 radians, and the source crossing takes
*     place at 16476379.4200 satcal secs.
*
*     The inscan value entered is 120 arc min which gives a half
*     inscan distance in radians of 0.017453292. The rate at which the
*     satellite was tracking was 3.849 arc secs per satcal sec, this
*     gives a radian rate of 0.0011196623 radians per satcal sec (the
*     data value being given in the SPFARCH file).
*
*     Therefore the satcal sec taken to cover this half in scan length
*     is 0.017453292 / 0.0011196623 = 15.588000 . This is then
*     translated to true secs (see below) giving 15.588000 / 1.000054
*     = 15.58715829. The satcal start of scan time is then 16476379.4200
*     - 15.58715829 = 16476363.8328. The satcal end of scan time is then
*     16476379.4200 + 15.58715829 = 16476395.0071.
*
*     -  Translation of secs to satcal.
*     The SATCAL clock gained 3.5 secs per day relative to real secs.
*     Therefore we expect more satcal "secs" for a given number of real
*     secs. Therefore satcal "secs" = secs * 1.000054(approx) (the data
*     value being given in the SPFARCH file).

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     27-JAN-1992 (DCP):
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

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      INTEGER IOBS
      INTEGER ISOP
      INTEGER ISOURC
      INTEGER OBNSQ
      REAL OBPSR
      REAL OBTH
      REAL SLPSI
      REAL SLTH
      REAL SPSLGR
      REAL SPSLGT
      REAL SPSPD
      DOUBLE PRECISION SPSTCL
      REAL SPSTCR
      INTEGER SPSTD
      REAL SPSTS
      DOUBLE PRECISION TIMCT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL TITRAL                ! Time translation from satcal to utcs
                                 ! allowance
      PARAMETER ( TITRAL = 5.0 )

*  Local Variables:
      INTEGER ISCAN              ! The pointer to the scan record to be
                                 ! filled
      INTEGER TESOSC             ! Temporary storage for the position of
                                 ! the scan pointer in the related
                                 ! source record.
      REAL TIMHST                ! Time to cover half the required scan
                                 ! length in secs.
      REAL TIMHSS                ! Time to cover half the required scan
                                 ! lenght in satcal.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the scan record to be filled to the next vacant scan record
      ISCAN = NOSCAN + 1

*  Set the pointer to the associated source in the scan record
      SCSOI( ISCAN ) = ISOURC

*  Add one to the number of scans for this source in the source common
*  record for this source
      SONOSC( ISOURC ) = SONOSC( ISOURC ) + 1

*  Make a temporary copy of this position in TESOSC
      TESOSC = SONOSC( ISOURC )

*  Store the scan number in the scan index for the source
      SOSCI( ISOURC, TESOSC ) = ISCAN

*  Set the cross scan value to the differnce between source theta and
*  observation theta
      SCXSC( ISCAN ) = SLTH - OBTH

*  Set the scan SOP number to the SOP currently being processed
      SCSOP( ISCAN ) = ISOP

*  Set the scan observation number to
      SCOBS( ISCAN ) = OBNSQ

*  Set the scan theta to the observation theta
      SCTH( ISCAN ) = OBTH

*  Set the scan nearest point psi to the source psi
      SCNPPS( ISCAN ) = SLPSI

*  Set the scan nearest point solar longitude to that calculated in
*  FIND19
      SCNPSL( ISCAN ) = SPSLGT

*  Set the scan nearest point SATCAL time to that calculated for the
*  last estimate of the crossing time
      SCNPST( ISCAN ) = TIMCT

*  Calculate the time in satcal secs needed to cover the begining of
*  the scan to the nearest point to the source. This is calculated from
*  the half inscan lenght required for the source, and dividing by the
*  observation psi rate.
      TIMHSS = SOINSZ( ISOURC ) / ( OBPSR * 2.0 )

*  Calculate the true secs time equivalent to TIMHSS. This is the TIMHSS
*  divided by the satcal rate of the scan
      TIMHST = TIMHSS / SPSTCR

*  Set the start of scan psi to be nearest point psi - time for half
*  scan (as satcal) * observation psi rate * sign of the psi rate
      SCSTPS( ISCAN ) = SCNPPS( ISCAN ) - TIMHSS * SPSPD * OBPSR

*  Set the end of scan psi to be nearest point psi + time for half
*  scan (as satcal) * observation psi rate * sign of the psi rate
      SCENPS( ISCAN ) = SCNPPS( ISCAN ) + TIMHSS * SPSPD * OBPSR

*  Set the start of scan satcal time to be nearest point time - time
*  for half scan (as satcal) * sign of the psi rate
      SCSTST( ISCAN ) = SCNPST( ISCAN ) - TIMHSS * SPSPD

*  Set the end of scan satcal time to be nearest point time + time
*  for half scan (as satcal) * sign of the psi rate
      SCENST( ISCAN ) = SCNPST( ISCAN ) + TIMHSS * SPSPD

*  Set the start of scan solar longitude to be nearest point solar
*  longitude - time for half scan (as satcal) * observation solar
*  longitude rate * sign of the psi rate
      SCSTSL( ISCAN ) = SCNPSL( ISCAN ) - TIMHSS * SPSLGR * SPSPD

*  Set the end of scan solar longitude to be nearest point solar
*  longitude + time for half scan (as satcal) * observation solar
*  longitude rate * sign of the psi rate
      SCENSL( ISCAN ) = SCNPSL( ISCAN ) + TIMHSS * SPSLGR * SPSPD

*  Translate the start of scan satcal time to UTCS and subtract
*  time translation allowance.
      CALL FIND42( SCSTST( ISCAN ), SPSTCL, SPSTCR, SPSTD, SPSTS,
     :             SCSTUT( ISCAN ), STATUS )
      SCSTUT( ISCAN ) = SCSTUT( ISCAN) - TITRAL

*  Translate the end of scan satcal time to UTCS and add
*  time translation allowance.
      CALL FIND42( SCENST( ISCAN ), SPSTCL, SPSTCR, SPSTD, SPSTS,
     :             SCENUT( ISCAN ), STATUS )
      SCENUT( ISCAN ) = SCENUT( ISCAN) + TITRAL

*  Set scan required flag to .TRUE.
      SCRQFL( ISCAN ) = .TRUE.

*  Update the number of scans in scan common
      NOSCAN = NOSCAN + 1

      END
