*+
*  Name:
*     stackcomm.inc

*  Purpose:
*     Specx STACKCOMM include file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INCLUDE 'STACKCOMM'

*  Description:
*     This is the include file for the stack common block in Specx.
*     Contains (9216+10880) 4-byte words.
*
*     byte  lwrd  type  content
*        0     0  D     LOFREQ(1), LOFREQ(2)
*       16     4  D     LOFREQ(3), LOFREQ(4)
*       32     8  D     LOFREQ(5), LOFREQ(6)
*       48    12  D     LOFREQ(7), LOFREQ(8)
*       64    16  D     IFFREQ(1), IFFREQ(2)
*       80    20  D     IFFREQ(3), IFFREQ(4)
*       96    24  D     IFFREQ(5), IFFREQ(6)
*      112    28  D     IFFREQ(7), IFFREQ(8)
*      128    32  D     RA,  DEC
*      144    36  R     DRA, DDEC, AZ, EL
*      160    40  R     TSYS(1), TSYS(2), TSYS(3), TSYS(4)
*      176    44  R     TSYS(5), TSYS(6), TSYS(7), TSYS(8)
*      192    48  R     VSL, VES, VTE, VLSR
*      208    52  I4    JFREST(1), JFREST(2), JFREST(3), JFREST(4)
*      224    56  I4    JFREST(5), JFREST(6), JFREST(7), JFREST(8)
*      240    60  I4    JFCEN(1), JFCEN(2), JFCEN(3), JFCEN(4)
*      256    64  I4    JFCEN(5), JFCEN(6), JFCEN(7), JFCEN(8)
*      272    68  I4    JFINC(1), JFINC(2), JFINC(3), JFINC(4)
*      288    72  I4    JFINC(5), JFINC(6), JFINC(7), JFINC(8)
*      304    76  I4    ITREC(1), ITREC(2), ITREC(3), ITREC(4)
*      320    80  I4    ITREC(5), ITREC(6), ITREC(7), ITREC(8)
*      336    84  I4    ITSKY(1), ITSKY(2), ITSKY(3), ITSKY(4)
*      352    88  I4    ITSKY(5), ITSKY(6), ITSKY(7), ITSKY(8)
*      368    92  I4    ITTEL(1), ITTEL(2), ITTEL(3), ITTEL(4)
*      384    96  I4    ITTEL(5), ITTEL(6), ITTEL(7), ITTEL(8)
*      400   100  I4    NPTS(1), NPTS(2), NPTS(3), NPTS(4)
*      416   104  I4    NPTS(5), NPTS(6), NPTS(7), NPTS(8)
*      432   108  I4    INTT, LSCAN, IMODE, NQUAD
*      448   112  I4    ICALZD, LSRFLG, IQCEN, IST
*
*                       +-------+-+----------------------
*      464   116        |IEND   |*|ITITLE(1:11)         >  *=IUTFLG
*                       +-------+-+-------------------+--
*      480   120        >ITITLE(12:26)                |*>  *=IDATE(1:1)
*                       ----------------+-------------+-+
*      496   124        >IDATE(2:9)     |ITIME(1:8)     |
*                       ----------------+---------------+
*      512   128

*  Authors:
*     rp: Rachael Padman (MRAO, UCB)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     {date} (rp):
*        Original version.
*     09 Aug 1993 (rp):
*        File format v3.0.
*     17 Dec 1993 (hme):
*        Re-order common block so that 8-byte quantities come first,
*        character variables last (before data).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Variables:
      DOUBLE PRECISION LOFREQ(8) ! Local oscillator frequ., GHz
      DOUBLE PRECISION IFFREQ(8) ! Intermediate frequ., GHz (signed, +=usb)

      DOUBLE PRECISION RA  ! R.A. of observation ( degrees)
      DOUBLE PRECISION DEC ! Dec. of observation ( degrees)

      REAL DRA             ! Offset in R.A. ( arcsec )
      REAL DDEC            ! Offset in Dec.  ( arcsec )      

      REAL AZ              ! Azimuth at start of observation
      REAL EL              ! Elevation at start of observation

      REAL TSYS(8)         ! System noise temp. in each quadrant/filter-bank

      REAL VSL             ! Velocity of sun w.r.t. LSR
      REAL VES             ! Velocity of barycentre w.r.t. Sun
      REAL VTE             ! Velocity of telescope w.r.t. barycentre
      REAL VLSR            ! LSR (or ref frame) velocity of source.

      INTEGER JFREST(8)    ! Rest frequency for each quadrant (kHz)
      INTEGER JFCEN(8)     ! Centre frequency of each quadrant ( kHz )
      INTEGER JFINC(8)     ! Channel spacing  "  ( Hz )
      INTEGER ITREC(8)     ! Receiver noise temperature ( K )
      INTEGER ITSKY(8)     ! Apparent sky temperature ( K )
      INTEGER ITTEL(8)     ! Apparent constant telescope temperature ( K )
      INTEGER NPTS(8)      ! Number of data points per sector.

      INTEGER INTT         ! Integration time at any frequency ( ms )

      INTEGER LSCAN        ! Sequence number of observation
      INTEGER IMODE        ! Mode of observation
      INTEGER NQUAD        ! Number of quadrants/filter-banks
      INTEGER ICALZD       ! Zenith angle of receiver/sky calibration
      INTEGER LSRFLG       ! Flag: Set = 1 if JFCEN corrected for lsr.
      INTEGER IQCEN        ! Sector to which LSR correction applied.

      INTEGER IST          ! Glue
      INTEGER IEND         ! Glue

      BYTE IUTFLG          ! Time is U.T. if = 1
      CHARACTER*26 ITITLE  ! Character title of observation.
      CHARACTER*9  IDATE   ! Character date of observation dd-MON-yy
      CHARACTER*8  ITIME   ! Character time of start of observation hh:mm:ss

      REAL DATA(1024)      ! Data buffer for current spectrum
      REAL STK(8064)       ! Remainder of stack space.
      REAL STORE(2176,5)   ! Storage for gain and STORE arrays.

      COMMON / STACK /
     :   LOFREQ, IFFREQ,
     :   RA, DEC, DRA, DDEC,
     :   AZ, EL,
     :   TSYS,
     :   VSL, VES, VTE, VLSR,
     :   JFREST, JFCEN, JFINC,
     :   ITREC, ITSKY, ITTEL,
     :   NPTS,
     :   INTT,
     :   LSCAN, IMODE, NQUAD, ICALZD, LSRFLG, IQCEN,
     :   IST, IEND,
     :   IUTFLG, ITITLE, IDATE, ITIME,
     :   DATA, STK, STORE

      INTEGER SCAN_HEADER(128)
      EQUIVALENCE ( LOFREQ(1), SCAN_HEADER(1) )

      SAVE   / STACK /

*. End of stackcomm.inc
