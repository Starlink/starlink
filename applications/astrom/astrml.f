      SUBROUTINE ASTRML (LUI, LUR, LUS, LUX, FTPREF, FITSWCSSTYLE)
*+
*     - - - - - - -
*      A S T R M L
*     - - - - - - -
*
*  Basic astrometry program - main routine.
*
*  Given:
*
*     LUI     i      I/O unit number for input file
*     LUR     i      I/O unit number for report file
*     LUS     i      I/O unit number for synopsis file
*     LUX     i      I/O unit number for log file (zero to suppress)
*     FRPREF  s      Filename prefix for generated FITS files (' ' to suppress)
*     FITSWCSSTYLE s WCS style to use for the FITS headers
*
*  Both the report and the synopsis file contain Fortran printer
*  format codes.  In the case of the synopsis file, the code is
*  always a space;  on some platforms this will appear on the
*  terminal as a space, but on others will be suppressed.
*
*  Called:  INDAT, EQEP, RADEC, EQEP, COCOMM, COCOMO, COCOOM, RDCAL,
*           TRANSF, sla_EPJ, sla_EPB2D, sla_EPJ2D, sla_GMST, sla_EPB,
*           sla_EPCO, sla_DRANGE, sla_DRANRM, sla_DSEP, sla_KBJ,
*           sla_DFLTIN, sla_CALDJ, sla_OBS, sla_DR2TF, sla_DR2AF,
*           sla_MAPPA, sla_AOPPA, sla_DJCL, sla_DS2TP, sla_PCD,
*           sla_SVD, sla_SVDSOL, sla_DTP2S, sla_SVDCOV, sla_INVF,
*           sla_DCMPF, sla_PXY, sla_XY2XY
*
*  P T Wallace   Starlink   8 April 1998
*  Norman Gray   Starlink   2 May 2003
*
*-

      IMPLICIT NONE

      INTEGER LUI,LUR,LUS,LUX
      CHARACTER*(*) FTPREF, FITSWCSSTYLE

*+
*
*  Format of input file:
*
*     The input file, which is an ordinary text file, contains one or
*     more sequences of records.  Each sequence performs a complete
*     and separate astrometric reduction.
*
*     Multiple input sequences are permitted.  A record beginning
*     '/' separates one sequence from the next.  The input file
*     is terminated either by end-of-file or by a record
*     beginning 'E'.
*
*     Uppercase and lowercase are both acceptable and may be mixed
*     freely.
*
*     Leading spaces are ignored.
*
*     The overall layout of each sequence is as follows:
*
*       group              no. of records     mandatory?
*
*       results equinox    1                  no (default J2000)
*       telescope type     1                  no (default Schmidt)
*       plate data         1                  yes
*       observation data   1-3                no (discussed below)
*       reference stars    2-3 per star       at least 2 stars
*       unknown stars      1-2 per star       no
*
*     The RESULTS EQUINOX record specifies the epoch of the mean
*     equator and equinox to be used, for example 'B1950'.
*
*     The TELESCOPE TYPE record specifies the projection geometry.
*     For small areas of a plate it can be omitted without serious
*     effect.  The various options are listed later.
*
*     The mandatory PLATE DATA record specifies the plate's central
*     RA,Dec, and also the epoch to be used for the proper motion
*     corrections (and possibly the transformation to observed
*     coordinates).
*
*     The optional OBSERVATION DATA records enable ASTROM to
*     reconstruct the precise appearance of the field, rather
*     than absorbing the various predictable rotations and
*     distortions into the fit.  The three records specify,
*     respectively, the date and time, the position of the
*     observatory, and the air temperature and pressure.  These
*     records can safely be omitted except where high precision is
*     required, and for wide fields and low elevations.
*
*     Each REFERENCE STAR requires a pair of records (specifying
*     the star's celestial and plate coordinates respectively),
*     optionally preceded by a colour record, which specifies the
*     effective wavelength of the star/emulsion/filter/atmosphere
*     combination.  Data for at least two stars are needed, to allow
*     the minimal 4-coefficient plate model to be determined.  Three
*     stars allow a 6-coefficient linear model to be fitted.  If at
*     least ten stars are provided, the option is available of
*     extending the 6-coefficient fit to include determination of the
*     radial distortion coefficient and/or plate centre, bringing the
*     number of coefficients to 7, 8 or 9.  Typical numbers of
*     reference stars in practice are 10-50;  the upper limit is
*     given by the parameter MAXREF (see below).
*
*     Each UNKNOWN STAR requires a single record (either the plate
*     or the celestial coordinates of the star), again optionally
*     preceded by a colour record.  It is not necessary to include
*     any unknown stars, where the object of the exercise is simply
*     to assess an astrometric solution.
*
*     The colour records, which optionally precede reference star
*     and unknown star data, are important for precise work,
*     especially at low elevations.  They have no effect unless the
*     optional observation data (time, location, refraction) have
*     been supplied.
*
*     Most of the records consist of or contain various numbers of
*     numeric fields, separated by spaces (or commas).  In many
*     cases it is simply the number of fields present which enables
*     ASTROM to determine which sort of record has been read.
*     Free-format number decoding is used throughout;  spaces can
*     be freely inserted between fields, and many other freedoms
*     are permitted (see the documentation for the sla_DFLTIN and
*     sla_DBJIN routines).  Completely blank records, and any
*     consisting solely of a comment, are ignored and can be used
*     to improve layout and provide commentary.
*
*     Within the data records, the following elementary forms are
*     used (in addition to simple numbers):
*
*        RA        -  Right Ascension:  3 numbers, h m s;  h and m
*                     must be integral.
*        Dec       -  Declination:  3 numbers, D ' ";  D and ' must
*                     be integral;  the sign of the Dec is given by
*                     the sign of D, even if D is zero.
*        PM        -  Proper motions in RA and Dec:  2 numbers, s/year
*                     and "/year respectively.
*        Equinox   -  A number, years, optionally preceded by B or J.
*        Epoch     -  As for equinox.
*        Parallax  -  A number, arcseconds;  must be between 0 and 1.
*        Date      -  The UT calendar date:  3 numbers, y m d;  all
*                     three numbers must be integral.
*        Time      -  The UT:  2 numbers, h m;  h must be integral.
*        ST        -  Local Sidereal Time: 2 numbers, h m;  h must be
*                     integral.
*        Obs ID    -  Observatory identifier (see the sla_OBS routine).
*        Long      -  Longitude (East +ve):  2 numbers, D ';  D must
*                     be integral;  the sign of the longitude is given
*                     by the sign of D, even if D is zero.
*        Lat       -  Latitude:  2 numbers, D ';  D must be integral;
*                     the sign of the latitude is given by the sign of
*                     D, even if D is zero.
*        Name      -  the 10 characters beginning with the first
*                     non-space following any '*'.
*
*     The different types of record are composed as follows.  Optional
*     items are given in brackets [thus].
*
*       Results equinox:        Equinox (defaults to J2000.0).
*       Telescope type:         'ASTR...' = astrograph
*                               'SCHM...' = Schmidt (default)
*                               'AAT2...' = AAT prime focus doublet
*                               'AAT3...' = AAT prime focus triplet
*                               'AAT8...' = AAT f/8 with bent plate
*                               'JKT8...' = JKT (f/8 Harmer-Wynne focus)
*                               'GENE...' p = general pincushion/barrel
*                                             distortion of size p
*                                             (see note 2, below)
*       Plate:                  central RA Dec Equinox [Epoch]
*       Date/time:       either 'T' Date Time
*                        or     'T' ST
*                        or     'T' Epoch
*       Observatory:     either 'O' Obs ID
*                            or 'O' [Long] Lat [Height (metre)]
*       Meteorological:         'M' Temp (degK) [Pressure (mB)]
*       Colour:                 'C' wavelength (nanometres)
*       Reference star: 1st rec RA Dec PM Equinox [Epoch] [Parallax] [Name]
*                            or RA Dec Equinox Epoch [Name]
*                       2nd rec X Y
*       Unknown star:    either X Y [Name]
*                            or RA Dec Equinox [Name]
*
*     To fit the radial distortion coefficient, precede the telescope
*     type with the symbol '~'.  To fit the plate centre, precede the
*     plate central RA,Dec with the symbol '~'.
*
*     Notes:
*
*       1)  Here is an example of an ASTROM input file.  It contains
*           two sequences.  The first is a typical reduction from
*           a Schmidt plate.  The second contains observation and
*           colour data, as would be required for precise work,
*           and requests fitting of the plate centre.
*
*  ---------------------------------------------------------------------
*
*     B1950                                     * Results in FK4
*     SCHM                                      * Schmidt geometry
*     19 04 00.0  -65 00 00  B1950.0  1974.5    * Plate centre, and epoch
*
*     18 56 39.426  -63 25 13.23  -0.0002  -0.036  B1950.0  * Ref 1
*     44.791   85.643
*
*     19 11 53.909  -63 17 57.57   0.0058  -0.044  1950.0   * Ref 2
*     -46.266   92.337
*
*     19 01 13.606  -63 49 14.84   0.0020  -0.026  1950.0   * Ref 3
*     17.246   64.945
*
*     19 08 29.088  -63 57 42.79   0.0016   0.018  1950.0   * Ref 4
*     -25.314   57.456
*
*     19 02 10.088  -63 29 16.73   0.0012  -0.019  1950.0   * Ref 5
*     11.890   82.766
*
*     -5.103    58.868                      *  Candidate
*     19 09 46.2  -63 51 27  J2000.0        *  Radio pos
*
*     /                                     *  End of first sequence
*
*     * AAT plate 2266 (f/8 RC)  NGC 3114
*     AAT8
*     ~ 10 01 00.0 -59 53 01 B1950
*     Time 1984 01 20  16 00
*     Obs AAT
*     Met 288 899
*     Colour 450    *  Default colour for reference stars
*     10 01 21.203 -59 52 14.05 B1950 J1984.1
*     9.0353 18.4211 *130
*     10 00 16.401 -59 52 52.16 B1950 J1984.1
*     1.7304 17.9282 *70
*     10 00 18.516 -59 53 10.20 B1950 J1984.1
*     1.9669 17.6566 *73
*     10 00 19.620 -59 49 01.62 B1950 J1984.1
*     2.1223 21.3760 *74
*     10 00 20.525 -59 52 01.09 B1950 J1984.1
*     2.2025 18.6888 *75
*     10 00 21.416 -59 51 30.27 B1950 J1984.1
*     2.3067 19.1501 *76
*     10 00 22.896 -59 53 49.60 B1950 J1984.1
*     2.4544 17.0626 *80
*     10 01 26.159 -59 50 38.50 B1950 J1984.1
*     9.6143 19.8435 *134
*     10 01 28.328 -59 51 16.86 B1950 J1984.1
*     9.8509 19.2653 *138
*     10 01 54.446 -59 54 39.28 B1950 J1984.1
*     12.7495 16.1963 *156
*     10 01 54.523 -59 50 01.72 B1950 J1984.1
*     12.8193 20.3493 *157
*     10 01 57.438 -59 51 26.29 B1950 J1984.1
*     13.1292 19.0793 *161
*     10 00 12.385 -60 08 08.51 B1950 J1984.1
*     1.1637 4.2188 *65
*     C 500
*     5.8265 12.7252 *104 red
*     C 400
*     5.8265 12.7252 *104 blue
*
*     END
*
*  ---------------------------------------------------------------------
*
*       2)  Only the first four characters of the telescope type are
*           checked.  All projection geometries are treated as
*           a correction, relative to tangent-plane geometry, to
*           radial distance R from the plate centre equal to p*R**3,
*           where the coefficient p depends on the telescope type.  In
*           the case of general pincushion/barrel distortion, for
*           which the telescope type is 'GENE...', the coefficient p
*           is supplied directly, as a number following the telescope
*           type.  In the other cases the coefficient p is selected
*           internally within ASTROM given the telescope type.  The
*           units of p satisfy the expression R'=R*(1+p*R**2), where R'
*           is the actual radial distance and R the tangent-plane radial
*           distance, and where R' and R are measured in units of one
*           focal length.
*
*       3)  Reference star celestial positions may be given in several
*           ways.  The most usual format includes proper motions and
*           has an optional epoch which defaults to that of the equinox.
*           Another common format has no proper motions and must have
*           an epoch as well as an equinox.  The second of these formats
*           should be used for stars whose positions are given in an
*           old-style reference frame (e.g. B1950.0) and whose proper
*           motions are presumed or known to be zero in an inertial
*           frame.  Such stars have spurious proper motions in the
*           old system and so the epoch is required.  Because the
*           new system is much closer to an inertial frame only the
*           first format should be used for such positions, with
*           zeroes specified for the proper motions.  The first format
*           (with proper motions) has the supplementary option of
*           allowing the annual parallax to be specified, following or
*           instead of the epoch.  In the case where the parallax is
*           supplied without an epoch, which of the two is meant is
*           deduced from the size of the number given.  In the case where
*           an epoch is supplied as well as well as a parallax, it is
*           assumed that the parallax has yet to be applied.  In other
*           words, the option to have the parallax removed from a reference
*           star at the given catalogue epoch and then put back in for
*           the epoch of the plate is NOT provided.  The parallax is
*           only taken into account (except for second-order effects on
*           the proper motion) when a reduction in observed place
*           has been requested (by supplying observatory, time and
*           refraction information - see Note 6, below).  Note that no
*           provision is made to specify the radial velocity of a
*           reference star.  This would only matter in cases where the
*           plate epoch is very distant from the reference star epoch
*           and where both radial velocity and parallax are large.
*
*       4)  X and Y may be in any units and have any origin;  however,
*           the various reports formats give the most satisfactory
*           results if X and Y are both in millimetres, and are not
*           too far from the origin.  The units in X and Y must be the
*           same if the 4-coefficient solution is to be of any use, but
*           may be different without impairing the higher order
*           solutions.  Handedness of X and Y is immaterial.
*
*       5)  When an unknown star X,Y is supplied, its celestial
*           position is computed;  if its RA,Dec is supplied the
*           corresponding X,Y is computed.  In the latter case,
*           the equinox has to be supplied;  the epoch is, of course,
*           that of the plate.  For unknown stars, no provision is
*           made for specifying either proper motion or parallax.
*
*       6)  The optional time, observatory, meteorological and colour
*           records allow a more precise form of reduction where the
*           fit is carried out using the "observed" positions of the
*           stars (i.e. as affected by various distortions, the most
*           important of which is refraction).  For Schmidt plates
*           exposed at very low elevations the results can be wrong
*           by several arcseconds if this information is omitted.
*
*           These records all begin with an explicit identifier, of
*           which only the first character (T, O, M or C) is
*           significant.
*
*           The time, observatory and meteorological records can be in
*           any order.  If all three are omitted, any colour records
*           subsequently encountered will be ignored.  In the absence
*           of full information, action is taken to make good the
*           deficiencies.  If insufficient information for the observed
*           place is available, warnings are issued and the astrometry
*           is done using mean place.
*
*           If any of the three observation data records appears twice,
*           the new information supplants the old, and no error is
*           reported.
*
*           The TIME record can either specify the UT date and time, the
*           epoch, or the local sidereal time, for mid-exposure.  If the
*           ST option is used, the epoch on the plate data record must
*           be specified (and should be accurate to a day or two if the
*           annual aberration and solar deflection are to be correctly
*           computed).  If the time record specifies a UT or an epoch,
*           and an epoch is specified on the plate data record, the
*           latter is ignored, without any error being reported.
*
*           The OBSERVATORY record can either specify one of the
*           observatory identifiers recognised by the sla_OBS routine,
*           or the observatory position can be given directly.  If the
*           TIME record specifies sidereal time, the observatory
*           longitude may optionally be omitted.  The height (metres
*           above sea level) is not really needed unless the
*           meteorological record is absent, in which case the height
*           is used to estimate the pressure.
*
*           COLOUR records can appear anywhere after the time,
*           observatory and meteorological records, except between a
*           pair of reference star records.  The effective
*           wavelength specified by such a record applies to all
*           stars from that point onwards.  Should two colour records
*           follow consecutively, the second supplants the first, and
*           no error is reported.  Prior to the first colour record,
*           a default of 500nm is assumed.
*
*       7)  The more precise reduction possible through the use of
*           the observation data records is useful for checking
*           independently computed data (fibre feed positions, guide
*           star coordinates, etc).
*
*       8)  The colour information is used to allow for atmospheric
*           dispersion and can can be important for low elevations
*           and detectors of wide spectral coverage, especially where
*           some of the stars are extremely red or blue.  The following
*           table suggests effective wavelengths for a few combinations
*           of emulsion, filter and colour;  the "A" column is for a
*           star of spectral type A, while "B" and "R" refer to very
*           blue and very red (thermal) sources respectively. (The
*           effects may, of course, be more extreme for emission-line
*           objects and other non-blackbody sources.)
*
*           Band       Emulsion  Filter      B      A      R
*
*           U          O )       UG 1       365    365    365
*                      J )
*
*           B          IIa O       -        410    410    420
*                      IIIa J    GG 385 )   410    410    500
*                                GG 395 )
*
*           V          IIa D     GG 495     550    560    600
*
*           R          IIIa F )  RG 610 )   675    675    675
*                      103a E )  RG 630 )
*                      098-04 )  GG 495     600    620    675
*
*           I          IV-N      GG 695     800    800    800
*
*     It must, however, be noted that there may be other important
*     colour effects, apart from atmospheric dispersion, notably where
*     refracting optics have been used.  There is no attempt in ASTROM
*     to model such effects.
*
*     9)  Reliable fitting of the radial distortion and the plate
*     centre depends on having substantial numbers of accurately
*     measured reference stars, distributed all over the plate.  The
*     plate centre is particularly ill-determined for the case of
*     Schmidt geometry.  If there are signs that the fit has not been
*     satisfactory, the results are not used.
*
*  Method:
*
*     For each input sequence, up to three astrometric solutions are
*     found.  The first is a 4-coefficient "solid body rotation" model
*     (zero points, orientation and scale only), which assumes equal
*     scales in X and Y, and which requires at least two reference
*     stars.  The second, computed in addition to the 4-coefficient
*     model if there are at least three reference stars, is a
*     6-coefficient linear model (zero points, scales in X and Y,
*     X/Y nonperpendicularity, and orientation).  Where at least ten
*     reference stars have been supplied, the option is available of
*     augmenting the model to include the radial distortion coefficient
*     and/or the plate centre, bringing the number of coefficients to
*     7, 8 or 9.  The 4-coefficient solution is useful (1) for rough
*     and ready astrometry, e.g. from a print using a ruler or graph
*     paper, and (2) for identifying an erroneous reference star, the
*     higher order fits having more scope for disguising the error.
*
*     Internally, the modelling is done in idealised plate coordinates
*     (tangential coordinates Xi,Eta, adjusted for radial distortion),
*     derived from either observed coordinates (if the optional
*     observation data records are provided) or mean coordinates of
*     date.  The star and X,Y data supplied as input, or output on
*     reports, are converted to and from this internal standard as
*     required.
*
*  Models:
*
*     XM,YM = measured x,y (as supplied)
*
*     XE,YE = expected x,y (i.e. deduced from the star's RA,Dec,
*                           knowing the plate centre and radial
*                           distortion coefficient D)
*
*     XP,YP = predicted x,y (i.e. the model applied to XM,YM)
*
*     4-coefficient model;  best of:
*
*        XP = A1 + A2*XM + A3*YM   or   A1 + A2*XM + A3*YM
*        YP = B1 - A3*XM + A2*YM        B1 + A3*XM - A2*YM
*
*     6-coefficient model:
*
*        XP = A1 + A2*XM + A3*YM
*        YP = B1 + B2*XM + B3*YM
*
*     The fits are, in fact, done in terms of small adjustments.  For
*     example the 6-coefficient fit comes from:
*
*        XE-XP = dA1 + dA2*XM + dA3*YM
*        YE-YP = dB1 + dB2*XM + dB3*YM
*
*     Extra terms for plate centre adjustment dX,dY:
*
*        XE-XP = . . . + dX * (XP**2 + D*(3*XP**2+YP**2))
*                      + dY * (XP*YP + D*2*XP*YP)
*        YE-YP = . . . + dX * (XP*YP + D*2*XP*YP)
*                      + dY * (YP**2 + D*(XP**2+3*YP**2))
*
*     Extra terms for radial distortion coefficient adjustment dD:
*
*        XE-XP = . . . - dD * (XP**2+YP**2)*XP
*        YE-YP = . . . - dD * (XP**2+YP**2)*YP
*
*     The fit minimises (XE-XP)**2+(YE-YP)**2.  Each reference
*     star thus produces two rows of design matrix - one for
*     x and one for y.
*
*     Fitting the plate centre and - especially - the distortion is
*     a highly non-linear process, each adjustment producing substantial
*     scale and zero point changes which affect (XE,YE).  To ensure
*     convergence, alternate iterations fit only the linear part of
*     the model.
*
*  Reference frames:
*
*     Both the old pre IAU 1976 (loosely FK4) system and the new
*     post IAU 1976 (loosely FK5) system are supported, and data in the
*     two systems can be freely mixed.  ASTROM follows the established
*     convention of using the equinox to distinguish between the two
*     systems:  if the equinox is prefixed by 'B' (which stands for
*     Besselian) then the position is an old FK4 one;  if a prefix of
*     'J' is used (standing for Julian), the position is a new FK5 one.
*     If no prefix is used, pre 1984.0 equinoxes indicate the old FK4
*     system, and equinoxes of 1984.0 or later indicate the new FK5
*     system.  The 'B' or 'J' prefix may also be used with epochs,
*     although the distinction is unlikely to be significant.  The two
*     most common equinoxes are B1950.0 and J2000.0.  When using the old
*     system (e.g. B1950.0) to specify the position of an object whose
*     proper motion is presumed zero, the second of the two principal
*     reference star formats given above (Note 3) should be used,
*     specifying the epoch at which the position was determined;  such
*     frames are not inertial and even extragalactic objects will have
*     spurious proper motions which need to be taken into account in
*     precise work.
*
*-

*  Degrees, seconds & arcseconds to radians
      DOUBLE PRECISION D2R,S2R,AS2R
      PARAMETER (D2R=1.745329251994329576923691D-2,
     :           S2R=0.72722052166430398D-4,
     :           AS2R=0.48481368110953599D-5)

*  90 deg, in radians
      DOUBLE PRECISION D90
      PARAMETER (D90=90D0*D2R)

*  0.5 minutes, in days
      DOUBLE PRECISION SEC30
      PARAMETER (SEC30=30D0/86400D0)

*  Input buffer
      INTEGER LINBUF
      PARAMETER (LINBUF=80)
      CHARACTER INBUF*(LINBUF)

*  Work areas - data input
      DOUBLE PRECISION WORK(11)
      CHARACTER*10 NAME

*  Buffer for proper motion fields of reference star report
      CHARACTER*16 PMREP

*
*  Reference stars
*
*  Maximum number
      INTEGER MAXREF
      PARAMETER (MAXREF=2000)
*  Actual number
      INTEGER NREF
*  RA,Dec in results system
      DOUBLE PRECISION REFRD(2,MAXREF)
*  RA,Dec in working system (either mean of date or observed)
      DOUBLE PRECISION REFRDW(2,MAXREF)
*  Measured X,Y
      DOUBLE PRECISION REFXYM(2,MAXREF)
*  Expected X,Y (deduced from RA,Dec)
      DOUBLE PRECISION REFXYE(2,MAXREF)
*  Predicted X,Y (deduced from fit)
      DOUBLE PRECISION REFXYP(2,MAXREF)
*  Names
      CHARACTER*10 RNAME(MAXREF)
*  Colours (nm)
      DOUBLE PRECISION REFWL(MAXREF)
*  Flags:  fit ill-conditioned, fit OK, plate centre OK
      LOGICAL ILLCON,FITOK,PCOK

*  Record type:
*    space = next record required
*    T, O, M or C = TIME, OBS, MET or COL record
*    / or E = end of sequence or end of file
*    ? = not yet identified
      CHARACTER RECTYP

*  Telescope type code, projection geometry string
      CHARACTER KTEL*4,KPROJ*40

*  Distortion coefficient and saved value before any fitting
      DOUBLE PRECISION DISTOR,DISTO

*  Approximately flag (applies to telescope type and plate centre only)
      LOGICAL APPROX

*  Fit distortion coefficient and plate centre flags
      LOGICAL FITDI,FITPC

*  Number of solutions, current solution number
      INTEGER NSOLS,NSOL

*  Number of coefficients and maximum so far
      INTEGER NC,NTERMS

*  Plate constants: 4, 6 and 6+ coeff solutions and inverses.
*  PLTCON(1..3,n) holds plate constants (a1,a2,a3), and
*  PLTCON(BVALS+1..3,n) constants (b1,b2,b3), for solution
*  n=NSOL=(1,2,..).  PLTCON(1..6,n+MAXSOL) holds the inverse of
*  PLTCON(1..6,n) for solution NSOL=n.  Maximum number of solutions is
*  MAXSOL.
      INTEGER MAXSOL,BVALS
      PARAMETER (MAXSOL=5,BVALS=3)
      DOUBLE PRECISION PLTCON(6,MAXSOL*2)

*  Pattern of extra non-linear fits to attempt (elements 1 and 2 ignored)
      INTEGER EXTRAFITS(MAXSOL)

*  Maximum number of terms in the 6+ coefficient model
      INTEGER MAXTRM
      PARAMETER (MAXTRM=9)

*  Error message
      CHARACTER FITERR*30

*  Derived quantities
      DOUBLE PRECISION XZERO,YZERO,XSCALE,YSCALE,PERP,ORIENT

*  RMS errors in X, Y and R
      DOUBLE PRECISION XRMS,YRMS,RRMS

*  For formatting
      CHARACTER NANOM*6,KSRA,KSDC
      INTEGER IRAVEC(4),IDCVEC(4)
      CHARACTER KSR1,KSD1
      INTEGER IRVEC1(4),IDVEC1(4)
      CHARACTER KSR2,KSD2
      INTEGER IRVEC2(4),IDVEC2(4)
      INTEGER IUTY,IUTMO,IUTD,IUTH,IUTM,ISTH,ISTM
      CHARACTER KLON
      INTEGER ILOND,ILONM,ILONT
      CHARACTER KPHI
      INTEGER IPHID,IPHIM,IPHIT

*  Input data flags
      INTEGER J1(11),J2(11)

*  Presence of data:
*   plate epoch
*   local sidereal time
*   UT date+time
*   observatory longitude, latitude, height
*   temperature
*   pressure
      LOGICAL GOTPEP,GOTLST,GOTUT,GOTLON,GOTLAT,GOTHM,GOTTDK,GOTPMB

*  Local sidereal time (radians)
      DOUBLE PRECISION STL

*  UT date and time as MJD (JD-2400000.5)
      DOUBLE PRECISION UTMJD

*  Longitude (E +ve, radians), latitude (radians), height (metres)
      DOUBLE PRECISION ELONG,PHI,HM

*  Observatory name
      CHARACTER OBSNAM*40

*  Ambient temperature (deg K) and pressure (mB) at the telescope
      DOUBLE PRECISION TDEGK,PMB

*  Effective wavelength (microns) and default value
      DOUBLE PRECISION WL,WLDEF
      PARAMETER (WLDEF=0.5D0)

*  Flag showing that reduction in observed place is possible
      LOGICAL REDOP

*  Mean-to-apparent (1-21) and apparent-to-observed (22-35) parameters
      DOUBLE PRECISION PRMS(35)

*
*  Celestial position and epoch variable names are in the
*  form AABBC.
*
*    AA can be any of the following:
*
*     RA    right ascension, radians
*     DC    declination, radians
*     KQ    equinox qualifier:  'B' = Besselian,  'J' = Julian
*     EQ    epoch of mean equator & equinox, years
*     KP    epoch qualifier:  'B' or 'J'
*     EP    epoch of observation, years
*
*    BB can be any of the following:
*
*     PC    plate centre
*     RS    reference star (as given)
*     US    unknown star
*
*    C can be any of the following:
*
*     G     mean place as given
*     W     working coordinates
*     O     saved value of working coordinates before any fitting
*     R     results mean coordinates, given
*     X     for plate centre, values to be reported
*
      CHARACTER KQR,KPR,KQPCG,KPPCG,KQRSG,KPRSG,KQUSG
      DOUBLE PRECISION EQR,EPR,
*  Plate centre
     :                 RAPCG,DCPCG,EQPCG,EPPCG,
     :                 RAPCW,DCPCW,
     :                 RAPCO,DCPCO,
     :                 RAPCR,DCPCR,
     :                 RAPCX,DCPCX,
*  Reference star
     :                 RARSG,DCRSG,EQRSG,EPRSG,
     :                 RARSW,DCRSW,
     :                 RARSR,DCRSR,
*  Unknown star
     :                 RAUSG,DCUSG,EQUSG,
     :                 RAUSW,DCUSW,
     :                 RAUSR,DCUSR

*  Reference star parallax (arcsec)
      DOUBLE PRECISION PX

*
*  Fit
*
*  Coefficients
      DOUBLE PRECISION A1,A2,A3,B1,B2,B3,DD,
*  Plate centre and distortion coefficient
     :                 RCE,DCE,DISTE,
*  Design matrix and unknown vector
     :                 A(2,MAXREF,MAXTRM),B(2,MAXREF),
*  Workspace for SVD
     :                 WMAT(MAXTRM),VMAT(MAXTRM,MAXTRM),
     :                 WK(MAXTRM*MAXTRM),
*  Covariance matrix
     :                 CVM(MAXTRM,MAXTRM),
*  Results of fit
     :                 XVEC(MAXTRM),
*  Variances/sigmas in plate centre x,y,r and distortion
     :                 VARX,VARY,SIGR,SIGDI
*  Iteration counter and number of iterations
      INTEGER NIT,NITS,
*  Solution number of 2 best solutions, and the numbers of coefficients
     :        NBEST2,NBEST,NC1,NC2
*  Flags: adjust plate centre, adjust distortion, only 1 solution
      LOGICAL TWEAKP,TWEAKD,ONESOL
*  FITS file output: Are we to use fits output?  FITS file name, temp
*  string, then fitsio status, unit, temp integer
      LOGICAL FITSOP
      CHARACTER*(200) FITSFN
      CHARACTER*(100) FTWS
      INTEGER FTSTAT,FTUNIT,FTNAXS(2),FTI
*  Miscellaneous
      INTEGER I,J,N,NFLD,JZ
      DOUBLE PRECISION W1,W2,W3,PRG,PDG,PR,PD,ZD,XI,ETA,XM,YM,
     :                 SUMR2,FNORM,DX,DY,
     :                 R,D,DR,SCALE,XE,YE,XX,YY,XY,W,RR,XP,YP,
     :                 WMAX,WRATIO,SUMSQ,SUMSQO,DDI,SEP,X1,Y1,X2,Y2
      CHARACTER NMORSP*2
      LOGICAL ILLCNO

*  Functions
      DOUBLE PRECISION sla_EPJ,sla_EPB2D,sla_EPJ2D,sla_GMST,sla_EPB,
     :                 sla_EPCO,sla_DRANGE,sla_DRANRM,sla_DSEP






*
*  START OF MAIN LOOP - NEW INPUT SEQUENCE
*  ---------------------------------------

 10   CONTINUE

*  Heading
      WRITE (LUR,'(1H1,''* * * * * * * * * * * * *''/' //
     :             '1X,''*  A S T R O M E T R Y  *''/' //
     :             '1X,''* * * * * * * * * * * * *''///)')

*  Defaults - equinox for results J2000.0, Schmidt plate, etc
      KQR='J'
      EQR=2000D0
      KTEL='SCHM'
      KPROJ='Schmidt (by default)'
      DISTOR=-1D0/3D0
      GOTPEP=.FALSE.
      GOTLST=.FALSE.
      GOTUT=.FALSE.
      GOTLON=.FALSE.
      GOTLAT=.FALSE.
      GOTHM=.FALSE.
      GOTTDK=.FALSE.
      GOTPMB=.FALSE.
      WL=WLDEF

*  Flag new input record needed
      RECTYP=' '

*   Are we planning to write out FITS files with the WCS information?
      FITSOP = (FTPREF(1:1).NE.' ')

**************************
*  PRINCIPAL PARAMETERS  *
**************************

*------ READY FOR RESULTS EQUINOX RECORD ----------------------------

*  Next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  End?
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 998

*  Results equinox record?
      IF (RECTYP.EQ.'?'.AND.NFLD.EQ.1) THEN

*     Acknowledge receipt of the record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*     Process the record
         EQR=WORK(1)
         CALL EQEP(EQR,J)
         IF (J.NE.0) GO TO 990
         CALL sla_KBJ(J2(1),EQR,KQR,J)
         IF (J.NE.0) GO TO 990
      END IF

*------ READY FOR TELESCOPE TYPE RECORD -----------------------------

*  Next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  If end, premature
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 991

*  Telescope type record?
      IF (RECTYP.EQ.'?'.AND.NFLD.EQ.0) THEN

*     Acknowledge receipt of the record
         RECTYP=' '

*     If "approx" flag set, distortion coefficient to be fitted
         FITDI=APPROX

*     Telescope type
         KTEL=INBUF(:4)

*     Validate and determine distortion coefficient
         IF (KTEL.EQ.'ASTR') THEN
            KPROJ='Astrograph'
            DISTOR=0D0
         ELSE IF (KTEL.EQ.'SCHM') THEN
            KPROJ='Schmidt'
            DISTOR=-1D0/3D0
         ELSE IF (KTEL.EQ.'AAT2') THEN
            KPROJ='AAT prime focus with doublet corrector'
            DISTOR=+147.1D0
         ELSE IF (KTEL.EQ.'AAT3') THEN
            KPROJ='AAT prime focus with triplet corrector'
            DISTOR=+178.6D0
         ELSE IF (KTEL.EQ.'AAT8') THEN
            KPROJ='AAT f/8 with vacuum plateholder'
            DISTOR=+21.2D0
         ELSE IF (KTEL.EQ.'JKT8') THEN
            KPROJ='JKT f/8 Harmer-Wynne'
            DISTOR=+14.7D0
         ELSE IF (KTEL.EQ.'GENE') THEN
            I=MAX(1,INDEX(INBUF,' '))
            CALL sla_DFLTIN(INBUF,I,DISTOR,J)
            IF (J.GT.0) GO TO 990
            IF (DISTOR.LT.0D0) THEN
               WRITE (KPROJ,
     :                  '(''Barrel distortion, coeff ='',F10.4)') DISTOR
            ELSE IF (DISTOR.EQ.0D0) THEN
               KPROJ='Astrograph'
            ELSE
               WRITE (KPROJ,
     :           '(''Pincushion distortion, coeff ='',SP,F10.4)') DISTOR
            END IF
         ELSE
           GO TO 990
         END IF
      END IF

*------ READY FOR PLATE DATA RECORD ---------------------------------

*  Next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  If end, premature
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 991

*  Plate data record?  (Mandatory)
      IF (RECTYP.NE.'?'.OR.(NFLD.NE.7.AND.NFLD.NE.8)) GO TO 990

*  Acknowledge receipt of the record
      RECTYP=' '

*  If "approx" flag set, plate centre to be fitted
      FITPC=APPROX

*  Obtain plate centre RA,Dec
      CALL RADEC(WORK,J1,RAPCG,DCPCG,J)
      IF (J.NE.0) THEN
         WRITE (LUS,1005) INBUF
         IF (LUX.GT.0) WRITE (LUX, 1006) INBUF
 1005    FORMAT (1X,A/1X,'^^^^^^  POSSIBLE DATA ERROR?  ^^^^^^')
 1006    FORMAT ('ERROR 001 Data error? <',a,'>')
      ENDIF

*  Obtain plate equinox
      EQPCG=WORK(7)
      CALL EQEP(EQPCG,J)
      IF (J.NE.0) GO TO 990
      CALL sla_KBJ(J2(7),EQPCG,KQPCG,J)
      IF (J.NE.0) GO TO 990

*  Obtain plate epoch if supplied
      IF (NFLD.EQ.8) THEN
         EPPCG=WORK(8)
         CALL EQEP(EPPCG,J)
         IF (J.NE.0) GO TO 990
         CALL sla_KBJ(J2(8),EPPCG,KPPCG,J)
         IF (J.NE.0) GO TO 990
         GOTPEP=.TRUE.
      ELSE
         GOTPEP=.FALSE.
      END IF

**********************
*  OBSERVATION DATA  *
**********************

*  Flag LST, UT, long, lat, height not yet supplied
      GOTLST=.FALSE.
      GOTUT=.FALSE.
      GOTLON=.FALSE.
      GOTLAT=.FALSE.
      GOTHM=.FALSE.

*  Reset observatory name
      OBSNAM='?'

*  Loop until all observation data records have been input
 20   CONTINUE

*------ READY FOR DATE/TIME RECORD ----------------------------------

*  Next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  If end, premature
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 991

*  Date/time record?
      IF (RECTYP.EQ.'T') THEN

*     Acknowledge receipt of record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*     Julian epoch, UT date/time, or LST?
         IF (NFLD.EQ.1) THEN

*         Julian epoch, double
*         Replace any plate epoch
            EPPCG=WORK(1)
            CALL EQEP(EPPCG,J)
            IF (J.NE.0) GO TO 990
            CALL sla_KBJ(J2(1),EPPCG,KPPCG,J)
            IF (J.NE.0) GO TO 990
            GOTPEP=.TRUE.

         ELSE IF (NFLD.EQ.2) THEN

*        LST h,m
            W1=WORK(1)
            W2=WORK(2)
            IF (MOD(W1,1D0).NE.0D0.OR.
     :          W1.LT.0D0.OR.W1.GT.23D0.OR.
     :          W2.LT.0D0.OR.W2.GE.60D0) THEN
               WRITE (LUS,1005) INBUF
               IF (LUX.GT.0) WRITE (LUX, 1006) INBUF
            ENDIF
            STL=S2R*(60D0*(60D0*W1+W2))
            GOTLST=.TRUE.

         ELSE IF (NFLD.EQ.5) THEN

*        UT y,m,d,h,m
            W1=WORK(1)
            W2=WORK(2)
            W3=WORK(3)
            IF (MOD(W1,1D0).NE.0D0.OR.
     :          MOD(W2,1D0).NE.0D0.OR.
     :          MOD(W3,1D0).NE.0D0) THEN
               WRITE (LUS,1005) INBUF
               IF (LUX.GT.0) WRITE (LUX, 1006) INBUF
            ENDIF
            CALL sla_CALDJ(INT(W1),INT(W2),INT(W3),UTMJD,J)
            IF (J.NE.0) GO TO 990
            W1=WORK(4)
            W2=WORK(5)
            IF (MOD(W1,1D0).NE.0D0.OR.
     :          W1.LT.0D0.OR.W1.GT.23D0.OR.
     :          W2.LT.0D0.OR.W2.GE.60D0) THEN
               WRITE (LUS,1005) INBUF
               IF (LUX.GT.0) WRITE (LUX, 1006) INBUF
            ENDIF
            UTMJD=UTMJD+(60D0*W1+W2)/1440D0
            GOTUT=.TRUE.

*        Replace any plate epoch
            EPPCG=sla_EPJ(UTMJD)
            KPPCG='J'
            GOTPEP=.TRUE.
         ELSE
            GO TO 990
         END IF
      END IF

*------ READY FOR OBSERVATORY RECORD --------------------------------

*  Next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  If end, premature
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 991

*  Observatory record?
      IF (RECTYP.EQ.'O') THEN

*     Acknowledge receipt of record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*     Which option?
         IF (NFLD.EQ.0) THEN

*        Observatory ID
            I=INDEX(INBUF,' ')
            IF (I.LE.0.OR.I.GE.LINBUF) GO TO 990
            CALL sla_OBS(0,INBUF(I+1:),OBSNAM,W1,PHI,HM)
            IF (OBSNAM.EQ.'?') GO TO 990
            ELONG=sla_DRANGE(-W1)
            GOTLON=.TRUE.
            GOTLAT=.TRUE.
            GOTHM=.TRUE.

         ELSE IF (NFLD.EQ.2.OR.NFLD.EQ.3) THEN

*        Latitude [and height]
            W1=WORK(1)
            W2=WORK(2)
            IF (MOD(W1,1D0).NE.0D0.OR.
     :          W2.LT.0D0.OR.W2.GE.60D0) THEN
               WRITE (LUS,1005) INBUF
               IF (LUX.GT.0) WRITE (LUX, 1006) INBUF
            ENDIF
            W1=ABS(W1)+W2/60D0
            IF (W1.GT.90D0) GO TO 990
            IF (J1(1).LT.0) W1=-W1
            PHI=W1*D2R
            GOTLAT=.TRUE.
            IF (NFLD.EQ.3) THEN
               HM=WORK(3)
               GOTHM=.TRUE.
            END IF

         ELSE IF (NFLD.EQ.4.OR.NFLD.EQ.5) THEN

*        Longitude, latitude [and height]
            W1=WORK(1)
            W2=WORK(2)
            IF (MOD(W1,1D0).NE.0D0.OR.
     :          W2.LT.0D0.OR.W2.GE.60D0) THEN
               WRITE (LUS,1005) INBUF
               IF (LUX.GT.0) WRITE (LUX, 1006) INBUF
            ENDIF
            W1=ABS(W1)+W2/60D0
            IF (W1.GT.360D0) GO TO 990
            IF (J1(1).LT.0) W1=-W1
            ELONG=sla_DRANGE(W1*D2R)
            GOTLON=.TRUE.
            W1=WORK(3)
            W2=WORK(4)
            IF (MOD(W1,1D0).NE.0D0.OR.
     :          W2.LT.0D0.OR.W2.GE.60D0) THEN
               WRITE (LUS,1005) INBUF
               IF (LUX.GT.0) WRITE (LUX, 1006) INBUF
            ENDIF
            W1=ABS(W1)+W2/60D0
            IF (W1.GT.90D0) GO TO 990
            IF (J1(3).LT.0) W1=-W1
            PHI=W1*D2R
            GOTLAT=.TRUE.
            IF (NFLD.EQ.5) THEN
               HM=WORK(5)
               GOTHM=.TRUE.
            END IF
         ELSE
            GO TO 990
         END IF
      END IF

*------ READY FOR METEOROLOGICAL RECORD -----------------------------

*  Next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  If end, premature
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 991

*  Meteorological record?
      IF (RECTYP.EQ.'M') THEN

*     Acknowledge receipt of record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*     Must be 1 or 2 numeric fields
         IF (NFLD.NE.1.AND.NFLD.NE.2) GO TO 990

*     Pick up temperature
         TDEGK=WORK(1)
         IF (TDEGK.LT.240D0.OR.TDEGK.GT.330D0) GO TO 990
         GOTTDK=.TRUE.

*     Pick up pressure if present
         IF (NFLD.EQ.2) THEN
            PMB=WORK(2)
            IF (PMB.LT.0D0.OR.PMB.GT.1200D0) GO TO 990
            GOTPMB=.TRUE.
         END IF
      END IF

*  Loop until all observation data records have been processed
      IF (RECTYP.EQ.' '.OR.
     :    RECTYP.EQ.'T'.OR.RECTYP.EQ.'O'.OR.RECTYP.EQ.'M') GO TO 20

*  No more observation data;  now validate as a whole
      IF (GOTLST.OR.GOTUT.OR.GOTLON.OR.
     :    GOTLAT.OR.GOTHM.OR.GOTTDK.OR.GOTPMB) THEN

*     Default the temperature if absent
         IF (.NOT.GOTTDK) THEN
            TDEGK=278D0
            GOTTDK=.TRUE.
         END IF

*     Default or estimate the pressure if absent
         IF (.NOT.GOTPMB) THEN
            IF (GOTHM) THEN
               PMB=1013.25D0*EXP(-HM/8149.9415D0)
            ELSE
               PMB=800D0
            END IF
            GOTPMB=.TRUE.
         END IF

*     Estimate height, if absent, from pressure, if available
         IF (.NOT.GOTHM.AND.GOTPMB) THEN
            HM=-8149.9415D0*LOG(PMB/1013.25D0)
            GOTHM=.TRUE.
         END IF

*     Deal with case where LST rather than UT has been supplied
         IF (GOTLST) THEN

*        Express plate epoch as MJD
            IF (KPPCG.EQ.'B') THEN
               UTMJD=sla_EPB2D(EPPCG)
            ELSE
               UTMJD=sla_EPJ2D(EPPCG)
            END IF
            GOTUT=.TRUE.

*        Corresponding longitude
            W1=sla_DRANGE(STL-sla_GMST(UTMJD))

*        If longitude not supplied, use calculated value
            IF (.NOT.GOTLON) THEN
               ELONG=W1
               GOTLON=.TRUE.

*        If longitude supplied, adjust UT (and plate epoch) to match
            ELSE
               UTMJD=UTMJD+(W1-ELONG)*0.9972695663D0/D2R/360D0
               IF (KPPCG.EQ.'B') THEN
                  EPPCG=sla_EPB(UTMJD)
               ELSE
                  EPPCG=sla_EPJ(UTMJD)
               END IF
            END IF
         END IF

*     Decide if reduction in observed place is possible
         REDOP=GOTUT.AND.GOTLON.AND.GOTLAT.AND.GOTHM.AND.
     :         GOTTDK.AND.GOTPMB

*     Warnings if observation data were present but incomplete
         IF (.NOT.REDOP.AND.
     :       (GOTUT.OR.GOTLON.OR.GOTLAT.OR.GOTHM.OR.
     :        GOTTDK.OR.GOTPMB)) THEN
            WRITE (LUR,1007)
            WRITE (LUS,1007)
            IF (LUX.GT.0) WRITE (LUX, 1008)
 1007       FORMAT (/
     :       1X,'Observation data were incomplete and will be ignored'/)
 1008       FORMAT
     :       ('WARNING 014 Observation data were incomplete: ignored')
         END IF
      ELSE

*     No observation data - reduction can only be in mean place
         REDOP=.FALSE.
      END IF

*  Error if plate epoch still unavailable
      IF (.NOT.GOTPEP) GO TO 992

*  Results epoch: same as plate epoch
      KPR=KQR
      EPR=sla_EPCO(KQR,KPPCG,EPPCG)

*  Transform plate centre to coordinate system of results
      CALL COCOMM(RAPCG,DCPCG,1,0D0,0D0,0D0,KQPCG,EQPCG,KPPCG,EPPCG,
     :                                      KQR,EQR,KPR,EPR,RAPCR,DCPCR)

*  Report results equinox and telescope type
      WRITE (LUR,
     :    '(1X,''Equinox for mean coordinates of results:  '',A,F6.1/)')
     :                                                           KQR,EQR
      WRITE (LUR,'(1X,''Projection geometry:  '',A/)') KPROJ

*  Report plate data
      CALL sla_DR2TF(1,RAPCG,KSRA,IRAVEC)
      CALL sla_DR2AF(0,DCPCG,KSDC,IDCVEC)
      WRITE (LUR,
     :     '(1X,''Plate centre: '',3I3.2,''.'',I1,3X,A,I2.2,2I3.2,5X,'//
     :                      '''Equinox '',A,F6.1,5X,''Epoch '',A,F8.3)')
     :             IRAVEC,KSDC,(IDCVEC(N),N=1,3),KQPCG,EQPCG,KPPCG,EPPCG
      IF (KQPCG.NE.KQR.OR.EQPCG.NE.EQR) THEN
         CALL sla_DR2TF(1,RAPCR,KSRA,IRAVEC)
         CALL sla_DR2AF(0,DCPCR,KSDC,IDCVEC)
         WRITE (LUR,
     :     '(15X,3I3.2,''.'',I1,3X,A,I2.2,2I3.2,13X,A,F6.1,11X,A,F8.3)')
     :                     IRAVEC,KSDC,(IDCVEC(N),N=1,3),KQR,EQR,KPR,EPR
      END IF

*  Are we to perform the reduction in observed coordinates?
      IF (REDOP) THEN

*     Yes: compute mean-to-observed parameters
         CALL sla_MAPPA(2000D0,UTMJD,PRMS)
         CALL sla_AOPPA(UTMJD,0D0,ELONG,PHI,HM,0D0,0D0,
     :                             TDEGK,PMB,0.5D0,WL,0.0065D0,PRMS(22))

*     Transform plate centre to observed place
         CALL COCOMO(RAPCR,DCPCR,
     :                          KQR,EQR,KPR,EPR,0D0,PRMS,RAPCW,DCPCW,ZD)

*     Test for unacceptable ZD
         IF (ZD.LT.D90) THEN

*        OK: report observation data
            CALL sla_DJCL(UTMJD+SEC30,IUTY,IUTMO,IUTD,W1,J)
            I=INT(W1*1440D0)
            IUTH=I/60
            IUTM=MOD(I,60)
            I=NINT(sla_DRANRM(PRMS(35))*4D0/D2R)
            ISTH=I/60
            ISTM=MOD(I,60)
            IF (ELONG.LT.0D0) THEN
               KLON='W'
            ELSE IF (ELONG.GT.0D0) THEN
               KLON='E'
            ELSE
               KLON=' '
            END IF
            I=NINT(ABS(sla_DRANGE(ELONG)*600D0/D2R))
            ILOND=I/600
            ILONM=MOD(I,600)
            ILONT=MOD(ILONM,10)
            ILONM=ILONM/10
            IF (PHI.LT.0D0) THEN
               KPHI='-'
            ELSE
               KPHI='+'
            END IF
            I=NINT(ABS(sla_DRANGE(PHI)*600D0/D2R))
            IPHID=I/600
            IPHIM=MOD(I,600)
            IPHIT=MOD(IPHIM,10)
            IPHIM=IPHIM/10
            WRITE (LUR,
     :    '(/1X,''Observation data:    UT'',I6,2I3.2,I4.2,'':'',I2.2,'//
     :            '4X,''LST'',I4.2,'':'',I2.2,4X,''ZD'',F6.1,'' deg'')')
     :                        IUTY,IUTMO,IUTD,IUTH,IUTM,ISTH,ISTM,ZD/D2R
            IF (OBSNAM.NE.'?') WRITE (LUR,'(22X,A)') OBSNAM
            WRITE (LUR,
     :    '(22X,A,I3,I3.2,''.'',I1,4X,A,I2.2,I3.2,''.'',I1,I8,'' m''/'//
     :                                '1X,F26.1,'' deg K'',I7,'' mB'')')
     :           KLON,ILOND,ILONM,ILONT,KPHI,IPHID,IPHIM,IPHIT,NINT(HM),
     :                                                   TDEGK,NINT(PMB)

*        Set heading for colour column in reports
            NMORSP='nm'

         ELSE

*        Excessive ZD: issue warnings and revert to mean place reduction
            WRITE (LUS,'(1X,''For the given observation data,'//
     :                 ' the plate centre ZD is'',F6.1,'' degrees!''/'//
     :                '1X,''Reduction will be in MEAN place.'')') ZD/D2R
            IF (LUX.GT.0) WRITE (LUX,'(''WARNING 002 Plate centre ZD='',
     :           F6.1,'' Reduction in mean place'')') ZD/D2R
            REDOP=.FALSE.
         END IF
      END IF

*     Reduction in mean place?  (May have just changed)
      IF (.NOT.REDOP) THEN

*     Yes: transform plate centre to mean place of date
         CALL COCOMM(RAPCG,DCPCG,1,0D0,0D0,0D0,KQPCG,EQPCG,KPPCG,EPPCG,
     :                                  KPPCG,EPPCG,KPR,EPR,RAPCW,DCPCW)

*     Suppress heading for colour column in reports
         NMORSP=' '
      END IF

*********************
*  REFERENCE STARS  *
*********************

*  Heading for reference stars section of report
      WRITE (LUR,
     :     '(//1X,''Reference stars:''//16X,''n'',4X,A,8X,''RA'',12X,'//
     :    '''Dec'',9X,''pmR'',4X,''pmD'',3X,''Equinox'',3X,''Epoch'','//
     :                   '6X,''px'',9X,''Xmeas'',8X,''Ymeas''/)') NMORSP

*  Initialise counter and sum of x**2+y**2
      NREF=0
      SUMR2=0D0

*------ READY FOR COLOUR RECORD -------------------------------------

 70   CONTINUE

*  Get the next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  Colour?
      IF (RECTYP.EQ.'C') THEN

*     Acknowledge receipt of the record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*     There must be one numeric field
         IF (NFLD.NE.1) GO TO 990

*     Pick up and validate the wavelength
         W1=WORK(1)
         IF (W1.LT.200D0.OR.W1.GT.20000D0) GO TO 990

*     Convert nanometres to microns
         WL=W1/1000D0

*     Recompute mean-to-observed parameters if wanted
         IF (REDOP) THEN
            CALL sla_MAPPA(2000D0,UTMJD,PRMS)
            CALL sla_AOPPA(UTMJD,0D0,ELONG,PHI,HM,0D0,0D0,
     :                             TDEGK,PMB,0.5D0,WL,0.0065D0,PRMS(22))
         END IF
      END IF

*------ READY FOR REFERENCE STAR RA,DEC RECORD ----------------------

*  Get the next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  Reference star RA,Dec record?
      IF (RECTYP.EQ.'?'.AND.NFLD.GE.8.AND.NFLD.LE.11) THEN

*     Acknowledge the record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*     Decode and report anything suspicious
         CALL RADEC(WORK,J1,RARSG,DCRSG,J)
         IF (J.NE.0) THEN
            WRITE (LUS,1005) INBUF
            IF (LUX.GT.0) WRITE (LUX, 1006) INBUF
         ENDIF

*     Which of the two formats has been employed?
         IF (NFLD.NE.8) THEN

*        The usual format: proper motions, equinox, epoch, parallax

*        Proper motion in RA
            PRG=WORK(7)
            IF (ABS(PRG).GE.1D0) GO TO 990
            PR=PRG*S2R

*        Proper motion in Dec
            PDG=WORK(8)
            IF (ABS(PDG).GE.15D0) GO TO 990
            PD=PDG*AS2R

*        Equinox (year and type)
            EQRSG=WORK(9)
            CALL EQEP(EQRSG,J)
            IF (J.NE.0) GO TO 990
            CALL sla_KBJ(J2(9),EQRSG,KQRSG,J)
            IF (J.NE.0) GO TO 990

*        Epoch and/or parallax supplied?
            IF (NFLD.EQ.9) THEN

*           No: defaults
               KPRSG=KQRSG
               EPRSG=EQRSG
               PX=0D0
            ELSE

*           Epoch and/or parallax supplied; assume epoch for now
               EPRSG=WORK(10)
               CALL EQEP(EPRSG,J)
               IF (J.NE.0) GO TO 990

*           Both epoch and parallax supplied?
               IF (NFLD.EQ.10) THEN

*              No: just one or the other.  Valid epoch?
                  IF (J.EQ.0) THEN

*                 Yes: default the parallax and get epoch type
                     PX=0D0
                     CALL sla_KBJ(J2(10),EPRSG,KPRSG,J)
                     IF (J.NE.0) GO TO 990
                  ELSE

*                 Not a valid epoch; must be a parallax
                     PX=EPRSG

*                 Default the epoch
                     KPRSG=KQRSG
                     EPRSG=EQRSG
                  END IF
               ELSE

*              Both epoch and equinox supplied; valid epoch?
                  IF (J.NE.0) GO TO 990

*              Get epoch type
                  CALL sla_KBJ(J2(10),EPRSG,KPRSG,J)
                  IF (J.NE.0) GO TO 990

*              Get the parallax
                  PX=WORK(11)
               END IF

*           Validate the parallax
               IF (PX.LT.0D0.OR.PX.GE.1D0) GO TO 990
            END IF

*        Reset proper motions not supplied flag
            JZ=0
         ELSE

*        The no proper motions format: obtain equinox, epoch
            EQRSG=WORK(7)
            CALL EQEP(EQRSG,J)
            IF (J.NE.0) GO TO 990
            CALL sla_KBJ(J2(7),EQRSG,KQRSG,J)
            IF (J.NE.0) GO TO 990
            EPRSG=WORK(8)
            CALL EQEP(EPRSG,J)
            IF (J.NE.0) GO TO 990
            CALL sla_KBJ(J2(8),EPRSG,KPRSG,J)
            IF (J.NE.0) GO TO 990

*        Set the no proper motions flag and zero the proper motions etc
            JZ=1
            PR=0D0
            PD=0D0
            PX=0D0
         END IF

*     Transform reference star position to coordinate system of results
         CALL COCOMM(RARSG,DCRSG,JZ,PR,PD,PX,KQRSG,EQRSG,KPRSG,EPRSG,
     :                                      KQR,EQR,KPR,EPR,RARSR,DCRSR)

*     Reduction in observed or mean place?
         IF (REDOP) THEN

*        Observed: transform reference star mean place to observed
            CALL COCOMO(RARSR,DCRSR,
     :                           KQR,EQR,KPR,EPR,PX,PRMS,RARSW,DCRSW,ZD)
         ELSE

*        Mean: transform reference star to mean place of date
            CALL COCOMM(RARSR,DCRSR,1,0D0,0D0,PX,KQR,EQR,KPR,EPR,
     :                                  KPPCG,EPPCG,KPR,EPR,RARSW,DCRSW)
         END IF

*     Increment reference star counter
         NREF=NREF+1
         IF (NREF.GT.MAXREF) GO TO 993

*     Store reference star data in arrays
         REFRD(1,NREF)=RARSR
         REFRD(2,NREF)=DCRSR
         REFRDW(1,NREF)=RARSW
         REFRDW(2,NREF)=DCRSW
         RNAME(NREF)=NAME
         REFWL(NREF)=WL

*------ NEXT RECORD HAS TO BE REFERENCE STAR X,Y --------------------

*     Get the next input record
         CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*     Premature end of data?
         IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 991

*     Must be reference star x,y record
         IF (RECTYP.NE.'?'.OR.NFLD.NE.2) GO TO 990

*     Acknowledge the record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*     Pick up and store the measured x,y
         XM=WORK(1)
         YM=WORK(2)
         REFXYM(1,NREF)=XM
         REFXYM(2,NREF)=YM

*     Accumulate R**2
         SUMR2=SUMR2+XM*XM+YM*YM

*     Compute tangential coordinates (Xi,Eta)
         CALL sla_DS2TP(RARSW,DCRSW,RAPCW,DCPCW,XI,ETA,J)

*     Adjust for telescope type giving expected plate coordinates X,Y
         XE=XI
         YE=ETA
         CALL sla_PCD(DISTOR,XE,YE)

*     Store
         REFXYE(1,NREF)=XE
         REFXYE(2,NREF)=YE

*     Report reference star data (as given)
         IF (REDOP) THEN
            WRITE (NANOM,'(I6)') NINT(REFWL(NREF)*1D3)
         ELSE
            NANOM=' '
         END IF
         CALL sla_DR2TF(3,RARSG,KSR1,IRVEC1)
         CALL sla_DR2AF(2,DCRSG,KSD1,IDVEC1)
         IF (JZ.EQ.0) THEN
            WRITE (PMREP,'(SP,F9.4,F7.3)') PRG,PDG
         ELSE
            PMREP=' '
         END IF
         WRITE (LUR,
     :         '(1X,A,I6,A,2X,3I3.2,''.'',I3.3,2X,A,I2.2,2I3.2,''.'','//
     :                     'I2.2,A,2X,A,F6.1,2X,A,F8.3,F7.3,SP,2F13.3)')
     :                  RNAME(NREF),NREF,NANOM,IRVEC1,KSD1,IDVEC1,PMREP,
     :                                  KQRSG,EQRSG,KPRSG,EPRSG,PX,XM,YM
      END IF

*  Loop unless all reference stars have been processed
      IF (RECTYP.EQ.' ') GO TO 70

*  Abort if too few reference stars
      IF (NREF.LT.2) GO TO 991

***************
*  SOLUTIONS  *
***************

*  Don't solve if current record is bad
      IF (RECTYP.NE.'/'.AND.RECTYP.NE.'E'.AND.
     :    RECTYP.NE.'?'.AND.NFLD.NE.2.AND.NFLD.NE.7) GO TO 990

*  Normalize measured x,y data
      IF (SUMR2.NE.0D0) THEN
         FNORM=SQRT(SUMR2/DBLE(NREF))
      ELSE
         FNORM=1D0
      END IF
      DO J=1,NREF
         DO I=1,2
            REFXYM(I,J)=REFXYM(I,J)/FNORM
         END DO
      END DO

*  Save the current distortion coefficient and plate centre
      DISTO=DISTOR
      RAPCO=RAPCW
      DCPCO=DCPCW

*  Initialise "best solution so far"
      NBEST=0

*  Initialise "terms used so far"
      NTERMS=0

*  Initialise number of coefficients
      NC=4

*  Initialise "ill-conditioned"
      ILLCON=.FALSE.

*  Decide how many solutions to attempt
      IF ((FITDI.OR.FITPC).AND.NREF.GE.10) THEN
         EXTRAFITS(1)=0
         EXTRAFITS(2)=0
         IF (FITDI.AND.FITPC) THEN
            NSOLS=5
            EXTRAFITS(3)=7
            EXTRAFITS(4)=8
            EXTRAFITS(5)=9
         ELSE
            NSOLS=3
            IF (FITDI) THEN
               EXTRAFITS(3)=7
            ELSE
               EXTRAFITS(3)=8
            ENDIF
            EXTRAFITS(4)=0
            EXTRAFITS(5)=0
         ENDIF
*      Following settings redundant
         FITDI=.FALSE.
         FITPC=.FALSE.
      ELSE IF (NREF.GE.3) THEN
         NSOLS=2
      ELSE
         NSOLS=1
      END IF

      IF (NSOLS.GT.MAXSOL) THEN
*      Ooops -- this isn't right
         WRITE (LUR, '("Can''t happen!  NSOLS=",I3/"ABORTING")') NSOLS
         WRITE (LUS, '("Can''t happen!  NSOLS=",I3/"ABORTING")') NSOLS
         GO TO 997
      ENDIF

*  Solutions (4 unflipped, 4 flipped, 6, 6+)
      DO NSOL=0,NSOLS

*     The round with NSOL=0 produces no output
         IF (LUX.GT.0 .AND. NSOL.GT.0) WRITE (LUX, '("FIT ",I3)') NSOL

*     Provisionally assume the fit is OK
         FITOK=.TRUE.

*     The plate centre values to be reported (in LUX) are the given ones,
*     unless we fit the plate centre successfully.
         RAPCX=RAPCG
         DCPCX=DCPCG

*     Set up number and initial values of the coefficients
*     and the number of iterations
         IF (NSOL.LE.1) THEN
            IF (NSOL.EQ.0) THEN
               RCE=RAPCW
               DCE=DCPCW
               DISTE=DISTOR
               NITS=2
            END IF
            A1=0D0
            A2=0D0
            A3=0D0
            B1=0D0
            B2=0D0
            B3=0D0
         ELSE IF (NSOL.EQ.2) THEN
            NC=6
         ELSE
*         3 <= NSOL,NSOLS <= MAXSOL
            NITS=20
*         XXX Should I reset both DISTE and PC to the same (initial)
*         values each extra time round here?
            DISTE=DISTOR
*         Which combination of non-linear fits are we attempting this time?
            IF (EXTRAFITS(NSOL).EQ.7) THEN
               FITDI=.TRUE.
               FITPC=.FALSE.
            ELSE IF (EXTRAFITS(NSOL).EQ.8) THEN
               FITDI=.FALSE.
               FITPC=.TRUE.
            ELSE IF (EXTRAFITS(NSOL).EQ.9) THEN
               FITDI=.TRUE.
               FITPC=.TRUE.
            ELSE
*            Ooops -- EXTRAFITS should only have values 7, 8, 9
               WRITE (LUR, '("Can''t happen!  EXTRAFITS(",I1,")=",I3
     :              /"ABORTING")') NSOL, EXTRAFITS(NSOL)
               WRITE (LUS, '("Can''t happen!  EXTRAFITS(",I1,")=",I3
     :              /"ABORTING")') NSOL, EXTRAFITS(NSOL)
               GO TO 997
            ENDIF
         END IF

*     Iterate
         DO NIT=1,NITS

*        If 6+ coefficient solution, decide what to fit this time,
*        according to the following schedule:
*
*                          What fitting has been requested:
*           Iteration      -- P only --- D only --- both --
*
*               1               L+P        L+D       L+P
*               2                L          L         L
*               3               L+P        L+D       L+D
*               4                L          L         L
*               5               L+P        L+D       L+P
*               6                L          L         L
*               7               L+P        L+D       L+D
*               8                L          L         L
*      then   odds              L+P        L+D      L+P+D
*             evens              L          L         L
*
*                                L = linear fit only
*                              L+P = linear and plate centre
*                              L+D = linear and distortion
*                            L+P+D = everything
*
*        The final iteration is even-numbered and fits the linear
*        model only.  On the penultimate iteration, the covariance
*        matrix is calculated in order to estimate the accuracy of
*        the nonlinear terms.

            IF (NSOL.EQ.0) THEN
               TWEAKP=.FALSE.
               TWEAKD=.FALSE.
            ELSE IF (NSOL.GT.2) THEN
               IF (NIT.EQ.1.OR.NIT.EQ.5) THEN
                  IF (FITPC) THEN
                     TWEAKP=.TRUE.
                  ELSE
                     TWEAKD=FITDI
                  END IF
               ELSE IF (NIT.EQ.3.OR.NIT.EQ.7) THEN
                  IF (FITDI) THEN
                     TWEAKD=.TRUE.
                  ELSE
                     TWEAKP=FITPC
                  END IF
               ELSE IF (MOD(NIT,2).EQ.1) THEN
                  TWEAKP=FITPC
                  TWEAKD=FITDI
               ELSE
                  TWEAKP=.FALSE.
                  TWEAKD=.FALSE.
               END IF
            END IF

*        Process each reference star in turn
            DO N=1,NREF

*           RA,Dec to Xi,Eta
               CALL sla_DS2TP(REFRDW(1,N),REFRDW(2,N),RCE,DCE,XI,ETA,J)
               IF (J.NE.0) THEN
                  WRITE (FITERR,'(''sla_DS2TP status'',I3)') J
                  GO TO 994
               END IF

*           Apply radial distortion giving expected x,y
               XE=XI
               YE=ETA
               CALL sla_PCD(DISTE,XE,YE)

*           Get the measured x,y
               XM=REFXYM(1,N)
               YM=REFXYM(2,N)

*           Apply the linear part of the model giving predicted x,y
               XP=A1+A2*XM+A3*YM
               YP=B1+B2*XM+B3*YM

*           Build the design matrix
               IF (NSOL.EQ.0) THEN
                  A(1,N,1)=1D0
                  A(2,N,1)=0D0
                  A(1,N,2)=XM
                  A(2,N,2)=YM
                  A(1,N,3)=YM
                  A(2,N,3)=-XM
                  A(1,N,4)=0D0
                  A(2,N,4)=1D0
               ELSE IF (NSOL.EQ.1) THEN
                  A(1,N,1)=1D0
                  A(2,N,1)=0D0
                  A(1,N,2)=XM
                  A(2,N,2)=-YM
                  A(1,N,3)=YM
                  A(2,N,3)=XM
                  A(1,N,4)=0D0
                  A(2,N,4)=1D0
               ELSE
                  A(1,N,1)=1D0
                  A(2,N,1)=0D0
                  A(1,N,2)=XM
                  A(2,N,2)=0D0
                  A(1,N,3)=YM
                  A(2,N,3)=0D0
                  A(1,N,4)=0D0
                  A(2,N,4)=1D0
                  A(1,N,5)=0D0
                  A(2,N,5)=XM
                  A(1,N,6)=0D0
                  A(2,N,6)=YM
                  IF (NSOL.GT.2) THEN
                     NC=6
                     XX=XP*XP
                     YY=YP*YP
                     XY=XP*YP
                     IF (TWEAKP) THEN
                        NC=NC+1
                        W=XY+DISTE*2D0*XY
                        A(1,N,NC)=XX+DISTE*(3D0*XX+YY)
                        A(2,N,NC)=W
                        NC=NC+1
                        A(1,N,NC)=W
                        A(2,N,NC)=YY+DISTE*(XX+3D0*YY)
                     END IF
                     IF (TWEAKD) THEN
                        NC=NC+1
                        RR=XX+YY
                        A(1,N,NC)=-RR*XP
                        A(2,N,NC)=-RR*YP
                     END IF
                  END IF
               END IF

*           Build the error vector
               B(1,N)=XE-XP
               B(2,N)=YE-YP

*           Remember the number of terms for reports
               NTERMS=MAX(NTERMS,NC)
            END DO

*        Factorise the design matrix
            CALL sla_SVD(2*NREF,NC,2*MAXREF,MAXTRM,A,WMAT,VMAT,WK,J)
            IF (J.LT.0) THEN
               WRITE (FITERR,'(''Impossible sla_SVD error!'')')
               GO TO 994
            ELSE IF (J.GT.0) THEN
               WRITE (LUR,1050) J
               WRITE (LUS,1050) J
               IF (LUX.GT.0) WRITE (LUX, 1051) J
 1050          FORMAT (/1X,'sla_SVD warning',I3)
 1051          FORMAT
     :            ('WARNING 003 sla_SVD warning (probably harmless)',I3)
            END IF

*        Edit singular values of W matrix
            WMAX=0D0
            DO I=1,NC
               WMAX=MAX(WMAX,WMAT(I))
            END DO
            IF (WMAX.GT.0D0) THEN
               DO I=1,NC
                  WRATIO=WMAT(I)/WMAX
                  IF (WRATIO.LT.1D-10) THEN
                     WMAT(I)=0D0
*                 Fatal if editing occurs during final two iterations
                     IF (NIT.GE.NITS-1) THEN
                        ILLCON=.TRUE.
                        FITOK=.FALSE.
                     END IF
                  END IF
               END DO
            END IF

*        Solve
            CALL sla_SVDSOL(2*NREF,NC,2*MAXREF,MAXTRM,
     :                                            B,A,WMAT,VMAT,WK,XVEC)

*        Tweak the coefficients
            A1=A1+XVEC(1)
            A2=A2+XVEC(2)
            A3=A3+XVEC(3)
            B1=B1+XVEC(4)
            IF (NSOL.EQ.0) THEN
               B2=-A3
               B3=A2
            ELSE IF (NSOL.EQ.1) THEN
               B2=A3
               B3=-A2
            ELSE
               B2=B2+XVEC(5)
               B3=B3+XVEC(6)
               IF (TWEAKP) THEN
                  DX=XVEC(7)
                  DY=XVEC(8)
                  CALL sla_DTP2S(DX,DY,RCE,DCE,R,D)
                  RCE=R
                  DCE=D
                  A1=A1-DX
                  B1=B1-DY
               END IF
               IF (TWEAKD) THEN
                  DD=XVEC(NC)
                  DISTE=DISTE+DD
               END IF
            END IF

*        If penultimate iteration for the 6+ coefficient model,
*        calculate the covariance matrix
            IF (NSOL.GT.2.AND.NIT.EQ.NITS-1)
     :                CALL sla_SVDCOV(NC,MAXTRM,MAXTRM,WMAT,VMAT,WK,CVM)

*        Iterate
         END DO

*     Save the linear part of the solution.  The unflipped and
*     flipped 4-coefficient solutions are stored in the first and
*     second PLTCON slots respectively.  The best of these two is
*     chosen and left in the first PLTCON slot.  The 6 and 6+
*     solutions are then stored in the second and third PLTCON
*     slots respectively.

         IF (NSOL.EQ.0) THEN
            N=1
         ELSE IF (NSOL.EQ.1) THEN
            N=2
         ELSE
            N=NSOL
         END IF
         PLTCON(1,N)=A1
         PLTCON(2,N)=A2
         PLTCON(3,N)=A3
         PLTCON(4,N)=B1
         IF (NSOL.EQ.0) THEN
            PLTCON(5,N)=-A3
            PLTCON(6,N)=A2
         ELSE IF (NSOL.EQ.1) THEN
            PLTCON(5,N)=A3
            PLTCON(6,N)=-A2
         ELSE
            PLTCON(5,N)=B2
            PLTCON(6,N)=B3
         END IF

*     Final pass to get expected coordinates and statistics
         SUMSQ=0D0
         DO N=1,NREF
            CALL sla_DS2TP(REFRDW(1,N),REFRDW(2,N),RCE,DCE,XI,ETA,J)
            XE=XI
            YE=ETA
            CALL sla_PCD(DISTE,XE,YE)
            REFXYE(1,N)=XE
            REFXYE(2,N)=YE
            XM=REFXYM(1,N)
            YM=REFXYM(2,N)
            XP=A1+A2*XM+A3*YM
            YP=B1+B2*XM+B3*YM
            REFXYP(1,N)=XP
            REFXYP(2,N)=YP
            DX=XE-XP
            DY=YE-YP
            SUMSQ=SUMSQ+DX*DX+DY*DY
         END DO

*     If unflipped 4-coefficient solution, save the details
         IF (NSOL.EQ.0) THEN
            SUMSQO=SUMSQ
            ILLCNO=ILLCON

*     If flipped 4-coefficient solution, decide whether better
         ELSE IF (NSOL.EQ.1) THEN
            IF ((SUMSQO.LT.SUMSQ.OR.NREF.EQ.2).AND..NOT.ILLCNO) THEN

*           Unflipped is best
               A1=PLTCON(1,1)
               A2=PLTCON(2,1)
               A3=PLTCON(3,1)
               B1=PLTCON(4,1)
               B2=PLTCON(5,1)
               B3=PLTCON(6,1)
               SUMSQ=SUMSQO
               ILLCON=ILLCNO
               NBEST=1
            ELSE

*           Flipped is best
               PLTCON(1,1)=A1
               PLTCON(2,1)=A2
               PLTCON(3,1)=A3
               PLTCON(4,1)=B1
               PLTCON(5,1)=B2
               PLTCON(6,1)=B3
            END IF

*     If 6+ coefficient solution, get variances and standard deviations
         ELSE IF (NSOL.GT.2) THEN
            W=DBLE(2*NREF-NTERMS)/SUMSQ
            IF (FITDI) SIGDI=SQRT(CVM(NTERMS,NTERMS)/W)
            IF (FITPC) THEN
               W=W*AS2R*AS2R
               VARX=CVM(7,7)/W
               VARY=CVM(8,8)/W
               SIGR=SQRT(VARX+VARY)
            END IF
         END IF

*
*     Report solution
*     ---------------

         IF (NSOL.GE.1) THEN
            WRITE (LUS,'(/)')
            WRITE (LUR,'(1H1,''Plate solution:'',I2,''-coefficient''/'//
     :                  '1X,''-----------------------------''/)') NTERMS

*        Was final iteration ill-conditioned?
            IF (ILLCON) THEN
               WRITE (LUR,1055)
               WRITE (LUS,1055)
               IF (LUX.GT.0) WRITE (LUX, 1056)
 1055          FORMAT (1X,'Fit was ill-conditioned!'/)
 1056          FORMAT ('WARNING 004 Fit was ill-conditioned')
               FITOK=.FALSE.
            ELSE

*           OK so far; extra checks and reports for 6+ coefficient fit
               IF (NSOL.GT.2) THEN

*              Report distortion coefficient and update if OK
                  IF (FITDI) THEN
                     DDI=DISTE-DISTOR
                     IF (ABS(DDI).LT.1000D0.AND.SIGDI.LT.100D0) THEN
                        WRITE (LUR,1060) DDI,DISTE,SIGDI
                        WRITE (LUS,1060) DDI,DISTE,SIGDI
 1060                   FORMAT (1X,'Radial distortion has changed by',
     :                     SP,F8.2,' to',F8.2,'  (std dev',SS,F6.2,')'/)
                        IF (LUX.GT.0) THEN
                           WRITE (LUX,
     :                       '("INFO 005 Radial distortion changed by ",
     :                          F8.2," to",F8.2,
     :                          "  (std dev",SS,F6.2,")")')
     :                          DDI,DISTE,SIGDI
*                        Write out the distortion coefficient DISTE
*                        in the units ASTROM uses (rad^{-2}) rather than the
*                        units we carefully converted it to above (deg^{-2}),
*                        since this will principally be used to control a
*                        future invocation of ASTROM.
                           WRITE (LUX, 1081) "q", DISTE,
     :                          "distortion, q (rad^{-2})"
                        ENDIF
                        DISTOR=DISTE
                     ELSE
                        WRITE (LUR,1062)
                        WRITE (LUS,1062)
                        IF (LUX.GT.0) WRITE (LUX, 1063)
 1062                   FORMAT (1X,'Radial distortion coefficient ',
     :                                'cannot reliably be determined!'/)
 1063                   FORMAT ('WARNING 006 Radial distortion ',
     :                       'cannot be reliably determined!')
                        FITOK=.FALSE.
                     END IF
                  ELSE
*                  If not FITDI, write q to the log anyway, so as to
*                  propagate the value to the output.
                     WRITE (LUX, 1081) "q", DISTE,
     :                    "distortion, q (rad^{-2})"
                  END IF

*              Report plate centre and update if OK
                  IF (FITPC) THEN
                     IF (SIGR.LT.1D4) THEN
                        IF (REDOP) THEN
                           CALL COCOOM(RCE,DCE,PRMS,KQPCG,EQPCG,
     :                                                  KPPCG,EPPCG,R,D)
                        ELSE
                           CALL COCOMM(RCE,DCE,1,0D0,0D0,0D0,
     :                  KPPCG,EPPCG,KPR,EPR,KQPCG,EQPCG,KPPCG,EPPCG,R,D)
                        END IF
                        SEP=sla_DSEP(R,D,RAPCG,DCPCG)/AS2R
                        IF (SEP.LT.9999.9D0) THEN
                           CALL sla_DR2TF(1,RAPCG,KSRA,IRAVEC)
                           CALL sla_DR2AF(0,DCPCG,KSDC,IDCVEC)
                           WRITE (LUS,
     :                       '(1X,''Plate centre has moved by'',F7.1,'//
     :             ''' arcsec  (std dev'',F6.1,'' arcsec)''/)') SEP,SIGR
                           IF (LUX.GT.0) WRITE (LUX,
     :                          '(''INFO 007 Plate centre has moved by'',
     :                          F7.1,'//
     :                          ''' arcsec  (std dev'',
     :                          F6.1,'' arcsec)'')') SEP,SIGR
                           WRITE (LUR,
     :      '(1X,''Plate centre has moved from'',I6.2,2I3.2,''.'',I1,'//
     :   '3X,A,I2.2,2I3.2,5X,''Equinox '',A,F6.1,5X,''Epoch '',A,F8.3)')
     :             IRAVEC,KSDC,(IDCVEC(N),N=1,3),KQPCG,EQPCG,KPPCG,EPPCG
                           CALL sla_DR2TF(1,R,KSRA,IRAVEC)
                           CALL sla_DR2AF(0,D,KSDC,IDCVEC)
                           WRITE (LUR,
     :           '(26X,''to'',I6.2,2I3.2,''.'',I1,3X,A,I2.2,2I3.2,3X,'//
     :       '''(std dev'',F6.1,'' arcsec EW,'',F6.1,'' arcsec NS)''/)')
     :                          IRAVEC,KSDC,(IDCVEC(N),N=1,3),
     :                          SQRT(VARX),SQRT(VARY)
*                        Save these coordinates as the ones to be reported,
*                        both to the log file, LUX, and to the FITS-WCS file.
                           RAPCX=R
                           DCPCX=D
*                        Save working coordinates
                           RAPCW=RCE
                           DCPCW=DCE
                           PCOK=.TRUE.
                        ELSE
                           PCOK=.FALSE.
                        END IF
                     ELSE
                        PCOK=.FALSE.
                     END IF
                     IF (.NOT.PCOK) THEN
                        WRITE (LUR,1068)
                        WRITE (LUS,1068)
                        IF (LUX.GT.0) WRITE (LUX,1069)
 1068                   FORMAT (1X,
     :                   'Plate centre cannot reliably be determined!'/)
 1069                   FORMAT ('WARNING 008 Plate centre cannot ',
     :                       'reliably be determined!')
                        FITOK=.FALSE.
                     END IF
                  END IF
               END IF
            END IF

*        Has the fit worked?
            IF (FITOK) THEN

*           Yes: the current solution is the "best" so far
               NBEST=NSOL

*           Report the forwards solution
               WRITE (LUR,
     :          '(5X,''X,Y = expected plate coordinates (radians)''//'//
     :                       '5X,SP,''X = '',G15.7,28X,''Y = '',G15.7)')
     :                              PLTCON(1,NSOL),PLTCON(BVALS+1,NSOL)
               WRITE (LUR,
     :             '(9X,sp,G15.7,'' * Xmeas'',24X,G15.7,'' * Xmeas''/'//
     :                '9X,G15.7,'' * Ymeas'',24X,G15.7,'' * Ymeas''//)')
     :          (PLTCON(I,NSOL)/FNORM,PLTCON(BVALS+I,NSOL)/FNORM,I=2,3)
*            Write out a block of results (terminated by `ENDFIT' below)
*            corresponding to one successful fit.
               IF (LUX.GT.0) THEN
                  IF (NSOL.GT.2) THEN
                     IF (FITDI) THEN
                        WRITE (LUX, 1081) "deltaq", DDI,
     :                       "change in radial distortion, arcsec"
                        WRITE (LUX, 1081) "deltaqsd", SIGDI, "s.d."
                     ENDIF
                     IF (FITPC) THEN
                        WRITE (LUX, 1081) "deltapc", SEP,
     :                       "change in plate centre, arcsec"
                        WRITE (LUX, 1081) "deltapcsd", SIGR, "s.d."
                     ENDIF
                  ENDIF
               ENDIF

*           Report the backwards solution
               CALL sla_INVF(PLTCON(1,NSOL),PLTCON(1,NSOL+MAXSOL),J)
               IF (J.NE.0) THEN
                  WRITE (FITERR,'(''sla_INVF status'',I3)') J
                  GO TO 994
               END IF
               WRITE (LUR,
     :             '(1X,SP,''Xmeas = '',G15.7,24X,''Ymeas = '',G15.7/'//
     :                         '9X,G15.7,'' * X'',28X,G15.7,'' * X''/'//
     :                        '9X,G15.7,'' * Y'',28X,G15.7,'' * Y''//)')
     :              (FNORM*PLTCON(I,NSOL+MAXSOL),
     :               FNORM*PLTCON(BVALS+I,NSOL+MAXSOL),I=1,3)

*           Plate scale(s), nonperpendicularity and orientation
               CALL sla_DCMPF(PLTCON(1,NSOL),
     :                            XZERO,YZERO,XSCALE,YSCALE,PERP,ORIENT)
               XSCALE=(XSCALE/FNORM)/AS2R
               YSCALE=(YSCALE/FNORM)/AS2R
               SCALE=SQRT((XSCALE*XSCALE+YSCALE*YSCALE)/2D0)
               IF (NSOL.EQ.1) THEN
                  WRITE (LUR,
     :   '(6X,''Plate scale (in measuring units):'',F18.4,'' arcsec'')')
     :                                                             SCALE
               ELSE
                  WRITE (LUR,
     :                   '(5X,''Plate scales (in measuring units):'','//
     :                                   '6X,''X'',F11.4,'' arcsec''/'//
     :                                  '45X,''Y'',F11.4,'' arcsec''/'//
     :                               '42X,''mean'',F11.4,'' arcsec''/'//
     :               '19X,''Nonperpendicularity:'',SP,F17.3,''  deg'')')
     :                            ABS(XSCALE),ABS(YSCALE),SCALE,PERP/D2R
               END IF
               IF (XSCALE*YSCALE.GE.0D0) THEN
                  WRITE (LUR,
     :         '(27X,''Orientation:'',SP,F17.3,''  deg''//)') ORIENT/D2R
               ELSE
                  WRITE (LUR,'(27X,''Orientation:'',SP,F17.3,'//
     :                 '''  deg and laterally inverted''//)') ORIENT/D2R
               END IF
               WRITE (LUR,
     :                 '(1X,''Reference stars:''/29X,''Mean RA,Dec'','//
     :              '9X,''Equinox'',1X,A,F6.1,6X,''Epoch'',1X,A,F8.3,'//
     :                                   '11X,''Residuals (arcsec)''/'//
     :          '16X,''n'',4X,A,14X,''catalogue'',21X,''calculated'','//
     :        '17X,''dX'',8X,''dY'',8X,''dR''/)') KQR,EQR,KPR,EPR,NMORSP

               IF (FITSOP) THEN
*               Write a FITS-WCS file.  See Calabretta and Greisen (C&G),
*               `Representations of Celestial Coordinates in FITS'
*               (still draft, but due to appear in A&A).
                  FTSTAT = 0

*               First, generate a filename from FTPREF and solution number NSOL
*               Trim blanks from FTPREF (TRIM() isn't implemented)
                  FTI = INDEX (FTPREF, ' ')
                  IF (FTI.EQ.0 .OR. FTI.GT.(LEN(FTPREF)-1) .OR.
     :                 FTI.GT.(LEN(FITSFN)-8)) THEN
*                  We're in trouble
                     WRITE (LUS, '(" FITS Filename <",a,
     :                  "> too long.  FITS writing abandoned")') FTPREF
                     IF (LUX.GT.0)
     :                    WRITE (LUX, '("ERROR 010 FITS filename <",a,
     :                    "> too long.  FITS writing abandoned")')
     :                    FTPREF
*                  Set FITSOP false to avoid coming this way again
                     FITSOP = .FALSE.
                     GOTO 1070
                  ENDIF
                  IF (FTI.EQ.1) THEN
*                  Nothing there at all!?
                     WRITE (LUS,
     :             '(" FITS filename blank!?  FITS writing abandoned")')
                     IF (LUX.GT.0) WRITE (LUX,
     :           '("ERROR 011 FITS filename blank. Writing abandoned")')
                     FITSOP = .FALSE.
                     GOTO 1070
                  ENDIF
                  FTI=FTI-1
                  WRITE (FITSFN, '(A,i2.2,".fits")') FTPREF(:FTI), NSOL
                  FTI = INDEX (FITSFN, ' ')-1

*               Right, now try opening the file, use a small, but, non-trivial
*               size so it can be opened by viewing programs.
                  CALL FTGIOU (FTUNIT, FTSTAT)
                  CALL FTINIT (FTUNIT, FITSFN, 1, FTSTAT)
                  WRITE (LUS, '(" FITS-WCS header for",I3,
     :                 "-component solution in file ",
     :                 A)') NTERMS, FITSFN(:FTI)
                  FTNAXS(1)=5
                  FTNAXS(2)=5

*               Write required keywords
                  CALL FTPHPR (FTUNIT, .TRUE., 8, 2, FTNAXS, 0,
     :                 1, .FALSE., FTSTAT)

*               Confess who wrote it
                  CALL FTPHIS (FTUNIT, 'Written by ASTROM', FTSTAT)

*               Write current date (should use FTGSTM, but that's not
*               available in older versions of FITSIO)
                  CALL FTGSDT (IUTD,IUTMO,IUTY, FTSTAT)
                  WRITE (FTWS, '(I4,"-",I2.2,"-",I2.2)')
     :                 IUTY, IUTMO, IUTD
                  CALL FTPKYS (FTUNIT, 'DATE', FTWS,
     :                 'Date file was written', FTSTAT)

                  WRITE (FTWS, '("ASTROM astrometric solution: ",i2,
     :                 "-component solution")') NTERMS
                  CALL FTPCOM (FTUNIT, FTWS, FTSTAT)

*               Write a comment, summarising the transformation matrix.
*               Reuse the results of the sla_DCMPF call above.
                  WRITE (FTWS,'("ASTROM scale=",F6.2,
     :                 "as/px, nonperp=",
     :                 F6.2,", angle=",F6.2,", inv=",L1)')
     :                 SCALE, PERP/D2R, ORIENT/D2R,
     :                 (XSCALE*YSCALE.LT.0D0)
                  CALL FTPCOM (FTUNIT, FTWS, FTSTAT)

*               Write the WCS headers out in the style requested in FITSWCSSTYLE
                  I=1
                  DO WHILE(FITSWCSSTYLE(I:I).NE.' '
     :                 .AND.I.LT.LEN(FITSWCSSTYLE))
                     I=I+1
                  ENDDO
                  I=I-1
                  IF (FITSWCSSTYLE(1:I).EQ."qtan") then
*                  Write out WCS headers to FTUNIT
                     CALL WFWCS0 (FTUNIT,
     :                    RAPCX,
     :                    DCPCX,
     :                    DCPCG,
     :                    FNORM*PLTCON(1,NSOL+MAXSOL),
     :                    FNORM*PLTCON(4,NSOL+MAXSOL),
     :                    PLTCON(2,NSOL)/FNORM,
     :                    PLTCON(3,NSOL)/FNORM,
     :                    PLTCON(BVALS+2,NSOL)/FNORM,
     :                    PLTCON(BVALS+3,NSOL)/FNORM,
     :                    KPROJ,
     :                    DISTOR,
     :                    'QV',
     :                    FTSTAT)
                  ELSE IF (FITSWCSSTYLE(1:I).EQ."xtan") then
*                  Write out WCS headers to FTUNIT
                     CALL WFWCS0 (FTUNIT,
     :                    RAPCX,
     :                    DCPCX,
     :                    DCPCG,
     :                    FNORM*PLTCON(1,NSOL+MAXSOL),
     :                    FNORM*PLTCON(4,NSOL+MAXSOL),
     :                    PLTCON(2,NSOL)/FNORM,
     :                    PLTCON(3,NSOL)/FNORM,
     :                    PLTCON(BVALS+2,NSOL)/FNORM,
     :                    PLTCON(BVALS+3,NSOL)/FNORM,
     :                    KPROJ,
     :                    DISTOR,
     :                    'PV',
     :                    FTSTAT)
                  ELSE
                     WRITE (LUS,
     :                    '(" Unrecognised FITS WCS style, ", a,
     :                    ".  FITS writing abandoned")')
     :                    FITSWCSSTYLE(1:I)
                     WRITE (LUX,
     :                    '("ERROR 015 Unrecognised FITS WCS style, ",
     :                    a,".  FITS writing abandoned")')
     :                    FITSWCSSTYLE(1:I)
                     FITSOP=.FALSE.
                     GOTO 1070
                  ENDIF

*               Write date of observation
                  IF (GOTPEP) THEN
*                  Calculate MJD
                     IF (KPPCG.EQ.'B') THEN
                        UTMJD=SLA_EPB2D(EPPCG)
                     ELSE
                        UTMJD=SLA_EPJ2D(EPPCG)
                     ENDIF
                     CALL sla_DJCL(UTMJD+SEC30,IUTY,IUTMO,IUTD,W1,J)
                     I=INT(W1*1440D0)
                     IUTH=I/60
                     IUTM=MOD(I,60)
                     WRITE (FTWS, '("Obs. start ",A,F8.3,
     :                    " (",I4,"-",I2.2,"-",I2.2,
     :                    "T",I2.2,":",I2.2,":00)")')
     :                    KPPCG, EPPCG,
     :                    IUTY, IUTMO, IUTD, IUTH, IUTM
                     CALL FTPKYG (FTUNIT, 'MJD-OBS', UTMJD, 7,
     :                    FTWS, FTSTAT)
                  ELSE
                     CALL FTPCOM (FTUNIT, 'no plate epoch available!!',
     :                    FTSTAT)
                  ENDIF

*               Finish up
                  CALL FTCLOS (FTUNIT, FTSTAT)
                  CALL FTFIOU (FTUNIT, FTSTAT)

*               Were there any errors?
                  IF (FTSTAT.NE.0) THEN
                     CALL FTGERR (FTSTAT,FTWS)
                     FTI=LEN(FTWS)
                     DO WHILE (FTWS(FTI:FTI).EQ.' '.AND.FTI.GT.0)
                        FTI=FTI-1
                     ENDDO
                     WRITE (LUS, '(" FITS error detected: ",a)')
     :                    FTWS(:FTI)
                     IF (LUX.GT.0)
     :                    WRITE (LUX, '("ERROR 013 FITS error: ",A)')
     :                    FTWS(:FTI)
                     CALL FTGMSG (FTWS)
*                  Returns with FTWS empty if no more messages
                     DO WHILE (LEN(FTWS).GT.0)
                        FTI=LEN(FTWS)
                        DO WHILE (FTWS(FTI:FTI).EQ.' '.AND.FTI.GT.0)
                           FTI=FTI-1
                        ENDDO
                        WRITE (LUS, '(" -- ",A)') FTWS(:FTI)
                        CALL FTGMSG (FTWS)
                     ENDDO
                  ENDIF
               ENDIF
*            End of FITS writing, and error exit
 1070          CONTINUE

               WRITE (LUS,
     :           '(1X,''Residuals,'',I2,''-coefficient fit:''/)') NTERMS
               WRITE (LUS,'(16X,''n'',11X,''dX'',8X,''dY'',8X,''dR''/)')

*           Compute predicted positions, residuals and statistics
               CALL sla_PXY(NREF,REFXYE,REFXYM,PLTCON(1,NSOL),
     :                                            REFXYP,XRMS,YRMS,RRMS)

*           Look at each reference star in turn
               DO I=1,NREF

*              Pick up measured and predicted x,y
                  XM=REFXYM(1,I)
                  YM=REFXYM(2,I)
                  XP=REFXYP(1,I)
                  YP=REFXYP(2,I)

*              Residuals
                  DX=(XP-REFXYE(1,I))/AS2R
                  DY=(YP-REFXYE(2,I))/AS2R
                  DR=SQRT(DX*DX+DY*DY)

*              Predicted X,Y to working RA,Dec
                  CALL RDCAL(XP,YP,RAPCW,DCPCW,DISTOR,RARSW,DCRSW)

*              Reduction in observed or mean place?
                  IF (REDOP) THEN

*                 Observed: to mean place in results system
                     CALL COCOOM(RARSW,DCRSW,PRMS,KQR,EQR,KPR,EPR,
     :                                                      RARSR,DCRSR)
                  ELSE

*                 Mean: to mean place in results system
                     CALL COCOMM(RARSW,DCRSW,1,0D0,0D0,0D0,
     :                  KPPCG,EPPCG,KPR,EPR,KQR,EQR,KPR,EPR,RARSR,DCRSR)
                  END IF

*              Reports
                  IF (REDOP) THEN
                     WRITE (NANOM,'(I6)') NINT(REFWL(I)*1D3)
                  ELSE
                     NANOM=' '
                  END IF
                  CALL sla_DR2TF(3,REFRD(1,I),KSR1,IRVEC1)
                  CALL sla_DR2AF(2,REFRD(2,I),KSD1,IDVEC1)
                  CALL sla_DR2TF(3,RARSR,KSR2,IRVEC2)
                  CALL sla_DR2AF(2,DCRSR,KSD2,IDVEC2)
                  WRITE (LUR,'(1X,A,I6,A,2X,2(3X,3I3.2,''.'',I3.3,2X,'//
     :              'A,I2.2,2I3.2,''.'',I2.2),SP,F12.3,F10.3,SS,F10.3)')
     :                              RNAME(I),I,NANOM,IRVEC1,KSD1,IDVEC1,
     :                                       IRVEC2,KSD2,IDVEC2,DX,DY,DR
                  WRITE (LUS,
     :          '(1X,A,I6,SP,F14.3,F10.3,SS,F10.3)') RNAME(I),I,DX,DY,DR
                  IF (LUX.GT.0) WRITE (LUX,
     :                 '("RESIDUAL ",I6,3F14.3)')
     :                 I,DX,DY,DR

*              Next reference star
               END DO

*           Report RMS of residuals
               XRMS=XRMS/AS2R
               YRMS=YRMS/AS2R
               RRMS=RRMS/AS2R
               WRITE (LUR,'(/82X,''RMS :'',3F10.3/)') XRMS,YRMS,RRMS
               WRITE (LUS,'(/16X,''RMS :'',3F10.3)') XRMS,YRMS,RRMS
*            Report overall results to log file
               IF (LUX.GT.0) THEN
*               Write out a selection of important results,
*               for parsing by the caller.

*               Fit details:
                  WRITE (LUX, 1083) "nstars",NREF,"no. ref stars"
                  WRITE (LUX, 1080) "xrms", XRMS, "arcsec"
                  WRITE (LUX, 1080) "yrms", YRMS, "arcsec"
                  WRITE (LUX, 1080) "rrms", RRMS, "arcsec"
                  WRITE (LUX, 1080) "plate", SCALE,
     :                 "(mean) plate scale, arcsec"
                  WRITE (LUX, 1080) "prms", RRMS/SCALE,
     :                 "rrms in pixels"
                  WRITE (LUX, 1083) "nterms", NTERMS, "no. terms in fit"

*               Write out the plate centre coordinates (RAPCX, DCPCX)
*               in radians as well as sexagesimal.
                  CALL sla_DR2TF(1,sla_DRANRM(RAPCX),KSRA,IRAVEC)
*               Because of the dranrm, KSRA is always '+'
                  CALL sla_DR2AF(0,sla_DRANGE(DCPCX),KSDC,IDCVEC)
                  WRITE (LUX, 1082) "rarad", RAPCX,
     :                 "projection pole RA, radians"
                  WRITE (LUX, 1082) "decrad",DCPCX,
     :                 "projection pole Dec, radians"
                  WRITE (LUX,'("RESULT ",A10, 1X,
     :                 I3,":",I2.2,":",I2.2,".",I1,10X,"# ",A)')
     :                 "rasex", IRAVEC,
     :                 "projection pole RA, sexagesimal"
                  WRITE (LUX,'("RESULT ",A10, 1X,
     :                 A,I2.2,":",I2.2,":",I2.2,12X,"# ",A)')
     :                 "decsex",
     :                 KSDC, (IDCVEC(N),N=1,3),
     :                 "projection pole Dec, sexagesimal"
 1080             FORMAT ("RESULT ",A10, 1X, F20.6, " # ", A)
 1081             FORMAT ("RESULT ",A10, 1X, F20.2, " # ", A)
 1082             FORMAT ("RESULT ",A10, 1X, F20.9, " # ", A)
 1083             FORMAT ("RESULT ",A10, 1X, I20,   " # ", A)

*               Name of the FITS file containing the astrometry (if there
*               was an error writing the FITS file, FITSOP will have been
*               set to false above).
                  IF (FITSOP) WRITE (LUX,'("RESULT ",A10,1X,A," # ",A)')
     :                 "wcs",
     :                 FITSFN(:FTI),
     :                 "FITS-WCS output file"

               ENDIF
*            End of IF(FITOK)
            END IF
         END IF

*      Indicate the end of the list of results for this fit.
*      The fit with NSOL=0 produces no output.
         IF (LUX.GT.0 .AND. NSOL.GT.0) THEN
            IF (FITOK) THEN
               WRITE (LUX, '("STATUS OK")')
            ELSE
               WRITE (LUX, '("STATUS BAD")')
            ENDIF
            WRITE (LUX, '("ENDFIT")')
         ENDIF

*     Next solution
      END DO

*******************
*  UNKNOWN STARS  *
*******************

*  End record detected?
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 999

*  No: begin unknowns reports
      WRITE (LUR,'(1H1,''Unknown stars:'',4X,A,F6.1,'//
     :           ''' mean places for epoch '',A,F8.3/)') KQR,EQR,KPR,EPR

*  Establish the solution numbers and number of coefficients to be used
      IF (NBEST.LT.1) THEN
         GO TO 995
      ELSE IF (NBEST.EQ.1) THEN
         NC1=4
         NBEST2=1
      ELSE IF (NBEST.EQ.2) THEN
         NC1=4
         NC2=6
         NBEST2=1
      ELSE
         NC1=6
         NC2=NTERMS
         NBEST2=2
      END IF

*  Set flag to indicate if only one solution is available
      ONESOL=NBEST.EQ.NBEST2

*  Print appropriate headings
      IF (ONESOL) THEN

*     One solution only:
         WRITE (LUR,
     :              '(13X,'':'',I16,''-coefficient model'',15X,'':''/'//
     :      '13X,'':   Xmeas    Ymeas'',10X,''RA'',12X,''Dec     :'','//
     :                           '4X,A/13X,'':'',49X,'':'')') NC1,NMORSP
         WRITE (LUS,
     :           '(//1X,''Unknowns,'',I2,''-coefficient model:''/)') NC1
      ELSE

*     Two solutions:
         WRITE (LUR,
     :          '(13X,'':'',I16,''-coefficient model'',15X,'':'',I17,'//
     :                             '''-coefficient model'',16X,'':''/'//
     :      '13X,'':   Xmeas    Ymeas'',10X,''RA'',12X,''Dec     :'','//
     :     '''   Xmeas    Ymeas'',10X,''RA'',13X,''Dec      :'',3X,A/'//
     :                  '13X,'':'',49X,'':'',51X,'':'')') NC1,NC2,NMORSP
         WRITE (LUS,'(//1X,''Unknowns,'',I2,''-coefficient model:''/)')
     :                                                               NC2
      END IF

*------ READY FOR COLOUR RECORD -------------------------------------

 180  CONTINUE

*  Get the next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  Colour?
      IF (RECTYP.EQ.'C') THEN

*     Acknowledge receipt of the record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*     There must be one numeric field
         IF (NFLD.NE.1) GO TO 990

*     Pick up and validate the wavelength
         W1=WORK(1)

*     Pick up and validate the wavelength
         W1=WORK(1)
         IF (W1.LT.200D0.OR.W1.GT.20000D0) GO TO 990

*     Convert nanometres to microns
         WL=W1/1000D0

*     Reduction in observed place?
         IF (REDOP) THEN

*        Yes: format wavelength for report
            WRITE (NANOM,'(I6)') NINT(WL*1D3)

*        Recompute mean-to-observed parameters if wanted
            CALL sla_MAPPA(2000D0,UTMJD,PRMS)
            CALL sla_AOPPA(UTMJD,0D0,ELONG,PHI,HM,0D0,0D0,
     :                             TDEGK,PMB,0.5D0,WL,0.0065D0,PRMS(22))
         END IF
      END IF

*------ READY FOR UNKNOWN STAR X,Y RECORD ----------------------

*  Get the next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  End?
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 999

*  Unknown star X,Y?
      IF (RECTYP.EQ.'?'.AND.NFLD.EQ.2) THEN

*     Acknowledge the record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*
*     Xmeas,Ymeas to RA,Dec
*     ---------------------

         XM=WORK(1)/FNORM
         YM=WORK(2)/FNORM

*     Measured X,Y to predicted X,Y and to working RA,Dec for solution 1
*     (n.b. uses saved distortion and plate centre before any fitting)
         CALL TRANSF(PLTCON(1,NBEST2),XM,YM,RAPCO,DCPCO,DISTO,
     :                                                XP,YP,RAUSW,DCUSW)

*     Reduction in observed or mean place?
         IF (REDOP) THEN

*        Observed: to mean place in results system
            CALL COCOOM(RAUSW,DCUSW,PRMS,KQR,EQR,KPR,EPR,RAUSR,DCUSR)
         ELSE

*        Mean: to mean place in results system
            CALL COCOMM(RAUSW,DCUSW,1,0D0,0D0,0D0,KPPCG,EPPCG,KPR,EPR,
     :                                      KQR,EQR,KPR,EPR,RAUSR,DCUSR)
         END IF

*     Format for reports
         CALL sla_DR2TF(3,RAUSR,KSR1,IRVEC1)
         CALL sla_DR2AF(2,DCUSR,KSD1,IDVEC1)

*     Find out if a second model also available
         IF (ONESOL) THEN

*        Report results from solution 1 only
            WRITE (LUR,1235)
     :              NAME,XM*FNORM,YM*FNORM,'->',IRVEC1,KSD1,IDVEC1,NANOM
 1235       FORMAT (1X,A,'  :',SP,2F9.3,SS,1X,A,3I3.2,'.',I3.3,
     :                                  2X,A,I2.2,2I3.2,'.',I2.2,' :',A)
            WRITE (LUS,1236)
     :                    NAME,XM*FNORM,YM*FNORM,'->',IRVEC1,KSD1,IDVEC1
 1236       FORMAT (1X,A,5X,SP,2F9.3,SS,3X,A,2X,3I3.2,'.',I3.3,
     :                                         2X,A,I2.2,2I3.2,'.',I2.2)
         ELSE

*        Two solutions available

*        Measured X,Y to predicted X,Y and to working RA,Dec for solution 2
            IF (NSOL.LE.2) THEN
               W=DISTO
               R=RAPCO
               D=DCPCO
            ELSE
               W=DISTOR
               R=RAPCW
               D=DCPCW
            END IF
            CALL TRANSF(PLTCON(1,NBEST),XM,YM,R,D,W,XP,YP,RAUSW,DCUSW)

*        Reduction in observed or mean place?
            IF (REDOP) THEN

*           Observed: to mean place in results system
               CALL COCOOM(RAUSW,DCUSW,PRMS,KQR,EQR,KPR,EPR,RAUSR,DCUSR)
            ELSE

*           Mean: to mean place in results system
               CALL COCOMM(RAUSW,DCUSW,1,0D0,0D0,0D0,
     :                  KPPCG,EPPCG,KPR,EPR,KQR,EQR,KPR,EPR,RAUSR,DCUSR)
            END IF

*        Report both results (full) or best only (synopsis)
            CALL sla_DR2TF(4,RAUSR,KSR2,IRVEC2)
            CALL sla_DR2AF(3,DCUSR,KSD2,IDVEC2)
            WRITE (LUR,1240) NAME,XM*FNORM,YM*FNORM,'->',IRVEC1,KSD1,
     :        IDVEC1,XM*FNORM,YM*FNORM,'->',IRVEC2,KSD2,IDVEC2,NANOM(2:)
 1240       FORMAT (1X,A,'  :',SP,2F9.3,SS,1X,A,3I3.2,'.',I3.3,
     :    2X,A,I2.2,2I3.2,'.',I2.2,' :',SP,2F9.3,SS,1X,A,3I3.2,'.',I4.4,
     :                                  2X,A,I2.2,2I3.2,'.',I3.3,' :',A)
            CALL sla_DR2TF(3,RAUSR,KSR2,IRVEC2)
            CALL sla_DR2AF(2,DCUSR,KSD2,IDVEC2)
            WRITE (LUS,1236)
     :                    NAME,XM*FNORM,YM*FNORM,'->',IRVEC2,KSD2,IDVEC2
         END IF
      END IF

*------ READY FOR UNKNOWN STAR RA,DEC RECORD -----------------------

*  Get the next input record
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)

*  End?
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 999

*  Unknown star RA,Dec?
      IF (RECTYP.EQ.'?'.AND.NFLD.EQ.7) THEN

*     Acknowledge the record
         RECTYP=' '

*     The "approx" flag is invalid here
         IF (APPROX) GO TO 990

*
*     RA,Dec to Xmeas,Ymeas
*     ---------------------

*     Obtain unknown star RA,Dec, equinox
         CALL RADEC(WORK,J1,RAUSG,DCUSG,J)
         IF (J.NE.0) WRITE (LUS,1005) INBUF
         EQUSG=WORK(7)
         CALL EQEP(EQUSG,J)
         IF (J.NE.0) GO TO 990
         CALL sla_KBJ(J2(7),EQUSG,KQUSG,J)
         IF (J.NE.0) GO TO 990

*     Transform unknown star position to coordinate system of results
         CALL COCOMM(RAUSG,DCUSG,1,0D0,0D0,0D0,KQUSG,EQUSG,KPR,EPR,
     :                                      KQR,EQR,KPR,EPR,RAUSR,DCUSR)

*     Reduction in observed or mean place?
         IF (REDOP) THEN

*        Observed: transform mean place to observed place
            CALL COCOMO(RAUSR,DCUSR,
     :                          KQR,EQR,KPR,EPR,0D0,PRMS,RAUSW,DCUSW,ZD)
         ELSE

*        Mean: transform mean place to mean-of-date
            CALL COCOMM(RAUSR,DCUSR,1,0D0,0D0,0D0,KQR,EQR,KPR,EPR,
     :                                  KPPCG,EPPCG,KPR,EPR,RAUSW,DCUSW)
         END IF

*     Compute tangential coordinates (Xi,Eta)
         IF (NSOL.LE.2) THEN
            W=DISTO
            R=RAPCO
            D=DCPCO
         ELSE
            W=DISTOR
            R=RAPCW
            D=DCPCW
         END IF
         CALL sla_DS2TP(RAUSW,DCUSW,R,D,XI,ETA,J)

*     Adjust for telescope type giving expected plate coordinates XE,YE
         XE=XI
         YE=ETA
         CALL sla_PCD(W,XE,YE)

*     Calculate 'measured' position

*     First model
         CALL sla_XY2XY(XE,YE,PLTCON(1,NBEST2+MAXSOL),X1,Y1)
         CALL sla_DR2TF(3,RAUSR,KSR1,IRVEC1)
         CALL sla_DR2AF(2,DCUSR,KSD1,IDVEC1)

*     Find out if a second model also available
         IF (ONESOL) THEN

*        No: report first result only
            WRITE (LUR,1235)
     :                    NAME,X1*FNORM,Y1*FNORM,'<-',IRVEC1,KSD1,IDVEC1
            WRITE (LUS,1236)
     :                    NAME,X1*FNORM,Y1*FNORM,'<-',IRVEC1,KSD1,IDVEC1
         ELSE

*        Yes: two models available

*        Second result
            CALL sla_XY2XY(XE,YE,PLTCON(1,NBEST+MAXSOL),X2,Y2)

*        Report both results (full) or best only (synopsis)
            CALL sla_DR2TF(4,RAUSR,KSR1,IRVEC2)
            CALL sla_DR2AF(3,DCUSR,KSD1,IDVEC2)
            WRITE (LUR,1240)
     :          NAME,X1*FNORM,Y1*FNORM,'<-',IRVEC1,KSD1,IDVEC1,
     :               X2*FNORM,Y2*FNORM,'<-',IRVEC2,KSD1,IDVEC2,NANOM(2:)
            WRITE (LUS,1236)
     :                    NAME,X2*FNORM,Y2*FNORM,'<-',IRVEC1,KSD1,IDVEC1
         END IF
      END IF

*  Loop if ready for next unknown
      IF (RECTYP.EQ.' '.OR.
     :   (RECTYP.EQ.'?'.AND.(NFLD.EQ.2.OR.NFLD.EQ.7)).OR.
     :    RECTYP.EQ.'C') GO TO 180

*  End?
      IF (RECTYP.EQ.'/'.OR.RECTYP.EQ.'E') GO TO 999

*  Bad data
 990  CONTINUE
      WRITE (LUR,8990) INBUF
      WRITE (LUS,8990) INBUF
      IF (LUX.GT.0) WRITE (LUX, 8890)
 8990 FORMAT (1X,A/
     :        1X,'*************** INVALID DATA ****************')
 8890 FORMAT ('ERROR 012 Invalid data')
      GO TO 998

*  Premature end of data
 991  CONTINUE
      WRITE (LUR,8991)
      WRITE (LUS,8991)
      IF (LUX.GT.0) WRITE (LUX,8891)
 8991 FORMAT (1X,'----------- PREMATURE END OF DATA -----------')
 8891 FORMAT ('ERROR 016 Premature end of data')
      GO TO 997

*  Plate epoch required
 992  CONTINUE
      WRITE (LUR,8992)
      WRITE (LUS,8992)
      IF (LUX.GT.0) WRITE (LUX, 8892)
 8992 FORMAT (1X,'******* PLATE EPOCH WAS NOT SPECIFIED *******')
 8892 FORMAT ('ERROR 017 Plate epoch was not specified')
      GO TO 997

*  Too many reference stars
 993  CONTINUE
      WRITE (LUR,8993) MAXREF
      WRITE (LUS,8993) MAXREF
      IF (LUX.GT.0) WRITE (LUX,8893) MAXREF
 8993 FORMAT (1X,'**** TOO MANY REFERENCE STARS; MAX',I4,' ****')
 8893 FORMAT ('ERROR 018 Too many reference stars: max=',I4)
      GO TO 997

*  Numerical disasters
 994  CONTINUE
      WRITE (LUR,8994) FITERR,NSOL
      WRITE (LUS,8994) FITERR,NSOL
      IF (LUX.GT.0) WRITE (LUX,8894) FITERR, NSOL

 8994 FORMAT (/1X,A/
     :        1X,'--------------- FIT',I2,
     :                                ' ABORTED ---------------')
 8894 FORMAT ('ERROR 009 ',A,': fit ', I2, ' aborted')
      GO TO 997

*  No solution
 995  CONTINUE
      WRITE (LUR,8995)
      WRITE (LUS,8995)
      IF (LUX.GT.0) WRITE (LUX, 8895)
 8995 FORMAT (1X,'--------- UNKNOWNS BUT NO SOLUTION ----------')
 8895 FORMAT ('ERROR 019 Unknowns but no solution')

*  Skip rest of input records if any
 997  CONTINUE
      IF (NFLD.LT.0) GO TO 999
 998  CONTINUE
      RECTYP=' '
      CALL INDAT(LUI,RECTYP,INBUF,APPROX,WORK,J1,J2,NFLD,NAME)
      IF (RECTYP.NE.'/'.AND.RECTYP.NE.'E') GO TO 998

*  End of data: loop unless end of file
 999  CONTINUE
      IF (RECTYP.NE.'E') GO TO 10

*  End of job

      END
