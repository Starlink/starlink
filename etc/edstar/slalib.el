(new-routine "SLA_ADDET"
             "Add the E-terms to a pre IAU 1976 mean place to conform to the old catalogue convention"
             '("RM" "DM" "EQ" "RC" "DC"))

(new-routine "SLA_AFIN"
             "Sexagesimal character string to angle conversion"
             '("STRING" "NSTRT" "RESLT" "JF"))

(new-routine "SLA_AMP"
             "Convert star RA,Dec from geocentric apparent to mean place"
             '("RA" "DA" "DATE" "EQ" "RM" "DM"))

(new-routine "SLA_AMPQK"
             "Convert star RA,Dec from geocentric apparent to mean place"
             '("RA" "DA" "AMPRMS" "RM" "DM"))

(new-routine "SLA_AOP"
             "Apparent to observed place, for optical sources distant from the solar system."
             '("RAP" "DAP" "DATE" "DUT" "ELONGM" "PHIM" "HM" "XP" "YP" "TDK" "PMB" "RH" "WL" "TLR" "AOB" "ZOB" "HOB" "DOB" "ROB"))

(new-routine "SLA_AOPPA"
             "Precompute apparent to observed place parameters required by SLA_AOPQK and SLA_OAPQK."
             '("DATE" "DUT" "ELONGM" "PHIM" "HM" "XP" "YP" "TDK" "PMB" "RH" "WL" "TLR" "AOPRMS"))

(new-routine "SLA_AOPPAT"
             "Recompute the sidereal time in the apparent to observed place star-independent parameter block."
             '("DATE" "AOPRMS"))

(new-routine "SLA_AOPQK"
             "Quick apparent to observed place"
             '("RAP" "DAP" "AOPRMS" "AOB" "ZOB" "HOB" "DOB" "ROB"))

(new-routine "SLA_AV2M"
             "Form the rotation matrix corresponding to a given axial vector."
             '("AXVEC" "RMAT"))

(new-routine "SLA_BEAR"
             "Bearing between points on a sphere"
             '("A1" "B1" "A2" "B2"))

(new-routine "SLA_CAF2R"
             "Convert degrees, arcminutes, arcseconds to radians"
             '("IDEG" "IAMIN" "ASEC" "RAD" "J"))

(new-routine "SLA_CALDJ"
             "Gregorian Calendar to Modified Julian Date"
             '("IY" "IM" "ID" "DJM" "J"))

(new-routine "SLA_CALYD"
             "Calendar to year and day in year"
             '("IY" "IM" "ID" "NY" "ND" "J"))

(new-routine "SLA_CC2S"
             "Direction cosines to spherical coordinates (single precision)"
             '("V" "A" "B"))

(new-routine "SLA_CC62S"
             "Conversion of position & velocity in Cartesian coordinates to spherical coordinates"
             '("V" "A" "B" "R" "AD" "BD" "RD"))

(new-routine "SLA_CD2TF"
             "Convert an interval in days into hours, minutes, seconds"
             '("NDP" "DAYS" "SIGN" "IHMSF"))

(new-routine "SLA_CLDJ"
             "Gregorian Calendar to Modified Julian Date"
             '("IY" "IM" "ID" "DJM" "J"))

(new-routine "SLA_CR2AF"
             "Convert an angle in radians into degrees, arcminutes, arcseconds"
             '("NDP" "ANGLE" "SIGN" "IDMSF"))

(new-routine "SLA_CR2TF"
             "Convert an angle in radians into hours, minutes, seconds"
             '("NDP" "ANGLE" "SIGN" "IHMSF"))

(new-routine "SLA_CS2C"
             "Spherical coordinates to direction cosines (single precision)"
             '("A" "B" "V"))

(new-routine "SLA_CS2C6"
             "Conversion of position & velocity in spherical coordinates to Cartesian coordinates"
             '("A" "B" "R" "AD" "BD" "RD" "V"))

(new-routine "SLA_CTF2D"
             "Convert hours, minutes, seconds to days"
             '("IHOUR" "IMIN" "SEC" "DAYS" "J"))

(new-routine "SLA_CTF2R"
             "Convert hours, minutes, seconds to radians"
             '("IHOUR" "IMIN" "SEC" "RAD" "J"))

(new-routine "SLA_DAF2R"
             "Convert degrees, arcminutes, arcseconds to radians"
             '("IDEG" "IAMIN" "ASEC" "RAD" "J"))

(new-routine "SLA_DAFIN"
             "Sexagesimal character string to angle conversion"
             '("STRING" "NSTRT" "DRESLT" "JF"))

(new-routine "SLA_DAT"
             "Increment to be applied to Coordinated Universal Time UTC to give International Atomic Time TAI"
             '("UTC"))

(new-routine "SLA_DAV2M"
             "Form the rotation matrix corresponding to a given axial vector."
             '("AXVEC" "RMAT"))

(new-routine "SLA_DBEAR"
             "Bearing between points on a sphere (double precision)"
             '("A1" "B1" "A2" "B2"))

(new-routine "SLA_DBJIN"
             "Convert free-format input into double precision floating point, using DFLTIN but with special syntax extensions."
             '("STRING" "NSTRT" "DRESLT" "J1" "J2"))

(new-routine "SLA_DC62S"
             "Conversion of position & velocity in Cartesian coordinates to spherical coordinates"
             '("V" "A" "B" "R" "AD" "BD" "RD"))

(new-routine "SLA_DCC2S"
             "Direction cosines to spherical coordinates (double precision)"
             '("V" "A" "B"))

(new-routine "SLA_DCMPF"
             "Decompose an [X,Y] linear fit into its constituent parameters: zero points, scales, nonperpendicularity and orientation."
             '("COEFFS" "XZ" "YZ" "XS" "YS" "PERP" "ORIENT"))

(new-routine "SLA_DCS2C"
             "Spherical coordinates to direction cosines (double precision)"
             '("A" "B" "V"))

(new-routine "SLA_DD2TF"
             "Convert an interval in days into hours, minutes, seconds"
             '("NDP" "DAYS" "SIGN" "IHMSF"))

(new-routine "SLA_DEULER"
             "Form a rotation matrix from the Euler angles - three successive rotations about specified Cartesian axes."
             '("ORDER" "PHI" "THETA" "PSI" "RMAT"))

(new-routine "SLA_DFLTIN"
             "Convert free-format input into double precision floating point"
             '("STRING" "NSTRT" "DRESLT" "JFLAG"))

(new-routine "SLA_DIMXV"
             "Performs the 3-D backward unitary transformation"
             '("DM" "VA" "VB"))

(new-routine "SLA_DJCAL"
             "Modified Julian Date to Gregorian Calendar"
             '("NDP" "DJM" "IYMDF" "J"))

(new-routine "SLA_DJCL"
             "Modified Julian Date to Gregorian year, month, day, and fraction of a day."
             '("DJM" "IY" "IM" "ID" "FD" "J"))

(new-routine "SLA_DM2AV"
             "From a rotation matrix, determine the corresponding axial vector."
             '("RMAT" "AXVEC"))

(new-routine "SLA_DMAT"
             "Matrix inversion & solution of simultaneous equations"
             '("N" "A" "Y" "D" "JF" "IW"))

(new-routine "SLA_DMXM"
             "Product of two 3x3 matrices: matrix C = matrix A x matrix B"
             '("A" "B" "C"))

(new-routine "SLA_DMXV"
             "Performs the 3-D forward unitary transformation: vector VB = matrix DM vector VA"
             '("DM" "VA" "VB"))

(new-routine "SLA_DR2AF"
             "Convert an angle in radians to degrees, arcminutes, arcseconds"
             '("NDP" "ANGLE" "SIGN" "IDMSF"))

(new-routine "SLA_DR2TF"
             "Convert an angle in radians to hours, minutes, seconds"
             '("NDP" "ANGLE" "SIGN" "IHMSF"))

(new-routine "SLA_DRANGE"
             "Normalise angle into range +/- pi (double precision)"
             '("ANGLE"))

(new-routine "SLA_DRANRM"
             "Normalise angle into range 0-2 pi (double precision)"
             '("ANGLE"))

(new-routine "SLA_DS2C6"
             "Conversion of position & velocity in spherical coordinates to Cartesian coordinates"
             '("A" "B" "R" "AD" "BD" "RD" "V"))

(new-routine "SLA_DS2TP"
             "Projection of spherical coordinates onto tangent plane ('gnomonic' projection - 'standard coordinates')"
             '("RA" "DEC" "RAZ" "DECZ" "XI" "ETA" "J"))

(new-routine "SLA_DSEP"
             "Angle between two points on a sphere"
             '("A1" "B1" "A2" "B2"))

(new-routine "SLA_DTF2D"
             "Convert hours, minutes, seconds to days"
             '("IHOUR" "IMIN" "SEC" "DAYS" "J"))

(new-routine "SLA_DTF2R"
             "Convert hours, minutes, seconds to radians"
             '("IHOUR" "IMIN" "SEC" "RAD" "J"))

(new-routine "SLA_DTP2S"
             "Transform tangent plane coordinates into spherical"
             '("XI" "ETA" "RAZ" "DECZ" "RA" "DEC"))

(new-routine "SLA_DTT"
             "Increment to be applied to Coordinated Universal Time UTC to give Terrestrial Dynamical Time TDT (formerly Ephemeris Time ET)"
             '("UTC"))

(new-routine "SLA_DVDV"
             "Scalar product of two 3-vectors (double precision)"
             '("VA" "VB"))

(new-routine "SLA_DVN"
             "Normalises a 3-vector also giving the modulus"
             '("V" "UV" "VM"))

(new-routine "SLA_DVXV"
             "Vector product of two 3-vectors (double precision)"
             '("VA" "VB" "VC"))

(new-routine "SLA_EARTH"
             "Approximate heliocentric position and velocity of the Earth"
             '("IY" "ID" "FD" "POSVEL"))

(new-routine "SLA_ECLEQ"
             "Transformation from ecliptic coordinates to J2000.0 equatorial coordinates"
             '("DL" "DB" "DATE" "DR" "DD"))

(new-routine "SLA_ECMAT"
             "Form the equatorial to ecliptic rotation matrix (IAU 1980 theory)"
             '("DATE" "RMAT"))

(new-routine "SLA_ECOR"
             "Component of Earth orbit velocity and heliocentric light time in a given direction"
             '("RM" "DM" "IY" "ID" "FD" "RV" "TL"))

(new-routine "SLA_EG50"
             "Transformation from B1950.0 'FK4' equatorial coordinates to IAU 1958 galactic coordinates"
             '("DR" "DD" "DL" "DB"))

(new-routine "SLA_EPB"
             "Conversion of Modified Julian Date to Besselian Epoch"
             '("DATE"))

(new-routine "SLA_EPB2D"
             "Conversion of Besselian Epoch to Modified Julian Date"
             '("EPB"))

(new-routine "SLA_EPCO"
             "Convert an epoch into the appropriate form - 'B' or 'J'"
             '("K0" "K" "E"))

(new-routine "SLA_EPJ"
             "Conversion of Modified Julian Date to Julian Epoch"
             '("DATE"))

(new-routine "SLA_EPJ2D"
             "Conversion of Julian Epoch to Modified Julian Date"
             '("EPJ"))

(new-routine "SLA_EQECL"
             "Transformation from J2000.0 equatorial coordinates to ecliptic coordinates"
             '("DR" "DD" "DATE" "DL" "DB"))

(new-routine "SLA_EQEQX"
             "Equation of the equinoxes (double precision)"
             '("DATE"))

(new-routine "SLA_EQGAL"
             "Transformation from J2000.0 equatorial coordinates to IAU 1958 galactic coordinates"
             '("DR" "DD" "DL" "DB"))

(new-routine "SLA_ETRMS"
             "Compute the E-terms (elliptic component of annual aberration) vector"
             '("EP" "EV"))

(new-routine "SLA_EULER"
             "Form a rotation matrix from the Euler angles - three successive rotations about specified Cartesian axes."
             '("ORDER" "PHI" "THETA" "PSI" "RMAT"))

(new-routine "SLA_EVP"
             "Barycentric and heliocentric velocity and position of the Earth"
             '("DATE" "DEQX" "DVB" "DPB" "DVH" "DPH"))

(new-routine "SLA_FITXY"
             "Fit a linear model to relate two sets of [X,Y] coordinates."
             '("ITYPE" "NP" "XYE" "XYM" "COEFFS" "J"))

(new-routine "SLA_FK425"
             "Convert B1950.0 FK4 star data to J2000.0 FK5"
             '("R1950" "D1950" "DR1950" "DD1950" "P1950" "V1950" "R2000" "D2000" "DR2000" "DD2000" "P2000" "V2000"))

(new-routine "SLA_FK45Z"
             "Convert B1950.0 FK4 star data to J2000.0 FK5 assuming zero proper motion in an inertial frame."
             '("R1950" "D1950" "BEPOCH" "R2000" "D2000"))

(new-routine "SLA_FK524"
             "Convert J2000.0 FK5 star data to B1950.0 FK4"
             '("R2000" "D2000" "DR2000" "DD2000" "P2000" "V2000" "R1950" "D1950" "DR1950" "DD1950" "P1950" "V1950"))

(new-routine "SLA_FK54Z"
             "Convert a J2000.0 FK5 star position to B1950.0 FK4 assuming zero proper motion and parallax."
             '("R2000" "D2000" "BEPOCH" "R1950" "D1950" "DR1950" "DD1950"))

(new-routine "SLA_FLOTIN"
             "Convert free-format input into single precision floating point"
             '("STRING" "NSTRT" "RESLT" "JFLAG"))

(new-routine "SLA_GALEQ"
             "Transformation from IAU 1958 galactic coordinates to J2000.0 equatorial coordinates"
             '("DL" "DB" "DR" "DD"))

(new-routine "SLA_GALSUP"
             "Transformation from IAU 1958 galactic coordinates to de Vaucouleurs supergalactic coordinates"
             '("DL" "DB" "DSL" "DSB"))

(new-routine "SLA_GE50"
             "Transformation from IAU 1958 galactic coordinates to B1950.0 'FK4' equatorial coordinates"
             '("DL" "DB" "DR" "DD"))

(new-routine "SLA_GEOC"
             "Convert geodetic position to geocentric"
             '("P" "H" "R" "Z"))

(new-routine "SLA_GMST"
             "Conversion from universal time to sidereal time"
             '("UT1"))

(new-routine "SLA_GRESID"
             "Generate pseudo-random normal deviate ( = 'Gaussian residual')"
             '("S"))

(new-routine "SLA_IMXV"
             "Performs the 3-D backward unitary transformation: vector VB = (inverse of matrix RM) vector VA"
             '("RM" "VA" "VB"))

(new-routine "SLA_INTIN"
             "Convert free-format input into integer"
             '("STRING" "NSTRT" "IRESLT" "JFLAG"))

(new-routine "SLA_INVF"
             "Invert a linear model of the type produced by the SLA_FITXY routine."
             '("FWDS" "BKWDS" "J"))

(new-routine "SLA_KBJ"
             "Select epoch prefix 'B' or 'J'"
             '("JB" "E" "K" "J"))

(new-routine "SLA_M2AV"
             "From a rotation matrix, determine the corresponding axial vector."
             '("RMAT" "AXVEC"))

(new-routine "SLA_MAP"
             "Transform star RA,Dec from mean place to geocentric apparent"
             '("RM" "DM" "PR" "PD" "PX" "RV" "EQ" "DATE" "RA" "DA"))

(new-routine "SLA_MAPPA"
             "Compute star-independent parameters in preparation for conversions between mean place and geocentric apparent place."
             '("EQ" "DATE" "AMPRMS"))

(new-routine "SLA_MAPQK"
             "Quick mean to apparent place"
             '("RM" "DM" "PR" "PD" "PX" "RV" "AMPRMS" "RA" "DA"))

(new-routine "SLA_MAPQKZ"
             "Quick mean to apparent place assuming zero parallax and proper motion."
             '("RM" "DM" "EHN" "GR2E" "ABV" "AB1" "AB2" "PNM" "RA" "DA"))

(new-routine "SLA_MOON"
             "Approximate geocentric position and velocity of the moon"
             '("IY" "ID" "FD" "POSVEL"))

(new-routine "SLA_MXM"
             "Product of two 3x3 matrices: matrix C = matrix A x matrix B"
             '("A" "B" "C"))

(new-routine "SLA_MXV"
             "Performs the 3-D forward unitary transformation: vector VB = matrix RM vector VA"
             '("RM" "VA" "VB"))

(new-routine "SLA_NUT"
             "Form the matrix of nutation for a given date (IAU 1980 theory)"
             '("DATE" "RMATN"))

(new-routine "SLA_NUTC"
             "Nutation: longitude & obliquity components and mean obliquity (IAU 1980 theory)"
             '("DATE" "DPSI" "DEPS" "EPS0"))

(new-routine "SLA_OAP"
             "Observed to apparent place"
             '("TYPE" "OB1" "OB2" "DATE" "DUT" "ELONGM" "PHIM" "HM" "XP" "YP" "TDK" "PMB" "RH" "WL" "TLR" "RAP" "DAP"))

(new-routine "SLA_OAPQK"
             "Quick observed to apparent place"
             '("TYPE" "OB1" "OB2" "AOPRMS" "RAP" "DAP"))

(new-routine "SLA_OBS"
             "Parameters of selected groundbased observing stations"
             '("N" "C" "NAME" "W" "P" "H"))

(new-routine "SLA_PCD"
             "Apply pincushion/barrel distortion to a tangent-plane [x,y]."
             '("DISCO" "X" "Y"))

(new-routine "SLA_PM"
             "Apply corrections for proper motion to a star RA,Dec"
             '("R0" "D0" "PR" "PD" "PX" "RV" "EP0" "EP1" "R1" "D1"))

(new-routine "SLA_PREBN"
             "Generate the matrix of precession between two epochs, using the old, pre-IAU1976, Bessel-Newcomb model, using Andoyer's formulation."
             '("BEP0" "BEP1" "RMATP"))

(new-routine "SLA_PREC"
             "Form the matrix of precession between two epochs (IAU1976/FK5)"
             '("EP0" "EP1" "RMATP"))

(new-routine "SLA_PRECES"
             "Precession - either FK4 (Bessel-Newcomb, pre-IAU1976) or FK5 (Fricke, post-IAU1976) as required."
             '("SYSTEM" "EP0" "EP1" "RA" "DC"))

(new-routine "SLA_PRENUT"
             "Form the matrix of precession and nutation (IAU1976/FK5)"
             '("EPOCH" "DATE" "RMATPN"))

(new-routine "SLA_PVOBS"
             "Position and velocity of an observing station"
             '("P" "H" "STL" "PV"))

(new-routine "SLA_PXY"
             "compute the array of PREDICTED coordinates and the RMS residuals"
             '("NP" "XYE" "XYM" "COEFFS" "XYP" "XRMS" "YRMS" "RRMS"))

(new-routine "SLA_RANDOM"
             "Generate pseudo-random real number in the range 0 <= X < 1."
             '("SEED"))

(new-routine "SLA_RANGE"
             "Normalise angle into range +/- pi (single precision)"
             '("ANGLE"))

(new-routine "SLA_RANORM"
             "Normalise angle into range 0-2 pi (single precision)"
             '("ANGLE"))

(new-routine "SLA_RCC"
             "Relativistic clock correction"
             '("TDB" "UT1" "WL" "U" "V"))

(new-routine "SLA_REFCO"
             "Determine constants A and B in atmospheric refraction model dZ = A tan Z + B tan3 Z."
             '("HM" "TDK" "PMB" "RH" "WL" "PHI" "TLR" "EPS" "REFA" "REFB"))

(new-routine "SLA_REFRO"
             "Atmospheric refraction for radio and optical wavelengths"
             '("ZOBS" "HM" "TDK" "PMB" "RH" "WL" "PHI" "TLR" "EPS" "REF"))

(new-routine "SLA_REFV"
             "Adjust an unrefracted Cartesian vector to include the effect of atmospheric refraction, using the simple A tan Z + B tan3 Z model."
             '("VU" "REFA" "REFB" "VR"))

(new-routine "SLA_REFZ"
             "Adjust an unrefracted zenith distance to include the effect of atmospheric refraction, using the simple A tan Z + B tan3 Z model."
             '("ZU" "REFA" "REFB" "ZR"))

(new-routine "SLA_RVEROT"
             "Velocity component in a given direction due to Earth rotation"
             '("PHI" "RA" "DA" "ST"))

(new-routine "SLA_RVGALC"
             "Velocity component in a given direction due to galactic rotation"
             '("R2000" "D2000"))

(new-routine "SLA_RVLG"
             "Velocity component due to galactic rotation and mean motion of the local group."
             '("R2000" "D2000"))

(new-routine "SLA_RVLSR"
             "Velocity component in a given direction due to the Sun's motion with respect to the Local Standard of Rest"
             '("R2000" "D2000"))

(new-routine "SLA_S2TP"
             "Projection of spherical coordinates onto tangent plane ('gnomonic' projection - 'standard coordinates')"
             '("RA" "DEC" "RAZ" "DECZ" "XI" "ETA" "J"))

(new-routine "SLA_SEP"
             "Angle between two points on a sphere"
             '("A1" "B1" "A2" "B2"))

(new-routine "SLA_SMAT"
             "Matrix inversion & solution of simultaneous equations"
             '("N" "A" "Y" "D" "JF" "IW"))

(new-routine "SLA_SUBET"
             "Remove the E-terms (elliptic component of annual aberration) from a pre IAU 1976 catalogue RA,Dec to give a mean place"
             '("RC" "DC" "EQ" "RM" "DM"))

(new-routine "SLA_SUPGAL"
             "Transformation from de Vaucouleurs supergalactic coordinates to IAU 1958 galactic coordinates"
             '("DSL" "DSB" "DL" "DB"))

(new-routine "SLA_SVD"
             "Singular value decomposition (double precision)"
             '("M" "N" "MP" "NP" "A" "W" "V" "WORK" "JSTAT"))

(new-routine "SLA_SVDCOV"
             "From the W and V matrices from the SVD factorisation of a matrix (as obtained from the SLA_SVD routine), obtain the covariance matrix."
             '("N" "NP" "NC" "W" "V" "WORK" "CVM"))

(new-routine "SLA_SVDSOL"
             "From a given vector and the SVD of a matrix (as obtained from the SVD routine), obtain the solution vector."
             '("M" "N" "MP" "NP" "B" "U" "W" "V" "WORK" "X"))

(new-routine "SLA_TP2S"
             "Transform tangent plane coordinates into spherical"
             '("XI" "ETA" "RAZ" "DECZ" "RA" "DEC"))

(new-routine "SLA_UNPCD"
             "Remove pincushion/barrel distortion from a distorted [x,y] to give tangent-plane [x,y]."
             '("DISCO" "X" "Y"))

(new-routine "SLA_VDV"
             "Scalar product of two 3-vectors (single precision)"
             '("VA" "VB"))

(new-routine "SLA_VN"
             "Normalises a 3-vector also giving the modulus"
             '("V" "UV" "VM"))

(new-routine "SLA_VXV"
             "Vector product of two 3-vectors (single precision)"
             '("VA" "VB" "VC"))

(new-routine "SLA_WAIT"
             "Interval wait"
             '("DELAY"))

(new-routine "SLA_XY2XY"
             "Transform one [X,Y] into another using a linear model of the type produced by the SLA_FITXY routine."
             '("X1" "Y1" "COEFFS" "X2" "Y2"))
