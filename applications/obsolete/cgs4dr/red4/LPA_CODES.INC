*+  LPA_CODES.INC - La Palma Archive code numbers
*    Description :
*     This INCLUDE file contains some of the code numbers used for the
*     La Palma Archive, as required by the CGS4 software. Only a small
*     percentage of the classification codes are defined here.
*
*     For a full description of the La Palma Archive, and a full list of
*     codes, see the document "La Palma Data Archive Users' Guide No. XIX"
*    Deficiencies :
*    Authors :
*     S M Beard  (ROE::SMB)
*    History :
*     30-Jul-1990: Original version                                (SMB)
*      6-Aug-1990: Names of observation type codes changed
*                  to distinguish them from the object class
*                  codes, which are an entirely different set.
*                  SKY observation type added. (This type is not
*                  included in the standard La Palma Archive). 
*                  STAR object class added, which is arbitratily
*                  set equal to the LATE_STAR object class for
*                  the time being.                                 (SMB)
*    endhistory
*    Global constants :

*   (1) Codes used to distinguish which class of object is being observed.

      INTEGER
     :  LPA__UNDEFINED,            ! Class not defined
     :  LPA__SOLAR_SYSTEM,         ! Solar System
     :  LPA__STAR,                 ! Star
     :  LPA__EARLY_STAR,           ! Early-type star
     :  LPA__LATE_STAR,            ! Late-type star
     :  LPA__MAINSEQUENCE_STAR,    ! Main sequence star
     :  LPA__VARIABLE_STAR,        ! Variable star
     :  LPA__PROTOSTAR,            ! Protostar/Protostellar cloud
     :  LPA__NEBULA,               ! Nebula
     :  LPA__GALACTIC_CENTRE,      ! Galactic centre
     :  LPA__GALAXY,               ! Galaxy
     :  LPA__ACTIVE_GALAXY,        ! Active galaxy
     :  LPA__QSO,                  ! Quasar/QSO
     :  LPA__GALAXY_GROUP          ! Group of galaxies

      PARAMETER ( LPA__UNDEFINED         =   0,
     :            LPA__SOLAR_SYSTEM      =  10,
     :            LPA__STAR              =  30, ! Same as LPA__LATE_STAR
     :            LPA__EARLY_STAR        =  20,
     :            LPA__LATE_STAR         =  30,
     :            LPA__MAINSEQUENCE_STAR =  40,
     :            LPA__VARIABLE_STAR     =  50,
     :            LPA__PROTOSTAR         =  90,
     :            LPA__NEBULA            = 100,   
     :            LPA__GALACTIC_CENTRE   = 110,
     :            LPA__GALAXY            = 120,   
     :            LPA__ACTIVE_GALAXY     = 130,
     :            LPA__QSO               = 150,
     :            LPA__GALAXY_GROUP      = 160 )

*   (2) Codes used to distinguish various observation types.

      INTEGER
     :  LPA__T_UNSPECIFIED, ! Observation type not specified
     :  LPA__T_OBJECT,      ! Regular exposure of astronomical object(s) (unspecified)
     :  LPA__T_LONGSLIT,    ! Long-slit spectroscopy of extended source
     :  LPA__T_SKY,         ! Regular exposure of blank sky
     :  LPA__T_ENGINEERING, ! Engineering exposure
     :  LPA__T_STANDARD,    ! Photometric of flux standard
     :  LPA__T_BIAS,        ! BIAS frame
     :  LPA__T_DARK,        ! DARK frame
     :  LPA__T_FLAT,        ! Flat-field (unspecified)
     :  LPA__T_SKYFLAT,     ! Sky flat-field
     :  LPA__T_CALIBRATION, ! Wavelength calibration exposure (e.g. ARC)
     :  LPA__T_COMB         ! Comb dekker for long slit spectroscopy

      PARAMETER ( LPA__T_UNSPECIFIED =  0,
     :            LPA__T_OBJECT      =  1,
     :            LPA__T_LONGSLIT    =  5,
     :            LPA__T_SKY         =  8,  ! This number has been made up
     :            LPA__T_ENGINEERING =  9,
     :            LPA__T_STANDARD    = 11,
     :            LPA__T_BIAS        = 20,
     :            LPA__T_DARK        = 21,
     :            LPA__T_FLAT        = 30,
     :            LPA__T_SKYFLAT     = 33,
     :            LPA__T_CALIBRATION = 40,
     :            LPA__T_COMB        = 55 )
