C
C***********************************************************************
C T H E   B L O C K   D A T A   " R O U T I N E "   -   D E F A U L T S
C***********************************************************************
C
      BLOCK DATA MAPBD
C
C The common block MAPCM1 contains transformation constants.
C
      COMMON /MAPCM1/ IPRJ,SINO,COSO,SINR,COSR,PHOC
C
C The common block MAPCM2 contains area-specification variables.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM
C
C The common block MAPCM3 contains parameters having to do with reading
C the data for outlines.
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,BLAG,SLAG,BLOG,SLOG,PNTS(200)
C
C The common block MAPCM4 contains most of the input parameters.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
C
      LOGICAL         INTF,LBLF,PRMF,ELPF
C
C The common block MAPCM5 contains various lists ("dictionaries") of
C two-character codes required by EZMAP for parameter-setting.
C
      COMMON /MAPCM5/ DDCT(5),LDCT(5),PDCT(10)
C
      CHARACTER*2     DDCT,LDCT,PDCT
C
C The common block MAPCM7 contains parameters describing the portion of
C the plotter frame being used.
C
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
C
C The common block MAPCM8 contains parameters set by MAPTRN and used by
C MAPIT in handling "cross-over" problems.
C
      COMMON /MAPCM8/ P,Q,R
C
C The common block MAPCMA contains values which are used to position
C dots along dotted outlines and to avoid drawing vectors which are
C too short.
C
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
C
C The common block MAPCMB contains the EZMAP error flag.
C
      COMMON /MAPCMB/ IIER
C
C The common block MAPCMP contains the buffers in which the x and y
C coordinates of points are collected for an eventual call to POINTS.
C
      COMMON /MAPCMP/ NPTB,XPTB(50),YPTB(50)
C
C The common block MAPNTS contains quantities specifying the intensities
C to be used for various portions of the plot.
C
      COMMON /MAPNTS/ INTS(7)
C
C The common block MAPSAT contains parameters for the satellite-view
C projection.
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C
C Below are descriptions of the variables in each of the common blocks,
C together with data statements giving default values to those variables
C which need default variables.
C
C
C Variables in MAPCM1:
C
C IPRJ is an integer between 1 and 12, specifying what projection is
C currently in use.  The values 10, 11, and 12 specify fast-path
C versions of the values 7, 8, and 9, respectively.  SINO, COSO, SINR,
C COSR, and PHOC are projection variables computed by MAPINT for use by
C MAPTRN.  PHOC, as it happens, is just a copy of PHIO, from the common
C block MAPCM4.
C
C
C Variables in MAPCM2:
C
C UMIN, UMAX, VMIN, and VMAX specify the limits of the rectangle to be
C drawn, in projection space.  UEPS and VEPS are set by MAPINT for use
C in MAPIT in testing for cross-over problems.  UCEN, VCEN, URNG, and
C VRNG are computed by MAPINT for use when the map perimeter is made
C elliptical (by setting the flag ELPF).  BLAM, SLAM, BLOM, and SLOM
C are respectively the biggest latitude, the  smallest latitude, the
C biggest longitude, and the smallest longitude on the map.  They are
C used in MAPGRD and in MAPLOT to make the drawing of grids and outlines
C more efficient.  UMIN and UMAX are given default values to prevent
C in MAPSTI and MAPSTR from blowing up when PLTR is set prior to the
C first call to MAPINT.
C
      DATA UMIN,UMAX / 0.,1. /
C
C
C Variables in MAPCM3:
C
C ITPN is the unit number of the "tape" from which outline data is to
C be read.  NOUT is the number of the outline to be used; the values 0
C through 5 imply 'NO', 'CO', 'US', 'PS', and 'PO', respectively; thus,
C if NOUT is zero, no outlines are to be used, and, if it is non-zero,
C it is the number of the "file" to be read from unit ITPN.  NPTS, just
C after a read, is the number of elements read into PNTS; it is then
C divided by 2 to become the number of points defined by the group just
C read.  IGID is an identifier for the group, so that, for example, one
C can distinguish a group belonging to a international boundary from
C one belonging to a U.S. state boundary.  BLAG, SLAG, BLOG, and SLOG
C specify the biggest and smallest latitude and the biggest and smallest
C longitude of the points in the group, so that, in some cases at least,
C one can decide quickly not to bother with the group.  PNTS contains
C NPTS coordinate pairs, each consisting of a latitude and a longitude,
C in degrees.
C
      DATA ITPN,NOUT / 1,1 /
C
C
C Variables in MAPCM4:
C
C INTF is a flag whose value at any given time indicates whether the
C package EZMAP is in need of initialization (.TRUE.) or not (.FALSE).
C JPRJ is an integer between 1 and 9 indicating the type of projection
C currently in use.  PHIA, PHIO, and ROTA are the pole latitude and
C longitude and the rotation angle specified by the last user call to
C MAPROJ.  ILTS is an integer between 1 and 5, specifying how the limits
C of the map are to be chosen.  PLA1-4 and PLB1-4 are the values given
C by the user for PLM1(1), PLM2(1), ..., PLM1(2), PLM2(2), ..., in the
C last call to MAPSET.  PLTR is the plotter resolution - effectively,
C the number of addressable points in the x direction.  GRID is the
C desired spacing between grid lines, in degrees of latitude/longitude.
C IDSH is the desired dash pattern (16-bit binary) for grid lines.  IDOT
C is a flag selecting solid outlines (0) or dotted outlines (1).  LBLF
C is a logical flag indicating whether the international date line, the
C equator, the Greenwich meridian, and the poles are to be labelled or
C not.  PRMF is a logical flag indicating whether or not a perimeter
C is to be drawn.  ELPF is a logical flag indicating whether the map
C perimeter is to be rectangular (.FALSE.) or elliptical (.TRUE.).
C XLOW, XROW, YBOW, and YTOW are fractions between 0. and 1. specifying
C the position of area of the plotter frame in which the map is to be
C put; the map is centered in this area and made as large as possible.
C IDTL is a flag specifying that MAPIT should draw solid outlines (0)
C or dotten outlines (1).  GRDR and SRCH are measured in degrees and
C lie in the range from .001 to 10.  GRDR specifies the resolution with
C which the grid is to be drawn and SRCH the accuracy with which the
C latitude/longitude limits of the map are to be found.  ILCW is the
C character width for characters in the label, as required for use in a
C call to PWRIT.
C
      DATA INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,PLB1,PLB2 /
     1   .TRUE.,   7,  0.,  0.,  0.,   1,  0.,  0.,  0.,  0.,  0.,  0. /
C
      DATA PLB3,PLB4, PLTR,GRID, IDSH,IDOT, LBLF , PRMF ,  ELPF ,IDTL /
     1       0.,  0.,4096., 10.,21845,   0,.TRUE.,.TRUE.,.FALSE.,   0 /
C
      DATA XLOW,XROW,YBOW,YTOW / .05,.95,.05,.95 /
C
      DATA GRDR,SRCH / 1.,1. /
C
      DATA ILCW / 1 /
C
C
C Variables in MAPCM5:
C
C DDCT is the dictionary of available datasets, LDCT the dictionary of
C map limit definition types, and PDCT the dictionary of map projection
C names.
C
      DATA DDCT / 'NO','CO','US','PS','PO' /
C
      DATA LDCT / 'MA','CO','PO','AN','LI' /
C
      DATA PDCT / 'LC','ST','OR','LE','GN','AE','CE','ME','MO','SV' /
C
C
C Variables in MAPCM7:
C
C ULOW, UROW, VBOW, and VTOW define the fraction of the plotter frame
C to be occupied by the map - they may be thought of as the first four
C arguments of the SET call or, in the GKS scheme, as the viewport.
C They are computed by MAPINT.  ULOW and UROW are given default values
C to prevent code in MAPSTI and MAPSTR from blowing up when PLTR is
C set prior to the first call to MAPINT.
C
      DATA ULOW,UROW / 0.,1. /
C
C
C Variables in MAPCM8:
C
C P, Q, and R are set by MAPTRN each time it maps (RLAT,RLON) to (U,V).
C Q is always equal to V, but P is not always equal to U.  Instead, it
C is a value of U from an intermediate step in the projection process.
C For the Lambert conformal conic, P is the distance, in longitude, from
C the central meridian.  For the cylindrical projections, P is a value
C of U prior to multiplication by a function of V shrinking the map
C toward a vertical bisector.  They are all used by MAPIT, while drawing
C lines from point to point, to detect "cross-over" (a jump from one
C side of the map to the other, caused by the projection's having slit
C the globe along some half of a great circle and laid it open with the
C two sides of the slit at opposite ends of the map).
C
C
C Variables in MAPCMA:
C
C DPLT is the mimimum vector length; MAPIT requires two points to be at
C least DPLT plotter units apart before it will join them with a vector.
C DDTS is the desired distance in plotter units between dots in a dotted
C outline.  These values are relative to the "plotter resolution" PLTR;
C DPLT/PLTR is a fraction of the plotter frame.  DSCA is the ratio of
C the length of a vector, measured in plotter units, to the length of
C the same vector, measured in the u/v plane.  Thus, given a vector of
C length D in the u/v plane, D*DSCA is its length in plotter units.
C DPSQ and DSSQ are the squares of DPLT and DSCA, respectively.  DBTD
C is the distance, in the u/v plane, between two dots DDTS plotter
C units apart.  DPLT and DDTS have the values given below and are not
C reset by the code; DSCA, DPSQ, DSSQ, and DBTD are computed by MAPINT.
C DSCA is given a default value only to keep the routines MAPSTI and
C MAPSTR from blowing up when DDTS is set prior to any call to MAPINT.
C DATL is used by MAPIT and MAPVP to keep track of where the next point
C along a curve should go.
C
      DATA DPLT,DDTS,DSCA / 4.,12.,1. /
C
C
C Variables in MAPCMB:
C
C IIER is an error flag, set whenever an error occurs during a call to
C one of the EZMAP routines.  Its value may be retrieved by a call to
C MAPGTI.
C
      DATA IIER / 0 /
C
C
C Variables in MAPCMP:
C
C NPTB is the number of points whose coordinates have been collected in
C the arrays XPTB and YPTB for eventual output by a call to POINTS.
C
      DATA NPTB / 0 /
C
C Variables in MAPNTS:
C
C The array INTS specifies intensities to be used for the perimeter, for
C the grid, for labelling, for limbs, for the continental outlines, for
C the U.S. state outlines, and for international political outlines.
C See the routine MAPCHI.  Each element is an integer in the range 0 to
C 255, inclusive.
C
      DATA INTS / 240,150,210,240,240,180,210 /
C
C
C Variables in MAPSAT:
C
C The absolute value of SALT, if greater than 1, serves as a flag that
C a satellite-view projection is to be used in place of an orthographic
C projection; its value is the distance of the satellite from the center
C of the earth, in units of earth radii.  In this case, SSMO is the
C square of SALT minus 1 and SRSS is the square root of SSMO.  If ALFA
C is zero, the projection shows the view seen by a satellite looking
C straight at the center of the earth; call this the basic satellite
C view.  If ALFA is non-zero, it and BETA are angles, in degrees,
C determining where the line of sight of the projection is.  If E is
C at the center of the earth, S is at the satellite, and P is a point
C along the line of sight, then ALFA measures the angle ESP.  If O is
C the point at the origin of the basic satellite view and P is the
C projection of the line of sight, then BETA measures the angular
C distance from the positive u axis to the line OP, positive if
C measured counter-clockwise.  SALF, CALF, SBET, and CBET are sines
C and cosines of ALFA and BETA.  The sign of SALT indicates whether a
C normal projection (positive) or an extended projection (negative)
C is to be used.  The latter makes it easier to overlay CONREC output
C on one of these projections, by projecting points out of sight around
C the limb to point just outside the limb on the projected view.
C
      DATA SALT,ALFA,BETA,SALF,CALF,SBET,CBET / 0.,0.,0.,0.,1.,0.,1. /
C
C Revision history:
C
C February, 1982   Added modifications so that points generated by the
C                  drawing of dotted continental outlines are buffered
C                  and then put out with a call to POINTS, instead of
C                  being put out one at a time with a call to POINT as
C                  before.  The latter resulted in huge overhead in the
C                  plot file.  Routines MAPLOT and MAPVP were modified,
C                  and a new common block MAPCMP was added.
C
C August, 1984     Converted to FORTRAN-77 and GKS.  Deleted the EZMAP
C                  entry point.
C
C March, 1985      Completely overhauled the code to simplify it and to
C                  remove known errors.  Updated the outline dataset
C                  to remove errors and to include international
C                  boundaries.  Implemented many controls aimed at
C                  obviating the need for source modification by users.
C
C May, 1985        Added code to prevent problems when a smoothing
C                  version of the dash package is loaded.  Added code
C                  in MAPIT to get around a CFT compiler problem.
C                  Added code to do extended orthographic and satellite-
C                  view projections.
C
C July, 1985       Fixed a missing declaration in the subroutine MAPSET
C                  and limited "CALL PLOTIT (0,0,0)" to the GKS version.
C
C August, 1985     Fixed a problem in MAPGRD which caused meridians on
C                  Mercator maps with vertical limits too close to the
C                  poles to be drawn improperly.  (The test for cross-
C                  over, in MAPIT, was being passed because the points
C                  used to draw the meridians were too far apart.)  Also
C                  fixed an error in the GKS code in MAPCHI and beefed
C                  up the implementors' instructions to say what to do
C                  with that routine when color is available.
C
C November, 1985   Added code to prevent GKS clipping from destroying
C                  part of the perimeter.
C
      END
