*+
*  Name:
*     maphd.inc

*  Purpose:
*     Specx MAPHD include file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INCLUDE 'MAPHD'

*  Description:
*     This include file describes the header block for Specx format
*     spectral maps.
*     Contains 32 4-byte words.
*
*     byte  lwrd  type  content
*        0     0  D     RAM, DECM
*       16     4  R     CELL_XSIZE, CELL_YSIZE, POS_ANGLE, MAP_VERSION
*       32     8  I4    MDUMMY()
*       48    12  I4    MSTEP, NSTEP, NPTS1, NSPEC
*
*                       +-------+------------------------
*       64    16        |NREDT  |IHEAD  |ID1    |MAP_OWN>
*                       +-------+---------------------+--
*       80    20        >ER_NAME        |MAP_ID(1:8)    >
*                       ----------------+-------------+-+
*       96    24        >MAP_ID(9:24)                   >
*                       ----------------+---------------+
*      112    28        >MAP_ID(25:40)                  |
*                       ----------------+---------------+
*      128    32

*  Authors:
*     rp: Rachael Padman (MRAO, UCB)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     {date} (rp):
*        Original version.
*     08 Aug 1993 (rp):
*        File format v3.0.
*     17 Dec 1993 (hme):
*        Re-order common block so that 8-byte quantities come first,
*        character variables last.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Variables:
      DOUBLE PRECISION RAM  ! Map centre RA (same format as scan header)
      DOUBLE PRECISION DECM ! Map centre Dec (same format as scan header)

      REAL CELL_XSIZE       ! X size of pixels (arcseconds)
      REAL CELL_YSIZE       ! Y size of pixels (arcseconds)
      REAL POS_ANGLE        ! Position angle of map y-axis
      REAL MAP_VERSION      ! Current version of map

      INTEGER MDUMMY(4)

      INTEGER MSTEP, NSTEP  ! X & Y sizes of map (pixels)
      INTEGER NPTS1         ! Number of spectral channels
      INTEGER NSPEC         ! Number of spectra in the file
      INTEGER NREDT         ! # of blocks of 'Redtape' (array INDEX).
      INTEGER IHEAD         ! Flag set to 1 when first entry made in map

      INTEGER ID1           ! Spare (Dummy)

      CHARACTER MAP_OWNER_NAME*12 ! Name of map 'owner' (reference only)
      CHARACTER MAP_ID*40         ! Map identification string

      COMMON / MAPHD /
     :   RAM, DECM,
     :   CELL_XSIZE, CELL_YSIZE, POS_ANGLE, MAP_VERSION,
     :   MDUMMY,
     :   MSTEP, NSTEP, NPTS1, NSPEC, NREDT, IHEAD,
     :   ID1,
     :   MAP_OWNER_NAME, MAP_ID

      SAVE  / MAPHD /

*. End of maphd.inc
