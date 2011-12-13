      SUBROUTINE IRCAM2NDF( STATUS )
*+
*  Name:
*     IRCAM2NDF

*  Purpose:
*     Converts an IRCAM data file to a series of NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRCAM2NDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This applications converts an HDS file in the IRCAM format listed
*     in IRCAM User Note 11 to one or more NDFs.  See the Notes for a
*     detailed list of the rules of the conversion.

*  Usage:
*     ircam2ndf in prefix obs [config]

*  ADAM Parameters:
*     CONFIG = LITERAL (Read)
*        The choice of data array to place in the NDF.  It can have one
*        of the following configuration values:
*           "STARE"    --- the image of the object or sky;
*           "CHOP"     --- the chopped image of the sky;
*           "KTCSTARE" --- the electronic pedestal or bias associated
*                          with the stare image of the object or sky;
*           "KTCCHOP"  --- the electronic pedestal or bias associated
*                          with the chopped image of the sky.
*        Note that at the time of writing chopping has not been
*        implemented for IRCAM.  For practical purposes CONFIG="STARE"
*        should be used.  The suggested default is the current value.
*        ["STARE"]
*     FMTCNV = _LOGICAL (Read)
*        This specifies whether or not format conversion may occur.
*        If FMTCNV is FALSE, the data type of the data array in the NDF
*        will be the same as that in the IRCAM file, and there is no
*        scale factor and offset applied.  If FMTCNV is TRUE, whenever
*        the IRCAM observation has non-null scale and offset values,
*        the observation data array will be converted to type _REAL in
*        the NDF, and the scale and offset applied to the input data
*        values to give the `true' data values.  A null scale factor is
*        1 and a null offset is 0. [FALSE]
*     IN = IRCAM (Read)
*        The name of the input IRCAM file to convert to NDFs.  The
*        suggested value is the current value.
*     OBS()  = LITERAL (Read)
*        A list of the observation numbers to be converted into NDFs.
*        Observations are numbered consecutively from 1 up to the
*        actual number of observations in the IRCAM file.  Single
*        observations or a set of adjacent observations may be
*        specified, e.g. entering [4,6-9,12,14-16] will read
*        observations 4,6,7,8,9,12,14,15,16.  (Note that the brackets
*        are required to distinguish this array of characters from a
*        single string including commas.  The brackets are unnecessary
*        when there is only one item.)
*
*        If you wish to extract all the observations enter the wildcard
*        *.  5-* will read from 5 to the last observation.  The
*        processing will continue until the last observation is
*        converted.  The suggested value is the current value.
*     PREFIX = LITERAL (Read)
*        The prefix of the output NDFs.  The name of an NDF is the
*        prefix followed by the observation number.  The suggested
*        value is the current value.

*  Examples:
*     ircam2ndf ircam_27aug89_1cl rhooph obs=*
*        This converts the IRCAM data file called ircam_27aug89_1cl into
*        a series of NDFs called rhooph1, rhooph2 etc.  There is no
*        format conversion applied.
*     ircam2ndf ircam_27aug89_1cl rhooph [32,34-36] fmtcnv
*        This converts four observations in the IRCAM data file called
*        ircam_27aug89_1cl into NDFs called rhooph32, rhooph34,
*        rhooph35, rhooph36.  The scale and offset are applied.
*     ircam2ndf in=ircam_04nov90_1c config="KTC" obs=5 prefix=bias
*        This converts the fifth observation in the IRCAM data file
*        called ircam_04nov90_1c into an NDF called bias5 containing
*        the electronic pedestal in its data array.  There is no format
*        conversion applied.

*  Notes:
*     - The rules for the conversion of the various components are as
*     follows:
*     _________________________________________________________________
*       IRCAM file                 NDF
*     -----------------------------------------------------------------
*       .OBS.PHASEA.DATA_ARRAY ->  .DATA_ARRAY
*                                  when Parameter CONFIG="STARE"
*       .OBS.PHASEB.DATA_ARRAY ->  .DATA_ARRAY
*                                  when Parameter CONFIG="CHOP"
*       .OBS.KTCA.DATA_ARRAY   ->  .DATA_ARRAY
*                                  when Parameter CONFIG="KTCSTARE"
*       .OBS.KTCB.DATA_ARRAY   ->  .DATA_ARRAY
*                                  when Parameter CONFIG="KTCCHOP"
*
*       .OBS.DATA_LABEL        ->  .LABEL
*       .OBS.DATA_UNITS        ->  .UNITS
*       .OBS.TITLE             ->  .TITLE
*                                  If .OBS.TITLE is a blank string,
*                                  OBS.DATA_OBJECT is copied to the
*                                  NDF title instead.
*
*       .OBS.AXIS1_LABEL       ->  .AXIS(1).LABEL
*       .OBS.AXIS2_LABEL       ->  .AXIS(2).LABEL
*       .OBS.AXIS1_UNITS       ->  .AXIS(1).UNITS
*       .OBS.AXIS2_UNITS       ->  .AXIS(2).UNITS
*       .GENERAL.INSTRUMENT.PLATE_SCALE becomes the increment between
*                                  the axis centres, with co-ordinate
*                                  (0.0,0.0) located at the image
*                                  centre.  The NDF axis units both
*                                  become "arcseconds".
*
*       .GENERAL               ->  .MORE.IRCAM.GENERAL
*       .GENERAL.x             ->  .MORE.IRCAM.GENERAL.x
*       .GENERAL.x.y           ->  .MORE.IRCAM.GENERAL.x.y
*
*       .OBS.x                 ->  .MORE.IRCAM.OBS.x
*                                  This excludes the components of OBS
*                                  already listed above and DATA_BLANK.
*
*     - The data types of the IRCAM GENERAL structures have not been
*     propagated to the NDF IRCAM extensions, because it would violate
*     the rules of SGP/38.  In the IRCAM file these all have the same
*     type STRUCTURE.  The new data types are as follows:
*     _________________________________________________________________
*       Extension Name               Data type
*     -----------------------------------------------------------------
*       IRCAM.GENERAL                IRCAM_GENERAL
*       IRCAM.GENERAL.INSTRUMENT     IRCAM_INSTRUM
*       IRCAM.GENERAL.ID             IRCAM_ID
*       IRCAM.GENERAL.TELESCOPE      IRCAM_TELESCOPE
*
*     -  Upon completion the number of observations successfully
*     converted to NDFs is reported.
*
*     Bad-pixel Handling:
*     Elements of the data array equal to the IRCAM component
*     .OBS.DATA_BLANK are replaced by the standard bad value.

*  Implementation Status:
*     - The data array in the NDF is in the primitive form.
*     - The application aborts if the data array chosen by parameter
*     CONFIG does not exist in the observation.

*  Implementation Deficiencies:
*     - There is a hidden parameter called OUT which is used to make
*     the output NDFs.  This will be removed once the NDF library
*     supports access to NDFs without recourse to the parameter system.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1993 August 22 (MJC):
*        Original version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
*     2008 June 18 (MJC):
*        Trim trailing blanks from output NDF character components.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! Primdat public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Exterenal References:
      INTEGER CHR_LEN            ! Effective length of a character
                                 ! string

*  Local Constants:
      INTEGER NDIM               ! Maximum number of dimensions in the
                                 ! NDF
      PARAMETER ( NDIM = 2 )
      INTEGER MAXOBS             ! Maximum number of observation
                                 ! specifications
      PARAMETER ( MAXOBS = 100 )

*  Local Variables:
      INTEGER AXPNTR             ! Pointer to the mapped axis centres
      CHARACTER * ( 72 ) AXCHAR  ! Axis character component
      LOGICAL BAD                ! Bad values found?
      REAL BLANK                 ! Data blank
      LOGICAL BLAPRE             ! True when the data blank is present
      CHARACTER * ( 72 ) CHACMP  ! NDF character component
      CHARACTER * ( DAT__SZLOC ) COLOC ! Locator to an OBS cell
      CHARACTER * ( 4 ) COBS     ! Observation number
      CHARACTER * ( 8 ) CONFIG   ! Data configuration
      CHARACTER * ( 6 ) DATNAM   ! Name of struct containing data array
      INTEGER DIMS( NDIM )       ! Dimensions of the data array/NDF
      CHARACTER * ( DAT__SZLOC ) DATLOC ! Locator to the data array
      LOGICAL DATPRE             ! True when the data array is present
      CHARACTER * ( DAT__SZLOC ) DSLOC ! Locator to the structure
                                 ! containing the input data array
      INTEGER EL                 ! Number of elements, mapped NDF array
      CHARACTER * ( DAT__SZLOC ) EXTLOC ! Locator to the extension
                                 ! structure
      CHARACTER * ( 132 ) FILE   ! Full filename of the IRCAM file
      INTEGER FIRST              ! Number of the first observation to
                                 ! convert
      LOGICAL FMTCNV             ! True means convert data array to
                                 ! floating point
      LOGICAL GENPRE             ! True when there is a GENERAL
                                 ! structure present in the IRCAM file
      CHARACTER * ( DAT__SZLOC ) GELOC ! Locator to the IRCAM_GENERAL
                                 ! extension
      CHARACTER * ( DAT__SZLOC ) GLOC ! Locator to the input GENERAL
      INTEGER IAXIS              ! Loop counter for axes
      INTEGER IBLANK             ! Data blank (integer version)
      INTEGER ICOMP              ! Loop counter for OBS components
      CHARACTER * ( DAT__SZLOC ) ILOC ! Locator to the IRCAM input file
      INTEGER IOBS               ! Loop counter of observations
      INTEGER IPNTR              ! Pointer to the input data array
      INTEGER ISPOBS             ! Loop counter of observation
                                 ! specifications
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type of NDF
      INTEGER LAST               ! Number of the last observation to
                                 ! convert
      INTEGER MDIM               ! Number of dimensions of data array
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of a component in OBS
      INTEGER NC                 ! Used length of string
      INTEGER NCO                ! Number of characters in obs. number
      INTEGER NCOMP              ! Number of components in input OBS
      INTEGER NCONV              ! Number of successful conversions
      INTEGER NDF                ! NDF identifier
      CHARACTER * ( 132 ) NDFNAM ! Name of the output NDF
      INTEGER NLEV               ! Number of levels in the path
      INTEGER NREP               ! Number of blank-for-bad replacements
      INTEGER NUMOBS             ! Number of observations
      INTEGER NUMSPE             ! Number of observation specifications
      CHARACTER * ( 16 ) OBS( MAXOBS ) ! Observation specifications
      CHARACTER * ( DAT__SZLOC ) OBSLOC ! Locator to the OBS extension
      CHARACTER * ( DAT__SZLOC ) OLOC ! Locator to the input OBS
      INTEGER OPNTR( 1 )         ! Pointer to the output data array
      CHARACTER * ( 132 ) PATH   ! Pathname to the IRCAM file
      CHARACTER * ( DAT__SZLOC ) PHALOC ! Locator to the PHASEA struct
      REAL PLTSCL                ! Plate scale per pixel
      CHARACTER * ( 128 ) PREFIX ! Prefix of the output NDFs
      LOGICAL SCADAT             ! True if data are to be scaled
      REAL SCALE                 ! Scale factor to obtain true data
                                 ! values from input array values
      LOGICAL THERE              ! True if a named component is present
      INTEGER TPNTR              ! Pointer to the temporary data array
      INTEGER TSTAT              ! Temporary status
      CHARACTER * ( DAT__SZLOC ) TMPLOC ! Temporary locator
      REAL ZERO                  ! Offset to obtain true data values
                                 ! from input array values

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new NDF context.
*  ========================
      CALL NDF_BEGIN

*  Open the input IRCAM file.
*  ==========================
      CALL DAT_ASSOC( 'IN', 'READ', ILOC, STATUS )

*  Get a locator to the GENERAL structure.
      CALL DAT_THERE( ILOC, 'GENERAL', GENPRE, STATUS )
      IF ( GENPRE ) CALL DAT_FIND( ILOC, 'GENERAL', GLOC, STATUS )

*  Find how many NDFs are to be created.
*  =====================================
*
*  Look for the OBS structure.
      CALL DAT_FIND( ILOC, 'OBS', OLOC, STATUS )

*  Obtain its dimension.
      CALL DAT_SIZE( OLOC, NUMOBS, STATUS )

*  Get the other parameters.
*  =========================

*  Get the prefix required.
      CALL PAR_GET0C( 'PREFIX', PREFIX, STATUS )

*  Get the type of the data array to copy.
      CALL PAR_CHOIC( 'CONFIG', 'STARE', 'STARE,CHOP,KTCSTARE,KTCCHOP',
     :                .TRUE., CONFIG, STATUS )

*  Convert the choice into a component name in the IRCAM file (OBS
*  structure).
      IF ( CONFIG .EQ. 'STARE' ) THEN
         DATNAM = 'PHASEA'

      ELSE IF ( CONFIG .EQ. 'CHOP' ) THEN
         DATNAM = 'PHASEB'

      ELSE IF ( CONFIG .EQ. 'KTCSTARE' ) THEN
         DATNAM = 'KTCA'

      ELSE IF ( CONFIG .EQ. 'KTCCHOP' ) THEN
         DATNAM = 'KTCB'
      END IF

*  Get the numbers of the observations to be converted into NDFs.
      CALL PAR_GET1C( 'OBS', MAXOBS, OBS, NUMSPE, STATUS )

*  Decide whether to convert the data to floating point.
      CALL PAR_GTD0L( 'FMTCNV', .FALSE., .TRUE., FMTCNV, STATUS )

*  Initialise the counter of the number of conversions.
      NCONV = 0

*  Exit if something has wrong, to avoid the loop.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Loop for each set of observations.
*  ==================================
      DO ISPOBS = 1, NUMSPE

*  Calculate the observation-number limits for this set of observations.
         CALL KPG1_CNLIM( OBS( ISPOBS ), FIRST, LAST, STATUS )

*  A wildcard in the first part of a range or a full wildcard indicates
*  the observations start from beginning, i.e. observation one to the
*  total the number of observations present.
         FIRST = MAX( 1, FIRST )
         LAST = MAX( FIRST, MIN( LAST, NUMOBS ) )

*  Loop for each output NDF.
*  =========================

*  Process each specified observation.
         DO IOBS = FIRST, LAST

*  Get a locator to the cell containing the current observation.
            CALL DAT_CELL( OLOC, 1, IOBS, COLOC, STATUS )

*  Find the number of components in the structure.
            CALL DAT_NCOMP( COLOC, NCOMP, STATUS )

*  The container file might be created with a larger OBS structure array
*  than has actually been filled with observations.  (This saves having
*  to reshape the structure during observing.)  Watch for this case, and
*  exit when it happens.  This may not be ideal if the user has supplied
*  the observations not in a monotonically increasing sequence, but it
*  would be annoying to get dozens of these when processing a whole
*  night's observations into an NDF.
            IF ( NCOMP .EQ. 0 ) THEN

*  Abort if the structure is empty.  Make an error report using the
*  file name only if the user has not requested up to the last
*  observation.  In this case it can be handled transparently.
               IF ( OBS( ISPOBS ) .NE. '*' .AND.
     :              INDEX( OBS( ISPOBS ), '-*' ) .EQ. 0 ) THEN
                  CALL HDS_TRACE( ILOC, NLEV, PATH, FILE, STATUS )
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'I', IOBS )
                  CALL MSG_SETC( 'IRCAM', FILE )
                  CALL ERR_REP( 'IRCAM2NDF_NOOBS',
     :              'IRCAM2NDF: There is no observation ^I in the '/
     :              /'IRCAM file ^IRCAM.', STATUS )
               END IF

*  Tidy the locator created in the loop thus far but not previously
*  annulled.
               CALL DAT_ANNUL( COLOC, STATUS )
               GOTO 999
            END IF

*  Initialise some variables.
            PLTSCL = -1.0

*  Determine the dimensions and data type of the NDF.
*  ==================================================

*  Look for the PHASEA structure to determine the dimensions of the
*  data array.  Start a new error context.
            CALL ERR_MARK
            CALL DAT_FIND( COLOC, 'PHASEA', PHALOC, STATUS )

*  The last observation usually has some objects present set to default
*  values, but it excludes the data arrays.  Watch for this case, and
*  exit when it happens.  This may not be ideal if the user has
*  supplied the observations not in a monotonically increasing
*  sequence, but it would be annoying to get a confusing message when
*  processing a whole night's observations into an NDF, when in fact
*  it was normal and expected.
            IF ( STATUS .NE. SAI__OK ) THEN

*  Abort if the structure lacks a data array. Make an error report
*  using the file name only if the user has not requested up to the
*  last observation.  In this case it can be handled transparently.
*  Use a temporatory status in order to obtain the file name and path.
               IF ( OBS( ISPOBS ) .NE. '*' .AND.
     :              INDEX( OBS( ISPOBS ), '-*' ) .EQ. 0 ) THEN

                  TSTAT = STATUS
                  CALL HDS_TRACE( ILOC, NLEV, PATH, FILE, STATUS )
                  STATUS = TSTAT

                  CALL MSG_SETI( 'I', IOBS )
                  CALL MSG_SETC( 'IRCAM', FILE )
                  CALL ERR_REP( 'IRCAM2NDF_NOOBS',
     :              'IRCAM2NDF: There is no observation ^I in the '/
     :              /'IRCAM file ^IRCAM.', STATUS )

*  Handle the expected error transparently by annuling it.
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF

*  Tidy the locator created in the loop thus far but not previously
*  annulled.
               CALL DAT_ANNUL( COLOC, STATUS )

*  Release the new error context.
               CALL ERR_RLSE
               GOTO 999

            END IF

*  Release the new error context.
            CALL ERR_RLSE

*  Get the shape of the DATA_ARRAY component.
            CALL CMP_SHAPE( PHALOC, 'DATA_ARRAY', NDIM, DIMS, MDIM,
     :                      STATUS )

*  Get the type of DATA_ARRAY component.
            CALL CMP_TYPE( PHALOC, 'DATA_ARRAY', ITYPE, STATUS )

*  Search for the scale and offset when format conversion has been
*  requested.  Since these components may not be present a new error
*  context is started, and an innitial checks for bad status is
*  necessary as a bad status could be annulled.
            IF ( FMTCNV .AND. STATUS .EQ. SAI__OK ) THEN
               CALL ERR_MARK

*  Get the scale value.  If it's not present annul the error and assign
*  the value to the default.
               CALL CMP_GET0R( PHALOC, 'DATA_SCALE', SCALE, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SCALE = 1.0
               END IF

*  Get the offset value.  If it's not present annul the error and assign
*  the value to the default.
               CALL CMP_GET0R( PHALOC, 'DATA_ZERO', ZERO, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  ZERO = 0.0
               END IF

*  Only perform a format conversion when the scale and offset do not
*  have the default values.  Record this fact.
               IF ( ABS( SCALE - 1.0 ) .GT. VAL__EPSR .OR.
     :              ABS( ZERO - 0.0 ) .GT. VAL__EPSR ) THEN
                  ITYPE = '_REAL'
                  SCADAT = .TRUE.
               ELSE
                  SCADAT = .FALSE.
               END IF

*  Annul the locator.
               CALL DAT_ANNUL( PHALOC, STATUS )

*  End the error context.
               CALL ERR_RLSE
            END IF

*  Create the NDF.
*  ===============

*  Convert the observation number to a string.
            CALL CHR_ITOC( IOBS, COBS, NCO )

*  Generate the name of the output NDF.
            NDFNAM = PREFIX( :CHR_LEN( PREFIX ) )//COBS( :NCO )

*  Put the file name into the parameter.  This is necessary because
*  NDF_ does not yet permit NDF creation by file name; it can only be
*  done through an ADAM parameter.
            CALL AIF_PTFNM( 'OUT', NDFNAM, STATUS )

*  Create the primitive NDF.
            CALL NDF_CREP( 'OUT', ITYPE, MDIM, DIMS, NDF, STATUS )

*  Validate the data array.
*  ========================

*  Inquire if the chosen structure is present.
            CALL DAT_THERE( COLOC, DATNAM, DATPRE, STATUS )

*  Abort if the structure is not present.  Make an error report using
*  the file name.
            IF ( .NOT. DATPRE .AND. STATUS .EQ. SAI__OK ) THEN
               CALL HDS_TRACE( ILOC, NLEV, PATH, FILE, STATUS )
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', IOBS )
               CALL MSG_SETC( 'D', CONFIG )
               CALL MSG_SETC( 'IRCAM', FILE )
               CALL ERR_REP( 'IRCAM2NDF_NODATA',
     :           'IRCAM2NDF: The ^D data array is not present in '/
     :           /'observation ^I of the IRCAM file ^IRCAM.', STATUS )

*  Tidy the locator created in the loop thus far but not previously
*  annulled.
               CALL DAT_ANNUL( COLOC, STATUS )
               GOTO 999
            END IF

*  Obtain a locator to the structure supposed to contain the data array.
            CALL DAT_FIND( COLOC, DATNAM, DSLOC, STATUS )

*  Inquire if the chosen data array is present.
            CALL DAT_THERE( DSLOC, 'DATA_ARRAY', DATPRE, STATUS )

*  Abort if the data array is not present.
            IF ( .NOT. DATPRE .AND. STATUS .EQ. SAI__OK ) THEN
               CALL HDS_TRACE( ILOC, NLEV, PATH, FILE, STATUS )
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', IOBS )
               CALL MSG_SETC( 'D', CONFIG )
               CALL MSG_SETC( 'IRCAM', FILE )
               CALL ERR_REP( 'IRCAM2NDF_NODATA',
     :           'IRCAM2NDF: The ^D data array is not present in '/
     :           /'observation ^I of the IRCAM file ^IRCAM.', STATUS )

*  Tidy the locator just created.
               CALL DAT_ANNUL( DSLOC, STATUS )

*  Tidy the locator created in the loop thus far but not previously
*  annulled.
               CALL DAT_ANNUL( COLOC, STATUS )
               GOTO 999
            END IF

*  Map the data arrays.
*  ====================

*  Obtain a locator to the chosen data array.
            CALL DAT_FIND( DSLOC, 'DATA_ARRAY', DATLOC, STATUS )

*  Map the input data array.
            CALL DAT_MAP( DATLOC, ITYPE, 'READ', MDIM, DIMS, IPNTR,
     :                    STATUS )

*  Map the output NDF data array.  EL will always be reurned with a
*  sensible value even if status is bad, and so reduces the number of
*  status checks.
            CALL NDF_MAP( NDF, 'Data', ITYPE, 'WRITE', OPNTR, EL,
     :                    STATUS )

*  Assign the data array.
*  ======================

*  The data array must be processed twice when there is scaling to
*  apply.  So get some mapped workspace, othwerwise clone the pointer.
            IF ( SCADAT ) THEN
               CALL PSX_CALLOC( EL, '_REAL', TPNTR, STATUS )
            ELSE
               TPNTR = OPNTR( 1 )
            END IF

*  Look for the data blank.
            CALL DAT_THERE( COLOC, 'DATA_BLANK', BLAPRE, STATUS )

*  If it is present, get its value and integer equivalent.
            IF ( BLAPRE ) THEN
               CALL CMP_GET0R( COLOC, 'DATA_BLANK', BLANK, STATUS )
               IBLANK = NINT( BLANK )

*  Assume that the data type is integer or real.  Get the bad values to
*  be replaced by the bad values.  This avoids dealing with workspace
*  as all the data can be passed through the replace routine.
            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               IBLANK = VAL__BADI
            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               BLANK = VAL__BADR
            END IF

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.  Call the appropriate routine for the
*  array data type.
            IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_CHVAI( EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                          IBLANK, VAL__BADI,
     :                          %VAL( CNF_PVAL( TPNTR ) ), NREP,
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_CHVAR( EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                          BLANK, VAL__BADR,
     :                          %VAL( CNF_PVAL( TPNTR ) ), NREP,
     :                          STATUS )

            END IF

*  Tidy the input array and the structure within which it lies.
            CALL DAT_ANNUL( DATLOC, STATUS )
            CALL DAT_ANNUL( DSLOC, STATUS )

            IF ( SCADAT ) THEN

*  Apply the scale and zero to convert the temporary array into the
*  output NDF's data array.
               CALL KPG1_SCLOF( EL, %VAL( CNF_PVAL( TPNTR ) ),
     :                          DBLE( SCALE ), DBLE( ZERO ),
     :                          %VAL( CNF_PVAL( OPNTR( 1 ) ) ), BAD,
     :                          STATUS )

*  Release the workspace.
               CALL PSX_FREE( TPNTR, STATUS )

            END IF

*  Unmap the NDF array.
            CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  Copy the ancillary data to the NDF.
*  ===================================

*  Look for the title.
            CALL DAT_THERE( COLOC, 'TITLE', THERE, STATUS )

*  If it is present, get its value and use it to update the NDF's title.
            IF ( THERE ) THEN
               CALL CMP_GET0C( COLOC, 'TITLE', CHACMP, STATUS )

*  In earlier data the title could be blank and the object name could
*  was used instead.
               IF ( CHACMP .EQ. ' ' ) THEN

*  Look for the object name.
                   CALL DAT_THERE( COLOC, 'OBJECT_NAME', THERE, STATUS )

*  If it is present, get its value and use it to update the NDF's title.
                   IF ( THERE ) THEN
                      CALL CMP_GET0C( COLOC, 'OBJECT_NAME', CHACMP,
     :                                STATUS )

                      NC = CHR_LEN( CHACMP )
                      CALL NDF_CPUT( CHACMP( :NC ), NDF, 'Title',
     :                               STATUS )
                   END IF
               ELSE
                  NC = CHR_LEN( CHACMP )
                  CALL NDF_CPUT( CHACMP, NDF, 'Title', STATUS )
               END IF
            END IF

*  Look for the label.
            CALL DAT_THERE( COLOC, 'DATA_LABEL', THERE, STATUS )

*  If it is present, get its value and use it to update the NDF's label.
            IF ( THERE ) THEN
               CALL CMP_GET0C( COLOC, 'DATA_LABEL', CHACMP, STATUS )
               NC = CHR_LEN( CHACMP )
               CALL NDF_CPUT( CHACMP( :NC ), NDF, 'Label', STATUS )
            END IF

*  Look for the units.
            CALL DAT_THERE( COLOC, 'DATA_UNITS', THERE, STATUS )

*  If it is present, get its value and use it to update the NDF's units.
            IF ( THERE ) THEN
               CALL CMP_GET0C( COLOC, 'DATA_UNITS', CHACMP, STATUS )
               NC = CHR_LEN( CHACMP )
               CALL NDF_CPUT( CHACMP( :NC ), NDF, 'Units', STATUS )
            END IF

*  Make an IRCAM extension.
*  ========================

*  Create the IRCAM extension.
            CALL NDF_XNEW( NDF, 'IRCAM', 'IRCAM_EXTENSION', 0, 0,
     :                     EXTLOC, STATUS )

*  Recursively copy the GENERAL structure to the extension.
            IF ( GENPRE .AND. STATUS .EQ. SAI__OK ) THEN
               CALL DAT_COPY( GLOC, EXTLOC, 'GENERAL', STATUS )

*  Get a locator to the copied GENERAL extension.
               CALL DAT_FIND( EXTLOC, 'GENERAL', GELOC, STATUS )

*  Set the types of the extensions.
*  ================================

*  The structures do not seem to have data types, so any software
*  reading these extensions would have to go by name.  This violates
*  SGP/38 rules.  Therefore give the extensions new types.

*  Start with the GENERAL extension.
               CALL DAT_RETYP( GELOC, 'IRCAM_GENERAL', STATUS )

*  Look for the INSTRUMENT structure.
               CALL DAT_THERE( GELOC, 'INSTRUMENT', THERE, STATUS )

*  If it's present then get a locator to it, re-type it, and release
*  the locator.
               IF ( THERE ) THEN
                  CALL DAT_FIND( GELOC, 'INSTRUMENT', TMPLOC, STATUS )
                  CALL DAT_RETYP( GELOC, 'IRCAM_INSTRUM', STATUS )

*  Also get the plate scale for making the axis centres.
                  CALL DAT_THERE( TMPLOC, 'PLATE_SCALE', THERE, STATUS )
                  IF ( THERE ) CALL CMP_GET0R( TMPLOC, 'PLATE_SCALE',
     :              PLTSCL, STATUS )
                  CALL DAT_ANNUL( TMPLOC, STATUS )
               END IF

*  Look for the ID structure.
               CALL DAT_THERE( GELOC, 'ID', THERE, STATUS )

*  If it's present then get a locator to it, re-type it, and release
*  the locator.
               IF ( THERE ) THEN
                  CALL DAT_FIND( GELOC, 'ID', TMPLOC, STATUS )
                  CALL DAT_RETYP( GELOC, 'IRCAM_ID', STATUS )
                  CALL DAT_ANNUL( TMPLOC, STATUS )
               END IF

*  Look for the TELESCOPE structure.
               CALL DAT_THERE( GELOC, 'TELESCOPE', THERE, STATUS )

*  If it's present then get a locator to it, re-type it, and release
*  the locator.
               IF ( THERE ) THEN
                  CALL DAT_FIND( GELOC, 'TELESCOPE', TMPLOC, STATUS )
                  CALL DAT_RETYP( GELOC, 'IRCAM_TELESCOPE', STATUS )
                  CALL DAT_ANNUL( TMPLOC, STATUS )
               END IF

*  Tidy the locator to the GENERAL extension.
               CALL DAT_ANNUL( GELOC, STATUS )
            END IF

*  Copy the axis data to the NDF.
*  ==============================

*  Create a default axis system.
            CALL NDF_ACRE( NDF, STATUS )

*  If there is a plate scale defined (positive), need to make an
*  axis array.
            IF ( PLTSCL .GT. 0.0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Loop for each dimension.
               DO IAXIS = 1, MDIM

*  Map the first axis-centre array.
                  CALL NDF_AMAP( NDF, 'Centre', IAXIS, '_REAL', 'WRITE',
     :                           AXPNTR, EL, STATUS )

*  Fill the centres' array using the step size (usually in arcseconds)
*  such that the centre of the axis is 0.
                  CALL KPG1_SSAZR( EL, DBLE( PLTSCL ),
     :                             -DBLE( EL ) * 0.5D0 * DBLE( PLTSCL ),
     :                             %VAL( CNF_PVAL( AXPNTR ) ), STATUS )

*  Unmap the axis-centre array.
                  CALL NDF_AUNMP( NDF, 'Centre', IAXIS, STATUS )

*  The axis units are now defined to be arcsec after the application
*  of the plate scale.
                  CALL NDF_ACPUT( 'arcseconds', NDF, 'Units', IAXIS,
     :                            STATUS )
               END DO

            END IF

*  Look for the axis labels.  Start with the first axis.
            CALL DAT_THERE( COLOC, 'AXIS1_LABEL', THERE, STATUS )

*  If it is present, get its value and use it to update the axis label
*  in the NDF.
            IF ( THERE ) THEN
               CALL CMP_GET0C( COLOC, 'AXIS1_LABEL', AXCHAR, STATUS )
               NC = CHR_LEN( AXCHAR )
               CALL NDF_ACPUT( AXCHAR( :NC ), NDF, 'Label', 1, STATUS )
            END IF

*  Now deal with the second axis.
            CALL DAT_THERE( COLOC, 'AXIS2_LABEL', THERE, STATUS )

*  If it is present, get its value and use it to update the axis label
*  in the NDF.
            IF ( THERE ) THEN
               CALL CMP_GET0C( COLOC, 'AXIS2_LABEL', AXCHAR, STATUS )
               NC = CHR_LEN( AXCHAR )
               CALL NDF_ACPUT( AXCHAR( :NC ), NDF, 'Label', 2, STATUS )
            END IF

*  Only copy the axis units (usually "pixels") if the centres are not
*  in arcseconds.
            IF ( PLTSCL .LE. 0.0 ) THEN

*  Look for the axis units.  Start with the first axis.
               CALL DAT_THERE( COLOC, 'AXIS1_UNITS', THERE, STATUS )

*  If it is present, get its value and use it to update the axis units
*  in the NDF.
               IF ( THERE ) THEN
                  CALL CMP_GET0C( COLOC, 'AXIS1_UNITS', AXCHAR, STATUS )
                  NC = CHR_LEN( AXCHAR )
                  CALL NDF_ACPUT( AXCHAR( :NC ), NDF, 'Units', 1,
     :                            STATUS )
               END IF

*  Now deal with the second axis.
               CALL DAT_THERE( COLOC, 'AXIS2_UNITS', THERE, STATUS )

*  If it is present, get its value and use it to update the axis units
*  in the NDF.
               IF ( THERE ) THEN
                  CALL CMP_GET0C( COLOC, 'AXIS2_UNITS', AXCHAR, STATUS )
                  NC = CHR_LEN( AXCHAR )
                  CALL NDF_ACPUT( AXCHAR( :NC ), NDF, 'Units', 2,
     :                            STATUS )
               END IF
            END IF

*  Move the other OBS-structure objects to the extension.
*  ======================================================

*  Create the OBS extension.
            CALL DAT_NEW( EXTLOC, 'OBS', 'IRCAM_OBS', 0, 0, STATUS )

*  Get a locator to the OBS extension.
            CALL DAT_FIND( EXTLOC, 'OBS', OBSLOC, STATUS )

*  Loop for the components of the cell in the OBS structure of the IRCAM
*  file.  Do not want to loop if something has gone wrong.
            IF ( STATUS .EQ. SAI__OK ) THEN
               DO ICOMP = 1, NCOMP

*  Get a locator for the next object in the OBS structure.
                  CALL DAT_INDEX( COLOC, ICOMP, TMPLOC, STATUS )

*  Get the object's name.
                  CALL DAT_NAME( TMPLOC, NAME, STATUS )

*  Do not copy certain objects to the NDF if they've already been used
*  to make an NDF component, or are the other data arrays.
                  IF ( NAME .NE. 'AXIS1_LABEL' .AND.
     :                 NAME .NE. 'AXIS2_LABEL' .AND.
     :                 NAME .NE. 'AXIS1_UNITS' .AND.
     :                 NAME .NE. 'AXIS2_UNITS' .AND.
     :                 NAME .NE. 'PHASEA' .AND. NAME .NE. 'PHASEB' .AND.
     :                 NAME .NE. 'KTCA' .AND. NAME .NE. 'KTCB' .AND.
     :                 NAME .NE. 'DATA_BLANK' .AND.
     :                 NAME .NE. 'DATA_LABEL' .AND.
     :                 NAME .NE. 'DATA_UNITS' .AND.
     :                 NAME .NE. 'TITLE' ) THEN

*  Copy the object to the OBS extension.
                     CALL DAT_COPY( TMPLOC, OBSLOC, NAME, STATUS )
                  END IF

*  Annul the temporary locator.
                  CALL DAT_ANNUL( TMPLOC, STATUS )
               END DO
            END IF

*  Tidy the locators defined in the loop.
            CALL DAT_ANNUL( OBSLOC, STATUS )
            CALL DAT_ANNUL( EXTLOC, STATUS )
            CALL DAT_ANNUL( COLOC, STATUS )

*  Tidy the NDF identifier created in the loop.
            CALL NDF_ANNUL( NDF, STATUS )

*  Count the conversions if there has been no error.  Exit the loops
*  when status is bad as there is no point continuing.  Give the user
*  the bad news as soon as possible.
            IF ( STATUS .EQ. SAI__OK ) THEN
               NCONV = NCONV + 1
            ELSE
               GOTO 999
            END IF

*  End the observation and observation-specification loops.
         END DO
      END DO

  999 CONTINUE

*  Tidy the locators.
      CALL DAT_ANNUL( OLOC, STATUS )
      CALL DAT_ANNUL( GLOC, STATUS )
      CALL DAT_ANNUL( ILOC, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message, otherwise
*  inform the user of the number of observations converted to NDFs.
      CALL MSG_SETI( 'N', NCONV )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRCAM2NDF_ERR',
     :     'IRCAM2NDF: Unable to convert the IRCAM file to a series '/
     :     /'of NDFs.  Failed after converting ^N observations.',
     :     STATUS )
      ELSE IF ( NCONV .NE. 1 ) THEN
         CALL MSG_OUT( 'NCONV',
     :     '^N observations were converted to NDFs.', STATUS )
      ELSE
         CALL MSG_OUT( 'NCONV',
     :     '^N observation was converted to an NDF.', STATUS )
      END IF

      END
