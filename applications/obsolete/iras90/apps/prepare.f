      SUBROUTINE PREPARE( STATUS )
*+
*  Name:
*     PREPARE

*  Purpose:
*     Prepare a group of imported FITS images for use by IRAS90.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PREPARE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine prepares a group of FITS images previously converted
*     to NDF format using KAPPA:FITSIN or KAPPA:FITSDIN (for instance)
*     for use by IRAS90 and KAPPA. Each input NDF should contain a FITS
*     extension containing the FITS header cards read from tape. These
*     header cards are assumed to accurately represent the data. For
*     this reason care should be exercised if any processing is
*     performed on the images prior to using this application.
*
*     The operations performed on the input NDFs depend on the type of
*     image stored in the NDF and are described in detail below, but
*     will always include the creation of an IRAS extension. The
*     created IRAS extensions may be examined using IRAS90 application
*     IRASTRACE. The FITS extension is retained without change in the
*     output NDFs, even though some of the keywords may no longer
*     accurately represent the data. For instance, BUNIT may no longer
*     reflect the units in which the data is stored if a value is given
*     for parameter UNITS.
*
*     A facility exists for automatically identifying pairs of
*     corresponding data and noise images which can be stored in the
*     DATA and VARIANCE components of the same output NDF.

*  Usage:
*     PREPARE IN OUT

*  ADAM Parameters:
*     COORDS = LITERAL (Read)
*        Specifies the sky coordinate system in which the field
*        positions are to be stored in the output NDFs. See help on
*        "Sky_coordinates" for more information on available sky
*        coordinate systems.
*                                        [current sky coordinate system]
*     FACTOR = REAL (Read)
*        If the units of the input NDF are not one of the standard
*        system of units recognised by IRAS90, then the user must use
*        parameter FACTOR to supply the factor for converting input
*        data values to the required output units (see also parameter
*        UNITS).
*     FIELDLAT = LITERAL (Read)
*        FIELDLAT may be used to specify the latitude (eg DEC, galactic
*        latitude, etc, depending on the value of the parameter COORDS) of
*        a field reference position for each output NDF. This should be
*        in the form of a group expression (see Help on "Group
*        Expressions") containing an entry for each of the input NDFs
*        specified by parameter IN.  If a null value is supplied for
*        FIELDLAT (or for parameter FIELDLON), then the used values are
*        derived from the FITS keyword CRVAL2.  See help on
*        "Sky_coordinates" for the formats allowed for these strings.
*                                                                    [!]
*     FIELDLON = LITERAL (Read)
*        FIELDLON may be used to specify the longitude (eg RA, galactic
*        longitude, etc, depending on the value of the parameter COORDS)
*        of a field reference position for each output NDF. This should
*        be in the form of a group expression (see Help on "Group
*        Expressions") containing an entry for each of the input NDFs
*        specified by parameter IN. If a null value is supplied for
*        FIELDLON (or for parameter FIELDLAT), then the used values are
*        derived from the FITS keyword CRVAL1.  See help on
*        "Sky_coordinates" for the formats allowed for these strings.
*                                                                    [!]
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDFs. See help on "History_in_IRAS90" for more
*        information on history. The history information will contain
*        the names of the input and output NDFs.
*                                              [current history setting]
*     IN = NDF (Read)
*        Specifies a group of input NDFs. This should be in the
*        form of a group expression (see help on "Group_expressions").
*        The group may contain images of any type (including noise
*        images), but every NDF should contain a FITS extension.
*     LABEL = LITERAL (Read)
*        A group of strings to be used as the labels for the
*        corresponding output NDFs. This should be in the form of a
*        group expression (see Help on "Group Expressions") containing
*        an entry for each of the input NDFs specified by parameter IN.
*        If a null value is supplied for this parameter then labels are
*        generated automatically for each output NDF.                [!]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     OUT = NDF (Write)
*        A group of output NDFs corresponding one-for-one with the list
*        of input NDFs given for parameter IN.  This should be in the
*        form of a group expression (see help on "Group_expressions").
*        Expressions such as "*_NEW" are expanded by replacing the "*"
*        character with each input NDF in turn. HISTORY and all
*        extensions are propagated from the input to the output NDFs.
*     PROJTYPE = LITERAL (Read)
*        If the input image is not of one of the types known to
*        PREPARE, then the type of map projection used when the image
*        was created should be given to parameter PROJTYPE, and will be
*        stored as part of the astrometry information in the output
*        NDF. The same projection type is assumed to apply for all
*        input images of unknown type specified by parameter IN.  Valid
*        projection types include GNOMONIC (i.e. tangent plane),
*        ORTHOGRAPHIC, LAMBERT and AITOFF.  See help on
*        "Map_projections" for more information on available
*        projections.                                         [GNOMONIC]
*     TITLE = LITERAL (Read)
*        A group of strings to be used as the titles for the
*        corresponding output NDFs. This should be in the form of a
*        group expression (see Help on "Group Expressions") containing
*        an entry for each of the input NDFs specified by parameter IN.
*        If a null value is supplied for this parameter then titles are
*        generated automatically for each output NDF.                [!]
*     UNITS = LITERAL (Read)
*        The units in which the data values in the output NDFs should
*        be stored.  See help on "Data_units" for a list of the
*        available units. If a null value is supplied then the output
*        NDFs are produced with the same units as the input NDFs.
*        Otherwise, the input data values are converted to the required
*        system of units before being written to the output NDF.     [!]
*     VAROUT = LOGICAL (Read)
*        If a true value is supplied for VAROUT, then any pairs of
*        corresponding data and noise images supplied by parameter IN
*        are combined into a single output NDF; the data image being
*        stored in the DATA component and the square of the noise image
*        being stored in the VARIANCE component.  Otherwise, input
*        noise images are stored in the DATA component of an output
*        NDF.                                                    [FALSE]

*  Examples:
*     PREPARE M51 *_PRE
*        This example prepare the image contained in the NDF M51, and
*        puts the result in the NDF M51_PRE.
*     PREPARE * *_PRE VAROUT
*        This example prepare all NDFs in the current directory,
*        putting the results in NDFs with same names but extended with
*        the string "_PRE". If possible, any noise image found in the
*        current directory are stored in the VARIANCE component of the
*        associated data image.
*     PREPARE ^OLD_NAMES.LIS *_PRE TITLE=^TITLES.LIS LABEL=^LABELS.LIS
*        The NDFs listed in the file OLD_NAMES.LIS are prepared and put
*        into NDFs with the same names extended with the string "_PRE".
*        The title and label to use for each NDF are read from the files
*        TITLES.LIS and LABELS.LIS.

*  IRAS Sky Survey Atlas Images:
*     NDFs should be created from ISSA FITS files using one of the
*     KAPPA applications FITSIN or FITSDIN, and PREPARE should then be
*     run specifying these NDFs for parameter IN. ISSA images are
*     identified automatically by looking at the value of the FITS
*     keyword INSTRUME. If the input NDFs contain floating point values
*     (either _REAL or _DOUBLE) then it is assumed that the data
*     numbers stored in the FITS file have been converted into data
*     values in the units specified by FITS keyword BUNIT. If the input
*     NDFs have any other data type, then the conversion to data values
*     is performed within PREPARE using the values of the FITS keywords
*     BSCALE and BZERO.  In addition, an extra scaling is applied in
*     order to produce values in the units specified by parameter UNITS
*     (if these are different to the units specified by FITS keyword
*     BUNIT).
*
*     It is assumed that the input data array is a Gnomonic projection,
*     and that the first axis is in the direction of decreasing RA and
*     that the second axis is in the direction of increasing DEC. An
*     IRAS extension is created in the output NDF holding astrometry
*     information taken from the FITS keywords CRVAL1, CRVAL2, CRPIX1,
*     CRPIX2, CDELT1 and CDELT2. The keywords CRVAL1 and CRVAL2 are
*     assumed to be RA and DEC values (referred to the equinox at an
*     epoch given by FITS keyword EPOCH).
*
*     In addition, the waveband, ISSA field number and HCON number are
*     stored in the IRAS extension, together with a string identifying
*     the image as an ISSA image.

*  Pointed Observation Images:
*     Pointed Observation images are processed in the same way as ISSA
*     images, with the following exceptions:
*
*     1) If the image is an INTENSITY data map, the background offset
*     specified by the FITS keyword BIAS is added on to the output data
*     values. No bias is added onto INTENSITY noise maps or FLUX grids.
*
*     2) The images are assumed to be orthographic projections of the
*     sky. The value of the keyword CROTA2 is assumed to be 360 degrees
*     minus the position angle of the second image axis.
*
*     3) The following information is stored in the IRAS extension;
*     waveband, grid type (whether FLUX or INTENSITY), image type
*     (whether noise or data), the SOP and observation numbers, the
*     median noise (taken from the comments at the end of the FITS
*     header).
*
*     By default, any noise maps supplied by parameter IN are stored in
*     separate output NDFs, independently of the associated data maps.
*     However, if parameter VAROUT is given a true value, then an
*     attempt is made to find any pairs of associated data and noise
*     maps specified by parameter IN. If any such pairs are found then
*     they are stored together in the same output NDF (the data map is
*     stored in the DATA component and the square of the noise map is
*     stored in the VARIANCE component). The user is told which input
*     NDFs are being stored together if this happens. The values
*     supplied for the noise map using parameters OUT, TITLE, LABEL,
*     FIELDLON or FIELDLAT are discarded in this case (the values given
*     for the data map are used in the output). In particular, note that
*     the name of the output NDF is that associated with the input NDF
*     holding the data map.

*  SKYFLUX Images:
*     SKYFLUX images are processed in the same way as ISSA images,
*     with the following exceptions:
*
*     1) It is assumed that the second axis of the input data array is 
*     in the direction of decreasing DEC. The output data array is 
*     flipped in the second dimension to make the second axis increase 
*     in the direction of increasing DEC (i.e. so that north is upwards 
*     when the image is displayed normally).
*
*     2) The following information is stored in the IRAS extension;
*     waveband, HCON, plate number, maximum and minimum SOP numbers, and
*     a flag indicating if this is an intensity or weight image.

*  Galactic Plane Images:
*     Galactic plane images are processed in the same way as ISSA
*     images, with the following exceptions:
*
*     1) The images are assumed to be Lambert equivalent cylindrical
*     projections of the sky, the first axis is assumed to be in the
*     direction of decreasing galactic longitude and the the second
*     axis in the direction of increasing galactic latitude. The image
*     is not flipped along either axis. The FITS keywords CRVAL1 and
*     CRVAL2 are assumed to give galactic longitude and latitude
*     values.
*
*     2) The following information is stored in the IRAS extension;
*     waveband, HCON and Galactic plane map number.

*  All Sky Images:
*     All sky images are processed in the same way as ISSA images, with
*     the following exceptions:
*
*     1) The images are assumed to be Aitoff projections of the sky,
*     the first axis is assumed to be in the direction of decreasing
*     galactic longitude and the the second axis in the direction of
*     increasing galactic latitude. The image is not flipped along
*     either axis. The FITS keywords CRVAL1 and CRVAL2 are assumed to
*     give galactic longitude and latitude values.
*
*     2) The following information is stored in the IRAS extension;
*     waveband, HCON, maximum and minimum SOP numbers, and a flag
*     indicating if this image is centred on the galactic centre or
*     anti-centre.

*  YORIC/HIRES Images:
*     Images produced by the YORIC processor at IPAC (also known as
*     HIRES images), are identified automatically by looking at the
*     value of the FITS keyword VERSION, which gives the version number
*     of the YORIC processor which created the image. Different types
*     of YORIC images are produced at IPAC. Some hold the surface
*     brightness of a region of the sky, others hold resolution
*     estimates, photometric noise estimates, correction factor
*     variances, and the data coverage at each pixel. These types are
*     identified automatically by looking at the comments at the end of
*     the FITS header. The processing of these images is the same as for
*     ISSA images, with the following exceptions:
*
*     1) The images are assumed to be orthographic projections of the
*     sky. The value of the keyword CROTA2 is assumed to be 360 degrees
*     minus the position angle of the second image axis.
*
*     2) The following information is stored in the IRAS extension;
*     waveband, and a string identifying the type of YORIC image.
*
*     YORIC surface brightness maps and associated photometric noise
*     maps can be stored together in the same output NDF in the same
*     way as for PO images (see "Preparing Pointed Observation
*     Images").

*  CPC Images:
*     A CPC FITS file consists of two images (the 50 and 100 um images)
*     stacked together to form a three dimensional array. PREPARE
*     splits this stack into two separate images, thus creating two
*     output NDFs from one input NDF. It is important to note that only
*     one output NDF name should be supplied for each input CPC file by
*     the user (using parameter OUT). The names of the two output NDFs
*     are derived automatically from the single supplied name by
*     appending the strings _50 (for the 50 um image) and _100 (for the
*     100 um image) to the end of the NDF name.  Likewise, any values
*     given for parameters TITLE or LABEL are extended by appending one
*     of the strings "(50 um)" or "(100 um)" to the end of the supplied
*     value. Each individual CPC image is processed like an ISSA image
*     with the following exceptions:
*
*     1) The images are assumed to be orthographic projections of the
*     sky. The value of the keyword CROTA2 is assumed to be 360 degrees 
*     minus the position angle of the second image axis.
*
*     2) The following information is stored in the IRAS extension;
*     waveband, and a flag indicating if it is a raw or cleaned CPC
*     image.

*  Other Images:
*     If any input images are supplied which cannot be identified as one
*     of the types listed above, then certain assumptions need to be
*     made about the interpretation of the image and the FITS keyword
*     values. These assumptions may not be appropriate, so users should
*     be aware of them and take corrective measures if possible (for
*     instance by suitable modifying the FITS image to make it conform
*     to the assumptions before running PREPARE). The assumptions made
*     are:
*
*     1) If the image is displayed normally, then rotation from north to
*     east is anti-clockwise.
*
*     2) The projection used to create the image is that given by
*     parameter PROJTYPE.
*     
*     3) The data comes from the survey array of detectors.
*
*     4) If non-zero, FITS keyword CROTA1 gives the position angle of
*     the second image axis (i.e the angle from north, through east, to
*     the second image axis). If CROTA1 is zero, then CROTA2 is used
*     instead.
*
*     5) The FITS keywords CRVAL1 and CRVAL2 give the coordinates of a
*     reference point, in the sky coordinate system given by parameter
*     COORDS.
*     
*     The only extra information stored in the IRAS extension is the
*     waveband, and a flag that the image is from an unknown source.
*     
*     If the units of the input image are unusual then the user will be
*     asked for the factor for converting input data values into the
*     units specified by parameter UNITS. The parameter FACTOR is used
*     for this purpose.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-DEC-1992 (DSB):
*        Original version, based on the version by WG.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FLDLAT*(IRA__SZFSC)! Sky latitude of a reference point
                                  ! within the current NDF.
      CHARACTER FLDLON*(IRA__SZFSC)! Sky longitude of a reference point
                                  ! within the current NDF.
      CHARACTER LABEL*(GRP__SZNAM)! Title for the output NDF.
      CHARACTER NGRID*(GRP__SZNAM)! Name of the current noise grid NDF.
      CHARACTER NDFIN*(GRP__SZNAM)! Name of the current input NDF.
      CHARACTER NDFOUT*(GRP__SZNAM)! Requested name for the output NDF.
      CHARACTER PRJLS*(IRA__SZPLS)! List of known projections.
      CHARACTER PROJ*(IRA__SZPRJ) ! Map projection for unknown input
                                  ! images.
      CHARACTER SCS*(IRA__SZSCS)  ! Sky Coordinate System for unknown
                                  ! input images, and FLDLON,LAT values.
      CHARACTER TITLE*(GRP__SZNAM)! Title for the output NDF.
      CHARACTER UNITS*(IRI__SZUNI)! The required output units.


      INTEGER I                  ! Index of current input NDF within the
                                 ! group IGRP(1).
      INTEGER IGRP( 8 )          ! GRP identifiers.
      INTEGER INDF1              ! NDF identifier for input NDF.
      INTEGER INDF2              ! NDF identifier for input noise grid.
      INTEGER NOUT               ! No. of good output NDFs created.
      INTEGER SIZE               ! The size of groups IGRP(1) - IGRP(7)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  Get a value for parameter COORDS. The system in which parameters 
*  FIELDLON and FIELDLAT are specified. Also, if the input image if of 
*  an unrecognised type, FITS keywords CRVAL1 and CRVAL2 are assumed to
*  be in the system specified by COORDS.
      CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )

*  Get a value for parameter PROJTYPE. This is the default projection 
*  type to be assumed for any unrecognised input images.
      CALL IRA_IPROJ( PRJLS, STATUS )    
      CALL PAR_CHOIC( 'PROJTYPE', ' ', PRJLS, .FALSE., PROJ, STATUS )

*  Get a group containing the names of the input NDFs to be prepared.
      CALL IRM_RDNDF( 'IN', 0, 1, '  Give more NDF names...', IGRP(1),
     :                SIZE, STATUS )

*  Similarly, get a group containing the output names corresponding to
*  the input NDFs.  Allow output names to be modified forms of the input
*  names.
      CALL IRM_WRNDF( 'OUT', IGRP(1), SIZE, SIZE,
     :               '  Give more NDF names...', IGRP(2), SIZE, STATUS )

*  Get a group holding the titles for the output NDFs. CPC titles are
*  extended by the addition of the strings "(50um)" and "(100um)". A
*  value of GRP__NOID is returned for IGRP4 if no titles are given.
      CALL PREPA0( 'TITLE', SIZE, '  Give more NDF titles...', IGRP(3),
     :              STATUS )

*  Get a group holding the labels for the output NDFs. The same label
*  is used for both 100um and 50um CPC output NDFs. A value of
*  GRP__NOID is returned for IGRP5 if no labels are given.
      CALL PREPA0( 'LABEL', SIZE, '  Give more NDF labels...', IGRP(4),
     :              STATUS )
  
*  Get groups holding the field longitudes and latitudes for the output
*  NDFs. IGRP6 and IGRP7 are both returned equal to GRP__NOID if either
*  longitude or latitude are not supplied.
      CALL PREPA1( 'FIELDLON', 'FIELDLAT', SIZE, SCS, IGRP(5), IGRP(6),
     :              STATUS )

*  Go through the supplied NDFs using the FITS keywords to identify any
*  pairs of associated noise and data grids. If any such pairs are
*  found, the name of the data grid is retained in IGRP(1), but the
*  name of the corresponding noise grid is transferred to group IGRP(7).
*  It is stored at the same index as the corresponding data grid in
*  group IGRP(1). Other elements of IGRP(7) are filled with blanks. The
*  gaps left by removing such noise grids from IGRP(1) are removed thus
*  reducing the size of the groups. Items of information relating to
*  such noise grids are also removed from the the groups.
      CALL PREPC2( 'VAROUT', IGRP, SIZE, STATUS )

*  There will be 2 outputs NDFs (50 um and 100um) for each input CPC
*  NDF.  The two output names are derived from the single name held in
*  group IGRP(2) by appending the strings "_50" and "_100". This means
*  that there will be more output NDFs than input NDFs (if any CPC
*  images are specified as input). Create a group to hold the actual
*  output NDF names.
      CALL GRP_NEW( 'Names of created NDFs', IGRP(8), STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the units required for the output NDFs.
      CALL IRI_GETUN( 'UNITS', IRI__MJPS, UNITS, STATUS )

*  If a null value was supplied for parameter UNITS, the output units
*  will be the same as the input units. Annull the error and set a blank
*  value for UNITS.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         UNITS = ' '
      END IF

*  Start up the IRAS90 astrometry package.
      CALL IRA_INIT( STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Loop round each NDF to be prepared.
      DO I = 1, SIZE
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP(1), I, 'READ', INDF1, STATUS )

*  Tell the user which input NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUTIF( MSG__NORM, 'PREPARE_MSG1',
     :                   '  Processing ^NDF...', STATUS )

*  If this input NDF has a corresponding noise grid, get an NDF
*  identifier for the noise grid. Otherwise, use the value NDF_NOID to
*  flag that there is no noise grid.
         IF( IGRP(7) .NE. GRP__NOID ) THEN
            CALL GRP_GET( IGRP(7), I, 1, NGRID, STATUS )

            IF( NGRID .NE. ' ' ) THEN
               CALL NDG_NDFAS( IGRP(7), I, 'READ', INDF2, STATUS )

            ELSE
               INDF2 = NDF__NOID
            END IF

         ELSE
            INDF2 = NDF__NOID
         END IF

*  Get the items of character information relating to the current input
*  NDF from groups 2,3,4,5, and 6. If any of the optional groups were
*  not supplied, blanks values are returned.
         CALL PREPA2( IGRP, I, NDFOUT, TITLE, LABEL, FLDLON, FLDLAT,
     :                STATUS)

*  Process the current input NDF.
         CALL PREPA3( 'FACTOR', 'HISTORY', INDF1, INDF2, NDFOUT, TITLE,
     :                LABEL, FLDLON, FLDLAT, SCS, PROJ, UNITS, IGRP(8),
     :                STATUS )

*  Annul the input NDF identifiers.
         CALL NDF_ANNUL( INDF1, STATUS )
         IF( INDF2 .NE. NDF__NOID ) CALL NDF_ANNUL( INDF2, STATUS )

*  If an error occured processing the current input NDF, flush the
*  error.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )

*  Warn the user that no output NDFs can be created for this input NDF.
            CALL GRP_GET( IGRP(1), I, 1, NDFIN, STATUS )

            CALL MSG_BLANKIF( MSG__QUIET, STATUS )
            CALL MSG_SETC( 'IN', NDFIN )
            CALL MSG_OUTIF( MSG__QUIET, 'PREPARE_MSG2',
     :                 'WARNING: No output NDFs can be created for ^IN',
     :                      STATUS )
            CALL MSG_BLANKIF( MSG__QUIET, STATUS )

         END IF

*  Process the next input NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get the number of good output NDFs created.
      CALL GRP_GRPSZ( IGRP(8), NOUT, STATUS )

*  Assign a group expression to the output parameter NDFLIST which
*  specifies all the output NDFs. NDFLIST should normally be associated 
*  with a suitable global parameter to cause its value to be passed on 
*  to the next application.  The output parameter NDFLIST is not 
*  advertised as a user parameter since users will normally not be 
*  aware of the existence of global parameter, and so will not know 
*  how to assign a value to it.
      IF( NOUT .GT. 0 ) CALL IRM_LISTN( 'NDFLIST', IGRP(8), 'PREPARE', 
     :                                   STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Delete all groups.
 999  CONTINUE

      DO I = 1, 8
         IF( IGRP(I) .NE. GRP__NOID ) CALL GRP_DELET( IGRP(I), STATUS )
      END DO

*  Close down the IRAS90 astrometry package.
      CALL IRA_CLOSE( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was requested, 
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'PREPARE_ERR1',
     :           'PREPARE: Unable to prepare images for use by IRAS90.',
     :                    STATUS )
         END IF

      END IF

      END
