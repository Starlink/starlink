#!/usr/bin/env python

'''
*+
*  Name:
*     POL2MAP

*  Purpose:
*     Create Q, U and I maps from a group of POL-2 "spin&scan" data files.

*  Description:
*     This script creates maps (Q, U and I) and a vector catalogue from a
*     set of POL-2 observation. New observations can be added into the map
*     without the need to re-process previously processed observations.
*     The output maps are all in units of pW.
*
*     Note, with the default configuration this script can take up to
*     an hour to run for each observation on a typical SCUBA-2-capabale
*     computer.
*
*     Masking of models within makemap (AST, etc) can be based either on the
*     SNR of the map created as the end of each iteration, or on an external
*     map, or on a fixed circle centred on the origin - see parameter MASK.
*
*     By default, the Q, U, I and PI catalogue values are in units of
*     mJy/beam (see parameter Jy).

*  Usage:
*     pol2map in iout qout uout [cat] [config] [pixsize] [qudir] [mapdir]
*             [mask] [masktype] [ipcor] [ipref] [reuse] [ref] [north]
*             [debias] [retain] [maskout1] [maskout2] [msg_filter] [ilevel]
*             [glevel] [logfile]

*  ADAM Parameters:
*     BINSIZE = _REAL (Read)
*        The bin size in the output vector catalogue, in arcsec. The
*        value supplied for parameter PIXSIZE is used as the default for
*        BINSIZE. An error is reported if BINSIZE is smaller than
*        PIXSIZE. []
*     CAT = LITERAL (Read)
*        The output FITS vector catalogue. No catalogue is created if
*        null (!) is supplied. The Q, U  and PI values in this catalogue
*        will be in units of pW or mJy/beam, as selected using parameter
*        JY . The bin size is specified by parameter BINSIZE. An extra
*        column named "AST" is added to this catalogue in addition to those
*        created by the polpack:polvec command. The AST column that holds a
*        non-zero integer for each row that corresponds to a point inside
*        the AST mask (i.e. a source point), and zero for all other rows. [!]
*     CONFIG = LITERAL (Read)
*        Extra parameter values to include in the MAKEMAP configuration
*        used to create both the I maps and the Q/U maps.
*
*        In general, it is important that the I, Q and U maps are all
*        created using the same configuration so that they can be compared
*        directly. However, if it is necessary to use a different
*        configuration for I and Q/U maps, the differences may be
*        specified using the ADAM parameters "ICONFIG" and "QUCONFIG". The
*        ADAM parameter "CONFIG" specifies the configuration parameters
*        that are always used, whether an I map or a Q/U map is being
*        created. In all cases the configuration parameters specified by
*        "CONFIG" are applied first, followed  by the configuration
*        parameters specified by "ICONFIG" (if creating an I map) or
*        "QUCONFIG" (if creating a Q or U map). Thus values supplied in
*        "ICONFIG" or "QUCONFIG" over-ride values for the same parameters
*        specified in "CONFIG".
*
*        The configurations specified by CONFIG, ICONFIG and QUCONFIG are
*        applied on top of the following set of default parameters:
*        ---
*           ^$STARLINK_DIR/share/smurf/.dimmconfig_pol2.lis
*           numiter = -200
*           modelorder=(com,gai,pca,ext,flt,ast,noi)
*
*           maptol = 0.05
*           maptol_mask = <undef>
*           maptol_mean = 0
*           maptol_box = 60
*           maptol_hits = 1
*
*           ast.mapspike_freeze = 5
*           pca.pcathresh = -150
*           pca.zero_niter = 0.5
*           com.zero_niter = 0.5
*           flt.zero_niter = 0.5
*           com.freeze_flags = 30
*        ---
*        Additional parameters are also set, depending on the value of
*        parameter MASK. If MASK is set to "AUTO", the following
*        parameters are added to the above default config:
*        ---
*           ast.skip = 10
*           ast.zero_snr = 3
*           ast.zero_snrlo = 2
*           ast.zero_freeze = 0.2
*
*           pca.pcathresh = -50
*           pca.zero_snr = 5
*           pca.zero_snrlo = 3
*           pca.zero_freeze = -1
*
*           com.zero_snr = 5
*           com.zero_snrlo = 3
*           com.zero_freeze = -1
*
*           flt.zero_snr = 5
*           flt.zero_snrlo = 3
*           flt.zero_freeze = -1
*        ---
*        If MASK is set to "CIRCLE", the following parameters are added
*        to the above default config:
*        ---
*           ast.zero_circle = 0.0083  (degrees, i.e. 30 arc-seconds)
*           pca.zero_circle = 0.0038
*           com.zero_circle = 0.0083
*           flt.zero_circle = 0.0083
*        ---
*        The default value for pca.pcathresh indicated above will be
*        changed if it is too high to allow convergence of the I maps
*        within the number of iterations allowed by numiter (this change
*        only occurs if parameter SKYLOOP is FALSE).
*
*        If MASK is set to the name of an NDF, this script creates fixed
*        masks from the NDF, and the following parameters are added
*        to the above default config:
*        ---
*           ast.zero_mask = mask2
*           pca.zero_mask = mask3
*           com.zero_mask = mask3
*           flt.zero_mask = mask3
*        ---
*        The above "mask2" mask consists of clumps of pixel with SNR greater
*        than 3, extended down to an SNR level of 2. The "mask3" mask
*        consists of clumps of pixel with SNR greater than 5, extended
*        down to an SNR level of 3. However, the above SNR levels are
*        raised if necessary to ensure that the source occupies no more
*        than 20% of the pixels within the "ref" mask, and 10% of the
*        pixels within the "mask3" mask.
*
*        The same configuration is used for all three Stokes parameters -
*        I, Q and U with the exception that "com.noflag=1" is added to
*        the configuration when creating maps for Q and U.
*
*        If a configuration is supplied using parameter CONFIG, values
*        supplied for any of the above parameters will over-write the
*        values specified above. In addition, the following mandatory
*        values are always appended to the end of the used configuration:
*        ---
*           flagslow = 0.01
*           downsampscale = 0
*           noi.usevar=1
*        ---
*        If null (!) or "def" is supplied, the above set of default
*        configuration parameters are used without change. ["def"]
*     CALCQUCONFIG = LITERAL (Read)
*        Extra parameter values to include in the CALCQU configuration
*        used to create the I, Q and U time-streams. [!]
*     DEBIAS = LOGICAL (Given)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarization and polarized intensity in the output
*        vector catalogue specified by parameter CAT. The bias estimator
*        to use is given by parameter DEBIASTYPE. [FALSE]
*     DEBIASTYPE = LOGICAL (Given)
*        Only used if DEBIAS is TRUE. It gives the type of bias estimator
*        to use, using the nomeclature of Montier at al "Polarization
*        measurements analysis II. Best estimators of polarization
*        fraction and angle" (A&A, 2018):
*          - "AS": The asymptotic estimator. See section 2.3 of Montier
*             et al. This estimator produces bad P and PI values if the
*             squared PI value is less than the variance in PI.
*          - "MAS": The modified asymptotic estimator. See section 2.5 of
*             Montier et al. This estimator does not produces bad P and PI
*             values, even if the squared PI value is less than the
*             variance in PI.
*        ["AS"]
*     FCF = _REAL (Read)
*        The FCF value that is used to convert I, Q and U values from pW
*        to Jy/Beam. If a null (!) value is supplied a default value is
*        used that depends on the waveband in use - 725.0 for 850 um and
*        962.0 for 450 um. [!]
*     GLEVEL = LITERAL (Read)
*        Controls the level of information to write to a text log file.
*        Allowed values are as for "ILEVEL". The log file to create is
*        specified via parameter "LOGFILE. In adition, the glevel value
*        can be changed by assigning a new integer value (one of
*        starutil.NONE, starutil.CRITICAL, starutil.PROGRESS,
*        starutil.ATASK or starutil.DEBUG) to the module variable
*        starutil.glevel. ["ATASK"]
*     ICONFIG = LITERAL (Read)
*        Extra parameter values to include in the MAKEMAP configuration
*        used to create I maps. The values specified by "ICONFIG" are
*        applied after those specified by "CONFIG". [!]
*     ILEVEL = LITERAL (Read)
*        Controls the level of information displayed on the screen by the
*        script. It can take any of the following values (note, these values
*        are purposefully different to the SUN/104 values to avoid confusion
*        in their effects):
*
*        - "NONE": No screen output is created
*
*        - "CRITICAL": Only critical messages are displayed such as warnings.
*
*        - "PROGRESS": Extra messages indicating script progress are also
*        displayed.
*
*        - "ATASK": Extra messages are also displayed describing each atask
*        invocation. Lines starting with ">>>" indicate the command name
*        and parameter values, and subsequent lines hold the screen output
*        generated by the command.
*
*        - "DEBUG": Extra messages are also displayed containing unspecified
*        debugging information.
*
*        In adition, the glevel value can be changed by assigning a new
*        integer value (one of starutil.NONE, starutil.CRITICAL,
*        starutil.PROGRESS, starutil.ATASK or starutil.DEBUG) to the module
*        variable starutil.glevel. ["PROGRESS"]
*     IN = NDF (Read)
*        A group of input files. Each specified file must be one of the
*        following types:
*
*        - a raw POL-2 data file. Any supplied raw POL-2 data files will
*        be converted into time-series Q,U and I files using SMURF:CALCQU
*        and placed in the directory specified by parameter QUDIR. These
*        will then be converted into maps using SMURF:MAKEMAP, and placed
*        in the directory specified by parameter MAPDIR.
*
*        - a time-series file holding Stokes Q, U or I values. Any supplied
*        time-series files will be converted into individual maps (one for
*        each file) using SMURF:MAKEMAP, and placed in the directory
*        specified by parameter MAPDIR. These maps are created only for
*        the required Stokes parameters - as indicated by parameters
*        IOUT, QOUT and UOUT.
*
*        - a two-dimensional map holding Stokes Q, U or I values. Any
*        maps must be in units of pW. The final output I map is created by
*        coadding any supplied I maps with the I maps created by this script.
*        These coadded maps are created only for the required Stokes
*        parameters - as indiciated by parameters IOUT, QOUT and UOUT. Note,
*        an error is reported if the pixel size in any supplied map is not
*        equal to the value of parameter PIXSIZE.
*
*        Any combination of the above types can be supplied. Note, if
*        parameter REUSE is TRUE, then any required output files that
*        already exist in the directory specified by parameter MAPDIR
*        are re-used rather than being re-created from the corresponding
*        input data.
*     INITSKYI = NDF (Read)
*        An NDF holding an initial guess at the final I map. This should
*        contain any a priori expectations of what the final I map should look
*        like. It is used to define the starting point for the iterative map-making
*        algorithm, in place of the usual flat map full of zeros. The data units
*        in the supplied NDF must be "pW". See also parameter REF. Note, an
*        error is reported if the pixel size in the supplied map is not equal
*        to the value of parameter PIXSIZE. [!]
*     INITSKYQ = NDF (Read)
*        An NDF holding an initial guess at the final Q map. This should
*        contain any a priori expectations of what the final Q map should look
*        like. It is used to define the starting point for the iterative map-making
*        algorithm, in place of the usual flat map full of zeros. The data units
*        in the supplied NDF must be "pW". See also parameter REF. Note, an
*        error is reported if the pixel size in the supplied map is not equal
*        to the value of parameter PIXSIZE. [!]
*     INITSKYU = NDF (Read)
*        An NDF holding an initial guess at the final U map. This should
*        contain any a priori expectations of what the final U map should look
*        like. It is used to define the starting point for the iterative map-making
*        algorithm, in place of the usual flat map full of zeros. The data units
*        in the supplied NDF must be "pW". See also parameter REF. Note, an
*        error is reported if the pixel size in the supplied map is not equal
*        to the value of parameter PIXSIZE. [!]
*     IOUT = NDF (Write)
*        The output NDF in which to return the total intensity (I) map
*        including all supplied observations. This will be in units of pW.
*        Supply null (!) if the I map is not to be retained on exit. In
*        this case, the I map will only be created if it is needed
*        to create the output vector catalogue (see parameter CAT) and
*        will be deleted on exit.
*     IPCOR = _LOGICAL (Read)
*        If TRUE, then IP correction is used when creating Q and U maps,
*        based on the values in the total intensity map specified by
*        parameter IPREF. If FALSE, then no IP correction is performed.
*        The default is TRUE if any Q or U output maps are being created,
*        and FALSE otherwise. []
*     IPREF = NDF (Read)
*        The total intensity map to be used for IP correction. Only
*        accessed if parameter IPCOR is set TRUE. If null (!) is supplied
*        for IPREF, the map supplied for parameter REF is used. The map must
*        be in units of pW. If the same value is supplied for both IOUT
*        and IPREF, the output I map will be used for IP correction. Note, an
*        error is reported if the pixel size in the supplied map is not equal
*        to the value of parameter PIXSIZE. [!]
*     JY = _LOGICAL (Read)
*        If TRUE, the I, Q and U values in the output catalogue will be
*        in units of mJy/beam. Otherwise they will be in units of pW. Note,
*        the Q, U and I maps are always in units of pW. The same FCF value
*        is used to convert all three Stokes parameters from pW to mJy/beam,
*        derived from the value supplied for parameter FCF. [TRUE]
*     LOGFILE = LITERAL (Read)
*        The name of the log file to create if GLEVEL is not NONE. The
*        default is "<command>.log", where <command> is the name of the
*        executing script (minus any trailing ".py" suffix), and will be
*        created in the current directory. Any file with the same name is
*        over-written. The script can change the logfile if necessary by
*        assign the new log file path to the module variable
*        "starutil.logfile". Any old log file will be closed befopre the
*        new one is opened. []
*     MAPDIR = LITERAL (Read)
*        The name of a directory in which to put the Q, U an I maps made
*        from each individual observation supplied via "IN", before
*        coadding them. If
*        null is supplied, the new maps are placed in the same temporary
*        directory as all the other intermediate files and so will be
*        deleted when the script exists (unless parameter RETAIN is set
*        TRUE). Note, these maps are always in units of pW. Each one will
*        contain FITS headers specifying the pointing corrections needed
*        to align the map with the reference map. [!]
*     MAPVAR = _LOGICAL (Read)
*        Determines how the variance information in the final I, Q and
*        U coadded maps (parameters IOUT, QOUT and UOUT) are derived.
*
*        If MAPVAR is FALSE, the variances in the coadded maps are
*        calculated by propagating the variance information from the
*        individual observation maps. These variances are determined by
*        makemap and are based on the spread of bolometer I, Q or U
*        values that fall in each pixel of the individual observation
*        map.
*
*        If MAPVAR is TRUE, the variances in the coadded maps are
*        determined from the spread of input values (i.e. the pixel
*        values from the individual observation maps) that fall in each
*        pixel of the coadd.
*
*        The two methods produce similar variance estimates in the
*        background regions, but MAPDIR=TRUE usually creates much higher
*        on-source errors than MAPDIR=FALSE. Only use MAPDIR=TRUE if you
*        have enough input observations to make the variance between the
*        individual observation maps statistically meaningful. [FALSE]
*     MASK = LITERAL (Read)
*        Specifies the type of masking to be used within makemap (the
*        same type of masking is used to create all three maps - I, Q
*        and U):
*
*        - "AUTO": makemap uses automatically generated masks based
*        on the SNR map at the end of each iteration. The SNR levels
*        used are specified by the "xxx.ZERO_SNR" and "xxx.ZERO_SNRLO"
*        configuration parameters (see parameter CONFIG).
*
*        - "CIRCLE": makemap uses a fixed circular mask of radius 60
*        arc-seconds centred on the expected source position.
*
*        - Any other value is assumed to be a group of one or two NDFs
*        that specify the "external" AST and PCA masks to be used. The
*        way in which these NDFs are used depends on the value of
*        parameter MASKTYPE. These NDFs must be aligned in pixel
*        coordinates with the reference map (parameter REF) Note, an
*        error is reported if the pixel size in the supplied map is not
*        equal to the value of parameter PIXSIZE.
*
*        ["AUTO"]
*     MASKOUT1 = LITERAL (Write)
*        If a non-null value is supplied for MASKOUT, it specifies the NDF
*        in which to store the AST mask created from the NDF specified by
*        parameter MASK. Only used if an NDF is supplied for parameter
*        MASK. [!]
*     MASKOUT2 = LITERAL (Write)
*        If a non-null value is supplied for MASKOUT, it specifies the NDF
*        in which to store the PCA mask created from the NDF specified by
*        parameter MASK. Only used if an NDF is supplied for parameter
*        MASK. [!]
*     MASKTYPE = LITERAL (Read)
*        Specifies the way in which NDFs supplied for parameter MASK
*        are to be used. This parameter can be set to either of the
*        following values:
*
*        - "Signal": A single NDF should be supplied for parameter MASK
*        holding the astronomical signal level at each pixel within the
*        astronomical field being mapped. It can be in any units, but
*        must have a Variance component. The AST and PCA masks are
*        created from this map by finding all clumps of contiguous pixels
*        above a fixed SNR limit, and then extending these clumps down to
*        a lower SNR limit. For the AST model, the upper and lower SNR
*        limits default to 3.0 and 2.0. For the PCA mask, the defaults are
*        5.0 and 3.0. These defaults can be over-ridden by supplying
*        values for AST.ZERO_SNR, AST.ZERO_SNRLO, PCA.ZERO_SNR and
*        PCA.ZERO_SNRLO within the configuration specified by parameter
*        CONFIG. The AST and PCA masks created in this way can be saved
*        using parameters MASKOUT1 and MASKOUT2.
*
*        - "Mask": A pair of NDFs should be supplied for parameter MASK,
*        each holding a mask in which background pixels have bad values
*        and source pixels have good values. The first supplied NDF is
*        used directly as the AST mask, and the second is used as the PCA
*        mask.
*
*        ["Signal"]
*     MSG_FILTER = LITERAL (Read)
*        Controls the default level of information reported by Starlink
*        atasks invoked within the executing script. This default can be
*        over-ridden by including a value for the msg_filter parameter
*        within the command string passed to the "invoke" function. The
*        accepted values are the list defined in SUN/104 ("None", "Quiet",
*        "Normal", "Verbose", etc). ["Normal"]
*     MULTIOBJECT = _LOGICAL (Read)
*        Indicates if it is acceptable for the list of input files to
*        include data for multiple objects. If FALSE, an error is reported
*        if data for more than one object is specified by parameter IN.
*        Otherwise, no error is reported if multiple objects are found.
*        [FALSE]
*     NEWMAPS = LITERAL (Read)
*        The name of a text file to create, in which to put the paths of
*        all the new maps written to the directory specified by parameter
*        MAPDIR (one per line). If a null (!) value is supplied no file is
*        created. [!]
*     NORMALISE = _LOGICAL (Read)
*        If TRUE, scale corrections for individual observations found in
*        any pre-existing auto-masked maps (e.g. made on a previous run
*        of this script) are applied when creating new maps. If False,
*        no scale corrections are applied. Scale correction factors are
*        created and stored at the same time as the pointing corrections.
*        The correction factor for a single observation is found by comparing
*        the data values in the map made from the single observation with
*        those in the coadd map made from all observation. The comparison
*        is limited to the areas inside the AST mask, and a factor of
*        unity is used for any observation that is not well correlated to
*        the coadd). The factor found in this way is stored in the FITS
*        extension of the map made  from the observation (header "CHUNKFAC").
*        [FALSE]
*     NORTH = LITERAL (Read)
*        Specifies the celestial coordinate system to use as the reference
*        direction in any newly created Q and U time series files. For
*        instance if NORTH="AZEL", then they use the elevation axis as the
*        reference direction, and if "ICRS" is supplied, they use the ICRS
*        Declination axis. If "TRACKING" is supplied, they use north in the
*        tracking system - what ever that may be. ["TRACKING"]
*     OBSWEIGHT = _LOGICAL (Write)
*        This parameter affects how maps from separate observations are
*        weighted when they are combined together to form a coadd. If it is
*        FALSE, each pixel in each map is weighted simply using the reciprocal
*        of the Variance value stored in the map. If it is TRUE, an extra
*        factor is included in the pixel weights that is constant for all
*        pixels in a map  but varies from observation to observation. In
*        other words, each observation is assigned a weight, which is used
*        to factor the pixel weights derived from the Variance values.
*        The purpose of this per-observation weight is to down-weight
*        observations that are very different to the other observations
*        and which would therefore contribute to a high Variance if parameter
*        MAPVAR is set TRUE. These weights are proportional to 1/(RMS*RMS),
*        where "RMS" is the RMS residual between an individual observation
*        map and the coadd of all observation maps, after they have been
*        aligned spatially to take account of any pointing error in the
*        individual observation. See also parameter WEIGHTLIM.
*
*        If skyloop is used (see parameter SKYLOOP) with OBSWEIGHT=YES,
*        then a set of observation maps must already exist in the MAPDIR
*        directory that were also created with OBSWEIGHT=YES. The weights
*        to be used by skyloop are read from these maps, which would normally
*        have been created by a prior run of this script.
*
*        WARNING: This option should only be used if the number of
*        observation being processed is sufficiently large to allow
*        aberrant observations to be identified with a reasonable degree
*        of confidence. [FALSE]
*     PIXSIZE = _REAL (Read)
*        Pixel dimensions in the output I, Q and U maps, in arcsec. The
*        default is 4 arc-sec for both 450 and 850 um data. The bin size for
*        the output catalogue can be specified separately - see parameter
*        BINSIZE and CAT.  Note the pixel size within any maps supplied as
*        input to this script should equal the value of parameter PIXSIZE.
*        An error will be reported if this is not the case. [4]
*     QOUT = NDF (Write)
*        The output NDF in which to return the Q map including all supplied
*        observations. This will be in units of pW. Supply null (!) if no Q
*        map is required.
*     QUCONFIG = LITERAL (Read)
*        Extra parameter values to include in the MAKEMAP configuration
*        used to create Q and U maps. The values specified by "QUCONFIG"
*        are applied after those specified by "CONFIG". [!]
*     QUDIR = LITTERAL (Read)
*        The name of a directory in which to put the Q, U and I time series
*        generated by SMURF:CALCQU, prior to generating maps from them. If
*        null (!) is supplied, they are placed in the same temporary directory
*        as all the other intermediate files and so will be deleted when the
*        script exists (unless parameter RETAIN is set TRUE). [!]
*     REF = NDF (Read)
*        An optional map defining the pixel grid for the output maps,
*        and which is used to determine pointing corrections. If null
*        (!) is supplied, then the map (if any) specified by parameter
*        MASK is used. Note, an error is reported if the pixel size in the
*        supplied map is not equal to the value of parameter PIXSIZE.
*
*        The value of the REF parameter is ignored if an NDF is supplied
*        for one or more of parameters INITSKYI, INITSKYQ or INITSKYU. In
*        such cases the first such NDF is used to define the pixel grid
*        for all output maps. [!]
*     REUSE = _LOGICAL (Read)
*        If TRUE, then any output maps or time-treams that already exist
*        (for instance, created by a previous run of this script) are re-used
*        rather than being re-created from the corresponding input files.
*        If FALSE, any previously created output maps or time-streams are
*        ignored and new ones are created from the corresponding input
*        files. [TRUE]
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]
*     SKYLOOP = _LOGICAL (Read)
*        Should the skyloop script be used in place of makemap to create the
*        maps from the I, Q and U time-series data? Note, when using skyloop
*        it is not possible to add in new observations to an existing
*        collection of I, Q and U maps - all observations must be processed
*        together. Therefore the value supplied for parameter REUSE will be
*        ignored and a value of FALSE assumed if the MAPDIR directory is
*        missing maps for any of the supplied observations. [FALSE]
*     SMOOTH450 = _LOGICAL (Read)
*        This parameter is only accessed if the input files specified
*        by parameter IN contain 450 um data. If SMOOTH450 is TRUE, the
*        450 um I, Q and U coadd maps will be smoothed prior to creating
*        any output vector catalogue so that the smoothed maps have the
*        850 um beam shape. This changes the resolution of the maps from
*        (approximately) 8 arc-sec (the 450 um beam width) to
*        (approximately) 13 arc-seconds (the 850 um beam width), and
*        also reduces the noise. The smoothing kernel is derived from the
*        two-component beam models described in the paper "SCUBA-2: on-sky
*        calibration using submillimetre standard sources" (Dempsey et al,
*        2018). If parameter MAPVAR is TRUE, the individual observation
*        maps will be smoothed prior to determining the variances, thus
*        ensuring that the resulting variances are still accurate. [FALSE]
*     TRIM = _REAL (Read)
*        This indicates how the edges of the final I, Q and U coadds should
*        be trimmed to remove the noisey edges. If a null (!) value is
*        supplied (the default), no trimming is performed. Otherwise, the
*        supplied value indicates the fraction of the mean expsoure time
*        at which the coadd should be trimmed. For instance, a value of 0.2
*        causes pixels to be set bad if the number of usable bolometer
*        samples that fall within the pixel is less than 0.2 times the mean
*        number of samples per pixel, taken over the whole coadd (excluding
*        parts that receive no samples at all). [!]
*     UOUT = NDF (Write)
*        The output NDF in which to return the U map including all supplied
*        observations. This will be in units of pW. Supply null (!) if no U
*        map is required.
*     WEIGHTLIM = _REAL (Read)
*        The lowest usable weight (see parameter OBSWEIGHT). Any observation
*        that has a weight below this value will not be included in the
*        final coadded I, Q or U maps or in the vector catalogue. This
*        can be useful since observations with very low weight can sometimes
*        cause makemap to crash. [0.05]

*  Copyright:
*     Copyright (C) 2017,2018 East Asian Observatory.
*     All Rights Reserved.

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
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     25-JAN-2017 (DSB):
*        Original version
*     27-FEB-2017 (DSB):
*        If makemap fails, ensure any resulting map is deleted.
*     3-MAY-2017 (DSB):
*        - Add parameter MAPVAR.
*        - Re-structure so that coadded maps can be created from a list
*        of input observation maps.
*        - Use kappa:wcsmosaic to create the coadded maps rather than
*        ccdpack:makemos. This is because wcsmosaic allows input variance
*        to be used for weighting, whilst  generating output variances from
*        the spread of the input values. makemos does not allow this
*        combination.
*     20-JUN-2017 (DSB):
*        - If parameter JY is true, create output values in mJy/beam,
*        rather than Jy/beam.
*     12-JUL-2017 (DSB):
*        Added parameters ICONFIG and QUCONFIG.
*     24-AUG-2017 (DSB):
*        When checking whether "modelorder" contains PCA in the user
*        supplied config, allow for the possibility that the config may contain
*        no modelorder value at all. This bug prevented the abortsoon
*        parameter being used with makemap in the majority of cases.
*     7-FEB-2018 (DSB):
*        Some maps have low correlation with the reference but can still
*        be aligned successfully using kappa:align2d. So remove the
*        correlation limit when running align2d. But guard against cases
*        where the map cannot be masked (in which case align2d may be
*        influenced by the bright noisey edge pixels) by masking off the
*        edges using the EXP_TIME values.
*     9-FEB-2018 (DSB):
*        Remove hardwired assumption of 850 data when looking for
*        pre-existing I,Q,U time-stream data, and when checking value of
*        NUMITER in the supplied config.
*     21-FEB-2018 (DSB):
*        Change default for PIXSIZE to 4 arc-seconds at both 450 and 850.
*     27-FEB-2018 (DSB):
*        Added SKYLOOP parameter.
*     13-MAR-2018 (DSB):
*        Added parameter OBSWEIGHT.
*     9-APR-2018 (DSB):
*        - Avoid infinite findclumps loop if no emission found in supplied mask.
*        - Added parameter MULTIOBJECT.
*     10-APR-2018 (DSB):
*        Added parameter NORMALISE.
*     17-MAY-2018 (DSB):
*        Check that weights are available in the auto-masked I maps if
*        this script is run with "skyloop=yes obsweight=yes". skyloop
*        requires CHUNKWGT values in the input time-stream data. These
*        are copied into the time-series data from the pre-calculated
*        automasked I maps.
*     10-JUL-2018 (DSB):
*        Added parameter WEIGHTLIM, to avoid observations with silly
*        CHUNKWGT and/or CHUNKFAC causing skyloop to abort.
*     2-OCT-2018 (DSB):
*        Added parameter TRIM.
*     15-OCT-2018 (DSB):
*        When skyloop=yes, ensure pointing correction ranges do not overlap.
*     18-OCT-2018 (DSB):
*        The PCA mask was previously limited to be no larger than half the
*        size of the AST mask. This was a bug. What was intended was
*        that the AST mask should be no more than 20% of the good pixels,
*        and the PCA mask should be no more than 10% of the good pixels.
*     9-JAN-2019 (DSB):
*        - When processing old (pre 20150930) and new data together in skyloop,
*        ensure that the pointing correction required for old data is not also
*        applied to the new data.
*        - Fix bug that caused wrong pointing corrections to be used when
*        skyloop is used to process multiple observations that do not all have
*        the same reference point.
*        - Prevent POINT_DX/Y FITS headers being stored in maps if no pointing
*        correction was used.
*    29-JAN-2019 (DSB):
*        Remove any third (wavelength) axis from the reference map. For
*        instance, this allows 450 um data to be aligned with a reference
*        map made from 850 um data.
*    13-FEB-2019 (DSB):
*        Always find the pointing correction by aligning with the I map made from
*        the same data, rather than using the supplied REF map (which may have different
*        structure and so may produce sub-optimal pointing corrections).
*    14-FEB-2019 (DSB):
*        As a consequence of the previous change (13-FEB-2019) the pixel
*        values in the REF map are no longer used for anything (the
*        pixel positions are the only thing used from the REF map), so
*        there is now no need to ensure that the REF map is in units of pW.
*    28-MAR-2019 (DSB):
*        - If an output catalogue is being created, but no output I map has been
*        requested, use any supplied IPREF map in favour of creating a new I map.
*        - If we are processing I data with makemap (i.e. "step 1"), and no ref
*        map was given, then use the I map created from the first observation as
*        the ref map for the remaining observations. This ensures that all the
*        auto-masked I maps are aligned with each other.
*    13-JUN-2019 (DSB):
*       Ensure units ("pW") are propagated to the output I, Q and U
*       coadded NDFs (gy default, kappa:maths does not propagatre units).
*    14-JUN-2019 (DSB):
*       - Ensure that any auto-masked maps for which no pointing correction
*       can be determined are nevertheless assigned a valid CHUNKCFAC value.
*       - If NORMALISE=YES, issue a warning if no CHUNKFAC header is found in
*       any input I map. Previously, pol2map would crash in such cases.
*       - Abort if reuse is true and we are about to over-write an existing map
*       (this can only happen if skyloop=yes). This can occur, for instance, if
*       the user forgets to set the MASK parameter on step 2.
*    9-JUL-2019 (DSB):
*       If an output catalogue is being created, but IOUT is null, then
*       the supplied IPREF map is used as the total intensity map when
*       creating the vectors. In this case, do not check that the individual
*       observation I maps exist since they have not been requested and
*       may not be available. Previously such checks caused pol2map to
*       abort if any of the individual observation I maps were missing.
*    10-JUL-2019 (DSB):
*       When using cupid:findclumps to generate AST and PCA masks from the
*       signal map specified by parameter MASK (with MASKTYPE=Signal), the
*       SNR thresholds were previously hard wired values ([3,2] for AST,
*       [5,3] for PCA). These values are still the defaults, but they can
*       now be over-ridden by supplying values via the CONFIG parameter for
*       AST.ZERO_SNR, AST.ZERO_SNRLO, PCA.ZERO_SNR and PCA.ZERO_SNRLO.
*    22-JUL-2019 (DSB):
*       Fix major SKYLOOP bug that caused pol2map to crash with error
*       message "name 'need_obsmaps' is not defined" unless pre-existing
*       I,Q,U maps were found.
*    13-AUG-2019 (DSB):
*       No need to create the coadd of I, Q or U maps if it already exists
*       and has not been surplanted by a new set of observation maps.
*    9-OCT-2019 (DSB):
*       Fixed bad bug that caused output catalogue values to be completely
*       wrong if a previously created coadd was reused AND a binned-up
*       output catalogue is being created (i.e. CAT is non-null, BINSIZE
*       is non-null and one or more of IOUT, QOUT and UOUT is null). The
*       problem was that the existing coadd was not binned up prior to
*       creating the catalogue, so the association between corresponding
*       (I,Q,U) values was lost. In other words, the wrong set of I, Q
*       and U values were being combined together to create each pair of
*       (P,ANG) values.
*    28-NOV-2019 (DSB):
*       Ensure the returned I, Q and U maps have the same bounds.
*    5-DEC-2019 (DSB):
*       If an existing I coadd is supplied (either explicitly or as the
*       IP reference map), and an output catalogue is being created, and
*       BINSIZE > PIXSIZE, and MAPVAR=yes, then the individual obs I maps
*       are needed (they are binned up to BINSIZE before being coadded to
*       form the I coadd used to create the catalogue). If they already
*       exist then they are re-used (if REUSE=yes). Otherwise, they need
*       to be re-created form the I time-series. The test to see if the
*       individual I obs maps already exist used to test for I obs maps
*       that cannot be created. For instance, if an observation was
*       split into two chunks - one long and one very short - then it
*       will probably not be possible to create an I map from the short
*       chunk, so the test should not look for such an I map. The
*       previous test failed in such a situation, causing all the
*       observation I maps to be re-created unnecessarily. In fact there
*       is no way that I can see to get a list of the expected I obs maps
*       that is 100% reliable. So the new test just checks that I obs
*       maps exist for more than 50% of the observation chunks. In
*       practice, it's probably going to be a case of all exist or none
*       exist.
*    16-DEC-2019 (DSB):
*       - Bug fix for 5-Dec-2019 change (the the sense of the check was
*       inverted).
*       - Added parameter SMOOTH450.
*    7-JAN-2020 (DSB):
*       Retain variances outside the mask if smooth450=yes and skyloop=yes
*    10-FEB-2020 (DSB):
*       When calculating the normalisation factors to be used on a subsequent
*       run of pol2map with NORMALISE=YES, assume a factor of 1.0 (i.e. no
*       change) if the correlation between the coadd and the individual
*       observation is too low (below 0.95).
*    1-APR-2020 (DSB):
*       Added parameter DEBIASTYPE.
*    20-APR-2020 (DSB):
*       Add column AST to the output catalogue if an AST mask is available.
*    15-APR-2020 (DSB):
*       Added parameter CALCQUCONFIG.
*    27-NOV-2020 (DSB):
*       When using sqorst to create the catalogue reference map, use
*       "centre=origin" so that he pixel origin retains ist sky position.
*       Without this, changes in map bounds (such as seen between 450 and 850
*       maps from the same observation) could cause the vector catalogues
*       to use slightly different vector positions.
*    9-MAR-2021 (DSB):
*       When using makemap (i.e. not skyloop), we do not need to re-create
*       the observation maps if the coadd already exists and the observation
*       maps are not needed to create the catalogue.
*    12-MAR-2021 (DSB):
*       - Remove unused REFFCF parameter.
*       - Incorporate POL-2 degradation factor of 1.35 when converting
*       non-POL2 I maps from pW to mJy/beam prior to creating the catalogue.
*    22-APR-2021 (DSB):
*       The AST column in the final catalogue now contains distinct non-zero
*       integers for vectors within each island of emission in the AST mask,
*       and FITS null values for vectors not within any AST island. Previously,
*       all islands used the same value "1" and background vectors used "0".
*    1-JUN-2021 (DSB):
*       - Check all supplied input maps have the pixel size specified by PIXSIZE.
*       - Report an error if mapvar=yes and skyloop=yes, but there are fewer
*       than 3 observations.
*    4-JUN-2021 (DSB):
*       Add a PCA column to the final catalogue that indicates the island
*       within the PCA mask that contains each vector.

'''

import glob
import os
import math
import shutil
import starutil
import numpy as np
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out
from starutil import msg_log
from starutil import append_logfile
from starutil import AtaskError
from starutil import get_fits_header
from starutil import get_task_par

#  Assume for the moment that we will not be retaining temporary files.
retain = 0

#  An NDG holding the smoothing kernel that smooths the 450 um beam to
#  create the 850 um beam.
kernel = None


#  A function to check the pixel size and units of a map and report an error
#  if either is not the required value.
def CheckNDF( param, ndf, pixsize, units ):
   if ndf:
      invoke("$KAPPA_DIR/ndftrace ndf={0}".format(ndf) )
      if pixsize:
         xsize = float(get_task_par( "FPIXSCALE(1)", "ndftrace" ))
         ysize = float(get_task_par( "FPIXSCALE(2)", "ndftrace" ))
         if abs( pixsize - xsize ) > 0.05 or abs( pixsize - ysize ) > 0.05:
            if xsize == ysize:
               raise starutil.InvalidParameterError("NDF '{0}' supplied for "
                  "parameter {1} has pixel size {2} arcsec, but parameter PIXSIZE "
                  "has value {3} arcsec.".format(ndf,param,xsize,pixsize))
            else:
               raise starutil.InvalidParameterError("NDF '{0}' supplied for "
                  "parameter {1} has pixel dimensions ({2},{3}) arcsec, but "
                  "parameter PIXSIZE has value {4} arcsec.".format(ndf,param,xsize,ysize,pixsize))
      if units:
         value = starutil.get_task_par( "UNITS", "ndftrace" ).replace(" ", "")
         if value != units:
               raise starutil.InvalidParameterError("NDF '{0}' supplied for "
                  "parameter {1} has units '{2}' - units must be '{3}'."
                  .format(ndf,param,value,units))


#  A function to create an NDF holding a two-component beam shape, as
#  defined in Dempsey et al 2018, using the supplied parameter values.
def Beam( alpha, beta, thetaM, thetaS, pixsize ):

#  Create an image of the main beam using the supplied pixel size.
#  Centre the Gaussian at the centre of pixel (1,1) (i.e. pixel
#  coords (0.5,0.5) ). The Gaussian has a peak value of 1.0.
   main = NDG( 1 )
   invoke( "$KAPPA_DIR/maths exp=\"'exp(-4*pa*(fx*fx+fy*fy)/(fb*fb))'\" "
           "fx='xa-px' fy='xb-py' fb='pb/pp' pp={0} px=0.5 py=0.5 "
           "pa=0.6931472 pb={1} out={2} type=_double lbound=\[-63,-63\] "
           "ubound=\[64,64\]".format( pixsize, thetaM, main ))

#  Similarly create an image of the secondary beam.
   sec = NDG( 1 )
   invoke( "$KAPPA_DIR/maths exp=\"'exp(-4*pa*(fx*fx+fy*fy)/(fb*fb))'\" "
           "fx='xa-px' fy='xb-py' fb='pb/pp' pp={0} px=0.5 py=0.5 "
           "pa=0.6931472 pb={1} out={2} type=_double lbound=\[-63,-63\] "
           "ubound=\[64,64\]".format( pixsize, thetaS, sec ))

#  Combine them in the right proportions to make the total beam.
   total = NDG( 1 )
   invoke( "$KAPPA_DIR/maths exp="'pa*ia+pb*ib'" out={0} ia={1} ib={2} "
           "pa={3} pb={4}".format( total, main, sec, alpha, beta ) )

#  Ensure the total 450 beam has a total data sum of 1.0.
   invoke("$KAPPA_DIR/stats ndf={0}".format(total))
   sum = float( get_task_par( "total", "stats" ) )
   result = NDG( 1 )
   invoke( "$KAPPA_DIR/cdiv in={0} scalar={1} out={2}".
           format( total, sum, result ))

#  Return the final beam.
   return result



#  Function to smooth a map using a kernel that converts the 450 um
#  beam to the 850 um beam.
def Smooth450( inmap, outmap ):
   global kernel

#  If "in" or "out" is a string, create an NDG from it.
   if isinstance( inmap, str):
      inmap = NDG( inmap )
   if isinstance( outmap, str):
      outmap = NDG( outmap, False )

#  If we have not yet created the moothing kernel, do it now.
   if kernel is None:

#  Get the input pixel size.
      invoke("$KAPPA_DIR/ndftrace ndf={0}".format(inmap) )
      xsize = float(get_task_par( "FPIXSCALE(1)", "ndftrace" ))
      ysize = float(get_task_par( "FPIXSCALE(2)", "ndftrace" ))
      pixsize = math.sqrt( xsize*ysize )

#  Create a pair of NDFs holding the expected model beam shape at
#  450 um and at 850 um. The expected beams are defined in Dempsey et al
#  2018.
      b450 = Beam( 0.94, 0.06, 7.9, 25.0, pixsize )
      b850 = Beam( 0.98, 0.02, 13.0, 48.0, pixsize )

#  Deconvolve the 850 um beam using the 450 um beam as the PSF. The
#  resulting map is the kernel that smooths a 450 um map so that the
#  result has the 850 um beam.
      kernel = NDG( 1 )
      invoke( "$KAPPA_DIR/wiener in={0} psf={1} out={2} xcentre=1 ycentre=1".
              format( b850, b450, kernel ))

#  Use the kernel to smooth the input map.
   invoke( "$KAPPA_DIR/convolve in={0} out={1} psf={2} xcentre=1 ycentre=1".
           format( inmap, outmap, kernel ))




#  Trim a map to set pixels bad if they have an exposure time less than
#  a given fraction of the mean expsoure time.
def exptrim(map,trim,out=None):
   invoke("$KAPPA_DIR/stats ndf={0}.more.smurf.exp_time".format(map) )
   mean = float( get_task_par( "MEAN", "stats" ))
   if out is None:
      result = NDG(1)
   else:
      result = out
   invoke( "$KAPPA_DIR/maths exp=\"'qif((ib.ge.pa),ia,<bad>)'\" "
           "ib={0}.more.smurf.exp_time ia={0} pa={1} out={2} units=yes".
           format(map,mean*trim,result) )
   return result

#  Return the median value in a supplied list.
def median(lst):
   n = len(lst)
   if n < 1:
      return None
   if n % 2 == 1:
      return sorted(lst)[n//2]
   else:
      return sum(sorted(lst)[n//2-1:n//2+1])/2.0

#  A function to find the PCA.PCATHRESH value used to create the most
#  recent auto-masked or ext-masked map.
def getPcaThresh( mapdir, automask ):
   if automask:
      base = "[iqu]map"
   else:
      base = "[IQU]map"

   pcathresh = 0
   this_map = None
   nsecmin = 1E30
   for tmap in glob.glob("{0}/*{1}.sdf".format(mapdir, base )):
      nsec = os.path.getmtime( tmap )
      if nsec < nsecmin:
         try:
            pcathresh = float( invoke("$KAPPA_DIR/configecho name=pca.pcathresh "
                                      "ndf={0} config=! application=makemap "
                                      "defaults=$SMURF_DIR/smurf_makemap.def"
                                      .format(tmap)))
            nsecmin = nsec
            this_map = tmap
         except starutil.AtaskError:
            pass

   return (pcathresh,this_map)



#  A function to clean up before exiting. Delete all temporary NDFs etc,
#  unless the script's RETAIN parameter indicates that they are to be
#  retained. Also delete the script's temporary ADAM directory.
def cleanup():
   global retain
   if retain:
      msg_out( "Retaining temporary files in {0}".format(NDG.tempdir))
   else:
      NDG.cleanup()
   ParSys.cleanup()


#  A function to create the coadd of all the observation maps. If I data
#  is being processed, it also determines pointing corrections that align
#  each individual I map with the coadd of all I maps. It then determines
#  a weight for each observation by comparing the aligned individual I map
#  for the observation with the I coadd - observations that are similar
#  to the coadd get higher weight. The weight for each observation is
#  stored in the CHUNKWGT header within the FITS extension of the
#  observation's I map.
def MakeCoadd( qui, qui_maps, i_maps, coadd, mapvar, automask, obsweight ):
   allmaps = []
   allkeys = []
   for key in qui_maps:
      allmaps.append( qui_maps[key] )
      allkeys.append( key )

   if len(allmaps) < 3 and mapvar:
      raise starutil.InvalidParameterError("Cannot combine {0} maps "
                           "using MAPVAR=YES since only {1} {0} maps "
                           "are available".format(qui, len(allmaps) ) )

   allmaps = NDG( allmaps )

#  No observation weighting.
   if not obsweight:
      msg_out("Coadding {0} maps from all observations with no weighting:".format(qui))

#  Form the coadd using equal weights for all observations.
      invoke("$KAPPA_DIR/wcsmosaic in={0} lbnd=! ref=! out={1} "
             "conserve=no method=near variance=yes genvar={2} "
             .format(allmaps,coadd,mapvar))

#  If we are processing I data, determine pointing and calibration corrections
#  by comparing each individual observation map with the new coadd. These
#  corrections are stored in the FITS headers of the observation I maps. We
#  determine the corrections from the I data since it is brighter than Q or U,
#  and then apply the same corrections to the Q and U data when creating maps
#  from the Q and U time streams.
      if qui == "I":
         StoreCorrections( qui_maps, coadd )

#  If we are weighting observations separately, first deal with I data.
   elif qui == "I":
      msg_out("Coadding I maps with a separate weight for each observation:")

#  Initialise the weights for all observations to 1.0. The weights are
#  stored in a text file so that they can be read by wcsmosaic.
      wfile = NDG.tempfile()
      with open( wfile, "w" ) as fd:
         for i in range(len(qui_maps)):
            fd.write("1.0\n")

#  Loop until convergence.
      this_coadd = None
      more = True
      meanerr_last = 1E30
      iter = 0
      while more:
         iter += 1
         msg_out("   Iteration {0}:".format(iter))

#  Form the coadd, using the current weights.
         previous_coadd = this_coadd
         this_coadd = NDG(1)
         if iter == 1:
            msg_out("     Making new coadd using unit weights...")
         else:
            msg_out("     Making new coadd using improved weights...")
         invoke("$KAPPA_DIR/wcsmosaic in={0} lbnd=! ref=! out={1} "
                "conserve=no method=near variance=yes genvar={2} "
                "weights=^{3}".format(allmaps,this_coadd,mapvar,wfile))

#  Report the mean error within the central 40 pixels of the coadd
         invoke("$KAPPA_DIR/stats ndf={0}'(0~40,0~40)' comp=err quiet=yes".format(this_coadd) )
         meanerr = float( get_task_par( "MEAN", "stats" ))
         msg_out("     Typical error at centre of new coadd = {1}".format(iter,meanerr))

#  Decide if converged. If so copy the current coadd to the required file
#  and leave the loop retaining the current pointing corrections and
#  weights.
         if meanerr > 0.99*meanerr_last:
            invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(this_coadd,coadd))
            msg_out("\n   Converged")
            more = False

#  If not converged, record the previous error level and find new pointing
#  corrections and weights based on the new improved coadd.
         else:
            meanerr_last = meanerr

#  Determine pointing corrections and weights by comparing each
#  individual oibservation map with the new coadd. The pointing
#  corrections are stored in the FITS headers of the observation maps.
#  The weights are returned in a list.
            weights = StoreCorrections( qui_maps, this_coadd )

#  Store the new weights in the weights file and in the I maps. Ensure
#  the order of the values in the weights file matches the order of the maps
#  in the allmaps NDG object.
            wfile = NDG.tempfile()
            with open( wfile, "w" ) as fd:
               msg_out("     Improved weights derived using new coadd:")
               for key in allkeys:
                  wgt = weights[key]
                  invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=CHUNKWGT "
                         "edit=a value={1} comment=\"'Weight for this chunk of data'\""
                         " position=! mode=interface".format(qui_maps[key],wgt))
                  msg_out("       {0}: {1}".format(key, wgt))
                  fd.write( "{0}\n".format(wgt) )
            msg_out(" ")


#  Now deal with Q and U data. These use the weights stored in the
#  corresponding I (ext-masked or auto-masked) maps.
   else:
      msg_out("Coadding {0} maps with a separate weight for each observation:".format(qui))
      wfile = NDG.tempfile()
      allone = True
      with open( wfile, "w" ) as fd:
         for key in allkeys:

#  If the supplied set of I maps does not contain the current
#  observation, attempt to guess the location of the I map from which the
#  weight for this observation should be read - the corresponding "imap"
#  if using auto-masking or the corresponding "Imap" otherwise.
            try:
               if key in i_maps:
                  indf = i_maps[key]
               else:
                  if automask:
                     gexp = "{0}|{1}map|imap|".format( qui_maps[key], qui.lower() )
                  else:
                     gexp = "{0}|{1}map|Imap|".format( qui_maps[key], qui.upper() )
                  indf = NDG( gexp )
               wgt = get_fits_header( indf, "CHUNKWGT", report=True )
            except Exception:
               indf = None
               wgt = 1.0
            fd.write("{0}\n".format(wgt))
            if float(wgt) < 1.0:
               allone = False
            if indf is None:
               msg_out("   {0}: 1.0 (default - no previous I map found)".format(key))
            else:
               msg_out("   {0}: {1} (from {2})".format(key, wgt, indf))

      if allone:
         weights = "!"
      else:
         weights = "^{0}".format(wfile)

      invoke("$KAPPA_DIR/wcsmosaic in={0} lbnd=! ref=! out={1} "
             "conserve=no method=near variance=yes genvar={2} "
             "weights={3}".format(allmaps,coadd,mapvar,weights))





#  Function to determine pointing correction, calibration correction and
#  weight for each individual observation by comparing the observation's
#  I map with the coadd of all I maps.
def StoreCorrections( qui_maps, imap ):

#  Choose the map with which to align each of the new I maps. We use the
#  I mosaic so long as it contains more than one observation (if there is
#  only one observation then the aligment is bound to be a unit transformation
#  because the mosaic and the individual map will be identical).
   if len(qui_maps) > 1 and imap:
      aref = imap
   else:
      aref = "!"

#  Initialise the returned weights.
   weights = {}
   swgt = 0.0
   nwgt = 0

#  Loop round all observations.
   comment = None
   msg_out( "     Improved pointing corrections derived using new coadd:" )
   for key in qui_maps:

#  Can't align if we have no reference.
      if aref == "!":
         continue

#  Decide on a suitable comment to store with the PNTRQ_DX/DY FITS headers,
#  indicating if the recorded value is an extra remaining error that needs
#  to be added to the values already stored in the POINT_DX/DY headers, or
#  is a first estimate at the total pointing error. We do this for the
#  first observation and then use the same comment for all subsequent
#  observations.
      if comment is None:
         there = invoke("$KAPPA_DIR/fitsmod ndf={0} edit=exist keyword=POINT_DX".
                        format( qui_maps[key] ), False ).strip()
         if there == "TRUE":
            comment = "Remaining {0} pointing error [arcsec]"
            scomment = "Relative calibration factor"
         else:
            comment = "Required {0} pointing correction [arcsec]"
            scomment = "Relative calibration factor"

#  The determination of the pointing correction is more accurate if we
#  first mask out background areas. Use the AST mask to define source pixels,
#  but only if the mask contains a reasonable number of pixels (very faint
#  sources will have very small or non-existant AST masks).
      invoke("$KAPPA_DIR/showqual ndf={0}".format(qui_maps[key]))
      if get_task_par( "QNAMES(1)", "showqual" ) == "AST":
         bb = 1
      elif get_task_par( "QNAMES(2)", "showqual" ) == "AST":
         bb = 2
      elif get_task_par( "QNAMES(3)", "showqual" ) == "AST":
         bb = 4
      else:
         bb = 0

      if bb > 0:
         invoke("$KAPPA_DIR/setbb ndf={0} bb={1}".format(qui_maps[key],bb))

#  Clear badbits to use the whole map if the above masking results in too
#  few pixels, and instead mask the map to remove pixels that have less
#  than the mean exposure time per pixel.
      invoke("$KAPPA_DIR/stats ndf={0}".format(qui_maps[key]))
      nused = float( get_task_par( "numgood", "stats" ) )
      if nused < 400:
         invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(qui_maps[key]))
         aligner = exptrim( qui_maps[key], 1.0 )
      else:
         aligner = qui_maps[key]

#  The individual observation map may not use the same WCS as the coadd
#  (e.g. different observations of the same source may use different
#  reference positions), so now align the observation map with the coadd
#  so that they both use the same pixel grid.
      aligner_A = NDG( 1 )
      invoke("$KAPPA_DIR/wcsalign in={0} lbnd=! out={1} ref={2} "
             "conserve=no method=sincsinc params=\[2,0\] rebin=yes".
             format(aligner,aligner_A,aref))

#  Find the pixel shift that aligns features in this masked, trimmed,
#  aligned I map with corresponding features in the reference map. Also
#  get the RMS residual between the two maps after alignment, and store
#  the corresponding weight.
      try:
         aligned = NDG(1)
         invoke("$KAPPA_DIR/align2d ref={0} out={2} in={1} form=3 "
                "method=sincsinc rebin=no conserve=no params=\[0,2\]".
                format(aref,aligner_A,aligned))
         dx = float( get_task_par( "TR(1)", "align2d" ) )
         dy = float( get_task_par( "TR(4)", "align2d" ) )
         rms = float( get_task_par( "RMS", "align2d" ) )
         weights[key] = 1/(rms*rms)
         swgt += weights[key]
         nwgt += 1

#  Get the scale factor that normalises the I map to the coadded I map.
#  Also get the correlation coefficient between the data values in the
#  two maps. If the correlation is below a preset limit, indicating that
#  the fit produced by kappa:normalize may not be reliable, then use a
#  scale factor of 1.0.
         invoke( "$KAPPA_DIR/normalize in1={0} in2={1} out=! device=! "
                 "zeroff=yes pcrange=\[10,99.5\]".format(aligned,imap))
         corr = float( get_task_par( "CORR", "normalize" ) )
         if corr < 0.95:
            scale = 1.0
         else:
            scale = 1/float( get_task_par( "SLOPE", "normalize" ) )

#  Ensure we don't use silly scale factors (this can happen for
#  observations that are very different to the mean and so have very
#  low weight).
            if scale < 0.5:
               scale = 0.5
            elif scale > 2.0:
               scale = 2.0

#  If align2d failed, use silly dx,dy values to ensure it is flagged by
#  the following code.
      except starutil.AtaskError:
         dx = 1E6
         dy = 1E6
         weights[key] = 0.0
         scale = 1.0

#  Ensure the bad-bits mask has been reset.
      if bb > 0:
         invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(qui_maps[key]))

#  If the shifts are suspiciously high, we do not believe them. In which
#  case we cannot do pointing ocorrection when creating the Q and U maps.
      if abs(dx) > 8 or abs(dy) > 8:
         msg_out( "\nWARNING: {0}: The I map created from the POL2 data cannot "
                  "be aligned with the supplied reference map. Check the maps "
                  "for the current observation to see if they should be omitted "
                  "from the reduction.\n".format(key) )

#  Otherwise, convert the offset in pixels to (longitude,latitude) offsets
#  in the sky system of the reference map, in arc-seconds....
      else:

#  Strip the wavelength axis off the total intensity map created above.
         imap2d = NDG( 1 )
         invoke("$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".format(qui_maps[key],imap2d))

#  Get the pixel coords at the centre of the total intensity map.
         invoke("$KAPPA_DIR/ndftrace ndf={0}".format(imap2d))
         lbndx = float( get_task_par( "LBOUND(1)", "ndftrace" ) )
         lbndy = float( get_task_par( "LBOUND(2)", "ndftrace" ) )
         ubndx = float( get_task_par( "UBOUND(1)", "ndftrace" ) )
         ubndy = float( get_task_par( "UBOUND(2)", "ndftrace" ) )
         cenx = 0.5*( lbndx + ubndx )
         ceny = 0.5*( lbndy + ubndy )

#  Convert to SKY coords, in radians. Use ATOOLS rather than pyast in
#  order to avoid the need for people to install pyast. Also, ATOOLS
#  integrates with NDFs more easily than pyast.
         (cena,cenb) = invoke("$ATOOLS_DIR/asttran2 this={0} forward=yes "
                              "xin={1} yin={2}".format( imap2d,cenx,ceny)).split()
         cena = float( cena )
         cenb = float( cenb )

#  Add on the pixel offsets, and convert to SKY coords, in radians.
         offx = cenx + dx
         offy = ceny + dy
         (offa,offb) = invoke("$ATOOLS_DIR/asttran2 this={0} forward=yes "
                              "xin={1} yin={2}".format( imap2d,offx,offy)).split()
         offa = float( offa )
         offb = float( offb )

#   Now find the arc-distance parallel to the longitude axis, between the central
#   and offset positions, and convert from radians to arc-seconds.
         dx = invoke("$ATOOLS_DIR/astdistance this={0}, point1=\[{1},{2}\] "
                     "point2=\[{3},{4}\]".format(imap2d,cena,cenb,offa,cenb))
         dx = 3600.0*math.degrees( float( dx ) )

#  The value returned by astDistance is always positive. Adjust the sign
#  of dx so that it goes the right way.
         da = offa - cena
         while da > math.pi:
            da -= math.pi
         while da < -math.pi:
            da += math.pi
         if da < 0.0:
            dx = -dx

#  Now find the arc-distance parallel to the latitude axis, between the central
#  and offset positions, and convert from radians to arc-seconds.
         dy = invoke("$ATOOLS_DIR/astdistance this={0}, point1=\[{1},{2}\] "
                           "point2=\[{3},{4}\]".format(imap2d,cena,cenb,cena,offb))
         dy = 3600.0*math.degrees( float( dy ) )

#  The value returned by astDistance is always positive. Adjust the sign
#  of dx so that it goes the right way.
         db = offb - cenb
         if db < 0.0:
            dy = -dy
         msg_out( "       {0}: ({1:5.1f},{2:5.1f}) arc-sec".format(key,dx,dy) )

#  Store the required pointing corrections as FITS headers within the map.
         sym = invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=get name='Symbol(1)'".
                            format(qui_maps[key]))
         com = comment.format(sym)
         invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=PNTRQ_DX "
                "edit=a value={1} comment=\"'{2}'\""
                " position=! mode=interface".format(qui_maps[key],dx,com))

         sym = invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=get name='Symbol(2)'".
                format(qui_maps[key]))
         com = comment.format(sym)
         invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=PNTRQ_DY edit=a value={1} "
                "comment=\"'{2}'\" position=! mode=interface".
                format(qui_maps[key],dy,com))

#  Also store the scale factor.
      invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=CHUNKFAC edit=a value={1} "
             "comment=\"'{2}'\" position=! mode=interface".
             format(qui_maps[key],scale,scomment))

#  Find the median of the weights.
   wmed = median( weights.values() )

#  Normalise the weights to a median value of 1.0 then limit them to be <= 1.0
#  to avoid biasing the results heavily towards observations that happen to
#  be close to the median, and return them rounded to 3 decimal places.
   for key in qui_maps:
      if weights[key] > wmed:
         weights[key] = 1.0
      else:
         weights[key] = round( weights[key]/wmed, 3 )

   return weights









#  Main entry...
#  Catch any exception so that we can always clean up, even if control-C
#  is pressed.
try:

#  Constants.
   pcathresh_def1 = -50    # Default value for auto-masking
   pcathresh_def2 = -150   # Default value for external-masking

#  Declare the script parameters. Their positions in this list define
#  their expected position on the script command line. They can also be
#  specified by keyword on the command line. No validation of default
#  values or values supplied on the command line is performed until the
#  parameter value is first accessed within the script, at which time the
#  user is prompted for a value if necessary. The parameters "MSG_FILTER",
#  "ILEVEL", "GLEVEL" and "LOGFILE" are added automatically by the ParSys
#  constructor.
   params = []



   params.append(starutil.ParNDG("IN", "The input POL2 data",
                                 get_task_par("DATA_ARRAY","GLOBAL",
                                              default=Parameter.UNSET)))

   params.append(starutil.ParNDG("IOUT", "The output total intensity map",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1 ))

   params.append(starutil.ParNDG("QOUT", "The output Q map",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1 ))

   params.append(starutil.ParNDG("UOUT", "The output U map",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1 ))

   params.append(starutil.Par0S("CAT", "The output FITS vector catalogue",
                                 default=None, noprompt=True))

   params.append(starutil.ParGrp("CONFIG", "Map-maker tuning parameters",
                                 "def", noprompt=True))

   params.append(starutil.Par0F("PIXSIZE", "Pixel size (arcsec)", 4.0,
                                 maxval=1000, minval=0.01, noprompt=True))

   params.append(starutil.Par0S("QUDIR", "Directory in which to save new "
                                "Q, U and I time series", None, noprompt=True))

   params.append(starutil.Par0S("MAPDIR", "Directory in which to save new "
                                "I maps before they are co-added", None,
                                noprompt=True))

   params.append(starutil.Par0S("MASK", "Type of masking to use in makemap",
                                "AUTO", noprompt=True ))

   params.append(starutil.ParChoice("MASKTYPE", ("SIGNAL","MASK"),
                                    "Type of map supplied for parameter MASK",
                                    "SIGNAL", noprompt=True ))

   params.append(starutil.Par0L("IPCOR", "Perform IP correction?", None,
                                 noprompt=True))

   params.append(starutil.ParNDG("IPREF", "The total intensity map to use "
                                 "for IP correction", default=None, exists=False,
                                 noprompt=True, minsize=0, maxsize=1 ))

   params.append(starutil.Par0L("REUSE", "Re-use existing time-streams and maps?", True,
                                 noprompt=True))

   params.append(starutil.ParNDG("REF", "Reference map defining the pixel grid", default=None,
                                 noprompt=True, minsize=0, maxsize=1 ))

   params.append(starutil.ParChoice( "NORTH", ("TRACKING","FK5","ICRS","AZEL",
                                     "GALACTIC","GAPPT","FK4","FK4-NO-E",
                                     "ECLIPTIC"), "Celestial system to "
                                     "use as reference direction", "TRACKING",
                                     noprompt=True ))

   params.append(starutil.Par0L("DEBIAS", "Remove statistical bias from P"
                                "and PI?", False, noprompt=True))

   params.append(starutil.ParChoice("DEBIASTYPE", ("AS","MAS"),
                                    "Bias estimator to be used",
                                    "AS", noprompt=True ))

   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

   params.append(starutil.ParNDG("MASKOUT1", "The output AST mask",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1, noprompt=True ))

   params.append(starutil.ParNDG("MASKOUT2", "The output PCA mask",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1, noprompt=True ))

   params.append(starutil.Par0S("NEWMAPS", "Text file to hold list of new map",
                                 default=None, noprompt=True))

   params.append(starutil.Par0L("MAPVAR", "Use variance between observation maps?",
                                 False, noprompt=True))

   params.append(starutil.Par0L("MULTIOBJECT", "Allow processing of data from multiple objects?",
                                 False, noprompt=True))

   params.append(starutil.Par0L("JY", "Should outputs be converted from pW to mJy/beam?",
                                True, noprompt=True))

   params.append(starutil.Par0F("FCF", "pW to Jy/beam conversion factor",
                                None, noprompt=True ))

   params.append(starutil.ParGrp("ICONFIG", "Map-maker tuning parameters for I maps",
                                 "def", noprompt=True))

   params.append(starutil.ParGrp("QUCONFIG", "Map-maker tuning parameters for Q/U maps",
                                 "def", noprompt=True))

   params.append(starutil.Par0F("BINSIZE", "Catalogue bin size (arcsec)", None,
                                 maxval=1000, minval=0.01, noprompt=True))

   params.append(starutil.Par0L("SKYLOOP", "Use skyloop instead of makemap?",
                                False, noprompt=True))

   params.append(starutil.Par0L("OBSWEIGHT", "Down-weight unusual observations?",
                                False, noprompt=True))

   params.append(starutil.Par0L("NORMALISE", "Normalise each observation to the mean?",
                                False, noprompt=True))

   params.append(starutil.Par0F("WEIGHTLIM", "Lowest usable observation weight",
                                 0.05, maxval=1.0, minval=0.0, noprompt=True))

   params.append(starutil.Par0F("TRIM", "Fractional exposure time at "
                                "which to trim coadds", None, maxval=10.0,
                                minval=0.0, noprompt=True))

   params.append(starutil.Par0L("SMOOTH450", "Smooth 450 maps to 850 um resolution?",
                                False, noprompt=True))

   params.append(starutil.ParGrp("CALCQUCONFIG", "CALCQU tuning parameters",
                                 "def", noprompt=True))

   params.append(starutil.ParNDG( "INITSKYI", "The initial I map", default=None,
                                  minsize=0, maxsize=1, noprompt=True ))

   params.append(starutil.ParNDG( "INITSKYQ", "The initial Q map", default=None,
                                  minsize=0, maxsize=1, noprompt=True ))

   params.append(starutil.ParNDG( "INITSKYU", "The initial U map", default=None,
                                  minsize=0, maxsize=1, noprompt=True ))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  It's a good idea to get parameter values early if possible, in case
#  the user goes off for a coffee whilst the script is running and does not
#  see a later parameter propmpt or error...

#  Get the input POL-2 data files. They should be supplied as the first item on
#  the command line, in the form of a Starlink "group expression" (i.e.
#  the same way they are supplied to other SMURF commands such as makemap).
   indata = parsys["IN"].value

#  Now get the I, Q and U output maps.
   imap = parsys["IOUT"].value
   qmap = parsys["QOUT"].value
   umap = parsys["UOUT"].value

#  The user-supplied calcqu config.
   calcquconfig = parsys["CALCQUCONFIG"].value

#  The user-supplied makemap configs, and pixel size.
   config = parsys["CONFIG"].value
   iconfig = parsys["ICONFIG"].value
   quconfig = parsys["QUCONFIG"].value
   pixsize = parsys["PIXSIZE"].value

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

#  See if existing output files are to be re-used. If not, they are
#  re-created from the corresponding input files.
   reuse = parsys["REUSE"].value

#  See if variances in the co-added maps are to be derived form the
#  spread of values in corresponding pixels of the observation maps.
#  If not, variances in the coadds are propagated from the variances
#  in the observation maps.
   mapvar = parsys["MAPVAR"].value

#  See if it is acceptable for the list of input files to include data
#  for multiple objects. This should usually be False to avoid
#  accidentally attempting to create maps that are large enough to contain
#  objects from different parts of the sky.
   multiobject = parsys["MULTIOBJECT"].value

#  Get the type of masking to use. If a map is supplied, assume external
#  masking.
   mask = parsys["MASK"].value
   upmask = mask.upper()

   astmask = None
   pcamask = None
   pcamaskpar = ""

   if upmask == "AUTO":
      automask = True
      circlemask = False
      maskmap = None
   elif upmask == "CIRCLE":
      automask = False
      circlemask = True
      maskmap = None

#  Otherwise, the MASK parameter must specify one (if MASKTYPE is
#  "Signal") or two (if MASKTYPE is "Mask") NDFs.
   else:
      automask = False
      circlemask = False
      maskmap = NDG(mask)

      masktype = parsys["MASKTYPE"].value
      if masktype == "SIGNAL":
         if len(maskmap) != 1:
            raise starutil.InvalidParameterError("More than one NDF "
                      "supplied for parameter MASK." )
         CheckNDF( "MASK", maskmap[0], pixsize, None )
         msg_out("Masking will be based on SNR values derived from {0}.".format(maskmap))

#  See where (if at all) the masks are to be saved.
         astmask = parsys["MASKOUT1"].value
         if astmask is None:
            astmask = NDG(1)
         pcamask = parsys["MASKOUT2"].value
         if pcamask is None:
            pcamask = NDG(1)

      else:
         if len(maskmap) != 2:
            raise starutil.InvalidParameterError("Exactly two NDFs must be"
                                            " supplied for parameter MASK." )
         astmask = maskmap[0]
         pcamask = maskmap[1]
         CheckNDF( "MASK", astmask, pixsize, None )
         CheckNDF( "MASK", pcamask, pixsize, None )
         msg_out("AST mask: {0}.".format(astmask))
         msg_out("PCA mask: {0}.".format(pcamask))

#  Get the initial sky maps. Check they are all in units of pW. Get the
#  pixel bounds of the first.
   ref = None
   first = True
   initskys = {}
   for qui in ('I', 'Q', 'U'):
      param = "INITSKY"+qui
      initskys[qui] = parsys[param].value
      if initskys[qui]:
         invoke("$KAPPA_DIR/ndftrace ndf={0} quiet=yes".format(initskys[qui]) )
         if first:
            first = False
            ref = initskys[qui]
            lx = starutil.get_task_par( "lbound(1)", "ndftrace" )
            ly = starutil.get_task_par( "lbound(2)", "ndftrace" )
            ux = starutil.get_task_par( "ubound(1)", "ndftrace" )
            uy = starutil.get_task_par( "ubound(2)", "ndftrace" )
         CheckNDF( param, initskys[qui], pixsize, "pW" )

#  If no initial sky maps were supplied, get the reference map
   if ref is None:
      ref = parsys["REF"].value
      CheckNDF( "REF", ref, pixsize, None )
   if not ref:
      if maskmap is not None:
         ref = maskmap[0]
      else:
         ref = "!"

#  Remove any third (wavelength) axis from the reference map. For
#  instance, this allows 450 um data to be aligned with a map made
#  from 850 um data.
      if ref != "!":
         ref2d = NDG( 1 )
         invoke( "$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".
                 format( ref, ref2d ) )
         ref = ref2d

#  Get the waveband of the supplied data (450 or 850).
   try:
      filter = int( float( starutil.get_fits_header( indata[0], "FILTER", True )))
   except starutil.NoValueError:
      filter = 850
      msg_out( "No value found for FITS header 'FILTER' in {0} - assuming 850".format(indata[0]))

   if filter != 450 and filter != 850:
      raise starutil.InvalidParameterError("Invalid FILTER header value "
             "'{0} found in {1}.".format( filter, indata[0] ) )

#  If we have 450 um data, see if the resulting maps and catalogues should be
#  smoothed to the 850 um resolution.
   if filter == 450:
      smooth450 = parsys["SMOOTH450"].value
   else:
      smooth450 = False

#  See if we should use skyloop instead of makemap.
   skyloop = parsys["SKYLOOP"].value

#  See if unusual observations should be down-weighted.
   obsweight = parsys["OBSWEIGHT"].value

#  Get the lowest usable weight.
   wgtlim = parsys["WEIGHTLIM"].value

#  See if observations are to be normalised to the mean of all
#  observations.
   normalise = parsys["NORMALISE"].value

#  Get the fractional exposure time at which to trim the mosaics. This is
#  a fraction of the mean expsoure time over the whole coadd.
   trim = parsys["TRIM"].value

#  See if we should store I, Q and U values in mJy/beam in the output
#  calatlogue.
   jy = parsys["JY"].value

#  If we are converting to mJy/beam, get the FCF (Jy/pw).
   if jy:
      fcf = parsys["FCF"].value

#  If no FCF supplied, get the default FCF for the waveband
      if fcf is None:
         if filter == 450:
            fcf = 962.0
         else:
            fcf = 725.0

#  If IP correction is to be performed, get the map to be used to define
#  the IP correction.
   ipref = None
   parsys["IPCOR"].default = ( qmap is not None or umap is not None )
   if parsys["IPCOR"].value:
      ipref = parsys["IPREF"].value
      if not ipref:
         ipref = ref
      if not ipref or ipref == "!":
         raise starutil.InvalidParameterError("IP correction requested "
                                        "but no IP reference map supplied.")
      elif ipref != imap:
         CheckNDF( "IPREF", ipref, pixsize, "pW" )
      ip = "ipref={0}".format(ipref)

#  Ensure no history record is added to the ip reference map suggesting it
#  was created by pol2map. This would otherwise happen at cleanup. If it *is*
#  created by pol2map (due to being the IOUT image), then a suitable history
#  record will be created as a result of it being assigned to parameter IOUT.
      parsys["IPREF"].exists = True
   else:
      ip = "ipref=!"

#  Get the output catalogue.
   outcat = parsys["CAT"].value

#  If a catalogue is required, we need to create all three maps, so
#  ensure this is the case (use temporary files for any that have not
#  been requested by the user).
   catref = None
   if outcat:
      if not imap:
         if ipref is not None:
            msg_out("The supplied IPREF map will be used as the total "
                    "intensity map when creating the polarisation values")
            imap = ipref
         else:
            msg_out("The total intensity map will also be created since "
                    "it is required to calculate the polarisation values")
            imap = NDG( 1 )
      if not qmap:
         msg_out("The Q map will also be created since "
                 "it is required to calculate the polarisation values")
         qmap = NDG( 1 )
      if not umap:
         msg_out("The U map will also be created since "
                 "it is required to calculate the polarisation values")
         umap = NDG( 1 )

#  Get the binsize for the catalogue.
      binsize = parsys["BINSIZE"].value

#  See if statistical debiasing is to be performed, and see which bias
#  estimator is to be used.
   debias = parsys["DEBIAS"].value
   debiastype = parsys["DEBIASTYPE"].value

#  See where to put new Q, U and I maps for individual observations, and
#  ensure the directory exists.
   mapdir =  parsys["MAPDIR"].value
   if not mapdir:
      mapdir = NDG.tempdir
   elif not os.path.exists(mapdir):
      os.makedirs(mapdir)

#  See where to put new Q, U and I time series, and ensure the directory
#  exists.
   qudir =  parsys["QUDIR"].value
   if not qudir:
      qudir = NDG.tempdir
   elif not os.path.exists(qudir):
      os.makedirs(qudir)

#  Get the reference direction.
   north = parsys["NORTH"].value

#  The name of the output text file to create in which to store the paths
#  to the new individual observation maps created by the current
#  invocation of this script.
   newmaps = parsys["NEWMAPS"].value







#  -----------  CLASSIFY THE INPUT DATA FILES ------------------------


#  Classify each input data file as raw, QUI time-series or QUI map. Create
#  three separate text files containing all input NDFs of each type (plus
#  a fourth holing non-POL2 data). Also, create another text file
#  containing a list of any missing raw sub-scan files.
   junks = NDG.tempfile()
   inraws = NDG.tempfile()
   inquis = NDG.tempfile()
   inmaps = NDG.tempfile()
   rawinfo = NDG.tempfile()
   missing = NDG.tempfile()
   mapinfo = NDG.tempfile()
   msg_out( " " )
   msg_out( "Checking the list of input data files is usable - "
            "this may take some time... " )
   invoke("$SMURF_DIR/pol2check in={0} quiet=yes junkfile={1} mapfile={2} "
          "rawfile={3} stokesfile={4} rawinfo={5} missing={6} mapinfo={7} "
          "multiobject={8}".format(indata,junks,inmaps,inraws,inquis,rawinfo,
                                   missing,mapinfo,multiobject))

#  Warn about any non-POL2 input data files that are being ignored.
   ok = True
   if get_task_par( "JUNKFOUND", "pol2check" ):
      ok = False
      msg_out( " ")
      msg_out( "WARNING: The following inappropriate input data files are "
               "being ignored: " )
      with open( junks ) as f:
         msg_out( f.read() )
      msg_out( " ")

#  Warn about any missing raw data scub-scans.
   if os.path.isfile( missing ):
      ok = False
      msg_out( " ")
      msg_out( "WARNING: The raw data files for the following sub-scans seem "
               "to be missing from the supplied list of input files: " )
      with open( missing ) as f:
         msg_out( f.read() )
      msg_out( " ")

   if ok:
      msg_out( "The list of input data files looks OK. Continuing." )

#  Initialise the list of all Stokes time-series files to be processed by
#  makemap so that it holds any Stokes time-series files supplied by
#  parameter IN.
   allquis = NDG.tempfile()
   if get_task_par( "STOKESFOUND", "pol2check" ):
      shutil.copyfile( inquis, allquis )

#  Initialise a list of new maps created by this run of pol2map.
   new_maps = []

#  Set up a dict for each Stokes parameter holding paths to any supplied maps
#  for that Stokes parameter. The keys are of the form "<UT>_<OBS>_<SUBSCAN>".
#  Check that any supplied maps are in units of pW and are created from
#  POL2 data.
   imaps = {}
   qmaps = {}
   umaps = {}

   if get_task_par( "MAPFOUND", "pol2check" ):

      with open(inmaps) as infile:
         lines = infile.readlines()
      paths = [line.strip() for line in lines]

      with open(mapinfo) as infile:
         lines = infile.readlines()
      infos = [line.strip() for line in lines]

      for (path,info) in zip( paths, infos ):
         (stokes,id) = info.split()
         if stokes == "I":
            imaps[id] = path
         elif stokes == "Q":
            qmaps[id] = path
         else:
            umaps[id] = path

         inmap = NDG(path)
         inbeam = get_fits_header( inmap, "INBEAM" )
         if not inbeam or ("pol" not in inbeam):
            raise starutil.InvalidParameterError("One of the {0} maps ({1}) "
                                          "was not created from POL2 data or "
                                          "is corrupt.".format(stokes,inmap))
         CheckNDF( "IN", inmap, pixsize, "pW" )



#  -----------  CREATE STOKES TIME SERIES FROM RAW DATA ------------------------


#  If any raw analysed intensity files were supplied, use smurf:calcqu to
#  convert them into Stokes paramater time-series files.
   if get_task_par( "RAWFOUND", "pol2check" ):
      msg_out( "Calculating Q, U and I time streams from raw analysed intensity data...")

#  Get a dict in which each key is an observation identifier of the form
#  <UT>_<OBS>, and each value is a list of raw data files for the observation.
      with open(inraws) as infile:
         lines = infile.readlines()
      paths = [line.strip() for line in lines]

      with open(rawinfo) as infile:
         lines = infile.readlines()
      infos = [line.strip() for line in lines]

      rawlist = {}
      for (path,id) in zip( paths, infos ):
         if id in rawlist:
            if path not in rawlist[id]:
               rawlist[id].append( path )
         else:
            rawlist[id] = [ path ]

#  Run calcqu separately on each observation.
      nobs = len(rawlist)
      iobs = 0
      for id in rawlist:
         iobs += 1

#  Create an NDG object holding the raw POL2 files for the current
#  observation.
         rawdata = NDG( rawlist[ id ] )

#  Use CALCQU to create the new Q, U and I time streams from the supplied
#  analysed intensity time streams. Put them in the QUDIR directory.
         new_q = NDG.tempfile()
         new_u = NDG.tempfile()
         new_i = NDG.tempfile()
         try:

#  If REUSE is TRUE and old Q, U and I time-streams exists, re-use them.
            try:
               if reuse:
                  w = filter // 100
                  aqts = NDG("{0}/s{2}a{1}\*_QT".format(qudir,id,w), True)
                  auts = NDG("{0}/s{2}a{1}\*_UT".format(qudir,id,w), True)
                  aits = NDG("{0}/s{2}a{1}\*_IT".format(qudir,id,w), True)
                  anq = len( aqts )
                  anu = len( auts )
                  ani = len( aits )
                  if anq != anu or anq != ani:
                     raise starutil.NoNdfError("Ignoring pre-existing data")

                  bqts = NDG("{0}/s{2}b{1}\*_QT".format(qudir,id,w), True)
                  buts = NDG("{0}/s{2}b{1}\*_UT".format(qudir,id,w), True)
                  bits = NDG("{0}/s{2}b{1}\*_IT".format(qudir,id,w), True)
                  bnq = len( bqts )
                  bnu = len( buts )
                  bni = len( bits )
                  if bnq != anq or bnu != anu or bni != ani:
                     raise starutil.NoNdfError("Ignoring pre-existing data")

                  cqts = NDG("{0}/s{2}c{1}\*_QT".format(qudir,id,w), True)
                  cuts = NDG("{0}/s{2}c{1}\*_UT".format(qudir,id,w), True)
                  cits = NDG("{0}/s{2}c{1}\*_IT".format(qudir,id,w), True)
                  cnq = len( cqts )
                  cnu = len( cuts )
                  cni = len( cits )
                  if cnq != anq or cnu != anu or cni != ani:
                     raise starutil.NoNdfError("Ignoring pre-existing data")

                  dqts = NDG("{0}/s{2}d{1}\*_QT".format(qudir,id,w), True)
                  duts = NDG("{0}/s{2}d{1}\*_UT".format(qudir,id,w), True)
                  dits = NDG("{0}/s{2}d{1}\*_IT".format(qudir,id,w), True)
                  dnq = len( dqts )
                  dnu = len( duts )
                  dni = len( dits )
                  if dnq != anq or dnu != anu or dni != ani:
                     raise starutil.NoNdfError("Ignoring pre-existing data")

                  msg_out("   Re-using previously created Q, U and I "
                          "time-streams for observation {0}".format(id))

                  with open(new_q, "w") as outfile:
                     for ndg in (aqts,bqts,cqts,dqts):
                        for ndf in ndg:
                           outfile.write(ndf+"\n")

                  with open(new_u, "w") as outfile:
                     for ndg in (auts,buts,cuts,duts):
                        for ndf in ndg:
                           outfile.write(ndf+"\n")

                  with open(new_i, "w") as outfile:
                     for ndg in (aits,bits,cits,dits):
                        for ndf in ndg:
                           outfile.write(ndf+"\n")

               else:
                  raise starutil.NoNdfError("Ignoring any pre-existing data")

#  Otherwise create new time-streams.
            except starutil.NoNdfError:
               msg_out("   {0}/{1}: Processing {2} raw data files from observation {3} ... ".
                       format(iobs,nobs,len(rawlist[ id ]), id ) )
               invoke("$SMURF_DIR/calcqu in={0} lsqfit=yes config={6} outq={1}/\*_QT "
                      "outu={1}/\*_UT outi={1}/\*_IT fix=yes north={2} outfilesi={3} "
                      "outfilesq={4} outfilesu={5}".
                      format( rawdata, qudir, north, new_i, new_q, new_u, calcquconfig ) )

#  Append the new Stokes parameter time series files created above to the
#  list of all Stokes parameter time series files.
            with open(allquis, 'a') as outfile:
               for fname in ( new_q, new_u, new_i ):
                   if os.path.isfile( fname ):
                       with open(fname) as infile:
                          outfile.write(infile.read())

         except starutil.AtaskError as err:
            msg_out( err )
            msg_out( "\nAn error occurred within CALCQU. The above observation will be ignored.\nContinuing to process any remaining observations...\n" )








#  -----------  CREATE INDIVIDUAL MAPS FROM STOKES TIME SERIES DATA ---------

#  Do some initialisation in case no time-series data is supplied.
   pcathresh_i = 0.0
   pcathresh_qu = 0.0

#  Initialise three dicts - one each for Q, U and I - holding Stokes
#  time-stream files to be processed.
   ilist = {}
   qlist = {}
   ulist = {}

#  Paths to config files.
   conf = NDG.tempfile()
   iconf = NDG.tempfile()
   quconf = NDG.tempfile()

#  If we have some Stokes parameter time-series files to process...
   if os.path.isfile(allquis):

#  Create a text file holding information about all the Stokes time-series
#  files to be processed. For each one, get the Stokes parameter (Q, U or I)
#  and a key that is unique for the chunk of data, of the form
#  "<UT>_<OBS>_<SUBSCAN>".
      stokesinfo = NDG.tempfile()
      quindg = NDG("^{0}".format(allquis) )
      invoke("$SMURF_DIR/pol2check in={0} quiet=yes stokesinfo={1} multiobject={2}".
             format(quindg,stokesinfo,multiobject))

#  Set up three dicts - one each for Q, U and I. Each key is as described
#  above. Each value is a list of paths for NDFs holding data with the same
#  key and the same Stokes parameter (Q, U or I).
      with open(allquis) as infile:
         lines = infile.readlines()
      paths = [line.strip() for line in lines]

      with open(stokesinfo) as infile:
         lines = infile.readlines()
      infos = [line.strip() for line in lines]

      for (path,info) in zip( paths, infos ):
         (stokes,id) = info.split()
         if stokes == "Q":
            if id in qlist:
               if path not in qlist[id]:
                  qlist[id].append( path )
            else:
               qlist[id] = [ path ]

         elif stokes == "U":
            if id in ulist:
               if path not in ulist[id]:
                  ulist[id].append( path )
            else:
               ulist[id] = [ path ]

         else:
            if id in ilist:
               if path not in ilist[id]:
                  ilist[id].append( path )
            else:
               ilist[id] = [ path ]

#  If required, generate the AST and PCA masks from the supplied MASK
#  map.
      if maskmap and masktype == "SIGNAL":

#  Get an SNR map from the supplied MASK signal map.
         snr = NDG(1)
         invoke("$KAPPA_DIR/makesnr in={0} out={1} minvar=0".format(maskmap,snr))

#  Get the SNR thresholds for the AST and PCA masks from the supplied config
#  file, using defaults if they are not there.
         try:
            ast_snr = float( invoke("$KAPPA_DIR/configecho name=ast.zero_snr "
                                    "config={0}".format(config)) )
         except Exception:
            ast_snr = 3.0

         try:
            ast_snrlo = float( invoke("$KAPPA_DIR/configecho name=ast.zero_snrlo "
                                    "config={0}".format(config)) )
         except Exception:
            ast_snrlo = 2.0

         try:
            pca_snr = float( invoke("$KAPPA_DIR/configecho name=pca.zero_snr "
                                    "config={0}".format(config)) )
         except Exception:
            pca_snr = 3.0

         try:
            pca_snrlo = float( invoke("$KAPPA_DIR/configecho name=pca.zero_snrlo "
                                    "config={0}".format(config)) )
         except Exception:
            pca_snrlo = 2.0

#  Very strong sources such as Orion A can create masks in which there
#  are insufficient background pixels to allow future invocations of
#  makemap to succeed. We therefore loop round raising the SNR limits for
#  the mask until no more than 20% of the originally good pixels are
#  designated as source pixels. The AST mask contains peaks above SNR=3,
#  followed down to SNR=2.
         invoke("$KAPPA_DIR/stats ndf={0}".format(snr))
         ngood = float( get_task_par( "numgood", "stats" ) )
         maxgood = ngood / 5

         noise = ast_snrlo
         minheight = ast_snr
         aconf = NDG.tempfile()

         while True:
            fd = open(aconf,"w")
            fd.write("FellWalker.FlatSlope=0\n")
            fd.write("FellWalker.MinDip=1.0E30\n")
            fd.write("FellWalker.Noise={0}\n".format(noise))
            fd.write("FellWalker.MinHeight={0}\n".format(minheight))
            fd.close()
            invoke("$CUPID_DIR/findclumps in={0} method=fellwalker rms=1 "
                   "outcat=! out={1} config=^{2}".format(snr,astmask,aconf))

            try:
               invoke("$KAPPA_DIR/stats ndf={0}".format(astmask))
               ngood = float( get_task_par( "numgood", "stats" ) )
               if ngood < 5:
                  raise starutil.InvalidParameterError( "No significant emission "
                               "found in total intensity map {0} supplied for "
                               "parameter MASK".format(maskmap))
            except starutil.AtaskError:
               raise starutil.InvalidParameterError( "No significant emission "
                               "found in total intensity map {0} supplied for "
                               "parameter MASK".format(maskmap))

            if ngood < maxgood:
               break
            else:
               if noise == minheight:
                  minheight *= 1.2
               noise = minheight

#  The source regions within the PCA mask need to be smaller than in the
#  AST mask. Make sure it uses no more than 10% of the original good
#  pixels. The PCA mask contains peaks above SNR=5, followed down to SNR=3.
         maxgood = maxgood / 2

         noise = pca_snrlo
         minheight = pca_snr
         pconf = NDG.tempfile()

         while True:
            fd = open(pconf,"w")
            fd.write("FellWalker.FlatSlope=0\n")
            fd.write("FellWalker.MinDip=1.0E30\n")
            fd.write("FellWalker.Noise={0}\n".format(noise))
            fd.write("FellWalker.MinHeight={0}\n".format(minheight))
            fd.close()
            invoke("$CUPID_DIR/findclumps in={0} method=fellwalker rms=1 "
                   "outcat=! out={1} config=^{2}".format(snr,pcamask,pconf))

            try:
               invoke("$KAPPA_DIR/stats ndf={0}".format(pcamask))
               ngood = float( get_task_par( "numgood", "stats" ) )
            except starutil.AtaskError:
               ngood = 0
               pcamask = None
               msg_out( "WARNING - No significant emission found in total "
                        "intensity map {0} supplied for parameter MASK - "
                        "shall proceed without PCA masking.".format(maskmap))
               break

            if ngood < maxgood:
               break
            else:
               if noise == minheight:
                  minheight *= 1.2
               noise = minheight


#  We need to decide on the value to use for the PCA.PCATHRESH config
#  parameter when running makemap below. If a value is given in the
#  user-supplied config, use it.
      try:
         pcathresh = float( invoke("$KAPPA_DIR/configecho name=pca.pcathresh "
                                   "config={0}".format(config)) )
      except Exception:
         pcathresh = 0

#  If no value is supplied in the config, the default values are -50
#  (pcathresh_def1) for auto-masked maps and -150 (pcathresh_def2) for
#  external-masked maps. However, for bright sources these default are
#  too high to allow convergence to be reached within a reasonable
#  number of iterations when creating the I maps (the Q and U maps
#  are easier since they are fainter, but for consistency we should
#  use the same value for Q and U as for I, even if convergence could
#  be achieved with a higher value of PCATHRESH). We look for any
#  existing maps in the mapdir, and re-use the same PCA.PCATHRESH value
#  if any are found.
      if pcathresh == 0:

#  Get the PCA.PCATHRESH value from the most recently created map (if any)
#  of the same type (auto or external mask) as the ones we are currently
#  creating.
         (pcathresh,tmap) = getPcaThresh( mapdir, automask )

#  If an existing map of the correct type was found, we use its PCATHRESH
#  value when creating maps below. Tell the user.
         if pcathresh != 0:
            msg_out("Will use PCA.PCATHRESH value of {0} inherited from existing "
                    "map {1}.".format(pcathresh,tmap))

#  If we are creating externally-masked maps, but no existing externally-masked
#  maps were found, see if any existing auto-masked maps can be found.
         elif not automask:
            (pcathresh,tmap) = getPcaThresh( mapdir, True )

#  If the PCATHRESH value used to create any auto-masked map was not equal
#  to the default value, then we use the same non-default value to create
#  the externally masked maps. Otherwise, we leave pcathresh set to zero to
#  indicate that makemap should determine a value for PCATHRESH itself by
#  repeatedly re-running with lower PCATHRESH values until a map converges.
            if pcathresh == pcathresh_def1:
               pcathresh = 0
            elif pcathresh != 0:
               msg_out("Will use PCA.PCATHRESH value of {0} inherited from existing "
                       "map {1}.".format(pcathresh,tmap))

#  If the user-supplied config includes a value for "modelorder", but
#  that value does not include PCA, we set "pcathresh" to a non-zero
#  value to indicate that the ABORTSOON parameter should not be set
#  when running makemap. The specific non-zero value used does not matter
#  as makemap will not be using it anyway (since modelorder order does
#  include PCA), but we choose to use the appropriate default value. Note,
#  if the user-supplied config does not include  a value for modelorder,
#  (i.e. configecho returns "<***>") then the default modelorder value
#  defined below (which includes PCA) will be used. In this case we want
#  to retain pcathresh at zero, so that ABORTSOON is used when running
#  makemap.
      if pcathresh == 0:
         try:
            models = invoke("$KAPPA_DIR/configecho name=modelorder "
                            "config={0}".format(config))
            if "<***>" not in models and "pca" not in models.lower():
               pcathresh = (pcathresh_def1 if automask else pcathresh_def2)
         except Exception:
            pass

#  See if the ICONFIG or QUCONFIG configurations provide a value for
#  PCA.PCATHRESH, in which case it over-rides the above value.
      try:
         pcathresh_i = float( invoke("$KAPPA_DIR/configecho name=pca.pcathresh "
                                     "config={0}".format(iconfig)) )
      except Exception:
         pcathresh_i = pcathresh

      try:
         pcathresh_qu = float( invoke("$KAPPA_DIR/configecho name=pca.pcathresh "
                                     "config={0}".format(quconfig)) )
      except Exception:
         pcathresh_qu = pcathresh

#  Create a config file to use with makemap. This file contains stuff
#  that is used when creating both I maps and Q/U maps.
      fd = open(conf,"w")

#  Store the default set of config parameters in the config file.
      fd.write("^$STARLINK_DIR/share/smurf/.dimmconfig_pol2.lis\n")
      fd.write("numiter = -200\n")
      fd.write("modelorder = (com,gai,pca,ext,flt,ast,noi)\n")

      fd.write("maptol = 0.05\n")
      fd.write("maptol_mask = <undef>\n")
      fd.write("maptol_mean = 0\n")
      fd.write("maptol_box = 60\n")
      fd.write("maptol_hits = 1\n")

      fd.write("pca.pcathresh = {0}\n".format( pcathresh_def2 if (pcathresh==0) else pcathresh))
      fd.write("ast.mapspike_freeze = 5\n")
      fd.write("pca.zero_niter = 0.5\n")
      fd.write("com.zero_niter = 0.5\n")
      fd.write("flt.zero_niter = 0.5\n")
      fd.write("com.freeze_flags = 30\n")

#  Some depend on the masking type.
      if automask:
         fd.write("ast.skip = 10\n")
         fd.write("ast.zero_snr = 3\n")
         fd.write("ast.zero_snrlo = 2\n")
         fd.write("ast.zero_freeze = 0.2\n")

         fd.write("pca.pcathresh = {0}\n".format( pcathresh_def1 if (pcathresh==0) else pcathresh))
         fd.write("pca.zero_snr = 5\n")
         fd.write("pca.zero_snrlo = 3\n")
         fd.write("pca.zero_freeze = -1\n")

         fd.write("com.zero_snr = 5\n")
         fd.write("com.zero_snrlo = 3\n")
         fd.write("com.zero_freeze = -1\n")

         fd.write("flt.zero_snr = 5\n")
         fd.write("flt.zero_snrlo = 3\n")
         fd.write("flt.zero_freeze = -1\n")

      elif circlemask:
         fd.write("ast.zero_circle = (0.0083)\n")
         fd.write("pca.zero_circle = (0.0083)\n")
         fd.write("com.zero_circle = (0.0083)\n")
         fd.write("flt.zero_circle = (0.0083)\n")

      else:
         fd.write("ast.zero_mask = mask2\n")
         if pcamask:
            pcamaskpar = "mask3={0}".format(pcamask)
            fd.write("pca.zero_mask = mask3\n")
            fd.write("com.zero_mask = mask3\n")
            fd.write("flt.zero_mask = mask3\n")

#  If the user supplied extra config parameters, append them to the
#  config file. Note, "config" will include any required "^" character and
#  so the format string below does not need to include an explicit "^"
#  character.
      if config and config != "def":
         fd.write("{0}\n".format(config))

#  Close the basic config file that contains stuff used when creating both
#  I and Q/U maps.
      fd.close()

#  We create two derived config files that inherit the above common config:
#  one for use when creating I maps and one for use when creating Q or U
#  maps. They may contain different values if the user supplies anything
#  for ICONFIG or QUCONFIG. First create the I config file.
      fd = open(iconf,"w")

#  Include the common config created above. Note, "conf" is a simple file
#  name - not a configuration - and so we need to include the "^" explicitly
#  in the format string.
      fd.write("^{0}\n".format(conf))

#  If the user has supplied any I-specific config parameters, include them
#  now so that they over-ride values in the common config. Note, "iconfig"
#  is a complete configuration, and so will already include any required "^"
#  character. So do not include a "^" in the format string.
      if iconfig and iconfig != "def":
         fd.write("{0}\n".format(iconfig))

#  Put in values that are absolutely required by this script. These
#  over-write any values in the user-supplied configs. This includes
#  resetting mask SNR thresholds so that any SNR limits intended for
#  use just by findclumps are not also used by makemap.
      fd.write("noi.usevar=1\n")
      fd.write("flagslow=0.01\n")
      fd.write("downsampscale=0\n")

      if maskmap and masktype == "SIGNAL":
         fd.write("ast.zero_snr=0\n")
         fd.write("pca.zero_snr=0\n")
         fd.write("flt.zero_snr=0\n")
         fd.write("com.zero_snr=0\n")
      fd.close()

#  Create the QU config file in the same way. For Q and U maps, the
#  astronomical signal is much weaker and the common mode is much less
#  well defined. This can cause the COM model to throw out huge amounts
#  of data. To prevent, this disable common-mode flagging when creating
#  Q/U maps.
      fd = open(quconf,"w")
      fd.write("com.noflag=1\n")
      fd.write("^{0}\n".format(conf))
      if quconfig and quconfig != "def":
         fd.write("{0}\n".format(quconfig))
      fd.write("noi.usevar=1\n")
      fd.write("flagslow=0.01\n")
      fd.write("downsampscale=0\n")
      fd.close()

#  Loop over each Stokes parameter, creating maps from each observation
#  if reqired.
   for qui in ('I', 'Q', 'U'):

#  Pass on to the next parameter if we are not creating a map for the
#  current parameter. Also set up pointers to the arrays etc to use for
#  the current Stokes parameter.
      if qui == 'I':
         if imap:
            qui_maps = imaps
            qui_list = ilist
            conf = iconf
            coadd = imap
            pcathresh = pcathresh_i
            imap_cat = NDG(1)
            coadd_cat = imap_cat
            this_ip = ""
         else:
            continue

      elif qui == 'Q':
         if qmap:
            qui_maps = qmaps
            qui_list = qlist
            conf = quconf
            coadd = qmap
            pcathresh = pcathresh_qu
            qmap_cat = NDG(1)
            coadd_cat = qmap_cat
            this_ip = ip
         else:
            continue

      else:
         if umap:
            qui_maps = umaps
            qui_list = ulist
            conf = quconf
            coadd = umap
            pcathresh = pcathresh_qu
            umap_cat = NDG(1)
            coadd_cat = umap_cat
            this_ip = ip
         else:
            continue


#  Set up the parameter string describing the initial sky map (if any),
#  together with (if not using skyloop) the map to use as the reference.
      initsky = ""
      if skyloop:
         if initskys[qui]:
            initsky = "initialsky={0}".format(initskys[qui])
      else:
         if initskys[qui]:
            tref = initskys[qui]
            fd = open( conf, "a" )
            fd.write('importsky=ref\n')
            fd.close()
            initsky = "lbnd=\[{0},{1}\] ubnd=\[{2},{3}\]".format(lx,ly,ux,uy)
         elif maskmap:
            tref = astmask
         else:
            tref = ref

      if maskmap:
         initsky += " mask2={0}".format(astmask)

#  Get the suffix to append to the end of individual observation maps
#  created below (e.g. "_Imap" for ext-masked I maps or "_imap" for
#  auto-masked I maps etc).
      if automask:
         suffix = qui.lower()+"map"
      else:
         suffix = qui+"map"

#  Issue a warning if no time-streams have been supplied for the current
#  Stokes parameter 9not an error since there may be some pre-existing
#  maps for the current Stokes parameter)..
      if len(qui_list) == 0:
         msg_out( "No usable {0} time-stream data supplied.".format(qui))
         msg_out( "Looking for pre-existing {0} maps...".format(qui))

#  Initialise a list to hold the identifiers for observations that have a
#  weight that is too low to use (i.e. which create maps that look very
#  little like the mean of all maps).
      badkeys = []

#  See if the coadd already exists.
      try:
         junk = NDG( coadd, "*" )
         coadd_exists = True
      except starutil.NoNdfError:
         coadd_exists = False

#  Do we need the individual observation maps? This will be so if an
#  output catalogue is being created with a BINSIZE greater than the map
#  pixel size AND the MAPVAR parameter is set true. In such cases we need
#  to bin up the individual observation maps before using them to
#  determine the variances needed to calculate the vector uncertainties.
      pxsize = 0.0
      if outcat:

#  If the map pixel size has not yet been determined, get it from the
#  supplied coadd (if it exists) or the reference map (if it exists)
#  or use the PIXSIZE parameter otherwise.
         if coadd_exists:
            invoke("$KAPPA_DIR/ndftrace ndf={0} quiet=yes".format(coadd) )
            pxsize = float(get_task_par( "FPIXSCALE(1)", "ndftrace" ))
         elif ref != "!":
            invoke("$KAPPA_DIR/ndftrace ndf={0} quiet=yes".format(ref) )
            pxsize = float(get_task_par( "FPIXSCALE(1)", "ndftrace" ))
         else:
            pxsize = pixsize

#  If the catalogue bin size was not specified, it defaults to the map pixel
#  size.
         if not binsize:
            binsize = pxsize

#  Report an error if the catalogue bin size is significantly smaller than
#  the pixel size.
         if binsize < pxsize*0.99:
            raise starutil.InvalidParameterError("Requested catalogue bin "
                          "size ({0}) is smaller than the map pixel size "
                          "({1}).".format(binsize,pxsize))

#  If the catalogue bin size is almost equal to the pixel size, then we do
#  not need the individual observation maps. Also, the catalogue will not be
#  binned.
         elif binsize < 1.01*pxsize:
            cat_needs_obsmaps = False
            binned_cat = False

#  If the catalogue bin size is significantly larger than the pixel size
#  and MAPVAR=no (i.e. we are assuming the makemap variances are accurate),
#  then we still do not need the individual observation maps. But now the
#  catalogue will be binned.
         elif not mapvar:
            cat_needs_obsmaps = False
            binned_cat = True

#  If the catalogue bin size is significantly larger than the pixel size,
#  and MAPVAR=yes (i.e. variances are measured from the spread of values),
#  we need the individual observation maps, and the catalogue will be binned.
         else:
            cat_needs_obsmaps = True
            binned_cat = True

#  If we are not creating an output catalogue then we do not need the
#  individual observation maps for catalogue creation.
      else:
         cat_needs_obsmaps = False
         binned_cat = False

#  Indicate that no new maps have yet been made.
      make_new_maps = False

#  If we are using skyloop to generate the observation maps...
#  -----------------------------------------------------------
      if skyloop:

#  If the current coadd already exists and is being used as the IP
#  reference map, we do not attempt to recreate it (or the individual
#  observation maps), regardless of the value of parameter REUSE. But
#  we do make individual observation maps if we need them to create the
#  catalogue.
         if coadd_exists and coadd == ipref and not cat_needs_obsmaps:
            make_new_maps = False

#  Otherwise, if most of the expected maps, including the coadd, already
#  exist and REUSE is True, we do not re-create the maps. But if most maps
#  are missing we need to re-create them all (when using skyloop we cannot
#  re-create some without re-creating them all). The problem here is that
#  we cannot determine the list of "expected" maps with 100% confidence. The
#  "qui_list" list may include very short chunks from observations/subscans
#  that were split, and for which skyloop could not create an observation
#  map. So we cannot just use "qui_list" to define the list of expected maps.
#  Instead, just impose an arbitrary - but probably safe - restriction that
#  at least 50% of the maps implied by "qui_list" must exist.
         else:
            make_new_maps = True
            if coadd_exists and reuse:
               nexist = 0
               for key in qui_list.keys():
                  obsmap_path = "{0}/{1}_{2}.sdf".format(mapdir,key,suffix)
                  if os.path.exists(obsmap_path):
                     qui_maps[key] = NDG(obsmap_path)
                     nexist += 1

               if nexist > len(qui_list)/2:
                  make_new_maps = False

         if not make_new_maps:
            msg_out("   Re-using previously created maps")
         else:
            badlist = []

#  Prevent accidental over-writing of existing maps (reuse=no gives us permission
#  to over-write existing maps).
            if reuse:
               nbad = 0
               for key in qui_list.keys():
                  obsmap_path = "{0}/{1}_{2}.sdf".format(mapdir,key,suffix)
                  if os.path.exists(obsmap_path):
                     if nbad == 0:
                        msg_out("\n ")
                     nbad += 1
                     msg_out( obsmap_path )

               if nbad > 0:
                  raise starutil.InvalidParameterError( "\npol2map would "
                           "over-write the {0} existing map(s) listed above."
                           " Is this what you want to do? If so, please "
                           "delete these existing maps and re-run pol2map.".
                           format(nbad) )

#  Create a directory in which to put the individual observation maps.
            obsdir = NDG.subdir()

#  Create a text file holding the paths to all the input time-stream
#  files for the current Stokes parameter. Note if there are any files
#  taken between 20150606 and 20150930, which require an extra Az/El
#  correction.
            azelcor = False
            inpaths  = NDG.tempfile()
            fd = open( inpaths, "w" )
            for key in qui_list:
               (ut,obs,subscan) = key.split('_')
               ut = int(ut)
               if ut >= 20150606 and ut <=20150929:
                 azelcor = True

               for path in qui_list[ key ]:
                  fd.write( path+"\n" )
            fd.close()

#  Create a text file holding the pointing corrections for all input
#  time-streams. First include any AZ/EL pointing correction, for
#  data between 20150606 and 20150930, ensuring the correction goes
#  to zero for later data (skyloop process multiple observations
#  simultaneously, so old and new observations may be processed together).
            if azelcor:
               pntfile = NDG.tempfile()
               fd = open(pntfile,"w")
               fd.write("# system=azel\n")
               fd.write("# tai dlon dlat\n")
               fd.write("57179 32.1 27.4\n")
               fd.write("57295.5 32.1 27.4\n")
               fd.write("57295.6 0.0 0.0\n")
               fd.write("57295.7 0.0 0.0\n")
               fd.close()
            else:
               pntfile = "!"

#  Create the timeframe and mapping used to convert DATE-OBS values (utc)
#  to mjd TAI values.
            utcframe = NDG.tempfile()
            invoke('$ATOOLS_DIR/asttimeframe options="timescale=utc" result={0}'.format(utcframe) )
            taiframe = NDG.tempfile()
            invoke('$ATOOLS_DIR/asttimeframe options=! result={0}'.format(taiframe) )
            utc2tai = NDG.tempfile()
            invoke('$ATOOLS_DIR/astconvert from={0} to={1} domainlist=! result={2}'.format(utcframe,taiframe,utc2tai) )

#  Store a weight within each time series file. Makemap uses these
#  weights when coadding all the observations together. The weights will
#  have been found on a previous run of pol2map as part of generating the
#  auto-masked I map. The weight or an observation will have been stored in
#  the CHUNKWGT header in the auto-masked imap. Transfer it to the time-series
#  file.
            allone = True
            if obsweight:
               nwgt = 0
               nnowgt = 0
               for key in qui_list:
                  try:
                     hmap = NDG("{0}/{1}_imap".format(mapdir,key))
                     wgt = float( get_fits_header( hmap, "CHUNKWGT", report=True ))
                     nwgt += 1
                  except Exception:
                     nnowgt += 1
                     wgt = 1.0

                  if wgt < 1.0:
                     allone = False

                  if wgt < wgtlim:
                     msg_out("WARNING: pol2map will exclude observation {0} "
                             "since it's weight ({1}) is below the value "
                             "of parameter WEIGHTLIM ({2}).".
                             format( key, wgt, wgtlim ))
                     for path in qui_list[ key ]:
                        badlist.append( path )
                     badkeys.append( key )

                  invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=CHUNKWGT "
                         "edit=a value={1} comment=\"'Weight for this chunk of data'\""
                         " position=! mode=interface".format(NDG(qui_list[key]),wgt))

               if nwgt == 0:
                  raise starutil.InvalidParameterError( "\npol2map was run "
                           "with 'skyloop=yes obsweight=yes', so each "
			   "pre-existing auto-masked I map is expected to "
			   "contain a weight. But none did. The "
			   "auto-masked I maps should have been created "
			   "using 'skyloop=no obsweight=yes'. Was this "
			   "done?\n" )
               elif nnowgt > 0:
                  msg_out("\nWARNING: pol2map was run with 'skyloop=yes "
 			   "obsweight=yes', so each pre-existing "
			   "auto-masked I map is expected to contain a "
			   "weight. But {0} did not (a default weight of "
			   "1.0 will be used for each such map). All "
			   "auto-masked I maps should have been created "
			   "using 'skyloop=no obsweight=yes'. Was this "
			   "done?\n".format(nnowgt))

#  If any of the weights are not 1.0, modify the config to indicate that
#  makemap should read the weights from the CHUNKWGT header in the
#  time-stream data . Otherwise ensure no weighting is done inside
#  skyloop by setting the config parameter "chunkweight" to its default
#  weight of 1.0.
            fd = open( conf, "a" )
            if not allone:
               fd.write('chunkweight="CHUNKWGT"\n')

#  If any observations have been omitted due to low weights, read the old
#  raw data list and create a new one omitting the required fiules.
               if len( badlist ) > 0:
                  oldpaths = inpaths
                  inpaths  = NDG.tempfile()
                  fd1 = open( inpaths, "w" )
                  fd2 = open( oldpaths, "r" )
                  for line in fd2:
                     line = line.strip()
                     if line and line not in badlist:
                        fd1.write( line+"\n" )
                  fd1.close()
                  fd2.close()

            else:
               fd.write('chunkweight=1.0\n')
            fd.close()

#  Store a calibration correction factor within each time series file.
#  Makemap uses these factors to scale the maps made from individual
#  observations. The factors will have been found on a previous run of
#  pol2map as part of generating the auto-masked I map. The factor for
#  an observation will have been stored in the CHUNKFAC header in the
#  auto-masked imap. Transfer it to the time-series file. Only use the
#  factors if all observations have a factor.
            allone = True
            if normalise:
               for key in qui_list:
                  try:
                     hmap = NDG("{0}/{1}_imap".format(mapdir,key))
                     factor = float( get_fits_header( hmap, "CHUNKFAC",
                                                      report=True ))
                  except starutil.NoValueError:
                     msg_out("\nWARNING: pol2map was run with 'normalise=yes', "
 			   "so each pre-existing I map is expected to contain "
                           "a normalisation factor in FITS header CHUNKFAC. But "
                           "{0} did not. A default factor of 1.0 will be used.".
			   format(hmap))
                     factor = 1.0

                  except starutil.NoNdfError:
                     factor = 1.0

                  if factor != 1.0:
                     allone = False

                  invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=CHUNKFAC edit=a "
                         "value={1} comment=\"'Calibration correction factor'\" "
                         "position=! mode=interface".format(NDG(qui_list[key]),factor))

#  If any of the factors are not 1.0, modify the config to indicate that
#  makemap should read the factors from the CHUNKFAC header in the
#  time-stream data . Otherwise ensure no factors are used inside
#  skyloop by setting the config parameter "chunkfactor" to its default
#  weight of 1.0.
            fd = open( conf, "a" )
            if not allone:
               fd.write('chunkfactor="CHUNKFAC"\n')
            else:
               fd.write('chunkfactor=1.0\n')
            fd.close()

#  Add pointing corrections to the file for each observation
            corrections = {}
            fd = None
            for key in qui_list:

#  If an auto-masked I map from a previous run exists for the current
#  observation, see if it has pointing corrections recorded in its FITS
#  header. If so, we use them when creating the new map.
               try:
                  hmap = NDG("{0}/{1}_imap".format(mapdir,key))
                  dx = get_fits_header( hmap, "PNTRQ_DX" )
                  dy = get_fits_header( hmap, "PNTRQ_DY" )
               except starutil.NoNdfError:
                  dx = None
                  dy = None

#  If required, create a file to hold the correction If a file is
#  already in use (because of the data being old) append the new
#  pointing correction to the end of the file, preceeded by an
#  "end-of-table" marker (two minus signs). Makemap will then apply
#  both correction.
               if dx is not None and dy is not None:
                  dx = float( dx )
                  dy = float( dy )
                  if pntfile == "!":
                     pntfile = NDG.tempfile()
                     fd = open(pntfile,"w")
                     fd.write("# system=tracking\n")
                     fd.write("# tai dlon dlat\n")

                  elif fd is None:
                     fd = open(pntfile,"a")
                     fd.write("--\n")
                     fd.write("# system=tracking\n")
                     fd.write("# tai dlon dlat\n")

#  Add two lines to the correction file - the first corresponds to the
#  earliest data in the current observation and the second corresponds
#  to the latest data in the current observation. The pointing correction
#  is constant between these two times. We need to use an AstTimeFrame to
#  get the MJD TAI corresponding to the DATE-OBS abd DATE-END FITS keywords.
#  Add 1 second to each time as a safety margin.
                  utcstart = get_fits_header( hmap, "DATE-OBS" )
                  invoke('$ATOOLS_DIR/astunformat this={0} axis=1 value={1}'.format(utcframe,utcstart) )
                  utcstart = get_task_par( "dval", "astunformat" )
                  taistart = invoke('$ATOOLS_DIR/asttran1 this={0} forward=true xin={1}'.format(utc2tai,utcstart) )
                  taistart = float(taistart) - 1.0/86400.0

                  utcend = get_fits_header( hmap, "DATE-END" )
                  invoke('$ATOOLS_DIR/astunformat this={0} axis=1 value={1}'.format(utcframe,utcend) )
                  utcend = get_task_par( "dval", "astunformat" )
                  taiend = invoke('$ATOOLS_DIR/asttran1 this={0} forward=true xin={1}'.format(utc2tai,utcend) )
                  taiend = float(taiend) + 1.0/86400.0

                  corrections[taistart] = "{0} {1} {2}\n".format(taistart,dx,dy)
                  corrections[taiend] = "{0} {1} {2}\n".format(taiend,dx,dy)

#  Ensure the corrections are sorted into monotonic increasing TAI
#  values, write them out to the file and then close the file.
            if fd is not None:
               for tai in sorted(corrections.keys()):
                  fd.write(corrections[tai])
               fd.close()

#  If we will be smoothing the map to the 850 um resolution, we need to
#  put the original (unsmoothed) coadd in a different NDG object. The final
#  (smoothed) coadd will be put into the NDF specified by parameter I/Q/UOUT .
            if smooth450:
               unsmoothed = NDG( 1 )
            else:
               unsmoothed = coadd

#  Invoke skyloop, putting the individual observation maps in obsdir.
            old_ilevel = starutil.ilevel
            starutil.ilevel = starutil.ATASK
            skylog = NDG.tempfile()
            extra = "pointing={0}".format(pntfile)
            msg_out("Invoking skyloop to create the {0} map from all "
                    "supplied observations".format(qui))
            msg_out("\n--------------------------------------------------\n")
            if not maskmap:
               invoke("$SMURF_DIR/skyloop.py in=^{0} config=^{1} out={2} ref={3} extra={4} "
                      "pixsize={5} {6} obsdir={7} retain={8} logfile={9} {10}".
                      format(inpaths,conf,unsmoothed,ref,extra,pixsize,this_ip,obsdir,
                             retain,skylog,initsky),
                             msg_level=starutil.PROGRESS,cmdscreen=False)
            else:
               invoke("$SMURF_DIR/skyloop.py in=^{0} config=^{1} out={2} ref={3} extra={4} "
                      "pixsize={5} {6} {7} obsdir={8} retain={9} logfile={10} {11}".
                      format(inpaths,conf,unsmoothed,astmask,extra,pixsize,this_ip,
                             pcamaskpar,obsdir,retain,skylog,initsky),
                             msg_level=starutil.PROGRESS,cmdscreen=False)

            starutil.ilevel = old_ilevel
            msg_out("\n--------------------------------------------------\n")
            msg_out("Back from skyloop...")

#  Append the skyloop logfile output to the pol2map logfile.
            msg_log("Contents of skyloop logfile follows...")
            append_logfile( skylog )
            msg_log("\n--------------------------------------------------\n")
            msg_log("Back to pol2map...")

#  Smooth the coadd to the 850 um resolution if required.
            if smooth450:
               Smooth450( unsmoothed, coadd )

#  Modify the names of the observation maps so that they use the same scheme
#  as those created by makemap below, and move them into the main maps
#  directory. If required, smooth them to the 850 um resolution at the
#  same time.
            ut_previous = ""
            obs_previous = ""
            for key in sorted(qui_list.keys()):
               (ut,obs,subscan) = key.split('_')
               obs = obs.lstrip('0')

#  First chunk for this ut/obs? If so, the corresponding skyloop map name
#  will include no chunk number.
               if ut != ut_previous or obs != obs_previous:
                  oldpath = "{0}/{1}_{2}.sdf".format(obsdir,ut,obs)
                  ut_previous = ut
                  obs_previous = obs
                  next_chunk = 1

#  Subsequent chunks for this ut/obs are assumed to be in the same order
#  as the subscans number within "key".
               else:
                  oldpath = "{0}/{1}_{2}_chunk{3}.sdf".format(obsdir,ut,obs,next_chunk)
                  next_chunk += 1

#  Get the new file name and check the old file exists.
               newpath = "{0}/{1}_{2}.sdf".format(mapdir,key,suffix)
               if os.path.exists(oldpath):

#  If so, copy quality information from the coadd created by skyloop to
#  the old file.
                  invoke("$KAPPA_DIR/setqual ndf={0} like={1}".format(oldpath,coadd))

#  Clear its bad bits mask.
                  invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(oldpath))

#  Do the rename, smoothing it to the 850 resolution at the same time if required.
                  if smooth450:
                     Smooth450( oldpath, newpath )
                  else:
                     shutil.move( oldpath, newpath )

#  Add it to the dictionary of maps holding the current stokes parameter.
                  qui_maps[key] = NDG(newpath)

#  Add it to the list of maps in mapdir.
                  new_maps.append( qui_maps[key] )

               else:
                  msg_out("Failed to find skyloop observation map "+oldpath+". Continuing without it..." )
                  del qui_list[key]



#  If we are using makemap to generate the observation maps...
#
#  If the current coadd already exists we do not attempt to recreate it (or
#  the individual observation maps), regardless of the value of parameter
#  REUSE. But we do make individual observation maps if we need them to
#  create the catalogue.
#  -----------------------------------------------------------
      elif ( not coadd_exists ) or cat_needs_obsmaps:

#  Loop over all the time series files for the current Stokes parameter. Each
#  separate observation will usually have one time series file (although
#  there may be more if the observation was split into two or more discontiguous
#  chunks). We form a map for each observation chunk present in the supplied
#  list of input raw data.
         for key in qui_list:

#  Get the Stokes time stream files for the current observation chunk.
            isdf = NDG( qui_list[ key ] )

#  Get the chunk weight. If it is below the minimum, skip this observation.
            wgt = get_fits_header( isdf[0], "CHUNKWGT" )
            if wgt and wgt < wgtlim:
               del qui_maps[key]
               badkeys.append( key )
               msg_out("\nSkipping {0} since its weight ({1}) is too low (<{2})...\n".format(key,wgt,wgtlim) )
               continue
            else:
               msg_out("\nMaking {1} map from {0}...\n".format(key,qui) )

#  AZ/EL pointing correction, for data between 20150606 and 20150930. Not
#  using skyloop here, so only one observation will be processed at any one
#  time, so no danger of having old and new data files together.
            ut = int(get_fits_header( isdf[0], "UTDATE", True ))
            if ut >= 20150606 and ut <= 20150929:
               pntfile = NDG.tempfile()
               fd = open(pntfile,"w")
               fd.write("# system=azel\n")
               fd.write("# tai dlon dlat\n")
               fd.write("54000 32.1 27.4\n")
               fd.write("56000 32.1 27.4\n")
               fd.close()
            else:
               pntfile = "!"

#  If an auto-masked I map from a previous run exists for the current
#  observation, see if it has pointing corrections recorded in its FITS
#  header. If so, we use them when creating the new map.
            try:
               hmap = NDG("{0}/{1}_imap".format(mapdir,key))
               dx = get_fits_header( hmap, "PNTRQ_DX" )
               dy = get_fits_header( hmap, "PNTRQ_DY" )
            except starutil.NoNdfError:
               dx = None
               dy = None

#  Create the pointing correction file to use when running makemap. If
#  a file is already in use (because of the data being old) append the
#  new pointing correction to the end of the file, preceeded by an
#  "end-of-table" Marker (two minus signs). Makemap will then apply
#  both correction.
            if dx is not None and dy is not None:
               dx = float( dx )
               dy = float( dy )
               if pntfile == "!":
                  pntfile = NDG.tempfile()
                  fd = open(pntfile,"w")
               else:
                  fd = open(pntfile,"a")
                  fd.write("--\n")

               fd.write("# system=tracking\n")
               fd.write("# tai dlon dlat\n")
               fd.write("54000 {0} {1}\n".format(dx,dy))
               fd.write("56000 {0} {1}\n".format(dx,dy))
               fd.close()

#  Get the path to the map.
            mapname = "{0}/{1}_{2}".format(mapdir,key,suffix)

#  If REUSE is True and an old map exists, re-use it.
            try:
               if reuse:
                  qui_maps[key] = NDG(mapname, True)
                  msg_out("   Re-using previously created map {0}".format(qui_maps[key]))
               else:
                  raise starutil.NoNdfError("Ignoring pre-existing data")

#  Otherwise create a new map.  The call signature for makemap depends on
#  whether an external mask is being supplied or not.
            except starutil.NoNdfError:
               make_new_maps = True

               if dx is not None and dy is not None:
                  msg_out( "   Using pre-calculated pointing corrections of ({0:5.1f},{1:5.1f}) arc-seconds".format(dx,dy) )

               qui_maps[key] = NDG(mapname, False)

#  If we will be smoothing the map to the 850 um resolution, we need to
#  put the original (unsmoothed) map in a different NDG object. The final
#  (smoothed) map will be put into the MAPDIR directory.
               if smooth450:
                  unsmoothed = NDG( 1 )
               else:
                  unsmoothed = qui_maps[key]

               try:

#  If we are using the default value for PCA.PCATHRESH (as indicated by
#  pcathresh being zero), we need to look out for makemap not converging.
#  This can happen for very bright sources. If makemap fails to converge, we
#  try again using a smaller value for the PCA.PCATHRESH parameter. Once we
#  have found a value for PCA.PCATHRESH that allows convergence to be reached,
#  we use this same value for all subsequent maps. Set ABORTSOON=YES so that
#  makemap aborts as soon as it becomes clear that convergence will not be
#  reached in the allowed number of iterations. We only do this if variable
#  "pcathresh" is zero, indicating that no value has yet been determined for
#  PCA.PCATHRESH. We also require the NUMITER config parameter is negative
#  - i.e. MAPTOL defines convergence.
                  sel =  "450=1,850=0" if ( filter == 450 ) else "450=0,850=1"
                  numiter = float( invoke("$KAPPA_DIR/configecho name=numiter config=^{0} "
                                          "defaults=$SMURF_DIR/smurf_makemap.def "
                                          "select=\"\'{1}\'\"".format(conf,sel)))
                  if pcathresh == 0 and numiter < 0:
                     pcathresh = pcathresh_def1 if automask else pcathresh_def2
                     abpar = "abortsoon=yes"
                  else:
                     abpar = ""

                  attempt = 0
                  again = True
                  while again:
                     attempt += 1

                     if not maskmap:
                        invoke("$SMURF_DIR/makemap in={0} config=^{1} out={2} ref={3} pointing={4} "
                               "pixsize={5} {6} {7} {8}".format(isdf,conf,unsmoothed,tref,pntfile,pixsize,ip,abpar,initsky))
                     else:
                        invoke("$SMURF_DIR/makemap in={0} config=^{1} out={2} ref={3} pointing={4} "
                            "pixsize={5} {6} {7} {8} {9}".format(isdf,conf,unsmoothed,tref,pntfile,
                                                                 pixsize,ip,pcamaskpar,abpar,initsky))

#  If we do not yet know what pcathresh value to use, see if makemap aborted
#  due to slow convergence. If so, reduce the number of PCA components
#  removed on each iteration by 25% and re-run makemap.
                     if abpar != "":
                        abortedat = int( float( get_task_par( "abortedat", "makemap" ) ) )
                        if abortedat == 0:
                           again = False
                           if attempt > 1:
                              msg_out( "MAKEMAP converged succesfully, so all further "
                                       "maps will be created using PCA.PCATHRESH={0}.".
                                       format( pcathresh ) )

                        elif attempt < 20:
                           reduction = int( -pcathresh * 0.25 )
                           if reduction < 2:
                              reduction = 2
                           pcathresh_old = pcathresh
                           pcathresh = -( -pcathresh - reduction )
                           if pcathresh > -5:
                              pcathresh = -5

                           if pcathresh <= pcathresh_old:
                              again = False
                              msg_out("MAKEMAP failed to converge but we have "
                                      "reached the lower limit for PCA.PCATHRESH, so "
                                      "all further maps will be created using "
                                      "PCA.PCATHRESH={0}.".format( pcathresh ) )
                           else:
                              msg_out("MAKEMAP failed to converge - trying "
                                      "the current observation again with "
                                      "PCA.PCATHRESH set to {0} (it was {1}).".
                                      format(pcathresh,pcathresh_old))
                              fd = open( conf, "a" )
                              fd.write( "pca.pcathresh = {0}\n".format( pcathresh ) )
                              fd.close()
                        else:
                           again = False
                           msg_out( "MAKEMAP failed to converge again - "
                                    "giving up and using PCA.PCATHRESH={0}.".
                                    format( pcathresh ) )

#  If we already knew the value to use for PCA.PCATHRESH, just proceeed without
#  checking convergence.
                     else:
                        again = False

#  If required smooth the output map to the resolution of the 850 um beam.
                  if smooth450:
                     Smooth450( unsmoothed, qui_maps[key] )

#  Store FITS headers holding the pointing corrections that were actually used.
                  if dx is not None:
                     sym = invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=get name='Symbol(1)'".
                                        format(qui_maps[key]))
                     invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=POINT_DX "
                            "edit=a value={1} comment=\"'Used {2} pointing correction [arcsec]'\""
                            " position=! mode=interface".format(qui_maps[key],dx,sym))

                  if dy is not None:
                     sym = invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=get name='Symbol(2)'".
                                  format(qui_maps[key]))
                     invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=POINT_DY "
                            "edit=a value={1} comment=\"'Used {2} pointing correction [arcsec]'\""
                            " position=! mode=interface".format(qui_maps[key],dy,sym))

#  If we are processing I data with makemap (i.e. "step 1"), and no ref
#  map was given, then use the I map just created as the ref map for the
#  remaining observations. This ensures that all the auto-masked I maps
#  are aligned with each other.
                  if ref == "!" and qui == 'I':
                     ref = qui_maps[key]

#  If makemap failed, warn the user and delete any map that was created,
#  and pass on to the next observation chunk.
               except starutil.AtaskError:
                  msg_out("WARNING: makemap failed - could not produce a {1} map "
                          "for observation chunk {0}".format(key,qui) )
                  try:
                     invoke("$KAPPA_DIR/erase object={0} ok=yes".format(qui_maps[key]))
                  except starutil.AtaskError:
                     pass
                  del qui_maps[key]
                  if abpar != "":
                     pcathresh = 0
                  continue

#  A map was obtained successfully. Add it to the list of maps in mapdir.
            new_maps.append( qui_maps[key] )













#  -----------  CREATE THE COADDED MAP FOR THE CURRENT STOKES PARAMETER -------------

#  We do not need to create the coadd if it already exists and has not
#  been surplanted by a new set of observation maps.
      if coadd_exists and not make_new_maps:
         msg_out("Re-using existing {0} coadd".format(qui))

      else:

#  Check some good maps remain to be processed.
         if len(qui_maps) == 0:
            raise starutil.InvalidParameterError("No usable {0} maps remains "
                                                 "to be coadded.".format(qui))

#  If skyloop was used above, the coadd will already exist. First deal
#  with cases where skyloop was not used.
         allmaps = NDG( list( qui_maps.values() ) )
         if not skyloop:

#  If we have only one observation just copy it to the output maps.
            if len(qui_maps) == 1:
               key = list(qui_maps)[0]
               invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(qui_maps[key],coadd))

#  If we have more than one observation, coadd them. If we are processing
#  I data, determine weights for each observation based on how similar
#  the observation's map is to the coadd (this weight is stored in the
#  observation's I map) . Also coadd the extension NDFs (EXP_TIMES and WEIGHTS),
#  but without normalisation so that the coadd is the sum rather than the
#  mean of the inputs. Note, for skyloop the extension NDFs need to be copied
#  from the coadds created by skyloop, rather than forming them by coadding
#  the individual maps (skyloop does not put the extension NDFs into the
#  individual observation maps).
            elif len(qui_maps) > 1:
               MakeCoadd( qui, qui_maps, imaps, coadd, mapvar, automask, obsweight )

               try:
                  invoke("$KAPPA_DIR/erase object={0}.more.smurf.exp_time ok=yes".format(coadd))
                  invoke("$KAPPA_DIR/wcsmosaic in={{{0}}}.more.smurf.exp_time lbnd=! ref=! "
                         "out={1}.more.smurf.exp_time conserve=no method=bilin norm=no "
                         "variance=no".format(allmaps,coadd))
               except starutil.AtaskError:
                  msg_out( "No exposure time array will be present in {0}".format(coadd))

               try:
                  invoke("$KAPPA_DIR/erase object={0}.more.smurf.weights ok=yes".format(coadd))
                  invoke("$KAPPA_DIR/wcsmosaic in={{{0}}}.more.smurf.weights lbnd=! ref=! "
                         "out={1}.more.smurf.weights conserve=no method=bilin norm=no "
                         "variance=no".format(allmaps,coadd))
               except starutil.AtaskError:
                  msg_out( "No weights array will be present in {0}".format(coadd))


#  Now deal with cases where a coadd has already been created by skyloop.
#  The existing coadd can be used directly unless the MAPVAR parameter is
#  True, in which case we need to replace the variances in the coadd with
#  variances derived from the dispersion of the individual observation maps
#  created by skyloop.
         elif mapvar and make_new_maps:
            msg_out("MAPVAR is YES, so forming new variances for {0} skyloop coadd:".format(qui))

            if len(allmaps) < 3:
               raise starutil.InvalidParameterError("\nCannot combine {0} {2} using "
                                  "MAPVAR=YES since only {1} {0} maps are available".
                                  format(qui, len(allmaps), "map" if (len(allmaps)==1) else "maps" ) )

            junk = NDG( 1 )
            invoke("$KAPPA_DIR/wcsmosaic in={0} lbnd=! ref=! out={1} "
                   "conserve=no method=near variance=yes genvar=yes".
                   format(allmaps,junk))
            invoke("$KAPPA_DIR/setvar ndf={0} from={1} comp=Variance".
                   format(coadd,junk))

#  If required, trim off the edges of the coadds that have an exposure
#  time less than "trim" times the mean exposure time.
         if trim is not None:
            msg_out("TRIM is {0}, so trimming the edges of {1}".format(trim,coadd))
            trimmed = exptrim( coadd, trim )
            invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(trimmed,coadd))

         if len( badkeys ) > 0:
            msg_out( "\n\nWARNING: The following observations were omitted "
                     "because their auto-masked maps look peculiar:")
            for key in badkeys:
               msg_out( key )



#  -----------  CREATE THE MAP TO USE WHEN CREATING THE CATALOGUE -------------

#  If an output vector catalogue is being created, we now create the I, Q
#  or U map that will be used in the creation of the catalogue.
      if outcat:

#  If the catalogue bin size is the same as the map pixel size, we just
#  use the above coadd when creating the catalogue.
         if not binned_cat:
            invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(coadd,coadd_cat))

#  If the catalogue bin size is greater than the map pixel size, we need
#  to bin the coadd up before using it to create the catalogue.
         else:

#  If we have not already done so, create a reference map defining the
#  binned up pixel grid to be used when creating the catalogue. Do this
#  by squashing the coadd map.
            if not catref:
               msg_out("The output vector catalogue will be based on maps "
                       "that are binned up to {0} arcsec pixels.".format(binsize))
               catref = NDG( 1 )
               invoke("$KAPPA_DIR/sqorst in={0} out={1} mode=pixelscale "
                      "method=near centre=origin pixscale=\"\'{2},{2},*\'\"".
                      format(coadd,catref,binsize))

#  Bin the coadd created above so that it is aligned with the above reference
#  map.
            invoke("$KAPPA_DIR/wcsalign in={0} lbnd=! out={1} ref={2} "
                   "conserve=no method=sincsinc params=\[2,0\] rebin=yes".
                   format(coadd,coadd_cat,catref))

#  If the catalogue bin size is significantly larger than the pixel size,
#  and MAPVAR=yes (i.e. variances are measured from the spread of values),
#  we need to be careful about variances The noise in the coadd is correlated
#  from pixel to pixel (i.e. we see random large scale artefacts in SCUBA-2
#  maps). If we were just to bin the coadd in the normal way (e.g. using
#  wcsalign or sqorst) to get the requested bin size, this correlated noise
#  would be ignored, so the binned up variances would be too small. Instead,
#  we need to bin up the individual observation maps first, then estimate
#  the variances by looking at the spread of binned-up values at each point.
            if mapvar:

#  If we do not have any individual observation maps, then we can't use
#  the MAPVAR=yes approach. In which case all we can do is retain the
#  variances in the binned coadd, and issue a warning.
               if len(qui_maps) == 0:
                  msg_out("\n\nWARNING: No observation {0} maps found in "
                          "{1} and MAPVAR=YES. Consequently the error "
                          "estimates in the output catalogue may be too "
                          "low.\n".format( qui, mapdir ) )

#  Otherwise, bin them all up so that they are aligned with the catalogue
#  reference map. Then make a coadd from them, generating variances at
#  the same time from the spread of values at each pixel. Then transfer
#  the variances into the binned-up coadd created above.
               else:
                  allmaps = NDG( list( qui_maps.values() ) )
                  catmaps = NDG( allmaps )
                  invoke("$KAPPA_DIR/wcsalign in={0} lbnd=! out={1} ref={2} "
                         "conserve=no method=sincsinc params=\[2,0\] rebin=yes".
                         format(allmaps,catmaps,catref))

                  if len(catmaps) < 3:
                     raise starutil.InvalidParameterError("\nCannot combine {0} {2} using "
                                  "MAPVAR=YES since only {1} {0} maps are available".
                                  format(qui, len(catmaps), "map" if (len(catmaps)==1) else "maps" ) )

                  temp = NDG(1)
                  invoke("$KAPPA_DIR/wcsmosaic in={0} lbnd=! ref=! out={1} "
                         "conserve=no method=near variance=yes genvar=yes".
                         format(catmaps,temp))

                  invoke("$KAPPA_DIR/setvar ndf={0} from={1} comp=Variance".
                         format(coadd_cat,temp))





#  ------------  END OF INDIVIDUAL I,Q,U   BLOCK. ---------------------------

#  Ensure the returned Q, U and I images all have the same bounds, equal to
#  the overlap region between them. To get the overlap region, use MATHS to
#  add them together. Then use setbound to set the bounds of each to match
#  the overlap area.
   if qmap and umap and imap:
      tmp = NDG( 1 )
      invoke( "$KAPPA_DIR/maths exp=\"'ia+ib+ic'\" ia={0} ib={1} ic={2} out={3}".
              format(qmap,umap,imap,tmp) )
      invoke( "$KAPPA_DIR/setbound ndf={0} like={1}".format(qmap,tmp) )
      invoke( "$KAPPA_DIR/setbound ndf={0} like={1}".format(umap,tmp) )
      invoke( "$KAPPA_DIR/setbound ndf={0} like={1}".format(imap,tmp) )




#  -----------  CREATE VECTOR CATALOGUE ------------------------


# The rest we only do if an output catalogue is reqired.
   if outcat:

#  We need I, Q and U maps to create a catalogue. The pixel size in
#  these maps will be equal to the value of parameter BINSIZE.
      if imap_cat and qmap_cat and umap_cat:

#  If the I coadd was created from non-POL2 data incorporate the POL2
#  degradation factor.
         inbeam = get_fits_header( imap_cat, "INBEAM" )
         if not inbeam or ("pol" not in inbeam):
            tmp = NDG( 1 )
            invoke( "$KAPPA_DIR/cdiv in={0} out={1} scalar=1.35".
                 format(imap_cat,tmp ))
            imap_cat = tmp

#  Ensure the Q, U and I images all have the same bounds, equal to the
#  overlap region between them. To get the overlap region, use MATHS to
#  add them together. Then use ndfcopy to produce the sections from each,
#  which match the overlap area.
         tmp = NDG( 1 )
         invoke( "$KAPPA_DIR/maths exp=\"'ia+ib+ic'\" ia={0} ib={1} ic={2} out={3}".
                 format(qmap_cat,umap_cat,imap_cat,tmp) )
         qtrim = NDG( 1 )
         invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(qmap_cat,tmp,qtrim) )
         utrim = NDG( 1 )
         invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(umap_cat,tmp,utrim) )
         itrim = NDG( 1 )
         invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(imap_cat,tmp,itrim) )

#  The polarisation vectors are calculated by the polpack:polvec command,
#  which requires the input Stokes vectors in the form of a 3D cube. Paste
#  the 2-dimensional Q, U and I images into a 3D cube.
         planes = NDG( [qtrim,utrim,itrim] )
         cube = NDG( 1 )
         invoke( "$KAPPA_DIR/paste in={0} shift=\[0,0,1\] out={1}".format(planes,cube))

#  The cube will have a 3D "POLANAL-SPECTRUM" WCS Frame, but POLVEC
#  requires a 2D POLANAL Frame. So use wcsframe to create the 2D Frame
#  from the 3D Frame, then delete the 3D Frame.
         invoke( "$KAPPA_DIR/wcsframe ndf={0} frame=POLANAL".format(cube) )
         invoke( "$KAPPA_DIR/wcsremove ndf={0} frames=POLANAL-SPECTRUM".format(cube) )

#  Re-instate SKY as the current Frame
         invoke( "$KAPPA_DIR/wcsframe ndf={0} frame=SKY".format(cube) )

#  POLPACK needs to know the order of I, Q and U in the 3D cube. Store
#  this information in the POLPACK enstension within "cube.sdf".
         invoke( "$POLPACK_DIR/polext in={0} stokes=qui".format(cube) )

#  If required, scale the I, Q and U values from pW to mJy/beam.
         if jy:
            tcube = NDG( 1 )
            invoke( "$KAPPA_DIR/cmult in={0} out={1} scalar={2}".
                    format(cube,tcube,1000*fcf) )
            invoke( "$KAPPA_DIR/setunits ndf={0} units=mJy/beam".format(tcube))
            cube = tcube

#  Create a FITS catalogue containing the polarisation vectors.
         if astmask is None and pcamask is None:
            tcat = outcat
         else:
            tcat = NDG.tempfile(".FIT")

         msg_out( "Creating the output catalogue: '{0}'...".format(outcat) )
         msg = invoke( "$POLPACK_DIR/polvec {0} cat={1} debias={2} debiastype={3} "
                       "radec=yes refupdate=no".format(cube,tcat,debias,debiastype) )
         msg_out( "\n{0}\n".format(msg) )

#  If we have an AST mask, add a column (called "AST") to the catalogue
#  containing a non-zero value for all the source vectors inside the AST mask.
         if astmask is not None:

            if pcamask is None:
               tcat2 = outcat
            else:
               tcat2 = NDG.tempfile(".FIT")

            try:
               invoke( "$POLPACK_DIR/poledit in={0} out={1} ndf={2} mode=addcol "
                       "col=AST maskcol=no units=\"' '\" comment=\"'Flags indicating AST mask'\"".
                       format(tcat,tcat2,astmask) )
            except starutil.NoNdfError:
               pass
         else:
            tcat2 = tcat

#  If we have a PCA mask, add a column (called "PCA") to the catalogue
#  containing a non-zero value for all the source vectors inside the PCA mask.
         if pcamask is not None:
            try:
               invoke( "$POLPACK_DIR/poledit in={0} out={1} ndf={2} mode=addcol "
                       "col=PCA maskcol=no units=\"' '\" comment=\"'Flags indicating PCA mask'\"".
                       format(tcat2,outcat,pcamask) )
            except starutil.NoNdfError:
               pass

#  -----------  TIDY UP ------------------------

#  Save the paths to any new single observation maps created above.
   if newmaps:
      with open(newmaps, "w") as fd:
         for path in new_maps:
            fd.write("{0}\n".format(path))

#  Remove temporary files.
   cleanup()

#  If an Exception of any kind occurred, display the message but hide the
#  python traceback. To see the trace back, uncomment "raise" instead.
except Exception as err:
#  raise
   msg_out( err, level=starutil.NOTSET )
   print( "See the end of the log file ({0}) for further details.".format(starutil.logfile) )
   cleanup()

# This is to trap control-C etc, so that we can clean up temp files.
except:
   cleanup()
   raise





