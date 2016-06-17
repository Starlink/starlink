#!/usr/bin/env python
'''
*+
*  Name:
*     pol2_ipdata

*  Purpose:
*     A collection of python functions to measure the POL2 IP signal.

*  Description:
*     This script creates an HDS container file holding values for the
*     parameters needed by the POL-2 "JK" (Johnstone-Kennedy) IP model
*     used by makemap. It seems to be incomplete as the container file
*     it generates has a different structure to that of the original JK
*     parameter file supplied by James (see smurf/data/ipdata.sdf), and
*     also many of the values it generates are clearly wrong (i.e. NaN,
*     or many orders of magnitude different to the existing parameter
*     values). However, it is archived here as a record of James' work.
*     The existing JK parameter file was created prior to the change
*     to the orientation of the fast axis of the HWP made on 20151111,
*     and so it may be worth while fixing this script so that a new JK
*     parameter file can be created and tested. For further information
*     see the  report "Pol-2 Instrumental Polarization" written by James
*     and included as an appendix in the POL-2 commissioning report.

*  Language:
*      python(2.7)

*  Copyright:
*     Does this still hold?   --- JK
*     Copyright (C) 2012-2013 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     Does this still hold?
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
*     JK: James Kennedy (University of Montreal, Canada)
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     01-OCT-2015 (JK):
*        Original version
*     17-JUN-2016 (DSB):
*        Added the Description section to the prologue. Added the RETAIN
*        parameter.

*-
'''

#import optparse
import numpy
import pylab
from scipy.optimize import minimize
import starutil
from starutil import invoke
from starutil import get_task_par
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out
from starlink.ndfpack import Ndf
import starlink.ndf as ndf
#  Assume for the moment that we will not be retaining temporary files.
retain = 0

def fit_ip_model(ip_data_Q,ip_data_U,var_Q,var_U,opacity_term,elevation,norm_source,pol_fixed,theta_ip,pol_screen=0.005,theta_offset=90.0,ip_dc_q=0.0,ip_dc_u=0.0):
    """

    takes Q and U data (from read_scans) and fits the ip_model.

    Invocation:

    Arguments:

    Returned Value:
    """

    starting_values = numpy.array([pol_screen,theta_offset,ip_dc_q,ip_dc_u])
    theta_ip = -53 # Change this to read the measured value.
    pol_fixed = 0.004 # Change this to read the fixed polarization.
    bnds = ((0.0,1.0),(0.0,180.0),(None,None),(None,None))
    ip_parameters = minimize(chisqfunc_ip_model, starting_values, args=(pol_fixed,theta_ip,opacity_term,elevation,norm_source,ip_data_Q,ip_data_U,var_Q,var_U),options={'disp':False},bounds=bnds)
    return ip_parameters

def fit_pol_cal_model(ip_data_Q,ip_data_U,var_Q,var_U,opacity_term,norm_source=20.0,theta_cal=-53,ip_dc_q=0.0,ip_dc_u=0.0):
    """

    takes Q and U data (from read_scans) and fits the ip_model.

    Invocation:

    Arguments:

    Returned Value:
    """

    starting_values = numpy.array([norm_source,theta_cal,ip_dc_q,ip_dc_u])
    bnds = ((0.0,None),(-90.0,90.0),(None,None),(None,None))
    pol_parameters = minimize(chisqfunc_pol_cal_model, starting_values, args=(opacity_term,ip_data_Q,ip_data_U,var_Q,var_U),options={'disp':False},bounds=bnds)
    return pol_parameters

def chisqfunc_ip_model((pol_screen,theta_offset,ip_dc_q,ip_dc_u),pol_fixed,theta_ip,opacity_term,elevation,norm_source,ip_data_Q,ip_data_U,var_Q,var_U):
    """

    chisqfunc_ip_model returns the combined chi^2 value of the ip_model to the ip Q and U data.

    Invocation:
        chisq_val = chisqfun_ip_model((pol_screen,theta_offset,ip_dc_q,ip_dc_u),pol_fixed,theta_ip,opacity_term,elevation,norm_source,ip_data_Q,ip_data_U,icm_Q,icm_U)

    Arguments:
        The first four arguments are to be included as one x = (pol_scree, theta_offset, ip_dc_q, ip_dc_u)
        pol_screen = float
                     The strength of the polarisation due to the windscreen (eg. 0.006).
        theta_offset = float
                       The angle offset between the fixed polarisation and the windscreen polarisation (in degrees).
        ip_dc_q = float
                  A DC offset to apply to the instrumental polarisation. (same units as norm_source)
        ip_dc_u = float
                  A DC offset to apply to the instrumental polarisation. (same units as norm_source)
        pol_fixed = float
                    The strength of the fixed polarisation component (eg. 0.004).
        pol_screen = float
                     The strength of the polarisation due to the windscreen (eg. 0.006).
        theta_ip = float
                   The angle of the fixed polarisation component (in degrees).
        opacity_term = numpy float array
                       The opacity signal (1 - exp(-tau*cscd(elevation)).
        elevation = numpy float array
                    The elevation of the observation (in degrees).
        norm_source = float
                      The normalisation of the source brightness (eg. The sky brightness at tau=0).
        ip_data_Q = numpy float array
                    An array of Q data
        ip_data_U = numpy float array
                    An array of U data
        var_Q = numpy array
                The variance on the Q data.
        var_U = numpy array
                The variance on the U data.

    Returned Value:
        The chisq value (float).
    """
    model_val_Q = ip_model(pol_fixed,pol_screen,theta_ip,theta_offset,opacity_term,elevation,norm_source,ip_dc_q,'Q')
    model_val_U = ip_model(pol_fixed,pol_screen,theta_ip,theta_offset,opacity_term,elevation,norm_source,ip_dc_u,'U')
    res_Q = ip_data_Q-model_val_Q
    res_U = ip_data_U-model_val_U
    if numpy.all(numpy.isnan(res_Q)) and numpy.all(numpy.isnan(res_U)):
        return numpy.nan
    else:
        chisq_Q = numpy.nansum(res_Q**2/var_Q)
        chisq_U = numpy.nansum(res_U**2/var_U)
        return numpy.asscalar(chisq_Q+chisq_U)

def chisqfunc_pol_cal_model((norm_source,theta_cal,ip_dc_Q,ip_dc_U),opacity_term,ip_data_Q,ip_data_U,var_Q,var_U):
    """

    chisqfunc_pol_cal_model returns ...

    Invocation:
        chisq_val = chisqfun_pol_cal_model()

    Arguments:
        The first four arguments are to be included as one x = (pol_scree, theta_offset, ip_dc_q, ip_dc_u)
        pol_screen = float
                     The strength of the polarisation due to the windscreen (eg. 0.006).
        theta_offset = float
                       The angle offset between the fixed polarisation and the windscreen polarisation (in degrees).
        ip_dc_q = float
                  A DC offset to apply to the instrumental polarisation. (same units as norm_source)
        ip_dc_u = float
                  A DC offset to apply to the instrumental polarisation. (same units as norm_source)
        pol_fixed = float
                    The strength of the fixed polarisation component (eg. 0.004).
        pol_screen = float
                     The strength of the polarisation due to the windscreen (eg. 0.006).
        theta_ip = float
                   The angle of the fixed polarisation component (in degrees).
        opacity_term = numpy float array
                       The opacity signal (1 - exp(-tau*cscd(elevation)).
        elevation = numpy float array
                    The elevation of the observation (in degrees).
        norm_source = float
                      The normalisation of the source brightness (eg. The sky brightness at tau=0).
        ip_data_Q = numpy float array
                    An array of Q data
        ip_data_U = numpy float array
                    An array of U data
        var_Q = numpy array
                The variance on the Q data.
        var_U = numpy array
                The variance on the U data.

    Returned Value:
        The chisq value (float).
    """
    model_val_Q = pol_cal(theta_cal,opacity_term,norm_source,ip_dc_Q,'Q')
    model_val_U = pol_cal(theta_cal,opacity_term,norm_source,ip_dc_U,'U')
    res_Q = ip_data_Q-model_val_Q
    res_U = ip_data_U-model_val_U
    if numpy.all(numpy.isnan(res_Q)) and numpy.all(numpy.isnan(res_U)):
        return numpy.nan
    else:
        chisq_Q = numpy.nansum(res_Q**2/var_Q)
        chisq_U = numpy.nansum(res_U**2/var_U)
        return numpy.asscalar(chisq_Q+chisq_U)

def ip_model(pol_fixed,pol_screen,theta_ip,theta_offset,opacity_term,elevation,norm_source,ip_dc,stokes_parameter):
    """

    ip_model returns the instrumental polarisation model for POL2.  It contains 3 components: a fixed polarisation,
    an elevation dependend polarisation due to signal absorption by the wind screen, and an elevation dependent emission
    polarisation from the wind screen (90 degrees out of phase with the source absorption).

    Invocation:
        result = ip_model(pol_fixed,pol_screen,theta_ip,theta_offset,opacity_term,elevation,norm_source,ip_dc,stokes_parameter)

    Arguments:
        pol_fixed = float
                    The strength of the fixed polarisation component (eg. 0.004).
        pol_screen = float
                     The strength of the polarisation due to the windscreen (eg. 0.006).
        theta_ip = float
                   The angle of the fixed polarisation component (in degrees).
        theta_offset = float
                       The angle offset between the fixed polarisation and the windscreen polarisation (in degrees).
        opacity_term = float
                       The opacity signal (1 - exp(-tau*cscd(elevation)).
        elevation = float
                    The elevation of the observation (in degrees).
        norm_source = float
                      The normalisation of the source brightness (eg. The sky brightness at tau=0).
        ip_dc = float
                A DC offset to apply to the instrumental polarisation. (same units as norm_source)
        stokes_parameter = string
                           The stokes parameter being considered ('Q' or 'U').

    Returned Value:
        A float indicating ip model (same units as the norm_source term).

    """

    if stokes_parameter.upper() == 'Q':
        fixed_term = numpy.cos(numpy.radians(2.0*theta_ip))
        elevation_absorption_term = numpy.cos(numpy.radians(2.0*(theta_ip+elevation+theta_offset)))
        elevation_emission_term = numpy.cos(numpy.radians(2.0*(theta_ip+elevation+theta_offset+90.0)))
    elif stokes_parameter.upper() == 'U':
        fixed_term = numpy.sin(numpy.radians(2.0*theta_ip))
        elevation_absorption_term = numpy.sin(numpy.radians(2.0*(theta_ip+elevation+theta_offset)))
        elevation_emission_term = numpy.sin(numpy.radians(2.0*(theta_ip+elevation+theta_offset+90.0)))

    ip_fixed = pol_fixed*norm_source*opacity_term*fixed_term
    ip_absorbed = pol_screen*norm_source*opacity_term*elevation_absorption_term
    ip_emission = pol_screen*norm_source*elevation_emission_term

    return ip_fixed+ip_absorbed+ip_emission+ip_dc

def pol_cal(theta_cal,opacity_term,norm_source,ip_dc,stokes_parameter):
    """

    pol_cal returns the polarisation model for an unpolarised sky with the calibrator in place.

    Invocation:
        result = pol_cal(theta_cal,opacity_term,elevation,norm_source,ip_dc,stokes_parameter)

    Arguments:
        theta_cal = float
                   The angle of the calibrator grid (in degrees).
        opacity_term = float
                       The opacity signal (1 - exp(-tau*cscd(elevation)).
        norm_source = float
                      The normalisation of the source brightness (eg. The sky brightness at tau=0).
        ip_dc = A dc offset in units of norm_source
        stokes_parameter = string
                           The stokes parameter being considered ('Q' or 'U').

    Returned Value:
        A float indicating polarised signal (same units as the norm_source term).

    """

    if stokes_parameter.upper() == 'Q':
        fixed_term = numpy.cos(numpy.radians(2.0*theta_cal))
    elif stokes_parameter.upper() == 'U':
        fixed_term = numpy.sin(numpy.radians(2.0*theta_cal))

    pol_signal = norm_source*opacity_term*fixed_term*0.5+ip_dc

    return pol_signal

def ip_model_dome(ip_data_Q,ip_data_U,norm_source):
    """

    takes Q and U data (from read_scans) and fits the polarization model expected when the dome is closed (wind screen emission/absorption cancel).

    Invocation:

    Arguments:

    Returned Value:
    """

    theta_temp = numpy.rad2deg(0.5*numpy.arctan2(ip_data_U,ip_data_Q))
    theta_ip = numpy.nanmean(theta_temp)
    Pol_screen_temp = ip_data_Q/norm_source/numpy.cos(numpy.radians(2.0*theta_temp))
    Pol_screen = numpy.nanmean(Pol_screen_temp[:])
    return (Pol_screen,theta_ip)

def run_calcqu(input_data,config,harmonic):
    #  The following call to SMURF:CALCQU creates two HDS container files -
    #  one holding a set of Q NDFs and the other holding a set of U NDFs. Create
    #  these container files in the NDG temporary directory.
    qcont = NDG(1)
    qcont.comment = "qcont"
    ucont = NDG(1)
    ucont.comment = "ucont"

    msg_out( "Calculating Q and U values for each bolometer...")
    invoke("$SMURF_DIR/calcqu in={0} config=\"{1}\" lsqfit=no outq={2} outu={3} "
           "harmonic={4} fix".format(input_data,starutil.shell_quote(config),
                                     qcont,ucont,harmonic) )
    return (qcont,ucont)

def get_filtered_skydip_data(qarray,uarray,clip,a):
    """

    This function takes q and u array data (output from calcqu), applies ffclean to remove spikes
    and puts in numpy array variable
    It borrows (copies) heavily from pol2cat.py (2015A)

    Invocation:
        ( qdata_total,qvar_total,udata_total,uvar_total,elevation,opacity_term,bad_pixel_ref ) = ...
            get_filtered_skydip_data(qarray,uarray,clip,a)

    Arguments:
        qarray = An NDF of Q array data (output from calcqu).
        uarray = An NDF of U array data (output form calcqu).
        clip = The sigma cut for ffclean.
           a = A string indicating the array (eg. 'S8A').

    Returned Value:
        qdata_total = A numpy array with the cleaned qarray data.
        qvar_total = A numpy array with the qarray variance data.
        udata_total = A numpy array with the cleaned uarray data.
        uvar_total = A numpy array with the uarray variance data.
        elevation = A numpy array with the elevation data
        opacity_term = A numpy array with the opacity brightness term (1-exp(-tau*air_mass))
            Here tau is calculated using the WVM data as input.

    """

    #  Remove spikes from the Q images for the current subarray. The cleaned NDFs
    #  are written to temporary NDFs specified by the new NDG object "qff", which
    #  inherit its size from the existing group "qarray"".
    msg_out( "Removing spikes from {0} bolometer Q values...".format(a))
    qff = NDG(qarray)
    qff.comment = "qff"
    invoke( "$KAPPA_DIR/ffclean in={0} out={1} genvar=yes box=3 clip=\[{2}\]".format(qarray,qff,clip) )

    #  Remove spikes from the U images for the current subarray. The cleaned NDFs
    #  are written to temporary NDFs specified by the new NDG object "uff", which
    #  inherit its size from the existing group "uarray"".
    msg_out( "Removing spikes from {0} bolometer U values...".format(a))
    uff = NDG(uarray)
    uff.comment = "uff"
    invoke( "$KAPPA_DIR/ffclean in={0} out={1} genvar=yes box=3 clip=\[{2}\]"
            .format(uarray,uff,clip) )

    elevation = []
    opacity_term = []
    for stare in range(len(qff[:])):
    # Stack Q data in numpy array
        # Get elevation information
        elevation.append(numpy.array( float( invoke( "$KAPPA_DIR/fitsmod ndf={0} edit=print keyword=ELSTART".format( qff[ stare ] ) ) ) ) )
        # Get Tau (Opacity) information
        tau_temp = numpy.array( float( invoke( "$KAPPA_DIR/fitsmod ndf={0} edit=print keyword=WVMTAUST".format( qff[ stare ] ) ) ) )
        # Convert to obs band.
        if '4' in a:
             tau_temp = 19.04*(tau_temp-0.018) # Eq from Dempsey et al
        elif '8' in a:
             tau_temp = 5.36*(tau_temp-0.006) # Eq from Dempsey et al.
        opacity_term.append(1-numpy.exp(-1*tau_temp/numpy.sin(numpy.radians(elevation[-1]))))
        invoke( "$KAPPA_DIR/ndftrace {0} quiet".format(qff[ stare ]))
        nx = get_task_par( "dims(1)", "ndftrace" )
        ny = get_task_par( "dims(2)", "ndftrace" )
        qdata_temp = numpy.reshape( Ndf( qff[ stare ] ).data, (ny,nx))
        qdata_temp[numpy.abs(qdata_temp)>1e300] = numpy.nan;
        if stare == 0:
            qdata_total = qdata_temp
        else:
            qdata_total = numpy.dstack((qdata_total,qdata_temp))
        qvar_temp = numpy.reshape( Ndf( qff[ stare ] ).var, (ny,nx))
        qdata_temp[numpy.abs(qvar_temp)>1e300] = numpy.nan;
        if stare == 0:
            qvar_total = qvar_temp
        else:
            qvar_total = numpy.dstack((qvar_total,qvar_temp))
        # Stack U data in numpy array
        invoke( "$KAPPA_DIR/ndftrace {0} quiet".format(uff[ stare ]))
        nx = get_task_par( "dims(1)", "ndftrace" )
        ny = get_task_par( "dims(2)", "ndftrace" )
        udata_temp = numpy.reshape( Ndf( uff[ stare ] ).data, (ny,nx))
        udata_temp[numpy.abs(udata_temp)>1e300] = numpy.nan;
        if stare == 0:
            udata_total = udata_temp
        else:
            udata_total = numpy.dstack((udata_total,udata_temp))
        uvar_temp = numpy.reshape( Ndf( uff[ stare ] ).var, (ny,nx))
        udata_temp[numpy.abs(uvar_temp)>1e300] = numpy.nan;
        if stare == 0:
            uvar_total = uvar_temp
        else:
            uvar_total = numpy.dstack((uvar_total,uvar_temp))

    # Create bad pixel reference.
    bad_pixel_ref = NDG(1)
    invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2}".format(qff,uff,bad_pixel_ref))
    return( qdata_total,qvar_total,udata_total,uvar_total,elevation,opacity_term,bad_pixel_ref )

def get_avg_value(data,threshold=0.5):
    """

    This function finds the average value in data which may contain non-Gaussian components.
    Rather than take the mean and standard deviation of the original data, get_avg_value tries
    to get an improved mean and standard deviation estimate by creating a histogram and
    iteratively selecting the data within 3-sigma.

    Invocation:
        result = get_avg_value(data,threshold=0.5)

    Arguements:
        shape = The a list of floats or numpy.arrays.
        threshold = A limit of data to cut.

    Returned Value:
        A mean and standard deviation estimate of the data (excluding non-Gaussian components).
    """
    data = numpy.array(data)

    #########################
    # Take peak of historgram
    sval = numpy.shape(data)
    new_size = 1
    for iteration in range(len(sval)):
        new_size = new_size*sval[iteration]
    data = numpy.reshape(data,new_size)
    # remove any nan values
    data = data[~numpy.isnan(data)]
    orig_data_size = float(len(data))
    ################################
    # Iteratively remove outliers
    for interval in range(3):
        amps,bins,patches = pylab.hist(data,bins=30)
        maxbin = numpy.where(amps==numpy.max(amps))
        max_bin_val = numpy.asscalar(bins[maxbin[0]])
        indx_keep = abs(data-max_bin_val) < 3.0*numpy.nanstd(data)
        if sum(indx_keep)/orig_data_size < threshold:
            break
        else:
            data = data[indx_keep]
    return( numpy.mean(data),numpy.std(data)/numpy.sqrt(sum(indx_keep)) )

def write_ip_NDF(data,bad_pixel_ref):
    """

    This function writes out the array ip parameter data to an ndf_file.

    Invocation:
        result = write_ip_NDF(data,bad_pixel_ref)

    Arguements:
        data = The array ip parameter data
        bad_ref = A NDF with bad pixel values to copy over.

    Returned Value:
        Writes NDF and returns handle.
    """

    ndf_name_orig = NDG(1)
    indf = ndf.open( ndf_name_orig[0], 'WRITE', 'NEW' )
    indf.new('_DOUBLE', 2, numpy.array([1,1]),numpy.array([32,40]))
    ndfmap = indf.map( 'DATA', '_DOUBLE', 'WRITE' )
    ndfmap.numpytondf( data )
    indf.annul()

    # Copy bad pixels
    ndf_name = NDG(1)
    invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2}".format(ndf_name_orig,bad_pixel_ref,ndf_name) )
    return ndf_name

def nans(shape, dtype=float):
    """

    A small util function to creat a numpy array of a given shape

    Invocation:
        result = nans(shape, dtype=float)

    Arguements:
        shape = The shape of the array to be created (eg. [3,2] is a 3x2 matrix)

    Returned Value:
        A numpy array filled with NANs
    """
    a = numpy.empty(shape, dtype)
    a.fill(numpy.nan)
    return a

#  A function to clean up before exiting. Delete all temporary NDFs etc,
#  unless the script's RETAIN parameter indicates that they are to be
#  retained. Also delete the script's temporary ADAM directory.
def cleanup():
   global retain
   ParSys.cleanup()
   if retain:
      msg_out( "Retaining temporary files in {0}".format(NDG.tempdir))
   else:
      NDG.cleanup()

#  Catch any exception so that we can always clean up, even if control-C
#  is pressed.
for zz in range(1):
    params = []
    params.append(starutil.ParNDG("IN", "The input POL2 time series NDFs (with the dome open and calibrator out)",
                                  starutil.get_task_par("DATA_ARRAY","GLOBAL",
                                                        default=Parameter.UNSET)))
    params.append(starutil.Par0S("OUT", "The output .sdf file containing the IPT parameters",
                                 "ipdata"))
    params.append(starutil.ParNDG("DomeClosedCalIn", "The input POL2 time series NDFs (with the dome closed and calibrator in)",
                                  starutil.get_task_par("DATA_ARRAY","GLOBAL",
                                                        default=Parameter.UNSET)))
    params.append(starutil.ParNDG("DomeClosedCalOut", "The input POL2 time series NDFs (with the dome closed and calibrator out)",
                                  starutil.get_task_par("DATA_ARRAY","GLOBAL",
                                                        default=Parameter.UNSET)))
    params.append(starutil.ParNDG("DomeOpenCalIn", "The input POL2 time series NDFs (with the dome open and the calibrator in)",
                                  starutil.get_task_par("DATA_ARRAY","GLOBAL",
                                                        default=Parameter.UNSET)))
    params.append(starutil.Par0F("NSIGMA", "No. of standard deviations at "
                                 "which to clip spikes", 3.0, noprompt=True))
    params.append(starutil.Par0S("CONFIG", "The cleaning config",
                                 "^$STARLINK_DIR/share/smurf/dimmconfig.lis",
                                  noprompt=True))
    params.append(starutil.Par0I("HARMONIC", "The harmonic for calculating "
                                 "Q and U", 4, minval=1, noprompt=True))

    params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

    #  Initialise the parameters to hold any values supplied on the command
    #  line.
    parsys = ParSys( params )
    indata = parsys["IN"].value
    dome_closed_cal_in = parsys["DomeClosedCalIn"].value
    dome_closed_cal_out = parsys["DomeClosedCalOut"].value
    dome_open_cal_in = parsys["DomeOpenCalIn"].value
    outdata = parsys["OUT"].value
    # Remove .sdf extension if given in outdata
    if len(outdata) > 3:
        if str.lower(outdata[-4:]) == '.sdf':
            outdata = outdata[0:-4]

    #  Now get the harmonic to use
    harmonic = parsys["HARMONIC"].value
    #  Get the clipping limit and create a string to use for FFCLEAN CLIP
    nsigma = parsys["NSIGMA"].value
    clip = "{0},{0},{0}".format(nsigma)

    #  Get the cleaning config.
    config = parsys["CONFIG"].value

    #  See if temp files are to be retained.
    retain = parsys["RETAIN"].value

    #  Initalize ip parameter variables
    ip_prms = {}

# First process the dome-closed, calibrator-in data to get the dome brightness.
# Then process the dome-closed, calibrator-out data to get the the fixed_polarization and theta_ip.
# The process the dome-open, calibrator-in to get the sky brightness.

##############################################
# Dome closed, calibrator in
    dome_brightness_keep =[]
    theta_cal_keep = []
    (qcont,ucont) = run_calcqu(dome_closed_cal_in,config,harmonic)
    for a in ('S4A','S4B','S4C','S4D','S8A','S8B','S8C','S8D'):
        print('Running on array '+a)
        #  Get NDG object that contains the Q and U maps for the current subarray.
        qarray = qcont.filter(a)
        uarray = ucont.filter(a)
        #  If any data was found for the current subarray...
        if qarray != None and uarray != None:
            ( qdata_total,qvar_total,udata_total,uvar_total,elevation,opacity_term,bad_pixel_ref ) = get_filtered_skydip_data(qarray,uarray,clip,a)
            rows = len(qdata_total[:,1,1])
            cols = len(qdata_total[1,:,1])
            # Preallocate data
            dome_brightness_temp = nans((rows,cols))
            theta_cal_temp = nans((rows,cols))
            opacity_term = numpy.array(opacity_term)
            elevation = numpy.array(elevation)
            # Starting values for pol_cal
            starting_values = numpy.array([14,-53,0,0]) # Dome brightness, theta_cal
            for row_val in range(rows):
                for col_val in range(cols):
                    ip_data_Q = qdata_total[row_val,col_val,:]
                    ip_data_U = udata_total[row_val,col_val,:]
                    (dome_brightness_temp[row_val,col_val], theta_cal_temp[row_val,col_val]) = ip_model_dome(ip_data_Q,ip_data_U,0.5)
            dome_brightness_keep.append(dome_brightness_temp)
            theta_cal_keep.append(theta_cal_temp)
        else:
            print('No data for array '+a)

    (dome_brightness,dome_brightness_err) = get_avg_value(dome_brightness_keep)
    (theta_cal,theta_cal_err) = get_avg_value(theta_cal_temp)
    print('Dome brightness is: '+str(dome_brightness)+' +/- '+str(dome_brightness_err)+' pW')
    print('Calibrator angle is: '+str(theta_cal)+' +/- '+str(theta_cal_err)+' deg');

##############################################
# Dome closed, calibrator out
    ipprm_keep = []
    theta_ip_keep = []
    (qcont,ucont) = run_calcqu(dome_closed_cal_out,config,harmonic)
    for a in ('S4A','S4B','S4C','S4D','S8A','S8B','S8C','S8D'):
        print('Running on array '+a)
        #  Get NDG object that contains the Q and U maps for the current subarray.
        qarray = qcont.filter(a)
        uarray = ucont.filter(a)
        #  If any data was found for the current subarray...
        if qarray != None and uarray != None:
            ( qdata_total,qvar_total,udata_total,uvar_total,elevation,opacity_term,bad_pixel_ref ) = get_filtered_skydip_data(qarray,uarray,clip,a)
            rows = len(qdata_total[:,1,1])
            cols = len(qdata_total[1,:,1])
            # Preallocate data
            ipprm_pf = nans((rows,cols))
            theta_ip = nans((rows,cols))
            opacity_term = numpy.array(opacity_term)
            elevation = numpy.array(elevation)
            # Starting values for pol_cal
            starting_values = numpy.array([14,-53,0,0]) # Dome brightness, theta_cal
            for row_val in range(rows):
                for col_val in range(cols):
                    ip_data_Q = qdata_total[row_val,col_val,:]
                    ip_data_U = udata_total[row_val,col_val,:]
                    (ipprm_pf[row_val,col_val], theta_ip[row_val,col_val]) = ip_model_dome(ip_data_Q,ip_data_U,dome_brightness)
            ip_prms['Pf_'+a[-1]] = ipprm_pf
            ip_prms['Theta_ip_'+a[-1]] = theta_ip
            ipprm_keep.append(ipprm_pf)
            theta_ip_keep.append(theta_ip)
        else:
            print('No data for array '+a)

    # Iteratively remove outliers and take the mean
    (avg_pf,pf_err) = get_avg_value(ipprm_keep)
    (avg_theta_ip,theta_ip_err) = get_avg_value(theta_ip_keep)
    print('Avg fixed polarization is: '+str(avg_pf*100)+' +/- '+str(pf_err*100)+' %')
    print('Avg theta_ip angle is: '+str(avg_theta_ip)+' +/- '+str(theta_ip_err)+' deg');

##############################################
# Dome open, calibrator in
    sky_brightness_keep = []
    theta_cal_temp = []
    (qcont,ucont) = run_calcqu(dome_open_cal_in,config,harmonic)
    for a in ('S4A','S4B','S4C','S4D','S8A','S8B','S8C','S8D'):
        print('Running on array '+a)
        #  Get NDG object that contains the Q and U maps for the current subarray.
        qarray = qcont.filter(a)
        uarray = ucont.filter(a)
        #  If any data was found for the current subarray...
        if qarray != None and uarray != None:
            ( qdata_total,qvar_total,udata_total,uvar_total,elevation,opacity_term,bad_pixel_ref ) = get_filtered_skydip_data(qarray,uarray,clip,a)
            # When dome is closed opacity_term = 1
            rows = len(qdata_total[:,1,1])
            cols = len(qdata_total[1,:,1])
            # Preallocate data
            sky_brightness_temp = nans((rows,cols))
            theta_cal_temp = nans((rows,cols))
            dc_Q = nans((rows,cols))
            dc_U = nans((rows,cols))
            opacity_term = numpy.array(opacity_term)
            #elevation = numpy.array(elevation)
            # Starting values for pol_cal
            for row_val in range(rows):
                for col_val in range(cols):
                    ip_data_Q = qdata_total[row_val,col_val,:]
                    ip_data_U = udata_total[row_val,col_val,:]
                    var_Q = qvar_total[row_val,col_val,:]
                    var_U = uvar_total[row_val,col_val,:]
                    ipprms = fit_pol_cal_model(ip_data_Q,ip_data_U,var_Q,var_U,opacity_term)
                    if ipprms.success is True:
                        #returnCode[row_val,col_val] = True
                        sky_brightness_temp[row_val,col_val] = ipprms.x[0]
                        theta_cal_temp[row_val,col_val] = ipprms.x[1]
                        dc_Q[row_val,col_val] = ipprms.x[2]
                        dc_U[row_val,col_val] = ipprms.x[3]
                        #chi2Vals[row_val,col_val] = ipprms.fun
                    #else:
                        #returnCode[row_val,col_val] = False
            sky_brightness_keep.append(sky_brightness_temp)
            theta_cal_keep.append(theta_cal_temp)
        else:
            print('No data for array '+a)

    (sky_brightness,sky_brightness_err) = get_avg_value(sky_brightness_keep)
    (theta_cal,theta_cal_err) = get_avg_value(theta_cal_temp)
    print('Sky brightness is: '+str(sky_brightness)+' +/- '+str(sky_brightness_err)+' pW')
    print('Calibrator angle is: '+str(theta_cal)+' +/- '+str(theta_cal_err)+' deg');


##############################################
# Dome open, calibrator out
    (qcont,ucont) = run_calcqu(indata,config,harmonic)
    for a in ('S4A','S4B','S4C','S4D','S8A','S8B','S8C','S8D'):
        print('Running on array '+a)
        #  Get NDG object that contains the Q and U maps for the current subarray.
        qarray = qcont.filter(a)
        uarray = ucont.filter(a)
        #  If any data was found for the current subarray...
        if qarray != None and uarray != None:
            ( qdata_total,qvar_total,udata_total,uvar_total,elevation,opacity_term,bad_pixel_ref ) = get_filtered_skydip_data(qcont,ucont,clip,a)
            rows = len(qdata_total[:,1,1])
            cols = len(qdata_total[1,:,1])
            # Preallocate data
            ipprms_pol_screen = nans((rows,cols))
            ipprms_Co = nans((rows,cols))
            ipprms_dc_Q = nans((rows,cols))
            ipprms_dc_U = nans((rows,cols))
            returnCode = nans((rows,cols))
            chi2Vals = nans((rows,cols))

            opacity_term = numpy.array(opacity_term)
            elevation = numpy.array(elevation)
            for row_val in range(rows):
                for col_val in range(cols):
                    ip_data_Q = qdata_total[row_val,col_val,:]
                    ip_data_U = udata_total[row_val,col_val,:]
                    var_Q = qvar_total[row_val,col_val,:]
                    var_U = uvar_total[row_val,col_val,:]
                    pol_fixed = ip_prms['Pf_'+a[-1]][row_val,col_val]
                    theta_ip = ip_prms['Theta_ip_'+a[-1]][row_val,col_val]
                    ipprms = fit_ip_model(ip_data_Q,ip_data_U,var_Q,var_U,opacity_term,elevation,20.0,pol_fixed,theta_ip)
                    if ipprms.success is True:
                        returnCode[row_val,col_val] = True
                        ipprms_pol_screen[row_val,col_val] = ipprms.x[0]
                        ipprms_Co[row_val,col_val] = ipprms.x[1]
                        ipprms_dc_Q[row_val,col_val] = ipprms.x[2]
                        ipprms_dc_U[row_val,col_val] = ipprms.x[3]
                        chi2Vals[row_val,col_val] = ipprms.fun
                    else:
                        returnCode[row_val,col_val] = False

            # Write NDFs.
            out_p0 = write_ip_NDF(ip_prms['Pf_'+a[-1]],bad_pixel_ref)
            out_p1 = write_ip_NDF(ipprms_pol_screen,bad_pixel_ref)
            out_c0 = write_ip_NDF(ipprms_Co,bad_pixel_ref)
            out_angc = write_ip_NDF(ip_prms['Theta_ip_'+a[-1]],bad_pixel_ref)

            # Fill any bad pixels with smooth function to match surrounding pixels
            msg_out( "Filling in bad pixel values for {0} bolometer IP parameters...".format(a))
            out_p0_filled = NDG(1)
            invoke( "$KAPPA_DIR/fillbad in={0} out={1} variance=true niter=10 size=15".format(out_p0,out_p0_filled) )
            out_p1_filled = NDG(1)
            invoke( "$KAPPA_DIR/fillbad in={0} out={1} variance=true niter=10 size=15".format(out_p1,out_p1_filled) )
            out_c0_filled = NDG(1)
            invoke( "$KAPPA_DIR/fillbad in={0} out={1} variance=true niter=10 size=15".format(out_c0,out_c0_filled) )
            out_angc_filled = NDG(1)
            invoke( "$KAPPA_DIR/fillbad in={0} out={1} variance=true niter=10 size=15".format(out_angc,out_angc_filled) )

            # Copy individual NDFs to single output.
            invoke( "$KAPPA_DIR/ndfcopy {0} {1}".format(out_p0,outdata+'_preclean.'+str.lower(a)+'p0'))
            invoke( "$KAPPA_DIR/ndfcopy {0} {1}".format(out_p1,outdata+'_preclean.'+str.lower(a)+'p1'))
            invoke( "$KAPPA_DIR/ndfcopy {0} {1}".format(out_c0,outdata+'_preclean.'+str.lower(a)+'c0'))
            invoke( "$KAPPA_DIR/ndfcopy {0} {1}".format(out_angc,outdata+'_preclean.'+str.lower(a)+'angc'))

            invoke( "$KAPPA_DIR/ndfcopy {0} {1}".format(out_p0_filled,outdata+'.'+str.lower(a)+'p0'))
            invoke( "$KAPPA_DIR/ndfcopy {0} {1}".format(out_p1_filled,outdata+'.'+str.lower(a)+'p1'))
            invoke( "$KAPPA_DIR/ndfcopy {0} {1}".format(out_c0_filled,outdata+'.'+str.lower(a)+'c0'))
            invoke( "$KAPPA_DIR/ndfcopy {0} {1}".format(out_angc_filled,outdata+'.'+str.lower(a)+'angc'))
        else:
            print('No data for array '+a)

    #  Remove temporary files.
    cleanup()
