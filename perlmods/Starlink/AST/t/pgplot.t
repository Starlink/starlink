#!perl

use strict;
use Test::More tests => 12;

require_ok("Starlink::AST");

Starlink::AST::Begin();

my @cards;
my $fchan = new Starlink::AST::FitsChan( );
while (<DATA>) {
  $fchan->PutFits($_ ,0);
}
$fchan->Clear( "Card" );

# Get FrameSet
my $wcsinfo = $fchan->Read();
isa_ok( $wcsinfo, "AstFrameSetPtr" );

PGPLOT::pgbegin(0,"/xserve",1,1);
my $plot = Starlink::AST::Plot->new( $wcsinfo, 
                               [0,0,1,1],[0,0,10000,10000], "Grid=1");
$plot->Grid();

# Done!
exit;

__DATA__
SSIMPLE  =                    T / file does conform to FITS standard             
BITPIX  =                  -32 / number of bits per data pixel                  
NAXIS   =                    2 / number of data axes                            
NAXIS1  =                  624 / length of data axis 1                          
NAXIS2  =                  625 / length of data axis 2                          
EXTEND  =                    T / FITS dataset may contain extensions            
COMMENT   FITS (Flexible Image Transport System) format is defined in 'Astronomy
COMMENT   and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H 
LBOUND1 =                  513 / Pixel origin along axis 1                      
LBOUND2 =                 -110 / Pixel origin along axis 2                      
DATE    = '2004-02-23T00:40:47'/ file creation date (YYYY-MM-DDThh:mm:ss UT)    
ORIGIN  = 'UFTI data Acquisition'/ Organization creating the file               
BSCALE  =                  1.0 / True_value = BSCALE * FITS_value + BZERO       
BZERO   =                  0.0 / True_value = BSCALE * FITS_value + BZERO       
HDUCLAS1= 'NDF     '           / Starlink NDF (hierarchical n-dim format)       
HDUCLAS2= 'DATA    '           / Array component subclass                       
                                                                                
                                 Identification:                                
TELESCOP= 'UKIRT             ' / Telescope name                                 
INSTRUME= 'UFTI    '           / Instrument                                     
DHSVER  = 'UKDHS 1999 June 18' / Data handling version                          
OBSERVER= 'nobody  '           / Observers present                              
USERID  = 'nobody  '           / Userid logged in as                            
OBSREF  = '00      '           / PATT or other reference                        
PROJECT = 'U/04A/H37B'         / Time-allocation code                           
MSBID   = 'adfebd99c553526eea6056f18e7160a4'/ Id min.-schedulable block         
                                                                                
                                 eStar headers:                                 
RMTAGENT= 'none    '           / Name of remote agent                           
AGENTID = 'none    '           / Unique identifier for remote agent             
                                                                                
OBJECT  = 'Bo      '           / Object name from telescope                     
                                                                                
                                 Data Reduction:                                
RECIPE  = 'JITTER_SELF_FLAT'   / Data reduction recipe to be used               
OBSNUM  =                  115 / Observation number                             
GRPNUM  =                  114 / The number of the group applied to all members 
GRPMEM  =                    T / Group membership                               
STANDARD=                    F / Is the target a standard star observation      
NOFFSETS=                   10 / Number of offset positions in a pattern        
OBSTYPE = 'OBJECT  '           / Type of observation                            
                                                                                
                                 Astrometric and time information:              
DATE-OBS= '2004-02-14T06:39:48.000'/ Date of observation                        
DATE-END= '2004-02-14T06:39:57Z'/ Date and time (UTC) of end   of observation   
CTYPE1  = 'RA---TAN'           / Equatorial tangent-plane projection            
CRPIX1  =                257.0 / Pixel at reference point along axis 1          
CRVAL1  =             86.55475 / [deg] RA (5:46:13.14) at reference point       
CDELT1  =         -2.523611E-5 / [deg] Increment per pixel at reference point   
CTYPE2  = 'DEC--TAN'           / Equatorial tangent-plane projection            
CRPIX2  =                368.0 / Reference pixel on axis 2                      
CRVAL2  =           -0.1013333 / [deg] Dec (-0:6: 4.8) at reference point       
CDELT2  =          2.523611E-5 / [deg] Increment per pixel at reference point   
CROTA2  =                0.827 / [deg] Angle DEC axis wrt axis 2 ccw            
EPOCH   =               2000.0 / Epoch of reference equinox                     
RABASE  =            5.7703167 / [h] RA base position                           
DECBASE =           -0.1013333 / [deg] DEC base position                        
TRAOFF  =                  5.0 / [arcsec] RA telescope offset                   
TDECOFF =                  5.0 / [arcsec] DEC telescope offset                  
AMSTART =                1.064 / Airmass at start of observation                
AMEND   =                1.064 / Airmass at end of observation                  
                                                                                
                                 Instrument aperture:                           
APER_X  =                19.89 / [arcsec] Aperture X axis coordinate            
APER_Y  =               -23.16 / [arcsec] Aperture Y axis coordinate            
                                                                                
UTSTART =                6.672 / [h] Start time of observation                  
UTEND   =                6.699 / [h] End time of observation                    
                                                                                
                                 Environment:                                   
AIRTEMP =               -0.013 / [degC] Air temperature                         
BARPRESS=              614.482 / Ambient pressure                               
DEWPOINT=              -11.076 / [degC] Dewpoint                                
DOMETEMP=                0.578 / [degC] Dome temperature                        
HUMIDITY=               43.285 / Relative Humidity                              
MIRR_NE =                 0.01 / [degC] Mirror temperature NE                   
MIRR_NW =                0.802 / [degC] Mirror temperature NW                   
MIRR_SE =                 0.01 / [degC] Mirror temperature SE                   
MIRR_SW =                0.203 / [degC] Mirror temperature SW                   
MIRRBTNW=                2.754 / [degC] Mirror bottom temp. SW                  
MIRRTPNW=                1.734 / [degC] Mirror top temp. SW                     
SECONDAR=               -0.277 / [degC] Temperature of secondary                
TOPAIRNW=                1.042 / [degC] Top air NW                              
TRUSSENE=                1.887 / [degC] Truss leg ENE                           
TRUSSWSW=                0.089 / [degC] Truss leg WSW                           
WIND_DIR=              287.931 / [deg] Wind direction, azimuth                  
WIND_SPD=               53.689 / [km/s] Wind speed                              
CSOTAU  =                0.137 / Tau at 225 GHz from CSO                        
TAUDATE = '2004-02-14T06:32'   / Time and date of Tau reading                   
TAUSRC  = 'CSO225GHZ'          / Source of opacity data                         
                                                                                
                                                                                
                                 Instrument configuration:                      
EXP_TIME=                  3.0 / [s] Integration time per exposure              
INT_TIME=                  6.0 / [s] Total exposure time for integration        
NEXP    =                    2 / Number of exposures in integration             
MODE    = 'NDSTARE '           / Readout mode - stare | NDstare                 
SPD_GAIN= 'Normal  '           / Readout speed (normal | fast) or high gain     
GAIN    =                5.454 / [e/ADU] Detector gain                          
FILTER  = '2.27    '           / Combined filter name                           
FILTER1 = '2.27    '           / Filter in wheel 1                              
FILTER2 = 'Open    '           / Filter in wheel 2                              
WPLANGLE=                  0.0 / [deg] IRPOL waveplate angle                    
FP_X    =                    0 / Fabry Perot X                                  
FP_Y    =                    0 / Fabry Perot Y                                  
FP_Z    =                    0 / Fabry Perot Z                                  
DETECTOR= 'Hawaii 1'           / Detector array used                            
DROWS   =                  512 / [pixel] Number of detectors in a row           
DCOLUMNS=                  512 / [pixel] Number of detectors in a column        
RDOUT_X1=                  513 / Start column of array readout                  
RDOUT_X2=                 1024 / End   column of array readout                  
RDOUT_Y1=                    1 / Start row    of array readout                  
RDOUT_Y2=                  512 / End   row    of array readout                  
                                                                                
                                 Engineering:                                   
USPPIXEL=                3.666 / [us/pixel] Read rate                           
BLACKLEV=                 1000 / [adu] Blacklevel                               
DCS_GAIN=                   12 / integral gain value                            
DCSSPEED=                   21 / DCS integration time                           
                                                                                
V1      =                -10.9 / [V] -15 V supply                               
V2      =                 -9.4 / [V] -9 V supply                                
V3      =                 -5.2 / [V] -5 V supply                                
V4      =                 23.4 / [V] +30 V supply                               
V5      =                 12.3 / [V] +15 V supply                               
V6      =                  9.3 / [V] +9 V supply                                
V7      =                  5.1 / [V] +5 V supply                                
V8      =                  5.0 / [V] Digital                                    
V13     =                 -0.1 / [V] Analog in                                  
V15     =               -273.0 / Temperature sensor on motherboard              
END                                                                             
