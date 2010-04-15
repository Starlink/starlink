/*
 * Created on Apr 8, 2003
 *
 */
package edu.hawaii.jach.gsd;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Tim Jenness (JAC)
 * @version $Id$
 *
 * This class provides a Lookup Table to map generic GSD item
 * names to the expanded JCMT name plus comments. Essentially a
 * helper class for GSDItem kept separate to keep GSDItem short
 * and manageable.
 */
final class GSDItemLut {
    // Should this be a singleton class instantiated once
    // or should it be completely static and just create a static
    // map the first time it is called?
    // It should clearly not redo the mapping each time we
    // instantiate an object!
    private final Map shortToLongName;
    private static GSDItemLut onlyOne;

    /**
     * Returns a lookup table object able to translate simple item names
     * to longer more self-describing names and comments.
     *
     * @return The lookup table. Always returns the same object.
     */
    static GSDItemLut getLut() {
        if (onlyOne == null) {
            onlyOne = new GSDItemLut();
        }
        return onlyOne;
    }

    private GSDItemLut() {
        // This should really read the lookup information from
        // the fortran include file. Do not know where to put the
        // fortran file in the Java hierarchy though.
        // (and there is no <DATA> handle)
        // Only option is to have lots of little constructors.
        this.shortToLongName = new HashMap();

        // Start insert
        shortToLongName.put(
            "C11PHA",
            new GSDLongItemInfo(
                "C11PHA",
                "PHASE_TABLE",
                "Phase table: switching scheme dependent"));
        shortToLongName.put(
            "C11VD",
            new GSDLongItemInfo(
                "C11VD",
                "PHASE_VARS",
                "Names of the cols. of phase table"));
        shortToLongName.put(
            "C12ALPHA",
            new GSDLongItemInfo(
                "C12ALPHA",
                "BES_ALPHA",
                "Ratio of signal sideband to image sideband sky transmission"));
        shortToLongName.put(
            "C12BM",
            new GSDLongItemInfo(
                "C12BM",
                "BES_BITMODE",
                "Correlation bit mode"));
        shortToLongName.put(
            "C12BW",
            new GSDLongItemInfo("C12BW", "BANDWIDTH", "Bandwidth"));
        shortToLongName.put(
            "C12CAL",
            new GSDLongItemInfo(
                "C12CAL",
                "DATA_UNITS",
                "Units of spectrum data"));
        shortToLongName.put(
            "C12CALTASK",
            new GSDLongItemInfo(
                "C12CALTASK",
                "BE_CAL_TASK",
                "Calibration instrument used (FE, BE, or USER)"));
        shortToLongName.put(
            "C12CALTYPE",
            new GSDLongItemInfo(
                "C12CALTYPE",
                "BE_CAL_TYPE",
                "Type of calibration (THREELOADS or TWOLOADS)"));
        shortToLongName.put(
            "C12CF",
            new GSDLongItemInfo("C12CF", "FE_NUOBS", "Centre frequency"));
        shortToLongName.put(
            "C12CM",
            new GSDLongItemInfo(
                "C12CM",
                "BES_CORR_MODE",
                "Correlation function mode"));
        shortToLongName.put(
            "C12CT",
            new GSDLongItemInfo("C12CT", "TCAL", "Calibration temperature"));
        shortToLongName.put(
            "C12ETASKY",
            new GSDLongItemInfo(
                "C12ETASKY",
                "BES_ETA_SKY",
                "Sky transmission from last calibration"));
        shortToLongName.put(
            "C12ETASKYIM",
            new GSDLongItemInfo(
                "C12ETASKYIM",
                "BES_ETA_SKY_IM",
                "Frontend-derived sky transmission"));
        shortToLongName.put(
            "C12ETATEL",
            new GSDLongItemInfo(
                "C12ETATEL",
                "BES_ETA_TEL",
                "Telescope transmission"));
        shortToLongName.put(
            "C12FR",
            new GSDLongItemInfo(
                "C12FR",
                "DELTANU",
                "Frequency resolution [MHz]"));
        shortToLongName.put(
            "C12GAINS",
            new GSDLongItemInfo("C12GAINS", "GAIN", "Gains"));
        shortToLongName.put(
            "C12GNORM",
            new GSDLongItemInfo(
                "C12GNORM",
                "G_NORM",
                "Data normalisation factor"));
        shortToLongName.put(
            "C12GREC",
            new GSDLongItemInfo(
                "C12GREC",
                "GREC",
                "Raw data units per Kelvin"));
        shortToLongName.put(
            "C12GS",
            new GSDLongItemInfo(
                "C12GS",
                "BES_G_S",
                "Normalizes signal sideband gain"));
        shortToLongName.put(
            "C12INFREQ",
            new GSDLongItemInfo(
                "C12INFREQ",
                "BE_NUIN",
                "BE input frequencies [GHz]"));
        shortToLongName.put(
            "C12NOI",
            new GSDLongItemInfo("C12NOI", "NOISE", "Noise value"));
        shortToLongName.put(
            "C12REDMODE",
            new GSDLongItemInfo(
                "C12REDMODE",
                "BE_RED_MODE",
                "Way of calibrating the data (RATIO or DIFFERENCE)"));
        shortToLongName.put(
            "C12RF",
            new GSDLongItemInfo("C12RF", "FE_NUREST", "Rest frequency"));
        shortToLongName.put(
            "C12RST",
            new GSDLongItemInfo(
                "C12RST",
                "TSYS_OFF",
                "Reference system temperature"));
        shortToLongName.put(
            "C12RT",
            new GSDLongItemInfo("C12RT", "TREC", "Receiver temperature"));
        shortToLongName.put(
            "C12SBRAT",
            new GSDLongItemInfo("C12SBRAT", "BE_SB_RATIO", "Sideband ratio"));
        shortToLongName.put(
            "C12SCAN_TABLE_1",
            new GSDLongItemInfo(
                "C12SCAN_TABLE_1",
                "SCAN_TABLE1",
                "Begin scan table"));
        shortToLongName.put(
            "C12SCAN_TABLE_2",
            new GSDLongItemInfo(
                "C12SCAN_TABLE_2",
                "SCAN_TABLE2",
                "End scan table"));
        shortToLongName.put(
            "C12SCAN_VARS1",
            new GSDLongItemInfo(
                "C12SCAN_VARS1",
                "SCAN_VARS1",
                "Names of the cols. of scan table1"));
        shortToLongName.put(
            "C12SCAN_VARS2",
            new GSDLongItemInfo(
                "C12SCAN_VARS2",
                "SCAN_VARS2",
                "Names of the cols. of scan table2"));
        shortToLongName.put(
            "C12SST",
            new GSDLongItemInfo(
                "C12SST",
                "TSYS_ON",
                "Source system temperature"));
        shortToLongName.put(
            "C12TAMB",
            new GSDLongItemInfo(
                "C12TAMB",
                "T_HOT",
                "Ambient load temperature"));
        shortToLongName.put(
            "C12TASKY",
            new GSDLongItemInfo(
                "C12TASKY",
                "BES_TA_SKY",
                "Ratio of signal sideband to image sideband sky transmission"));
        shortToLongName.put(
            "C12TCOLD",
            new GSDLongItemInfo("C12TCOLD", "T_COLD", "Cold load temperature"));
        shortToLongName.put(
            "C12TSKY",
            new GSDLongItemInfo(
                "C12TSKY",
                "TSKY",
                "Sky temperature at last calibration"));
        shortToLongName.put(
            "C12TSKYIM",
            new GSDLongItemInfo(
                "C12TSKYIM",
                "BES_T_SKY_IM",
                "Frontend-derived Tsky, image sideband"));
        shortToLongName.put(
            "C12TSYSIM",
            new GSDLongItemInfo(
                "C12TSYSIM",
                "BES_T_SYS_IM",
                "Frontend-derived Tsys, image sideband"));
        shortToLongName.put(
            "C12TTEL",
            new GSDLongItemInfo(
                "C12TTEL",
                "TTEL",
                "Telescope temp. from last skydip"));
        shortToLongName.put(
            "C12VCOLD",
            new GSDLongItemInfo("C12VCOLD", "IF_V_COLD", ""));
        shortToLongName.put(
            "C12VDEF",
            new GSDLongItemInfo(
                "C12VDEF",
                "VEL_DEFN",
                "Velocity definition code - radio, optical, or relativistic"));
        shortToLongName.put(
            "C12VHOT",
            new GSDLongItemInfo("C12VHOT", "IF_V_HOT", ""));
        shortToLongName.put(
            "C12VREF",
            new GSDLongItemInfo(
                "C12VREF",
                "VEL_REF",
                "Velocity frame of reference - LSR, Bary-, Helio-, or Geo- centric"));
        shortToLongName.put(
            "C12VSKY",
            new GSDLongItemInfo("C12VSKY", "IF_V_SKY", ""));
        shortToLongName.put(
            "C12WO",
            new GSDLongItemInfo("C12WO", "H2O_OPACITY", "Water opacity"));
        shortToLongName.put(
            "C13DAT",
            new GSDLongItemInfo("C13DAT", "DATA", "Reduced data"));
        shortToLongName.put(
            "C13ERR",
            new GSDLongItemInfo("C13ERR", "ERROR", "Standard error"));
        shortToLongName.put(
            "C13RAW_ERROR",
            new GSDLongItemInfo("C13RAW_ERROR", "DATA_ERROR", ""));
        shortToLongName.put(
            "C13RAW_ERROR_OP",
            new GSDLongItemInfo(
                "C13RAW_ERROR_OP",
                "RAW_ERROR_OP",
                "Raw (out of phase) error also to be stored at end scan"));
        shortToLongName.put(
            "C13RESP",
            new GSDLongItemInfo("C13RESP", "RESP", "array of responsivities"));
        shortToLongName.put(
            "C13SPV",
            new GSDLongItemInfo("C13SPV", "SAMPLES", ""));
        shortToLongName.put(
            "C13SPV_OP",
            new GSDLongItemInfo(
                "C13SPV_OP",
                "SAMPLES_OP",
                "Raw out of phase data samples in each phase"));
        shortToLongName.put(
            "C13STD",
            new GSDLongItemInfo(
                "C13STD",
                "STDEV",
                "Phase data standard deviation"));
        shortToLongName.put(
            "C14PHIST",
            new GSDLongItemInfo(
                "C14PHIST",
                "MAP_TABLE",
                "List of xy offsets for each scan"));
        shortToLongName.put(
            "C1BKE",
            new GSDLongItemInfo("C1BKE", "BACKEND", "Name of the backend"));
        shortToLongName.put(
            "C1BTYP",
            new GSDLongItemInfo("C1BTYP", "BE_TYPE", "Type of backend"));
        shortToLongName.put(
            "C1DP",
            new GSDLongItemInfo(
                "C1DP",
                "DATA_PRECISION",
                "Precision of the data from the backend"));
        shortToLongName.put(
            "C1FTYP",
            new GSDLongItemInfo("C1FTYP", "FE_TYPE", "Type of frontend"));
        shortToLongName.put(
            "C1HGT",
            new GSDLongItemInfo(
                "C1HGT",
                "TEL_HEIGHT",
                "Height of telescope above sea level"));
        shortToLongName.put(
            "C1IFS",
            new GSDLongItemInfo("C1IFS", "IF_DEVICE", "Name of the IF device"));
        shortToLongName.put(
            "C1LAT",
            new GSDLongItemInfo(
                "C1LAT",
                "TEL_LATITUDE",
                "Geodetic latitude of telescope (North +ve)"));
        shortToLongName.put(
            "C1LONG",
            new GSDLongItemInfo(
                "C1LONG",
                "TEL_LONGITUDE",
                "Geographical longitude of telescope (West +ve)"));
        shortToLongName.put(
            "C1OBS",
            new GSDLongItemInfo(
                "C1OBS",
                "PROJECT_OBS_1",
                "Name of the primary observer"));
        shortToLongName.put(
            "C1ONA",
            new GSDLongItemInfo(
                "C1ONA",
                "PROJECT_OBS_1",
                "Name of the primary observer"));
        shortToLongName.put(
            "C1ONA1",
            new GSDLongItemInfo(
                "C1ONA1",
                "PROJECT_OBS_2",
                "Name of the support scientist"));
        shortToLongName.put(
            "C1ONA2",
            new GSDLongItemInfo(
                "C1ONA2",
                "PROJECT_OBS_3",
                "Name of the telescope operator"));
        shortToLongName.put(
            "C1PID",
            new GSDLongItemInfo(
                "C1PID",
                "PROJECT",
                "Identifies the observing program"));
        shortToLongName.put(
            "C1RCV",
            new GSDLongItemInfo("C1RCV", "FRONTEND", "Name of the frontend"));
        shortToLongName.put(
            "C1SNA1",
            new GSDLongItemInfo(
                "C1SNA1",
                "CENTRE_NAME_1",
                "Source name part 1"));
        shortToLongName.put(
            "C1SNA2",
            new GSDLongItemInfo(
                "C1SNA2",
                "CENTRE_NAME_2",
                "Source name part 2 or altern. name"));
        shortToLongName.put(
            "C1SNO",
            new GSDLongItemInfo("C1SNO", "NOBS", "Observation number"));
        shortToLongName.put(
            "C1STC",
            new GSDLongItemInfo("C1STC", "OBS_TYPE", "Type of observation"));
        shortToLongName.put(
            "C1TEL",
            new GSDLongItemInfo("C1TEL", "TEL_NAME", "Telescope name"));
        shortToLongName.put(
            "C2FL",
            new GSDLongItemInfo(
                "C2FL",
                "DY",
                "Secondary mirror y displacement from nominal at observation start"));
        shortToLongName.put(
            "C2FR",
            new GSDLongItemInfo(
                "C2FR",
                "DZ",
                "Secondary mirror z displacement from nominal at observation start"));
        shortToLongName.put(
            "C2FV",
            new GSDLongItemInfo(
                "C2FV",
                "DX",
                "Secondary mirror x displacement from nominal at observation start"));
        shortToLongName.put(
            "C2ORI",
            new GSDLongItemInfo(
                "C2ORI",
                "SECONDARY_ORI",
                "Rotation or polarization angle orientation of the frontend/reflector"));
        shortToLongName.put(
            "C2PC1",
            new GSDLongItemInfo(
                "C2PC1",
                "TEL_PC_LAN",
                "Angle by which lower axis is north of ideal"));
        shortToLongName.put(
            "C2PC2",
            new GSDLongItemInfo(
                "C2PC2",
                "TEL_PC_LAE",
                "Angle by which lower axis is east of ideal"));
        shortToLongName.put(
            "C2PC3",
            new GSDLongItemInfo(
                "C2PC3",
                "TEL_PC_UANP",
                "Angle by which upper axis is not perpendicular to lower"));
        shortToLongName.put(
            "C2PC4",
            new GSDLongItemInfo(
                "C2PC4",
                "TEL_PC_BNP",
                "Angle by which beam is not perpendicular to upper axis"));
        shortToLongName.put(
            "C2XPC",
            new GSDLongItemInfo(
                "C2XPC",
                "TEL_PC_LAZ",
                "Azimuth/RA enc.zero; enc.reading = az + pc_laz"));
        shortToLongName.put(
            "C2YPC",
            new GSDLongItemInfo(
                "C2YPC",
                "TEL_PC_UAZ",
                "Altitude/DEC enc.zero; enc.reading = az + pc_uaz"));
        shortToLongName.put(
            "C3BEFENULO",
            new GSDLongItemInfo(
                "C3BEFENULO",
                "BES_FE_NULO",
                "Copy of frontend LO frequency per backend section"));
        shortToLongName.put(
            "C3BEFESB",
            new GSDLongItemInfo(
                "C3BEFESB",
                "BES_FE_SB_SIGN",
                "Copy of frontend sideband sign per backend section"));
        shortToLongName.put(
            "C3BEINCON",
            new GSDLongItemInfo(
                "C3BEINCON",
                "BE_IN_CONN",
                "IF output channels connected to BE input channels"));
        shortToLongName.put(
            "C3BESCONN",
            new GSDLongItemInfo(
                "C3BESCONN",
                "BES_CONN",
                "BE input channels connected to this section"));
        shortToLongName.put(
            "C3BESSPEC",
            new GSDLongItemInfo(
                "C3BESSPEC",
                "BES_SPECTRUM",
                "Subsystem nr to which each backend section belongs."));
        shortToLongName.put(
            "C3BETOTIF",
            new GSDLongItemInfo(
                "C3BETOTIF",
                "BES_TOT_IF",
                "Total IF per backend section"));
        shortToLongName.put(
            "C3CAL",
            new GSDLongItemInfo(
                "C3CAL",
                "OBS_CALIBRATION",
                "Calibration observation?"));
        shortToLongName.put(
            "C3CEN",
            new GSDLongItemInfo(
                "C3CEN",
                "OBS_CENTRE",
                "Centre moves between scans?"));
        shortToLongName.put(
            "C3CL",
            new GSDLongItemInfo(
                "C3CL",
                "CYCLE_TIME",
                "Duration of each cycle"));
        shortToLongName.put(
            "C3CONFIGNR",
            new GSDLongItemInfo(
                "C3CONFIGNR",
                "DAS_CONF_NR",
                "Backend configuration"));
        shortToLongName.put(
            "C3DASCALSRC",
            new GSDLongItemInfo(
                "C3DASCALSRC",
                "DAS_CAL_SOURCE",
                "DAS calibration source for backend calibration (POWER or DATA)"));
        shortToLongName.put(
            "C3DASOUTPUT",
            new GSDLongItemInfo(
                "C3DASOUTPUT",
                "DAS_OUTPUT",
                "Description of output in DAS DATA (SPECTRUM, T_REC, T_SYS, etc.)"));
        shortToLongName.put(
            "C3DASSHFTFRAC",
            new GSDLongItemInfo(
                "C3DASSHFTFRAC",
                "DAS_SHIFT_FRAC",
                "DAS calibration source for backend calibration (POWER or DATA)"));
        shortToLongName.put(
            "C3DAT",
            new GSDLongItemInfo(
                "C3DAT",
                "OBS_UT1D",
                "UT1 date of observation"));
        shortToLongName.put(
            "C3FLY",
            new GSDLongItemInfo(
                "C3FLY",
                "OBS_CONTINUOUS",
                "Data taken on the fly or in discrete mode?"));
        shortToLongName.put(
            "C3FOCUS",
            new GSDLongItemInfo("C3FOCUS", "OBS_FOCUS", "Focus observation?"));
        shortToLongName.put(
            "C3INTT",
            new GSDLongItemInfo(
                "C3INTT",
                "INTGRN_TIME",
                "Scan integration time"));
        shortToLongName.put(
            "C3LSPC",
            new GSDLongItemInfo(
                "C3LSPC",
                "NO_BES_O_CH",
                "Number of channels per backend section"));
        shortToLongName.put(
            "C3LST",
            new GSDLongItemInfo(
                "C3LST",
                "OBS_LST",
                "Local sidereal time at the start of the observation"));
        shortToLongName.put(
            "C3MAP",
            new GSDLongItemInfo("C3MAP", "OBS_MAP", "Map observation?"));
        shortToLongName.put(
            "C3MXP",
            new GSDLongItemInfo(
                "C3MXP",
                "NO_SCAN_PNTS",
                "Maximum number of map points done in a phase"));
        shortToLongName.put(
            "C3NCH",
            new GSDLongItemInfo(
                "C3NCH",
                "NO_BE_O_CH",
                "No.backend output channels"));
        shortToLongName.put(
            "C3NCI",
            new GSDLongItemInfo(
                "C3NCI",
                "NO_CYCLES",
                "Maximum number of cycles in the scan"));
        shortToLongName.put(
            "C3NCP",
            new GSDLongItemInfo(
                "C3NCP",
                "NO_CYCLE_PNTS",
                "Total number of xy positions observed during a cycle"));
        shortToLongName.put(
            "C3NCYCLE",
            new GSDLongItemInfo(
                "C3NCYCLE",
                "NCYCLE",
                "Number of cycles done in the scan"));
        shortToLongName.put(
            "C3NFOC",
            new GSDLongItemInfo(
                "C3NFOC",
                "NO_FE_O_CH",
                "NO_FE_O_CH:No. of frontend output channels"));
        shortToLongName.put(
            "C3NIS",
            new GSDLongItemInfo("C3NIS", "NO_SCANS", "Number of scans"));
        shortToLongName.put(
            "C3NLOOPS",
            new GSDLongItemInfo(
                "C3NLOOPS",
                "NO_SCANS",
                "Number of scans per observation commanded at observation start"));
        shortToLongName.put(
            "C3NMAP",
            new GSDLongItemInfo(
                "C3NMAP",
                "NO_MAP_PNTS",
                "Number of map points"));
        shortToLongName.put(
            "C3NOIFPBES",
            new GSDLongItemInfo(
                "C3NOIFPBES",
                "NO_IF_PER_BES",
                "Number of IF inputs to each section (2 for correlator, 1 for AOS)"));
        shortToLongName.put(
            "C3NO_SCAN_VARS1",
            new GSDLongItemInfo(
                "C3NO_SCAN_VARS1",
                "NO_SCAN_VARS1",
                "Number of scan table 1 variables"));
        shortToLongName.put(
            "C3NO_SCAN_VARS2",
            new GSDLongItemInfo(
                "C3NO_SCAN_VARS2",
                "NO_SCAN_VARS2",
                "Number of scan table 2 variables"));
        shortToLongName.put(
            "C3NPP",
            new GSDLongItemInfo(
                "C3NPP",
                "NO_MAP_DIMS",
                "Number of dimension in the map table"));
        shortToLongName.put(
            "C3NRC",
            new GSDLongItemInfo(
                "C3NRC",
                "NRC",
                "NO_BE_I_CH:No.backend input channels"));
        shortToLongName.put(
            "C3NRS",
            new GSDLongItemInfo(
                "C3NRS",
                "NO_BES",
                "Number of backend sections"));
        shortToLongName.put(
            "C3NSAMPLE",
            new GSDLongItemInfo("C3NSAMPLE", "NSCAN", "Number of scans done"));
        shortToLongName.put(
            "C3NSV",
            new GSDLongItemInfo(
                "C3NSV",
                "NO_PHASE_VARS",
                "Number of phase table variables"));
        shortToLongName.put(
            "C3OVERLAP",
            new GSDLongItemInfo("C3OVERLAP", "BES_OVERLAP", "Subband overlap"));
        shortToLongName.put(
            "C3PPC",
            new GSDLongItemInfo(
                "C3PPC",
                "NO_PHASES",
                "Number of phases per cycle"));
        shortToLongName.put(
            "C3SRT",
            new GSDLongItemInfo(
                "C3SRT",
                "SCAN_TIME",
                "Total time of scan (=total integration time if OBS_CONTINUOUS = .FALSE.)"));
        shortToLongName.put(
            "C3UT",
            new GSDLongItemInfo("C3UT", "OBS_UT1H", "UT1 hour of observation"));
        shortToLongName.put(
            "C3UT1C",
            new GSDLongItemInfo(
                "C3UT1C",
                "OBS_UT1C",
                "UT1-UTC correction interpolated from time service telex (in days)"));
        shortToLongName.put(
            "C4AMPL_EW",
            new GSDLongItemInfo(
                "C4AMPL_EW",
                "AMPL_EW",
                "Secondary mirror chopping amplitude parallel to lower axis"));
        shortToLongName.put(
            "C4AMPL_NS",
            new GSDLongItemInfo(
                "C4AMPL_NS",
                "AMPL_NS",
                "Secondary mirror chopping amplitude parallel to upper axis"));
        shortToLongName.put(
            "C4AXY",
            new GSDLongItemInfo(
                "C4AXY",
                "CELL_X2Y",
                "Angle between cell y axis and x-axis (CCW)"));
        shortToLongName.put(
            "C4AZ",
            new GSDLongItemInfo(
                "C4AZ",
                "CENTRE_AZ",
                "Azimuth at observation date"));
        shortToLongName.put(
            "C4AZERR",
            new GSDLongItemInfo(
                "C4AZERR",
                "SDIS(7)",
                "DAZ:Net Az offset at start (inc.tracker ball setting and user correction)"));
        shortToLongName.put(
            "C4CECO",
            new GSDLongItemInfo(
                "C4CECO",
                "CENTRE_CODE",
                "centre coords. AZ= 1;EQ=3;RD=4;RB=6;RJ=7;GA=8"));
        shortToLongName.put(
            "C4CSC",
            new GSDLongItemInfo(
                "C4CSC",
                "CENTRE_COORDS",
                "Character code of commanded centre or source coordinate system"));
        shortToLongName.put(
            "C4DAZ",
            new GSDLongItemInfo(
                "C4DAZ",
                "DAZ_SM",
                "Telescope lower axis correction for secondary mirror XYZ"));
        shortToLongName.put(
            "C4DECDATE",
            new GSDLongItemInfo(
                "C4DECDATE",
                "CENTRE_DEC",
                "Declination of date"));
        shortToLongName.put(
            "C4DEL",
            new GSDLongItemInfo(
                "C4DEL",
                "DEL_SM",
                "Telescope upper axis correction for secondary mirror XYZ"));
        shortToLongName.put(
            "C4DO1",
            new GSDLongItemInfo(
                "C4DO1",
                "CELL_X",
                "Cell x dimension; descriptive origin item 1"));
        shortToLongName.put(
            "C4DO2",
            new GSDLongItemInfo(
                "C4DO2",
                "CELL_Y",
                "Cell y dimension; descriptive origin item 2"));
        shortToLongName.put(
            "C4DO3",
            new GSDLongItemInfo(
                "C4DO3",
                "CELL_V2X",
                "Angle by which the cell x axis is oriented with respect to local vertical"));
        shortToLongName.put(
            "C4EDC",
            new GSDLongItemInfo("C4EDC", "CENTRE_DEC", "Declination of date"));
        shortToLongName.put(
            "C4EDEC",
            new GSDLongItemInfo(
                "C4EDEC",
                "CENTRE_DEC1950",
                "Declination of source for EPOCH"));
        shortToLongName.put(
            "C4EDEC2000",
            new GSDLongItemInfo(
                "C4EDEC2000",
                "CENTRE_DEC2000",
                "Declination J2000"));
        shortToLongName.put(
            "C4EL",
            new GSDLongItemInfo(
                "C4EL",
                "CENTRE_EL",
                "Elevation at observation date"));
        shortToLongName.put(
            "C4ELERR",
            new GSDLongItemInfo(
                "C4ELERR",
                "SDIS(8)",
                "DEL:Net El offset at start (inc.tracker ball setting and user correction)"));
        shortToLongName.put(
            "C4EPH",
            new GSDLongItemInfo(
                "C4EPH",
                "CENTRE_EPOCH",
                "Date of the RA/DEC coordinates (1950)"));
        shortToLongName.put(
            "C4EPT",
            new GSDLongItemInfo(
                "C4EPT",
                "EPOCH_TYPE",
                "Type of epoch, JULIAN, BESSELIAN or APPARENT"));
        shortToLongName.put(
            "C4ERA",
            new GSDLongItemInfo(
                "C4ERA",
                "CENTRE_RA1950",
                "Right ascension of source for EPOCH"));
        shortToLongName.put(
            "C4EW_ENCODER",
            new GSDLongItemInfo(
                "C4EW_ENCODER",
                "AMPL_E_SET",
                "Secondary mirror ew encoder value"));
        shortToLongName.put(
            "C4EW_SCALE",
            new GSDLongItemInfo(
                "C4EW_SCALE",
                "EW_AMPL_SCALE",
                "Secondary mirror ew chop scale"));
        shortToLongName.put(
            "C4FRQ",
            new GSDLongItemInfo(
                "C4FRQ",
                "FREQUENCY",
                "Secondary mirror chopping period"));
        shortToLongName.put(
            "C4FUN",
            new GSDLongItemInfo(
                "C4FUN",
                "WAVEFORM",
                "Secondary mirror chopping waveform"));
        shortToLongName.put(
            "C4GB",
            new GSDLongItemInfo("C4GB", "CENTRE_GB", "Galactic latitude"));
        shortToLongName.put(
            "C4GL",
            new GSDLongItemInfo("C4GL", "CENTRE_GL", "Galactic longitude"));
        shortToLongName.put(
            "C4LSC",
            new GSDLongItemInfo(
                "C4LSC",
                "CELL_COORDS",
                "Char. code for local x-y coord.system"));
        shortToLongName.put(
            "C4MCF",
            new GSDLongItemInfo(
                "C4MCF",
                "CENTRE_MOVING",
                "Centre moving flag (solar system object)"));
        shortToLongName.put(
            "C4MOCO",
            new GSDLongItemInfo(
                "C4MOCO",
                "TEL_COORDS",
                "Mounting of telescope; defined as LOWER/UPPER axes, e.g; AZ/ALT"));
        shortToLongName.put(
            "C4NS_ENCODER",
            new GSDLongItemInfo(
                "C4NS_ENCODER",
                "AMPL_N_SET",
                "Secondary mirror ns encoder value"));
        shortToLongName.put(
            "C4NS_SCALE",
            new GSDLongItemInfo(
                "C4NS_SCALE",
                "NS_AMPL_SCALE",
                "Secondary mirror ns chop scale"));
        shortToLongName.put(
            "C4ODCO",
            new GSDLongItemInfo(
                "C4ODCO",
                "CELL_UNIT",
                "Units of cell and mapping coordinates;offset definition code"));
        shortToLongName.put(
            "C4OFFS_EW",
            new GSDLongItemInfo(
                "C4OFFS_EW",
                "OFFS_EW",
                "Secondary mirror offset parallel to lower axis (East-West Tilt)"));
        shortToLongName.put(
            "C4OFFS_NS",
            new GSDLongItemInfo(
                "C4OFFS_NS",
                "OFFS_NS",
                "Secondary mirror offset parallel to upper axis (North-South Tilt)"));
        shortToLongName.put(
            "C4PER",
            new GSDLongItemInfo(
                "C4PER",
                "PERIOD",
                "Secondary mirror chopping period"));
        shortToLongName.put(
            "C4POSANG",
            new GSDLongItemInfo(
                "C4POSANG",
                "POSANG",
                "Secondary mirror chop position angle"));
        shortToLongName.put(
            "C4RA2000",
            new GSDLongItemInfo(
                "C4RA2000",
                "CENTRE_RA2000",
                "Right ascension J2000"));
        shortToLongName.put(
            "C4RADATE",
            new GSDLongItemInfo(
                "C4RADATE",
                "CENTRE_RA",
                "Right Ascension of date"));
        shortToLongName.put(
            "C4RX",
            new GSDLongItemInfo(
                "C4RX",
                "REFERENCE_X",
                "Reference x position (JCMT cells wrt to centre; NRAO abs. degrees)"));
        shortToLongName.put(
            "C4RY",
            new GSDLongItemInfo(
                "C4RY",
                "REFERENCE_Y",
                "Reference y position (JCMT cells wrt to centre; NRAO abs. degrees)"));
        shortToLongName.put(
            "C4SM",
            new GSDLongItemInfo(
                "C4SM",
                "CHOPPING",
                "Secondary mirror is chopping"));
        shortToLongName.put(
            "C4SMCO",
            new GSDLongItemInfo(
                "C4SMCO",
                "COORDS",
                "Secondary mirror chopping coordinate system"));
        shortToLongName.put(
            "C4SX",
            new GSDLongItemInfo(
                "C4SX",
                "CENTRE_OFFSET_X",
                "Commanded x centre position (JCMT cells wrt to centre; NRAO abs. degrees)"));
        shortToLongName.put(
            "C4SY",
            new GSDLongItemInfo(
                "C4SY",
                "CENTRE_OFFSET_Y",
                "Commanded y centre position (JCMT cells wrt to centre; NRAO abs. degrees)"));
        shortToLongName.put(
            "C4THROW",
            new GSDLongItemInfo(
                "C4THROW",
                "THROW",
                "Secondary mirror chop throw"));
        shortToLongName.put(
            "C4X",
            new GSDLongItemInfo(
                "C4X",
                "X",
                "Secondary mirror absolute X position at observation start"));
        shortToLongName.put(
            "C4Y",
            new GSDLongItemInfo(
                "C4Y",
                "Y",
                "Secondary mirror absolute Y position at observation start"));
        shortToLongName.put(
            "C4Z",
            new GSDLongItemInfo(
                "C4Z",
                "Z",
                "Secondary mirror absolute Z position at observation start"));
        shortToLongName.put(
            "C5AT",
            new GSDLongItemInfo("C5AT", "TAMB", "Ambient temperature"));
        shortToLongName.put(
            "C5DP",
            new GSDLongItemInfo(
                "C5DP",
                "DEW_POINT",
                "Mean atmospheric dew point"));
        shortToLongName.put(
            "C5IR",
            new GSDLongItemInfo(
                "C5IR",
                "REF_INDEX",
                "Mean atmospheric refractive index (alternative)"));
        shortToLongName.put(
            "C5IR1",
            new GSDLongItemInfo(
                "C5IR1",
                "TEL_REFR_A",
                "Refraction constant A (see MTIN026)"));
        shortToLongName.put(
            "C5IR2",
            new GSDLongItemInfo(
                "C5IR2",
                "TEL_REFR_B",
                "Refraction constant B (see MTIN026)"));
        shortToLongName.put(
            "C5IR3",
            new GSDLongItemInfo(
                "C5IR3",
                "TEL_REFR_C",
                "Refraction constant C (see MTIN026)"));
        shortToLongName.put(
            "C5MM",
            new GSDLongItemInfo(
                "C5MM",
                "VAP_PRESSURE",
                "Mean atmospheric vapour pressure"));
        shortToLongName.put(
            "C5PRS",
            new GSDLongItemInfo("C5PRS", "PAMB", "Mean atmospheric pressure"));
        shortToLongName.put(
            "C5RH",
            new GSDLongItemInfo(
                "C5RH",
                "HAMB",
                "Mean atmospheric relative humidity"));
        shortToLongName.put(
            "C6CYCLREV",
            new GSDLongItemInfo(
                "C6CYCLREV",
                "CYCLE_REVERSAL",
                "Cycle reversal flag"));
        shortToLongName.put(
            "C6DX",
            new GSDLongItemInfo(
                "C6DX",
                "CELL_X",
                "Cell x dim,; descriptive origin item 1"));
        shortToLongName.put(
            "C6DY",
            new GSDLongItemInfo(
                "C6DY",
                "CELL_Y",
                "Cell y dimension; descriptive origin item 2"));
        shortToLongName.put(
            "C6FC",
            new GSDLongItemInfo(
                "C6FC",
                "CELL_CODE",
                "Local x-y AZ= 1;EQ=3;RD=4;RB=6;RJ=7;GA=8"));
        shortToLongName.put(
            "C6MODE",
            new GSDLongItemInfo("C6MODE", "SWITCH_MODE", "Observation mode"));
        shortToLongName.put(
            "C6MSA",
            new GSDLongItemInfo(
                "C6MSA",
                "CELL_V2X",
                "Scanning angle - angle from local vertical to x axis measured CW"));
        shortToLongName.put(
            "C6NP",
            new GSDLongItemInfo(
                "C6NP",
                "NCYCLE_PNTS",
                "Number of sky points completed in the observation"));
        shortToLongName.put(
            "C6REV",
            new GSDLongItemInfo(
                "C6REV",
                "SCAN_REVERSAL",
                "Map rows scanned in alternate directions?"));
        shortToLongName.put(
            "C6SD",
            new GSDLongItemInfo(
                "C6SD",
                "OBS_DIRECTION",
                "Map rows are in X (horizontal) or Y(vertical) direction"));
        shortToLongName.put(
            "C6ST",
            new GSDLongItemInfo("C6ST", "OBS_TYPE", "Type of observation"));
        shortToLongName.put(
            "C6XGC",
            new GSDLongItemInfo(
                "C6XGC",
                "X_MAP_START",
                "X coordinate of the first map point"));
        shortToLongName.put(
            "C6XNP",
            new GSDLongItemInfo(
                "C6XNP",
                "NO_X_MAP_PNTS",
                "X map dimension; number of points in the x-direction"));
        shortToLongName.put(
            "C6XPOS",
            new GSDLongItemInfo(
                "C6XPOS",
                "X_MAP_POSITIVE",
                "In first row x increases (TRUE) or decreases (FALSE)"));
        shortToLongName.put(
            "C6YGC",
            new GSDLongItemInfo(
                "C6YGC",
                "Y_MAP_START",
                "Y coordinate of the first map point"));
        shortToLongName.put(
            "C6YNP",
            new GSDLongItemInfo(
                "C6YNP",
                "NO_Y_MAP_PNTS",
                "Y map dimension; number of points in the y-direction"));
        shortToLongName.put(
            "C6YPOS",
            new GSDLongItemInfo(
                "C6YPOS",
                "Y_MAP_POSITIVE",
                "In first row y increases (TRUE) or decreases (FALSE)"));
        shortToLongName.put(
            "C7AP",
            new GSDLongItemInfo("C7AP", "APERTURE", "Aperture"));
        shortToLongName.put(
            "C7BCV",
            new GSDLongItemInfo("C7BCV", "BAD_CHANNEL", "Bad channel value"));
        shortToLongName.put(
            "C7CAL",
            new GSDLongItemInfo(
                "C7CAL",
                "CAL_TYPE",
                "Calibration type (standard or direct, chopperwheel)"));
        shortToLongName.put(
            "C7FIL",
            new GSDLongItemInfo("C7FIL", "FILTER", "Filter"));
        shortToLongName.put(
            "C7HP",
            new GSDLongItemInfo(
                "C7HP",
                "FWHM",
                "FWHM of the beam profile (mean)"));
        shortToLongName.put(
            "C7NIF",
            new GSDLongItemInfo("C7NIF", "NO_IF_CH", "Number of IF channels"));
        shortToLongName.put(
            "C7OSN",
            new GSDLongItemInfo(
                "C7OSN",
                "OBS_REF_SCAN",
                "Calibration observation number (in which a standard was observed)"));
        shortToLongName.put(
            "C7PHASE",
            new GSDLongItemInfo("C7PHASE", "PHASE", "Lockin phase"));
        shortToLongName.put(
            "C7SEEING",
            new GSDLongItemInfo("C7SEEING", "SAO_SEEING", "Seeing at JCMT"));
        shortToLongName.put(
            "C7SEETIME",
            new GSDLongItemInfo(
                "C7SEETIME",
                "SAO_YYMMDDHHMM",
                "SAO seeing time (YYMMDDHHMM)"));
        shortToLongName.put(
            "C7SNSTVTY",
            new GSDLongItemInfo(
                "C7SNSTVTY",
                "SENSITIVITY",
                "Lockin sensitivity in scale range units"));
        shortToLongName.put(
            "C7SNTVTYRG",
            new GSDLongItemInfo(
                "C7SNTVTYRG",
                "RANGE",
                "Sensitivity range of lockin"));
        shortToLongName.put(
            "C7SZVRAD",
            new GSDLongItemInfo(
                "C7SZVRAD",
                "SZVRAD",
                "Number of elements of vradial array"));
        shortToLongName.put(
            "C7TAU225",
            new GSDLongItemInfo("C7TAU225", "CSO_TAU", "CSO tau at 225GHz"));
        shortToLongName.put(
            "C7TAURMS",
            new GSDLongItemInfo("C7TAURMS", "CSO_TAU_RMS", "CSO tau rms"));
        shortToLongName.put(
            "C7TAUTIME",
            new GSDLongItemInfo(
                "C7TAUTIME",
                "CSO_YYMMDDHHMM",
                "CSO tau time (YYMMDDHHMM)"));
        shortToLongName.put(
            "C7TIMECNST",
            new GSDLongItemInfo(
                "C7TIMECNST",
                "TIME_CONSTANT",
                "Lockin time constant"));
        shortToLongName.put(
            "C7VC",
            new GSDLongItemInfo("C7VC", "VEL_COR", "Velocity correction"));
        shortToLongName.put(
            "C7VR",
            new GSDLongItemInfo(
                "C7VR",
                "VELOCITY",
                "Radial velocity of the source"));
        shortToLongName.put(
            "C7VRD",
            new GSDLongItemInfo(
                "C7VRD",
                "VEL_DEF",
                "Velocity definition code; method of computing the velocity"));
        shortToLongName.put(
            "C7VREF",
            new GSDLongItemInfo(
                "C7VREF",
                "VEL_REF",
                "Velocity reference code; reference point for telescope & source velocity"));
        shortToLongName.put(
            "C8AAE",
            new GSDLongItemInfo(
                "C8AAE",
                "APERTURE_EFF",
                "Ratio total power observed/incident on the telescope"));
        shortToLongName.put(
            "C8ABE",
            new GSDLongItemInfo(
                "C8ABE",
                "BEAM_EFF",
                "Fraction of beam in diffraction limited main beam"));
        shortToLongName.put(
            "C8EF",
            new GSDLongItemInfo(
                "C8EF",
                "ETAFSS",
                "Forward spillover and scattering efficiency"));
        shortToLongName.put(
            "C8EL",
            new GSDLongItemInfo(
                "C8EL",
                "ETAL",
                "Rear spillover and scattering efficiency"));
        shortToLongName.put(
            "C8GN",
            new GSDLongItemInfo("C8GN", "ANTENNA_GAIN", "Antenna gain"));
        shortToLongName.put(
            "C90T",
            new GSDLongItemInfo(
                "C90T",
                "TEL_TOLERANCE",
                "Observing tolerance"));
        shortToLongName.put(
            "C9OT",
            new GSDLongItemInfo(
                "C9OT",
                "TEL_TOLERANCE",
                "Observing tolerance"));
        shortToLongName.put(
            "CELL_V2Y",
            new GSDLongItemInfo(
                "CELL_V2Y",
                "CELL_V2Y",
                "Position angle of cell y axis (CCW)"));
        shortToLongName.put(
            "UAZ",
            new GSDLongItemInfo("UAZ", "SDIS(36)", "User az correction"));
        shortToLongName.put(
            "UEL",
            new GSDLongItemInfo("UEL", "SDIS(37)", "User el correction"));

        // End insert

    }

    /**
     * Given a short NRAO GSD item name, return the long JCMT name.
     * Returns the short name if there is no translation to a long
     * name.
     *
     */
    String getLongName(String itemName) {
        GSDLongItemInfo info = (GSDLongItemInfo) shortToLongName.get(itemName);
        if (info == null) {
            return itemName;
        } else {
            return info.getLongName();
        }
    }

    /**
     * Given an short item name, returns the corresponding JCMT
     * comment.
     *
     * @param itemName
     * @return The comment. Returns "" if no comment could be found.
      */

    String getComment(String itemName) {
        GSDLongItemInfo info = (GSDLongItemInfo) shortToLongName.get(itemName);
        if (info == null) {
            return "";
        } else {
            return info.getComment();
        }
    }

    /**
     *
     * @author timj
     *
     * To change the template for this generated type comment go to
     * Window>Preferences>Java>Code Generation>Code and Comments
     */

    // Each item must be its own little object
    private class GSDLongItemInfo {
        private final String longName;
        private final String comment;
        private final String shortName;

        private GSDLongItemInfo(
            String shortname,
            String longname,
            String comment) {
            this.shortName = shortname;
            this.longName = longname;
            this.comment = comment;
        }

        /**
         * @return
         */
        private String getComment() {
            return comment;
        }

        /**
         * @return
         */
        private String getLongName() {
            return longName;
        }

        /**
         * @return
         */
        private String getShortName() {
            return shortName;
        }

    }

}
