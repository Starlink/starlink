(new-routine "GACTM"
             "Accumulate transformation matrix"
             '("MIN" "X0" "Y0" "DX" "DY" "PHI" "FX" "FY" "GKS$SW" "MOUT"))

(new-routine "GACWK"
             "Activate workstation"
             '("WKID"))

(new-routine "GCA"
             "Cell array"
             '("PX" "PY" "QX" "QY" "NX" "NY" "DIMX" "COLIA"))

(new-routine "GCLKS"
             "Close GKS"
             ( ))

(new-routine "GCLRWK"
             "Clear workstation"
             '("WKID" "GKS$COFL"))

(new-routine "GCLSG"
             "Close segment"
             ( ))

(new-routine "GCLWK"
             "Close workstation"
             '("WKID"))

(new-routine "GCRSG"
             "Create segment"
             '("SGNA"))

(new-routine "GDAWK"
             "Deactivate workstation"
             '("WKID"))

(new-routine "GDGP"
             "Generalised drawing primitive"
             '("N" "PX" "PY" "PRIMID" "LDR" "DATREC"))

(new-routine "GDSG"
             "Delete segment"
             '("SGNA"))

(new-routine "GDSGWK"
             "Delete segment from workstation"
             '("WKID" "SGNA"))

(new-routine "GECLKS"
             "Emergency close GKS"
             ( ))

(new-routine "GERHND"
             "Error handling"
             '("ERRNR" "FCTID" "ERRFIL"))

(new-routine "GERLOG"
             "Error logging"
             '("ERRNR" "FCTID" "ERRFIL"))

(new-routine "GESC"
             "Escape"
             '("FCTID" "LDR" "DATREC"))

(new-routine "GEVTM"
             "Evaluate transformation matrix"
             '("X0" "Y0" "DX" "DY" "PHI" "FX" "FY" "GKS$SW" "MOUT"))

(new-routine "GFA"
             "Fill area"
             '("NP" "PXA" "PYA"))

(new-routine "GGTITM"
             "Get item type from GKSM"
             '("WKID" "TYPE" "LDR"))

(new-routine "GIITM"
             "Interpret item"
             '("TYPE" "LDR" "DATREC"))

(new-routine "GINCH"
             "Initialise choice"
             '("WKID" "CHDNR" "ICHNR" "PET" "XMIN" "XMAX" "YMIN" "YMAX" "IL" "CA"))

(new-routine "GINLC"
             "Initialise locator"
             '("WKID" "LCDNR" "TNR" "IPX" "IPY" "PET" "XMIN" "XMAX" "YMIN" "YMAX" "IL" "CA"))

(new-routine "GINSK"
             "Initialise stroke"
             '("WKID" "SKDNR" "TNR" "MP" "IPX" "IPY" "PET" "XMIN" "XMAX" "YMIN" "YMAX" "BUFLEN" "IL" "CA"))

(new-routine "GINST"
             "Initialise string"
             '("WKID" "STDNR" "ISTR" "PET" "XMIN" "XMAX" "YMIN" "YMAX" "BUFLEN" "INIPOS" "IL" "CA"))

(new-routine "GINVL"
             "Initialise valuator"
             '("WKID" "VLDNR" "IVAL" "PET" "XMIN" "XMAX" "YMIN" "YMAX" "LOVAL" "HIVAL" "IL" "CA"))

(new-routine "GKS_ANNUL"
             "Close graphics workstation without cancelling parameter"
             '("WKID" "STATUS"))

(new-routine "GKS_ASSOC"
             "Associate graphics workstation with parameter and open it"
             '("PARAM" "ACMODE" "WKID" "STATUS"))

(new-routine "GKS_CANCL"
             "Close graphics workstation and cancel parameter"
             '("PARAM" "STATUS"))

(new-routine "GKS_DEACT"
             "Deactivate ADAM GKS after use by an application"
             '("STATUS"))

(new-routine "GKS_ERR"
             "GKS symbolic error code definitions (include file)"
             ( ))

(new-routine "GKS_GSTAT"
             "Inquire if GKS has reported an error"
             '("STATUS"))

(new-routine "GKS_PAR"
             "GKS symbolic constant definitions (include file)"
             ( ))

(new-routine "GMSG"
             "Message"
             '("WKID" "MESS"))

(new-routine "GOPKS"
             "Open GKS"
             '("ERRFIL"))

(new-routine "GOPWK"
             "Open workstation"
             '("WKID" "CONID" "WTYPE"))

(new-routine "GPL"
             "Polyline"
             '("NP" "PXA" "PYA"))

(new-routine "GPM"
             "Polymarker"
             '("NP" "PXA" "PYA"))

(new-routine "GQACWK"
             "Inquire set member of active workstations"
             '("N" "ERRIND" "OL" "WKID"))

(new-routine "GQASF"
             "Inquire aspect source flags"
             '("ERRIND" "LASF"))

(new-routine "GQASWK"
             "Inquire set member of associated workstations"
             '("SGNA" "N" "ERRIND" "OL" "WKID"))

(new-routine "GQCF"
             "Inquire colour facilities"
             '("WTYPE" "ERRIND" "NCOLI" "COLA" "NPCI"))

(new-routine "GQCHH"
             "Inquire character height"
             '("ERRIND" "CHH"))

(new-routine "GQCHS"
             "Inquire choice device state"
             '("WKID" "CHDNR" "MLDR" "ERRIND" "MODE" "ESW" "ICHNR" "PET" "EAREA" "LDR" "DATREC"))

(new-routine "GQCHSP"
             "Inquire character spacing"
             '("ERRIND" "CHSP"))

(new-routine "GQCHUP"
             "Inquire character up vector"
             '("ERRIND" "CHUX" "CHUY"))

(new-routine "GQCHXP"
             "Inquire character expansion factor"
             '("ERRIND" "CHXP"))

(new-routine "GQCLIP"
             "Inquire clipping indicator"
             '("ERRIND" "CLIP"))

(new-routine "GQCNTN"
             "Inquire current normalization transformation number"
             '("ERRIND" "CTNR"))

(new-routine "GQCR"
             "Inquire colour representation"
             '("WKID" "COLI" "GKS$TYPE" "ERRIND" "RED" "GREEN" "BLUE"))

(new-routine "GQDCH"
             "Inquire default choice device state"
             '("WTYPE" "DEVNO" "N" "MLDR" "ERRIND" "MALT" "OL" "PET" "EAREA" "LDR" "DATREC"))

(new-routine "GQDDS"
             "Inquire default deferral state values"
             '("WTYPE" "ERRIND" "DEFMOD" "REGMOD"))

(new-routine "GQDLC"
             "Inquire default locator device data"
             '("WTYPE" "DEVNO" "N" "MLDR" "ERRIND" "DPX" "DPY" "OL" "PET" "EAREA" "LDR" "DATREC"))

(new-routine "GQDSGA"
             "Inquire dynamic modification of segment attributes"
             '("WTYPE" "ERRIND" "SGTR" "VONOFF" "VOFFON" "HIGH" "SGPR" "ADD" "SGDEL"))

(new-routine "GQDSK"
             "Inquire default stroke device data"
             '("WTYPE" "DEVNO" "N" "MLDR" "ERRIND" "DBUFSK" "OL" "PET" "EAREA" "BUFLEN" "LDR" "DATREC"))

(new-routine "GQDST"
             "Inquire default string device data"
             '("WTYPE" "DEVNO" "N" "MLDR" "ERRIND" "MBUFF" "OL" "PET" "EAREA" "BUFLEN" "LDR" "DATREC"))

(new-routine "GQDVL"
             "Inquire default valuator device data"
             '("WTYPE" "DEVNO" "N" "MLDR" "ERRIND" "DVAL" "OL" "PET" "EAREA" "LOVAL" "HIVAL" "LDR" "DATREC"))

(new-routine "GQDWKA"
             "Inquire dynamic modification of workstation attributes"
             '("WTYPE" "ERRIND" "PLBUN" "PMBUN" "TXBUN" "FABUN" "PAREP" "COLREP" "WKTR"))

(new-routine "GQECI"
             "Inquire list element of colour indices"
             '("WKID" "N" "ERRIND" "OL" "COLIND"))

(new-routine "GQEFAI"
             "Inquire list element of fill area indices"
             '("WKID" "N" "ERRIND" "OL" "FAIND"))

(new-routine "GQEGDP"
             "Inquire list element of available generalized drawing primitives"
             '("WTYPE" "N" "ERRIND" "NGDP" "GDPL"))

(new-routine "GQENTN"
             "Inquire list element of normalization transformation numbers"
             '("N" "ERRIND" "OL" "NPRIO"))

(new-routine "GQEPAI"
             "Inquire list element of pattern indices"
             '("WKID" "N" "ERRIND" "OL" "PAIND"))

(new-routine "GQEPLI"
             "Inquire list element of polyline indices"
             '("WKID" "N" "ERRIND" "OL" "PLIND"))

(new-routine "GQEPMI"
             "Inquire list element of polymarker indices"
             '("WKID" "N" "ERRIND" "OL" "PMIND"))

(new-routine "GQETXI"
             "Inquire list element of text indices"
             '("WKID" "N" "ERRIND" "OL" "TXIND"))

(new-routine "GQEWK"
             "Inquire list element of available workstation types"
             '("N" "ERRIND" "NUMBER" "WKTYP"))

(new-routine "GQFACI"
             "Inquire fill area colour index"
             '("ERRIND" "COLI"))

(new-routine "GQFAF"
             "Inquire fill area facilities"
             '("WTYPE" "NI" "NH" "ERRIND" "NIS" "IS" "NHS" "HS" "NPFAI"))

(new-routine "GQFAI"
             "Inquire fill area index"
             '("ERRIND" "INDEX"))

(new-routine "GQFAIS"
             "Inquire fill area interior style"
             '("ERRIND" "GKS$INTS"))

(new-routine "GQFAR"
             "Inquire fill area representation"
             '("WKID" "FAI" "GKS$TYPE" "ERRIND" "STYLE" "STYLID" "COLI"))

(new-routine "GQFASI"
             "Inquire fill area style index"
             '("ERRIND" "STYLI"))

(new-routine "GQGDP"
             "Inquire generalised drawing primitive"
             '("WTYPE" "GDP" "ERRIND" "NBND" "BNDL"))

(new-routine "GQLCS"
             "Inquire locator device state"
             '("WKID" "LCDNR" "GKS$TYPE" "MLDR" "ERRIND" "MODE" "ESW" "ITNR" "ILPX" "ILPY" "PET" "EAREA" "LDR" "DATREC"))

(new-routine "GQLI"
             "Inquire number of available logical input devices"
             '("WTYPE" "ERRIND" "NLCD" "NSKD" "NVLD" "NCHD" "NPCD" "NSTD"))

(new-routine "GQLN"
             "Inquire linetype"
             '("ERRIND" "LTYPE"))

(new-routine "GQLVKS"
             "Inquire level of GKS"
             '("ERRIND" "LEVEL"))

(new-routine "GQLWK"
             "Inquire maximum number of workstation state tables"
             '("WTYPE" "ERRIND" "MPLBTE" "MPMBTE" "MTXBTE" "MFABTE" "MPAI" "MCOLI"))

(new-routine "GQLWSC"
             "Inquire linewidth scale factor"
             '("ERRIND" "LWIDTH"))

(new-routine "GQMDS"
             "Inquire maximum display surface size"
             '("WTYPE" "ERRIND" "DCUNIT" "RX" "RY" "LX" "LY"))

(new-routine "GQMK"
             "Inquire markertype"
             '("ERRIND" "MTYPE"))

(new-routine "GQMKSC"
             "Inquire marker size scale factor"
             '("ERRIND" "MSZSF"))

(new-routine "GQMNTN"
             "Inquire maximum normalisation transformation number"
             '("ERRIND" "MAXTNR"))

(new-routine "GQNT"
             "Inquire normalisation transformation"
             '("NTNR" "ERRIND" "WINDOW" "VIEWPT"))

(new-routine "GQOPS"
             "Inquire operating state value"
             '("OPSTA"))

(new-routine "GQOPSG"
             "Inquire name of open segment"
             '("ERRIND" "SEGNAM"))

(new-routine "GQOPWK"
             "Inquire set member of open workstations"
             '("N" "ERRIND" "OL" "WKID"))

(new-routine "GQPA"
             "Inquire pattern size"
             '("ERRIND" "SZX" "SZY"))

(new-routine "GQPAF"
             "Inquire pattern facilities"
             '("WTYPE" "ERRIND" "NPPAI"))

(new-routine "GQPAR"
             "Inquire pattern representation"
             '("WKID" "PAI" "GKS$TYPE" "NMX" "MMX" "ERRIND" "N" "M" "PARRAY"))

(new-routine "GQPARF"
             "Inquire pattern reference point"
             '("ERRIND" "RFX" "RFY"))

(new-routine "GQPCR"
             "Inquire predefined colour representation"
             '("WTYPE" "PCI" "ERRIND" "RED" "GREEN" "BLUE"))

(new-routine "GQPFAR"
             "Inquire predefined fill area representation"
             '("WTYPE" "PFAI" "ERRIND" "STYLE" "STYLID" "COLI"))

(new-routine "GQPLCI"
             "Inquire polyline colour index"
             '("ERRIND" "COLI"))

(new-routine "GQPLF"
             "Inquire polyline facilities"
             '("WTYPE" "N" "ERRIND" "NLT" "LT" "NLW" "NOMLW" "RLWMIN" "RLWMAX" "NPPLI"))

(new-routine "GQPLI"
             "Inquire plyline index"
             '("ERRIND" "INDEX"))

(new-routine "GQPLR"
             "Inquire polyline representation"
             '("WKID" "PLI" "GKS$TYPE" "ERRIND" "LNTYPE" "LWIDTH" "COLI"))

(new-routine "GQPMCI"
             "Inquire polymarker colour index"
             '("ERRIND" "COLI"))

(new-routine "GQPMF"
             "Inquire polymarker facilities"
             '("WTYPE" "N" "ERRIND" "NMT" "MT" "NMS" "NOMMS" "RMSMIN" "RMSMAX" "NPPMI"))

(new-routine "GQPMI"
             "Inquire polymarker index"
             '("ERRIND" "INDEX"))

(new-routine "GQPMR"
             "Inquire polymarker representation"
             '("WKID" "PMI" "GKS$TYPE" "ERRIND" "MKTYPE" "MKSSCF" "COLI"))

(new-routine "GQPPAR"
             "Inquire predefined pattern representation"
             '("WTYPE" "PPAI" "NMX" "MMX" "ERRIND" "N" "M" "PARRAY"))

(new-routine "GQPPLR"
             "Inquire predefined polyline representation"
             '("WTYPE" "PLI" "ERRIND" "LNTYPE" "LWIDTH" "COLI"))

(new-routine "GQPPMR"
             "Inquire predefined polymarker representation"
             '("WTYPE" "PMI" "ERRIND" "MKTYPE" "MKSSCF" "COLI"))

(new-routine "GQPTXR"
             "Inquire predefined text representation"
             '("WTYPE" "PTXI" "ERRIND" "FONT" "PREC" "CHARXP" "CHARSP" "COLI"))

(new-routine "GQPX"
             "Inquire pixel"
             '("WKID" "PX" "PY" "ERRIND" "COLI"))

(new-routine "GQPXA"
             "Inquire pixel array"
             '("WKID" "PX" "PY" "DX" "DY" "DIMX" "ERRIND" "INVVAL" "COLIA"))

(new-routine "GQPXAD"
             "Inquire pixel array dimensions"
             '("WKID" "PX" "PY" "QX" "QY" "ERRIND" "N" "M"))

(new-routine "GQSGA"
             "Inquire segment attributes"
             '("SGNA" "ERRIND" "SEGTM" "VIS" "HIGH" "SGPR" "DET"))

(new-routine "GQSGP"
             "Inquire number of segment priorities supported"
             '("WTYPE" "ERRIND" "NSG"))

(new-routine "GQSGUS"
             "Inquire set member of segment names in use"
             '("N" "ERRIND" "OL" "SEGNAM"))

(new-routine "GQSGWK"
             "Inquire set member of segment names on workstation"
             '("WKID" "N" "ERRIND" "OL" "SEGNAM"))

(new-routine "GQSKS"
             "Inquire stroke device state"
             '("WKID" "SKDNR" "GKS$TYPE" "N" "MLDR" "ERRIND" "MODE" "ESW" "ITNR" "NO" "PX" "PY" "PET" "EAREA" "BUFLEN" "LDR" "DATREC"))

(new-routine "GQSTS"
             "Inquire string device state"
             '("WKID" "STDNR" "MLDR" "ERRIND" "MODE" "ESW" "LOSTR" "ISTR" "PET" "EAREA" "BUFLEN" "INIPOS" "LDR" "DATREC"))

(new-routine "GQTXAL"
             "Inquire text alignment"
             '("ERRIND" "TXALH" "TXALV"))

(new-routine "GQTXCI"
             "Inquire text colour index"
             '("ERRIND" "COLI"))

(new-routine "GQTXF"
             "Inquire text facilities"
             '("WTYPE" "N" "ERRIND" "NFPP" "FONT" "PREC" "NCHH" "MINCHH" "MAXCHH" "NCHX" "MINCHX" "MAXCHX" "NPTXI"))

(new-routine "GQTXFP"
             "Inquire text font and precision"
             '("ERRIND" "FONT" "PREC"))

(new-routine "GQTXI"
             "Inquire text index"
             '("ERRIND" "INDEX"))

(new-routine "GQTXP"
             "Inquire text path"
             '("ERRIND" "TXP"))

(new-routine "GQTXR"
             "Inquire text representation"
             '("WKID" "TXI" "GKS$TYPE" "ERRIND" "FONT" "PREC" "CHARXP" "CHARSP" "COLI"))

(new-routine "GQTXX"
             "Inquire text extent"
             '("WKID" "PX" "PY" "STR" "ERRIND" "CPX" "CPY" "TXEXPX" "TXEXPY"))

(new-routine "GQVLS"
             "Inquire valuator device state"
             '("WKID" "VLDNR" "MLDR" "ERRIND" "MODE" "ESW" "IVAL" "PET" "EAREA" "LOVAL" "HIVAL" "LDR" "DATREC"))

(new-routine "GQWKC"
             "Inquire workstation connection and type"
             '("WKID" "ERRIND" "CONID" "WTYPE"))

(new-routine "GQWKCA"
             "Inquire workstation category"
             '("WTYPE" "ERRIND" "WKCAT"))

(new-routine "GQWKCL"
             "Inquire workstation classification"
             '("WTYPE" "ERRIND" "VRTYPE"))

(new-routine "GQWKDU"
             "Inquire workstation deferral and update states"
             '("WKID" "ERRIND" "DEFMOD" "REGMOD" "DEMPTY" "NFRAME"))

(new-routine "GQWKM"
             "Inquire workstation maximum numbers"
             '("ERRIND" "MXOPWK" "MXACWK" "MXWKAS"))

(new-routine "GQWKS"
             "Inquire workstation state"
             '("WKID" "ERRIND" "STATE"))

(new-routine "GQWKT"
             "Inquire workstation transformation"
             '("WKID" "ERRIND" "TUS" "RWINDO" "CWINDO" "RVIEWP"))

(new-routine "GRDITM"
             "Read item from GKSM"
             '("WKID" "MLDR" "LDR" "DATREC"))

(new-routine "GRENSG"
             "Rename segment"
             '("OLD" "NEW"))

(new-routine "GRQCH"
             "Request choice"
             '("WKID" "CHDNR" "STAT" "CHNR"))

(new-routine "GRQLC"
             "Request locator"
             '("WKID" "LCDNR" "STAT" "TNR" "PX" "PY"))

(new-routine "GRQSK"
             "Request stroke"
             '("WKID" "SKDNR" "N" "STAT" "TNR" "NP" "PX" "PY"))

(new-routine "GRQST"
             "Request string"
             '("WKID" "STDNR" "STAT" "LOSTR" "STR"))

(new-routine "GRQVL"
             "Request valuator"
             '("WKID" "VLDNR" "STAT" "VAL"))

(new-routine "GRSGWK"
             "Redraw all segments on workstation"
             '("WKID"))

(new-routine "GSASF"
             "Set aspect source flags"
             '("ASFS"))

(new-routine "GSCHH"
             "Set character height"
             '("CHH"))

(new-routine "GSCHM"
             "Set choice mode"
             '("WKID" "IDNR" "GKS$MODE" "GKS$ESW"))

(new-routine "GSCHSP"
             "Set character spacing"
             '("CHSP"))

(new-routine "GSCHUP"
             "Set character up vector"
             '("CHUX" "CHUY"))

(new-routine "GSCHXP"
             "Set character expansion factor"
             '("CHXP"))

(new-routine "GSCLIP"
             "Set clipping indicator"
             '("GKS$CLSW"))

(new-routine "GSCR"
             "Set color representation"
             '("WKID" "COLI" "CR" "CG" "CB"))

(new-routine "GSDS"
             "Set deferral state"
             '("WKID" "GKS$DEFMOD" "GKS$REGMOD"))

(new-routine "GSDTEC"
             "Set detectability"
             '("SGNA" "GKS$DET"))

(new-routine "GSELNT"
             "Set normalisation transformation"
             '("TNR"))

(new-routine "GSFACI"
             "Set fill area colour index"
             '("FACOLI"))

(new-routine "GSFAI"
             "Set fill area index"
             '("FAI"))

(new-routine "GSFAIS"
             "Set fill area interior style"
             '("GKS$INTS"))

(new-routine "GSFAR"
             "Set fill area representation"
             '("WKID" "FAI" "GKS$INTS" "STYLI" "COLI"))

(new-routine "GSFASI"
             "Set fill area style index"
             '("STYLI"))

(new-routine "GSHLIT"
             "Set highlighting"
             '("SGNA" "GKS$HIL"))

(new-routine "GSLCM"
             "Set locator mode"
             '("WKID" "IDNR" "GKS$MODE" "GKS$ESW"))

(new-routine "GSLN"
             "Set linetype"
             '("GKS$LTYPE"))

(new-routine "GSLWSC"
             "Set linewidth scale factor"
             '("LWSC"))

(new-routine "GSMK"
             "Set marker type"
             '("GKS$MTYPE"))

(new-routine "GSMKSC"
             "Set marker size scale factor"
             '("MSZSF"))

(new-routine "GSPA"
             "Set pattern size"
             '("SZX" "SZY"))

(new-routine "GSPAR"
             "Set pattern representation"
             '("WKID" "PAI" "DX" "DY" "DIMX" "COLIA"))

(new-routine "GSPARF"
             "Set pattern reference point"
             '("RFX" "RFY"))

(new-routine "GSPLCI"
             "Set polyline colour index"
             '("PLCOLI"))

(new-routine "GSPLI"
             "Set polyline index"
             '("PLI"))

(new-routine "GSPLR"
             "Set polyline representation"
             '("WKID" "PLI" "GKS$LTYPE" "LWIDTH" "COLI"))

(new-routine "GSPMCI"
             "Set polymarker colour index"
             '("PMCOLI"))

(new-routine "GSPMI"
             "Set polymarker index"
             '("PMI"))

(new-routine "GSPMR"
             "Set polymarker representation"
             '("WKID" "PMI" "GKS$MTYPE" "MSZSF" "COLI"))

(new-routine "GSSGP"
             "Set segment priority"
             '("SGNA" "PRIOR"))

(new-routine "GSSGT"
             "Set segment transformation"
             '("SGNA" "M"))

(new-routine "GSSKM"
             "Set stroke mode"
             '("WKID" "IDNR" "GKS$MODE" "GKS$ESW"))

(new-routine "GSSTM"
             "Set string mode"
             '("WKID" "IDNR" "GKS$MODE" "GKS$ESW"))

(new-routine "GSTXAL"
             "Set text alignment"
             '("GKS$TXAH" "GKS$TXAV"))

(new-routine "GSTXCI"
             "Set text colour index"
             '("TXCOLI"))

(new-routine "GSTXFP"
             "Set text font and precision"
             '("FONT" "GKS$PREC"))

(new-routine "GSTXI"
             "Set text index"
             '("TXI"))

(new-routine "GSTXP"
             "Set text path"
             '("GKS$TXP"))

(new-routine "GSTXR"
             "Set text representation"
             '("WKID" "TXI" "FONT" "GKS$PREC" "CHXP" "CHSP" "COLI"))

(new-routine "GSVIS"
             "Set visibility"
             '("SGNA" "GKS$VIS"))

(new-routine "GSVLM"
             "Set valuator mode"
             '("WKID" "IDNR" "GKS$MODE" "GKS$ESW"))

(new-routine "GSVP"
             "Set viewport"
             '("TRN" "XMIN" "XMAX" "YMIN" "YMAX"))

(new-routine "GSVPIP"
             "Set viewport input priority"
             '("TNR" "RTNR" "GKS$RELPRI"))

(new-routine "GSWKVP"
             "Set workstation viewport"
             '("WKID" "XMIN" "XMAX" "YMIN" "YMAX"))

(new-routine "GSWKWN"
             "Set workstation window"
             '("WKID" "XMIN" "XMAX" "YMIN" "YMAX"))

(new-routine "GSWN"
             "Set window"
             '("TNR" "XMIN" "XMAX" "YMIN" "YMAX"))

(new-routine "GTX"
             "Text"
             '("PX" "PY" "CHARS"))

(new-routine "GUWK"
             "Update workstation"
             '("WKID" "GKS$REGFL"))

(new-routine "GWITM"
             "Write item to GKSM"
             '("WKID" "TYPE" "LDR" "DATREC"))
