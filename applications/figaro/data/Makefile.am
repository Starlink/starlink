## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = $(DATA_STRUCTURE_DEFINITIONS)

bindatadir = $(bindir)
dist_bindata_DATA = $(DATA_FILES) $(LINE_IDS) $(DEMO_FLUX_STANDARDS)

dist_staretc_DATA = $(MISC_FILES) $(ARCLINES) $(EXTINCTION_COEFFS) \
               $(FILTER_TRANSMISSIONS) $(FLUX_STANDARDS) $(LUTS) \
               $(MENUS) $(MISC_FILES)

# Lookup tables and demonstration data files.
DATA_FILES = demo.arc demo.sdf standard_lut.sdf test_data.sdf \
test_sdist.sdf test_stand.sdf twodspec_defaults.sdf

# DSA data-structure definitions.   See Figaro Programmers' Guide.
# Have moved spectrum.def from etc/figaro to bin/figaro. 
DATA_STRUCTURE_DEFINITIONS = continuum.def ext.def res.def \
spectrum.def spiketrum.def

# Flux standards used in demos.
DEMO_FLUX_STANDARDS = g158m100.tab hd84937.tab

# Spectral-line identifications, mostly arcs.
LINE_IDS = absorption.ids cuar.ids emission.ids h2.ids helium.ids \
krypton_ir.ids mercury.ids neon.ids redlines.ids sky.ids thor.ids \
uv.ids xenon_ir.ids


# Wavelengths of arc lines.
ARCLINES = argon.arc cgs4_argon.arc cgs4_krypton.arc cgs4_oh.arc \
cgs4_xenon.arc hc.arc helium.arc iron.arc neon.arc thar.arc 

# Extinction coefficients as function of wavelength for observatories.
EXTINCTION_COEFFS = aaoext.tab extin.tab palomar.tab

# AAO filter transmissions versus wavelength.
FILTER_TRANSMISSIONS = aaond10.tab aaond120.tab aaond13.tab \
aaond146.tab aaond16.tab aaond178.tab aaond18.tab aaond210.tab \
aaond21.tab aaond245.tab aaond26.tab aaond30.tab aaond34.tab \
aaond5.tab aaond62.tab aaond7.tab aaond88.tab

# Flux standards miostly from Oke and Hiltner.
FLUX_STANDARDS = 40eriba.tab 40erib.tab agd185a.tab agd185.tab \
al970m30a.tab al970m30.tab bd174708a.tab bd174708.tab bd262606a.tab \
bd262606.tab bdp253941a.tab bdp253941.tab bdp284211a.tab bdp284211.tab \
bdp332642a.tab bdp332642.tab bdp404032a.tab bdp404032.tab \
bdp82015a.tab bdp82015.tab bgd185a.tab bgd185.tab bl970m30a.tab \
bl970m30.tab echselect.txt f110a.tab f110.tab f15a.tab f15.tab \
f24a.tab f24.tab f25a.tab f25.tab f34a.tab f34.tab f56a.tab f56.tab \
f92a.tab f92.tab f98a.tab f98.tab g138m31a.tab g138m31.tab \
g158m100a.tab g158m100.tab g191b2ba.tab g191b2b.tab g47m18a.tab \
g47m18.tab g9937a.tab g9937.tab gauss.txt gd140a.tab gd140.tab \
gd190a.tab gd190.tab gdm248a.tab gdm248.tab grwp705824a.tab \
grwp705824.tab grwp708247a.tab grwp708247.tab grwp738031a.tab \
grwp738031.tab hd140283a.tab hd140283.tab hd19445a.tab hd19445.tab \
hd84937a.tab hd84937.tab he3a.tab he3.tab hil102a.tab hil600a.tab \
hil600.tab hz14a.tab hz14.tab hz15a.tab hz15.tab hz21a.tab hz21.tab \
hz29a.tab hz29.tab hz2a.tab hz2.tab hz43a.tab hz43.tab hz44a.tab \
hz44.tab hz4a.tab hz4.tab hz7a.tab hz7.tab kmags.dat kop27a.tab \
kop27.tab l1363m3a.tab l1363m3.tab l1512m34ba.tab l1512m34b.tab \
l1573m31a.tab l1573m31.tab l74546a.tab l74546.tab l745m46aa.tab \
l745m46a.tab l8702a.tab l8702.tab l870m2a.tab l870m2.tab l93080a.tab \
l93080.tab l930m80a.tab l930m80.tab l97030a.tab l97030.tab lb1240a.tab \
lb1240.tab lb227a.tab lb227.tab lds235a.tab lds235ba.tab lds235b.tab \
lds235.tab lds749ba.tab lds749b.tab lpoext.tab ltt4816a.tab \
ltt4816.tab ltt7379a.tab ltt7379.tab r627a.tab r627.tab ross640a.tab \
ross640.tab sa29m130a.tab sa29m130.tab ton573a.tab ton573.tab \
vma2a.tab vma2.tab w1346a.tab w1346.tab w485aa.tab w485a.tab w485.tab

# Colour tables.
LUTS = contour_lut.sdf false_lut.sdf grjt_lut.sdf iras_lut.sdf \
jos_lut.sdf ndpn_lut.sdf ndppal_lut.sdf ndpredsh_lut.sdf \
ndprnbow_lut.sdf ndps_lut.sdf pandora_lut.sdf standard_lut.sdf

MENUS = arcmenu.txt echarcmenu.txt

MISC_FILES = abline.txt arcparm.txt echselect.txt gauss.txt kmags.dat


