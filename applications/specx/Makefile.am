## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@


dist_pkgdata_DATA = $(RUNTIME_FILES) $(EXAMPLES_FILES)


# These files are needed for specx to run
# specx_welcome.txt needs to have PACKAGE_VERSION inserted.
# Probably easiest to move it to cwd
RUNTIME_FILES = misc/graphcap.txt misc/init.spx \
misc/specx_init.dat misc/specx_welcome.txt

# These are the actual specx command macros
# Currently should be installed into $SPECX_COMMAND/
COMFILES = comfiles/baseline.spx comfiles/image.spx		     \
comfiles/specx_to_dbmem.spx comfiles/convert.spx comfiles/map_av.spx \
comfiles/stack.spx comfiles/fetch.spx comfiles/newton_root.spx	     \
comfiles/st.spx comfiles/fiddle.spx comfiles/rddualgsd.spx	     \
comfiles/tofits.spx comfiles/frqfix.spx comfiles/rdwidesp.spx	     \
comfiles/tsky.spx comfiles/fsw.spx comfiles/readall.spx		     \
comfiles/unaverage.spx comfiles/gsd2fits.spx comfiles/showastro.spx  \
comfiles/vertical.spx comfiles/helpfits.spx comfiles/source.spx	     \
comfiles/vms_demo.spx



# These are example map and spectra
EXAMPLES_FILES = data/l483cmap.map data/l483core.dat \
data/obs_das_0011.dat data/scan_0043.dat

# Include files are specified here as noinst so they
# are included in a dist bundle
noinst_HEADERS = $(PRIVATE_INCLUDES)

PRIVATE_INCLUDES = inc/COMMAND_TABLE inc/FILHDI2 inc/MAPS inc/PLOTPAR1 \
inc/SYMBOLS inc/CUBE inc/FLAGCOMM inc/MAPTITLES inc/PROTOTYPE	       \
inc/WEIGHTS inc/DOPPLER inc/HEADER inc/MAPV4 inc/SPECX_FITS inc/ERRORS \
inc/IODATA inc/NEWXY inc/SPECX_PARS inc/FILES inc/JPI inc/NOKEEP       \
inc/STACKCOMM inc/FILHD inc/MAPHD inc/PLOT2D inc/STAKPAR
