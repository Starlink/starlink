stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

sun273.pdf sun273.htx_tar: ../atools_mon.tex

../atools_mon.tex: $(wildcard ../ast*.f)
	make -C .. $(notdir $@)
