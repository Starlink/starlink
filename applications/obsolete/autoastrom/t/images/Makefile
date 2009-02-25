
# Run extractor, and get the output in the form which autoastrom requires
%.extractor: %.sdf
	AUTOASTROMTEMPCATALOGUE=$@ extractor \
		$< config=$$AUTOASTROM_DIR//extractor.config >$@.log 2>&1

# Extract the WCS information into a separate dummy NDF
%-justwcs.sdf: %.sdf
	$$KAPPA_DIR/creframe 'lbound=[1,1]' 'ubound=[1,1]' mode=fl mean=0 \
		out=${@:.sdf=}
	$$KAPPA_DIR/wcscopy ndf=${@:.sdf=} like=${<:.sdf=}

# Copy the WCS information in a dummy NDF into a new NDF
%-withwcs.sdf: %-justwcs.sdf
	cp ${@:-withwcs.sdf=.sdf} $@
	$$KAPPA_DIR/wcscopy ndf=${@:.sdf=} like=${<:.sdf=} ok=yes

# Remove all AST information from an NDF (but not FITS cards,
# which are in MORE.FITS)
%-noast.sdf: %.sdf
	cp $< $@
	$$KAPPA_DIR/erase {@:.sdf=}.WCS
