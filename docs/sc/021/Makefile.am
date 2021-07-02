## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sc21_selected_params.tex

sc21.pdf: $(LATEX_SOURCES)

LATEX_SOURCES = sc21_app_clean.tex sc21_calib.tex sc21_cog.tex \
                sc21_data_files.tex sc21_dimm.tex sc21_examples.tex \
                sc21_fcfsred.tex sc21_fcfstime.tex sc21_fits.tex \
                sc21_intro.tex sc21_mapstats.tex sc21_matchfilter.tex \
                sc21_param_categories.tex sc21_pipeline.tex \
                sc21_postprocess.tex sc21_scuba2_overview.tex \
                sc21_tweak.tex sc21_using_smurf.tex

CLEANFILES = sc21.htx_tar sc21.ps sc21.lof sc21.lot sc21.toc
