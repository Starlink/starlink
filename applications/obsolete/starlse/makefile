TPU_SOURCE = align_comment.tpu comment.tpu detach.tpu do_cli.tpu \
fix_continuation.tpu flush_buffers.tpu generic.tpu getenv.tpu \
goto_previous_buffer.tpu help.tpu key_procedures.tpu post_key.tpu \
pre_key.tpu print.tpu show_buffer.tpu sort.tpu \
starlink_fortran_package.tpu starlse_initialisation.tpu \
update_default.tpu where.tpu

LSE_SOURCE = adam_constructs.lse agi.lse ary.lse build.lse \
chr.lse ems.lse err.lse fio.lse gks.lse grp.lse gwm.lse hds.lse \
idi.lse ifl.lse key_definitions.lse mag.lse msg.lse ndf.lse par.lse \
pgplot.lse primdat.lse psx.lse sgs.lse slalib.lse \
starlink_fortran_a.lse starlink_fortran_adjust.lse \
starlink_fortran_alias.lse starlink_fortran_b.lse trn.lse

UNIX_RELEASE = $(RELEASE)/unix_starlse

unix_release: $(RELEASE) $(UNIX_RELEASE) \
              $(UNIX_RELEASE)/starlse_source.tar \
              $(UNIX_RELEASE)/mk \
              $(UNIX_RELEASE)/makefile \
              $(UNIX_RELEASE)/sun105.tex \
              $(UNIX_RELEASE)/starlse.news

$(UNIX_RELEASE)/starlse_source.tar: $(TPU_SOURCE) $(LSE_SOURCE)
	tar cvf $(UNIX_RELEASE)/starlse_source.tar $(TPU_SOURCE) $(LSE_SOURCE)

$(UNIX_RELEASE)/mk:           unix_mk;       cp -p $? $@
$(UNIX_RELEASE)/makefile:     unix_makefile; cp -p $? $@
$(UNIX_RELEASE)/sun105.tex:   sun105.tex;    cp -p $? $@
$(UNIX_RELEASE)/starlse.news: starlse.news;  cp -p $? $@

$(RELEASE) $(UNIX_RELEASE):
	mkdir $@
