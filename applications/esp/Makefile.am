## Process this file with automake to produce Makefile.in

bin_SCRIPTS = esp_link_adam esp.csh esp.sh

bin_MONOLITHS = esp_mon
esp_mon_TASKS = corr ellfou ellpro esphelp fastmed \
    gaufit graphs histpeak hsub loback mask \
    mixup sector selfc selfcw skew topped

esp_mon_SOURCES = \
	esp_mon.f \
	$(esp_mon_TASKS:=.f) \
	$(F_SUB1) \
	d1mach.fpp
esp_mon_LDADD = `$(srcdir)/esp_link_adam`
##esp_mon_AM_FCFLAGS = -fno-globals

F_SUB1 = \
	ards.f dnsg.f \
        esp1_asffr.f esp1_crpt.f esp1_dsfrm.f esp1_gtpos.f esp1_gtpsz.f \
        esp1_inpos.f esp1_pr2s.f esp1_s2pr.f esp1_xyfmt.f esp1_farr.f \
        elp1_filer.f elp1_texto.f elp1_cato.f \
        kpg1_pgesc.f esp1_vers.f

