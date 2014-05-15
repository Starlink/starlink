## Process this file with automake to produce Makefile.in

bin_SCRIPTS = polmap.csh polmap.mtd

dist_noinst_SCRIPTS = polmap_link
dist_pkgdata_DATA = sn1987a.sdf test.cmd
dist_starnews_DATA = polmap.news

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

bin_PROGRAMS = polmap

polmap_SOURCES = main.f $(F_ROUTINES)
polmap_LDADD = `$(srcdir)/polmap_link`

noinst_INCLUDES = ARRAY_SIZE

F_ROUTINES = $(F_SUB1) $(F_SUB2) $(F_SUB3)

F_SUB1 = get_cparam.f get_errors.f get_param.f get_paramd.f rdalas.f \
rdstk.f rdtsp.f read_it.f read_stk.f wr_error.f wralas.f write_it.f \
write_stk.f wrstk.f wrtsp.f pwrite.f

F_SUB2 = arrowstyle.f cursor.f cwrange.f drawline.f iplot.f quplot.f \
qupoint.f triplot.f triplot_iface.f

F_SUB3 = begin_plot.f boxsmooth.f chopw.f cirange.f cmath.f const_bin.f \
contdef.f contfit.f contlist.f cpfrange.f cprange.f cremove.f ctrange.f \
determ.f edit.f fit_pa.f font.f get.f get_limits.f help.f icadd.f \
icmult.f integ.f iquadd.f ls.f isfit2.f linterp.f list.f locate.f \
lsfun2.f madd.f marquardt.f maxpol.f mean_counts.f merge.f motd.f \
pa_calib.f parse.f pmath.f polfit.f pol_sch.f ptheta.f put.f qsm.f \
quswap.f regrid.f retitle.f rotpa.f rtitle.f rv.f sdelete.f serkthru.f \
setcolour.f setline.f setsymb.f set_paper.f sindex.f smooth.f sort.f \
srange.f ssort.f sstrip.f swap.f tappend.f text.f tolowerc.f tov.f \
tow.f tpoly.f tri_const_bin.f wmerge.f xadd.f xgrid.f
