## Process this file with automake to produce Makefile.in

LN = @LN_S@

TESTS = exampl tcnqck tconan tconre tdashp tgrida tisohr tpwrtx tpwrzs \
tthree tcnsmt tconaq tdashc tdashs thafto tisosr tpwry tpwrzt   \
tvelvc tautog tcnsup tconas tdashl tezmap thstgr tlocal tpwrzi tsrfac

check_PROGRAMS = exampl tcnqck tconan tconre tdashp tgrida tisohr     \
tpwrtx tpwrzs tthree tcnsmt tconaq tdashc tdashs thafto tisosr \
tpwry tpwrzt tvelvc tautog tcnsup tconas tdashl tezmap thstgr tlocal  \
tpwrzi tsrfac


exampl_SOURCES = exampl.f
exampl_LDADD = ../libncar.la ../libncar_dashsupr.la ../libncar_conpms.la `gks_link` `sgs_link`

# These examples do not really compile well since ncarex needs to be 
# edited for each example and then you need to know the GKS workstation
# number (3800 seems to be xwindows). 
# If _FCFLAGS worked it would at least compile without the hack

tautog_SOURCES = tautog.f ncarex_tautog.fpp
tautog_LDADD = ../libncar.la `gks_link`
tautog_FCFLAGS = -DUSE_TAUTOG

# Hack to make the source compile with different FCFLAGS so that
# a different example can be called each time

ncarex_tautog.fpp: ncarex.fpp
	$(LN_S) ncarex.fpp ncarex_tautog.fpp

ncarex_tautog.o: ncarex_tautog.fpp
	$(FCOMPILE) -DUSE_TAUTOG -c -o $@ $<

tcnqck_SOURCES = tcnqck.f ncarex.fpp
tcnqck_LDADD = ../libncar.la `gks_link`

tcnsmt_SOURCES = tcnsmt.f ncarex.fpp
tcnsmt_LDADD = ../libncar.la `gks_link`

tcnsup_SOURCES = tcnsup.f ncarex.fpp
tcnsup_LDADD = ../libncar.la `gks_link`

tconan_SOURCES = tconan.f ncarex.fpp
tconan_LDADD = ../libncar.la `gks_link`

tconaq_SOURCES = tconaq.f ncarex.fpp
tconaq_LDADD = ../libncar.la `gks_link`

tconas_SOURCES = tconas.f ncarex.fpp
tconas_LDADD = ../libncar.la `gks_link`

tconre_SOURCES = tconre.f ncarex.fpp
tconre_LDADD = ../libncar.la `gks_link`

tdashc_SOURCES = tdashc.f ncarex.fpp
tdashc_LDADD = ../libncar.la `gks_link`

tdashl_SOURCES = tdashl.f ncarex.fpp
tdashl_LDADD = ../libncar.la `gks_link`

tdashp_SOURCES = tdashp.f ncarex.fpp
tdashp_LDADD = ../libncar.la `gks_link`

tdashs_SOURCES = tdashs.f ncarex.fpp
tdashs_LDADD = ../libncar.la `gks_link`

tezmap_SOURCES = tezmap.f ncarex.fpp
tezmap_LDADD = ../libncar.la `gks_link`

tgrida_SOURCES = tgrida.f ncarex.fpp
tgrida_LDADD = ../libncar.la `gks_link`

thafto_SOURCES = thafto.f ncarex.fpp
thafto_LDADD = ../libncar.la `gks_link`

thstgr_SOURCES = thstgr.f ncarex.fpp
thstgr_LDADD = ../libncar.la `gks_link`

tisohr_SOURCES = tisohr.f ncarex.fpp
tisohr_LDADD = ../libncar.la `gks_link`

tisosr_SOURCES = tisosr.f ncarex.fpp
tisosr_LDADD = ../libncar.la `gks_link`

tlocal_SOURCES = tlocal.f
tlocal_LDADD = ../libncar.la `gks_link`

tpwrtx_SOURCES = tpwrtx.f ncarex.fpp
tpwrtx_LDADD = ../libncar.la `gks_link`

tpwry_SOURCES = tpwry.f ncarex.fpp
tpwry_LDADD = ../libncar.la `gks_link`

tpwrzi_SOURCES = tpwrzi.f ncarex.fpp
tpwrzi_LDADD = ../libncar.la `gks_link`

tpwrzs_SOURCES = tpwrzs.f ncarex.fpp
tpwrzs_LDADD = ../libncar.la `gks_link`

tpwrzt_SOURCES = tpwrzt.f ncarex.fpp
tpwrzt_LDADD = ../libncar.la `gks_link`

tsrfac_SOURCES = tsrfac.f ncarex.fpp
tsrfac_LDADD = ../libncar.la `gks_link`

tthree_SOURCES = tthree.f ncarex.fpp
tthree_LDADD = ../libncar.la `gks_link`

tvelvc_SOURCES = tvelvc.f ncarex.fpp
tvelvc_LDADD = ../libncar.la `gks_link`
