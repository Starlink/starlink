## Process this file with automake to produce Makefile.in

## Scripts which end up in the binary installation directory.

stardocs_DATA = analysis_lis  docs_lis  mud_lis  subject_lis news
pkgdata_DATA = appform.ps ssi support INFO_CONDITIONS

package-info.wiki: package-info.csv
	rm -f $@
	awk '-F	' '/^\$$/{printf "\n%s\n",$$1};/^[a-zA-Z]/{printf "|  %s  |  %s  |  %s  | %s  | %s  | %s  |\n", $$1,$$2,$$3,$$4,$$5,$$6}' $< >$@
