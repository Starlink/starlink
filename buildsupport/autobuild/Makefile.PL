# $Id$
# -*- perl -*-

use ExtUtils::MakeMaker;

WriteMakefile(
              NAME => "Test-AutoBuild",
              VERSION_FROM => 'lib/Test/AutoBuild.pm',
	      AUTHOR => "Daniel Berrange (dan[at]berrange[dot]com)",
              EXE_FILES => ['bin/auto-build.pl'],
              PREREQ_PM => {
                 'BSD::Resource' => "1.10",
                 'IO::Scalar' => "0",
                 'Net::SMTP' => "2.15",
                 'File::Copy' => '0',
                 'File::Find' => '0',
                 'File::Path' => '0',
                 'Carp' => '0',
                 'Sys::Hostname' => '0',
                 'Digest::MD5' => '0',
                 'Fcntl' => '0',
                 'Config::Record' => '1.0.3',
                 'Template' => '0',
                 'POSIX' => '0'
              },
              dist =>
              {
                  COMPRESS => 'gzip --force --best',
              },

              clean =>
              {
                  FILES => '*.bak *~',
              },

              depend => {
                  Makefile => '$(VERSION_FROM)',
                  Test-AutoBuild.spec => '$(VERSION_FROM)',
                  dist => 'Test-AutoBuild.spec',
              },

              realclean => {
                  FILES => 'Test-AutoBuild-*.tar.gz Test-AutoBuild.spec',
              },
              );

package MY;

sub libscan
{
    my ($self, $path) = @_;
    ($path =~ /\~$/) ? undef : $path;
}


sub postamble
  {
    '
sysconfdir      = $(PREFIX)/etc
pkgsysconfdir   = $(sysconfdir)/auto-build.d
starlink_build  = $(PREFIX)/build
starlink        = $(PREFIX)/starlink
myprefix        = $(PREFIX)

install ::
	if [ -f $(pkgsysconfdir)/auto-build.conf ]; then \
	  install -c -D -m 0644 conf/auto-build.conf $(pkgsysconfdir)/auto-build.conf-example; \
	  echo "Not overwriting existing config file $(pkgsysconfdir)/auto-build.conf" ;\
	else \
	  install -c -D -m 0644 conf/auto-build.conf $(pkgsysconfdir)/auto-build.conf; \
	fi
	sed -e \'s#BUILDDIR#$(starlink_build)#g\' starlink/build.sh > starlink/build.sh-tmp
	sed -e \'s#BUILDDIR#$(starlink_build)#g\' starlink/buildsupport.sh > starlink/buildsupport.sh-tmp
	sed -e \'s#BUILDDIR#$(starlink_build)#g\' starlink/buildworld.sh > starlink/buildworld.sh-tmp
	sed -e \'s#PREFIX#$(myprefix)#g\' starlink/cronbuild.sh > starlink/cronbuild.sh-tmp
	sed -e \'s#BUILDDIR#$(starlink_build)#;s#TEMPLATES#$(pkgsysconfdir)/templates#;s#PREFIX#$(myprefix)#g\' starlink/autobuild.xslt > starlink/autobuild.xslt-tmp
	install -d -m 0755 $(starlink)
	install -d -m 0755 $(starlink_build)
	install -d -m 0755 $(starlink_build)/temp
	install -d -m 0755 $(starlink_build)/build-home
	install -d -m 0755 $(starlink_build)/build-root
	install -d -m 0755 $(starlink_build)/build-cache
	install -d -m 0755 $(starlink_build)/public_html
	install -d -m 0755 $(starlink_build)/public_html/isos
	install -d -m 0755 $(starlink_build)/public_html/dist
	install -d -m 0755 $(starlink_build)/public_html/logs
	install -d -m 0755 $(starlink_build)/public_ftp
	install -d -m 0755 $(starlink_build)/packages
	install -d -m 0755 $(starlink_build)/packages/rpm
	install -d -m 0755 $(starlink_build)/packages/rpm/BUILD
	install -d -m 0755 $(starlink_build)/packages/rpm/RPMS
	install -d -m 0755 $(starlink_build)/packages/rpm/RPMS/noarch
	install -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i386
	install -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i486
	install -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i586
	install -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i686
	install -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i786
	install -d -m 0755 $(starlink_build)/packages/rpm/RPMS/sparc
	install -d -m 0755 $(starlink_build)/packages/rpm/SPECS
	install -d -m 0755 $(starlink_build)/packages/rpm/SOURCES
	install -d -m 0755 $(starlink_build)/packages/rpm/SRPMS
	install -d -m 0755 $(starlink_build)/packages/zips
	install -d -m 0755 $(starlink_build)/packages/tars
	install -d -m 0755 $(starlink_build)/packages/debian
	install -d -m 0755 $(starlink_build)/packages/pkg
	install -d -m 0755 $(starlink_build)/install
	install -c -m 0755 starlink/build.sh-tmp $(starlink)/build.sh
	install -c -m 0755 starlink/buildsupport.sh-tmp $(starlink)/buildsupport.sh
	install -c -m 0755 starlink/buildworld.sh-tmp $(starlink)/buildworld.sh
	install -c -m 0755 starlink/cronbuild.sh-tmp $(starlink)/cronbuild.sh        
	install -c -m 0644 starlink/autobuild.xslt-tmp $(starlink)/autobuild.xslt
	rm -f starlink/build.sh-tmp
	rm -f starlink/buildsupport.sh-tmp
	rm -f starlink/buildworld.sh-tmp
	rm -f starlink/autobuild.xslt-tmp
	rm -f starlink/cronbuild.sh-tmp
	sed -e \'s#PREFIX#$(myprefix)#g\' conf/auto-build.cron > conf/auto-build.cron-tmp
	install -c -D -m 0644 conf/auto-build.cron-tmp $(pkgsysconfdir)/auto-build.cron;
	rm -f conf/auto-build.cron-tmp
	install -d -m 0755 $(pkgsysconfdir)/templates
	install -c -m 0644 templates/*.txt $(pkgsysconfdir)/templates/
	install -c -m 0644 templates/*.html $(pkgsysconfdir)/templates/
	install -c -m 0644 templates/*.tt $(pkgsysconfdir)/templates/
	install -c -m 0644 templates/*.css $(pkgsysconfdir)/templates/
	install -c -m 0644 templates/*.rss $(pkgsysconfdir)/templates/
';
  }


__END__
