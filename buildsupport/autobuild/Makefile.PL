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
	./install-sh -d -m 0755 $(sysconfdir)
	./install-sh -d -m 0755 $(pkgsysconfdir)
	if [ -f $(pkgsysconfdir)/auto-build.conf ]; then \
	  ./install-sh -c -m 0644 conf/auto-build.conf $(pkgsysconfdir)/auto-build.conf-example; \
	  echo "Not overwriting existing config file $(pkgsysconfdir)/auto-build.conf" ;\
	else \
	  ./install-sh -c -m 0644 conf/auto-build.conf $(pkgsysconfdir)/auto-build.conf; \
	fi
	sed -e \'s#BUILDDIR#$(starlink_build)#g\' starlink/build.sh > starlink/build.sh-tmp
	sed -e \'s#BUILDDIR#$(starlink_build)#g\' starlink/buildsupport.sh > starlink/buildsupport.sh-tmp
	sed -e \'s#BUILDDIR#$(starlink_build)#g\' starlink/buildworld.sh > starlink/buildworld.sh-tmp
	sed -e \'s#PREFIX#$(myprefix)#g\' starlink/cronbuild.sh > starlink/cronbuild.sh-tmp
	sed -e \'s#BUILDDIR#$(starlink_build)#;s#TEMPLATES#$(pkgsysconfdir)/templates#;s#PREFIX#$(myprefix)#g\' starlink/autobuild.xslt > starlink/autobuild.xslt-tmp
	sed -e \'s#BUILDDIR#$(starlink_build)#g\' templates/email.txt > templates/email.txt-tmp
	./install-sh -d -m 0755 $(starlink)
	./install-sh -d -m 0755 $(starlink_build)
	./install-sh -d -m 0755 $(starlink_build)/temp
	./install-sh -d -m 0755 $(starlink_build)/build-home
	./install-sh -d -m 0755 $(starlink_build)/build-root
	./install-sh -d -m 0755 $(starlink_build)/build-cache
	./install-sh -d -m 0755 $(starlink_build)/public_html
	./install-sh -d -m 0755 $(starlink_build)/public_html/isos
	./install-sh -d -m 0755 $(starlink_build)/public_html/dist
	./install-sh -d -m 0755 $(starlink_build)/public_html/logs
	./install-sh -d -m 0755 $(starlink_build)/public_ftp
	./install-sh -d -m 0755 $(starlink_build)/packages
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/BUILD
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/RPMS
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/RPMS/noarch
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i386
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i486
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i586
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i686
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/RPMS/i786
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/RPMS/sparc
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/SPECS
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/SOURCES
	./install-sh -d -m 0755 $(starlink_build)/packages/rpm/SRPMS
	./install-sh -d -m 0755 $(starlink_build)/packages/zips
	./install-sh -d -m 0755 $(starlink_build)/packages/tars
	./install-sh -d -m 0755 $(starlink_build)/packages/debian
	./install-sh -d -m 0755 $(starlink_build)/packages/pkg
	./install-sh -d -m 0755 $(starlink_build)/install
	./install-sh -c -m 0755 starlink/build.sh-tmp $(starlink)/build.sh
	./install-sh -c -m 0755 starlink/buildsupport.sh-tmp $(starlink)/buildsupport.sh
	./install-sh -c -m 0755 starlink/buildworld.sh-tmp $(starlink)/buildworld.sh
	./install-sh -c -m 0755 starlink/cronbuild.sh-tmp $(starlink)/cronbuild.sh        
	./install-sh -c -m 0644 starlink/autobuild.xslt-tmp $(starlink)/autobuild.xslt
	rm -f starlink/build.sh-tmp
	rm -f starlink/buildsupport.sh-tmp
	rm -f starlink/buildworld.sh-tmp
	rm -f starlink/autobuild.xslt-tmp
	rm -f starlink/cronbuild.sh-tmp
	sed -e \'s#PREFIX#$(myprefix)#g\' conf/auto-build.cron > conf/auto-build.cron-tmp
	./install-sh -c -m 0644 conf/auto-build.cron-tmp $(pkgsysconfdir)/auto-build.cron;
	rm -f conf/auto-build.cron-tmp
	./install-sh -d -m 0755 $(pkgsysconfdir)/templates
	./install-sh -c -m 0644 templates/email.txt-tmp $(pkgsysconfdir)/templates/email.txt
	./install-sh -c -m 0644 templates/*.html $(pkgsysconfdir)/templates/
	./install-sh -c -m 0644 templates/*.tt $(pkgsysconfdir)/templates/
	./install-sh -c -m 0644 templates/*.css $(pkgsysconfdir)/templates/
	./install-sh -c -m 0644 templates/*.rss $(pkgsysconfdir)/templates/
	rm -f templates/email.txt-tmp
';
  }


__END__
