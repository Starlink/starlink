import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
      name='starlink-astviewer',
      version='1.1.0',
      packages=['astviewer'],
      package_data={'':['*.html','*.png']},
      eager_resources=['astviewer/astviewer.html','astviewer/example-frameset.png'],
      description='A utility for exploring Starlink AST FrameSet objects',
      long_description=long_description,
      long_description_content_type="text/markdown",
      author='David S. Berry',
      author_email='davidstuartberry@gmail.com',
      url="https://github.com/dsberry/astviewer",
      scripts=['astviewer/astviewer'],
      install_requires=['starlink-pyast>=3.11.0','numpy', 'pyqt5'],
      classifiers=[
          'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
          'Programming Language :: Python',
          'Topic :: Scientific/Engineering :: Astronomy'
      ])
