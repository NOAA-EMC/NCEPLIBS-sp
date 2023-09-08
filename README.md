![Status](https://github.com/NOAA-EMC/NCEPLIBS-sp/workflows/developer/badge.svg)

# NCEPLIBS-sp

The spectral transform library (splib) contains FORTRAN subprograms to
be used for a variety of spectral transform functions. This is part of
the [NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For more detailed documentation see
https://noaa-emc.github.io/NCEPLIBS-sp/

To submit bug reports, feature requests, or other code-related issues including installation and usage questions, please create a [GitHub issue](https://github.com/NOAA-EMC/NCEPLIBS-sp/issues). For general NCEPLIBS inquiries, contact [Edward Hartnett](mailto:edward.hartnett@noaa.gov) (secondary point of contact [Alex Richert](mailto:alexander.richert@noaa.gov)).

## Authors

NCEP/EMC developers.

Code Manager: [Alex Richert](mailto:alexander.richert@noaa.gov), [Edward Hartnett](mailto:edward.hartnett@noaa.gov)

## Installing

```
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install ..
make -j2
make install
```

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.
