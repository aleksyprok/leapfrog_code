# Leapfrog code readme
This Fortran 90 code solves the cold ideal MHD equations using the leapfrog trapezoidal algorithm (see [1]). For more information on how the code works see Manual/manual.pdf.

The makefile is currently setup with the gfortran compiler. To compile, type make, this will output an executable called leapfrog.exe which can be run using ./leapfrog.exe on the terminal.

Note that the command make clean will remove all .o, .mod, .exe files and empty the contents of the Data and Figures directories.

[1] Rickard, G. J., and A. N. Wright, Alfven resonance excitation and fast wave propagation in magnetospheric waveguides, J. Geophys. Res., 99, 13455, 1994.
