R --vanilla < cyrils_lib_make_version.R
R --vanilla < run-roxygen.R

R CMD build --force cyrils
R CMD INSTALL --build cyrils

del cyrils.html
rm cyrils.html



