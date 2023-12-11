FC=gfortran

$FC -c kinds.F90
#$FC -c model_size.f90
#$FC -c field.f90
#$FC -c MPIinitialize.f90
$FC -c constants.F90
#$FC -c ncdf_wrapper.f90 `nf-config --fflags --flibs`
#$FC -c configurationMod.f90
#$FC -c gridmodule.f90 
#$FC -c data_read.f90
#$FC -c FieldCollection.f90 
#$FC -c read_write.f90
$FC -c operators.F90
$FC -c filterModule.F90
$FC -c weight_objects.F90
$FC -c test.F90
#$FC -c parallel_filter.f90 
#$FC -c main.f90 

#$FC kinds.o field.o model_size.o constants.o MPIinitialize.o ncdf_wrapper.o configurationMod.o gridmodule.o data_read.o FieldCollection.o read_write.o Operators.o parallel_filter.o main.o -o main.exe `nf-config --fflags --flibs`


#rm -f *.o *.mod
