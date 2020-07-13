fc=gfortran
objc= 	mod_param.o\
		mod_variables.o\
		mod_model.o\
		mod_read_write.o\
		mod_allocate_mat_vec.o\
        mod_source.o\
        mod_pml.o\
		mod_geometry.o\
		mod_utilities.o\
		mod_calculate.o\
		mod_derivative.o\
		mod_derivative_compact.o\
		main.o

# for windows 	 
#main.exe:$(objc)
#	$(fc) $(objc) -o main.exe

# for linux
$(info Present configration is for Linux)

main:$(objc)
	$(fc) $(objc) -o main

    
main.o:main.f95
	$(fc) -c main.f95
    
mod_param.o:mod_param.f95
	$(fc) -c $<
    
mod_variables.o:mod_variables.f95
	$(fc) -c $<
    
mod_model.o:mod_model.f95 mod_param.o mod_allocate_mat_vec.o mod_read_write.o mod_utilities.o
	$(fc) -c $<	
    
mod_read_write.o: mod_read_write.f95 	
	$(fc) -c $<
    
mod_allocate_mat_vec.o: mod_allocate_mat_vec.f95 
	$(fc) -c $<
    
mod_source.o:mod_source.f95
	$(fc) -c $<
    
mod_pml.o: mod_pml.f95  mod_param.o  mod_utilities.o
	$(fc) -c $<
    
mod_geometry.o: mod_geometry.f95
	$(fc) -c $<
    
mod_utilities.o: mod_utilities.f95
	$(fc) -c $<
    
mod_calculate.o: mod_calculate.f95 mod_derivative.o mod_derivative_compact.o
	$(fc) -c $<
	
mod_derivative.o: mod_derivative.f95
	$(fc) -c $<
	
mod_derivative_compact.o: mod_derivative_compact.f95
	$(fc) -c $<
    
clean:
	rm *.o *.mod main.exe