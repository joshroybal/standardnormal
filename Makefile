FC = gfortran
FFLAGS = -O2
LDFLAGS = -s
DIRS = obj mod bin

module = obj/statmodule.o
objects = obj/statmodule.o obj/compute.o obj/report.o obj/normal.o
program = bin/normal
all: $(program)

$(shell mkdir -p $(DIRS))

$(module): src/statmodule.f95
	$(FC) $(FFLAGS) -Jmod -c $< -o $@

obj/compute.o: src/compute.f95 $(module)
	$(FC) $(FFLAGS) -Imod -c $< -o $@

obj/report.o: src/report.f95 $(module)
	$(FC) $(FFLAGS) -Imod -c $< -o $@

obj/normal.o: src/normal.f95 $(module)
	$(FC) $(FFLAGS) -Imod -c $< -o $@

$(program): $(objects)
	$(FC) $(LDFLAGS) $^ -o $@

.PHONY : clean
clean :
	# $(RM) $(program) obj/*.o mod/*.mod src/*~
	$(RM) -rf $(DIRS)
