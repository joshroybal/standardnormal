FC = gfortran
FFLAGS = -O2
LDFLAGS = -static -s
DIRS = obj mod bin

objects = obj/statmodule.o obj/compute.o obj/report.o obj/normal.o
program = bin/normal
all: $(program) $(directories)

$(shell mkdir -p $(DIRS))

obj/%.o: src/%.f95
	$(FC) $(FFLAGS) -Imod -Jmod -c $< -o $@

$(program): $(objects)
	$(FC) $(LDFLAGS) $^ -o $@

.PHONY : clean
clean :
	$(RM) $(program) obj/*.o mod/*.mod src/*~
