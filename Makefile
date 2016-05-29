# Markov Chain Estimator - Victhor Simões Sartório

FC=gfortran
FF=-O2 -march=native -Wall -Wextra -Wpedantic

OBJS=arguments.o file_handler.o sorting.o reporter.o estimator.o main.o

# Build system

all: mce

mce: $(OBJS)
	$(FC) $(FF) -o mce $(OBJS)

main.o: main.f95 file_handler.mod estimator.mod reporter.mod
	$(FC) $(FF) -c main.f95

estimator.mod: estimator.o

estimator.o: estimator.f95 file_handler.mod sorting.mod
	$(FC) $(FF) -c estimator.f95

reporter.mod: reporter.o

reporter.o: reporter.f95 estimator.mod file_handler.mod
	$(FC) $(FF) -c reporter.f95

sorting.mod: sorting.o

sorting.o: sorting.f95
	$(FC) $(FF) -c sorting.f95

file_handler.mod: file_handler.o

file_handler.o: file_handler.f95 arguments.mod
	$(FC) $(FF) -c file_handler.f95

arguments.mod: arguments.o

arguments.o: arguments.f95
	$(FC) $(FF) -c arguments.f95

# Utilities

rebuild: clean all

clean:
	rm -f *.o *.mod

test: mce
	./mce markov.dat
