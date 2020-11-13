CDF=/apps/netcdf/4.7.0/intel/18.0.5.274
#####################################################################
# compiler options
#####################################################################
#FOPT = -O
#FOPT = -C -warn all
#FOPT = -C
#FOPT = -convert big_endian
#FOPT = -p
FOPT = -O2

F90 = ifort
#F90 = ifort -warn

#####################################################################
#
#####################################################################
opt1 = -Ddo_np
#opt1 = -Ddo_sp

optall = $(opt1) $(opt2) $(opt3) $(opt4)

OBJS = param.o charstrings.o cdf.o variablelist.o grdvar.o stats.o caldata.o runparams.o nsidc.o icestats.o tm_secs_from_bc.o setup_cdf.o write_obs.o

makeit: $(OBJS)
	$(F90) $(FOPT) -o makeit $(OBJS) -L$(CDF)/lib -lnetcdff -lnetcdf

%.o: %.F90
	$(F90) $(FOPT) $(optall) -c -I$(CDF)/include $<
	cpp $(optall) -I$(CDF)/include $*.F90>$*.i

clean:
	/bin/rm -f makeit *.o *.i *.mod
