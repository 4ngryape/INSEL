all: inselUB

sourcesF := $(wildcard C:/Users/Marcus.Brennenstuhl/Documents/insel.work/eclipse/INSEL/src/*.f)
sourcesC := $(wildcard C:/Users/Marcus.Brennenstuhl/Documents/insel.work/eclipse/INSEL/src/*.cpp)
objectsF := $(patsubst %.f,%.o,$(sourcesF))
objectsC := $(patsubst %.cpp,%.o,$(sourcesC))
objects  := $(patsubst C:/Users/Marcus.Brennenstuhl/Documents/insel.work/eclipse/INSEL/src%,.%,$(objectsF)) $(patsubst C:/Users/Marcus.Brennenstuhl/Documents/insel.work/eclipse/INSEL/src%,.%,$(objectsC))

inselUB:

	@echo Building $@.dll ...
	
# For DEBUG add option -g3 to g++ and gfortran
	gfortran -c -O0 -Wall -g3  \
	-fno-automatic -fno-underscoring -fmessage-length=0 \
	$(sourcesF)
	g++ -g -O0 -Wall -c -fmessage-length=0 \
	$(sourcesC)
	gfortran -shared -g3 -o  "C:/Users/Marcus.Brennenstuhl/Documents/insel.work/inselUB/resources/inselUB.dll" \
	-Wall -L "C:/Users/Marcus.Brennenstuhl/Documents/insel.work/inselUB/resources" -linselTools -linselRefpropHFT \
	$(objects) 


#	-Wall -L "C:/Users/Marcus.Brennenstuhl/Documents/insel.work/inselUB/resources" -inselRefpropHFT \
 
	
	del *.o
 
clean:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     