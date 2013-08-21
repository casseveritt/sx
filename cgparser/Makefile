CC=g++
OBJS=atom.o binding.o cgcmain.o cgstruct.o check.o compile.o constfold.o cpp.o generic_hal.o glslesv_hal.o hal.o inline.o memory.o parser.o printutils.o scanner.o semantic.o stdlib.o support.o support_iter.o symbols.o tokens.o

TOKENIZE_OBJS=atom.o binding.o cgstruct.o check.o compile.o constfold.o cpp.o generic_hal.o hal.o inline.o memory.o parser.o printutils.o scanner.o semantic.o support.o support_iter.o symbols.o tokens.o tokenize.o

.cpp.o:
	$(CC) -g -c $<

sx: $(OBJS)
	$(CC) -g -o $@ $^

tokenize: $(TOKENIZE_OBJS)
	$(CC) -g -o $@ $^
	
stdlib.cpp: stdlib.cg tokenize
	./tokenize stdlib.cg >stdlib.cpp

parser.cpp parser.h: parser.y
	bison parser.y --defines=parser.h --output=parser.cpp --verbose

clean:
	rm -f *.o sx tokenize stdlib.cpp
