USEDLIBS = -lclangFrontend -lclangSerialization -lclangDriver \
		   -lclangTooling -lclangParse -lclangSema -lclangAnalysis \
			-lclangEdit \
		   -lclangAST -lclangLex -lclangBasic
#		   -lclangRewriteFrontend -lclangRewriteCore 

LLVMCONFIG = /scratch/m4burns/llvm/bin/llvm-config
CXX = /scratch/m4burns/llvm/bin/clang++

all: aodb.so c_compiler.o ss_env

aodb.so: aodb.c
	gcc -Wall -fPIC -shared -O2 -g aodb.c -o aodb.so -lrt

c_compiler.o: c_compiler.cpp
	$(CXX) `$(LLVMCONFIG) --cxxflags` -o c_compiler.o -c c_compiler.cpp

c_compiler: c_compiler.cpp
	$(CXX) `$(LLVMCONFIG) --cxxflags` -DTEST -o c_compiler.o -c c_compiler.cpp
	$(CXX) `$(LLVMCONFIG) --ldflags` -o c_compiler c_compiler.o $(USEDLIBS) `$(LLVMCONFIG) --libs`

ss_env: ss_env.c
	gcc -Wall -O2 -D_FORTIFY_SOURCE=2 -g ss_env.c -o ss_env

clean:
	rm -fr aodb.so compiled ss_env c_compiler.o c_compiler

.PHONY: all clean
