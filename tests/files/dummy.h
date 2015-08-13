#ifndef DUMMY_H
#define DUMMY_H

typedef int RidiculousIntType;

//func pointer type that returns int
typedef int (*FunctionPointerTypeA)(int);

//func pointer type that returns func pointer, with two forms
typedef FunctionPointerTypeA (*FunctionPointerTypeB)(int);
//typedef int (*(*FunctionPointerTypeC(int)))(int);

//func pointer global
int (*sillyGlobalFunctionPointerA)(int);
//using the typedef'd func pointer type
FunctionPointerTypeA sillyGlobalFunctionPointerB;

#endif
