# NOTICE
MLang has been in development on and off privately for the past year as a standalone parser, written in Python, to get the general idea of how the language should work  
This repository will contain the actual implementations of MLang, starting with the Lua GMod addon  

Note that most of this readme talks about future features and plans as though they're implemented, MLang is not currently usable but has been thought out and tested in the standalone implementation mentioned above  

The purpose of making this repo public early is to get feedback on the non-prototype version of MLang before everything is set in stone  

# MLang

MLang (or Multi Language) is a statically typed programming language designed to be easily extendable, embeddable, and familiar to existing programmers while simultaneously being easy for novices.  
Note that this is not meant to be a full blown widely supported language, instead it's an improved alternative to Expression 2 and StarfallEx with as much future proofing as possible.  

*The name comes from the fact it's a mix of ideas and paradigms of various other languages.*  

## Why MLang?

MLang gives both the improved performance of StarfallEx and the static typing of E2, in a far more well designed, feature rich, and modern package.  
I also plan to implement MLang as a binary module, giving multithreading and GPU capabilities to the language, allowing users to learn a single language for multiple applications.  

From the back end it provides an extremely flexible and easy to use extension system, by default adding runtime typechecking wrappers to an extension's functions, 
with the ability to disable them for increased performance once type safety has been tested.  

### Benefits over Expression 2
* Significantly better performance
* Well written
* Full class system and overall more refined implementation of static typing and OOP
* Clientside code execution

### Benefits over StarfallEx
* Type safety
* More concrete OOP
* **(CURRENTLY UNTESTED BUT THEORETICALLY TRUE)** Heavy use of compile time checks allowing for less runtime overhead (minor performance increase)

## Complex Syntax Example
```cpp
class Example<T> {
	private T privateMember;
	public num i = 8;

	Example(T constructorParam) {
		self.privateMember = constructorParam;
	}

	// Return types implicitly the same as the class declaration above
	// Separate functions for reversable operands (will look for an op on lhs first, and if not present will check rhs)
	operator +(Example<T> a, num b) {
		return a.num + b;
	}
	operator +(num b, Example<T> a) {
		return a.num + b;
	}
}

num MakeExample(num n) {
	return Example<num>(n).i;
}

num MakeExample(string s) { // function overloads (same method reversable operators use)
	return Example<string>(s).i;
}
```
