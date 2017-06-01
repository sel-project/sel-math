/*
 * Copyright (c) 2016-2017 SEL
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 * 
 */
module sel.math.vector;

import std.algorithm : reverse, canFind;
import std.array : join, split;
import std.conv : to, ConvException;
static import std.math;
import std.range.primitives : ElementType;
import std.string : replace;
import std.traits : IntegralTypeOf, staticIndexOf, isNumeric, isArray, CommonType, isFloatingPointTrait = isFloatingPoint, isImplicitlyConvertible;
import std.typecons : isTuple;
import std.typetuple : TypeTuple;

static import std.typecons;

/**
 * Vector for coordinates storing and operations.
 */
struct Vector(T, char[] c) if(c.length > 1 && areValidCoordinates(c)) {
	
	public alias Type = T;
	public alias coordinates = c;
	
	mixin("alias Tuple = std.typecons.Tuple!(T, \"" ~ join(coordinates.idup.split(""), "\", T, \"") ~ "\");");
	
	mixin("public enum coords = TypeTuple!('" ~ join(coordinates.idup.split(""), "','") ~ "');");
	
	enum bool isFloatingPoint = isFloatingPointTrait!T;
	
	private Tuple value;
	
	mixin((){
		string ret;
		foreach(immutable c ; coords) {
			ret ~= "public pure nothrow @property @safe @nogc T " ~ c ~ "(){ return this.value." ~ c ~ "; }";
		}
		return ret;
	}());
	
	public pure nothrow @safe @nogc this(Tuple value) {
		this.value = value;
	}
	
	public pure nothrow @safe @nogc this(T value) {
		foreach(immutable c ; coords) {
			mixin("this.value." ~ c) = value;
		}
	}
	
	public @safe this(F...)(F args) if(F.length == coordinates.length) {
		foreach(i, immutable c; coords) {
			mixin("this.value." ~ c) = cast(T)args[i];
		}
	}
	
	public @safe @nogc this(T[coords.length] variables) {
		foreach(i, immutable c; coords) {
			mixin("this.value." ~ c) = variables[i];
		}
	}
	
	public @safe @nogc this(T[] variables) {
		foreach(i, immutable c; coords) {
			mixin("this.value." ~ c) = variables[i];
		}
	}
	
	/**
	 * Gets the vector as a constant tuple.
	 * Example:
	 * ---
	 * auto v = vector(0, 3, 4);
	 * assert(v.tuple == typeof(v).Tuple(0, 3, 4));
	 * ---
	 */
	public pure nothrow @property @safe @nogc const(Tuple) tuple() {
		return this.value;
	}
	
	/**
	 * Compares the vector with another vector of the same length or with
	 * a single number.
	 * Returns: true if all the values are equals, false otherwise
	 * Example:
	 * ---
	 * assert(Vector2!int(0, 10) == Vector2!int(0, 10));
	 * assert(Vector3!ubyte(1, 1, 255) == Vector3!real(1, 1, 255));
	 * assert(vector(0, 0, 0, 0) == 0);
	 * assert(vector(1, 2) == [1, 2]);
	 * assert(vector(float.nan, float.nan) != vector(float.nan, float.nan));
	 * ---
	 */
	public bool opEquals(F)(F value) {
		static if(isVector!F && coords == F.coords) return this.opEqualsImpl!"this.{c}==value.{c}"(value);
		else static if(isArray!F) return value.length == coords.length && this.opEqualsImpl!"this.{c}==value[{i}]"(value);
		else static if(__traits(compiles, T.init == F.init)) return this.opEqualsImpl!"this.{c}==value"(value);
		else return false;
	}
	
	private bool opEqualsImpl(string op, F)(F value) {
		mixin((){
				string[] ret;
				foreach(i, immutable c; coords) {
					ret ~= op.replace("{c}", to!string(c)).replace("{i}", to!string(i));
				}
				return "return " ~ ret.join("&&") ~ ";";
			}());
	}
	
	/**
	 * Performs an unary operation on the vector.
	 * Returns: the new vector
	 * Example:
	 * ---
	 * auto v = vector(-1, 0, 1);
	 * assert(-v == vector(1, 0, -1));
	 * assert(++v == vector(0, 1, 2)); // this will change the original vector's values!
	 * assert(v-- == vector(0, 1, 2) && v == vector(-1, 0, 1));
	 * ---
	 */
	public typeof(this) opUnary(string op)() if(__traits(compiles, { mixin("T t;t=" ~ op ~ "t;"); })) {
		typeof(this) ret;
		foreach(immutable c ; coords) {
			mixin("ret.value." ~ c ~ "=" ~ op ~ "this.value." ~ c ~ ";");
		}
		return ret;
	}
	
	/**
	 * Performs a binary operation on the vector.
	 * Params:
	 * 		value = a number, a vector or an array with the same size
	 * Returns: the new vector
	 * Example:
	 * ---
	 * assert(vector(1, 1) - 1 == vector(0, 0));
	 * assert(vector(10, 10) * vector(0, 9) == vector(0, 90));
	 * assert(vector(16, 15) & [15, 3] == vector(0, 3));
	 * assert(1 - vector(100, 0, -100) == vector(-99, 1, 101));
	 * ---
	 */
	public typeof(this) opBinary(string op, F)(F value) if(op != "in") {
		return this.dup.opOpAssign!op(value);
	}
	
	public typeof(this) opBinaryRight(string op, F)(F value) if(op != "in" && __traits(compiles, typeof(this)(value))) {
		return typeof(this)(value).opBinary!op(this);
	}
	
	/**
	 * Performs an assign operation on the vector, modifying it.
	 * Params:
	 * 		value = a number, a vector or an array with the same size
	 * Returns:
	 * Example:
	 * ---
	 * auto v = vector(1, 2);
	 * v += 4;
	 * v *= [0, 2];
	 * assert(v == vector(0, 12));
	 * ---
	 */
	public typeof(this) opOpAssign(string op, F)(F value) if(isVector!F && coordinates == F.coordinates) {
		return this.opAssignImpl!("this.value.{c}" ~ op ~ "=value.{c}")(value);
	}
	
	/// ditto
	public typeof(this) opOpAssign(string op, F)(F value) if(isArray!F) {
		return this.opAssignImpl!("this.value.{c}" ~ op ~ "=value[{i}]")(value);
	}
	
	/// ditto
	public typeof(this) opOpAssign(string op, F)(F value) if(isImplicitlyConvertible!(F, T)) {
		return this.opAssignImpl!("this.value.{c}" ~ op ~ "=value")(value);
	}
	
	private typeof(this) opAssignImpl(string query, F)(F value) {
		foreach(i, immutable c; coords) {
			mixin(query.replace("{c}", to!string(c)).replace("{i}", to!string(i)) ~ ";");
		}
		return this;
	}
	
	/**
	 * Converts the vector to the given one, mantaining the variables's
	 * value when possible.
	 * Example:
	 * ---
	 * assert(cast(Vector2!int)vector(.1, .1, 14) == vector(0, 14));
	 * assert(cast(Vector4!real)vector(.5, 100) == vector(.5, 0, 100, 0));
	 * // this will only return the vector
	 * assert(cast(Vector2!int
	 * ---
	 */
	public @safe auto opCast(F)() if(isVector!F) {
		static if(is(T == F) && coordinates == F.coordinates) {
			return this;
		} else {
			F ret;
			foreach(immutable c; F.coords) {
				static if(coordinates.canFind(c)) {
					mixin("ret.value." ~ c) = to!(F.Type)(mixin("this." ~ c));
				}
			}
			return ret;
		}
	}
	
	/**
	 * Converts the vector into an array of the same size.
	 * Example:
	 * ---
	 * assert(cast(int[])vector(1, 2) == [1, 2]);
	 * assert(cast(long[])vector(.1, 1.5, -.1) == [0L, 1L, 0L]);
	 * ---
	 */
	public @safe auto opCast(F)() if(isArray!F) {
		F array = new typeof(F.init[0])[coords.length];
		foreach(i, coord; coords) {
			array[i] = to!(typeof(F.init[0]))(mixin("this." ~ coord));
		}
		return array;
	}
	
	/**
	 * Changes the vector's type.
	 */
	public auto type(F)() if(isImplicitlyConvertible!(F, T)) {
		Vector!(F, coordinates) ret;
		foreach(immutable c ; coords) {
			mixin("ret.value." ~ c) = mixin("this." ~ c);
		}
		return ret;
	}
	
	/**
	 * Duplicates the vector, mantaing the type, variables'
	 * names and their value.
	 * Example:
	 * ---
	 * assert(vector(1, 1).dup == vector(1, 1));
	 * ---
	 */
	alias dup = type!T;
	
	/**
	 * Gets the vector's length.
	 */
	public @property double length() {
		double length = 0;
		foreach(immutable c ; coords) {
			length += mixin("this." ~ c) ^^ 2;
			//mixin("length += this.value." ~ c ~ " * this.value." ~ c ~ ";");
		}
		return std.math.sqrt(length);
	}
	
	/**
	 * Sets the vector's length.
	 */
	public @property double length(double length) {
		double mult = length / this.length;
		foreach(immutable c ; coords) {
			static if(is(T == double)) {
				mixin("this.value." ~ c) *= mult;
			} else {
				mixin("this.value." ~ c) = cast(T)(mixin("this." ~ c) * mult);
			}
		}
		return length;
	}
	
	/**
	 * Converts the vector into a string for logging and debugging purposes.
	 */
	public string toString() {
		string[] cs;
		foreach(i, coord; coords) {
			cs ~= to!string(mixin("this." ~ coord));
		}
		return "Vector!(" ~ T.stringof ~ ", \"" ~ coordinates.idup ~ "\")(" ~ cs.join(", ") ~ ")";
	}
	
}

/// ditto
alias Vector(T, string coords) = Vector!(T, coords.dup);

/// ditto
alias Vector2(T) = Vector!(T, "xz");

/// ditto
alias Vector3(T) = Vector!(T, "xyz");

/// ditto
alias Vector4(T) = Vector!(T, "xyzw");

private bool areValidCoordinates(char[] coords) {
	foreach(i, char c; coords[0..$-1]) {
		if(coords[i+1..$].canFind(c)) return false;
	}
	return true;
}

/**
 * Automatically creates a vector if the number of the
 * given arguments matches one of the default vectors.
 * Example:
 * ---
 * assert(is(typeof(vector(1, 1)) == Vector2!int));
 * assert(is(typeof(vector(2Lu, 4)) == Vector2!ulong));
 * assert(is(typeof(vector(5, 5, 19.0)) == Vector3!double));
 * assert(is(typeof(vector(0, real.nan, double.nan, float.nan)) == Vector4!real));
 * ---
 */
public auto vector(E...)(E args) if(E.length > 1 && E.length <= 4 && !is(CommonType!E == void)) {
	return mixin("Vector" ~ to!string(E.length) ~ "!(CommonType!E)")(args);
}

/// Checks if the given type is a vector
enum bool isVector(T) = __traits(compiles, Vector!(T.Type, T.coordinates)(T.Type.init));

public nothrow @safe T mathFunction(alias func, T)(T vector) if(isVector!T) {
	T.Type[] values;
	foreach(immutable c ; T.coords) {
		values ~= cast(T.Type)func(mixin("vector." ~ c));
	}
	return T(values);
}

/**
 * Rounds a vector to the nearest integer.
 * Example:
 * ---
 * assert(round(vector(.25, .5, .75)) == vector(0, 1, 1));
 * ---
 */
public nothrow @safe T round(T)(T vector) if(isVector!T) {
	return mathFunction!(std.math.round)(vector);
}

/**
 * Floors a vector to the nearest integer.
 * Example:
 * ---
 * assert(floor(vector(.25, .5, .75)) == vector(0, 0, 0));
 * ---
 */
public nothrow @safe T floor(T)(T vector) if(isVector!T) {
	return mathFunction!(std.math.floor)(vector);
}

/**
 * Ceils a vector to the nearest integer.
 * Example:
 * ---
 * assert(ceil(vector(.25, .5, .75)) == vector(1, 1, 1));
 * ---
 */
public nothrow @safe T ceil(T)(T vector) if(isVector!T) {
	return mathFunction!(std.math.ceil)(vector);
}

/**
 * Calculate the absolute value of the array.
 * Example:
 * ---
 * assert(abs(vector(-1, 0, 90)) == vector(1, 0, 90));
 * ---
 */
public nothrow @safe T abs(T)(T vector) if(isVector!T) {
	return mathFunction!(std.math.abs)(vector);
}

/**
 * Checks whether or not every member of the vector is finite
 * (not infite, -inifite, nan).
 * Example:
 * ---
 * assert(isFinite(vector(1, 2)));
 * assert(isFinite(vector(float.min, float.max)));
 * assert(!isFinite(vector(1, float.nan)));
 * assert(!isFinite(vector(-float.infinity, 1f/0f)));
 * ---
 */
public pure nothrow @safe @nogc bool isFinite(T)(T vector) if(isVector!T && T.isFloatingPoint) {
	foreach(immutable c ; T.coords) {
		if(!std.math.isFinite(mixin("vector." ~ c))) return false;
	}
	return true;
}

/**
 * Checks whether or not at least one member of the vector
 * is not a number (nan).
 * Example:
 * ---
 * assert(!isNaN(vector(0, 2.1)));
 * assert(isNaN(vector(float.init, -double.init)));
 * assert(isNaN(vector(0, float.nan)));
 * ---
 */
public pure nothrow @safe @nogc bool isNaN(T)(T vector) if(isVector!T && T.isFloatingPoint) {
	foreach(immutable c ; T.coords) {
		if(std.math.isNaN(mixin("vector." ~ c))) return true;
	}
	return false;
}

public @safe double distanceSquared(F, G)(F vector1, G vector2) if(isVector!F && isVector!G && F.coordinates == G.coordinates) {
	double sum = 0;
	foreach(immutable c ; F.coords) {
		sum += std.math.pow(mixin("vector1." ~ c) - mixin("vector2." ~ c), 2);
	}
	return sum;
}

/**
 * Calculates the distance between to vectors of the
 * same length.
 * Params:
 * 		vector1 = the first vector
 * 		vector2 = the second vector
 * Returns: the distance between the two vectors (always higher or equals than  0)
 * Example:
 * ---
 * assert(distance(vector(0, 0), vector(1, 0)) == 1);
 * assert(distance(vector(0, 0, 0) == vector(1, 1, 1)) == 3 ^^ .5); // 3 ^^ .5 is the squared root of 3
 * ---
 */
public @safe double distance(T, char[] coords, E)(Vector!(T, coords) vector1, Vector!(E, coords) vector2) {
	return std.math.sqrt(distanceSquared(vector1, vector2));
}

public pure nothrow @safe double dot(T, char[] coords, E)(Vector!(T, coords) vector1, Vector!(E, coords) vector2) {
	double dot = 0;
	foreach(immutable c ; Vector!(T, coords).coords) {
		dot += mixin("vector1." ~ c) * mixin("vector2." ~ c);
	}
	return dot;
}

public pure nothrow @safe Vector!(CommonType!(A, B), coords) cross(A, B, char[] coords)(Vector!(A, coords) a, Vector!(B, coords) b) {
	foreach(immutable exc ; Vector!(T, coords).coords) {
		
	}
}

unittest {
	
	Vector3!int v3 = Vector3!int(-1, 0, 12);
	
	// comparing
	assert(v3.x == -1);
	assert(v3.y == 0);
	assert(v3.z == 12);
	assert(v3 == Vector3!int(-1, 0, 12));
	assert(v3 == Vector3!float(-1, 0, 12));
	assert(v3 != Vector3!double(-1, 0, 12.00000001));
	
	// unary
	assert(-v3 == Vector3!int(1, 0, -12));
	assert(++v3 == Vector3!int(0, 1, 13) && v3 == Vector3!int(0, 1, 13));
	assert(v3-- == Vector3!int(0, 1, 13) && v3 == Vector3!int(-1, 0, 12));
	
	// binary operator
	assert(v3 + 3 == Vector3!int(2, 3, 15));
	assert(v3 * 100 == Vector3!int(-100, 0, 1200));
	assert(v3 - v3 == Vector3!int(0, 0, 0));
	assert(Vector3!double(.5, 0, 0) + v3 == Vector3!double(-.5, 0, 12));
	assert(v3 * [1, 2, 3] == Vector3!int(-1, 0, 36));
	assert((v3 & 1) == Vector3!int(1, 0, 0));
	assert(1 - v3 == Vector3!int(2, 1, -11));
	
	// assign operator
	assert((v3 *= 3) == Vector3!int(-3, 0, 36));
	assert(v3 == Vector3!int(-3, 0, 36));
	v3 >>= 1;
	assert(v3 == Vector3!int(-2, 0, 18));
	
	// cast
	Vector3!float v3f = cast(Vector3!float)v3;
	Vector2!int reduced = cast(Vector2!int)v3;
	Vector4!long bigger = cast(Vector4!long)v3;
	assert(v3f == Vector3!float(-2, 0, 18));
	assert(reduced == Vector2!float(-2, 18));
	assert(bigger == Vector4!long(-2, 0, 18, 0));
	
	// vector function
	assert(vector(8, 19).Type.stringof == "int");
	assert(vector(1.0, 2, 99.9).Type.stringof == "double");
	assert(vector(1f, .01, 12L).Type.stringof == "double");
	
	// math functions
	assert(round(vector(.2, .7)) == vector(0, 1));
	assert(floor(vector(.2, .7)) == vector(0, 0));
	assert(ceil(vector(.2, .7)) == vector(1, 1));
	assert(abs(vector(-.2, .7)) == vector(.2, .7));
	
	// distance
	assert(distance(vector(0, 0), vector(0, 1)) == 1);
	assert(distance(vector(0, 0, 0), vector(1, 1, 1)) == 3 ^^ .5);
	
}
