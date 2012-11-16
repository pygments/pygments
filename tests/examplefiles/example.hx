package;
package net.onthewings;

import net.onthewings.Test;
import net.onthewings.*;

using Lambda;
using net.onthewings.Test;

class Test {
	@values(-1,100) public var readOnlyField(default,null):Int;
	var a(get_a, set_a):String;
	function get_a():String return a
	function set_a(v:String):String {
		return a = v;
	}
	
	private function new():Void {
		inline function innerFun(a:Int, b:Int):Int {
			return readOnlyField = a + b;
		}
		
		innerFun(1, 2.3);
	}
	
	static public var instance(get,null):Test;
	static function get_instance():Test {
		return instance != null ? instance : instance = new Test();
	}
}

@:native("Test") private class Test2 {}

extern class Ext {}

@:macro class M {
	@:macro static function test(e:Array<Expr>):ExprOf<String> {
		return macro "ok";
	}
}

enum Color {
    Red;
    Green;
    Blue;
    Grey( v : Int );
    Rgb( r : Int, g : Int, b : Int );
    Alpha( a : Int, col : Color );
}

class Colors {
    static function toInt( c : Color ) : Int {
        return switch( c ) {
            case Red: 0xFF0000;
            case Green: 0x00FF00;
            case Blue: 0x0000FF;
            case Grey(v): (v << 16) | (v << 8) | v;
            case Rgb(r,g,b): (r << 16) | (g << 8) | b;
            case Alpha(a,c): (a << 24) | (toInt(c) & 0xFFFFFF);
        }
    }
}

class EvtQueue<T : (Event, EventDispatcher)> {
    var evt : T;
}

typedef DS = Dynamic<String>;
typedef Pt = {
	var x:Float;
	var y:Float;
	@:optional var z:Float; /* optional z */
	function add(pt:Pt):Void;
}
typedef Pt2 = {
	x:Float,
	y:Float,
	?z:Float, //optional z
	add : Point -> Void,
}

var point = { "x" : 1, "y" : -5 };

0; // Int
-134; // Int
0xFF00; // Int

123.0; // Float
.14179; // Float
13e50; // Float
-1e-99; // Float

"hello"; // String
"hello \"world\" !"; // String
'hello "world" !'; // String

true; // Bool
false; // Bool

null; // Unknown<0>

~/[a-z]+/i; // EReg : regular expression

{
    var x;
    var y = 3;
    var z : String;
    var w : String = "";
    var a, b : Bool, c : Int = 0;
}