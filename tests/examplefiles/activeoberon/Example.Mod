(* A torture test for the Active Oberon lexer: (* nested comments *),
   escaped strings, math arrays, active objects and inline assembler. *)
(** A documentation comment, extracted by the A2 documentation tools. *)

MODULE Example;

IMPORT SYSTEM, KernelLog, Streams, Commands, Log := KernelLog;

CONST
  Max* = 100;
  Mask = {0, 2, 4..7};
  Message = "double quoted";
  Other = 'single quoted, may contain "';
  Raw = \"an escaped string: it may contain " and \n verbatim"\;
  Hex = 0FFFFFFFFH;
  Big = 1'000'000;
  Bin = 0b1010'1010;
  Cee = 0x2A;
  Nl = 0AX;
  Pi = 3.14159;
  Tiny = 1.5E-3;

TYPE
  Name* = ARRAY 32 OF CHAR;
  Handler* = PROCEDURE {DELEGATE} (n: SIGNED32; CONST s: ARRAY OF CHAR);
  Callback = PROCEDURE {WINAPI} (handle: ADDRESS): UNSIGNED32;

  Color* = ENUM Red*, Green*, Blue* END;

  Date* = RECORD {FINAL}
    year*: SIGNED16;
    month-, day-: SIGNED8;
  END;

  Gpio = POINTER {UNSAFE} TO RECORD
    input, output: UNSIGNED32;
  END;

  Vector* = ARRAY [*] OF FLOAT64;
  Matrix* = ARRAY [*, *] OF FLOAT64;
  Tensor* = ARRAY [?] OF COMPLEX64;

  Shape* = OBJECT {ABSTRACT}
    PROCEDURE Area*(): FLOAT64;
    BEGIN {ABSTRACT}
    END Area;
  END Shape;

  Buffer* = OBJECT (Shape)
  VAR
    data: ARRAY 16 OF SIGNED32;
    head, tail, count: SIZE;
    cache {UNTRACED}: POINTER TO ARRAY OF CHAR;
    stamp {ALIGNED(32)}: UNSIGNED64;
    alive: BOOLEAN;

    PROCEDURE &Init*(CONST name: ARRAY OF CHAR);
    BEGIN
      head := 0; tail := 0; count := 0; alive := TRUE
    END Init;

    PROCEDURE Put*(x: SIGNED32);
    BEGIN {EXCLUSIVE}
      AWAIT(count < LEN(data));
      data[tail] := x; tail := (tail + 1) MOD LEN(data); INC(count)
    END Put;

    PROCEDURE Get*(): SIGNED32;
    VAR x: SIGNED32;
    BEGIN {EXCLUSIVE}
      AWAIT(count > 0);
      x := data[head]; head := (head + 1) MOD LEN(data); DEC(count);
      RETURN x
    END Get;

    PROCEDURE Area*(): FLOAT64;
    BEGIN {OVERRIDE}
      RETURN 0.0
    END Area;

  BEGIN {ACTIVE, SAFE, PRIORITY(High)}
    WHILE alive DO Log.Int(SELF.Get(), 1); Log.Ln END
  END Buffer;

  (* Active Cells: hardware description *)
  Adder = CELL {DataMemorySize(1024), ChannelWidth(32)} (in: PORT IN;
      out: PORT OUT);
  VAR x: SIGNED32;
  BEGIN
    LOOP in ? x; out ! x + 1 END
  END Adder;

  Net = CELLNET {Backend("TRM")} ;
  VAR a, b: Adder;
  BEGIN
    CONNECT(a.out, b.in)
  END Net;

VAR
  counter-: SIGNED32;
  origin: Date;

OPERATOR "+"* (CONST a, b: Date): Date;
VAR r: Date;
BEGIN
  r := a; INC(r.year, b.year);
  RETURN r
END "+";

PROCEDURE Statements(n: SIGNED32; VAR out: SIGNED32): SIGNED32;
VAR i, sum: SIZE; c: CHAR; color: Color; s: SET; any: ANY;
BEGIN
  IF n < 0 THEN n := -n ELSIF n = 0 THEN n := 1 ELSE n := n DIV 2 END;
  CASE c OF
      "a".."m": sum := 1
    | "n".."z": sum := 2
  ELSE sum := 0
  END;
  WHILE n > 1 DO n := n DIV 2 END;
  REPEAT INC(n) UNTIL n >= 4;
  FOR i := 0 TO 10 BY 2 DO INC(sum, i) END;
  LOOP INC(i); IF i > 100 THEN EXIT END END;
  s := {0, 3, 5..7} + Mask; s := s * {1}; INCL(s, 9); EXCL(s, 0);
  IF 3 IN s THEN color := Color.Green END;
  WITH any: Buffer DO Log.Int(any.Get(), 1) END;
  IGNORE Statements(1, out);
  VAR local := sum + 1;
  ASSERT(local > 0);
  RETURN SIGNED32(local)
FINALLY
  RETURN 0
END Statements;

PROCEDURE MathArrays;
VAR v, w: Vector; m: Matrix; f: FLOAT64;
BEGIN
  NEW(v, 3); NEW(w, 3); NEW(m, 3, 3);
  v := [1.0, 2.0, 3.0];
  w := v .* v; w := 2 * v; w := v + w; m := m`; f := SUM(v .* w);
  IF v .= w THEN f := 0 END;
  Log.Int(LEN(v, 0), 1); Log.Int(DIM(m), 1); Log.Ln
END MathArrays;

PROCEDURE {PLAIN} LowLevel(adr: ADDRESS);
VAR value: UNSIGNED32;
BEGIN {UNCHECKED, UNCOOPERATIVE}
  SYSTEM.GET(adr, value);
  SYSTEM.PUT32(adr + 4, value);
  value := SYSTEM.VAL(UNSIGNED32, adr)
END LowLevel;

PROCEDURE {NORETURN} Reset;
CODE
#IF I386 THEN
  loop:
  CMP ECX, 0
  JE end          ; a label may well be called end
  MOV EAX, [EBP+8]
  JMP loop
  end:
  HLT
#ELSE
  NOP
#END
END Reset;

PROCEDURE Command*(context: Commands.Context);
VAR name: Name;
BEGIN
  context.arg.SkipWhitespace; context.arg.String(name);
  IF name = "" THEN COPY("world", name) END;
  context.out.String(Message); context.out.String(name); context.out.Ln;
  context.out.Update
END Command;

BEGIN
  counter := 0;
  KernelLog.String("Example loaded"); KernelLog.Ln
END Example.

Compiler.Compile Example.Mod ~
Example.Command Active Oberon ~
System.Free Example ~
