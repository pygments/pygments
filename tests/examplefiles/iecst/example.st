(* Example ST Code covering various features *)

FUNCTION_BLOCK FB_Example
VAR_INPUT
    Enable  : BOOL;
    InputVal: REAL;
    LimitSet: INT := 100;
END_VAR
VAR_OUTPUT
    OutputVal : REAL;
    StatusFlag: BOOL;
END_VAR
VAR
    i : INT;
    temp : REAL;
    TimerInst : TON; // IEC timer instance
END_VAR

// Single line comment
(* Multi-line
   (* with nested comment *)
   comment continues *)

IF Enable THEN
    CASE LimitSet OF
        1 TO 5:
            i := 50;
        10:
            i := 10;
        ELSE
            i := 0;
    END_CASE;

    FOR i := 1 TO 10 DO
        temp := temp + 1.0;
    END_FOR;

    WHILE temp < InputVal DO
        temp := temp * 1.05;
    END_WHILE;

    OutputVal := SIN(InputVal) * 2.0;
    StatusFlag := TRUE;
ELSE
    StatusFlag := FALSE;
END_IF;

// IEC Literals
VarTime := T#1h_30m_5s;
VarDate := D#2025-11-24;
VarHex  := 16#F00D;
VarBin  := 2#1101_0110;
VarString := 'This is a string literal.';

// Function Block call
TimerInst(IN := Enable, PT := T#5s, Q => StatusFlag);