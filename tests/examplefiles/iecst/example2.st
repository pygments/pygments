// =======================================================================
// Function Block Timed Counter: Incremental count of the timed interval
// =======================================================================
FUNCTION_BLOCK FB_Timed_Counter
    VAR_INPUT
        Execute: BOOL := FALSE; // Trigger signal to begin Timed Counting
        Time_Increment: REAL := 1.25; // Enter Cycle Time (Seconds) between counts
        Count_Cycles: INT := 20; // Number of Desired Count Cycles
    END_VAR

    VAR_OUTPUT
        Timer_Done_Bit: BOOL := FALSE; // One Shot Bit indicating Timer Cycle Done
        Count_Complete: BOOL := FALSE; // Output Bit indicating the Count is complete
        Current_Count: INT := 0; // Accumulating Value of Counter
    END_VAR

    VAR
        CycleTimer: TON; // Timer FB from Command Library
        CycleCounter: CTU; // Counter FB from Command Library
        TimerPreset: TIME; // Converted Time_Increment in Seconds to MS
    END_VAR

    // Start of Function Block programming
    TimerPreset := REAL_TO_TIME(in := Time_Increment) * 1000;

    CycleTimer(
        in := Execute AND NOT CycleTimer.Q,
        pt := TimerPreset);

    Timer_Done_Bit := CycleTimer.Q;

    CycleCounter(
        cu := CycleTimer.Q,
        r := NOT Execute,
        pv := Count_Cycles);

    Current_Count := CycleCounter.cv;
    Count_Complete := CycleCounter.q;

END_FUNCTION_BLOCK
