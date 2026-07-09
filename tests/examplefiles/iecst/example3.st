// PLC configuration
CONFIGURATION DefaultCfg
    VAR_GLOBAL
        b_Start_Stop: BOOL; // Global variable to represent a boolean.
        b_ON_OFF: BOOL; // Global variable to represent a boolean.
        Start_Stop AT %IX0.0: BOOL; // Digital input of the PLC (Address 0.0)
        ON_OFF AT %QX0.0: BOOL; // Digital output of the PLC (Address 0.0). (Coil)
    END_VAR

    // Schedule the main program to be executed every 20 ms
    TASK Tick(INTERVAL := T#20ms);

    PROGRAM Main WITH Tick: Monitor_Start_Stop;
END_CONFIGURATION

PROGRAM Monitor_Start_Stop // Actual Program
    VAR_EXTERNAL
        Start_Stop: BOOL;
        ON_OFF: BOOL;
    END_VAR
    VAR // Temporary variables for logic handling
        ONS_Trig: BOOL;
        Rising_ONS: BOOL;
    END_VAR

    // Start of Logic
    // Catch the Rising Edge One Shot of the Start_Stop input
    ONS_Trig := Start_Stop AND NOT Rising_ONS;

    // Main Logic for Run_Contact -- Toggle ON / Toggle OFF ---
    ON_OFF := (ONS_Trig AND NOT ON_OFF) OR (ON_OFF AND NOT ONS_Trig);

    // Rising One Shot logic
    Rising_ONS := Start_Stop;
END_PROGRAM
