// A small drone model that exercises the lexical structure of SysML v2.
package 'Drone Example' {
    private import ScalarValues::*;
    private import ISQ::*;
    private import SI::*;
    private import NumericalFunctions::sum;
    private import SequenceFunctions::size;
    private import ControlFunctions::select;

    //* A multiline note.
        The parser ignores it, unlike the regular comments below. */

    doc /* Models of a small quadcopter, see https://www.omg.org/spec/SysML/. */

    comment Units about Drone locale "en_US"
        /* All values are in SI units. */

    metadata def Safety {
        attribute isMandatory : Boolean default true;
    }
    metadata def <ss> SafetySpecific :> Metaobjects::SemanticMetadata {
        :>> baseType = drones meta SysML::PartUsage;
    }

    abstract part def Vehicle;

    part def Drone :> Vehicle {
        doc /* A drone with four rotors. */
        attribute mass : MassValue = 1.25 [kg];
        attribute 'max. speed' : SpeedValue = 2.0E1 [m/s];
        attribute serial : String = "DR-01 \"alpha\"";
        attribute manual : String = "https://example.org/drones"; // Don't "quote" notes.
        attribute flightTime : DurationValue[0..1];

        #Safety part battery : Battery {
            attribute capacity : Real = 5.4e3;
        }
        #'Drone Example'::Safety part spare : Battery;
        part rotors : Rotor[4] ordered nonunique;
        part 'pilot\'s seat';
        port controlPort : ~ControlPort;
        ref part pilot : Pilot[0..*];

        constraint { mass <= 2 [kg] and flightTime >= 900 [s] }
    }

    part def Battery;
    part def Rotor;
    part def Pilot;
    port def ControlPort { in attribute command : String; }
    item def Command;

    part drones : Drone[*];
    #SafetySpecific part drone : Drone {
        @Safety { isMandatory = false; }
    }

    action def Fly {
        in item target;
        out attribute success : Boolean;
        first start then takeOff;
        action takeOff;
        then action cruise;
        then done;
    }

    state def FlightStates {
        entry; then idle;
        state idle;
        transition idle_to_flying
            first idle
            accept Command
            if drone.battery.capacity > .5
            do action launch
            then flying;
        state flying;
    }

    requirement def <'R1.1'> MaxMass {
        subject drone : Drone;
        require constraint { drone.mass <= 2 [kg] }
    }

    calc def Average {
        in values : Real[1..*];
        return result : Real = sum(values) / size(values);
    }

    ref part selected : Drone[*] = drones->select { in d : Drone; d.mass < 1.5 [kg] };
    ref part firstDrone : Drone = selected#(1);
    attribute isDrone : Boolean = drone @ Drone and drone istype Vehicle
        and not (drone hastype Rotor) xor false implies true;
    attribute isPart : Boolean = drone @@ SysML::PartUsage;
    attribute serials : String[*] = drones.?{ in d : Drone; d.serial != null }.serial ?? "none";
    attribute total : Real = (1 + 2 * 3 - 4 / 5 % 6) ** 2 ^ 1;
    attribute same : Boolean = drone === drone or drone !== firstDrone;
    ref part root : Drone = $::'Drone Example'::drone;

    rep inOCL language "ocl" /* self.drones->size() <= 10 */
}
