// A small model that exercises the lexical structure of KerML.
library package Vehicles {
    private import ScalarValues::*;
    public import Base::Anything;
    alias Thing for Anything;

    doc /* Classifiers, features and expressions in KerML. */

    comment Units about Car locale "en_US"
        /* All values are in SI units. */

    abstract classifier Vehicle specializes Anything;
    classifier Truck;
    subclassifier Truck specializes Vehicle;
    datatype Speed :> Real;
    class Wheel;
    class Engine;
    struct Car :> Vehicle {
        feature wheels : Wheel[4] ordered nonunique;
        composite feature engine : Engine[1];
        derived var feature speed : Speed;
        const feature serial : String;
        portion feature 'body\'s frame' : Car[0..1];
    }
    assoc Ownership {
        end feature owner : Car[1];
        end feature owned : Wheel[1];
    }
    metaclass Tag;
    behavior Drive { in feature distance : Real; }
    function Double { in x : Real; return : Real = x * 2; }
    predicate IsFast { in s : Speed; s > 1.5e2 }
    interaction Handshake {
        end feature caller : Car[1];
        end feature callee : Car[1];
    }

    #Tag feature tagged : Car = new Car();
    feature cars : Car[*];
    feature trucks : Truck[*];
    feature spareWheels : Wheel[*];
    feature fleet : Vehicle[*] unions cars, trucks;
    feature both : Vehicle[*] intersects cars, trucks;
    feature others : Vehicle[*] differences fleet, cars;
    feature drivers[*] featured by Car;
    feature carEngines chains cars.engine;
    feature owners : Car[*] inverse of Ownership::owner;
    multiplicity few subsets Base::exactlyOne;

    disjoining d disjoint Car from Wheel;
    subset trucks subsets fleet;
    typing cars typed by Car;
    featuring fleetFeaturing of fleet by Anything;

    step drive : Drive;
    succession first drive then drive;
    connector link : Ownership from cars to spareWheels;
    binding of fleet = others;

    expr half { in x : Real; x / 2 }
    bool positive { in x : Real; x > 0 }
    inv { true and not false }
    feature x : Real = 1 + 2 * 3 - 4 / 5 % 6 ** 7 ^ 8;
    feature y : Real = (null ?? 0) as Real;
    feature isCar : Boolean = tagged istype Car and tagged hastype Car
        or tagged @ Vehicle xor false implies true;
    feature isFeature : Boolean = tagged @@ KerML::Feature;
    feature firstCar : Car = cars#(1);
    feature check : Integer = if true ? 1 else 2;
    feature root : Car[*] = $::Vehicles::cars;

    namespace N { doc /* A plain namespace. */ }
    rep language "alf" /* this.x = 1; */
}
