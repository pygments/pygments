pragma solidity ^0.4.20;

pragma solidity >=0.4.0 <0.7.0;

// one-line singleline comment

/* one-line multiline comment */

/*
  multi-line multiline comment
*/

contract ContractName {
    address public publicaddress;

    uint varname1 = 1234;
    int varname2 = 0x12abcdEF;

    string astringsingle = 'test "string" value\' single';
    string astringdouble = "test 'string' value\" double";

    address public shipper;
    address private arbiter;
    address astronaut;

    enum State {
        NotStarted,
        WorkInProgress,
        Done
    }
    State public state;

    struct AStruct {
        string name;
        uint8 atype;
    }

    mapping(address => AStruct) registry;

    event Paid(uint256 value);
    event Received(uint256 time);
    event Withdraw(uint256 value);

    function addRegistry(string _name, uint8 _type) {
        AStruct memory newItem = AStruct({name: _name, atype: _type});

        registry[msg.sender] = newItem;
    }

    function getHash(AStruct item) returns (uint) {
        return uint(keccak256(item.name, item.atype));
    }

    function pay() public payable {
        require(msg.sender == astronaut);
        state = State.Paid;
        Paid(msg.value);
    }

    function receive() public {
        require(msg.sender == arbiter);
        require(state == State.Paid);
        state = State.Received;
        Received(now);
    }

    function withdraw() public {
        require(msg.sender == shipper);
        require(state == State.Received);
        state = State.Withdrawn;
        Withdraw(this.balance);
        shipper.transfer(this.balance);
    }

    function max(uint a, uint b) public returns (uint) {
        if (a > b && b < a) {
            return a;
        } else {
            return b;
        }
    }

    function operators() public {
        uint a = 20;
        uint b = 10;
        // arithmetic
        uint _sum = a + b;
        uint _diff = a - b;
        uint _mul = a * b;
        uint _div = a / b;
        uint _mod = a % b;
        uint _dec = --b;
        uint _inc = ++a;

        // comparison
        bool _eq = a == b;
        bool _noteq = a != b;
        bool _greater = a > b;
        bool _less = a < b;
        bool _geq = a >= b;
        bool _leq = a <= b;

        // logical boolean
        bool x = true;
        bool y = false;
        bool _and = x && y;
        bool _or = a || b;
        bool _not = !a;

        // bitwise
        uint64 i = 20;
        uint64 j = 10;
        uint64 _bitand = i & j;
        uint64 _bitor = i | j;
        uint64 _bitxor = i ^ j;
        uint64 _leftshift = i << j;
        uint64 _rightshift = i >> j;
        uint64 _bitnot = ~i;
    }
}
