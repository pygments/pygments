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

    enum State {
      NotStarted,
      WorkInProgress,
      Done
    }
    State public state;

    struct AStruct {
        string name;
        uint8 type;
    }

    mapping(address => AStruct) registry;

    event Paid(uint256 value);
    event Received(uint256 time);
    event Withdraw(uint256 value);

    function addRegistry(string _name, uint8 _type) {
        AStruct memory newItem = AStruct({
            name: _name,
            type: _type
        });

        registry[msg.sender] = newItem;
    }

    function getHash(AStruct item) returns(uint) {
        return uint(keccak256(item.name, item.type));
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
}
