# pragma version ^0.4.0
# pragma optimize gas
# pragma evm-version cancun

"""
@title Example contract showcasing Vyper 0.4.x features
@notice Demonstrates flags, modules, exports, new builtins
"""

from ethereum.ercs import IERC20
import ownable as Ownable

initializes: Ownable
uses: Ownable

exports: Ownable.owner

implements: IERC20

# --------------------------------- Flags -----------------------------------

flag Status:
    ACTIVE
    PAUSED
    DEPRECATED

flag Permissions:
    READ
    WRITE
    ADMIN

# --------------------------------- Events -----------------------------------

event StatusChanged:
    old_status: indexed(Status)
    new_status: indexed(Status)

event Deposit:
    sender: indexed(address)
    amount: uint256
    data: Bytes[1024]

# --------------------------------- Types ------------------------------------

struct Position:
    owner: address
    amount: uint256
    timestamp: uint256

# --------------------------------- Storage ----------------------------------

status: public(Status)
positions: HashMap[address, Position]
balances: public(HashMap[address, uint256])
allowances: HashMap[address, HashMap[address, uint256]]

total_supply: public(uint256)
name: public(constant(String[32])) = "Example Token"
symbol: public(constant(String[8])) = "EXM"
decimals: public(constant(uint8)) = 18

owner: public(address)
fee_rate: public(immutable(uint256))

# transient storage
reentrancy_lock: transient(bool)

# Various integer sizes
small_val: uint24
medium_val: int72
large_val: uint160

# Dynamic arrays
recent_deposits: DynArray[uint256, 100]

# --------------------------------- Init ------------------------------------

@deploy
def __init__(_fee_rate: uint256):
    fee_rate = _fee_rate
    self.owner = msg.sender
    self.status = Status.ACTIVE
    Ownable.__init__()

# --------------------------------- External --------------------------------

@external
@nonreentrant
def deposit(amount: uint256):
    assert not self.reentrancy_lock, "reentrant"
    self.reentrancy_lock = True

    assert self.status == Status.ACTIVE, "not active"
    assert amount > 0 and amount <= max_value(uint256)

    extcall IERC20(msg.sender).transferFrom(msg.sender, self, amount)

    self.balances[msg.sender] += amount
    self.total_supply += amount
    self.recent_deposits.append(amount)

    log Deposit(msg.sender, amount, b"")

    self.reentrancy_lock = False

@external
def withdraw(amount: uint256):
    assert self.balances[msg.sender] >= amount

    self.balances[msg.sender] -= amount
    self.total_supply -= amount

    extcall IERC20(msg.sender).transfer(msg.sender, amount)

@external
def set_status(new_status: Status):
    assert msg.sender == self.owner
    old: Status = self.status
    self.status = new_status
    log StatusChanged(old, new_status)

# --------------------------------- View ------------------------------------

@external
@view
def get_position(user: address) -> Position:
    return self.positions[user]

@external
@view
def get_block_info() -> (uint256, uint256, address, bytes32, uint256, uint256):
    return (
        block.timestamp,
        block.number,
        block.coinbase,
        block.prevrandao,
        block.basefee,
        block.gaslimit,
    )

@external
@view
def get_tx_info() -> (address, uint256, uint256):
    return (tx.origin, tx.gasprice, chain.id)

@external
@view
def compute_hash(data: Bytes[1024]) -> bytes32:
    return keccak256(data)

@external
@view
def compute_sha(data: Bytes[1024]) -> bytes32:
    return sha256(data)

# --------------------------------- Pure ------------------------------------

@external
@pure
def math_operations(a: uint256, b: uint256) -> uint256:
    x: uint256 = unsafe_add(a, b)
    y: uint256 = unsafe_mul(a, b)
    z: uint256 = isqrt(a)
    w: uint256 = uint256_addmod(a, b, 100)
    m: uint256 = uint256_mulmod(a, b, 100)
    p: uint256 = pow_mod256(a, b)
    return max(min(x, y), z)

@external
@pure
def abi_operations(data: Bytes[1024]) -> Bytes[1024]:
    encoded: Bytes[1024] = abi_encode(42, empty(address))
    val: uint256 = abi_decode(data, uint256)
    return encoded

@external
@pure
def type_info() -> (uint256, uint256, decimal):
    a: uint256 = max_value(uint256)
    b: uint256 = min_value(uint256)
    c: decimal = epsilon(decimal)
    return (a, b, c)

@external
@pure
def bitwise_ops(a: uint256, b: uint256) -> uint256:
    x: uint256 = a & b
    y: uint256 = a | b
    z: uint256 = a ^ b
    w: uint256 = ~a
    left: uint256 = a << 8
    right: uint256 = a >> 8
    return x

@external
@pure
def string_ops(s: String[100]) -> uint256:
    a: Bytes[200] = concat(b"hello", b" world")
    b_val: uint256 = len(s)
    c: Bytes[10] = slice(b"hello world", 0, 5)
    d: String[32] = uint2str(42)
    return b_val

@external
@pure
def convert_types(a: uint256) -> int256:
    return convert(a, int256)

@external
@pure
def loop_example(n: uint256) -> uint256:
    total: uint256 = 0
    for i: uint256 in range(n, bound=100):
        if i == 50:
            break
        if i % 2 == 0:
            continue
        total += i
    return total

# --------------------------------- Internal --------------------------------

@internal
def _validate_amount(amount: uint256) -> bool:
    if amount == 0:
        return False
    elif amount > max_value(uint128):
        return False
    else:
        return True

# --------------------------------- Raw ops ---------------------------------

@external
@payable
def call_external(target: address, data: Bytes[1024]) -> Bytes[1024]:
    assert msg.sender == self.owner

    success: bool = False
    response: Bytes[1024] = b""
    success, response = raw_call(
        target,
        data,
        max_outsize=1024,
        value=msg.value,
        revert_on_failure=False,
    )
    assert success
    return response

@external
def create_child(code: Bytes[4096]) -> address:
    return raw_create(code, value=0)

@external
def emit_raw():
    raw_log(
        [keccak256("RawEvent()")],
        b"",
    )

# --------------------------------- Hex literals ----------------------------

@external
@pure
def hex_example() -> bytes4:
    x: bytes4 = 0x12345678
    y: Bytes[4] = 0x"deadbeef"
    return x

# --------------------------------- Default ---------------------------------

@external
@payable
def __default__():
    pass

# --------------------------------- Staticcall ------------------------------

@external
@view
def check_balance(token: address) -> uint256:
    return staticcall IERC20(token).balanceOf(self)
