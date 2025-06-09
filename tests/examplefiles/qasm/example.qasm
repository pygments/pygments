// OpenQASM Test File for Syntax Highlighting
OPENQASM 3.0;
import "stdgates.inc";

// Register declarations
qubit[5] q;
bit[5] c;

// Single-qubit gates
h q[0];      // Hadamard
x q[1];      // Pauli-X
y q[2];      // Pauli-Y
z q[3];      // Pauli-Z
s q[4];      // Phase gate
sdg q[0];    // S-dagger gate
t q[1];      // T gate
tdg q[2];    // T-dagger gate

// Rotation gates
rx(1.5708) q[0];  // RX rotation
ry(1.5708) q[1];  // RY rotation
rz(1.5708) q[2];  // RZ rotation
p(3.1415) q[3];   // Phase shift

// Multi-qubit gates
cx q[0], q[1];        // CNOT
ccx q[1], q[2], q[3]; // Toffoli (CCX)
swap q[2], q[3];      // SWAP gate

// Controlled rotations
u1(0.5) q[4];
u2(0.5, 1.0) q[0];
u3(0.5, 1.0, 1.5) q[1];

// Measurement and reset
c[0] = measure q[0];
c[1] = measure q[1];
reset q[2];

// Barrier
barrier;
