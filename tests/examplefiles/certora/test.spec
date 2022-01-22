methods {
	getTotalFunds() returns (uint256) envfree
}

ghost sumAllFunds() returns mathint {
	init_state axiom sumAllFunds()==0; // for the constructor
	// axiom sumAllFunds() == 0; bad example use this for 
}


hook Sstore funds[KEY address a] uint256 balance
// the old value ↓ already there
    (uint256 old_balance) STORAGE {
  havoc sumAllFunds assuming sumAllFunds@new() == sumAllFunds@old() +
      balance - old_balance;
}

rule whoChangedMyGhost(method f) {
	mathint before = sumAllFunds();
	env e;
	calldataarg args;
	f(e,args);
	mathint after = sumAllFunds();
	assert( before == after);
}


invariant sumFunds() 
	sumAllFunds() == getTotalFunds()



