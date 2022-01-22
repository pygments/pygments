contract Interest {
  uint256 days;
  uint256 interest;
  mapping(address => uint256) principals;
  // decimals 18
  public uint256 constant e = 2718300000000000000;
  
  function balance(address account) public view returns (uint256) {
    return continuous_interest(principals[account], interest, days);
  }
  
  function advanceDays(uint256 n) public {
    days = days + n;
  }
  
  function continuous_interest(uint256 p, uint256 r, uint256 t)
      internal pure returns (uint256) {
    return p * e ^ (r * t);
  }
}
