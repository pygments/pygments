component {
	
	public any function init(arg1){
		var this.myVar = arg1;

		return this;
	}

	private void function testFunc(arg1){
		if(structKeyExists(arguments, "arg1")){
			writeoutput("Argument exists");
		}
	}

}