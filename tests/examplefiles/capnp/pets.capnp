@0x0123456789abcdef;  # unique file ID

struct Pet {
	name @0 :Text;
	owner @1 :List(Owner);
	birthdate @2 :Date;

	struct Owner {
		name @0 :Text;
		contact @1 :List(Contact);
		
		struct Contact {
			address @0 :Text;
			type @1 :Type;

			enum Type {
				email @0;
				phone @1;
				msgr @2;
			}
		}
	}
}

# a comment

struct Date {
	year @0 :Int16;
	month @1 :UInt8;
	day @2 :UInt8;
}
