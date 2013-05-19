#import "Somefile.h"

@implementation ABC

- (id)a:(B)b {
    return 1;
}

@end

@implementation ABC

- (void)xyz;

@end

NSDictionary *dictionary = [NSDictionary dictionaryWithObjectsAndKeys:
    @"quattuor", @"four", @"quinque", @"five", @"sex", @"six", nil];


NSString *key;
for (key in dictionary) {
    NSLog(@"English: %@, Latin: %@", key, [dictionary valueForKey:key]);
}

// Literals
NSArray *a = @[ @"1", @"2" ];

NSDictionary *d = @{ @"key": @"value" };

NSNumber *n1 = @( 1 );
NSNumber *n2 = @( [a length] );
