trait DeflectionSensing:
    def fetch_reading(self) -> Float64:
        ...

    comptime absolute_tolerance: Float64 = 0.05  # This is made up for this example

    def within_tolerance(self) -> Bool:
        return abs(self.fetch_reading()) <= Self.absolute_tolerance


trait CalibratableDeflectionSensing(DeflectionSensing):
    def calibrate(mut self):
        ...

struct EddyCurrentSensor(CalibratableDeflectionSensing):
    def fetch_reading(self) -> Float64:
        # its implementation

    def calibrate(mut self):
        # its implementation
