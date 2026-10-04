% MATLAB function argument validation (R2019b+)
% https://www.mathworks.com/help/matlab/matlab_prog/function-argument-validation-1.html

function result = myFunc(a, b, c)
    arguments
        a (1,1) double
        b (1,:) string = "default"
        c {mustBeNumeric, mustBeNonnegative} = 0
    end
    result = a + c;
end

function result = withRepeating(varargin)
    arguments (Repeating)
        x (1,1) double
    end
    result = sum([x{:}]);
end

function result = withInputOutput(a)
    arguments (Input)
        a (1,1) double {mustBePositive}
    end
    arguments (Output)
        result (1,1) double
    end
    result = sqrt(a);
end

% arguments used as a field name (should NOT be a keyword)
s.arguments = 42;
disp(s.arguments);
