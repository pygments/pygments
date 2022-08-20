// =============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2007-2008 - INRIA
//
//  This file is distributed under the same license as the Scilab package.
// =============================================================================

//==============================================================================
// Benchmark for chol function
//==============================================================================

// <-- BENCH NB RUN : 10 -->

stacksize(30000000);

a = 0;
b = 0;
a = rand(900, 900, 'n');
a = a'*a;

// <-- BENCH START -->
b = chol(a);
// <-- BENCH END -->
