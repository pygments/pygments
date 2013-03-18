use NQPP6QRegex;
use QAST;

# This powers the optimization pass. It takes place after we've done all
# of the stuff in the grammar and actions, which means CHECK time is over.
# Thus we're allowed to assume that lexpads are immutable, declarations are
# over and done with, multi candidate lists won't change and so forth.
class Perl6::Optimizer {
    # Tracks the nested blocks we're in; it's the lexical chain, essentially.
    has @!block_stack;

    # How deep a chain we're in, for chaining operators.
    has $!chain_depth;
    
    # Unique ID for topic ($_) preservation registers.
    has $!pres_topic_counter;
    
    # Unique ID for inline args variables.
    has $!inline_arg_counter;
    
    # Things that should cause compilation to fail; keys are errors, value is
    # array of line numbers.
    has %!deadly;
    
    # Things that should be warned about; keys are warnings, value is an array
    # of line numbers.
    has %!worrying;
    
    # The type type, Mu.
    has $!Mu;
    
    has %!foldable_junction;
    has %!foldable_outer;

    # Entry point for the optimization process.
    method optimize($past, *%adverbs) {
        # Initialize.
        @!block_stack := [$past[0]];
        $!chain_depth := 0;
        $!pres_topic_counter := 0;
        $!inline_arg_counter := 0;
        %!deadly := nqp::hash();
        %!worrying := nqp::hash();
        my $*DYNAMICALLY_COMPILED := 0;
        %!foldable_junction{'&infix:<|>'} :=  '&infix:<||>';
        %!foldable_junction{'&infix:<&>'} :=  '&infix:<&&>';

        # until there's a good way to figure out flattening at compile time,
        # don't support these junctions
        #%!foldable_junction{'&any'} := '&infix:<||>';
        #%!foldable_junction{'&all'} :=  '&infix:<&&>';

        %!foldable_outer{'&prefix:<?>'} := 1;
        %!foldable_outer{'&prefix:<!>'} := 1;
        %!foldable_outer{'&prefix:<so>'} := 1;
        %!foldable_outer{'&prefix:<not>'} := 1;

        %!foldable_outer{'if'} := 1;
        %!foldable_outer{'unless'} := 1;
        %!foldable_outer{'while'} := 1;
        %!foldable_outer{'until'} := 1;

        # Work out optimization level.
        my $*LEVEL := nqp::existskey(%adverbs, 'optimize') ??
            +%adverbs<optimize> !! 2;
        
        # Locate UNIT and some other useful symbols.
        my $unit := $past<UNIT>;
        my $*GLOBALish := $past<GLOBALish>;
        my $*W := $past<W>;
        unless nqp::istype($unit, QAST::Block) {
            nqp::die("Optimizer could not find UNIT");
        }
        nqp::push(@!block_stack, $unit);
        $!Mu := self.find_lexical('Mu');
        nqp::pop(@!block_stack);
        
        # Walk and optimize the program.
        self.visit_block($unit);
        
        # Die if we failed check in any way; otherwise, print any warnings.
        if +%!deadly {
            my @fails;
            for %!deadly {
                my @parts := nqp::split("\n", $_.key);
                my $headline := @parts.shift();
                @fails.push("$headline (line" ~ (+$_.value == 1 ?? ' ' !! 's ') ~
                    nqp::join(', ', $_.value) ~ ")" ~
                    (+@parts ?? "\n" ~ nqp::join("\n", @parts) !! ""));
            }
            nqp::die("CHECK FAILED:\n" ~ nqp::join("\n", @fails))
        }
        if +%!worrying {
            pir::printerr__vs("WARNINGS:\n");
            my @fails;
            for %!worrying {
                pir::printerr__vs($_.key ~ " (line" ~ (+$_.value == 1 ?? ' ' !! 's ') ~
                    nqp::join(', ', $_.value) ~ ")\n");
            }
        }
        
        $past
    }
    
    # Called when we encounter a block in the tree.
    method visit_block($block) {
        # Push block onto block stack.
        @!block_stack.push($block);
        
        # Visit children.
        if $block<DYNAMICALLY_COMPILED> {
            my $*DYNAMICALLY_COMPILED := 1;
            self.visit_children($block);
        }
        else {
            self.visit_children($block);
        }
        
        # Pop block from block stack.
        @!block_stack.pop();
        
        # If the block is immediate, we may be able to inline it.
        my $outer := @!block_stack[+@!block_stack - 1];
        if $block.blocktype eq 'immediate' && !$*DYNAMICALLY_COMPILED {
            # Scan symbols for any non-interesting ones.
            my @sigsyms;
            for $block.symtable() {
                my $name := $_.key;
                if $name ne '$_' && $name ne 'call_sig' && $name ne '$*DISPATCHER' {
                    @sigsyms.push($name);
                }
            }
            
            # If we have no interesting ones, then we can inline the
            # statements.
            # XXX We can also check for lack of colliding symbols and
            # do something in that case. However, it's non-trivial as
            # the static lexpad entries will need twiddling with.
            if +@sigsyms == 0 {
                if $*LEVEL >= 3 {
                    return self.inline_immediate_block($block, $outer);
                }
            }
        }
        
        $block
    }
    method is_from_core($name) {
        my $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym && nqp::existskey(%sym, 'value') {
                my %sym := $block.symbol("!CORE_MARKER");
                if +%sym {
                    return 1;
                }
                return 0;
            }
        }
        return 0;
    }

    method can_chain_junction_be_warped($node) {
        sub has_core-ish_junction($node) {
            if nqp::istype($node, QAST::Op) && $node.op eq 'call' &&
                    nqp::existskey(%!foldable_junction, $node.name) {
                if self.is_from_core($node.name) {
                    # TODO: special handling for any()/all(), because they create
                    #       a Stmts with a infix:<,> in it.
                    if +$node.list == 1 {
                        return 0;
                    }
                    return 1;
                }
            }
            return 0;
        }

        if has_core-ish_junction($node[0]) {
            return 0;
        } elsif has_core-ish_junction($node[1]) {
            return 1;
        }
        return -1;
    }
    
    # Called when we encounter a QAST::Op in the tree. Produces either
    # the op itself or some replacement opcode to put in the tree.
    method visit_op($op) {
        # If it's a QAST::Op of type handle, needs some special attention.
        my $optype := $op.op;
        if $optype eq 'handle' {
            return self.visit_handle($op);
        }
        
        # A chain with exactly two children can become the op itself.
        if $optype eq 'chain' {
            $!chain_depth := $!chain_depth + 1;
            $optype := 'call' if $!chain_depth == 1 &&
                !(nqp::istype($op[0], QAST::Op) && $op[0].op eq 'chain') &&
                !(nqp::istype($op[1], QAST::Op) && $op[1].op eq 'chain');
        }

        # there's a list of foldable outers up in the constructor.
        sub is_outer_foldable() {
            if $op.op eq "call" {
                if nqp::existskey(%!foldable_outer, $op.name) && self.is_from_core($op.name) {
                    return 1;
                }
            } elsif nqp::existskey(%!foldable_outer, $op.op) {
                return 1;
            }
            return 0;
        }

        # we may be able to unfold a junction at compile time.
        if $*LEVEL >= 2 && is_outer_foldable() && nqp::istype($op[0], QAST::Op) && $op[0].op eq "chain" {
            my $exp-side := self.can_chain_junction_be_warped($op[0]);
            if $exp-side != -1 {
                my $juncop := $op[0][$exp-side].name eq '&infix:<&>' ?? 'if' !! 'unless';
                my $juncname := %!foldable_junction{$op[0][$exp-side].name};
                my $chainop := $op[0].op;
                my $chainname := $op[0].name;
                my $values := $op[0][$exp-side];
                my $ovalue := $op[0][1 - $exp-side];

                # the first time $valop is refered to, create a bind op for a
                # local var, next time create a reference var op.
                my %reference;
                sub refer_to($valop) {
                    my $id := $valop;
                    if nqp::existskey(%reference, $id) {
                        QAST::Var.new(:name(%reference{$id}), :scope<local>);
                    } else {
                        %reference{$id} := $op.unique('junction_unfold');
                        QAST::Op.new(:op<bind>,
                                     QAST::Var.new(:name(%reference{$id}),
                                                   :scope<local>,
                                                   :decl<var>),
                                     $valop);
                    }
                }

                # create a comparison operation for the inner comparisons
                sub chain($value) {
                    if $exp-side == 0 {
                        QAST::Op.new(:op($chainop), :name($chainname),
                                     $value,
                                     refer_to($ovalue));
                    } else {
                        QAST::Op.new(:op($chainop), :name($chainname),
                                     refer_to($ovalue),
                                     $value);
                    }
                }

                # create a chain of outer logical junction operators with inner comparisons
                sub create_junc() {
                    my $junc := QAST::Op.new(:name($juncname), :op($juncop));

                    $junc.push(chain($values.shift()));

                    if +$values.list > 1 {
                        $junc.push(create_junc());
                    } else {
                        $junc.push(chain($values.shift()));
                    }
                    return $junc;
                }

                $op.shift;
                $op.unshift(create_junc());
                #say($op.dump);
                return self.visit_op($op);
            }
        }
        
        # Visit the children.
        self.visit_children($op);
        
        # Calls are especially interesting as we may wish to do some
        # kind of inlining.
        if $optype eq 'call' && $op.name ne '' {
            # See if we can find the thing we're going to call.
            my $obj;
            my $found;
            try {
                $obj := self.find_lexical($op.name);
                $found := 1;
            }
            if $found {
                # If it's an onlystar proto, we have a couple of options.
                # The first is that we may be able to work out what to
                # call at compile time. Failing that, we can at least inline
                # the proto.
                my $dispatcher;
                try { if $obj.is_dispatcher { $dispatcher := 1 } }
                if $dispatcher && $obj.onlystar {
                    # Try to do compile-time multi-dispatch. Need to consider
                    # both the proto and the multi candidates.
                    my @ct_arg_info := self.analyze_args_for_ct_call($op);
                    if +@ct_arg_info {
                        my @types := @ct_arg_info[0];
                        my @flags := @ct_arg_info[1];
                        my $ct_result_proto := pir::perl6_trial_bind_ct__IPPP($obj.signature, @types, @flags);
                        my @ct_result_multi := pir::perl6_multi_dispatch_ct__PPPP($obj, @types, @flags);
                        if $ct_result_proto == 1 && @ct_result_multi[0] == 1 {
                            my $chosen := @ct_result_multi[1];
                            if $op.op eq 'chain' { $!chain_depth := $!chain_depth - 1 }
                            if $*LEVEL >= 2 {
                                return nqp::can($chosen, 'inline_info') && nqp::istype($chosen.inline_info, QAST::Node)
                                    ?? self.inline_call($op, $chosen)
                                    !! self.call_ct_chosen_multi($op, $obj, $chosen);
                            }
                        }
                        elsif $ct_result_proto == -1 || @ct_result_multi[0] == -1 {
                            self.report_innevitable_dispatch_failure($op, @types, @flags, $obj,
                                :protoguilt($ct_result_proto == -1));
                        }
                    }
                    
                    # Otherwise, inline the proto.
                    if $op.op eq 'chain' { $!chain_depth := $!chain_depth - 1 }
                    if $*LEVEL >= 2 {
                        return self.inline_proto($op, $obj);
                    }
                }
                elsif !$dispatcher && nqp::can($obj, 'signature') {
                    # If we know enough about the arguments, do a "trial bind".
                    my @ct_arg_info := self.analyze_args_for_ct_call($op);
                    if +@ct_arg_info {
                        my @types := @ct_arg_info[0];
                        my @flags := @ct_arg_info[1];
                        my $ct_result := pir::perl6_trial_bind_ct__IPPP($obj.signature, @types, @flags);
                        if $ct_result == 1 {
                            if $op.op eq 'chain' { $!chain_depth := $!chain_depth - 1 }
                            #say("# trial bind worked!");
                            if $*LEVEL >= 2 {
                                return nqp::can($obj, 'inline_info') && nqp::istype($obj.inline_info, QAST::Node)
                                    ?? self.inline_call($op, $obj)
                                    !! copy_returns($op, $obj);
                            }
                        }
                        elsif $ct_result == -1 {
                            self.report_innevitable_dispatch_failure($op, @types, @flags, $obj);
                        }
                    }
                }
            }
            else {
                # We really should find routines; failure to do so is a CHECK
                # time error. Check that it's not just compile-time unknown,
                # however (shows up in e.g. sub foo(&x) { x() }).
                unless self.is_lexical_declared($op.name) {
                    self.add_deadly($op, "Undefined routine '" ~ $op.name ~ "' called");
                }
            }
        }
        
        # If it's a private method call, we can sometimes resolve it at
        # compile time. If so, we can reduce it to a sub call in some cases.
        elsif $*LEVEL >= 3 && $op.op eq 'callmethod' && $op.name eq 'dispatch:<!>' {
            if $op[1].has_compile_time_value && nqp::istype($op[1], QAST::Want) && $op[1][1] eq 'Ss' {
                my $name := $op[1][2].value; # get raw string name
                my $pkg  := $op[2].returns;  # actions always sets this
                my $meth := $pkg.HOW.find_private_method($pkg, $name);
                if $meth {
                    try {
                        $*W.get_ref($meth); # may fail, thus the try; verifies it's in SC
                        my $call := QAST::WVal.new( :value($meth) );
                        my $inv  := $op.shift;
                        $op.shift; $op.shift; # name, package (both pre-resolved now)
                        $op.unshift($inv);
                        $op.unshift($call);
                        $op.op('call');
                        $op.name(nqp::null());
                    }
                }
                else {
                    self.add_deadly($op, "Undefined private method '" ~ $name ~ "' called");
                }
            }
        }
        
        # If we end up here, just leave op as is.
        if $op.op eq 'chain' {
            $!chain_depth := $!chain_depth - 1;
        }
        $op
    }
    
    # Handles visiting a QAST::Op :op('handle').
    method visit_handle($op) {
        self.visit_children($op, :skip_selectors);
        $op
    }
    
    # Handles visiting a QAST::Want node.
    method visit_want($want) {
        # Just visit the children for now. We ignore the literal strings, so
        # it all works out.
        self.visit_children($want, :skip_selectors)
    }
    
    # Handles visit a variable node.
    method visit_var($var) {
        # Nothing to do yet.
    }
    
    # Checks arguments to see if we're going to be able to do compile
    # time analysis of the call.
    my @allo_map := ['', 'Ii', 'Nn', 'Ss'];
    my %allo_rev := nqp::hash('Ii', 1, 'Nn', 2, 'Ss', 3);
    method analyze_args_for_ct_call($op) {
        my @types;
        my @flags;
        my @allomorphs;
        my $num_prim := 0;
        my $num_allo := 0;
        
        # Initial analysis.
        for @($op) {
            # Can't cope with flattening or named.
            if $_.flat || $_.named ne '' {
                return [];
            }
            
            # See if we know the node's type; if so, check it.
            my $type := $_.returns();
            my $ok_type := 0;
            try $ok_type := nqp::istype($type, $!Mu);
            if $ok_type {
                my $prim := pir::repr_get_primitive_type_spec__IP($type);
                my $allo := $_.has_compile_time_value && nqp::istype($_, QAST::Want)
                    ?? $_[1] !! '';
                @types.push($type);
                @flags.push($prim);
                @allomorphs.push($allo);
                $num_prim := $num_prim + 1 if $prim;
                $num_allo := $num_allo + 1 if $allo;
            }
            else {
                return [];
            }
        }
        
        # See if we have an allomorphic constant that may allow us to do
        # a native dispatch with it; takes at least one declaratively
        # native argument to make this happen.
        if @types == 2 && $num_prim == 1 && $num_allo == 1 {
            my $prim_flag := @flags[0] || @flags[1];
            my $allo_idx := @allomorphs[0] ?? 0 !! 1;
            if @allomorphs[$allo_idx] eq @allo_map[$prim_flag] {
                @flags[$allo_idx] := $prim_flag;
            }
        }
        
        # Alternatively, a single arg that is allomorphic will prefer
        # the literal too.
        if @types == 1 && $num_allo == 1 {
            @flags[0] := %allo_rev{@allomorphs[0]} // 0;
        }
        
        [@types, @flags]
    }
    
    method report_innevitable_dispatch_failure($op, @types, @flags, $obj, :$protoguilt) {
        my @arg_names;
        my $i := 0;
        while $i < +@types {
            @arg_names.push(
                @flags[$i] == 1 ?? 'int' !!
                @flags[$i] == 2 ?? 'num' !!
                @flags[$i] == 3 ?? 'str' !!
                @types[$i].HOW.name(@types[$i]));
            $i := $i + 1;
        }
        self.add_deadly($op,
            ($protoguilt ?? "Calling proto of '" !! "Calling '") ~ 
            $obj.name ~ "' will never work with " ~
            (+@arg_names == 0 ??
                "no arguments" !!
                "argument types (" ~ nqp::join(', ', @arg_names) ~ ")"),
            $obj.is_dispatcher && !$protoguilt ??
                multi_sig_list($obj) !!
                ["    Expected: " ~ $obj.signature.perl]);
    }
    
    # Signature list for multis.
    sub multi_sig_list($dispatcher) {
        my @sigs := ["    Expected any of:"];
        for $dispatcher.dispatchees {
            @sigs.push("    " ~ $_.signature.perl);
        }
        @sigs
    }
    
    # Visits all of a nodes children, and dispatches appropriately.
    method visit_children($node, :$skip_selectors) {
        my $i := 0;
        while $i < +@($node) {
            unless $skip_selectors && $i % 2 {
                my $visit := $node[$i];
                if nqp::istype($visit, QAST::Op) {
                    $node[$i] := self.visit_op($visit)
                }
                elsif nqp::istype($visit, QAST::Want) {
                    self.visit_want($visit);
                }
                elsif nqp::istype($visit, QAST::Var) {
                    self.visit_var($visit);
                }
                elsif nqp::istype($visit, QAST::Block) {
                    $node[$i] := self.visit_block($visit);
                }
                elsif nqp::istype($visit, QAST::Stmts) {
                    self.visit_children($visit);
                }
                elsif nqp::istype($visit, QAST::Stmt) {
                    self.visit_children($visit);
                }
            }
            $i := $i + 1;
        }
    }
    
    # Locates a lexical symbol and returns its compile time value. Dies if
    # it does not exist.
    method find_lexical($name) {
        my $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym {
                if nqp::existskey(%sym, 'value') {
                    return %sym<value>;
                }
                else {
                    nqp::die("Optimizer: No lexical compile time value for $name");
                }
            }
        }
        nqp::die("Optimizer: No lexical $name found");
    }
    
    # Checks if a given lexical is declared, though it needn't have a compile
    # time known value.
    method is_lexical_declared($name) {
        my $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym {
                return 1;
            }
        }
        0
    }
    
    # Inlines an immediate block.
    method inline_immediate_block($block, $outer) {
        # Sanity check.
        return $block if +@($block) != 2;

        # Extract interesting parts of block.
        my $decls := $block.shift;
        my $stmts := $block.shift;

        # Turn block into an "optimized out" stub (deserialization
        # or fixup will still want it to be there).
        $block.blocktype('declaration');
        $block[0] := QAST::Op.new( :op('die_s'),
            QAST::SVal.new( :value('INTERNAL ERROR: Execution of block eliminated by optimizer') ) );
        $outer[0].push($block);
        
        # Copy over interesting stuff in declaration section.
        for @($decls) {
            if nqp::istype($_, QAST::Op) && ($_.op eq 'p6bindsig' || 
                    $_.op eq 'bind' && $_[0].name eq 'call_sig') {
                # Don't copy this binder call or setup.
            }
            elsif nqp::istype($_, QAST::Var) && ($_.name eq '$/' || $_.name eq '$!' ||
                    $_.name eq '$_' || $_.name eq '$*DISPATCHER') {
                # Don't copy this variable node.
            }
            else {
                $outer[0].push($_);
            }
        }

        # Hand back the statements, but be sure to preserve $_
        # around them.
        $!pres_topic_counter := $!pres_topic_counter + 1;
        $outer[0].push(QAST::Var.new( :scope('local'),
            :name("pres_topic_$!pres_topic_counter"), :decl('var') ));
        return QAST::Stmts.new(
            :resultchild(1),
            QAST::Op.new( :op('bind'),
                QAST::Var.new( :name("pres_topic_$!pres_topic_counter"), :scope('local') ),
                QAST::Var.new( :name('$_'), :scope('lexical') )
            ),
            $stmts,
            QAST::Op.new( :op('bind'),
                QAST::Var.new( :name('$_'), :scope('lexical') ),
                QAST::Var.new( :name("pres_topic_$!pres_topic_counter"), :scope('local') )
            )
        );
    }
    
    # Inlines a proto.
    method inline_proto($call, $proto) {
        $call.unshift(QAST::Op.new(
            :op('p6mdthunk'),
            QAST::Var.new( :name($call.name), :scope('lexical') )));
        $call.name(nqp::null());
        $call.op('call');
        $call
    }
    
    # Inlines a call to a sub.
    method inline_call($call, $code_obj) {
        # If the code object is marked soft, can't inline it.
        if nqp::can($code_obj, 'soft') && $code_obj.soft {
            return $call;
        }
        
        # Bind the arguments to temporaries.
        my $inlined := QAST::Stmts.new();
        my @subs;
        for $call.list {
            my $temp_name := '_inline_arg_' ~ ($!inline_arg_counter := $!inline_arg_counter + 1);
            my $temp_type := $_.returns;
            $inlined.push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($temp_name), :scope('local'), :returns($temp_type), :decl('var') ),
                $_));
            nqp::push(@subs, QAST::Var.new( :name($temp_name), :scope('local'), :returns($temp_type) ));
        }
        
        # Now do the inlining.
        $inlined.push($code_obj.inline_info.substitute_inline_placeholders(@subs));
        if $call.named -> $name {
            $inlined.named($name);
        }
        $inlined.node($call.node);
        
        $inlined
    }
    
    # If we decide a dispatch at compile time, this emits the direct call.
    method call_ct_chosen_multi($call, $proto, $chosen) {
        my @cands := $proto.dispatchees();
        my $idx := 0;
        for @cands {
            if $_ =:= $chosen {
                $call.unshift(QAST::Op.new(
                    :op('p6mdcandthunk'),
                    QAST::Var.new( :name($call.name), :scope('lexical') ),
                    QAST::IVal.new( :value($idx) )
                ));
                $call.name(nqp::null());
                $call.op('call');
                #say("# Compile-time resolved a call to " ~ $proto.name);
                last;
            }
            $idx := $idx + 1;
        }
        $call := copy_returns($call, $chosen);
        $call
    }
    
    # Adds an entry to the list of things that would cause a check fail.
    method add_deadly($past_node, $message, @extras?) {
        my $mnode := $past_node.node;
        my $line := HLL::Compiler.lineof($mnode.orig, $mnode.from);
        my $key := $message ~ (+@extras ?? "\n" ~ nqp::join("\n", @extras) !! "");
        unless %!deadly{$key} {
            %!deadly{$key} := [];
        }
        %!deadly{$key}.push($line);
    }
    
    my @prim_spec_ops := ['', 'p6box_i', 'p6box_n', 'p6box_s'];
    my @prim_spec_flags := ['', 'Ii', 'Nn', 'Ss'];
    sub copy_returns($to, $from) {
        if nqp::can($from, 'returns') {
            my $ret_type := $from.returns();
            if pir::repr_get_primitive_type_spec__IP($ret_type) -> $primspec {
                $to := QAST::Want.new(
                    :named($to.named),
                    QAST::Op.new( :op(@prim_spec_ops[$primspec]), $to ),
                    @prim_spec_flags[$primspec], $to);
            }
            $to.returns($ret_type);
        }
        $to
    }
}
